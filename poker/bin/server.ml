type client = {
  id : int;
  input : Lwt_io.input_channel;
  output : Lwt_io.output_channel;
  joined : bool;
}

type server_state = {
  lobby : Poker.Lobby.t;
  clients : client list;
  game : Poker.Types.game_state option;
  cash_out_votes : int list;
}

type event =
  | Client_connected of {
      input : Lwt_io.input_channel;
      output : Lwt_io.output_channel;
    }
  | Client_message of {
      player_id : int;
      message : Poker.Protocol.client_message;
    }
  | Client_disconnected of int

(* Empty lobby. *)
let initial_state =
  {
    lobby = Poker.Lobby.empty ();
    clients = [];
    game = None;
    cash_out_votes = [];
  }

let safe_send output message =
  Lwt.catch
    (fun () -> Poker.Wire.send_server_message output message)
    (fun _ -> Lwt.return_unit)

let close_client_channels client =
  Lwt.catch
    (fun () ->
      let%lwt () = Lwt_io.close client.input in
      Lwt_io.close client.output)
    (fun _ -> Lwt.return_unit)

(* Send message to entire lobby. *)
let broadcast state message =
  Lwt_list.iter_p (fun client -> safe_send client.output message) state.clients

(* Send lobby state to entire lobby. *)
let broadcast_lobby state =
  let snapshot = Poker.Lobby.snapshot state.lobby in
  broadcast state (Poker.Protocol.Lobby_update snapshot)

let send_game_update game client =
  match Poker.Protocol.player_view_of_game game ~player_id:client.id with
  | None -> Lwt.return_unit
  | Some view -> safe_send client.output (Poker.Protocol.Game_update view)

let broadcast_game_update state game =
  (* Each view is personalized because hole cards are private. *)
  Lwt_list.iter_p (send_game_update game) state.clients

(* Normalizes player name. *)
let trim_name raw_name =
  let trimmed = String.trim raw_name in
  if trimmed = "" then None
  else
    let max_len = Poker.Protocol.max_name_length in
    let length = min (String.length trimmed) max_len in
    Some (String.sub trimmed 0 length)

let find_client state player_id =
  List.find_opt (fun client -> client.id = player_id) state.clients

let update_client state updated_client =
  let clients =
    List.map
      (fun client ->
        if client.id = updated_client.id then updated_client else client)
      state.clients
  in
  { state with clients }

let remove_client state player_id =
  let lobby = Poker.Lobby.remove_player state.lobby ~player_id in
  let client, clients =
    List.fold_right
      (fun current (removed, kept) ->
        if current.id = player_id then (Some current, kept)
        else (removed, current :: kept))
      state.clients (None, [])
  in
  ({ state with lobby; clients }, client)

let player_name state player_id = Poker.Lobby.player_name state.lobby ~player_id

let joined_client_count state =
  List.fold_left
    (fun count client -> if client.joined then count + 1 else count)
    0 state.clients

let hole_cards_message cards =
  cards
  |> List.map Poker.Cards.card_to_long_string
  |> String.concat " and "
  |> Printf.sprintf "Your hole cards: %s"

let player_name_at players index = (List.nth players index).Poker.Types.name

let community_cards_message cards =
  cards
  |> List.map Poker.Cards.card_to_long_string
  |> String.concat ", " |> Printf.sprintf "Board: %s"

let reveal_message_for_street (game : Poker.Types.game_state) =
  let cards = game.table.community_cards in
  match (game.table.street, List.length cards) with
  | Flop, 3 ->
      Some (Printf.sprintf "Flop revealed. %s" (community_cards_message cards))
  | Turn, 4 ->
      Some (Printf.sprintf "Turn revealed. %s" (community_cards_message cards))
  | River, 5 ->
      Some (Printf.sprintf "River revealed. %s" (community_cards_message cards))
  | _ -> None

let player_name_at players index = (List.nth players index).Poker.Types.name

let current_turn_message (game : Poker.Types.game_state) =
  let current_player =
    List.nth game.Poker.Types.players game.table.turn_index
  in
  Printf.sprintf "Player's turn: %s" current_player.name

let table_setup_messages (game : Poker.Types.game_state) =
  let players = game.Poker.Types.players in
  let dealer_name = player_name_at players game.table.dealer_index in
  let small_blind_index =
    (game.table.dealer_index + 1) mod List.length players
  in
  let big_blind_index = (game.table.dealer_index + 2) mod List.length players in
  let small_blind_player = List.nth players small_blind_index in
  let big_blind_player = List.nth players big_blind_index in
  let blind_message label amount player =
    if player.Poker.Types.round_bet < amount then
      Printf.sprintf "%s: %s is all-in for $%d" label player.name
        player.round_bet
    else Printf.sprintf "%s: %s posts $%d" label player.name amount
  in
  [
    Printf.sprintf "Dealer: %s" dealer_name;
    blind_message "Small blind" game.small_blind small_blind_player;
    blind_message "Big blind" game.big_blind big_blind_player;
    Printf.sprintf "Pot: $%d. Current bet: $%d." game.table.pot
      game.table.current_bet;
    current_turn_message game;
  ]

let prompt_current_player (_game : Poker.Types.game_state) _clients =
  Lwt.return_unit

let public_action_message name action =
  match action with
  | Poker.Types.Fold -> Printf.sprintf "%s folds." name
  | Check -> Printf.sprintf "%s checks." name
  | Call -> Printf.sprintf "%s calls." name
  | Raise amount -> Printf.sprintf "%s raises by $%d." name amount
  | Bet amount -> Printf.sprintf "%s bets $%d." name amount

let send_balance_update state player_id players =
  match
    ( find_client state player_id,
      List.find_opt (fun player -> player.Poker.Types.id = player_id) players )
  with
  | Some client, Some player ->
      safe_send client.output
        (Poker.Protocol.Info
           (Printf.sprintf "Your stack: $%d. Your round bet: $%d." player.chips
              player.round_bet))
  | _ -> Lwt.return_unit

let send_private_hole_cards_for_game state (game : Poker.Types.game_state) =
  Lwt_list.iter_p
    (fun client ->
      match
        List.find_opt
          (fun player -> player.Poker.Types.id = client.id)
          game.players
      with
      | None -> Lwt.return_unit
      | Some player ->
          safe_send client.output
            (Poker.Protocol.Info (hole_cards_message player.hole_cards)))
    state.clients

let final_standings_messages (game : Poker.Types.game_state) =
  let standings =
    game.Poker.Types.players
    |> List.map (fun p ->
           Printf.sprintf "%s: $%d" p.Poker.Types.name p.chips)
  in
  "Game over. Final standings:" :: standings

let everyone_voted_to_cash_out (game : Poker.Types.game_state) votes =
  let surviving =
    List.filter (fun p -> p.Poker.Types.chips > 0) game.players
  in
  match surviving with
  | [] -> false
  | _ -> List.for_all (fun p -> List.mem p.Poker.Types.id votes) surviving

let handle_player_action state player_id action =
  match state.game with
  | None -> (
      match find_client state player_id with
      | None -> Lwt.return state
      | Some client ->
          let%lwt () =
            safe_send client.output
              (Poker.Protocol.Error "The game has not started yet.")
          in
          Lwt.return state)
  | Some game -> (
      match Poker.Game.apply_action game ~player_id action with
      | Error message -> (
          match find_client state player_id with
          | None -> Lwt.return state
          | Some client ->
              let%lwt () =
                safe_send client.output (Poker.Protocol.Error message)
              in
              Lwt.return state)
      | Ok outcome -> (
          let actor_name =
            match
              List.find_opt
                (fun player -> player.Poker.Types.id = player_id)
                game.players
            with
            | Some player -> player.name
            | None -> "A player"
          in
          let%lwt () =
            broadcast state
              (Poker.Protocol.Info (public_action_message actor_name action))
          in
          match outcome with
          | Poker.Game.Next_turn next_game ->
              let next_state = { state with game = Some next_game } in
              let%lwt () =
                send_balance_update next_state player_id next_game.players
              in
              let%lwt () = broadcast_game_update next_state next_game in
              let%lwt () =
                match reveal_message_for_street next_game with
                | None -> Lwt.return_unit
                | Some message ->
                    broadcast next_state (Poker.Protocol.Info message)
              in
              let%lwt () =
                broadcast next_state
                  (Poker.Protocol.Info (current_turn_message next_game))
              in
              let%lwt () =
                send_private_hole_cards_for_game next_state next_game
              in
              let%lwt () = prompt_current_player next_game next_state.clients in
              Lwt.return next_state
          | Poker.Game.Betting_round_complete next_game ->
              let next_state = { state with game = Some next_game } in
              let%lwt () =
                send_balance_update next_state player_id next_game.players
              in
              let%lwt () = broadcast_game_update next_state next_game in
              let%lwt () =
                broadcast next_state
                  (Poker.Protocol.Info
                     "Betting round complete. Street progression is not \
                      implemented yet.")
              in
              let%lwt () =
                send_private_hole_cards_for_game next_state next_game
              in
              Lwt.return next_state
          | Poker.Game.Hand_complete (next_game, winner) ->
              let winner_name =
                if winner.Poker.Types.status = Poker.Types.Folded then
                  actor_name
                else winner.name
              in
              let pot_amount = next_game.Poker.Types.table.pot in
              let settled =
                Poker.Game.award_pot next_game ~winner_id:winner.id
              in
              let intermediate_state = { state with game = Some settled } in
              let%lwt () =
                send_balance_update intermediate_state player_id settled.players
              in
              let%lwt () = broadcast_game_update intermediate_state settled in
              let%lwt () =
                broadcast intermediate_state
                  (Poker.Protocol.Info
                     (Printf.sprintf "Hand complete. %s wins $%d."
                        winner_name pot_amount))
              in
              let end_now =
                everyone_voted_to_cash_out settled state.cash_out_votes
              in
              if end_now then (
                let next_state =
                  { state with game = None; cash_out_votes = [] }
                in
                let%lwt () =
                  Lwt_list.iter_s
                    (fun text ->
                      broadcast next_state (Poker.Protocol.Info text))
                    (final_standings_messages settled)
                in
                Lwt.return next_state)
              else
                match Poker.Game.next_hand settled with
                | None ->
                    let next_state =
                      { state with game = None; cash_out_votes = [] }
                    in
                    let%lwt () =
                      Lwt_list.iter_s
                        (fun text ->
                          broadcast next_state (Poker.Protocol.Info text))
                        (final_standings_messages settled)
                    in
                    Lwt.return next_state
                | Some new_game ->
                    let next_state = { state with game = Some new_game } in
                    let%lwt () =
                      broadcast next_state
                        (Poker.Protocol.Info "Dealing next hand.")
                    in
                    let%lwt () =
                      Lwt_list.iter_s
                        (fun text ->
                          broadcast next_state (Poker.Protocol.Info text))
                        (table_setup_messages new_game)
                    in
                    let%lwt () =
                      broadcast_game_update next_state new_game
                    in
                    let%lwt () =
                      send_private_hole_cards_for_game next_state new_game
                    in
                    let%lwt () =
                      prompt_current_player new_game next_state.clients
                    in
                    Lwt.return next_state))

let start_game_if_ready state =
  if
    state.game <> None
    || joined_client_count state <> Poker.Protocol.seats_total
  then Lwt.return state
  else
    let game = Poker.Game.start (Poker.Lobby.players state.lobby) in
    let next_state = { state with game = Some game } in
    let%lwt () =
      broadcast next_state
        (Poker.Protocol.Info "4/4 players joined. Starting the game.")
    in
    let%lwt () =
      Lwt_list.iter_s
        (fun text -> broadcast next_state (Poker.Protocol.Info text))
        (table_setup_messages game)
    in
    let%lwt () = broadcast_game_update next_state game in
    let%lwt () = prompt_current_player game next_state.clients in
    Lwt.return next_state

let send_welcome client =
  safe_send client.output
    (Poker.Protocol.Welcome
       {
         player_id = client.id;
         starting_chips = Poker.Types.default_config.starting_chips;
         seats_total = Poker.Protocol.seats_total;
       })

let rec client_reader player_id input push_event =
  Lwt.catch
    (fun () ->
      let%lwt message = Poker.Wire.read_client_message input in
      push_event (Some (Client_message { player_id; message }));
      client_reader player_id input push_event)
    (fun _ ->
      push_event (Some (Client_disconnected player_id));
      Lwt.return_unit)

let start_client_reader player_id input push_event =
  Lwt.async (fun () -> client_reader player_id input push_event)

let handle_new_connection state input output push_event =
  if List.length state.clients >= Poker.Protocol.seats_total then
    let%lwt () =
      safe_send output (Poker.Protocol.Error "The lobby is already full.")
    in
    let%lwt () =
      safe_send output (Poker.Protocol.Info "Server closed this connection.")
    in
    let temp_client = { id = -1; input; output; joined = false } in
    let%lwt () = close_client_channels temp_client in
    Lwt.return state
  else
    let lobby, player = Poker.Lobby.add_player state.lobby in
    let client = { id = player.id; input; output; joined = false } in
    let next_state =
      { state with lobby; clients = state.clients @ [ client ] }
    in
    let () = start_client_reader player.id input push_event in
    Lwt.return next_state

let handle_join state player_id requested_name =
  match trim_name requested_name with
  | None -> (
      match find_client state player_id with
      | None -> Lwt.return state
      | Some client ->
          let%lwt () =
            safe_send client.output
              (Poker.Protocol.Error "Names cannot be blank.")
          in
          Lwt.return state)
  | Some name -> (
      match find_client state player_id with
      | None -> Lwt.return state
      | Some client ->
          let previous_name = player_name state player_id in
          let lobby = Poker.Lobby.rename_player state.lobby ~player_id name in
          let next_state =
            if client.joined then { state with lobby }
            else
              update_client { state with lobby } { client with joined = true }
          in
          let%lwt () =
            if client.joined then
              match previous_name with
              | Some old_name when old_name <> name ->
                  broadcast next_state
                    (Poker.Protocol.Info
                       (old_name ^ " is now known as " ^ name ^ "."))
              | _ -> Lwt.return_unit
            else send_welcome { client with joined = true }
          in
          let%lwt () =
            if client.joined then Lwt.return_unit
            else
              broadcast next_state
                (Poker.Protocol.Info (name ^ " joined the lobby."))
          in
          let%lwt () = broadcast_lobby next_state in
          if client.joined then Lwt.return next_state
          else start_game_if_ready next_state)

let handle_chat state player_id text =
  let cleaned = String.trim text in
  if cleaned = "" then Lwt.return state
  else
    match player_name state player_id with
    | None -> Lwt.return state
    | Some from_name ->
        let%lwt () =
          broadcast state
            (Poker.Protocol.Chat_message { from_name; text = cleaned })
        in
        Lwt.return state

let handle_cash_out state player_id wants =
  let already = List.mem player_id state.cash_out_votes in
  let cash_out_votes =
    if wants then
      if already then state.cash_out_votes
      else state.cash_out_votes @ [ player_id ]
    else List.filter (fun id -> id <> player_id) state.cash_out_votes
  in
  let next_state = { state with cash_out_votes } in
  let name =
    match player_name state player_id with
    | Some n -> n
    | None -> "A player"
  in
  let msg =
    if wants then Printf.sprintf "%s will cash out after this hand." name
    else Printf.sprintf "%s is no longer cashing out." name
  in
  let%lwt () = broadcast next_state (Poker.Protocol.Info msg) in
  Lwt.return next_state

let handle_disconnect state player_id =
  let name_before_removal = player_name state player_id in
  let next_state, removed_client = remove_client state player_id in
  let next_state = { next_state with game = None; cash_out_votes = [] } in
  match removed_client with
  | None -> Lwt.return state
  | Some client ->
      let info =
        match name_before_removal with
        | None -> "A player disconnected."
        | Some name -> name ^ " disconnected."
      in
      let%lwt () = close_client_channels client in
      let%lwt () = broadcast next_state (Poker.Protocol.Info info) in
      let%lwt () = broadcast_lobby next_state in
      Lwt.return next_state

let handle_event state push_event = function
  | Client_connected { input; output } ->
      handle_new_connection state input output push_event
  | Client_message { player_id; message } -> (
      match message with
      | Poker.Protocol.Join requested_name ->
          handle_join state player_id requested_name
      | Poker.Protocol.Player_action action ->
          handle_player_action state player_id action
      | Poker.Protocol.Send_chat text -> handle_chat state player_id text
      | Poker.Protocol.Cash_out wants ->
          handle_cash_out state player_id wants
      | Poker.Protocol.Disconnect -> handle_disconnect state player_id)
  | Client_disconnected player_id -> handle_disconnect state player_id

let rec event_loop state stream push_event =
  let%lwt next_event = Lwt_stream.get stream in
  match next_event with
  | None -> Lwt.return_unit
  | Some event ->
      let%lwt next_state = handle_event state push_event event in
      event_loop next_state stream push_event

let socket_address port = Unix.ADDR_INET (Unix.inet_addr_any, port)

(* Accepted sockets are turned into queue events instead of mutating state
   directly. *)
let rec accept_loop socket push_event =
  let%lwt client_socket, _ = Lwt_unix.accept socket in
  let output_socket = Lwt_unix.dup client_socket in
  let input = Lwt_io.of_fd ~mode:Lwt_io.Input client_socket in
  let output = Lwt_io.of_fd ~mode:Lwt_io.Output output_socket in
  push_event (Some (Client_connected { input; output }));
  accept_loop socket push_event

(* Startup binds the socket, starts producers, and leaves all state to the event
   loop. *)
let start_server port =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  let%lwt () = Lwt_unix.bind socket (socket_address port) in
  Lwt_unix.listen socket Poker.Protocol.seats_total;
  let%lwt () = Lwt_io.printf "Listening on port %d\n%!" port in
  let stream, push_event = Lwt_stream.create () in
  let () = Lwt.async (fun () -> accept_loop socket push_event) in
  event_loop initial_state stream push_event

let port =
  if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 9000

let () = Lwt_main.run (start_server port)
