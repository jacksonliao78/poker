type client = {
  id : int;
  input : Lwt_io.input_channel;
  output : Lwt_io.output_channel;
}

type server_state = {
  lobby : Poker.Lobby.t;
  clients : client list;
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
let initial_state = { lobby = Poker.Lobby.empty (); clients = [] }

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

let remove_client state player_id =
  let lobby = Poker.Lobby.remove_player state.lobby ~player_id in
  let client, clients =
    List.fold_right
      (fun current (removed, kept) ->
        if current.id = player_id then (Some current, kept)
        else (removed, current :: kept))
      state.clients (None, [])
  in
  ({ lobby; clients }, client)

let player_name state player_id = Poker.Lobby.player_name state.lobby ~player_id

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
    let temp_client = { id = -1; input; output } in
    let%lwt () = close_client_channels temp_client in
    Lwt.return state
  else
    let lobby, player = Poker.Lobby.add_player state.lobby in
    let client = { id = player.id; input; output } in
    let next_state = { lobby; clients = state.clients @ [ client ] } in
    let welcome =
      Poker.Protocol.Welcome
        {
          player_id = player.id;
          starting_chips = player.chips;
          seats_total = Poker.Protocol.seats_total;
        }
    in
    let%lwt () = safe_send output welcome in
    let%lwt () = broadcast_lobby next_state in
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
  | Some name ->
      let lobby = Poker.Lobby.rename_player state.lobby ~player_id name in
      let next_state = { state with lobby } in
      let%lwt () =
        broadcast next_state (Poker.Protocol.Info (name ^ " joined the lobby."))
      in
      let%lwt () = broadcast_lobby next_state in
      Lwt.return next_state

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

let handle_disconnect state player_id =
  let name_before_removal = player_name state player_id in
  let next_state, removed_client = remove_client state player_id in
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
      | Poker.Protocol.Send_chat text -> handle_chat state player_id text
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
