let default_host = "127.0.0.1"
let default_port = 9000
let event_limit = 8

type event_kind =
  | Info
  | Chat
  | Problem

type ui_event = {
  kind : event_kind;
  text : string;
}

type ui_state = {
  host : string;
  port : int;
  mutable player_id : int option;
  mutable lobby : Poker.Protocol.lobby_snapshot option;
  mutable game : Poker.Protocol.player_view option;
  mutable events : ui_event list;
  mutable input_buffer : string;
  mutable action_index : int option;
  mutable show_recent_activity : bool;
  redraw : bool;
  style : Poker.Terminal_ui.style;
}

let parse_endpoint () =
  let host = if Array.length Sys.argv > 1 then Sys.argv.(1) else default_host in
  let port =
    if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2)
    else default_port
  in
  (host, port)

let resolve_host host =
  try Unix.inet_addr_of_string host
  with Failure _ ->
    let entry = Unix.gethostbyname host in
    entry.h_addr_list.(0)

let supports_redraw () =
  Unix.isatty Unix.stdout
  &&
  match Sys.getenv_opt "TERM" with
  | None -> true
  | Some "dumb" -> false
  | Some _ -> true

let terminal_style redraw =
  (* Plain output keeps logs and redirected sessions free of escape codes. *)
  if redraw && Sys.getenv_opt "NO_COLOR" = None then Poker.Terminal_ui.ansi
  else Poker.Terminal_ui.plain

let create_ui_state host port =
  let redraw = supports_redraw () in
  {
    host;
    port;
    player_id = None;
    lobby = None;
    game = None;
    events = [];
    input_buffer = "";
    action_index = None;
    show_recent_activity = true;
    redraw;
    style = terminal_style redraw;
  }

let add_event state kind text =
  let rec take remaining events =
    if remaining = 0 then []
    else
      match events with
      | [] -> []
      | event :: rest -> event :: take (remaining - 1) rest
  in
  state.events <- take event_limit ({ kind; text } :: state.events)

let render_player_summary (player : Poker.Protocol.player_summary) =
  Printf.sprintf "#%d  %-20s  $%d" player.Poker.Protocol.id player.name
    player.chips

let render_lobby (snapshot : Poker.Protocol.lobby_snapshot) =
  let header =
    Printf.sprintf "Lobby: %d/%d seated"
      (List.length snapshot.Poker.Protocol.players)
      Poker.Protocol.seats_total
  in
  let players =
    match snapshot.Poker.Protocol.players with
    | [] -> [ "Waiting for players..." ]
    | players -> List.map render_player_summary players
  in
  String.concat "\n"
    (header
    :: players
    @ [ Printf.sprintf "Open seats: %d" snapshot.seats_open ])

let render_marker player =
  let markers =
    [
      (player.Poker.Protocol.is_dealer, "D");
      (player.is_small_blind, "SB");
      (player.is_big_blind, "BB");
      (player.is_turn, "TURN");
    ]
    |> List.filter_map (fun (enabled, label) ->
           if enabled then Some label else None)
  in
  if markers = [] then "" else "[" ^ String.concat "," markers ^ "]"

let render_money state width amount =
  let open Poker.Terminal_ui in
  let text = Printf.sprintf "%*s" width ("$" ^ string_of_int amount) in
  green state.style text

let render_public_player state player =
  let open Poker.Terminal_ui in
  let raw_name =
    if Some player.Poker.Protocol.id = state.player_id then player.name ^ " (you)"
    else player.name
  in
  let padded_name = Printf.sprintf "%-24s" raw_name in
  let name =
    if Some player.Poker.Protocol.id = state.player_id then
      bold state.style padded_name
    else padded_name
  in
  let status = render_status player.status in
  let turn_prefix = if player.is_turn then yellow state.style ">" else " " in
  Printf.sprintf "%s %s %6s  bet %-5s %-7s %s" turn_prefix name
    (render_money state 6 player.chips)
    (render_money state 5 player.round_bet)
    status (render_marker player)

let render_actions state actions =
  let open Poker.Terminal_ui in
  match actions with
  | [] -> dim state.style "No poker action available. Type chat, /name <name>, or /quit."
  | actions ->
      actions
      |> List.map (render_legal_action state.style)
      |> String.concat "    "

let render_game state view =
  let open Poker.Terminal_ui in
  let table = view.Poker.Protocol.table in
  let players = List.map (render_public_player state) view.players in
  String.concat "\n"
    ([
       bold state.style "Table";
       Printf.sprintf "Street: %s    Pot: %s    Current bet: %s    Min raise: %s"
         (render_street table.street)
         (money state.style table.pot)
         (money state.style table.current_bet)
         (money state.style table.min_raise);
       "Board: " ^ render_cards state.style table.community_cards;
       "";
       bold state.style "Seats";
     ]
    @ players
    @ [
        "";
        bold state.style "Your hand";
        render_cards state.style view.your_hole_cards;
        "";
        bold state.style "Available actions";
        render_actions state view.legal_actions;
      ])

let render_event state event =
  let open Poker.Terminal_ui in
  match event.kind with
  | Info -> dim state.style event.text
  | Chat -> cyan state.style event.text
  | Problem -> red state.style event.text

let prompt_label state =
  match state.game with
  | Some view when view.Poker.Protocol.legal_actions <> [] -> "Action > "
  | Some _ -> "Chat > "
  | None -> "Command > "

let render_screen state =
  let open Poker.Terminal_ui in
  let player =
    match state.player_id with
    | None -> "joining"
    | Some id -> "#" ^ string_of_int id
  in
  let main =
    match state.game with
    | Some view -> render_game state view
    | None -> (
        match state.lobby with
        | Some lobby -> render_lobby lobby
        | None -> "Waiting for lobby update...")
  in
  let events =
    state.events |> List.rev |> List.map (render_event state) |> String.concat "\n"
  in
  let recent =
    if state.show_recent_activity then
      [
        "";
        bold state.style "Recent";
        (if events = "" then dim state.style "(no messages yet)" else events);
      ]
    else []
  in
  String.concat "\n"
    ([
       bold state.style
         (Printf.sprintf "Poker  %s:%d  Player %s" state.host state.port player);
       String.make 72 '-';
       main;
     ]
    @ recent
    @ [ ""; prompt_label state ^ state.input_buffer ])

let redraw state =
  if state.redraw then
    Lwt_io.printf "%s%s%!" Poker.Terminal_ui.clear_screen (render_screen state)
  else Lwt.return_unit

let legacy_text_of_message = function
  | Poker.Protocol.Welcome { player_id; starting_chips; seats_total } ->
      Printf.sprintf
        "Connected as player #%d. Starting stack: $%d. Seats: %d." player_id
        starting_chips seats_total
  | Lobby_update snapshot -> render_lobby snapshot
  | Chat_message { from_name; text } -> Printf.sprintf "[%s] %s" from_name text
  | Game_update _ -> "Game state updated."
  | Error message -> "Error: " ^ message
  | Info message -> message

let apply_server_message state = function
  | Poker.Protocol.Welcome { player_id; starting_chips; seats_total } ->
      state.player_id <- Some player_id;
      add_event state Info
        (Printf.sprintf "Connected. Stack %s across %d seats."
           (Poker.Terminal_ui.money state.style starting_chips)
           seats_total)
  | Lobby_update snapshot ->
      state.lobby <- Some snapshot;
      if state.game = None then add_event state Info "Lobby updated."
  | Chat_message { from_name; text } ->
      add_event state Chat (Printf.sprintf "[%s] %s" from_name text)
  | Game_update view ->
      state.game <- Some view;
      state.player_id <- Some view.your_id
  | Error message -> add_event state Problem ("Error: " ^ message)
  | Info message -> add_event state Info message

let handle_server_message state message =
  apply_server_message state message;
  if state.redraw then redraw state
  else Lwt_io.printf "%s\n%!" (legacy_text_of_message message)

let rec listen_for_updates state input =
  let%lwt message = Poker.Wire.read_server_message input in
  let%lwt () = handle_server_message state message in
  listen_for_updates state input

let words_of_line line =
  line |> String.trim |> String.split_on_char ' '
  |> List.filter (fun word -> word <> "")

let has_action predicate state =
  match state.game with
  | None -> false
  | Some view -> List.exists predicate view.Poker.Protocol.legal_actions

let raise_minimum state =
  match state.game with
  | None -> None
  | Some view ->
      List.find_map
        (function Poker.Protocol.Can_raise amount -> Some amount | _ -> None)
        view.legal_actions

let action_command = function
  | Poker.Protocol.Can_fold -> "/fold"
  | Can_check -> "/check"
  | Can_call _ -> "/call"
  | Can_raise _ -> "/raise "

let available_action_commands state =
  match state.game with
  | None -> []
  | Some view -> List.map action_command view.Poker.Protocol.legal_actions

let cycle_action state direction =
  match available_action_commands state with
  | [] -> ()
  | actions ->
      let count = List.length actions in
      let current =
        match state.action_index with
        | Some index -> index
        | None -> if direction < 0 then 0 else -1
      in
      let next = (current + direction + count) mod count in
      state.action_index <- Some next;
      state.input_buffer <- List.nth actions next

let reset_input state =
  state.input_buffer <- "";
  state.action_index <- None

let set_recent_preference state value =
  match String.lowercase_ascii value with
  | "show" ->
      state.show_recent_activity <- true;
      add_event state Info "Recent activity is visible.";
      Ok None
  | "hide" ->
      state.show_recent_activity <- false;
      add_event state Info "Recent activity is hidden.";
      Ok None
  | "clear" ->
      state.events <- [];
      Ok None
  | _ -> Error "Use /pref recent show, /pref recent hide, or /pref recent clear."

let handle_preference state words =
  match words with
  | [ "/pref"; "recent"; value ] -> set_recent_preference state value
  | [ "/pref" ] ->
      Error "Use /pref recent show, /pref recent hide, or /pref recent clear."
  | [ "/pref"; _ ] ->
      Error "Use /pref recent show, /pref recent hide, or /pref recent clear."
  | "/pref" :: _ ->
      Error "Use /pref recent show, /pref recent hide, or /pref recent clear."
  | _ -> Error "Unknown preference command."

let poker_action_allowed state action =
  let open Poker.Protocol in
  match action with
  | Poker.Types.Fold -> has_action (function Can_fold -> true | _ -> false) state
  | Check -> has_action (function Can_check -> true | _ -> false) state
  | Call -> has_action (function Can_call _ -> true | _ -> false) state
  | Raise amount -> (
      match raise_minimum state with
      | Some minimum -> amount >= minimum
      | None -> false)
  | Bet _ -> false

let command_to_message state line =
  let trimmed = String.trim line in
  let words = words_of_line line in
  if trimmed = "" then Ok None
  else if String.get trimmed 0 <> '/' then
    Ok (Some (Poker.Protocol.Send_chat trimmed))
  else if trimmed = "/quit" then
    Ok (Some Poker.Protocol.Disconnect)
  else if trimmed = "/cashout" || trimmed = "/cash_out" then
    Ok (Some (Poker.Protocol.Cash_out true))
  else if trimmed = "/cashin" || trimmed = "/cash_in" then
    Ok (Some (Poker.Protocol.Cash_out false))
  else if List.length words > 0 && List.hd words = "/pref" then
    handle_preference state words
  else if String.length trimmed >= 6 && String.sub trimmed 0 6 = "/name " then
    Ok
      (Some
         (Poker.Protocol.Join (String.sub trimmed 6 (String.length trimmed - 6))))
  else
    let action_result =
      match words with
      | [ "/fold" ] | [ "/f" ] -> Some (Ok Poker.Types.Fold)
      | [ "/call" ] | [ "/c" ] -> Some (Ok Poker.Types.Call)
      | [ "/check" ] | [ "/x" ] -> Some (Ok Poker.Types.Check)
      | [ "/raise"; amount ] | [ "/r"; amount ] -> (
          match int_of_string_opt amount with
          | Some amount when amount > 0 -> Some (Ok (Poker.Types.Raise amount))
          | _ -> Some (Error "Raise needs a positive amount, e.g. /raise 20."))
      | "/raise" :: _ | "/r" :: _ ->
          Some (Error "Raise needs a positive amount, e.g. /raise 20.")
      | _ ->
          Some
            (Error
               "Unknown command. Type chat without /, or use /name, /pref, /fold, /call, /check, /raise, /cashout, /cashin, /quit.")
    in
    match action_result with
    | None -> Ok (Some (Poker.Protocol.Send_chat trimmed))
    | Some (Error message) -> Error message
    | Some (Ok action) ->
        if poker_action_allowed state action then
          Ok (Some (Poker.Protocol.Player_action action))
        else Error "That poker action is not available right now."

let with_raw_terminal f =
  let fd = Unix.stdin in
  if not (Unix.isatty fd) then f ()
  else
    let original = Unix.tcgetattr fd in
    let raw = { original with c_icanon = false; c_echo = false; c_vmin = 1; c_vtime = 0 } in
    Unix.tcsetattr fd Unix.TCSANOW raw;
    Lwt.finalize f (fun () ->
        Unix.tcsetattr fd Unix.TCSANOW original;
        Lwt.return_unit)

let append_input state char =
  state.input_buffer <- state.input_buffer ^ String.make 1 char;
  state.action_index <- None

let backspace_input state =
  let length = String.length state.input_buffer in
  if length > 0 then state.input_buffer <- String.sub state.input_buffer 0 (length - 1);
  state.action_index <- None

let submit_input state output =
  let line = state.input_buffer in
  match command_to_message state line with
  | Error message ->
      add_event state Problem message;
      state.action_index <- None;
      redraw state
  | Ok None ->
      reset_input state;
      redraw state
  | Ok (Some Poker.Protocol.Disconnect) ->
      reset_input state;
      Poker.Wire.send_client_message output Poker.Protocol.Disconnect
  | Ok (Some message) ->
      reset_input state;
      let%lwt () = Poker.Wire.send_client_message output message in
      redraw state

let rec read_escape_sequence state =
  let%lwt second = Lwt_io.read_char_opt Lwt_io.stdin in
  match second with
  | Some '[' -> (
      let%lwt third = Lwt_io.read_char_opt Lwt_io.stdin in
      match third with
      | Some 'D' ->
          cycle_action state (-1);
          redraw state
      | Some 'C' ->
          cycle_action state 1;
          redraw state
      | _ -> redraw state)
  | _ -> redraw state

let rec read_raw_commands state output =
  let%lwt char = Lwt_io.read_char_opt Lwt_io.stdin in
  match char with
  | None -> Poker.Wire.send_client_message output Poker.Protocol.Disconnect
  | Some '\004' -> Poker.Wire.send_client_message output Poker.Protocol.Disconnect
  | Some '\n' | Some '\r' ->
      let%lwt () = submit_input state output in
      read_raw_commands state output
  | Some '\t' ->
      cycle_action state 1;
      let%lwt () = redraw state in
      read_raw_commands state output
  | Some '\027' ->
      let%lwt () = read_escape_sequence state in
      read_raw_commands state output
  | Some '\b' | Some '\127' ->
      backspace_input state;
      let%lwt () = redraw state in
      read_raw_commands state output
  | Some char ->
      append_input state char;
      let%lwt () = redraw state in
      read_raw_commands state output

let rec read_line_commands state output =
  let%lwt () =
    if not state.redraw then Lwt_io.printf "%s%!" (prompt_label state)
    else Lwt.return_unit
  in
  let%lwt line = Lwt_io.read_line_opt Lwt_io.stdin in
  match line with
  | None -> Poker.Wire.send_client_message output Poker.Protocol.Disconnect
  | Some line -> (
      match command_to_message state line with
      | Error message ->
          add_event state Problem message;
          let%lwt () = redraw state in
          read_line_commands state output
      | Ok None -> read_line_commands state output
      | Ok (Some Poker.Protocol.Disconnect) ->
          Poker.Wire.send_client_message output Poker.Protocol.Disconnect
      | Ok (Some message) ->
          let%lwt () = Poker.Wire.send_client_message output message in
          read_line_commands state output)

let read_commands state output =
  if state.redraw then with_raw_terminal (fun () -> read_raw_commands state output)
  else read_line_commands state output

let run_client host port =
  let state = create_ui_state host port in
  let%lwt input, output =
    Lwt_io.open_connection (Unix.ADDR_INET (resolve_host host, port))
  in
  let%lwt () =
    let open Poker.Terminal_ui in
    Lwt_io.printf "%s\n%s\n%s %!" (bold state.style "Poker Client")
      (cyan state.style (Printf.sprintf "Connected to %s:%d" host port))
      (bold state.style "Enter your name:")
  in
  let%lwt requested_name = Lwt_io.read_line_opt Lwt_io.stdin in
  let join_name =
    match requested_name with
    | None -> "Player"
    | Some name -> name
  in
  let%lwt () =
    Poker.Wire.send_client_message output (Poker.Protocol.Join join_name)
  in
  add_event state Info
    "Use chat without a prefix. Commands: /name <new name>, /pref recent show|hide|clear, /fold, /call, /check, /raise <amount>, /quit.";
  let%lwt () = redraw state in
  let listener =
    Lwt.catch
      (fun () -> listen_for_updates state input)
      (function
        | End_of_file -> Lwt_io.printl "Server closed the connection."
        | exn -> Lwt.fail exn)
  in
  let commands = read_commands state output in
  Lwt.pick [ listener; commands ]

let () =
  let host, port = parse_endpoint () in
  Lwt_main.run (run_client host port)
