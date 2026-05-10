let default_host = "127.0.0.1"
let default_port = 9000
let event_limit = 8
let connection_timeout_seconds = 5.0

exception Connection_timeout

type event_kind =
  | Info of int (* color cycle index *)
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
  mutable info_color_index : int;
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

let connection_error_message host port = function
  | Connection_timeout ->
      Printf.sprintf
        "Connection failed: no poker server answered at %s:%d within %.0f \
         seconds. Start the server there, or check the host and port."
        host port connection_timeout_seconds
  | Unix.Unix_error (Unix.EBADF, _, _) ->
      Printf.sprintf
        "Connection failed: no poker server answered at %s:%d within %.0f \
         seconds. Start the server there, or check the host and port."
        host port connection_timeout_seconds
  | Unix.Unix_error
      ( ( Unix.ECONNREFUSED
        | Unix.EHOSTUNREACH
        | Unix.ENETUNREACH
        | Unix.ETIMEDOUT ),
        _,
        _ ) ->
      Printf.sprintf
        "Connection failed: no poker server is accepting connections at %s:%d. \
         Start the server there, or check the host and port."
        host port
  | Unix.Unix_error ((Unix.EACCES | Unix.EPERM), _, _) ->
      Printf.sprintf
        "Unix connection failed: the operating system denied access to %s:%d. \
         Check the host, port, firewall, and socket permissions."
        host port
  | Unix.Unix_error (error, _, _) ->
      Printf.sprintf "Connection failed for %s:%d: %s." host port
        (Unix.error_message error)
  | Not_found ->
      Printf.sprintf "Connection failed: could not resolve host %s." host
  | Failure message ->
      Printf.sprintf "Connection failed for %s:%d: %s." host port message
  | exn ->
      Printf.sprintf "Connection failed for %s:%d: %s." host port
        (Printexc.to_string exn)

let supports_redraw () =
  Unix.isatty Unix.stdout
  &&
  match Sys.getenv_opt "TERM" with
  | None -> true
  | Some "dumb" -> false
  | Some _ -> true

let terminal_style redraw =
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
    info_color_index = 0;
    redraw;
    style = terminal_style redraw;
  }

let starts_with prefix s =
  let lp = String.length prefix and ls = String.length s in
  ls >= lp && String.sub s 0 lp = prefix

let contains_substring sub s =
  let ls = String.length s and lsub = String.length sub in
  let rec scan i =
    i + lsub <= ls && (String.sub s i lsub = sub || scan (i + 1))
  in
  scan 0

let is_turn_message text = starts_with "Player's turn:" text

let is_action_message text =
  contains_substring " folds." text
  || contains_substring " calls." text
  || contains_substring " checks." text
  || contains_substring " raises by " text
  || contains_substring " bets " text

let is_round_event text =
  starts_with "Hand complete." text
  || starts_with "Dealing next hand." text
  || starts_with "Game over." text

let is_reveal_event text =
  starts_with "Flop revealed." text
  || starts_with "Turn revealed." text
  || starts_with "River revealed." text

let add_info state text =
  if not (is_action_message text || is_round_event text || is_reveal_event text)
  then ()
  else
    let starting = if is_reveal_event text then [] else state.events in
    let kind = Info state.info_color_index in
    state.info_color_index <- state.info_color_index + 1;
    let event = { kind; text } in
    let rec take remaining = function
      | _ when remaining = 0 -> []
      | [] -> []
      | event :: rest -> event :: take (remaining - 1) rest
    in
    state.events <- take 3 (event :: starting)

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
    ((header :: players)
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

let action_label player =
  if player.Poker.Protocol.status = Poker.Types.Folded then "[FOLD]"
  else
    match player.last_street_action with
    | None -> ""
    | Some Poker.Types.Fold -> "[FOLD]"
    | Some Check -> "[CHECK]"
    | Some Call -> "[CALL]"
    | Some (Raise n) -> Printf.sprintf "[RAISE %d]" n
    | Some (Bet n) -> Printf.sprintf "[BET %d]" n

let render_public_player state player =
  let open Poker.Terminal_ui in
  let label = action_label player in
  let name_with_label =
    if label = "" then player.Poker.Protocol.name
    else player.Poker.Protocol.name ^ " " ^ label
  in
  let padded_name = Printf.sprintf "%-32s" name_with_label in
  let name =
    if player.is_turn then bold state.style padded_name else padded_name
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
  | [] ->
      dim state.style
        "No poker action available. Type chat, /name <name>, or /quit."
  | actions ->
      actions
      |> List.map (render_legal_action state.style)
      |> String.concat "    "

let render_seats_block state view =
  let open Poker.Terminal_ui in
  let players =
    List.map (render_public_player state) view.Poker.Protocol.players
  in
  bold state.style "Seats" :: players

let render_table_block state view =
  let open Poker.Terminal_ui in
  let table = view.Poker.Protocol.table in
  let your_stack =
    match
      List.find_opt (fun p -> p.Poker.Protocol.id = view.your_id) view.players
    with
    | Some p -> p.chips
    | None -> 0
  in
  [
    bold state.style "Table";
    Printf.sprintf
      "Street: %s    Pot: %s    Current bet: %s    Min raise: %s    Stack: %s"
      (render_street table.street)
      (money state.style table.pot)
      (money state.style table.current_bet)
      (money state.style table.min_raise)
      (money state.style your_stack);
  ]

let visible_width s =
  let len = String.length s in
  let rec loop i width in_escape =
    if i >= len then width
    else
      let c = s.[i] in
      if in_escape then
        if c = 'm' then loop (i + 1) width false else loop (i + 1) width true
      else if c = '\027' then loop (i + 1) width true
      else if Char.code c < 0x80 then loop (i + 1) (width + 1) false
      else if Char.code c < 0xC0 then loop (i + 1) width false
      else loop (i + 1) (width + 1) false
  in
  loop 0 0 false

let pad_to width s =
  let w = visible_width s in
  if w >= width then s ^ "    " else s ^ String.make (width - w) ' '

let render_hand_block state view =
  let open Poker.Terminal_ui in
  let table = view.Poker.Protocol.table in
  let hand_label = "Your hand" in
  let board_label = "Board" in
  let label_width = 24 in
  [
    bold state.style (pad_to label_width hand_label ^ board_label);
    pad_to label_width (render_cards state.style view.your_hole_cards)
    ^ render_cards state.style table.community_cards;
  ]

let render_actions_block state view =
  let open Poker.Terminal_ui in
  [
    bold state.style "Available actions";
    render_actions state view.Poker.Protocol.legal_actions;
  ]

let render_event state event =
  let open Poker.Terminal_ui in
  match event.kind with
  | Info _ -> orange state.style event.text
  | Chat -> cyan state.style event.text
  | Problem -> red state.style event.text

let prompt_label state =
  match state.game with
  | Some view when view.Poker.Protocol.legal_actions <> [] -> (
      let to_call =
        List.find_map
          (function
            | Poker.Protocol.Can_call amount -> Some amount
            | _ -> None)
          view.legal_actions
      in
      let to_raise =
        List.find_map
          (function
            | Poker.Protocol.Can_raise amount -> Some amount
            | _ -> None)
          view.legal_actions
      in
      match (to_call, to_raise) with
      | Some n, _ -> Printf.sprintf "Your turn ($%d to call) > " n
      | None, Some n -> Printf.sprintf "Your turn ($%d to raise) > " n
      | None, None -> "Your turn > ")
  | Some _ -> "Chat > "
  | None -> "Command > "

let render_screen state =
  let open Poker.Terminal_ui in
  let player =
    match state.player_id with
    | None -> "joining"
    | Some id -> "#" ^ string_of_int id
  in
  let events =
    state.events |> List.rev
    |> List.map (render_event state)
    |> String.concat "\n"
  in
  let recent_block =
    if state.show_recent_activity then
      [
        bold state.style "Recent";
        (if events = "" then dim state.style "(no messages yet)" else events);
      ]
    else []
  in
  let body =
    match state.game with
    | Some view ->
        let sep = [ "" ] in
        render_seats_block state view
        @ sep @ recent_block @ sep
        @ render_actions_block state view
        @ sep
        @ render_table_block state view
        @ sep
        @ render_hand_block state view
    | None ->
        let lobby_lines =
          match state.lobby with
          | Some lobby -> [ render_lobby lobby ]
          | None -> [ "Waiting for lobby update..." ]
        in
        if recent_block = [] then lobby_lines
        else lobby_lines @ [ "" ] @ recent_block
  in
  String.concat "\n"
    ([
       bold state.style
         (Printf.sprintf "Poker  %s:%d  Player %s" state.host state.port player);
       String.make 72 '-';
     ]
    @ body
    @ [ ""; prompt_label state ^ state.input_buffer ])

let redraw state =
  if state.redraw then
    Lwt_io.printf "%s%s%!" Poker.Terminal_ui.clear_screen (render_screen state)
  else Lwt.return_unit

let legacy_text_of_message = function
  | Poker.Protocol.Welcome { player_id; starting_chips; seats_total } ->
      Printf.sprintf "Connected as player #%d. Starting stack: $%d. Seats: %d."
        player_id starting_chips seats_total
  | Lobby_update snapshot -> render_lobby snapshot
  | Chat_message { from_name; text } -> Printf.sprintf "[%s] %s" from_name text
  | Game_update _ -> "Game state updated."
  | Error message -> "Error: " ^ message
  | Info message -> message

let apply_server_message state = function
  | Poker.Protocol.Welcome { player_id; starting_chips; seats_total } ->
      state.player_id <- Some player_id;
      add_info state
        (Printf.sprintf "Connected. Stack %s across %d seats."
           (Poker.Terminal_ui.money state.style starting_chips)
           seats_total)
  | Lobby_update snapshot ->
      state.lobby <- Some snapshot;
      if state.game = None then add_info state "Lobby updated."
  | Chat_message { from_name; text } ->
      add_event state Chat (Printf.sprintf "[%s] %s" from_name text)
  | Game_update view ->
      state.game <- Some view;
      state.player_id <- Some view.your_id
  | Error message -> add_event state Problem ("Error: " ^ message)
  | Info message -> add_info state message

let handle_server_message state message =
  apply_server_message state message;
  if state.redraw then redraw state
  else Lwt_io.printf "%s\n%!" (legacy_text_of_message message)

let rec listen_for_updates state input =
  let%lwt message = Poker.Wire.read_server_message input in
  let%lwt () = handle_server_message state message in
  listen_for_updates state input

let legal_actions state =
  match state.game with
  | None -> []
  | Some view -> view.Poker.Protocol.legal_actions

let available_action_commands state =
  Poker.Client_command.available_action_commands (legal_actions state)

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

let set_recent_preference state = function
  | Poker.Client_command.Show_recent ->
      state.show_recent_activity <- true;
      add_info state "Recent activity is visible.";
      Ok None
  | Poker.Client_command.Hide_recent ->
      state.show_recent_activity <- false;
      add_info state "Recent activity is hidden.";
      Ok None
  | Poker.Client_command.Clear_recent ->
      state.events <- [];
      Ok None

let command_to_message state line =
  match
    Poker.Client_command.parse ~legal_actions:(legal_actions state) line
  with
  | Poker.Client_command.Noop -> Ok None
  | Poker.Client_command.Send message -> Ok (Some message)
  | Poker.Client_command.Set_preference preference ->
      set_recent_preference state preference
  | Poker.Client_command.Error message -> Error message

let with_raw_terminal f =
  let fd = Unix.stdin in
  if not (Unix.isatty fd) then f ()
  else
    let original = Unix.tcgetattr fd in
    let raw =
      {
        original with
        c_icanon = false;
        c_echo = false;
        c_vmin = 1;
        c_vtime = 0;
      }
    in
    Unix.tcsetattr fd Unix.TCSANOW raw;
    Lwt.finalize f (fun () ->
        Unix.tcsetattr fd Unix.TCSANOW original;
        Lwt.return_unit)

let append_input state char =
  state.input_buffer <- state.input_buffer ^ String.make 1 char;
  state.action_index <- None

let backspace_input state =
  let length = String.length state.input_buffer in
  if length > 0 then
    state.input_buffer <- String.sub state.input_buffer 0 (length - 1);
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
  | Some '\004' ->
      Poker.Wire.send_client_message output Poker.Protocol.Disconnect
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
  if state.redraw then
    with_raw_terminal (fun () -> read_raw_commands state output)
  else read_line_commands state output

let open_connection_with_timeout host port =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.set_blocking socket false;
  let address = Unix.ADDR_INET (resolve_host host, port) in
  let close_socket_no_error () =
    Lwt.catch (fun () -> Lwt_unix.close socket) (fun _ -> Lwt.return_unit)
  in
  let timeout =
    let%lwt () = Lwt_unix.sleep connection_timeout_seconds in
    Lwt.async close_socket_no_error;
    Lwt.fail Connection_timeout
  in
  Lwt.catch
    (fun () ->
      let%lwt () = Lwt.pick [ Lwt_unix.connect socket address; timeout ] in
      let output_socket = Lwt_unix.dup socket in
      Lwt.return
        ( Lwt_io.of_fd ~mode:Lwt_io.input socket,
          Lwt_io.of_fd ~mode:Lwt_io.output output_socket ))
    (function
      | Connection_timeout as exn -> Lwt.fail exn
      | exn ->
          Lwt.async close_socket_no_error;
          Lwt.fail exn)

let run_client host port =
  let state = create_ui_state host port in
  Lwt.catch
    (fun () ->
      let%lwt input, output = open_connection_with_timeout host port in
      let%lwt () =
        let open Poker.Terminal_ui in
        Lwt_io.printf "%s\n%s\n%s %!"
          (bold state.style "Poker Client")
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
      add_info state
        "Use chat without a prefix. Commands: /name <new name>, /pref recent \
         show|hide|clear, /fold, /call, /check, /raise <amount>, /quit.";
      let%lwt () = redraw state in
      let listener =
        Lwt.catch
          (fun () -> listen_for_updates state input)
          (function
            | End_of_file -> Lwt_io.printl "Server closed the connection."
            | exn -> Lwt.fail exn)
      in
      let commands = read_commands state output in
      Lwt.pick [ listener; commands ])
    (fun exn -> Lwt_io.eprintl (connection_error_message host port exn))

let () =
  let host, port = parse_endpoint () in
  Lwt_main.run (run_client host port)
