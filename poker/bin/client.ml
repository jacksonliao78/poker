let default_host = "127.0.0.1"
let default_port = 9000

let ansi_reset = "\027[0m"
let ansi_blue = "\027[34m"
let ansi_cyan = "\027[36m"
let ansi_green = "\027[32m"
let ansi_yellow = "\027[33m"
let ansi_red = "\027[31m"
let ansi_bold = "\027[1m"

let colorize color text = color ^ text ^ ansi_reset
let section title = colorize (ansi_bold ^ ansi_blue) ("\n" ^ title)

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

let render_player player =
  Printf.sprintf "#%d  %-20s  $%d" player.Poker.Protocol.id player.name
    player.chips

let render_lobby snapshot =
  let header =
    Printf.sprintf "%s\n"
      (section
         (Printf.sprintf "Lobby: %d/%d seated"
            (List.length snapshot.Poker.Protocol.players)
            Poker.Protocol.seats_total))
  in
  let players =
    match snapshot.Poker.Protocol.players with
    | [] -> [ colorize ansi_yellow "Waiting for players..." ]
    | players -> List.map render_player players
  in
  String.concat "\n"
    (header
    :: players
    @ [ colorize ansi_cyan (Printf.sprintf "Open seats: %d" snapshot.seats_open) ])

let print_server_message = function
  | Poker.Protocol.Welcome { player_id; starting_chips; seats_total } ->
      Lwt_io.printf
        "%s\n%s\n%!"
        (section "Connected")
        (colorize ansi_green
           (Printf.sprintf "Player #%d | Starting stack: $%d | Seats: %d"
              player_id starting_chips seats_total))
  | Lobby_update snapshot -> Lwt_io.printf "%s\n\n%!" (render_lobby snapshot)
  | Chat_message { from_name; text } ->
      Lwt_io.printf "%s %s\n\n%!"
        (colorize ansi_cyan (Printf.sprintf "[%s]" from_name))
        text
  | Error message ->
      Lwt_io.printf "%s\n%s\n\n%!"
        (section "Error")
        (colorize ansi_red message)
  | Info message ->
      Lwt_io.printf "%s\n%s\n\n%!"
        (section "Game Update")
        (colorize ansi_green message)

let rec listen_for_updates input =
  let%lwt message = Poker.Wire.read_server_message input in
  let%lwt () = print_server_message message in
  listen_for_updates input

let command_to_message line =
  let trimmed = String.trim line in
  let words =
    trimmed |> String.split_on_char ' ' |> List.filter (fun word -> word <> "")
  in
  if trimmed = "" then None
  else if trimmed = "quit" then Some Poker.Protocol.Disconnect
  else if String.length trimmed >= 6 && String.sub trimmed 0 6 = "/name " then
    Some
      (Poker.Protocol.Join (String.sub trimmed 6 (String.length trimmed - 6)))
  else
    match words with
    | [ "fold" ] -> Some (Poker.Protocol.Player_action Poker.Types.Fold)
    | [ "call" ] -> Some (Poker.Protocol.Player_action Poker.Types.Call)
    | [ "check" ] -> Some (Poker.Protocol.Player_action Poker.Types.Check)
    | [ "raise"; amount ] -> (
        match int_of_string_opt amount with
        | Some amount when amount > 0 ->
            Some (Poker.Protocol.Player_action (Poker.Types.Raise amount))
        | _ -> Some (Poker.Protocol.Send_chat trimmed))
    | _ -> Some (Poker.Protocol.Send_chat trimmed)

let rec read_commands output =
  let%lwt line = Lwt_io.read_line_opt Lwt_io.stdin in
  match line with
  | None -> Poker.Wire.send_client_message output Poker.Protocol.Disconnect
  | Some line -> (
      match command_to_message line with
      | None -> read_commands output
      | Some Poker.Protocol.Disconnect ->
          Poker.Wire.send_client_message output Poker.Protocol.Disconnect
      | Some message ->
          let%lwt () = Poker.Wire.send_client_message output message in
          read_commands output)

let run_client host port =
  let%lwt input, output =
    Lwt_io.open_connection (Unix.ADDR_INET (resolve_host host, port))
  in
  let%lwt () =
    Lwt_io.printf "%s\n%s\n%s %!"
      (section "Poker Client")
      (colorize ansi_cyan (Printf.sprintf "Connected to %s:%d" host port))
      (colorize ansi_bold "Enter your name:")
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
  let%lwt () =
    Lwt_io.printlf "%s\n%s"
      (section "Commands")
      (colorize ansi_yellow
         "Type chat, `/name <new name>`, `fold`, `call`, `check`, `raise 20`, or `quit`.")
  in
  let listener =
    Lwt.catch
      (fun () -> listen_for_updates input)
      (function
        | End_of_file -> Lwt_io.printl "Server closed the connection."
        | exn -> Lwt.fail exn)
  in
  let commands = read_commands output in
  Lwt.pick [ listener; commands ]

let () =
  let host, port = parse_endpoint () in
  Lwt_main.run (run_client host port)
