let send_client_message output message =
  let%lwt () = Lwt_io.write_value output message in
  Lwt_io.flush output

let read_client_message input = Lwt_io.read_value input

let send_server_message output message =
  let%lwt () = Lwt_io.write_value output message in
  Lwt_io.flush output

let read_server_message input = Lwt_io.read_value input
