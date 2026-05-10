(** [send_client_message output message] writes one client frame and flushes
    immediately so the server is not left waiting on buffered input. *)
val send_client_message :
  Lwt_io.output_channel -> Protocol.client_message -> unit Lwt.t

(** [read_client_message input] blocks until one full client message arrives. *)
val read_client_message : Lwt_io.input_channel -> Protocol.client_message Lwt.t

(** [send_server_message output message] writes one server message and flushes
    immediately. *)
val send_server_message :
  Lwt_io.output_channel -> Protocol.server_message -> unit Lwt.t

(** [read_server_message input] blocks until one full server message arrives. *)
val read_server_message : Lwt_io.input_channel -> Protocol.server_message Lwt.t
