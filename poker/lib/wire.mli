(** Writes one client frame and flushes immediately. *)
val send_client_message :
  Lwt_io.output_channel -> Protocol.client_message -> unit Lwt.t

(** Blocks until one full client message arrives. *)
val read_client_message : Lwt_io.input_channel -> Protocol.client_message Lwt.t

(** Writes one server message to clients. *)
val send_server_message :
  Lwt_io.output_channel -> Protocol.server_message -> unit Lwt.t

(** Blocks until one full server message arrives. *)
val read_server_message : Lwt_io.input_channel -> Protocol.server_message Lwt.t
