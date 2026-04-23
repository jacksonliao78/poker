(** Messages a client may send to the server during the lobby phase. *)
type client_message =
  | Join of string
  | Send_chat of string
  | Disconnect

(** Public player information that can be shown to every connected client. *)
type player_summary = {
  id : int;
  name : string;
  chips : int;
  connected : bool;
}

(** Snapshot of the lobby view broadcast to clients. *)
type lobby_snapshot = {
  players : player_summary list;
  seats_open : int;
}

(** Messages the server may send to connected clients. *)
type server_message =
  | Welcome of {
      player_id : int;
      starting_chips : int;
      seats_total : int;
    }
  | Lobby_update of lobby_snapshot
  | Chat_message of {
      from_name : string;
      text : string;
    }
  | Error of string
  | Info of string

(** Maximum seats accepted by the lobby server. *)
val seats_total : int

(** Long names are capped to keep the text UI readable. *)
val max_name_length : int
