(** Public state stored for each connected player in the pre-game lobby. *)
type player = {
  id : int;
  name : string;
  chips : int;
  connected : bool;
}

(** Internal lobby state tracked by the server. *)
type t

(** Empty lobby before any connections are accepted. *)
val empty : unit -> t

(** Allocates the next seat with the default starting stack. *)
val add_player : t -> t * player

(** Updates a player's display name. *)
val rename_player : t -> player_id:int -> string -> t

(** Drops a seat after disconnect. *)
val remove_player : t -> player_id:int -> t

(** Looks up the current display name. *)
val player_name : t -> player_id:int -> string option

(** Public player list in seat order. *)
val players : t -> player list

(** Lobby projection sent to clients after each change. *)
val snapshot : t -> Protocol.lobby_snapshot
