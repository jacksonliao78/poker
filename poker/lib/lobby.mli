(** Public state stored for each connected player in the pre-game lobby. *)
type player = {
  id : int;
  name : string;
  chips : int;
  connected : bool;
}

(** Internal lobby state tracked by the server. *)
type t

(** [empty ()] returns the lobby state before any connections are accepted. *)
val empty : unit -> t

(** [add_player lobby] allocates the next seat with the default starting stack.
    The returned player is the same seat appended to the returned lobby.
    Requires: [lobby] was produced by this module, so [next_id] remains unique.
*)
val add_player : t -> t * player

(** [rename_player lobby ~player_id name] updates a player's display name.
    Unknown [player_id] values leave the lobby unchanged. Requires: [name] is
    already trimmed and capped for display. *)
val rename_player : t -> player_id:int -> string -> t

(** [remove_player lobby ~player_id] drops a seat after disconnect. Unknown
    [player_id] values leave the lobby unchanged. *)
val remove_player : t -> player_id:int -> t

(** [player_name lobby ~player_id] looks up the current display name for
    [player_id]. *)
val player_name : t -> player_id:int -> string option

(** [players lobby] returns the public player list in seat order. *)
val players : t -> player list

(** [snapshot lobby] returns the lobby projection sent to clients after each
    change. The optional [host_id] is the seat permitted to issue [/start].
    Requires: [lobby] does not contain more than [Protocol.seats_total]
    players. *)
val snapshot : ?host_id:int -> t -> Protocol.lobby_snapshot
