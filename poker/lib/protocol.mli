(** Messages a client may send to the server during the lobby phase. *)
type client_message =
  | Join of string
  | Player_action of Types.action
  | Send_chat of string
  | Cash_out of bool
  | Start_game
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

(** Commands the current game state allows this client to submit. *)
type legal_action =
  | Can_fold
  | Can_check
  | Can_call of int
  | Can_raise of int

(** Public per-seat state; private cards are intentionally excluded. *)
type public_player = {
  id : int;
  name : string;
  chips : int;
  round_bet : int;
  status : Types.player_status;
  is_dealer : bool;
  is_small_blind : bool;
  is_big_blind : bool;
  is_turn : bool;
  last_street_action : Types.action option;
}

(** Public table details needed for a complete terminal redraw. *)
type table_view = {
  community_cards : Types.card list;
  pot : int;
  current_bet : int;
  min_raise : int;
  street : Types.street;
}

(** Personalized view sent to one client; only [your_hole_cards] is private. *)
type player_view = {
  your_id : int;
  your_hole_cards : Types.card list;
  table : table_view;
  players : public_player list;
  legal_actions : legal_action list;
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
  | Game_update of player_view
  | Error of string
  | Info of string

(** [seats_total] is the maximum seat count accepted by the lobby server. *)
val seats_total : int

(** [max_name_length] caps display names so the terminal table can redraw
    without uncontrolled wrapping. *)
val max_name_length : int

(** [legal_actions_for_player game ~player_id] derives turn-sensitive legal
    actions from the authoritative game state. Unknown players and players who
    cannot act receive an empty action list. Requires: [game.players] is
    nonempty and [game.table.turn_index] is a valid index into it. Raises:
    [Failure] if [game.table.turn_index] is outside [game.players];
    [Invalid_argument] if [game.table.turn_index] is negative. *)
val legal_actions_for_player :
  Types.game_state -> player_id:int -> legal_action list

(** [player_view_of_game game ~player_id] builds the personalized table view
    without leaking another player's cards. Returns [None] for an unknown
    player. Requires: [game.players] is nonempty, and dealer/turn indexes are
    valid. Raises: [Division_by_zero] if [game.players] is empty and [player_id]
    is present in malformed state. *)
val player_view_of_game :
  Types.game_state -> player_id:int -> player_view option
