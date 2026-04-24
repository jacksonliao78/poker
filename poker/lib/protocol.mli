(** Messages a client may send to the server during the lobby phase. *)
type client_message =
  | Join of string
  | Player_action of Types.action
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

(** Maximum seats accepted by the lobby server. *)
val seats_total : int

(** Long names are capped to keep the text UI readable. *)
val max_name_length : int

(** Derives turn-sensitive legal actions from the authoritative game state. *)
val legal_actions_for_player : Types.game_state -> player_id:int -> legal_action list

(** Builds the personalized table view without leaking another player's cards. *)
val player_view_of_game : Types.game_state -> player_id:int -> player_view option
