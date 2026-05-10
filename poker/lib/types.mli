(** The four suits used in a standard 52-card deck. *)
type suit =
  | Hearts
  | Diamonds
  | Clubs
  | Spades

(** The ordered card ranks in a standard deck of cards. *)
type rank =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

(** A single playing card. *)
type card = {
  rank : rank;
  suit : suit;
}

(** The betting streets in Texas Hold'em. *)
type street =
  | Preflop
  | Flop
  | Turn
  | River

(** A player's declared betting decision. *)
type action =
  | Fold
  | Check
  | Call
  | Bet of int
  | Raise of int

(** A player's status within the current hand. *)
type player_status =
  | Active
  | Folded
  | AllIn

(** Bot play-style personalities. *)
type bot_style =
  | Passive
  | Aggressive
  | Randomized

(** Who controls a seat. *)
type controller =
  | Human
  | Bot of bot_style

(** State tracked for a seated player. *)
type player_state = {
  id : int;
      (** Stable seat identity used by the server instead of display names. *)
  name : string;  (** Display name already suitable for the terminal layout. *)
  controller : controller;
      (** Whether the seat is human-controlled or automated. *)
  chips : int;  (** Chips not yet committed to the current pot. *)
  hole_cards : card list;  (** Two private cards dealt to a player. *)
  round_bet : int;  (** Chips committed on the current betting street. *)
  status : player_status;
      (** Whether this player can still act or win the current hand. *)
  last_street_action : action option;
      (** Last action kept for redraw context until the next betting street. *)
}

(** Shared table state visible to every player. *)
type table_state = {
  community_cards : card list;  (** Public board cards in deal order. *)
  pot : int;  (** Chips committed to the hand and waiting to be awarded. *)
  current_bet : int;
      (** Largest per-player commitment required to stay in the street. *)
  min_raise : int;  (** Smallest legal raise over the amount needed to call. *)
  dealer_index : int;  (** Index of the button in [game_state.players]. *)
  turn_index : int;  (** Index of the current actor in [game_state.players]. *)
  street : street;
}

(** Complete game state. *)
type game_state = {
  players : player_state list;
  deck : card list;  (** Remaining cards in the deck for a game. *)
  table : table_state;
  small_blind : int;
      (** Small blind amount copied into the hand for stable rule checks. *)
  big_blind : int;
      (** Big blind amount copied into the hand for minimum-raise resets. *)
}

(** Table-wide constants used when creating new lobby seats and hands. *)
type config = {
  starting_chips : int;  (** Initial stack assigned to a new lobby player. *)
  small_blind : int;  (** Forced bet posted by the small blind. *)
  big_blind : int;  (** Forced bet posted by the big blind. *)
}

(** [default_config] is the table configuration used by lobby and hand setup. *)
val default_config : config
