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

(** Bot play-style personalities . *)
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
  name : string;
  controller : controller;
      (** Whether the seat is human-controlled or automated. *)
  chips : int;
  hole_cards : card list;  (** Two private cards dealt to a player. *)
  round_bet : int;
  status : player_status;
  last_street_action : action option;
}

(** Shared table state visible to every player. *)
type table_state = {
  community_cards : card list;
  pot : int;
  current_bet : int;
  min_raise : int;
  dealer_index : int;
  turn_index : int;
  street : street;
}

(** Complete game state. *)
type game_state = {
  players : player_state list;
  deck : card list;  (** Remaining cards in the deck for a game. *)
  table : table_state;
  small_blind : int;
  big_blind : int;
}

(** Table-wide constants *)
type config = {
  starting_chips : int;
  small_blind : int;
  big_blind : int;
}

(** Default configuration. *)
val default_config : config
