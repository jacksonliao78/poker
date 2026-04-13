type suit =
  | Hearts
  | Diamonds
  | Clubs
  | Spades

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

type card = {
  rank : rank;
  suit : suit;
}

type street =
  | Preflop
  | Flop
  | Turn
  | River

type action =
  | Fold
  | Check
  | Call
  | Bet of int
  | Raise of int

type player_status =
  | Active
  | Folded
  | AllIn

(* this could also just be one set type*)
type bot_style =
  | Passive
  | Aggressive
  | Randomized

type controller =
  | Human
  | Bot of bot_style

type player_state = {
  id : int;
  name : string;
  controller : controller;
  chips : int;
  hole_cards : card list;
  round_bet : int;
  status : player_status;
}

type table_state = {
  community_cards : card list;
  pot : int;
  current_bet : int;
  min_raise : int;
  dealer_index : int;
  turn_index : int;
  street : street;
}

type game_state = {
  players : player_state list;
  deck : card list;
  table : table_state;
  small_blind : int;
  big_blind : int;
}

type config = {
  starting_chips : int;
  small_blind : int;
  big_blind : int;
}
