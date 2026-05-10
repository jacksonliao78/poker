(** [trim_name raw_name] trims surrounding whitespace and caps the result to
    {!Protocol.max_name_length}. *)
val trim_name : string -> string option

(** [hole_cards_message cards] formats the private hole-card line for one
    player. Requires: [cards] is the player's private card list.*)
val hole_cards_message : Types.card list -> string

(** [community_cards_message cards] formats a public board line. Requires:
    [cards] is ordered from earliest to latest dealt.*)
val community_cards_message : Types.card list -> string

(** [reveal_message_for_street game] returns the street reveal line when the
    public board has exactly the expected number of cards for the street.
    Requires: [game] has a consistent table street and community-card count. *)
val reveal_message_for_street : Types.game_state -> string option

(** [current_turn_message game] names the player at [game.table.turn_index].
    Requires: [game.players] is nonempty and [game.table.turn_index] is a valid
    index into it. Raises: [Failure] if the game has no players or the turn
    index is past the player list; [Invalid_argument] if the turn index is
    negative. *)
val current_turn_message : Types.game_state -> string

(** [table_setup_messages game] formats the initial dealer, blind, pot, and turn
    lines for a newly dealt hand. Requires: [game.players] contains enough
    players for the dealer and blind indices implied by
    [game.table.dealer_index]. Raises: [Failure] if [game.players] is empty or a
    required seat index is past the player list; [Invalid_argument] if a
    required seat index is negative. *)
val table_setup_messages : Types.game_state -> string list

(** [public_action_message name action] formats the broadcast line for a
    completed player action. Requires: [name] is already trimmed for display.*)
val public_action_message : string -> Types.action -> string

(** [final_standings_messages game] formats the terminal standings after the
    game ends.*)
val final_standings_messages : Types.game_state -> string list

(** [everyone_voted_to_cash_out game votes] is true when every player with chips
    remaining has a matching id in [votes]. Requires: [votes] contains player
    ids.*)
val everyone_voted_to_cash_out : Types.game_state -> int list -> bool
