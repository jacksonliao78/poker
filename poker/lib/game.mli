(** Action that happens at the end of an event. *)
type action_outcome =
  | Next_turn of Types.game_state
      (** Action passed to the next active player without leaving the street. *)
  | Betting_round_complete of Types.game_state
      (** Current betting round closed without selecting a hand winner yet. *)
  | Hand_complete of Types.game_state * Types.player_state
      (** Hand ended with the final state and the winning player selected for
          pot award/broadcast. *)

(** [draw_community_cards game ~count] moves up to [count] cards from the deck
    to the public board. Requires: [count] is nonnegative and [game.deck] is
    ordered with the next card at the head. *)
val draw_community_cards : Types.game_state -> count:int -> Types.game_state

(** [reset_bets game] clears per-street bets and restores the minimum raise for
    the next street. *)
val reset_bets : Types.game_state -> Types.game_state

(** [postflop_turn_index game] returns the first active seat after the dealer.
    Requires: [game.players] is nonempty and [game.table.dealer_index] is a
    valid index into it. Raises: [Division_by_zero] if [game.players] is empty;
    [Failure] if the dealer-derived search indexes do not match the player list.
*)
val postflop_turn_index : Types.game_state -> int option

(** [advance_street game] advances from the current street, dealing board cards
    as needed or resolving the river showdown. Requires: [game] has a nonempty
    player list, a valid dealer index, enough deck cards for the next street,
    and a community-card count consistent with the current street. Raises:
    [Failure] if an expected active player is missing from malformed state;
    [Division_by_zero] if [game.players] is empty. *)
val advance_street : Types.game_state -> action_outcome

(** [resolve_showdown game] returns every non-folded player tied for the best
    five-card hand. Requires: each contender has enough private plus community
    cards to form a meaningful poker hand. *)
val resolve_showdown : Types.game_state -> Types.player_state list

(** [start_next_hand game] starts a fresh hand from every current player without
    eliminating broke players or rotating around only surviving seats. Requires:
    [game.players] contains enough players for blind posting. Raises:
    [Division_by_zero] if [game.players] is empty; [Failure] if the derived
    blind indexes are invalid. *)
val start_next_hand : Types.game_state -> Types.game_state

(** [award_pot game ~winner_id] moves the current pot into the winner's stack,
    zeroes the pot, and marks any player reduced to zero chips as [Out].
    Requires: [winner_id] names a player in [game] if the pot should be paid
    out. *)
val award_pot : Types.game_state -> winner_id:int -> Types.game_state

(** [next_hand game] starts the next hand, keeping all seats in their original
    order. Players with chips remaining are dealt cards and set [Active];
    players with zero chips are set [Out] with empty hole cards so the UI can
    show who was eliminated. Returns [None] if zero or one player has chips
    left. Requires: surviving player ids are unique and
    [game.table.dealer_index] is valid for [game.players]. Raises:
    [Division_by_zero] if malformed state reaches the hand starter with no
    players; [Failure] if a derived seat index is invalid. *)
val next_hand : Types.game_state -> Types.game_state option

(** [start players] creates and deals the first hand for lobby [players].
    Requires: [players] contains enough seats to post both blinds. Raises:
    [Division_by_zero] if [players] is empty; [Failure] if the derived blind
    indexes are invalid. *)
val start : Lobby.player list -> Types.game_state

(** [apply_action game ~player_id action] applies [action] for the current turn
    player and advances the hand when the betting state closes. Invalid player
    actions are returned as [Error message] so the server can report them
    without crashing. Requires: [game.players] is nonempty,
    [game.table.turn_index] is valid, and [player_id] comes from the active
    game. Raises: [Failure] or [Invalid_argument] if [game] itself is malformed.
*)
val apply_action :
  Types.game_state ->
  player_id:int ->
  Types.action ->
  (action_outcome, string) result
