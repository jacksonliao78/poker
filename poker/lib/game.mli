type action_outcome =
  | Next_turn of Types.game_state
  | Betting_round_complete of Types.game_state
  | Hand_complete of Types.game_state * Types.player_state

val draw_community_cards : Types.game_state -> count:int -> Types.game_state
val reset_bets : Types.game_state -> Types.game_state
val postflop_turn_index : Types.game_state -> int option
val advance_street : Types.game_state -> action_outcome
val resolve_showdown : Types.game_state -> Types.player_state list
val start_next_hand : Types.game_state -> Types.game_state
val start : Lobby.player list -> Types.game_state

val apply_action :
  Types.game_state ->
  player_id:int ->
  Types.action ->
  (action_outcome, string) result
