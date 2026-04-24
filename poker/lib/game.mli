type action_outcome =
  | Next_turn of Types.game_state
  | Betting_round_complete of Types.game_state
  | Hand_complete of Types.game_state * Types.player_state

val start : Lobby.player list -> Types.game_state

val apply_action :
  Types.game_state ->
  player_id:int ->
  Types.action ->
  (action_outcome, string) result
