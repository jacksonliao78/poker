type style = {
  color : bool;
  unicode : bool;
}

val plain : style
val ansi : style
val clear_screen : string
val bold : style -> string -> string
val dim : style -> string -> string
val red : style -> string -> string
val green : style -> string -> string
val yellow : style -> string -> string
val cyan : style -> string -> string
val blue : style -> string -> string
val orange : style -> string -> string
val money : style -> int -> string
val command : style -> string -> string
val render_card : style -> Types.card -> string
val render_cards : style -> Types.card list -> string
val render_street : Types.street -> string
val render_status : Types.player_status -> string
val render_legal_action : style -> Protocol.legal_action -> string
