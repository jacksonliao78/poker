(** Display style of the terminal. *)
type style = {
  color : bool;  (** Whether ANSI escape sequences should be emitted. *)
  unicode : bool;  (** Whether cards should use compact Unicode suit symbols. *)
}

(** [plain] renders without ANSI color or Unicode symbols. *)
val plain : style

(** [ansi] renders with ANSI color and Unicode symbols for a modern terminal. *)
val ansi : style

(** [clear_screen] is an ANSI sequence that clears scrollback and resets the
    cursor for full-screen redraws. *)
val clear_screen : string

(** [bold style text] renders [text] in bold when [style.color] permits it. *)
val bold : style -> string -> string

(** [dim style text] renders [text] dimmed when [style.color] permits it. *)
val dim : style -> string -> string

(** [red style text] renders [text] red when [style.color] permits it. *)
val red : style -> string -> string

(** [green style text] renders [text] green when [style.color] permits it. *)
val green : style -> string -> string

(** [yellow style text] renders [text] yellow when [style.color] permits it. *)
val yellow : style -> string -> string

(** [cyan style text] renders [text] cyan when [style.color] permits it. *)
val cyan : style -> string -> string

(** [blue style text] renders [text] blue when [style.color] permits it. *)
val blue : style -> string -> string

(** [orange style text] renders [text] orange when [style.color] permits it. *)
val orange : style -> string -> string

(** [money style amount] renders a chip amount with the table's money styling.
*)
val money : style -> int -> string

(** [command style text] renders a command token with command styling. *)
val command : style -> string -> string

(** [render_card style card] renders [card] for the terminal. *)
val render_card : style -> Types.card -> string

(** [render_cards style cards] renders a space-separated card list, or a dim
    placeholder for an empty list. *)
val render_cards : style -> Types.card list -> string

(** [render_street street] renders a betting street label. *)
val render_street : Types.street -> string

(** [render_status status] renders a player status label. *)
val render_status : Types.player_status -> string

(** [render_legal_action style action] renders one legal action hint. Requires:
    [action] comes from the current player view. *)
val render_legal_action : style -> Protocol.legal_action -> string
