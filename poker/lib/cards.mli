open Types

(** [all_suits] lists every suit in deck-construction order. *)
val all_suits : suit list

(** [all_ranks] lists every rank in ascending order. *)
val all_ranks : rank list

(** [full_deck] is a standard 52-card deck before shuffling. *)
val full_deck : card list

(** [card_to_string card] renders [card] in compact protocol-friendly form. *)
val card_to_string : card -> string

(** [card_to_long_string card] renders [card] in display-friendly English. *)
val card_to_long_string : card -> string

(** [deal_n n deck] returns the first [n] cards and the remaining deck. If
    [deck] has fewer than [n] cards, all available cards are dealt. *)
val deal_n : int -> card list -> card list * card list

(** [shuffle deck] returns a randomized permutation of [deck]. *)
val shuffle : card list -> card list
