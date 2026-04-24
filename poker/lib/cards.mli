open Types

(** All suits in deck-construction order. *)
val all_suits : suit list

(** All ranks in ascending order. *)
val all_ranks : rank list

(** A full standard 52-card deck before shuffling. *)
val full_deck : card list

(** Textual rendering of card. *)
val card_to_string : card -> string

(** Verbose textual rendering of card. *)
val card_to_long_string : card -> string

(** Returns the first [n] cards and the remaining deck. *)
val deal_n : int -> card list -> card list * card list

(** Returns a randomized permutation of [deck]. *)
val shuffle : card list -> card list
