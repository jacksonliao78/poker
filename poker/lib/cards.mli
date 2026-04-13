open Types

val all_suits : suit list
val all_ranks : rank list
val full_deck : card list
val card_to_string : card -> string
val deal_n : int -> card list -> card list * card list
val shuffle : card list -> card list
