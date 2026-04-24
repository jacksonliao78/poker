open Types

let () = Random.self_init ()
let all_suits = [ Hearts; Diamonds; Clubs; Spades ]

let all_ranks =
  [
    Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace;
  ]

let full_deck : card list =
  List.concat_map
    (fun suit -> List.map (fun rank -> { rank; suit }) all_ranks)
    all_suits

let card_to_string (card : card) : string =
  let rank_str =
    match card.rank with
    | Two -> "2"
    | Three -> "3"
    | Four -> "4"
    | Five -> "5"
    | Six -> "6"
    | Seven -> "7"
    | Eight -> "8"
    | Nine -> "9"
    | Ten -> "T"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | Ace -> "A"
  in
  let suit_str =
    match card.suit with
    | Hearts -> "h"
    | Diamonds -> "d"
    | Clubs -> "c"
    | Spades -> "s"
  in
  rank_str ^ suit_str

let card_to_long_string (card : card) : string =
  let rank_str =
    match card.rank with
    | Two -> "2"
    | Three -> "3"
    | Four -> "4"
    | Five -> "5"
    | Six -> "6"
    | Seven -> "7"
    | Eight -> "8"
    | Nine -> "9"
    | Ten -> "10"
    | Jack -> "Jack"
    | Queen -> "Queen"
    | King -> "King"
    | Ace -> "Ace"
  in
  let suit_str =
    match card.suit with
    | Hearts -> "Hearts"
    | Diamonds -> "Diamonds"
    | Clubs -> "Clubs"
    | Spades -> "Spades"
  in
  rank_str ^ " of " ^ suit_str

let take_n n xs =
  let rec aux i acc rest =
    if i <= 0 then (List.rev acc, rest)
    else
      match rest with
      | [] -> (List.rev acc, [])
      | x :: tl -> aux (i - 1) (x :: acc) tl
  in
  aux n [] xs

let deal_n n deck = take_n n deck

let shuffle (deck : card list) : card list =
  let arr = Array.of_list deck in
  let last_index = Array.length arr - 1 in
  for i = last_index downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr
