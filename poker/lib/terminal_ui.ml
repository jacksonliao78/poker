type style = {
  color : bool;
  unicode : bool;
}

let plain = { color = false; unicode = false }
let ansi = { color = true; unicode = true }
let clear_screen = "\027[2J\027[H"

let wrap style code text =
  if style.color then "\027[" ^ code ^ "m" ^ text ^ "\027[0m" else text

let bold style text = wrap style "1" text
let dim style text = wrap style "2" text
let red style text = wrap style "31" text
let green style text = wrap style "32" text
let yellow style text = wrap style "33" text
let cyan style text = wrap style "36" text

let rank_short = function
  | Types.Two -> "2"
  | Three -> "3"
  | Four -> "4"
  | Five -> "5"
  | Six -> "6"
  | Seven -> "7"
  | Eight -> "8"
  | Nine -> "9"
  | Ten -> "10"
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"
  | Ace -> "A"

let suit_symbol = function
  | Types.Hearts -> "♥"
  | Diamonds -> "♦"
  | Clubs -> "♣"
  | Spades -> "♠"

let color_card style suit text =
  match suit with
  | Types.Hearts | Diamonds -> red style text
  | Clubs -> green style text
  | Spades -> cyan style text

let render_card style (card : Types.card) =
  if style.unicode then
    color_card style card.suit (rank_short card.rank ^ suit_symbol card.suit)
  else Cards.card_to_long_string card

let render_cards style cards =
  match cards with
  | [] -> dim style "(none)"
  | cards -> cards |> List.map (render_card style) |> String.concat "  "

let render_street = function
  | Types.Preflop -> "Preflop"
  | Flop -> "Flop"
  | Turn -> "Turn"
  | River -> "River"

let render_status = function
  | Types.Active -> "active"
  | Folded -> "folded"
  | AllIn -> "all-in"

let render_legal_action = function
  | Protocol.Can_fold -> "fold"
  | Can_check -> "check"
  | Can_call amount -> Printf.sprintf "call $%d" amount
  | Can_raise amount -> Printf.sprintf "raise <amount> (min $%d)" amount
