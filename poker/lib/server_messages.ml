let trim_name raw_name =
  let trimmed = String.trim raw_name in
  if trimmed = "" then None
  else
    let max_len = Protocol.max_name_length in
    let length = min (String.length trimmed) max_len in
    Some (String.sub trimmed 0 length)

let hole_cards_message cards =
  cards
  |> List.map Cards.card_to_long_string
  |> String.concat " and "
  |> Printf.sprintf "Your hole cards: %s"

let community_cards_message cards =
  cards
  |> List.map Cards.card_to_long_string
  |> String.concat ", " |> Printf.sprintf "Board: %s"

let reveal_message_for_street (game : Types.game_state) =
  let cards = game.table.community_cards in
  match (game.table.street, List.length cards) with
  | Flop, 3 ->
      Some (Printf.sprintf "Flop revealed. %s" (community_cards_message cards))
  | Turn, 4 ->
      Some (Printf.sprintf "Turn revealed. %s" (community_cards_message cards))
  | River, 5 ->
      Some (Printf.sprintf "River revealed. %s" (community_cards_message cards))
  | _ -> None

let player_name_at players index = (List.nth players index).Types.name

let current_turn_message (game : Types.game_state) =
  let current_player = List.nth game.Types.players game.table.turn_index in
  Printf.sprintf "Player's turn: %s" current_player.name

let table_setup_messages (game : Types.game_state) =
  let players = game.Types.players in
  let dealer_name = player_name_at players game.table.dealer_index in
  let small_blind_index =
    (game.table.dealer_index + 1) mod List.length players
  in
  let big_blind_index = (game.table.dealer_index + 2) mod List.length players in
  let small_blind_player = List.nth players small_blind_index in
  let big_blind_player = List.nth players big_blind_index in
  let blind_message label amount player =
    if player.Types.round_bet < amount then
      Printf.sprintf "%s: %s is all-in for $%d" label player.name
        player.round_bet
    else Printf.sprintf "%s: %s posts $%d" label player.name amount
  in
  [
    Printf.sprintf "Dealer: %s" dealer_name;
    blind_message "Small blind" game.small_blind small_blind_player;
    blind_message "Big blind" game.big_blind big_blind_player;
    Printf.sprintf "Pot: $%d. Current bet: $%d." game.table.pot
      game.table.current_bet;
    current_turn_message game;
  ]

let public_action_message name action =
  match action with
  | Types.Fold -> Printf.sprintf "%s folds." name
  | Check -> Printf.sprintf "%s checks." name
  | Call -> Printf.sprintf "%s calls." name
  | Raise amount -> Printf.sprintf "%s raises by $%d." name amount
  | Bet amount -> Printf.sprintf "%s bets $%d." name amount

let final_standings_messages (_game : Types.game_state) = [ "Game over." ]

let everyone_voted_to_cash_out (game : Types.game_state) votes =
  let surviving = List.filter (fun p -> p.Types.chips > 0) game.players in
  match surviving with
  | [] -> false
  | _ -> List.for_all (fun p -> List.mem p.Types.id votes) surviving
