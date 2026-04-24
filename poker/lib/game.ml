open Types

type action_outcome =
  | Next_turn of game_state
  | Betting_round_complete of game_state
  | Hand_complete of game_state * player_state

(* Draws some ammount of cards (for flop/turn/river) *)
let draw_community_cards game ~count =
  let drawn, deck = Cards.deal_n count game.deck in
  let community_cards = List.concat [ game.table.community_cards; drawn ] in
  let table = { game.table with community_cards } in
  { game with deck; table }

(* Clears the bets after each street *)
let reset_bets game =
  let players =
    List.map (fun player -> { player with round_bet = 0 }) game.players
  in
  let table = { game.table with current_bet = 0; min_raise = game.big_blind } in
  { game with players; table }

let reset_betting_round = reset_bets

(* Finds first actor for flop/turn/river. *)
let postflop_turn_index game =
  let player_count = List.length game.players in
  let start_index = (game.table.dealer_index + 1) mod player_count in
  let rec search steps index =
    if steps >= player_count then None
    else
      let player = List.nth game.players index in
      if player.status = Active then Some index
      else search (steps + 1) ((index + 1) mod player_count)
  in
  search 0 start_index

(* Advances the game after actions *)
let rec advance_street game =
  let active_players players =
    List.filter (fun player -> player.status <> Folded) players
  in
  let move_to next_street draw_count =
    let game = draw_community_cards game ~count:draw_count in
    let game = reset_bets game in
    match postflop_turn_index game with
    | None ->
        let winners = resolve_showdown game in
        let winner =
          match winners with
          | winner :: _ -> winner
          | [] -> List.hd (active_players game.players)
        in
        Hand_complete (game, winner)
    | Some turn_index ->
        Next_turn
          {
            game with
            table = { game.table with street = next_street; turn_index };
          }
  in
  match game.table.street with
  | Preflop -> move_to Flop 3
  | Flop -> move_to Turn 1
  | Turn -> move_to River 1
  | River ->
      let winners = resolve_showdown game in
      let winner =
        match winners with
        | winner :: _ -> winner
        | [] -> List.hd (active_players game.players)
      in
      Hand_complete (game, winner)

(* Determines winners after the river *)
and resolve_showdown game =
  let _ = game in
  failwith "TODO"

let deal_hole_cards players deck =
  let player_count = List.length players in
  let first_pass, deck = Cards.deal_n player_count deck in
  let second_pass, deck = Cards.deal_n player_count deck in
  let hole_cards =
    List.map2 (fun first second -> [ first; second ]) first_pass second_pass
  in
  (hole_cards, deck)

let player_state_of_lobby_player player hole_cards =
  {
    id = player.Lobby.id;
    name = player.name;
    controller = Human;
    chips = player.chips;
    hole_cards;
    round_bet = 0;
    status = Active;
  }

let post_blind amount player =
  let committed = min amount player.chips in
  let remaining_chips = player.chips - committed in
  let status = if remaining_chips = 0 then AllIn else player.status in
  ( { player with chips = remaining_chips; round_bet = committed; status },
    committed )

let update_player_at index f players =
  List.mapi (fun i player -> if i = index then f player else player) players

let next_active_index players start_index =
  let player_count = List.length players in
  let rec search steps index =
    if steps >= player_count then None
    else
      let player = List.nth players index in
      if player.status = Active then Some index
      else search (steps + 1) ((index + 1) mod player_count)
  in
  search 0 start_index

let active_players players =
  List.filter (fun player -> player.status <> Folded) players

let betting_round_complete players current_bet =
  List.for_all
    (fun player ->
      player.status = Folded || player.status = AllIn
      || player.round_bet = current_bet)
    players

let commit_chips player amount =
  let committed = min amount player.chips in
  let chips = player.chips - committed in
  let status = if chips = 0 then AllIn else player.status in
  ( { player with chips; round_bet = player.round_bet + committed; status },
    committed )

let apply_to_player game player_id f =
  match List.find_opt (fun player -> player.id = player_id) game.players with
  | None -> Error "Unknown player."
  | Some current_player -> (
      let current_index = game.table.turn_index in
      let table_player = List.nth game.players current_index in
      if table_player.id <> player_id then Error "It is not your turn."
      else if current_player.status <> Active then
        Error "You cannot act right now."
      else
        try
          let updated_player, table_delta = f current_player game.table in
          let players =
            List.map
              (fun player ->
                if player.id = player_id then updated_player else player)
              game.players
          in
          Ok (players, table_delta)
        with Invalid_argument message -> Error message)

let advance_after_action game =
  let remaining_players = active_players game.players in
  if List.length remaining_players = 1 then
    Hand_complete (game, List.hd remaining_players)
  else if betting_round_complete game.players game.table.current_bet then
    advance_street game
  else
    let next_index =
      next_active_index game.players
        ((game.table.turn_index + 1) mod List.length game.players)
    in
    match next_index with
    | None -> Betting_round_complete game
    | Some turn_index ->
        Next_turn { game with table = { game.table with turn_index } }

let start players =
  let deck = Cards.full_deck |> Cards.shuffle in
  let hole_cards_by_player, deck = deal_hole_cards players deck in
  let players =
    List.map2 player_state_of_lobby_player players hole_cards_by_player
  in
  let player_count = List.length players in
  let dealer_index = 0 in
  let small_blind_index = (dealer_index + 1) mod player_count in
  let big_blind_index = (dealer_index + 2) mod player_count in
  let small_blind_player = List.nth players small_blind_index in
  let small_blind_player, small_blind =
    post_blind Types.default_config.small_blind small_blind_player
  in
  let players =
    update_player_at small_blind_index (fun _ -> small_blind_player) players
  in
  let big_blind_player = List.nth players big_blind_index in
  let big_blind_player, big_blind =
    post_blind Types.default_config.big_blind big_blind_player
  in
  let players =
    update_player_at big_blind_index (fun _ -> big_blind_player) players
  in
  let current_bet = max small_blind big_blind in
  let turn_index =
    match
      next_active_index players ((big_blind_index + 1) mod player_count)
    with
    | Some index -> index
    | None -> big_blind_index
  in
  let table =
    {
      community_cards = [];
      pot = small_blind + big_blind;
      current_bet;
      min_raise = Types.default_config.big_blind;
      dealer_index;
      turn_index;
      street = Preflop;
    }
  in
  {
    players;
    deck;
    table;
    small_blind = Types.default_config.small_blind;
    big_blind = Types.default_config.big_blind;
  }

(* Starts next hand. *)
let start_next_hand game =
  let lobby_players =
    List.map
      (fun player ->
        {
          Lobby.id = player.id;
          name = player.name;
          chips = player.chips;
          connected = true;
        })
      game.players
  in
  start lobby_players

let apply_action game ~player_id action =
  let to_call player table = max 0 (table.current_bet - player.round_bet) in
  let result =
    match action with
    | Fold ->
        apply_to_player game player_id (fun player _table ->
            ({ player with status = Folded }, fun table -> table))
    | Check ->
        apply_to_player game player_id (fun player table ->
            if to_call player table <> 0 then
              raise (Invalid_argument "Cannot check when facing a bet.")
            else (player, fun current_table -> current_table))
    | Call ->
        apply_to_player game player_id (fun player table ->
            let call_amount = to_call player table in
            let player, committed = commit_chips player call_amount in
            ( player,
              fun current_table ->
                { current_table with pot = current_table.pot + committed } ))
    | Raise raise_amount ->
        apply_to_player game player_id (fun player table ->
            let call_amount = to_call player table in
            if raise_amount < table.min_raise then
              raise
                (Invalid_argument "Raise is smaller than the minimum raise.")
            else if player.chips < call_amount + raise_amount then
              raise (Invalid_argument "You do not have enough chips to raise.")
            else
              let player, committed =
                commit_chips player (call_amount + raise_amount)
              in
              ( player,
                fun current_table ->
                  {
                    current_table with
                    pot = current_table.pot + committed;
                    current_bet = player.round_bet;
                    min_raise = raise_amount;
                  } ))
    | Bet _ -> Error "Bet is not supported yet."
  in
  match result with
  | Error _ as error -> error
  | Ok (players, table_update) -> (
      try
        let table = table_update game.table in
        let game = { game with players; table } in
        Ok (advance_after_action game)
      with Invalid_argument message -> Error message)
