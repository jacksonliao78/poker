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
  let rank_value = function
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Eight -> 8
    | Nine -> 9
    | Ten -> 10
    | Jack -> 11
    | Queen -> 12
    | King -> 13
    | Ace -> 14
  in
  let compare_int_desc left right = Stdlib.compare right left in
  let rec compare_rank_lists left right =
    match (left, right) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | left_head :: left_tail, right_head :: right_tail ->
        let cmp = Stdlib.compare left_head right_head in
        if cmp <> 0 then cmp else compare_rank_lists left_tail right_tail
  in
  let combinations cards =
    let rec pick n source =
      if n = 0 then [ [] ]
      else
        match source with
        | [] -> []
        | head :: tail ->
            let with_head =
              List.map (fun choice -> head :: choice) (pick (n - 1) tail)
            in
            let without_head = pick n tail in
            with_head @ without_head
    in
    pick 5 cards
  in
  let straight_high ranks_desc =
    let unique_desc =
      List.sort_uniq compare_int_desc ranks_desc
    in
    let with_wheel =
      if List.mem 14 unique_desc then unique_desc @ [ 1 ] else unique_desc
    in
    let rec scan = function
      | a :: (b :: (c :: (d :: (e :: _)))) as all_tail ->
          if a = b + 1 && b = c + 1 && c = d + 1 && d = e + 1 then Some a
          else scan (List.tl all_tail)
      | _ -> None
    in
    scan with_wheel
  in
  let evaluate_five cards =
    let ranks_desc =
      cards
      |> List.map (fun card -> rank_value card.rank)
      |> List.sort compare_int_desc
    in
    let rank_counts =
      List.fold_left
        (fun counts rank ->
          let current =
            match List.assoc_opt rank counts with
            | Some count -> count
            | None -> 0
          in
          (rank, current + 1)
          :: List.remove_assoc rank counts)
        [] ranks_desc
    in
    let groups =
      List.sort
        (fun (rank_a, count_a) (rank_b, count_b) ->
          let count_cmp = Stdlib.compare count_b count_a in
          if count_cmp <> 0 then count_cmp else Stdlib.compare rank_b rank_a)
        rank_counts
    in
    let is_flush =
      match cards with
      | [] -> false
      | first :: rest -> List.for_all (fun card -> card.suit = first.suit) rest
    in
    let straight = straight_high ranks_desc in
    match (is_flush, straight, groups) with
    | true, Some 14, _ -> (9, [ 14 ])
    | true, Some high, _ -> (8, [ high ])
    | _, _, [ (quad_rank, 4); (kicker, 1) ] -> (7, [ quad_rank; kicker ])
    | _, _, [ (trip_rank, 3); (pair_rank, 2) ] -> (6, [ trip_rank; pair_rank ])
    | true, None, _ -> (5, ranks_desc)
    | false, Some high, _ -> (4, [ high ])
    | _, _, [ (trip_rank, 3); (kicker_a, 1); (kicker_b, 1) ] ->
        let kickers = List.sort compare_int_desc [ kicker_a; kicker_b ] in
        (3, trip_rank :: kickers)
    | _, _, [ (high_pair, 2); (low_pair, 2); (kicker, 1) ] ->
        let pair_ranks = List.sort compare_int_desc [ high_pair; low_pair ] in
        (2, pair_ranks @ [ kicker ])
    | _, _, [ (pair_rank, 2); (kicker_a, 1); (kicker_b, 1); (kicker_c, 1) ] ->
        let kickers =
          List.sort compare_int_desc [ kicker_a; kicker_b; kicker_c ]
        in
        (1, pair_rank :: kickers)
    | _ -> (0, ranks_desc)
  in
  let compare_scores (category_a, ranks_a) (category_b, ranks_b) =
    let category_cmp = Stdlib.compare category_a category_b in
    if category_cmp <> 0 then category_cmp
    else compare_rank_lists ranks_a ranks_b
  in
  let evaluate_seven cards =
    let best_five_hands = combinations cards in
    match best_five_hands with
    | [] -> (0, [])
    | first :: rest ->
        List.fold_left
          (fun best hand ->
            let score = evaluate_five hand in
            if compare_scores score best > 0 then score else best)
          (evaluate_five first) rest
  in
  let contenders =
    List.filter (fun player -> player.status <> Folded) game.players
  in
  let hand_score player =
    let all_cards = List.concat [ player.hole_cards; game.table.community_cards ] in
    evaluate_seven all_cards
  in
  match contenders with
  | [] -> []
  | first :: rest ->
      let best =
        List.fold_left
          (fun current player ->
            let score = hand_score player in
            if compare_scores score current > 0 then score else current)
          (hand_score first) rest
      in
      List.filter
        (fun player -> compare_scores (hand_score player) best = 0)
        contenders

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

let round_start_index game =
  let player_count = List.length game.players in
  let nominal_start =
    match game.table.street with
    | Preflop -> (game.table.dealer_index + 3) mod player_count
    | Flop | Turn | River -> (game.table.dealer_index + 1) mod player_count
  in
  match next_active_index game.players nominal_start with
  | Some index -> index
  | None -> nominal_start

let active_players players =
  List.filter (fun player -> player.status <> Folded) players

let betting_round_complete players current_bet =
  List.for_all
    (fun player ->
      player.status = Folded || player.status = AllIn
      || player.round_bet = current_bet)
    players

let big_blind_index game =
  let player_count = List.length game.players in
  (game.table.dealer_index + 2) mod player_count

(* In preflop with no raise beyond the posted big blind, action must return to
   the big blind for their option before the street can close. *)
let preflop_big_blind_option_pending game next_index =
  game.table.street = Preflop
  && game.table.current_bet = game.big_blind
  &&
  let bb = List.nth game.players (big_blind_index game) in
  bb.status = Active
  &&
  let bb_next =
    next_active_index game.players (big_blind_index game)
  in
  bb_next = Some (big_blind_index game)
  &&
  match next_index with
  | Some index -> index = big_blind_index game
  | None -> false

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
  let player_count = List.length game.players in
  let next_index =
    next_active_index game.players
      ((game.table.turn_index + 1) mod player_count)
  in
  let zero_bet_round_closed =
    if game.table.current_bet <> 0 then false
    else
      let start_index = round_start_index game in
      match next_index with
      | Some index -> index = start_index
      | None -> true
  in
  if List.length remaining_players = 1 then
    Hand_complete (game, List.hd remaining_players)
  else if
    betting_round_complete game.players game.table.current_bet
    && not (preflop_big_blind_option_pending game next_index)
    && (game.table.current_bet <> 0 || zero_bet_round_closed)
  then
    advance_street game
  else
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
