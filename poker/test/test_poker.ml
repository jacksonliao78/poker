open OUnit2

let test_default_config _ =
  assert_equal 500 Poker.Types.default_config.starting_chips

let test_lobby_add_player _ =
  let lobby, player = Poker.Lobby.add_player (Poker.Lobby.empty ()) in
  let snapshot = Poker.Lobby.snapshot lobby in
  assert_equal 1 player.id;
  assert_equal 500 player.chips;
  assert_equal 1 (List.length snapshot.Poker.Protocol.players);
  assert_equal 3 snapshot.seats_open

let compare_card left right =
  Stdlib.compare
    (Poker.Cards.card_to_string left)
    (Poker.Cards.card_to_string right)

let test_shuffle_preserves_deck _ =
  let original = Poker.Cards.full_deck in
  let shuffled = Poker.Cards.shuffle original in
  assert_equal 52 (List.length shuffled);
  assert_equal
    (List.sort compare_card original)
    (List.sort compare_card shuffled)

let test_game_start_deals_two_cards_each _ =
  let lobby = Poker.Lobby.empty () in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let game = Poker.Game.start (Poker.Lobby.players lobby) in
  assert_equal 4 (List.length game.Poker.Types.players);
  assert_bool "each player has two cards"
    (List.for_all
       (fun player -> List.length player.Poker.Types.hole_cards = 2)
       game.players);
  assert_equal 44 (List.length game.deck)

let test_game_start_posts_blinds_and_sets_turn _ =
  let lobby = Poker.Lobby.empty () in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let game = Poker.Game.start (Poker.Lobby.players lobby) in
  let players = game.Poker.Types.players in
  let table = game.table in
  assert_equal 0 table.dealer_index;
  assert_equal 3 table.turn_index;
  assert_equal 15 table.pot;
  assert_equal 10 table.current_bet;
  assert_equal 500 (List.nth players 0).chips;
  assert_equal 495 (List.nth players 1).chips;
  assert_equal 5 (List.nth players 1).round_bet;
  assert_equal 490 (List.nth players 2).chips;
  assert_equal 10 (List.nth players 2).round_bet;
  assert_equal 500 (List.nth players 3).chips

let test_call_matches_current_bet_and_advances_turn _ =
  let lobby = Poker.Lobby.empty () in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let game = Poker.Game.start (Poker.Lobby.players lobby) in
  match
    Poker.Game.apply_action game
      ~player_id:(List.nth game.Poker.Types.players 3).id Poker.Types.Call
  with
  | Error message -> assert_failure message
  | Ok (Poker.Game.Next_turn next_game) ->
      assert_equal 10 (List.nth next_game.players 3).round_bet;
      assert_equal 490 (List.nth next_game.players 3).chips;
      assert_equal 25 next_game.table.pot;
      assert_equal 0 next_game.table.turn_index
  | Ok _ -> assert_failure "expected next turn after call"

let test_fold_marks_player_folded _ =
  let lobby = Poker.Lobby.empty () in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let game = Poker.Game.start (Poker.Lobby.players lobby) in
  match
    Poker.Game.apply_action game
      ~player_id:(List.nth game.Poker.Types.players 3).id Poker.Types.Fold
  with
  | Error message -> assert_failure message
  | Ok (Poker.Game.Next_turn next_game) ->
      assert_equal Poker.Types.Folded (List.nth next_game.players 3).status;
      assert_equal 0 next_game.table.turn_index
  | Ok _ -> assert_failure "expected next turn after fold"

let test_check_while_facing_bet_returns_error _ =
  let lobby = Poker.Lobby.empty () in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let game = Poker.Game.start (Poker.Lobby.players lobby) in
  match
    Poker.Game.apply_action game ~player_id:(List.nth game.Poker.Types.players 3).id
      Poker.Types.Check
  with
  | Error message -> assert_equal "Cannot check when facing a bet." message
  | Ok _ -> assert_failure "expected check to fail while facing a bet"

let make_four_player_game () =
  let lobby = Poker.Lobby.empty () in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  Poker.Game.start (Poker.Lobby.players lobby)

let apply_ok game player_index action =
  match
    Poker.Game.apply_action game
      ~player_id:(List.nth game.Poker.Types.players player_index).id action
  with
  | Error message -> assert_failure message
  | Ok outcome -> outcome

let test_preflop_to_flop _ =
  let game = make_four_player_game () in
  let game =
    match apply_ok game 3 Poker.Types.Call with
    | Poker.Game.Next_turn game -> game
    | _ -> assert_failure "expected next_turn after UTG preflop call"
  in
  let game =
    match apply_ok game 0 Poker.Types.Call with
    | Poker.Game.Next_turn game -> game
    | _ -> assert_failure "expected next_turn after dealer preflop call"
  in
  let game =
    match apply_ok game 1 Poker.Types.Call with
    | Poker.Game.Next_turn game ->
        assert_equal Poker.Types.Preflop game.table.street;
        assert_equal 2 game.table.turn_index;
        game
    | _ -> assert_failure "expected big blind option after small blind call"
  in
  match apply_ok game 2 Poker.Types.Check with
  | Poker.Game.Next_turn game ->
      assert_equal Poker.Types.Flop game.table.street;
      assert_equal 3 (List.length game.table.community_cards);
      assert_bool "hole cards stay private in player state"
        (List.for_all
           (fun player -> List.length player.Poker.Types.hole_cards = 2)
           game.players)
  | _ -> assert_failure "expected transition to flop after big blind option"

let test_preflop_big_blind_can_raise_option _ =
  let game = make_four_player_game () in
  let game =
    match apply_ok game 3 Poker.Types.Call with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected next_turn"
  in
  let game =
    match apply_ok game 0 Poker.Types.Call with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected next_turn"
  in
  let game =
    match apply_ok game 1 Poker.Types.Call with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected big blind option"
  in
  match apply_ok game 2 (Poker.Types.Raise 10) with
  | Poker.Game.Next_turn game ->
      assert_equal Poker.Types.Preflop game.table.street;
      assert_equal 20 game.table.current_bet;
      assert_equal 50 game.table.pot;
      assert_equal 3 game.table.turn_index
  | _ -> assert_failure "expected preflop to continue after big blind raises"

let test_checkdown_to_river _ =
  let game = make_four_player_game () in
  let game =
    match apply_ok game 3 Poker.Types.Call with Poker.Game.Next_turn g -> g | _ -> assert_failure "expected next_turn"
  in
  let game =
    match apply_ok game 0 Poker.Types.Call with Poker.Game.Next_turn g -> g | _ -> assert_failure "expected next_turn"
  in
  let game =
    match apply_ok game 1 Poker.Types.Call with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected big blind option"
  in
  let game =
    match apply_ok game 2 Poker.Types.Check with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected flop transition"
  in
  let game =
    match apply_ok game 1 Poker.Types.Check with Poker.Game.Next_turn g -> g | _ -> assert_failure "expected flop next_turn"
  in
  let game =
    match apply_ok game 2 Poker.Types.Check with Poker.Game.Next_turn g -> g | _ -> assert_failure "expected flop next_turn"
  in
  let game =
    match apply_ok game 3 Poker.Types.Check with Poker.Game.Next_turn g -> g | _ -> assert_failure "expected flop next_turn"
  in
  let game =
    match apply_ok game 0 Poker.Types.Check with Poker.Game.Next_turn g -> g | _ -> assert_failure "expected turn transition"
  in
  assert_equal Poker.Types.Turn game.table.street;
  assert_equal 4 (List.length game.table.community_cards);
  let game =
    match apply_ok game 1 Poker.Types.Check with Poker.Game.Next_turn g -> g | _ -> assert_failure "expected turn next_turn"
  in
  let game =
    match apply_ok game 2 Poker.Types.Check with Poker.Game.Next_turn g -> g | _ -> assert_failure "expected turn next_turn"
  in
  let game =
    match apply_ok game 3 Poker.Types.Check with Poker.Game.Next_turn g -> g | _ -> assert_failure "expected turn next_turn"
  in
  match apply_ok game 0 Poker.Types.Check with
  | Poker.Game.Next_turn game ->
      assert_equal Poker.Types.River game.table.street;
      assert_equal 5 (List.length game.table.community_cards)
  | _ -> assert_failure "expected river transition"

let card rank suit = { Poker.Types.rank; suit }

let player ?(status = Poker.Types.Active) id hole_cards =
  {
    Poker.Types.id;
    name = Printf.sprintf "P%d" id;
    controller = Poker.Types.Human;
    chips = 100;
    hole_cards;
    round_bet = 0;
    status;
  }

let showdown_game ~players ~board =
  let table =
    {
      Poker.Types.community_cards = board;
      pot = 40;
      current_bet = 0;
      min_raise = 10;
      dealer_index = 0;
      turn_index = 0;
      street = Poker.Types.River;
    }
  in
  {
    Poker.Types.players;
    deck = [];
    table;
    small_blind = 5;
    big_blind = 10;
  }

let winner_ids winners =
  winners
  |> List.map (fun p -> p.Poker.Types.id)
  |> List.sort Stdlib.compare

let test_showdown_flush_beats_straight _ =
  let board =
    [
      card Poker.Types.Ace Poker.Types.Hearts;
      card Poker.Types.King Poker.Types.Hearts;
      card Poker.Types.Seven Poker.Types.Hearts;
      card Poker.Types.Six Poker.Types.Clubs;
      card Poker.Types.Five Poker.Types.Diamonds;
    ]
  in
  let p1 =
    player 1
      [
        card Poker.Types.Three Poker.Types.Hearts;
        card Poker.Types.Two Poker.Types.Hearts;
      ]
  in
  let p2 =
    player 2
      [
        card Poker.Types.Nine Poker.Types.Spades;
        card Poker.Types.Eight Poker.Types.Clubs;
      ]
  in
  let winners = Poker.Game.resolve_showdown (showdown_game ~players:[ p1; p2 ] ~board) in
  assert_equal [ 1 ] (winner_ids winners)

let test_showdown_full_house_beats_flush _ =
  let board =
    [
      card Poker.Types.Ace Poker.Types.Hearts;
      card Poker.Types.Ace Poker.Types.Diamonds;
      card Poker.Types.Ace Poker.Types.Clubs;
      card Poker.Types.King Poker.Types.Hearts;
      card Poker.Types.Two Poker.Types.Hearts;
    ]
  in
  let p1 =
    player 1
      [
        card Poker.Types.King Poker.Types.Diamonds;
        card Poker.Types.King Poker.Types.Spades;
      ]
  in
  let p2 =
    player 2
      [
        card Poker.Types.Queen Poker.Types.Hearts;
        card Poker.Types.Jack Poker.Types.Hearts;
      ]
  in
  let winners = Poker.Game.resolve_showdown (showdown_game ~players:[ p1; p2 ] ~board) in
  assert_equal [ 1 ] (winner_ids winners)

let test_showdown_pair_kicker_breaks_tie _ =
  let board =
    [
      card Poker.Types.Ace Poker.Types.Clubs;
      card Poker.Types.Seven Poker.Types.Diamonds;
      card Poker.Types.Four Poker.Types.Spades;
      card Poker.Types.Two Poker.Types.Clubs;
      card Poker.Types.Nine Poker.Types.Hearts;
    ]
  in
  let p1 =
    player 1
      [
        card Poker.Types.Ace Poker.Types.Diamonds;
        card Poker.Types.King Poker.Types.Diamonds;
      ]
  in
  let p2 =
    player 2
      [
        card Poker.Types.Ace Poker.Types.Spades;
        card Poker.Types.Queen Poker.Types.Diamonds;
      ]
  in
  let winners = Poker.Game.resolve_showdown (showdown_game ~players:[ p1; p2 ] ~board) in
  assert_equal [ 1 ] (winner_ids winners)

let test_showdown_board_tie_splits _ =
  let board =
    [
      card Poker.Types.Ace Poker.Types.Spades;
      card Poker.Types.King Poker.Types.Spades;
      card Poker.Types.Queen Poker.Types.Spades;
      card Poker.Types.Jack Poker.Types.Spades;
      card Poker.Types.Ten Poker.Types.Spades;
    ]
  in
  let p1 =
    player 1
      [
        card Poker.Types.Two Poker.Types.Clubs;
        card Poker.Types.Three Poker.Types.Diamonds;
      ]
  in
  let p2 =
    player 2
      [
        card Poker.Types.Four Poker.Types.Clubs;
        card Poker.Types.Five Poker.Types.Diamonds;
      ]
  in
  let winners = Poker.Game.resolve_showdown (showdown_game ~players:[ p1; p2 ] ~board) in
  assert_equal [ 1; 2 ] (winner_ids winners)

let test_showdown_ace_low_straight _ =
  let board =
    [
      card Poker.Types.Four Poker.Types.Clubs;
      card Poker.Types.Three Poker.Types.Diamonds;
      card Poker.Types.Two Poker.Types.Hearts;
      card Poker.Types.King Poker.Types.Spades;
      card Poker.Types.Nine Poker.Types.Clubs;
    ]
  in
  let p1 =
    player 1
      [
        card Poker.Types.Ace Poker.Types.Diamonds;
        card Poker.Types.Five Poker.Types.Spades;
      ]
  in
  let p2 =
    player 2
      [
        card Poker.Types.Ace Poker.Types.Clubs;
        card Poker.Types.King Poker.Types.Diamonds;
      ]
  in
  let winners = Poker.Game.resolve_showdown (showdown_game ~players:[ p1; p2 ] ~board) in
  assert_equal [ 1 ] (winner_ids winners)

let tests =
  "poker"
  >::: [
         "default_config_uses_500" >:: test_default_config;
         "lobby_add_player_updates_snapshot" >:: test_lobby_add_player;
         "shuffle_preserves_full_deck" >:: test_shuffle_preserves_deck;
         "game_start_deals_two_cards_each"
         >:: test_game_start_deals_two_cards_each;
         "game_start_posts_blinds_and_sets_turn"
         >:: test_game_start_posts_blinds_and_sets_turn;
         "call_matches_current_bet_and_advances_turn"
         >:: test_call_matches_current_bet_and_advances_turn;
         "fold_marks_player_folded" >:: test_fold_marks_player_folded;
         "check_while_facing_bet_returns_error"
         >:: test_check_while_facing_bet_returns_error;
         "preflop_to_flop" >:: test_preflop_to_flop;
         "preflop_big_blind_can_raise_option"
         >:: test_preflop_big_blind_can_raise_option;
         "checkdown_to_river" >:: test_checkdown_to_river;
         "showdown_flush_beats_straight" >:: test_showdown_flush_beats_straight;
         "showdown_full_house_beats_flush"
         >:: test_showdown_full_house_beats_flush;
         "showdown_pair_kicker_breaks_tie"
         >:: test_showdown_pair_kicker_breaks_tie;
         "showdown_board_tie_splits" >:: test_showdown_board_tie_splits;
         "showdown_ace_low_straight" >:: test_showdown_ace_low_straight;
       ]

let () = run_test_tt_main tests
