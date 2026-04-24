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
    Poker.Game.apply_action game ~player_id:(List.nth game.Poker.Types.players 3).id
      Poker.Types.Call
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
    Poker.Game.apply_action game ~player_id:(List.nth game.Poker.Types.players 3).id
      Poker.Types.Fold
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
       ]

let () = run_test_tt_main tests
