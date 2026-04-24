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

let ace_spades = { Poker.Types.rank = Ace; suit = Spades }
let ten_hearts = { Poker.Types.rank = Ten; suit = Hearts }

let test_terminal_card_rendering _ =
  let unicode = { Poker.Terminal_ui.color = false; unicode = true } in
  let colored = { Poker.Terminal_ui.color = true; unicode = true } in
  assert_equal "A♠" (Poker.Terminal_ui.render_card unicode ace_spades);
  assert_equal "10♥" (Poker.Terminal_ui.render_card unicode ten_hearts);
  assert_equal "Ace of Spades"
    (Poker.Terminal_ui.render_card Poker.Terminal_ui.plain ace_spades);
  assert_bool "plain fallback should not include ansi escapes"
    (not
       (String.contains
          (Poker.Terminal_ui.render_card Poker.Terminal_ui.plain ten_hearts)
          '\027'));
  assert_bool "colored unicode should include ansi escapes"
    (String.contains (Poker.Terminal_ui.render_card colored ten_hearts) '\027')

let four_player_game () =
  let lobby = Poker.Lobby.empty () in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  Poker.Game.start (Poker.Lobby.players lobby)

let test_player_view_keeps_hole_cards_private _ =
  let game = four_player_game () in
  let player = List.nth game.Poker.Types.players 0 in
  let other = List.nth game.players 1 in
  match Poker.Protocol.player_view_of_game game ~player_id:player.id with
  | None -> assert_failure "expected player view"
  | Some view ->
      assert_equal player.hole_cards view.Poker.Protocol.your_hole_cards;
      assert_bool "view must not expose another player's private cards"
        (view.your_hole_cards <> other.hole_cards)

let test_legal_actions_follow_turn_and_bet _ =
  let game = four_player_game () in
  let current = List.nth game.Poker.Types.players game.table.turn_index in
  let waiting = List.nth game.players 0 in
  assert_equal []
    (Poker.Protocol.legal_actions_for_player game ~player_id:waiting.id);
  assert_equal
    [ Poker.Protocol.Can_fold; Can_call 10; Can_raise 10 ]
    (Poker.Protocol.legal_actions_for_player game ~player_id:current.id)

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
         "terminal_card_rendering" >:: test_terminal_card_rendering;
         "player_view_keeps_hole_cards_private"
         >:: test_player_view_keeps_hole_cards_private;
         "legal_actions_follow_turn_and_bet"
         >:: test_legal_actions_follow_turn_and_bet;
       ]

let () = run_test_tt_main tests
