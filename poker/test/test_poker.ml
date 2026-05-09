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

let four_player_game () =
  let lobby = Poker.Lobby.empty () in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  let lobby, _ = Poker.Lobby.add_player lobby in
  Poker.Game.start (Poker.Lobby.players lobby)

let test_check_while_facing_bet_returns_error _ =
  let game = four_player_game () in
  match
    Poker.Game.apply_action game
      ~player_id:(List.nth game.Poker.Types.players 3).id Poker.Types.Check
  with
  | Error message -> assert_equal "Cannot check when facing a bet." message
  | Ok _ -> assert_failure "expected check to fail while facing a bet"

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
  let game =
    match apply_ok game 2 Poker.Types.Check with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected flop transition"
  in
  let game =
    match apply_ok game 1 Poker.Types.Check with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected flop next_turn"
  in
  let game =
    match apply_ok game 2 Poker.Types.Check with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected flop next_turn"
  in
  let game =
    match apply_ok game 3 Poker.Types.Check with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected flop next_turn"
  in
  let game =
    match apply_ok game 0 Poker.Types.Check with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected turn transition"
  in
  assert_equal Poker.Types.Turn game.table.street;
  assert_equal 4 (List.length game.table.community_cards);
  let game =
    match apply_ok game 1 Poker.Types.Check with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected turn next_turn"
  in
  let game =
    match apply_ok game 2 Poker.Types.Check with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected turn next_turn"
  in
  let game =
    match apply_ok game 3 Poker.Types.Check with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected turn next_turn"
  in
  match apply_ok game 0 Poker.Types.Check with
  | Poker.Game.Next_turn game ->
      assert_equal Poker.Types.River game.table.street;
      assert_equal 5 (List.length game.table.community_cards)
  | _ -> assert_failure "expected river transition"

let test_full_deck_has_52_unique_cards _ =
  let deck = Poker.Cards.full_deck in
  assert_equal 52 (List.length deck);
  let unique =
    List.sort_uniq
      (fun a b ->
        Stdlib.compare
          (Poker.Cards.card_to_string a)
          (Poker.Cards.card_to_string b))
      deck
  in
  assert_equal 52 (List.length unique);
  List.iter
    (fun suit ->
      let count =
        List.length (List.filter (fun c -> c.Poker.Types.suit = suit) deck)
      in
      assert_equal ~msg:"13 cards per suit" 13 count)
    Poker.Cards.all_suits

let test_deal_n_splits_deck _ =
  let drawn, remaining = Poker.Cards.deal_n 5 Poker.Cards.full_deck in
  assert_equal 5 (List.length drawn);
  assert_equal 47 (List.length remaining);
  assert_equal Poker.Cards.full_deck (drawn @ remaining)

let test_card_rendering_short_and_long _ =
  let ace_hearts = { Poker.Types.rank = Ace; suit = Hearts } in
  let ten_clubs = { Poker.Types.rank = Ten; suit = Clubs } in
  assert_equal "Ah" (Poker.Cards.card_to_string ace_hearts);
  assert_equal "Tc" (Poker.Cards.card_to_string ten_clubs);
  assert_equal "Ace of Hearts" (Poker.Cards.card_to_long_string ace_hearts);
  assert_equal "10 of Clubs" (Poker.Cards.card_to_long_string ten_clubs)

let test_lobby_rename_player _ =
  let lobby, player = Poker.Lobby.add_player (Poker.Lobby.empty ()) in
  let lobby = Poker.Lobby.rename_player lobby ~player_id:player.id "Alice" in
  assert_equal (Some "Alice")
    (Poker.Lobby.player_name lobby ~player_id:player.id);
  assert_equal None (Poker.Lobby.player_name lobby ~player_id:999)

let test_lobby_remove_player _ =
  let lobby, first = Poker.Lobby.add_player (Poker.Lobby.empty ()) in
  let lobby, second = Poker.Lobby.add_player lobby in
  let lobby = Poker.Lobby.remove_player lobby ~player_id:first.id in
  let snapshot = Poker.Lobby.snapshot lobby in
  assert_equal 1 (List.length snapshot.Poker.Protocol.players);
  assert_equal second.id (List.hd snapshot.players).id

let test_raise_updates_bet_and_min_raise _ =
  let game = make_four_player_game () in
  match apply_ok game 3 (Poker.Types.Raise 20) with
  | Poker.Game.Next_turn next_game ->
      let raiser = List.nth next_game.players 3 in
      assert_equal 470 raiser.chips;
      assert_equal 30 raiser.round_bet;
      assert_equal 45 next_game.table.pot;
      assert_equal 30 next_game.table.current_bet;
      assert_equal 20 next_game.table.min_raise;
      assert_equal 0 next_game.table.turn_index
  | _ -> assert_failure "expected next turn after raise"

let test_raise_below_min_is_rejected _ =
  let game = make_four_player_game () in
  match
    Poker.Game.apply_action game
      ~player_id:(List.nth game.Poker.Types.players 3).id (Poker.Types.Raise 5)
  with
  | Error message ->
      assert_equal "Raise is smaller than the minimum raise." message
  | Ok _ -> assert_failure "expected raise below min to fail"

let test_fold_out_ends_hand _ =
  let game = make_four_player_game () in
  let game =
    match apply_ok game 3 Poker.Types.Fold with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected next_turn after UTG fold"
  in
  let game =
    match apply_ok game 0 Poker.Types.Fold with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected next_turn after dealer fold"
  in
  match apply_ok game 1 Poker.Types.Fold with
  | Poker.Game.Hand_complete (_, winner) ->
      let big_blind = List.nth game.Poker.Types.players 2 in
      assert_equal big_blind.id winner.id
  | _ ->
      assert_failure
        "expected hand_complete when only one active player remains"

let test_start_next_hand_resets_state _ =
  let game = make_four_player_game () in
  let next_hand = Poker.Game.start_next_hand game in
  assert_equal 4 (List.length next_hand.Poker.Types.players);
  assert_equal 44 (List.length next_hand.deck);
  assert_equal Poker.Types.Preflop next_hand.table.street;
  assert_equal [] next_hand.table.community_cards;
  assert_bool "each player gets two fresh hole cards"
    (List.for_all
       (fun p -> List.length p.Poker.Types.hole_cards = 2)
       next_hand.players)

let test_draw_community_cards_moves_from_deck _ =
  let game = make_four_player_game () in
  let original_deck_size = List.length game.Poker.Types.deck in
  let drawn_game = Poker.Game.draw_community_cards game ~count:3 in
  assert_equal 3 (List.length drawn_game.table.community_cards);
  assert_equal (original_deck_size - 3) (List.length drawn_game.deck)

let test_resolve_showdown_picks_highest_rank _ =
  let base_game = make_four_player_game () in
  let players =
    List.mapi
      (fun i p ->
        match i with
        | 0 ->
            {
              p with
              Poker.Types.hole_cards =
                [
                  { Poker.Types.rank = Ace; suit = Spades };
                  { rank = Two; suit = Hearts };
                ];
            }
        | 1 ->
            {
              p with
              Poker.Types.hole_cards =
                [
                  { Poker.Types.rank = King; suit = Spades };
                  { rank = Three; suit = Hearts };
                ];
            }
        | _ -> { p with status = Poker.Types.Folded })
      base_game.Poker.Types.players
  in
  let table =
    {
      base_game.table with
      community_cards =
        [
          { Poker.Types.rank = Four; suit = Clubs };
          { rank = Five; suit = Diamonds };
          { rank = Six; suit = Hearts };
          { rank = Seven; suit = Spades };
          { rank = Eight; suit = Clubs };
        ];
    }
  in
  let game = { base_game with players; table } in
  let winners = Poker.Game.resolve_showdown game in
  assert_equal 2 (List.length winners);
  let winner_ids = winners |> List.map (fun p -> p.Poker.Types.id) |> List.sort compare in
  let expected_ids =
    [ (List.nth players 0).id; (List.nth players 1).id ] |> List.sort compare
  in
  assert_equal expected_ids winner_ids

let test_player_view_exposes_seat_roles _ =
  let game = make_four_player_game () in
  let viewer = List.nth game.Poker.Types.players 0 in
  match Poker.Protocol.player_view_of_game game ~player_id:viewer.id with
  | None -> assert_failure "expected player view"
  | Some view ->
      let by_index i = List.nth view.Poker.Protocol.players i in
      assert_bool "dealer flag on seat 0" (by_index 0).is_dealer;
      assert_bool "small blind flag on seat 1" (by_index 1).is_small_blind;
      assert_bool "big blind flag on seat 2" (by_index 2).is_big_blind;
      assert_bool "turn flag on seat 3" (by_index 3).is_turn;
      assert_equal Poker.Types.Preflop view.table.street;
      assert_equal 15 view.table.pot

let test_bet_action_is_rejected _ =
  let game = make_four_player_game () in
  match
    Poker.Game.apply_action game
      ~player_id:(List.nth game.Poker.Types.players 3).id (Poker.Types.Bet 20)
  with
  | Error message -> assert_equal "Bet is not supported yet." message
  | Ok _ -> assert_failure "expected Bet to be rejected"

let test_terminal_street_and_status _ =
  assert_equal "Preflop" (Poker.Terminal_ui.render_street Poker.Types.Preflop);
  assert_equal "Flop" (Poker.Terminal_ui.render_street Poker.Types.Flop);
  assert_equal "Turn" (Poker.Terminal_ui.render_street Poker.Types.Turn);
  assert_equal "River" (Poker.Terminal_ui.render_street Poker.Types.River);
  assert_equal "active" (Poker.Terminal_ui.render_status Poker.Types.Active);
  assert_equal "folded" (Poker.Terminal_ui.render_status Poker.Types.Folded);
  assert_equal "all-in" (Poker.Terminal_ui.render_status Poker.Types.AllIn)

let contains_substring haystack needle =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  let rec loop i =
    if i + nlen > hlen then false
    else if String.sub haystack i nlen = needle then true
    else loop (i + 1)
  in
  nlen = 0 || loop 0

let test_terminal_legal_action_rendering _ =
  let plain = Poker.Terminal_ui.plain in
  assert_equal "/fold"
    (Poker.Terminal_ui.render_legal_action plain Poker.Protocol.Can_fold);
  assert_equal "/check"
    (Poker.Terminal_ui.render_legal_action plain Poker.Protocol.Can_check);
  let call_text =
    Poker.Terminal_ui.render_legal_action plain (Poker.Protocol.Can_call 10)
  in
  assert_bool "call text mentions /call" (contains_substring call_text "/call");
  let raise_text =
    Poker.Terminal_ui.render_legal_action plain (Poker.Protocol.Can_raise 20)
  in
  assert_bool "raise text mentions /raise"
    (contains_substring raise_text "/raise");
  assert_bool "raise text mentions min" (contains_substring raise_text "min")

let test_deal_n_beyond_deck_returns_all _ =
  let short_deck =
    [
      { Poker.Types.rank = Ace; suit = Spades }; { rank = King; suit = Hearts };
    ]
  in
  let drawn, remaining = Poker.Cards.deal_n 5 short_deck in
  assert_equal 2 (List.length drawn);
  assert_equal [] remaining;
  let empty_drawn, empty_remaining = Poker.Cards.deal_n 0 short_deck in
  assert_equal [] empty_drawn;
  assert_equal short_deck empty_remaining

let test_lobby_snapshot_tracks_open_seats _ =
  let lobby = Poker.Lobby.empty () in
  assert_equal Poker.Protocol.seats_total
    (Poker.Lobby.snapshot lobby).seats_open;
  let lobby =
    List.fold_left
      (fun acc _ ->
        let acc, _ = Poker.Lobby.add_player acc in
        acc)
      lobby
      (List.init Poker.Protocol.seats_total (fun _ -> ()))
  in
  let snapshot = Poker.Lobby.snapshot lobby in
  assert_equal 0 snapshot.seats_open;
  assert_equal Poker.Protocol.seats_total (List.length snapshot.players)

let test_game_start_preserves_blind_constants _ =
  let game = make_four_player_game () in
  assert_equal Poker.Types.default_config.small_blind
    game.Poker.Types.small_blind;
  assert_equal Poker.Types.default_config.big_blind game.big_blind;
  assert_equal game.big_blind game.table.min_raise;
  assert_equal Poker.Types.Preflop game.table.street;
  assert_equal [] game.table.community_cards

let test_check_succeeds_when_no_bet _ =
  let game = make_four_player_game () in
  let game =
    match apply_ok game 3 Poker.Types.Call with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected next_turn after UTG call"
  in
  let game =
    match apply_ok game 0 Poker.Types.Call with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected next_turn after dealer call"
  in
  let game =
    match apply_ok game 1 Poker.Types.Call with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected big blind option before flop"
  in
  assert_equal Poker.Types.Preflop game.table.street;
  assert_equal 2 game.table.turn_index;
  let game =
    match apply_ok game 2 Poker.Types.Check with
    | Poker.Game.Next_turn g -> g
    | _ -> assert_failure "expected transition to flop after big blind check"
  in
  assert_equal Poker.Types.Flop game.table.street;
  assert_equal 0 game.table.current_bet;
  match apply_ok game 1 Poker.Types.Check with
  | Poker.Game.Next_turn next_game ->
      assert_equal 0 (List.nth next_game.players 1).round_bet;
      assert_equal 2 next_game.table.turn_index
  | _ -> assert_failure "expected next_turn after check on flop"

let test_reset_bets_clears_round_bets _ =
  let game = make_four_player_game () in
  let reset = Poker.Game.reset_bets game in
  assert_bool "all round_bets cleared"
    (List.for_all
       (fun p -> p.Poker.Types.round_bet = 0)
       reset.Poker.Types.players);
  assert_equal 0 reset.table.current_bet;
  assert_equal game.big_blind reset.table.min_raise

let test_protocol_returns_none_and_empty_for_unknown _ =
  let game = make_four_player_game () in
  assert_equal None (Poker.Protocol.player_view_of_game game ~player_id:999);
  assert_equal [] (Poker.Protocol.legal_actions_for_player game ~player_id:999);
  let off_turn = List.nth game.Poker.Types.players 0 in
  assert_equal []
    (Poker.Protocol.legal_actions_for_player game ~player_id:off_turn.id)

let test_terminal_render_cards_variants _ =
  let plain = Poker.Terminal_ui.plain in
  assert_equal "(none)" (Poker.Terminal_ui.render_cards plain []);
  let cards =
    [ { Poker.Types.rank = Ace; suit = Spades }; { rank = Ten; suit = Hearts } ]
  in
  let rendered = Poker.Terminal_ui.render_cards plain cards in
  assert_bool "contains first card name"
    (contains_substring rendered "Ace of Spades");
  assert_bool "contains second card name"
    (contains_substring rendered "10 of Hearts");
  assert_bool "uses two-space separator" (contains_substring rendered "  ")

let test_long_card_rendering_all_ranks_and_suits _ =
  assert_equal
    [
      "2 of Diamonds";
      "3 of Diamonds";
      "4 of Diamonds";
      "5 of Diamonds";
      "6 of Diamonds";
      "7 of Diamonds";
      "8 of Diamonds";
      "9 of Diamonds";
      "10 of Diamonds";
      "Jack of Diamonds";
      "Queen of Diamonds";
      "King of Diamonds";
      "Ace of Diamonds";
    ]
    (List.map
       (fun rank ->
         Poker.Cards.card_to_long_string { Poker.Types.rank; suit = Diamonds })
       Poker.Cards.all_ranks);
  assert_equal
    [ "Ace of Hearts"; "Ace of Diamonds"; "Ace of Clubs"; "Ace of Spades" ]
    (List.map
       (fun suit ->
         Poker.Cards.card_to_long_string { Poker.Types.rank = Ace; suit })
       Poker.Cards.all_suits)

let test_terminal_unicode_rendering_all_ranks_and_suits _ =
  let style = { Poker.Terminal_ui.color = false; unicode = true } in
  assert_equal
    [
      "2♣";
      "3♣";
      "4♣";
      "5♣";
      "6♣";
      "7♣";
      "8♣";
      "9♣";
      "10♣";
      "J♣";
      "Q♣";
      "K♣";
      "A♣";
    ]
    (List.map
       (fun rank ->
         Poker.Terminal_ui.render_card style { Poker.Types.rank; suit = Clubs })
       Poker.Cards.all_ranks);
  assert_equal
    [ "A♥"; "A♦"; "A♣"; "A♠" ]
    (List.map
       (fun suit ->
         Poker.Terminal_ui.render_card style { Poker.Types.rank = Ace; suit })
       Poker.Cards.all_suits)

let test_apply_action_rejects_invalid_players _ =
  let game = make_four_player_game () in
  let current = List.nth game.Poker.Types.players game.table.turn_index in
  let waiting = List.nth game.players 0 in
  (match Poker.Game.apply_action game ~player_id:999 Poker.Types.Call with
  | Error message -> assert_equal "Unknown player." message
  | Ok _ -> assert_failure "expected unknown player to fail");
  (match
     Poker.Game.apply_action game ~player_id:waiting.id Poker.Types.Call
   with
  | Error message -> assert_equal "It is not your turn." message
  | Ok _ -> assert_failure "expected off-turn player to fail");
  let players =
    List.mapi
      (fun index player ->
        if index = game.table.turn_index then
          { player with Poker.Types.status = Folded }
        else player)
      game.players
  in
  match
    Poker.Game.apply_action { game with players } ~player_id:current.id
      Poker.Types.Call
  with
  | Error message -> assert_equal "You cannot act right now." message
  | Ok _ -> assert_failure "expected folded current player to fail"

let test_award_pot_transfers_to_winner _ =
  let game = make_four_player_game () in
  let pot_before = game.Poker.Types.table.pot in
  assert_bool "pot is non-zero after blinds" (pot_before > 0);
  let winner = List.hd game.players in
  let chips_before = winner.chips in
  let settled = Poker.Game.award_pot game ~winner_id:winner.id in
  assert_equal 0 settled.table.pot;
  let winner_after =
    List.find (fun p -> p.Poker.Types.id = winner.id) settled.players
  in
  assert_equal (chips_before + pot_before) winner_after.chips

let test_next_hand_rotates_dealer _ =
  let game = make_four_player_game () in
  match Poker.Game.next_hand game with
  | None -> assert_failure "expected next hand to start"
  | Some next ->
      assert_equal 4 (List.length next.Poker.Types.players);
      (* dealer rotated from player at index 0 to player previously at index 1 *)
      let prev_seat_1_id = (List.nth game.players 1).id in
      let new_dealer_id =
        (List.nth next.players next.table.dealer_index).id
      in
      assert_equal prev_seat_1_id new_dealer_id

let test_next_hand_returns_none_when_one_player_has_chips _ =
  let game = make_four_player_game () in
  let players =
    List.mapi
      (fun i p ->
        if i = 0 then p else { p with Poker.Types.chips = 0 })
      game.Poker.Types.players
  in
  assert_equal None (Poker.Game.next_hand { game with players })

let test_advance_street_on_river_completes_hand _ =
  let game = make_four_player_game () in
  let table = { game.Poker.Types.table with street = River } in
  match Poker.Game.advance_street { game with table } with
  | Poker.Game.Hand_complete (_, winner) ->
      assert_bool "winner is one of the active players"
        (List.exists
           (fun player -> player.Poker.Types.id = winner.id)
           game.players)
  | _ -> assert_failure "expected river to complete the hand"

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
         "terminal_card_rendering" >:: test_terminal_card_rendering;
         "player_view_keeps_hole_cards_private"
         >:: test_player_view_keeps_hole_cards_private;
         "legal_actions_follow_turn_and_bet"
         >:: test_legal_actions_follow_turn_and_bet;
         "preflop_to_flop" >:: test_preflop_to_flop;
         "preflop_big_blind_can_raise_option"
         >:: test_preflop_big_blind_can_raise_option;
         "checkdown_to_river" >:: test_checkdown_to_river;
         "full_deck_has_52_unique_cards" >:: test_full_deck_has_52_unique_cards;
         "deal_n_splits_deck" >:: test_deal_n_splits_deck;
         "card_rendering_short_and_long" >:: test_card_rendering_short_and_long;
         "lobby_rename_player" >:: test_lobby_rename_player;
         "lobby_remove_player" >:: test_lobby_remove_player;
         "raise_updates_bet_and_min_raise"
         >:: test_raise_updates_bet_and_min_raise;
         "raise_below_min_is_rejected" >:: test_raise_below_min_is_rejected;
         "fold_out_ends_hand" >:: test_fold_out_ends_hand;
         "start_next_hand_resets_state" >:: test_start_next_hand_resets_state;
         "award_pot_transfers_to_winner" >:: test_award_pot_transfers_to_winner;
         "next_hand_rotates_dealer" >:: test_next_hand_rotates_dealer;
         "next_hand_returns_none_when_one_player_has_chips"
         >:: test_next_hand_returns_none_when_one_player_has_chips;
         "draw_community_cards_moves_from_deck"
         >:: test_draw_community_cards_moves_from_deck;
         "resolve_showdown_picks_highest_rank"
         >:: test_resolve_showdown_picks_highest_rank;
         "player_view_exposes_seat_roles"
         >:: test_player_view_exposes_seat_roles;
         "bet_action_is_rejected" >:: test_bet_action_is_rejected;
         "terminal_street_and_status" >:: test_terminal_street_and_status;
         "terminal_legal_action_rendering"
         >:: test_terminal_legal_action_rendering;
         "deal_n_beyond_deck_returns_all"
         >:: test_deal_n_beyond_deck_returns_all;
         "lobby_snapshot_tracks_open_seats"
         >:: test_lobby_snapshot_tracks_open_seats;
         "game_start_preserves_blind_constants"
         >:: test_game_start_preserves_blind_constants;
         "check_succeeds_when_no_bet" >:: test_check_succeeds_when_no_bet;
         "reset_bets_clears_round_bets" >:: test_reset_bets_clears_round_bets;
         "protocol_returns_none_and_empty_for_unknown"
         >:: test_protocol_returns_none_and_empty_for_unknown;
         "terminal_render_cards_variants"
         >:: test_terminal_render_cards_variants;
         "long_card_rendering_all_ranks_and_suits"
         >:: test_long_card_rendering_all_ranks_and_suits;
         "terminal_unicode_rendering_all_ranks_and_suits"
         >:: test_terminal_unicode_rendering_all_ranks_and_suits;
         "apply_action_rejects_invalid_players"
         >:: test_apply_action_rejects_invalid_players;
         "advance_street_on_river_completes_hand"
         >:: test_advance_street_on_river_completes_hand;
       ]

let () = run_test_tt_main tests
