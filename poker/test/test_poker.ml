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

let tests =
  "poker"
  >::: [
         "default_config_uses_500" >:: test_default_config;
         "lobby_add_player_updates_snapshot" >:: test_lobby_add_player;
       ]

let () = run_test_tt_main tests
