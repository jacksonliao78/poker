type client_message =
  | Join of string
  | Player_action of Types.action
  | Send_chat of string
  | Cash_out of bool
  | Start_game
  | Disconnect

type player_summary = {
  id : int;
  name : string;
  chips : int;
  connected : bool;
}

type lobby_snapshot = {
  players : player_summary list;
  seats_open : int;
  host_id : int option;
  min_players : int;
}

type legal_action =
  | Can_fold
  | Can_check
  | Can_call of int
  | Can_raise of int

type public_player = {
  id : int;
  name : string;
  chips : int;
  round_bet : int;
  status : Types.player_status;
  is_dealer : bool;
  is_small_blind : bool;
  is_big_blind : bool;
  is_turn : bool;
  last_street_action : Types.action option;
}

type table_view = {
  community_cards : Types.card list;
  pot : int;
  current_bet : int;
  min_raise : int;
  street : Types.street;
}

type player_view = {
  your_id : int;
  your_hole_cards : Types.card list;
  table : table_view;
  players : public_player list;
  legal_actions : legal_action list;
}

type server_message =
  | Welcome of {
      player_id : int;
      starting_chips : int;
      seats_total : int;
    }
  | Lobby_update of lobby_snapshot
  | Chat_message of {
      from_name : string;
      text : string;
    }
  | Game_update of player_view
  | Error of string
  | Info of string

(* Fixing the seat count in the protocol keeps the client UX consistent. *)
let seats_total = 10

let min_players_to_start = 4

(* Short names make the text lobby easier to redraw without wrapping badly. *)
let max_name_length = 20

let legal_actions_for_player (game : Types.game_state) ~player_id =
  let current_player = List.nth game.players game.table.turn_index in
  match
    List.find_opt (fun player -> player.Types.id = player_id) game.players
  with
  | None -> []
  | Some player when current_player.id <> player.id -> []
  | Some player when player.status <> Types.Active -> []
  | Some player ->
      let to_call = max 0 (game.table.current_bet - player.round_bet) in
      let active_opponents =
        List.filter
          (fun p -> p.Types.id <> player.Types.id && p.status = Types.Active)
          game.players
      in
      let effective_max_raise =
        match active_opponents with
        | [] -> 0
        | _ ->
            let min_opp =
              List.fold_left
                (fun acc p -> min acc (p.Types.chips + p.round_bet))
                max_int active_opponents
            in
            min_opp - game.table.current_bet
      in
      let raise =
        if
          player.chips >= to_call + game.table.min_raise
          && game.table.min_raise <= effective_max_raise
        then [ Can_raise game.table.min_raise ]
        else []
      in
      if to_call > 0 then [ Can_fold; Can_call to_call ] @ raise
      else [ Can_check ] @ raise

let player_view_of_game (game : Types.game_state) ~player_id =
  match
    List.find_opt (fun player -> player.Types.id = player_id) game.players
  with
  | None -> None
  | Some viewer ->
      let player_count = List.length game.players in
      let small_blind_index = (game.table.dealer_index + 1) mod player_count in
      let big_blind_index = (game.table.dealer_index + 2) mod player_count in
      let players =
        List.mapi
          (fun index player ->
            {
              id = player.Types.id;
              name = player.name;
              chips = player.chips;
              round_bet = player.round_bet;
              status = player.status;
              is_dealer = index = game.table.dealer_index;
              is_small_blind = index = small_blind_index;
              is_big_blind = index = big_blind_index;
              is_turn = index = game.table.turn_index;
              last_street_action = player.last_street_action;
            })
          game.players
      in
      Some
        {
          your_id = player_id;
          your_hole_cards = viewer.hole_cards;
          table =
            {
              community_cards = game.table.community_cards;
              pot = game.table.pot;
              current_bet = game.table.current_bet;
              min_raise = game.table.min_raise;
              street = game.table.street;
            };
          players;
          legal_actions = legal_actions_for_player game ~player_id;
        }
