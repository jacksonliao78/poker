type client_message =
  | Join of string
  | Player_action of Types.action
  | Send_chat of string
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
  | Error of string
  | Info of string

(* Fixing the seat count in the protocol keeps the client UX consistent. *)
let seats_total = 4

(* Short names make the text lobby easier to redraw without wrapping badly. *)
let max_name_length = 20
