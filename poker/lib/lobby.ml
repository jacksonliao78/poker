type player = {
  id : int;
  name : string;
  chips : int;
  connected : bool;
}

type t = {
  next_id : int;
  players : player list;
}

let empty () = { next_id = 1; players = [] }
let players lobby = lobby.players

let add_player lobby =
  let player =
    {
      id = lobby.next_id;
      name = Printf.sprintf "Player %d" lobby.next_id;
      chips = Types.default_config.starting_chips;
      connected = true;
    }
  in
  ({ next_id = lobby.next_id + 1; players = lobby.players @ [ player ] }, player)

let rename_player lobby ~player_id name =
  let players =
    List.map
      (fun player ->
        if player.id = player_id then { player with name } else player)
      lobby.players
  in
  { lobby with players }

let remove_player lobby ~player_id =
  let players =
    List.filter (fun player -> player.id <> player_id) lobby.players
  in
  { lobby with players }

let player_name lobby ~player_id =
  lobby.players
  |> List.find_opt (fun player -> player.id = player_id)
  |> Option.map (fun player -> player.name)

let snapshot lobby =
  let players =
    List.map
      (fun player ->
        {
          Protocol.id = player.id;
          name = player.name;
          chips = player.chips;
          connected = player.connected;
        })
      lobby.players
  in
  { Protocol.players; seats_open = Protocol.seats_total - List.length players }
