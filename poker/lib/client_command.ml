type preference =
  | Show_recent
  | Hide_recent
  | Clear_recent

type parsed =
  | Noop
  | Send of Protocol.client_message
  | Set_preference of preference
  | Error of string

let words_of_line line =
  line |> String.trim |> String.split_on_char ' '
  |> List.filter (fun word -> word <> "")

let has_action predicate actions = List.exists predicate actions

let raise_minimum actions =
  List.find_map
    (function
      | Protocol.Can_raise amount -> Some amount
      | _ -> None)
    actions

let action_command = function
  | Protocol.Can_fold -> "/fold"
  | Can_check -> "/check"
  | Can_call _ -> "/call"
  | Can_raise _ -> "/raise "

let available_action_commands actions = List.map action_command actions

let preference_error =
  "Use /pref recent show, /pref recent hide, or /pref recent clear."

let parse_preference words =
  match words with
  | [ "/pref"; "recent"; value ] -> (
      match String.lowercase_ascii value with
      | "show" -> Set_preference Show_recent
      | "hide" -> Set_preference Hide_recent
      | "clear" -> Set_preference Clear_recent
      | _ -> Error preference_error)
  | [ "/pref" ] | [ "/pref"; _ ] | "/pref" :: _ -> Error preference_error
  | _ -> Error "Unknown preference command."

let poker_action_allowed actions action =
  let open Protocol in
  match action with
  | Types.Fold ->
      has_action
        (function
          | Can_fold -> true
          | _ -> false)
        actions
  | Check ->
      has_action
        (function
          | Can_check -> true
          | _ -> false)
        actions
  | Call ->
      has_action
        (function
          | Can_call _ -> true
          | _ -> false)
        actions
  | Raise amount -> (
      match raise_minimum actions with
      | Some minimum -> amount >= minimum
      | None -> false)
  | Bet _ -> false

let parse_action words =
  match words with
  | [ "/fold" ] | [ "/f" ] -> Some (Ok Types.Fold)
  | [ "/call" ] | [ "/c" ] -> Some (Ok Types.Call)
  | [ "/check" ] | [ "/x" ] -> Some (Ok Types.Check)
  | [ "/raise"; amount ] | [ "/r"; amount ] -> (
      match int_of_string_opt amount with
      | Some amount when amount > 0 -> Some (Ok (Types.Raise amount))
      | _ -> Some (Error "Raise needs a positive amount, e.g. /raise 20."))
  | "/raise" :: _ | "/r" :: _ ->
      Some (Error "Raise needs a positive amount, e.g. /raise 20.")
  | _ ->
      Some
        (Error
           "Unknown command. Type chat without /, or use /name, /pref, /fold, \
            /call, /check, /raise, /start, /cashout, /cashin, /quit.")

let parse ~legal_actions line =
  let trimmed = String.trim line in
  let words = words_of_line line in
  if trimmed = "" then Noop
  else if String.get trimmed 0 <> '/' then Send (Protocol.Send_chat trimmed)
  else if trimmed = "/quit" then Send Protocol.Disconnect
  else if trimmed = "/cashout" || trimmed = "/cash_out" then
    Send (Protocol.Cash_out true)
  else if trimmed = "/cashin" || trimmed = "/cash_in" then
    Send (Protocol.Cash_out false)
  else if trimmed = "/start" then Send Protocol.Start_game
  else if List.length words > 0 && List.hd words = "/pref" then
    parse_preference words
  else if String.length trimmed >= 6 && String.sub trimmed 0 6 = "/name " then
    Send (Protocol.Join (String.sub trimmed 6 (String.length trimmed - 6)))
  else
    match parse_action words with
    | None -> Send (Protocol.Send_chat trimmed)
    | Some (Error message) -> Error message
    | Some (Ok action) ->
        if poker_action_allowed legal_actions action then
          Send (Protocol.Player_action action)
        else Error "That poker action is not available right now."
