(** A local preference command that changes only the current client UI. *)
type preference =
  | Show_recent
  | Hide_recent
  | Clear_recent

(** The result of parsing one submitted line. *)
type parsed =
  | Noop
  | Send of Protocol.client_message
  | Set_preference of preference
  | Error of string

(** [action_command action] returns the command text used to prefill the input
    buffer for [action]. *)
val action_command : Protocol.legal_action -> string

(** [available_action_commands actions] returns the input-buffer presets for the
    currently legal actions. Requires: [actions] comes from the latest player
    view if the caller wants turn-accurate suggestions. *)
val available_action_commands : Protocol.legal_action list -> string list

(** [parse ~legal_actions line] parses one submitted client line. Bare nonblank
    text is chat. Slash-prefixed poker actions are accepted only when present in
    [legal_actions], because the client should reject stale or impossible
    actions before sending them to the server. Requires: [legal_actions] is the
    action list from the current player view, or [[]] when no game action is
    available. *)
val parse : legal_actions:Protocol.legal_action list -> string -> parsed
