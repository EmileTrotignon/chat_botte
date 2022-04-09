open Disml
open Models

type permission = Member | Admin | HasRole of Role_id.t

type condition =
  | Prefix of string
  | ExactPrefix of string
  | Any
  | Substring of string

type elt =
  {condition: condition; permission: permission; command: Message.t -> unit}

let v condition permission command = {condition; permission; command}

type t = elt list
