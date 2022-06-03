open Disml
open Models

type condition =
  | Admin
  | HasRole of Role_id.t
  | Prefix of string
  | ExactPrefix of string
  | Any
  | Substring of string
  | Or of condition list
  | And of condition list
  | Not of condition

type elt = {condition: condition; command: Message.t -> unit}

let v condition command = {condition; command}

type t = elt list
