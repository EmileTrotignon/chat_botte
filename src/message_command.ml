open Disml
open Models

type permission = [`Member | `Admin]

type elt =
  { prefix: string
  ; exact: bool
  ; permission: permission
  ; command: Message.t -> unit }

let v prefix exact permission command = {prefix; permission; exact; command}

type t = elt list
