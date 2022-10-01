open Common
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

type elt =
  {condition: condition; command: Message.t -> unit Lwt.t; description: string}

let role_id (`Role_id id) = PPPrint.(!^(Printf.sprintf "%s%i%s" "<@" id "%s"))

let rec print_condition cond =
  PPPrint.(
    group
    @@
    match cond with
    | Admin ->
        !^"est un admin"
    | HasRole id ->
        !^"possède le role " ^-^ role_id id
    | Any ->
        !^"anyone"
    | Prefix str ->
        !^"commence par" ^-^ bquotes (!^Config.command_prefix ^^ !^str)
    | ExactPrefix str ->
        !^"est " ^-^ bquotes (!^Config.command_prefix ^^ !^str)
    | Substring str ->
        !^"contient " ^-^ bquotes (!^Config.command_prefix ^^ !^str)
    | Not cond ->
        !^"ne vérifie pas la condition suivante :" ^/^ print_condition cond
    | Or conds ->
        !^"vérifie soit :"
        ^/^ separate_map
              (break 1 ^^ !^"soit :" ^^ break 1)
              print_condition conds
    | And conds ->
        !^"vérifie :"
        ^/^ separate_map (break 1 ^^ !^"et :" ^^ break 1) print_condition conds )

let v condition description command = {condition; description; command}

let v_async condition description command =
  { condition
  ; description
  ; command=
      (fun message ->
        let+ match_variable = command message in
        match match_variable with
        | Error e ->
            MLog.error_t "While executing command" e
        | Ok () ->
            () ) }

type t = elt list

let print_t li =
  PPPrint.(
    li
    |> List.map (fun {condition; description; _} ->
           minus ^^ space ^^ space
           ^^ align
                ( !^"Condition :" ^-^ print_condition condition ^^ hardline
                ^^ !^"Description :" ^-^ !^description )
           ^^ hardline )
    |> concat )

let to_string f arg =
  PPPrint.(
    let doc = f arg in
    let buffer = Buffer.create 10 in
    ToBuffer.pretty 0.8 80 buffer doc ;
    Buffer.contents buffer )

let to_string li = to_string print_t li
