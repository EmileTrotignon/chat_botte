open Async
open Core
open Disml
module TmpMember = Member
open Models
module Member = TmpMember

(* Create a function to handle message_create. *)

let check_command (message : Message.t) =
  let Message.{content; _} = message in
  if String.(is_prefix content ~prefix:"!score") then Commands.get_score message

let main () =
  (* Register the event handler *)
  Client.message_create := check_command ;
  Client.reaction_add := Commands.update_score_add ;
  Client.reaction_remove := Commands.update_score_remove ;
  (* Start the client. It's recommended to load the token from an env var or other config file. *)
  Client.start Config.token
  >>> fun _client -> MLog.info "Connected successfully"

let _ =
  (* Launch the Async scheduler. You must do this for anything to work. *)
  Scheduler.go_main ~main ()
