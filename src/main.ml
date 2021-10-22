open Core
open Disml
module TmpMember = Member
open Models
module Member = TmpMember
open Async

(* Create a function to handle message_create. *)

let check_command (message : Message.t) =
  let Message.{content; _} = message in
  (* if String.(is_prefix content ~prefix:"!members") then
     don't_wait_for
       (let guild_id = Option.value_exn Message.(message.guild_id) in
        let%map members = MCache.get_members guild_id in
        MLog.info
          {%eml|<%Member.Set.iter ~f:(fun m ->%><%- Member.visible_name m %> <%) members ;%>|}
       ) ; *)
  if String.(is_prefix content ~prefix:"!score") then
    if String.(content = "!score") then Commands.get_score_of_author message
    else if String.(content = "!score @everyone") then
      Commands.get_scores_of_everyone message
    else Commands.get_scores_of_mentions message

let main () =
  (* Register the event handler *)
  Client.message_create := check_command ;
  Client.reaction_add := Commands.update_score_add ;
  Client.reaction_remove := Commands.update_score_remove ;
  (* Start the client. It's recommended to load the token from an env var or other config file. *)
  Async.Deferred.don't_wait_for
  @@ let%map _client = Client.start Config.token in
     MLog.info "Connected successfully"

let _ =
  (* Launch the Async scheduler. You must do this for anything to work. *)
  Scheduler.go_main ~main ()
