open Core
open Disml
module TmpMember = Member
open Models
module Member = TmpMember
open Async
open Disml_aux

(* Create a function to handle message_create. *)

let commands =
  Message_command.
    [ v (ExactPrefix "update cache") `Admin Commands.update_cache
    ; v (ExactPrefix "crunch") `Admin Commands.crunch_scores
    ; v (Prefix "score") `Member (Commands.get_smart_scores "score")
    ; v (Substring "https://www.youtube.com/watch?v=gJirxeFwVzA") `Member
        Commands.stupid_message
    ; v (Substring "GASPAR") `Member Commands.stupid_message
    ; v (Substring "CANAR") `Member Commands.stupid_message ]

let execute_commands commands message =
  let Message.{content; author; guild_id; _} = message in
  let guild_id = Option.value_exn guild_id in
  don't_wait_for
  @@ let%map is_admin = user_is_admin guild_id author in
     commands
     |> List.find ~f:(fun Message_command.{condition; permission; _} ->
            let authorized =
              match permission with `Admin -> is_admin | `Member -> true
            in
            authorized
            &&
            match condition with
            | Any ->
                true
            | Prefix prefix ->
                let command_prefix = Config.command_prefix ^ prefix in
                String.(is_prefix content ~prefix:command_prefix)
            | ExactPrefix prefix ->
                let command_prefix = Config.command_prefix ^ prefix in
                String.(content = command_prefix)
            | Substring substring ->
                String.is_substring ~substring content )
     |> Option.iter ~f:(fun Message_command.{command; _} -> command message)

let check_command (message : Message.t) = execute_commands commands message

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
