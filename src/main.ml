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
    [ v (And [ExactPrefix "update cache"; Admin]) Commands.update_cache
    ; v (And [ExactPrefix "crunch"; Admin]) Commands.crunch_scores
    ; v (ExactPrefix "score") Commands.get_score_of_author
    ; v (ExactPrefix "rank") Commands.rank_members
    ; v (Prefix "score") (Commands.get_smart_scores "score")
    ; v (Prefix "affuble") Commands.change_nick
    ; v (Substring "gJirxeFwVzA") Commands.stupid_message
    ; v (Substring "GASPAR") Commands.stupid_message
    ; v (Substring "CANAR") Commands.stupid_message
    ; v
        (HasRole (`Role_id Config.Roles.warning))
        (Commands.chance_of_delete 0.33)
    ; v (And [Prefix "ping"]) Commands.send_dm ]

let execute_commands commands message =
  let Message.{content; author; guild_id; _} = message in
  guild_id
  |> Option.iter ~f:(fun guild_id ->
         don't_wait_for
         @@ let%bind is_admin = user_is_admin guild_id author in
            match%map member_of_user guild_id author with
            | Error e ->
                MLog.error_t "While converting author to member " e
            | Ok member ->
                let rec validates condition =
                  match condition with
                  | Message_command.Admin ->
                      is_admin
                  | Message_command.HasRole role_id ->
                      Member.has_role member role_id
                  | Message_command.Prefix prefix ->
                      let command_prefix = Config.command_prefix ^ prefix in
                      String.(is_prefix content ~prefix:command_prefix)
                  | Message_command.ExactPrefix prefix ->
                      let command_prefix = Config.command_prefix ^ prefix in
                      String.(content = command_prefix)
                  | Message_command.Any ->
                      true
                  | Message_command.Substring substring ->
                      String.is_substring ~substring content
                  | Message_command.Or li ->
                      List.exists ~f:validates li
                  | Message_command.And li ->
                      List.for_all ~f:validates li
                  | Message_command.Not cond ->
                      not (validates cond)
                in
                commands
                |> List.find ~f:(fun Message_command.{condition; _} ->
                       validates condition )
                |> Option.iter ~f:(fun Message_command.{command; _} ->
                       command message ) )

let commands_edit =
  Message_command.
    [v (HasRole (`Role_id Config.Roles.edit_punished)) Commands.delete_message]

let rec main () =
  (* Register the event handler *)
  let%bind result =
    Async.try_with (fun () ->
        (Client.message_update :=
           fun update ->
             don't_wait_for
             @@ let%map message =
                  Event.MessageUpdate.(
                    message_of_id update.id update.channel_id)
                in
                (* Yojson.Safe.pretty_to_channel stdout
                   (Event.MessageUpdate.to_yojson update) ; *)
                match message with
                | Ok message ->
                    let message =
                      { message with
                        guild_id= Event.MessageUpdate.(update.guild_id) }
                    in
                    (* Yojson.Safe.pretty_to_channel stdout (Message.to_yojson message) ; *)
                    execute_commands commands_edit message
                | Error e ->
                    MLog.error_t "while convering message update to message" e
        ) ;
        (Client.member_update :=
           fun event ->
             Commands.update_cached_members
               Event.GuildMemberUpdate.(event.guild_id) ) ;
        (Client.member_leave :=
           fun event ->
             Commands.update_cached_members
               Event.GuildMemberRemove.(event.guild_id) ) ;
        (Client.member_join :=
           fun event -> Commands.update_cached_members Member.(event.guild_id)
        ) ;
        (Client.role_create :=
           fun event ->
             Commands.update_cached_roles Event.GuildRoleCreate.(event.guild_id)
        ) ;
        (Client.role_update :=
           fun event ->
             Commands.update_cached_roles Event.GuildRoleUpdate.(event.guild_id)
        ) ;
        (Client.role_delete :=
           fun event ->
             Commands.update_cached_roles Event.GuildRoleDelete.(event.guild_id)
        ) ;
        Client.message_create := execute_commands commands ;
        Client.reaction_add := Commands.update_score_add ;
        Client.reaction_remove := Commands.update_score_remove ;
        (* Start the client. It's recommended to load the token from an env var or other config file. *)
        let%map _client = Client.start Config.token in
        MLog.info "Connected successfully" )
  in
  match result with
  | Ok () ->
      Deferred.return ()
  | Error exn ->
      Exn.pp Format.err_formatter exn ;
      main ()

let main () = don't_wait_for (main ())

let _ =
  (* Launch the Async scheduler. You must do this for anything to work. *)
  Scheduler.go_main ~main ()
