open Core
open Disml
module TmpMember = Member
open Models
module Member = TmpMember
open Async
open Disml_aux
open Letop.Deferred

(* Create a function to handle message_create. *)

let commands help =
  Message_command.
    [ v
        (And [ExactPrefix "update cache"; Admin])
        "Mets a jour le cache" Commands.update_cache
    ; v
        (And [ExactPrefix "crunch"; Admin])
        "Remonte tout les messages pour calculer le score"
        Commands.crunch_scores
    ; v (ExactPrefix "score") "Affiche ton score" Commands.get_score_of_author
    ; v (ExactPrefix "rank") "Mets a jour les rangs" Commands.rank_members
    ; v (Prefix "score") "Affiche le score des sélectionnés"
        (Commands.get_smart_scores "score")
    ; v (Prefix "affuble") "Affuble un bouffon d'un surnom ridicule"
        Commands.change_nick
    ; v
        (HasRole (`Role_id Config.Roles.warning))
        "Supprime le message avec proba .33"
        (Commands.chance_of_delete 0.33)
    ; v (Prefix "ping") "Pingue les heureux élus" Commands.send_dm
    ; v (ExactPrefix "help") "Affiche l'aide" help ]

let rec help m = logged_reply m (Message_command.to_string (commands help))

let commands = commands help

let hidden_commands =
  Message_command.
    [ v (Substring "gJirxeFwVzA") "Serge ..." Commands.delete_message
    ; v (Substring "GASPAR") "Serge ..." Commands.delete_message
    ; v (Substring "CANAR") "Serge ..." Commands.delete_message ]

let commands = commands @ hidden_commands

let execute_commands commands message =
  let Message.{content; author; guild_id; _} = message in
  guild_id
  |> Option.iter ~f:(fun guild_id ->
         don't_wait_for
         @@ let+ is_admin = user_is_admin guild_id author in
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
    [ v
        (HasRole (`Role_id Config.Roles.edit_punished))
        "Supprime tout de suite" Commands.delete_message ]

let main () =
  (Client.message_update :=
     fun update ->
       don't_wait_for
       @@ let* message =
            Event.MessageUpdate.(message_of_id update.id update.channel_id)
          in
          (* Yojson.Safe.pretty_to_channel stdout
             (Event.MessageUpdate.to_yojson update) ; *)
          match message with
          | Ok message ->
              let message =
                {message with guild_id= Event.MessageUpdate.(update.guild_id)}
              in
              (* Yojson.Safe.pretty_to_channel stdout (Message.to_yojson message) ; *)
              execute_commands commands_edit message
          | Error e ->
              MLog.error_t "while convering message update to message" e ) ;
  (Client.member_update :=
     fun event ->
       Commands.update_cached_members Event.GuildMemberUpdate.(event.guild_id)
  ) ;
  (Client.member_leave :=
     fun event ->
       Commands.update_cached_members Event.GuildMemberRemove.(event.guild_id)
  ) ;
  (Client.member_join :=
     fun event -> Commands.update_cached_members Member.(event.guild_id) ) ;
  (Client.role_create :=
     fun event ->
       Commands.update_cached_roles Event.GuildRoleCreate.(event.guild_id) ) ;
  (Client.role_update :=
     fun event ->
       Commands.update_cached_roles Event.GuildRoleUpdate.(event.guild_id) ) ;
  (Client.role_delete :=
     fun event ->
       Commands.update_cached_roles Event.GuildRoleDelete.(event.guild_id) ) ;
  Client.message_create := execute_commands commands ;
  Client.reaction_add := Commands.update_score_add ;
  Client.reaction_remove := Commands.update_score_remove ;
  (* Start the client. It's recommended to load the token from an env var or other config file. *)
  let* _client = Client.start Config.token in
  MLog.info "Connected successfully"

let main () = don't_wait_for (main ())

let _ =
  (* Launch the Async scheduler. You must do this for anything to work. *)
  Scheduler.go_main ~main ()
