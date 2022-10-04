open Core
open Common
open Disml
module TmpMember = Member
open Models
module Member = TmpMember
open Disml_aux

let do_with_cost ~guild_id ~action ~cost ~actor ~on_failure =
  let* score = Data.score_of_user guild_id actor in
  if score < cost then Lwt.return @@ on_failure ()
  else
    let* () = Data.add_to_score guild_id actor.id (-cost) in
    action ()
(*
let or_log ~f ~message_error =
  match f () with Ok () -> () | Error e -> MLog.error_t message_error e

let deferred_or_log ~f ~message_error =
  let+ match_variable = f () in match match_variable with Ok () -> () | Error e -> MLog.error_t message_error e *)

let update_cached_members guild_id = MCache.update_members guild_id

let update_cached_roles guild_id = MCache.update_roles guild_id

let score_of_pemoji (emoji : Emoji.partial_emoji) =
  match String.Map.find Config.reacts Emoji.(emoji.name) with
  | None ->
      0
  | Some score ->
      score

let score_of_emoji (emoji : Emoji.t) =
  match String.Map.find Config.reacts Emoji.(emoji.name) with
  | None ->
      0
  | Some score ->
      score
(*
let or_error_iter_both ~ok ~error v =
  match v with Error e -> error e | Ok v -> ok v *)

let score_message guild_id user score =
  let+ member = member_of_user guild_id user in
  let open Result_syntax in
  let+ member = member in
  {%eml|/!\ @everyone /!\ Score of <%- Member.ping_text member %> is <%i- score %>.|}

let score_message_of_user guild_id user =
  let* member = member_of_user guild_id user in
  let+ score = Data.score_of_id guild_id user.id in
  let open Result_syntax in
  let+ member = member in
  {%eml|/!\ @everyone /!\ Score of <%- Member.ping_text member %> is <%i- score %>.|}

let score_messages guild_id users =
  let* scored_users =
    Lwt_list.map_s
      (fun user -> Lwt.both (Lwt.return user) (Data.score_of_user guild_id user))
      users
  in
  let scored_users =
    List.sort
      ~compare:(fun (_user1, score1) (_user2, score2) ->
        -Int.compare score1 score2 )
      scored_users
  in
  let+ messages =
    Lwt_list.map_s
      (fun (user, score) -> score_message guild_id user score)
      scored_users
  in
  let messages = combine_errors messages in
  let open Result_syntax in
  let+ messages = messages in
  {%eml|<% List.iter (fun message -> %><%- message%>
<%) messages ;%>|}

let get_score message user =
  let Message.{guild_id; _} = message in
  let guild_id = Option.value_exn guild_id in
  let+ match_variable = score_message_of_user guild_id user in
  match match_variable with
  | Error e ->
      MLog.error_t "While getting score of single user" e
  | Ok reply ->
      logged_reply message reply

let get_score_of_author message = get_score message Message.(message.author)

let get_scores_of_mentions message =
  let Message.{mentions; mention_roles; guild_id; _} = message in
  let guild_id = Option.value_exn guild_id in
  let* mentions_through_role =
    Lwt_list.map_s (fun role -> members_of_role_id guild_id role) mention_roles
  in
  let mentions =
    mentions_through_role
    |> List.fold ~init:Member.Set.empty ~f:Member.Set.union
    |> Member.Set.elements
    |> List.map ~f:(fun m -> Member.(m.user))
    |> List.append mentions
  in
  let+ match_variable = score_messages guild_id mentions in
  match match_variable with
  | Error e ->
      MLog.error_t "While getting score of mentionned users" e
  | Ok reply ->
      logged_reply message reply

let get_scores_of_everyone message =
  let Message.{guild_id; _} = message in
  let guild_id = Option.value_exn guild_id in
  let* members = MCache.get_members guild_id in
  let members = Member.Set.elements members in
  let members = List.map ~f:(fun m -> m.user) members in
  let+ match_variable = score_messages guild_id members in
  match match_variable with
  | Error e ->
      MLog.error_t "While getting score of everyone" e
  | Ok reply ->
      logged_reply message reply

let get_smart_scores prefix message =
  let Message.{guild_id; content; _} = message in
  let prefix = Config.command_prefix ^ prefix in
  let prefix_size = String.length prefix in
  let content = String.chop_prefix_exn ~prefix content in
  let guild_id = Option.value_exn guild_id in
  match Rolelang.parse content with
  | Error (text, spos, epos) ->
      Lwt.return
      @@ logged_reply message
           {%eml|<%-text%> from char <%i- prefix_size + spos.pos_cnum%> to  <%i- prefix_size + epos.pos_cnum%>.|}
  | Ok rolelang_expr -> (
      let* mentions = Rolelang_interpretor.eval guild_id rolelang_expr in
      let mentions =
        mentions |> Member.Set.elements
        |> List.map ~f:(fun m -> Member.(m.user))
      in
      let+ match_variable = score_messages guild_id mentions in
      match match_variable with
      | Error e ->
          MLog.error_t "While getting score of mentionned users" e
      | Ok reply ->
          logged_reply message reply )

let update_score ?(remove = false) user_id guild_id emoji message_id channel_id
    =
  let* match_variable = message_of_id message_id channel_id in
  match match_variable with
  | Error e ->
      Lwt.return @@ MLog.error_t "While updating score" e
  | Ok message ->
      let user = Message.(message.author) in
      (* Reacts by the author of the message are not taken into account *)
      if Int.(User_id.get_id user_id <> User_id.get_id User.(user.id)) then
        let guild_id = Option.value_exn guild_id in
        let score = score_of_pemoji emoji in
        let score = if remove then -score else score in
        Data.add_to_score guild_id user.id score
      else Lwt.return ()

let update_score_add react =
  let Event.ReactionAdd.{user_id; guild_id; emoji; message_id; channel_id} =
    react
  in
  update_score user_id guild_id emoji message_id channel_id

let update_score_remove react =
  let Event.ReactionRemove.{user_id; guild_id; emoji; message_id; channel_id} =
    react
  in
  update_score ~remove:true user_id guild_id emoji message_id channel_id

let update_cache message =
  let Message.{guild_id; _} = message in
  let guild_id = Option.value_exn guild_id in
  MCache.update_all guild_id

let update_score_from_message guild_id add_score message =
  let reacts = Message.(message.reactions) in
  let score =
    List.fold_left ~init:0
      ~f:(fun acc react ->
        let Reaction.{emoji; count; _} = react in
        acc + (count * score_of_emoji emoji) )
      reacts
  in
  let+ match_variable = member_of_user guild_id Message.(message.author) in
  match match_variable with
  | Error e ->
      MLog.error_t "While getting author of message" e
  | Ok author ->
      add_score score author

let scores_from_history_channel guild_id add_score (channel : Channel.t) =
  match channel with
  | `GuildText text_channel ->
      let title = Channel.(text_channel.name) in
      MLog.info {%eml|Getting messages of channel <%-title%>|} ;
      let+ () =
        Message_history.iter_deferred
          ~f:(update_score_from_message guild_id add_score)
          channel
      in
      MLog.info {%eml|Messages of channel <%-title%> got.|}
  | _ ->
      Lwt.return ()

let scores_from_history guild_id =
  let* guild = guild_of_id guild_id in
  let channels = Guild.channels guild in
  let scores = Member.Hashtbl.create () in
  let add_score score member =
    let previous_score =
      Option.value ~default:0 @@ Member.Hashtbl.find scores member
    in
    let score = score + previous_score in
    Member.Hashtbl.set scores ~key:member ~data:score
  in
  let+ () =
    channels |> Lwt_list.iter_s (scores_from_history_channel guild_id add_score)
  in
  scores

let set_scores guild_id scores =
  Member.Hashtbl.fold ~init:(Lwt.return ()) scores ~f:(fun ~key ~data acc ->
      let* () = acc in
      Data.set_score guild_id key.user.id data )

let crunch_scores message =
  let guild_id = Option.value_exn Message.(message.guild_id) in
  let* scores = scores_from_history guild_id in
  set_scores guild_id scores

let stupid_message message =
  let+ match_variable = Message.delete message in
  match match_variable with
  | Error e ->
      MLog.error_t "While deleting stupid message" e
  | Ok () ->
      MLog.info "stupid message deleted"

let remove_roles roles member =
  let guild_id = Member.guild_id member in
  roles |> Array.to_list
  |> Lwt_list.iter_s (fun role ->
         let* match_variable = role_of_id guild_id (`Role_id role) in
         match match_variable with
         | None ->
             Lwt.return @@ MLog.error "Role not found"
         | Some role -> (
             let+ match_variable = Member.remove_role ~role member in
             match match_variable with
             | Error e ->
                 MLog.error_t "While removing role" e
             | Ok () ->
                 () ) )

let rank_members message =
  let guild_id = Option.value_exn Message.(message.guild_id) in
  let* members = MCache.get_members guild_id in
  let* members =
    members |> Member.Set.elements
    |> Lwt_list.map_s (fun member ->
           Lwt.both (Lwt.return member)
             (Data.score_of_user guild_id Member.(member.user)) )
    |> Lwt.map (List.sort ~compare:(fun (_, s1) (_, s2) -> compare s1 s2))
  in
  let n = List.length members in
  let n_ranks = Array.length Config.Roles.ranks in
  Lwt_list.iteri_s
    (fun i (member, _score) ->
      let role_id = `Role_id Config.Roles.ranks.(i * n_ranks / n) in
      let* role = role_of_id guild_id role_id in
      match role with
      | None ->
          Lwt.return @@ MLog.error "While trying to retrieve a role from its id"
      | Some role -> (
          let role_repr = Role.name role in
          if Member.has_role member role_id then Lwt.return ()
            (* @@ logged_reply message
                 {|/!\ @everyone /!\ <%- Member.ping_text member %> keeps rank <%- role_repr %>.|} *)
          else
            let* () = remove_roles Config.Roles.ranks member in
            let+ match_variable = Member.add_role ~role member in
            match match_variable with
            | Error e ->
                MLog.error_t "While adding role" e
            | Ok () ->
                logged_reply message
                  {%eml|/!\ @everyone /!\ <%- Member.ping_text member %> now has rank <%- role_repr %>.|}
          ) )
    members

let chance_of_delete chance message =
  if Float.(Random.float 1. < chance) then
    let+ match_variable = Message.delete message in
    match match_variable with
    | Error e ->
        MLog.error_t "While removing message" e
    | Ok () ->
        ()
  else Lwt.return_unit

let delete_message message =
  let+ match_variable = Message.delete message in
  match match_variable with
  | Error e ->
      MLog.error_t "While removing message" e
  | Ok () ->
      ()

let send_dm message =
  let Message.{guild_id; content; _} = message in
  let guild_id = Option.value_exn guild_id in
  let prefix = "ping" in
  let prefix = Config.command_prefix ^ prefix in
  let prefix_size = String.length prefix in
  let content = String.chop_prefix_exn ~prefix content in
  let* dms = MCache.get_dms guild_id in
  let roleexpr, answer_content =
    match String.lsplit2 content ~on:'=' with
    | None ->
        (content, "kikou tu as été pingué")
    | Some (roleexpr, answer_content) ->
        (roleexpr, answer_content)
  in
  let answer_content = answer_content ^ sprintf "\n%s" (Message.link message) in
  match Rolelang.parse roleexpr with
  | Error (text, spos, epos) ->
      Lwt.return
      @@ logged_reply message
           {%eml|<%-text%> from char <%i- prefix_size + spos.pos_cnum%> to  <%i- prefix_size + epos.pos_cnum%>.|}
  | Ok rolelang_expr -> (
      let* mentions = Rolelang_interpretor.eval guild_id rolelang_expr in
      let actor = Message.(message.author) in
      let* match_variable = member_of_user guild_id actor in
      match match_variable with
      | Error e ->
          Lwt.return @@ MLog.error_t "while converting user to member" e
      | Ok member ->
          let cost =
            if Member.has_role member (`Role_id Config.Roles.free_ping) then 0
            else Config.dm_ping_cost * Member.Set.cardinal mentions
          in
          do_with_cost ~guild_id ~cost ~actor
            ~on_failure:(fun () ->
              logged_reply message
                "Commence par regarder ton score dans les yeux avant de \
                 chercher a pinguer les autres." )
            ~action:(fun () ->
              mentions |> Member.Set.elements
              |> Lwt_list.iter_s (fun member ->
                     member |> Member.user
                     |> Fun.flip User.Map.find_opt dms
                     |> function
                     | None ->
                         let block_bot_cost = -10 in
                         logged_reply message
                           {%eml|<%- User.mention member.user %> se croit tout permis et bloque le bot. Bouuuu ! <%i- block_bot_cost %> points !|} ;
                         Data.add_to_score guild_id member.user.id
                           block_bot_cost
                     | Some channel -> (
                         let+ match_variable =
                           Channel.say answer_content channel
                         in
                         match match_variable with
                         | Error e ->
                             MLog.error_t "While sending dm" e
                         | Ok _ ->
                             () ) ) ) )

let change_nick message =
  let Message.{guild_id; content; _} = message in
  let prefix = "affuble" in
  let prefix = Config.command_prefix ^ prefix in
  let prefix_size = String.length prefix in
  let content = String.chop_prefix_exn ~prefix content in
  match String.lsplit2 content ~on:'=' with
  | None ->
      Lwt.return @@ logged_reply message "Did not find '=' in message"
  | Some (person, new_nick) -> (
      let guild_id = Option.value_exn guild_id in
      match Rolelang.parse person with
      | Error (text, spos, epos) ->
          Lwt.return
          @@ logged_reply message
               {%eml|<%-text%> from char <%i- prefix_size + spos.pos_cnum%> to  <%i- prefix_size + epos.pos_cnum%>.|}
      | Ok rolelang_expr -> (
          let* mentions = Rolelang_interpretor.eval guild_id rolelang_expr in
          let mentions =
            mentions |> Member.Set.elements
            |> List.map ~f:(fun m -> Member.(m.user))
          in
          match mentions with
          | [member] -> (
              let author = Message.(message.author) in
              if User.(compare author member = 0) then
                Lwt.return
                @@ logged_reply message
                     "Impossible de s'affubler soi-même, essaye plutot de \
                      pleurer chez Coutarel."
              else
                let* match_variable = member_of_user guild_id member in
                match match_variable with
                | Error e ->
                    Lwt.return
                    @@ MLog.error_t "while converting user to member" e
                | Ok member ->
                    let cost =
                      if
                        Member.has_role member
                          (`Role_id Config.Roles.free_rename)
                      then 0
                      else Config.change_nick_cost
                    in
                    do_with_cost ~guild_id ~cost ~actor:author
                      ~on_failure:(fun () ->
                        logged_reply message
                          "Commence par regarder ton score dans les yeux avant \
                           de chercher a affubler les autres." )
                      ~action:(fun () ->
                        let* () =
                          Data.add_to_score guild_id author.id
                            (-Config.change_nick_cost)
                        in
                        let+ match_variable =
                          Member.change_nick member new_nick
                        in
                        match match_variable with
                        | Ok () ->
                            logged_reply message
                              "Tu as bien affublé ce gros bouffon."
                        | Error e ->
                            MLog.error_t "while affubling" e ) )
          | _ :: _ ->
              Lwt.return
              @@ logged_reply message
                   "Can only change the nickname of one member, you mentionned \
                    multiple."
          | [] ->
              Lwt.return
              @@ logged_reply message
                   "Can only change the nickname of one member, you mentionned \
                    zero" ) )
