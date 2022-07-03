open Async
open Core
open Disml
module TmpMember = Member
open Models
module Member = TmpMember
open Disml_aux

let update_cached_members guild_id =
  don't_wait_for (MCache.update_members guild_id)

let update_cached_roles guild_id = don't_wait_for (MCache.update_roles guild_id)

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
  let%map member = member_of_user guild_id user in
  let module Let_syntax = Result in
  let%map member = member in
  {%eml|/!\ @everyone /!\ Score of <%- Member.ping_text member %> is <%i- score %>.|}

let score_message_of_user guild_id user =
  let%bind member = member_of_user guild_id user in
  let%map score = Data.score_of_id guild_id user.id in
  let module Let_syntax = Result in
  let%map member = member in
  {%eml|/!\ @everyone /!\ Score of <%- Member.ping_text member %> is <%i- score %>.|}

let score_messages guild_id users =
  let%bind scored_users =
    Deferred.List.map
      ~f:(fun user ->
        Deferred.both (Deferred.return user) (Data.score_of_user guild_id user)
        )
      users
  in
  let scored_users =
    List.sort
      ~compare:(fun (_user1, score1) (_user2, score2) ->
        -Int.compare score1 score2 )
      scored_users
  in
  let%map messages =
    Deferred.List.map
      ~f:(fun (user, score) -> score_message guild_id user score)
      scored_users
  in
  let messages = Or_error.combine_errors messages in
  let module Let_syntax = Result in
  let%map messages = messages in
  {%eml|<% List.iter (fun message -> %><%- message%>
<%) messages ;%>|}

let get_score message user =
  (let Message.{guild_id; _} = message in
   let guild_id = Option.value_exn guild_id in
   match%map score_message_of_user guild_id user with
   | Error e ->
       MLog.error_t "While getting score of single user" e
   | Ok reply ->
       logged_reply message reply )
  |> don't_wait_for

let get_score_of_author message = get_score message Message.(message.author)

let get_scores_of_mentions message =
  (let Message.{mentions; mention_roles; guild_id; _} = message in
   let guild_id = Option.value_exn guild_id in
   let%bind mentions_through_role =
     Deferred.List.map
       ~f:(fun role -> members_of_role_id guild_id role)
       mention_roles
   in
   let mentions =
     mentions_through_role |> Member.Set.union_list |> Member.Set.elements
     |> List.map ~f:(fun m -> Member.(m.user))
     |> List.append mentions
   in
   match%map score_messages guild_id mentions with
   | Error e ->
       MLog.error_t "While getting score of mentionned users" e
   | Ok reply ->
       logged_reply message reply )
  |> don't_wait_for

let get_scores_of_everyone message =
  (let Message.{guild_id; _} = message in
   let guild_id = Option.value_exn guild_id in
   let%bind members = MCache.get_members guild_id in
   let members = Member.Set.elements members in
   let members = List.map ~f:(fun m -> m.user) members in
   match%map score_messages guild_id members with
   | Error e ->
       MLog.error_t "While getting score of everyone" e
   | Ok reply ->
       logged_reply message reply )
  |> don't_wait_for

let get_smart_scores prefix message =
  (let Message.{guild_id; content; _} = message in
   let prefix = Config.command_prefix ^ prefix in
   let prefix_size = String.length prefix in
   let content = String.chop_prefix_exn ~prefix content in
   let guild_id = Option.value_exn guild_id in
   match Rolelang.parse content with
   | Error (text, spos, epos) ->
       Deferred.return
       @@ logged_reply message
            {%eml|<%-text%> from char <%i- prefix_size + spos.pos_cnum%> to  <%i- prefix_size + epos.pos_cnum%>.|}
   | Ok rolelang_expr -> (
       let%bind mentions = Rolelang_interpretor.eval guild_id rolelang_expr in
       let mentions =
         mentions |> Member.Set.elements
         |> List.map ~f:(fun m -> Member.(m.user))
       in
       match%map score_messages guild_id mentions with
       | Error e ->
           MLog.error_t "While getting score of mentionned users" e
       | Ok reply ->
           logged_reply message reply ) )
  |> don't_wait_for

let update_score ?(remove = false) user_id guild_id emoji message_id channel_id
    =
  ( match%bind message_of_id message_id channel_id with
  | Error e ->
      Deferred.return @@ MLog.error_t "While updating score" e
  | Ok message ->
      let user = Message.(message.author) in
      (* Reacts by the author of the message are not taken into account *)
      if Int.(User_id.get_id user_id <> User_id.get_id User.(user.id)) then
        let guild_id = Option.value_exn guild_id in
        let score = score_of_pemoji emoji in
        let score = if remove then -score else score in
        Data.add_to_score guild_id user.id score
      else Deferred.return () )
  |> don't_wait_for

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
  (let Message.{guild_id; _} = message in
   let guild_id = Option.value_exn guild_id in
   MCache.update_all guild_id )
  |> don't_wait_for

let update_score_from_message guild_id add_score message =
  let reacts = Message.(message.reactions) in
  let score =
    List.fold_left ~init:0
      ~f:(fun acc react ->
        let Reaction.{emoji; count; _} = react in
        acc + (count * score_of_emoji emoji) )
      reacts
  in
  match%map member_of_user guild_id Message.(message.author) with
  | Error e ->
      MLog.error_t "While getting author of message" e
  | Ok author ->
      add_score score author

let scores_from_history_channel guild_id add_score (channel : Channel.t) =
  match channel with
  | `GuildText text_channel ->
      let title = Channel.(text_channel.name) in
      MLog.info {%eml|Getting messages of channel <%-title%>|} ;
      let%map () =
        Message_history.iter_deferred
          ~f:(update_score_from_message guild_id add_score)
          channel
      in
      MLog.info {%eml|Messages of channel <%-title%> got.|}
  | _ ->
      Deferred.return ()

let scores_from_history guild_id =
  let guild = guild_of_id guild_id in
  let channels = guild.channels in
  let scores = Member.Hashtbl.create () in
  let add_score score member =
    let previous_score =
      Option.value ~default:0 @@ Member.Hashtbl.find scores member
    in
    let score = score + previous_score in
    Member.Hashtbl.set scores ~key:member ~data:score
  in
  let%map () =
    channels
    |> Deferred.List.iter ~f:(scores_from_history_channel guild_id add_score)
  in
  scores

let set_scores guild_id scores =
  Member.Hashtbl.fold ~init:(Deferred.return ()) scores
    ~f:(fun ~key ~data acc ->
      let%bind () = acc in
      Data.set_score guild_id key.user.id data )

let crunch_scores message =
  let guild_id = Option.value_exn Message.(message.guild_id) in
  (let%bind scores = scores_from_history guild_id in
   set_scores guild_id scores )
  |> don't_wait_for

let stupid_message message =
  ( match%map Message.delete message with
  | Error e ->
      MLog.error_t "While deleting stupid message" e
  | Ok () ->
      MLog.info "stupid message deleted" )
  |> don't_wait_for

let remove_roles roles member =
  let guild_id = Member.guild_id member in
  Deferred.Array.iter
    ~f:(fun role ->
      match%bind role_of_id guild_id (`Role_id role) with
      | None ->
          Deferred.return @@ MLog.error "Role not found"
      | Some role -> (
          match%map Member.remove_role ~role member with
          | Error e ->
              MLog.error_t "While removing role" e
          | Ok () ->
              () ) )
    roles

let rank_members message =
  let guild_id = Option.value_exn Message.(message.guild_id) in
  don't_wait_for
  @@ let%bind members = MCache.get_members guild_id in
     let%map members =
       members |> Member.Set.elements
       |> Deferred.List.map ~f:(fun member ->
              Deferred.both (Deferred.return member)
                (Data.score_of_user guild_id Member.(member.user)) )
       |> Deferred.map
            ~f:(List.sort ~compare:(fun (_, s1) (_, s2) -> compare s1 s2))
     in
     let n = List.length members in
     let n_ranks = Array.length Config.Roles.ranks in
     List.iteri
       ~f:(fun i (member, _score) ->
         (let role_id = `Role_id Config.Roles.ranks.(i * n_ranks / n) in
          let%bind role = role_of_id guild_id role_id in
          match role with
          | None ->
              Deferred.return
              @@ MLog.error "While trying to retrieve a role from its id"
          | Some role -> (
              let role_repr = Role.name role in
              if Member.has_role member role_id then Deferred.return ()
                (* @@ logged_reply message
                     {|/!\ @everyone /!\ <%- Member.ping_text member %> keeps rank <%- role_repr %>.|} *)
              else
                let%bind () = remove_roles Config.Roles.ranks member in
                match%map Member.add_role ~role member with
                | Error e ->
                    MLog.error_t "While adding role" e
                | Ok () ->
                    logged_reply message
                      {%eml|/!\ @everyone /!\ <%- Member.ping_text member %> now has rank <%- role_repr %>.|}
              ) )
         |> don't_wait_for )
       members

let chance_of_delete chance message =
  if Float.(Random.float 1. < chance) then
    don't_wait_for
    @@ match%map Message.delete message with
       | Error e ->
           MLog.error_t "While removing message" e
       | Ok () ->
           ()

let delete_message message =
  don't_wait_for
  @@ match%map Message.delete message with
     | Error e ->
         MLog.error_t "While removing message" e
     | Ok () ->
         ()

let do_with_cost ~guild_id ~action ~cost ~actor ~on_failure =
  let%bind score = Data.score_of_user guild_id actor in
  if score < cost then on_failure ()
  else
    let%map () = Data.add_to_score guild_id actor.id (-cost) in
    action ()

let send_dm message =
  (let Message.{guild_id; content; _} = message in
   let guild_id = Option.value_exn guild_id in
   let prefix = "ping" in
   let prefix = Config.command_prefix ^ prefix in
   let prefix_size = String.length prefix in
   let content = String.chop_prefix_exn ~prefix content in
   let%bind dms = MCache.get_dms guild_id in
   match Rolelang.parse content with
   | Error (text, spos, epos) ->
       Deferred.return
       @@ logged_reply message
            {%eml|<%-text%> from char <%i- prefix_size + spos.pos_cnum%> to  <%i- prefix_size + epos.pos_cnum%>.|}
   | Ok rolelang_expr ->
       let%bind mentions = Rolelang_interpretor.eval guild_id rolelang_expr in
       let actor = Message.(message.author) in
       do_with_cost ~guild_id ~cost:Config.dm_ping_cost ~actor
         ~on_failure:(fun () ->
           Deferred.return
           @@ logged_reply message
                "Commence par regarder ton score dans les yeux avant de \
                 chercher a pinguer les autres." )
         ~action:(fun () ->
           Member.Set.iter mentions ~f:(fun member ->
               member |> Member.user |> User.Map.find dms
               |> Option.iter ~f:(fun channel ->
                      ( match%map
                          Channel.say "kikou tu as été pingué" channel
                        with
                      | Error e ->
                          MLog.error_t "While sending dm" e
                      | Ok _ ->
                          () )
                      |> don't_wait_for ) ) ) )
  |> don't_wait_for

let change_nick message =
  (let Message.{guild_id; content; _} = message in
   let prefix = "affuble" in
   let prefix = Config.command_prefix ^ prefix in
   let prefix_size = String.length prefix in
   let content = String.chop_prefix_exn ~prefix content in
   match String.lsplit2 content ~on:'=' with
   | None ->
       Deferred.return @@ logged_reply message "Did not find '=' in message"
   | Some (new_nick, content) -> (
       let guild_id = Option.value_exn guild_id in
       match Rolelang.parse content with
       | Error (text, spos, epos) ->
           Deferred.return
           @@ logged_reply message
                {%eml|<%-text%> from char <%i- prefix_size + spos.pos_cnum%> to  <%i- prefix_size + epos.pos_cnum%>.|}
       | Ok rolelang_expr -> (
           let%bind mentions =
             Rolelang_interpretor.eval guild_id rolelang_expr
           in
           let mentions =
             mentions |> Member.Set.elements
             |> List.map ~f:(fun m -> Member.(m.user))
           in
           match mentions with
           | [member] -> (
               match%bind member_of_user guild_id member with
               | Error e ->
                   Deferred.return
                   @@ MLog.error_t "while converting user to user" e
               | Ok member -> (
                   let author = Message.(message.author) in
                   let%bind score = Data.score_of_user guild_id author in
                   if score < Config.change_nick_cost then
                     Deferred.return
                     @@ logged_reply message
                          "Commence par regarder ton score dans les yeux avant \
                           de chercher a affubler les autres."
                   else
                     let%bind () =
                       Data.add_to_score guild_id author.id
                         (-Config.change_nick_cost)
                     in
                     match%map Member.change_nick member new_nick with
                     | Ok () ->
                         logged_reply message
                           "Tu as bien affublé ce gros bouffon."
                     | Error e ->
                         MLog.error_t "while affubling" e ) )
           | _ :: _ ->
               Deferred.return
               @@ logged_reply message
                    "Can only change the nickname of one member, you \
                     mentionned multiple."
           | [] ->
               Deferred.return
               @@ logged_reply message
                    "Can only change the nickname of one member, you \
                     mentionned zero" ) ) )
  |> don't_wait_for
