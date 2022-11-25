open Async
open Core
open Disml
module TmpMember = Member
open Models
module Member = TmpMember
open Disml_aux
open Letop.Deferred

type t = Message.t -> unit Deferred.Or_error.t

let log_don't_wait command arg =
  (let* r = command arg in
   match r with Ok () -> () | Error e -> MLog.error_t "Command" e )
  |> don't_wait_for

let option_of_result ~msg r =
  match r with Ok v -> Some v | Error e -> MLog.error_t msg e ; None

let result_of_option = function
  | Some v ->
      Ok v
  | None ->
      Error (Error.of_string "Option was None")

let rolelang_parse ~prefix_size content =
  Result.map_error (Rolelang.parse content) ~f:(fun (text, spos, epos) ->
      Error.of_string
        {%eml|<%-text%> from char <%i- prefix_size + spos.pos_cnum%> to  <%i- prefix_size + epos.pos_cnum%>.|} )

let do_with_cost ~guild_id ~action ~cost ~actor ~on_failure =
  let open Letop.Deferred_or_error in
  let+ score = Deferred.return @@ Data.score_of_user guild_id (User.id actor) in
  if score < cost then on_failure ()
  else
    let+ () =
      Deferred.return @@ Data.add_to_score guild_id (User.id actor) (-cost)
    in
    action ()

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
  let* member = member_of_user guild_id user in
  let open Letop.Result in
  let* member = member in
  {%eml|/!\ @everyone /!\ Score of <%- Member.ping_text member %> is <%i- score %>.|}

let score_message_of_user guild_id user =
  let* member = member_of_user guild_id user in
  let open Letop.Result in
  let+ score = Data.score_of_user guild_id user.id in
  let* member = member in
  {%eml|/!\ @everyone /!\ Score of <%- Member.ping_text member %> is <%i- score %>.|}

let score_messages guild_id users =
  let msg = "score_messages" in
  let scored_users =
    List.filter_map
      ~f:(fun user ->
        let open Letop.Option in
        let* score =
          Data.score_of_user guild_id (User.id user) |> option_of_result ~msg
        in
        (user, score) )
      users
  in
  let scored_users =
    List.sort
      ~compare:(fun (_user1, score1) (_user2, score2) ->
        -Int.compare score1 score2 )
      scored_users
  in
  let* messages =
    Deferred.List.map
      ~f:(fun (user, score) -> score_message guild_id user score)
      scored_users
  in
  let messages = Or_error.combine_errors messages in
  let open Letop.Result in
  let* messages = messages in
  {%eml|<% List.iter (fun message -> %><%- message%>
<%) messages ;%>|}

let get_score message user : unit Deferred.Or_error.t =
  let Message.{guild_id; _} = message in
  let open Letop.Deferred_or_error in
  let+ guild_id = Deferred.return @@ result_of_option guild_id in
  let+ reply = score_message_of_user guild_id user in
  long_reply message reply

let get_score_of_author message = get_score message Message.(message.author)

let get_scores_of_mentions message =
  let Message.{mentions; mention_roles; guild_id; _} = message in
  let open Letop.Deferred_or_error in
  let+ guild_id = Deferred.return @@ result_of_option guild_id in
  let open Letop.Deferred in
  let+ mentions_through_role =
    Deferred.List.map
      ~f:(fun role -> members_of_role_id guild_id role)
      mention_roles
  in
  let mentions =
    mentions_through_role |> Member.Set.union_list |> Member.Set.elements
    |> List.map ~f:(fun m -> Member.(m.user))
    |> List.append mentions
  in
  let open Letop.Deferred_or_error in
  let+ reply = score_messages guild_id mentions in
  long_reply message reply

let get_scores_of_everyone message =
  let Message.{guild_id; _} = message in
  let open Letop.Deferred_or_error in
  let+ guild_id = Deferred.return @@ result_of_option guild_id in
  let open Letop.Deferred in
  let+ members = MCache.get_members guild_id in
  let members = Member.Set.elements members in
  let members = List.map ~f:(fun m -> m.user) members in
  let open Letop.Deferred_or_error in
  let+ reply = score_messages guild_id members in
  long_reply message reply

let get_smart_scores prefix message =
  let Message.{guild_id; content; _} = message in
  let prefix = Config.command_prefix ^ prefix in
  let prefix_size = String.length prefix in
  let content = String.chop_prefix_exn ~prefix content in
  let open Letop.Deferred_or_error in
  let+ guild_id = Deferred.return @@ result_of_option guild_id in
  let+ rolelang_expr = Deferred.return @@ rolelang_parse ~prefix_size content in
  let open Letop.Deferred in
  let+ mentions = Rolelang_interpretor.eval guild_id rolelang_expr in
  let mentions =
    mentions |> Member.Set.elements |> List.map ~f:(fun m -> Member.(m.user))
  in
  let open Letop.Deferred_or_error in
  let+ reply = score_messages guild_id mentions in
  long_reply message reply

let update_score ?(remove = false) user_id guild_id emoji message_id channel_id
    =
  let open Letop.Deferred_or_error in
  let+ message = message_of_id message_id channel_id in
  let user = Message.(message.author) in
  (* Reacts by the author of the message are not taken into account *)
  if Int.(User_id.get_id user_id <> User_id.get_id User.(user.id)) then
    let guild_id = Option.value_exn guild_id in
    let score = score_of_pemoji emoji in
    let score = if remove then -score else score in
    Deferred.return @@ Data.add_to_score guild_id user.id score
  else Deferred.Or_error.return ()

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
  let open Letop.Deferred_or_error in
  let+ guild_id = Deferred.return @@ result_of_option guild_id in
  let open Letop.Deferred in
  let* () = MCache.update_all guild_id in
  Ok ()

let update_score_from_message guild_id add_score message =
  let reacts = Message.(message.reactions) in
  let score =
    List.fold_left ~init:0
      ~f:(fun acc react ->
        let Reaction.{emoji; count; _} = react in
        acc + (count * score_of_emoji emoji) )
      reacts
  in
  let open Letop.Deferred_or_error in
  let+ author = member_of_user guild_id Message.(message.author) in
  add_score score author

let scores_from_history_channel guild_id add_score (channel : Channel.t) =
  let open Letop.Deferred_or_error in
  match channel with
  | `GuildText text_channel ->
      let title = Channel.(text_channel.name) in
      MLog.info {%eml|Getting messages of channel <%-title%>|} ;
      let* () =
        Message_history.iter_deferred
          ~f:(update_score_from_message guild_id add_score)
          channel
      in
      MLog.info {%eml|Messages of channel <%-title%> got.|}
  | _ ->
      Deferred.return @@ Ok ()

let scores_from_history guild_id =
  let open Letop.Deferred_or_error in
  let guild = guild_of_id guild_id in
  let channels = guild.channels in
  let scores = Member.Hashtbl.create () in
  let add_score score member =
    let previous_score =
      Option.value ~default:0 @@ Member.Hashtbl.find scores member
    in
    let score = score + previous_score in
    Deferred.Or_error.return
    @@ Member.Hashtbl.set scores ~key:member ~data:score
  in
  let* () =
    channels
    |> Deferred.Or_error.List.iter
         ~f:(scores_from_history_channel guild_id add_score)
  in
  scores

let set_scores guild_id scores =
  Member.Hashtbl.iteri scores ~f:(fun ~key ~data ->
      match Data.set_score guild_id key.user.id data with
      | Error e ->
          MLog.error_t "While setting score" e
      | Ok () ->
          () )

let crunch_scores message =
  let open Letop.Deferred_or_error in
  let guild_id = Option.value_exn Message.(message.guild_id) in
  let* scores = scores_from_history guild_id in
  set_scores guild_id scores

let delete_message message = Message.delete message

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
  let msg = "rank_members" in
  let guild_id = Option.value_exn Message.(message.guild_id) in
  let+ members = MCache.get_members guild_id in
  let members =
    members |> Member.Set.to_list
    |> List.filter_map ~f:(fun member ->
           let open Letop.Option in
           let* score =
             Data.score_of_user guild_id (member |> Member.user |> User.id)
             |> option_of_result ~msg
           in
           (member, score) )
    |> List.sort ~compare:(fun (_, s1) (_, s2) -> compare s1 s2)
  in
  let n = List.length members in
  let n_ranks = Array.length Config.Roles.ranks in
  Deferred.Or_error.List.iteri ~how:`Parallel
    ~f:(fun i (member, _score) ->
      let role_id = `Role_id Config.Roles.ranks.(i * n_ranks / n) in
      let open Letop.Deferred in
      let+ role = role_of_id guild_id role_id in
      let open Letop.Deferred_or_error in
      let+ role = Deferred.return @@ result_of_option @@ role in
      let role_repr = Role.name role in
      if Member.has_role member role_id then Deferred.return @@ Ok ()
        (* long_reply message
             {|/!\ @everyone /!\ <%- Member.ping_text member %> keeps rank <%- role_repr %>.|} *)
      else
        let open Letop.Deferred in
        let+ () = remove_roles Config.Roles.ranks member in
        let open Letop.Deferred_or_error in
        let+ () = Member.add_role ~role member in
        long_reply message
          {%eml|/!\ @everyone /!\ <%- Member.ping_text member %> now has rank <%- role_repr %>.|}
      )
    members

let chance_of_delete chance message =
  if Float.(Random.float 1. < chance) then Message.delete message
  else Deferred.Or_error.return ()

let send_dm message =
  let Message.{guild_id; content; _} = message in
  let guild_id = Option.value_exn guild_id in
  let prefix = "ping" in
  let prefix = Config.command_prefix ^ prefix in
  let prefix_size = String.length prefix in
  let content = String.chop_prefix_exn ~prefix content in
  let+ dms = MCache.get_dms guild_id in
  let roleexpr, answer_content =
    match String.lsplit2 content ~on:'=' with
    | None ->
        (content, "kikou tu as été pingué")
    | Some (roleexpr, answer_content) ->
        (roleexpr, answer_content)
  in
  let answer_content = answer_content ^ sprintf "\n%s" (Message.link message) in
  let open Letop.Deferred_or_error in
  let+ rolelang_expr =
    Deferred.return @@ rolelang_parse ~prefix_size roleexpr
  in
  let open Letop.Deferred in
  let+ mentions = Rolelang_interpretor.eval guild_id rolelang_expr in
  let actor = Message.(message.author) in
  let open Letop.Deferred_or_error in
  let+ member = member_of_user guild_id actor in
  let cost =
    if Member.has_role member (`Role_id Config.Roles.free_ping) then 0
    else Config.dm_ping_cost * Member.Set.length mentions
  in
  do_with_cost ~guild_id ~cost ~actor
    ~on_failure:(fun () ->
      long_reply message
        "Commence par regarder ton score dans les yeux avant de chercher a \
         pinguer les autres." )
    ~action:(fun () ->
      mentions |> Member.Set.elements
      |> Deferred.Or_error.List.iter ~f:(fun member ->
             member |> Member.user |> User.Map.find dms
             |> function
             | None ->
                 Deferred.return @@ Ok ()
             | Some channel ->
                 let* _ = Channel.say answer_content channel in
                 () ) )

let change_nick message =
  let Message.{guild_id; content; _} = message in
  let prefix = "affuble" in
  let prefix = Config.command_prefix ^ prefix in
  let prefix_size = String.length prefix in
  let content = String.chop_prefix_exn ~prefix content in
  match String.lsplit2 content ~on:'=' with
  | None ->
      long_reply message "Did not find '=' in message"
  | Some (person, new_nick) -> (
      let guild_id = Option.value_exn guild_id in
      match Rolelang.parse person with
      | Error (text, spos, epos) ->
          long_reply message
            {%eml|<%-text%> from char <%i- prefix_size + spos.pos_cnum%> to  <%i- prefix_size + epos.pos_cnum%>.|}
      | Ok rolelang_expr -> (
          let open Letop.Deferred in
          let+ mentions = Rolelang_interpretor.eval guild_id rolelang_expr in
          let mentions =
            mentions |> Member.Set.elements
            |> List.map ~f:(fun m -> Member.(m.user))
          in
          let open Letop.Deferred_or_error in
          match mentions with
          | [member] ->
              let author = Message.(message.author) in
              if User.(compare author member = 0) then
                long_reply message
                  "Impossible de s'affubler soi-même, essaye plutot de pleurer \
                   chez Coutarel."
              else
                let+ member = member_of_user guild_id member in
                let cost =
                  if Member.has_role member (`Role_id Config.Roles.free_rename)
                  then 0
                  else Config.change_nick_cost
                in
                do_with_cost ~guild_id ~cost ~actor:author
                  ~on_failure:(fun () ->
                    long_reply message
                      "Commence par regarder ton score dans les yeux avant de \
                       chercher a affubler les autres." )
                  ~action:(fun () ->
                    let+ () =
                      Deferred.return
                      @@ Data.add_to_score guild_id author.id
                           (-Config.change_nick_cost)
                    in
                    let+ () = Member.change_nick member new_nick in
                    long_reply message "Tu as bien affublé ce gros bouffon." )
          | _ :: _ ->
              long_reply message
                "Can only change the nickname of one member, you mentionned \
                 multiple."
          | [] ->
              long_reply message
                "Can only change the nickname of one member, you mentionned \
                 zero" ) )
