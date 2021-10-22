open Async
open Core
open Disml
module TmpMember = Member
open Models
module Member = TmpMember
open Disml_aux

let string_of_error e =
  Error.pp Format.str_formatter e ;
  Format.flush_str_formatter ()

let log_error reason error =
  MLog.error {%eml|<%-reason%> : <%-string_of_error error%>.|}

let logged_reply message reply =
  let Message.{content; _} = message in
  MLog.info {%eml|Replying to message <%S-content%> with <%S-reply%>.|} ;
  don't_wait_for
  @@ match%map Message.reply message reply with
     | Ok _ ->
         MLog.info "Reply succesful."
     | Error e ->
         log_error "during reply" e

let score_of_emoji (emoji : Emoji.partial_emoji) =
  match String.Map.find Config.reacts Emoji.(emoji.name) with
  | None ->
      0
  | Some score ->
      score

let score_message guild_id user =
  let%bind member = member_of_user user guild_id in
  let%map score = Data.score_of_id user.id guild_id in
  let module Let_syntax = Result in
  let%map member = member in
  {%eml|/!\ @everyone /!\ Score of <%- Member.ping_text member %> is <%i- score %>.|}

let score_messages guild_id users =
  let%map messages = Deferred.List.map ~f:(score_message guild_id) users in
  let messages = Or_error.combine_errors messages in
  let module Let_syntax = Result in
  let%map messages = messages in
  {%eml|<% List.iter (fun message -> %><%- message%>
<%) messages ;%>|}

let get_score message user =
  don't_wait_for
  @@
  let Message.{guild_id; _} = message in
  let guild_id = Option.value_exn guild_id in
  match%map score_message guild_id user with
  | Error e ->
      log_error "While getting score of single user" e
  | Ok reply ->
      logged_reply message reply

let get_score_of_author message = get_score message Message.(message.author)

let get_scores_of_mentions message =
  don't_wait_for
  @@
  let Message.{mentions; mention_roles; guild_id; _} = message in
  let guild_id = Option.value_exn guild_id in
  let%bind mentions_through_role =
    Deferred.List.map
      ~f:(fun role -> members_of_role_id role guild_id)
      mention_roles
  in
  let mentions_through_role =
    mentions_through_role |> Member.Set.union_list |> Member.Set.elements
    |> List.map ~f:(fun m -> Member.(m.user))
  in
  let mentions = List.append mentions mentions_through_role in
  match%map score_messages guild_id mentions with
  | Error e ->
      log_error "While getting score of mentionned users" e
  | Ok reply ->
      logged_reply message reply

let get_scores_of_everyone message =
  don't_wait_for
  @@
  let Message.{guild_id; _} = message in
  let guild_id = Option.value_exn guild_id in
  let%bind members = MCache.get_members guild_id in
  let members = Member.Set.elements members in
  let members = List.map ~f:(fun m -> m.user) members in
  match%map score_messages guild_id members with
  | Error e ->
      log_error "While getting score of everyone" e
  | Ok reply ->
      logged_reply message reply

let update_score ?(remove = false) user_id guild_id emoji message_id channel_id
    =
  don't_wait_for
  @@ match%map message_of_id message_id channel_id with
     | Error e ->
         log_error "While updating score" e
     | Ok message ->
         let user = Message.(message.author) in
         (* Reacts by the author of the message are not taken into account *)
         if Int.(User_id.get_id user_id <> User_id.get_id User.(user.id)) then
           let guild_id = Option.value_exn guild_id in
           let score = score_of_emoji emoji in
           let score = if remove then -score else score in
           Data.add_to_score user.id guild_id score |> don't_wait_for

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