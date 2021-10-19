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

let logged_reply message reply =
  let Message.{content; _} = message in
  MLog.info @@ sprintf "Replying to message %S with %S." content reply ;
  Message.reply message reply
  >>> function
  | Ok _ ->
      MLog.info "Reply succesful."
  | Error e ->
      MLog.error @@ sprintf "Could not reply : %s." (string_of_error e)

let score_of_emoji (emoji : Emoji.partial_emoji) =
  match String.Map.find Config.reacts Emoji.(emoji.name) with
  | None ->
      0
  | Some score ->
      score

let get_score message =
  let Message.{author; guild_id; _} = message in
  let guild_id = Option.value_exn guild_id in
  logged_reply message
    (sprintf "Okay, querying score of %s" User.(author.username))
  |> ignore ;
  let member = member_of_user author guild_id in
  member
  >>> fun member ->
  let nick = Option.value ~default:author.username member.nick in
  Data.score member
  >>> fun score ->
  let reply = sprintf {|/!\ @everyone /!\ Score of <@%s> is %d.|} nick score in
  logged_reply message reply

let update_score ?(remove = false) user_id guild_id emoji message_id channel_id
    =
  message_of_id message_id channel_id
  >>> fun message ->
  let user = Message.(message.author) in
  (* Reacts by the author of the message are not taken into account *)
  if Int.(User_id.get_id user_id <> User_id.get_id User.(user.id)) then
    let guild_id = Option.value_exn guild_id in
    let member = member_of_user user guild_id in
    let score = score_of_emoji emoji in
    let score = if remove then -score else score in
    member
    >>> fun member -> Data.add_to_score member score |> Async.don't_wait_for

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
