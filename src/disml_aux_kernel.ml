open Async
open Disml
open Models

let guild_of_id (guild_id : Guild_id.t) =
  Cache.(
    let cache = Mvar.peek_exn cache in
    GuildMap.find_exn cache.guilds guild_id)
(*
let get_value = function
  | Ok v ->
      v
  | Error e ->
      (* Warning : very wrong *)
      let info = Error.to_info e in
      Info.pp Format.err_formatter info ;
      exit 1 *)

let message_of_id message_id channel_id =
    (Channel_id.get_message ~id:(Message_id.get_id message_id) channel_id)

let member_of_id user_id guild_id =
  let guild = guild_of_id guild_id in
  Guild.get_member ~id:user_id guild

let member_of_user user guild_id = member_of_id User.(user.id) guild_id