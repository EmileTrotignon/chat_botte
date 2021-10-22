open Core
open Async
open Disml
module TmpMember = Member
open Models
module Member = TmpMember


let guild_of_id (guild_id : Guild_id.t) =
  Cache.(
    let cache = Mvar.peek_exn cache in
    GuildMap.find_exn cache.guilds guild_id)

let members : (Member.Set.t, read_write) Mvar.t = Mvar.create ()

let update guild_id =
  let guild = guild_of_id guild_id in
  let%map new_members = Guild.request_members guild in
  match new_members with
  | Ok new_members ->
      MLog.info
        {%eml|Successfully queryed <%i- List.length new_members %> members.|} ;
      let new_members = Member.Set.of_list new_members in
      Mvar.set members new_members
  | Error e ->
      Error.pp Format.str_formatter e ;
      let str = Buffer.contents Format.stdbuf in
      Buffer.reset Format.stdbuf ;
      MLog.error {%eml|Could not read error : <%-str%>|}

let get_members guild_id =
  match Mvar.take_now members with
  | Some members ->
      Deferred.return members
  | None ->
      don't_wait_for (update guild_id) ;
      Mvar.take members
