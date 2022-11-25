open Async
open Disml
open Models

let guild_of_id (guild_id : Guild_id.t) =
  Cache.(
    let cache = Mvar.peek_exn cache in
    Guild_id.Map.find_exn cache.guilds guild_id )
