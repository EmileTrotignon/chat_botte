
open Common
open Disml
open Models
let guild_of_id (guild_id : Guild_id.t) =
  Cache.(
    let+ cache = read_copy cache in
    Guild_id.Map.find guild_id cache.guilds )