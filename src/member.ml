open Core
open Disml
open Models

module Self = struct
  include Member

  let ( = ) (m1 : t) (m2 : t) =
    Int.(
      Guild_id.get_id m1.guild_id = Guild_id.get_id m2.guild_id
      && User_id.get_id m1.user.id = User_id.get_id m2.user.id)

  let compare (m1 : t) (m2 : t) =
    Int.(
      let comp_guild =
        compare (Guild_id.get_id m1.guild_id) (Guild_id.get_id m2.guild_id)
      in
      if comp_guild <> 0 then comp_guild
      else compare (User_id.get_id m1.user.id) (User_id.get_id m2.user.id))
end

module Set = Set.Make (Self)
module Map = Map.Make (Self)
include Self

let visible_name member =
  Option.value ~default:User.(member.user.username) member.nick
