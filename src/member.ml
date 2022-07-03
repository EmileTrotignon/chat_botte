open Core
open Disml
open Models

module Self = struct
  include Member

  let ( = ) (m1 : t) (m2 : t) =
    Int.(
      Guild_id.get_id m1.guild_id = Guild_id.get_id m2.guild_id
      && User_id.get_id m1.user.id = User_id.get_id m2.user.id)
end

module Hashtbl = Hashtbl.Make (Self)
include Self

let visible_name member =
  Option.value ~default:User.(member.user.username) member.nick

let ping_text member =
  let id = User_id.get_id member.user.id in
  {%eml|<@<%i- id %>>|}

let has_role member role_id =
  List.exists
    ~f:(fun r -> Int.(Role_id.get_id r = Role_id.get_id role_id))
    Member.(member.roles)
