open Async
open Core
open Disml
module TmpMember = Member
open Models
module Member = TmpMember
include Disml_aux_kernel

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
  Channel_id.get_message ~id:(Message_id.get_id message_id) channel_id

let member_of_id user_id guild_id =
  let%map members = MCache.get_members guild_id in
  match
    Member.Set.find
      ~f:(fun member ->
        User_id.(get_id Member.(member.user).id = get_id user_id) )
      members
  with
  | Some m ->
      Ok m
  | None ->
      Error
        (Error.of_string
           {%eml|Could not find member with id <%i- User_id.get_id user_id %> in guild <%i- Guild_id.get_id guild_id %>|} )

let member_of_user user guild_id = member_of_id User.(user.id) guild_id

let members_of_role_id role_id guild_id =
  let%map members = MCache.get_members guild_id in
  Member.Set.filter ~f:(fun m -> Member.has_role m role_id) members

let role_of_id guild_id role_id =
  let%map roles = MCache.get_roles guild_id in
  List.find
    ~f:(fun role -> Int.(Role.(Role_id.(get_id role.id = get_id role_id))))
    roles

let user_is_admin guild_id user =
  match%map member_of_user user guild_id with
  | Error _ ->
      false
  | Ok member ->
      List.exists Config.admin_roles ~f:(fun admin_role ->
          Member.has_role member (`Role_id admin_role) )
