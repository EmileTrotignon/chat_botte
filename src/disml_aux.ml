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

let member_of_id guild_id user_id =
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

let member_of_user guild_id user = member_of_id guild_id User.(user.id)

let members_of_role_id guild_id role_id =
  let%map members = MCache.get_members guild_id in
  Member.Set.filter ~f:(fun m -> Member.has_role m role_id) members

let role_of_id guild_id role_id =
  let%map roles = MCache.get_roles guild_id in
  List.find
    ~f:(fun role -> Int.(Role.(Role_id.(get_id role.id = get_id role_id))))
    roles

let user_is_admin guild_id user =
  match%map member_of_user guild_id user with
  | Error _ ->
      false
  | Ok member ->
      List.exists Config.Roles.admins ~f:(fun admin_role ->
          Member.has_role member (`Role_id admin_role) )

let length_limit = 2000

let logged_reply message reply =
  let Message.{content; _} = message in
  MLog.info {%eml|Replying to message <%S-content%> with <%S-reply%>.|} ;
  match%map Message.reply message reply with
  | Ok _ ->
      MLog.info "Reply succesful."
  | Error e ->
      MLog.error_t "during reply" e

let long_reply message (content : string) =
  don't_wait_for
  @@
  let content = String.split ~on:'\n' content in
  let buffer = Buffer.create length_limit in
  let empty_buffer () =
    let content = Buffer.contents buffer in
    Buffer.reset buffer ;
    let n_replies = (String.length content / length_limit) + 1 in
    Deferred.for_ 1 ~to_:n_replies ~do_:(fun i ->
        let lo = (i - 1) * length_limit in
        let hi = i * length_limit in
        let hi = min (String.length content) hi in
        logged_reply message (String.sub ~pos:lo ~len:(hi - lo) content) )
  in
  let%bind () =
    Deferred.List.iter
      ~f:(fun line ->
        let line = line ^ "\n" in
        if Buffer.length buffer + String.length line <= length_limit then
          Deferred.return @@ Buffer.add_string buffer line
        else
          let%map () = empty_buffer () in
          Buffer.add_string buffer line )
      content
  in
  empty_buffer ()

let logged_reply message reply = long_reply message reply
