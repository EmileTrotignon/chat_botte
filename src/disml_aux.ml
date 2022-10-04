open Common
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
  (*let+ cache = Cache.(read_copy cache) in
    let guild = Cache.guild guild_id cache in
    match guild with
    | None ->
        Error {%eml|Could not find guild <%i- Guild_id.get_id guild_id %>|}
    | Some guild -> (
        let members = Guild.members guild in
        print_endline {%eml|Found <%i- List.length members %> members.|} ;
        match
          List.find members ~f:(fun member ->
              let id_member =
                member |> Member.user |> User.id |> User_id.get_id
              in
              let id_user = User_id.get_id user_id in
              print_endline {%eml|<%i- id_member %> = <%i- id_user %> ?|} ;
              id_member = id_user )
        with
        | None ->
            Error
              {%eml|Could not find member with id <%i- User_id.get_id user_id %> in guild <%i- Guild_id.get_id guild_id %>|}
        | Some m ->
            Ok m )
  *)
  let+ members = MCache.get_members guild_id in
  print_endline {%eml|Found <%i- Member.Set.cardinal members %> members.|} ;
  match
    members |> Member.Set.elements
    |> List.find ~f:(fun member ->
           let id_member = member |> Member.user |> User.id |> User_id.get_id in
           let id_user = User_id.get_id user_id in
           print_endline {%eml|<%i- id_member %> = <%i- id_user %> ?|} ;
           id_member = id_user )
  with
  | Some m ->
      print_endline "found member" ;
      Ok m
  | None ->
      (*Member.Set.iter
        (fun member ->
          print_endline
            {%eml|<%i-User_id.get_id @@ User.id @@ Member.user member%>|} )
        members ;*)
      Error
        {%eml|Could not find member with id <%i- User_id.get_id user_id %> in guild <%i- Guild_id.get_id guild_id %>|}

let member_of_user guild_id user = member_of_id guild_id User.(user.id)

let members_of_role_id guild_id role_id =
  let+ members = MCache.get_members guild_id in
  Member.Set.filter (fun m -> Member.has_role m role_id) members

let role_of_id guild_id role_id =
  let+ roles = MCache.get_roles guild_id in
  List.find
    ~f:(fun role -> Int.(Role.(Role_id.(get_id role.id = get_id role_id))))
    roles

let user_is_admin guild_id user =
  let+ member = member_of_user guild_id user in
  match member with
  | Error _ ->
      false
  | Ok member ->
      Array.exists Config.Roles.admins ~f:(fun admin_role ->
          Member.has_role member (`Role_id admin_role) )

let length_limit = 2000

let logged_reply message reply =
  let Message.{content; _} = message in
  MLog.info {%eml|Replying to message <%S-content%> with <%S-reply%>.|} ;
  let+ reply = Message.reply message reply in
  match reply with
  | Ok _ ->
      MLog.info "Reply succesful."
  | Error e ->
      MLog.error_t "during reply" e

let long_reply message (content : string) =
  dont_wait_exn
  @@ fun () ->
  let content = String.split ~on:'\n' content in
  let buffer = Buffer.create length_limit in
  let empty_buffer () =
    let content = Buffer.contents buffer in
    Buffer.reset buffer ;
    let n_replies = (String.length content / length_limit) + 1 in
    List.init n_replies ~f:(fun i -> i)
    |> Lwt_list.iter_s (fun i ->
           let lo = (i - 1) * length_limit in
           let hi = i * length_limit in
           let hi = min (String.length content) hi in
           logged_reply message (String.sub ~pos:lo ~len:(hi - lo) content) )
  in
  let* () =
    Lwt_list.iter_s
      (fun line ->
        let line = line ^ "\n" in
        if Buffer.length buffer + String.length line <= length_limit then
          Lwt.return @@ Buffer.add_string buffer line
        else
          let+ () = empty_buffer () in
          Buffer.add_string buffer line )
      content
  in
  empty_buffer ()

let logged_reply message reply = long_reply message reply
