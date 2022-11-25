open Core
open Async
open Disml
module TmpMember = Member
open Models
module Member = TmpMember
open Letop.Deferred

let guild_of_id (guild_id : Guild_id.t) =
  Cache.(
    let cache = Mvar.peek_exn cache in
    Guild_id.Map.find_exn cache.guilds guild_id )

module type I = sig
  type t

  type arg

  val get_new : arg -> t Deferred.Or_error.t

  val log_success : t -> unit

  val log_failure : Error.t -> unit
end

module Cached (A : I) = struct
  let value : (A.t, read_write) Mvar.t = Mvar.create ()

  let udpating : (bool, read_write) Mvar.t = Mvar.create ()

  let update arg =
    let already_updating = Option.value ~default:false (Mvar.peek udpating) in
    if already_updating then return ()
    else
      let+ new_value = A.get_new arg in
      match new_value with
      | Ok new_value ->
          A.log_success new_value ;
          Deferred.return @@ Mvar.set value new_value
      | Error e ->
          A.log_failure e ; Mvar.set udpating false ; Deferred.never ()

  let get arg =
    match Mvar.peek value with
    | Some value ->
        Deferred.return value
    | None ->
        don't_wait_for (update arg) ;
        let* () = Mvar.value_available value in
        Mvar.peek_exn value
end

module CachedMembers = Cached (struct
  type t = Member.Set.t

  type arg = Guild_id.t

  let get_new guild_id =
    let guild = guild_of_id guild_id in
    let* new_members = Guild.request_members guild in
    Or_error.map ~f:Member.Set.of_list new_members

  let log_success new_members =
    MLog.info
      {%eml|Successfully queryed <%i- Member.Set.length new_members %> members.|}

  let log_failure e =
    Error.pp Format.str_formatter e ;
    let str = Buffer.contents Format.stdbuf in
    Buffer.reset Format.stdbuf ;
    MLog.error {%eml|Request members failed : <%-str%>|}
end)

module CachedRoles = Cached (struct
  type t = Role.t list

  type arg = Guild_id.t

  let get_new guild_id =
    let guild = guild_of_id guild_id in
    Guild.request_roles guild

  let log_success new_roles =
    MLog.info {%eml|Successfully queryed <%i- List.length new_roles %> roles.|}

  let log_failure e = MLog.error_t "Request roles failed" e
end)

module CachedDM = Cached (struct
  type t = Channel.t User.Map.t

  type arg = Guild_id.t

  let get_new guild_id : t Deferred.Or_error.t =
    let+ members = CachedMembers.get guild_id in
    members |> Member.Set.to_list
    |> Deferred.List.filter_map ~f:(fun member ->
           let user = Member.user member in
           let* channel = User_id.create_dm user.id in
           match channel with
           | Error e ->
               MLog.error_t "While opening a DM channel" e ;
               None
           | Ok channel ->
               Some (user, channel) )
    |> Deferred.map ~f:User.Map.of_alist_exn
    |> Deferred.map ~f:Or_error.return

  let log_success _new_channels = MLog.info {|Successfully opened DM channels|}

  let log_failure e = MLog.error_t "While opening DM channels" e
end)

(*
let last_message : (Message.t, read_write) Mvar.t = Mvar.create ()

let set_last_message new_message = Mvar.set last_message new_message *)

let update_members = CachedMembers.update

let get_members = CachedMembers.get

let update_roles = CachedRoles.update

let get_roles = CachedRoles.get

let get_dms = CachedDM.get

let update_dms = CachedDM.update

let update_all guild_id =
  let+ () = update_members guild_id in
  let+ () = update_dms guild_id in
  update_roles guild_id
