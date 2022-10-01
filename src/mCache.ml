open Core
open Common
open Disml
module TmpMember = Member
open Models
module Member = TmpMember

let guild_of_id (guild_id : Guild_id.t) =
  Cache.(
    let+ cache = read_copy cache in
    Guild_id.Map.find guild_id cache.guilds )

module type I = sig
  type t

  type arg

  val get_new : arg -> (t, string) Lwt_result.t

  val log_success : t -> unit

  val log_failure : string -> unit
end

module Cached (A : I) = struct
  let value : A.t Option.t ref = ref None

  let udpating : Lwt_mutex.t = Lwt_mutex.create ()

  let update arg =
    if Lwt_mutex.is_locked udpating then Lwt.return ()
    else
      Lwt_mutex.with_lock udpating (fun () ->
          let* new_value = A.get_new arg in
          match new_value with
          | Ok new_value ->
              A.log_success new_value ;
              value := Some new_value ;
              Lwt.return ()
          | Error e ->
              A.log_failure e ; Lwt.return () )

  let get arg =
    let* () = Lwt_mutex.lock udpating in
    match !value with
    | Some value ->
        Lwt_mutex.unlock udpating ; Lwt.return value
    | None ->
        Lwt_mutex.unlock udpating ;
        let+ () = update arg in
        Option.value_exn !value
end

module CachedMembers = Cached (struct
  type t = Member.Set.t

  type arg = Guild_id.t

  let get_new guild_id =
    let* guild = guild_of_id guild_id in
    let+ new_members = Guild.request_members guild in
    Result.map ~f:Member.Set.of_list new_members

  let log_success new_members =
    MLog.info
      {%eml|Successfully queryed <%i- Member.Set.cardinal new_members %> members.|}

  let log_failure e = MLog.error {%eml|Request members failed : <%-e%>|}
end)

module CachedRoles = Cached (struct
  type t = Role.t list

  type arg = Guild_id.t

  let get_new guild_id =
    let* guild = guild_of_id guild_id in
    Guild.request_roles guild

  let log_success new_roles =
    MLog.info {%eml|Successfully queryed <%i- List.length new_roles %> roles.|}

  let log_failure e = MLog.error_t "Request roles failed" e
end)

module CachedDM = Cached (struct
  type t = Channel.t User.Map.t

  type arg = Guild_id.t

  let get_new guild_id =
    let* members = CachedMembers.get guild_id in
    members |> Member.Set.to_seq |> Lwt_seq.of_seq
    |> Lwt_seq.filter_map_s (fun member ->
           let user = Member.user member in
           let+ channel = User_id.create_dm user.id in
           match channel with
           | Error e ->
               MLog.error_t "While opening a DM channel" e ;
               None
           | Ok channel ->
               Some (user, channel) )
    |> Lwt_seq.to_list
    |> Lwt.map (fun d ->
           d |> Stdlib.List.to_seq |> User.Map.of_seq |> Result.return )

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
  let* () = update_members guild_id in
  let* () = update_dms guild_id in
  update_roles guild_id