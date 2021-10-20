open Core
open Disml
module TmpMember = Member
open Models
module Member = TmpMember

let config = Irmin_git.config ~bare:true Config.database_location

let key_of_member Member.{user; guild_id; _} =
  let User.{id; username; _} = user in
  let guild_id = guild_id |> Guild_id.get_id |> string_of_int in
  let id = id |> User_id.get_id |> string_of_int in
  ["scores"; guild_id; username ^ id]

module Contents = struct
  type t = int

  let t = Irmin.Type.int

  let merge ~old a b =
    let open Irmin.Merge.Infix in
    old ()
    >|=* fun old ->
    let old = match old with None -> 0 | Some o -> o in
    a + b - old

  let merge = Irmin.Merge.(option (v t merge))
end

module Store = Irmin_unix.Git.FS.KV (Contents)

let repo = Store.Repo.v config

open Lwt.Infix

let info message = Irmin_unix.info ~author:"Chat-botté" "%s" message

let score member =
  repo >>= Store.master
  >>= fun t -> Store.find t (key_of_member member) >|= Option.value ~default:0

let add_to_score member points =
  repo >>= Store.master
  >>= fun t ->
  score member
  >>= fun score ->
  let score = score + points and key = key_of_member member in
  let message =
    sprintf "Setting the score of %s to %d" (Member.visible_name member) score
  in
  MLog.info message ;
  let info = info message in
  Store.set ~info t key score
  >|= function
  | Ok () -> () | Error _e -> MLog.error "Could not write to database"


open Async

let score member = In_thread.run (fun () -> Lwt_main.run (score member))

let add_to_score member points =
  In_thread.run (fun () -> Lwt_main.run (add_to_score member points))
