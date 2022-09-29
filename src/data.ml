open Core
open Disml
module TmpMember = Member
open Models
module Member = TmpMember

let config = Irmin_git.config ~bare:true Config.database_location

module Key = struct
  module Self = struct
    type t = string list [@@deriving sexp]

    let compare = List.compare String.compare

    let to_string (key : t) =
      {%eml|[<%List.iter (fun (s:string) -> (%><%- s %>; <%):string -> unit) key ;%>]|}

    let of_ids guild_id id =
      let guild_id = guild_id |> Guild_id.get_id |> string_of_int
      and id = id |> User_id.get_id |> string_of_int in
      ["scores"; guild_id; id]

    (* let to_member =
       Async.(
         function
         | ["scores"; guild_id; id] -> (
           try
             let guild_id = `Guild_id (int_of_string guild_id)
             and id = `User_id (int_of_string id) in
             Deferred.map ~f:Option.return (member_of_id id guild_id)
           with Failure _ -> Deferred.return None )
         | _ ->
             Deferred.return None) *)
  end

  module Map = Map.Make (Self)
  include Self
end

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

let score_of_key key =
  repo >>= Store.main
  >>= fun t -> Store.find t key >|= Option.value ~default:0

let add_to_score_of_key key points =
  repo >>= Store.main
  >>= fun t ->
  score_of_key key
  >>= fun score ->
  let score = score + points in
  let message =
    {%eml|Setting the score of <%-(Key.to_string key)%> to <%i-score%>|}
  in
  MLog.info message ;
  let info = info message in
  Store.set ~info t key score
  >|= function
  | Ok () -> () | Error _e -> MLog.error "Could not write to database"

let set_score_of_key key score =
  repo >>= Store.main
  >>= fun t ->
  let message =
    {%eml|Setting the score of <%-(Key.to_string key)%> to <%i-score%>|}
  in
  MLog.info message ;
  let info = info message in
  Store.set ~info t key score
  >|= function
  | Ok () -> () | Error _e -> MLog.error "Could not write to database"

let set_score_of_id guild_id id = set_score_of_key (Key.of_ids guild_id id)

let add_to_score_of_id guild_id id =
  add_to_score_of_key (Key.of_ids guild_id id)

open Async

let score_of_key key = In_thread.run (fun () -> Lwt_main.run (score_of_key key))

let score_of_id guild_id id = score_of_key (Key.of_ids guild_id id)

let score_of_user guild_id user = score_of_id guild_id User.(user.id)

let add_to_score guild_id id points =
  In_thread.run (fun () ->
      Lwt_main.run (add_to_score_of_id guild_id id points) )

let set_score guild_id id score =
  In_thread.run (fun () -> Lwt_main.run (set_score_of_id guild_id id score))

(*let scores () = In_thread.run (fun () -> Lwt_main.run (scores ()))

  let scores () =
    let%map scores = scores () in
    Key.Map.to_alist scores

  let scores () =
    Deferred.join
    @@ let%map scores = scores () in
       Deferred.List.filter_map
         ~f:(fun (key, data) ->
           let%map key, data =
             Deferred.both (Key.to_member key)
               (Deferred.return @@ Option.return data)
           in
           Option.both key data )
         scores

  let _scores () =
    let%map scores = scores () in
    List.fold scores ~init:Member.Map.empty ~f:(fun map (key, data) ->
        Member.Map.add_exn ~key ~data map )
*)