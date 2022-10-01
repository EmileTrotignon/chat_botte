open Disml

open Models

val update_members : Guild_id.t -> unit Lwt.t

val get_members : Guild_id.t -> Member.Set.t Lwt.t

val update_dms : Guild_id.t -> unit Lwt.t

val get_dms : Guild_id.t -> Channel.t User.Map.t Lwt.t

val update_roles : Guild_id.t -> unit Lwt.t

val get_roles : Guild_id.t -> Role.t list Lwt.t

val update_all : Guild_id.t -> unit Lwt.t