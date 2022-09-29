open Disml
open Async
open Models

val update_members : Guild_id.t -> unit Deferred.t

val get_members : Guild_id.t -> Member.Set.t Deferred.t

val update_dms : Guild_id.t -> unit Deferred.t

val get_dms : Guild_id.t -> Channel.t User.Map.t Deferred.t

val update_roles : Guild_id.t -> unit Deferred.t

val get_roles : Guild_id.t -> Role.t list Deferred.t

val update_all : Guild_id.t -> unit Deferred.t