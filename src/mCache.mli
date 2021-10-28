open Disml
open Async

val update_members : Models.Guild_id.t -> unit Deferred.t

val get_members : Models.Guild_id.t -> Member.Set.t Deferred.t

val update_roles : Models.Guild_id.t -> unit Deferred.t

val get_roles : Models.Guild_id.t -> Models.Role.t list Deferred.t

val update_all : Models.Guild_id.t -> unit Deferred.t