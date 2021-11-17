open Disml
open Models
open Async

val score_of_id : Guild_id.t -> User_id.t -> int Deferred.t

val score_of_user : Guild_id.t -> User.t -> int Deferred.t

val add_to_score : Guild_id.t -> User_id.t -> int -> unit Deferred.t

val set_score : Guild_id.t -> User_id.t -> int -> unit Deferred.t
