open Disml
open Models
open Async

val score_of_id : User_id.t -> Guild_id.t -> int Deferred.t

val score_of_user : User.t -> Guild_id.t -> int Deferred.t

val add_to_score : User_id.t -> Guild_id.t -> int -> unit Deferred.t

val set_score : User_id.t -> Guild_id.t -> int -> unit Deferred.t
