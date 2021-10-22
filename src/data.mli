open Disml
open Models

val score_of_id : User_id.t -> Guild_id.t -> int Async.Deferred.t

val add_to_score : User_id.t -> Guild_id.t -> int -> unit Async.Deferred.t
