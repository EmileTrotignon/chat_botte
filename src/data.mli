val score : Member.t -> int Async.Deferred.t

val add_to_score : Member.t -> int -> unit Async.Deferred.t

(* val scores : unit -> int Member.Map.t Async.Deferred.t *)