open Disml.Models
open Async

type t = Message.t -> unit Deferred.Or_error.t

val log_don't_wait : ('a -> unit Deferred.Or_error.t) -> 'a -> unit

val update_cached_members : Guild_id.t -> unit Deferred.t

val update_cached_roles : Guild_id.t -> unit Deferred.t

val get_score_of_author : t

val get_scores_of_mentions : t

val get_scores_of_everyone : t

val get_smart_scores : string -> t

val update_score_add : Event.ReactionAdd.t -> unit Deferred.Or_error.t

val update_score_remove : Event.ReactionRemove.t -> unit Deferred.Or_error.t

val update_cache : t

val crunch_scores : t

val rank_members : t

val chance_of_delete : float -> t

val delete_message : t

val change_nick : t

val send_dm : t
