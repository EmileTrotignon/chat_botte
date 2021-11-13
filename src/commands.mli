open Disml.Models

val get_score_of_author : Message.t -> unit

val get_scores_of_mentions : Message.t -> unit

val get_scores_of_everyone : Message.t -> unit

val get_smart_scores : string -> Message.t -> unit

val update_score_add : Event.ReactionAdd.t -> unit

val update_score_remove : Event.ReactionRemove.t -> unit

val update_cache : Message.t -> unit

val crunch_scores : Message.t -> unit