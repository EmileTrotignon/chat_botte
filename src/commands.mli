open Disml.Models

val score_of_emoji : Emoji.partial_emoji -> int

val get_score_of_author : Message.t -> unit

val get_scores_of_mentions : Message.t -> unit

val get_scores_of_everyone : Message.t -> unit

val update_score_add : Event.ReactionAdd.t -> unit

val update_score_remove : Event.ReactionRemove.t -> unit
