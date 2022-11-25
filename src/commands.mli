open Disml.Models

val update_cached_members : Guild_id.t -> unit

val update_cached_roles : Guild_id.t -> unit

val get_score_of_author : Message.t -> unit

val get_scores_of_mentions : Message.t -> unit

val get_scores_of_everyone : Message.t -> unit

val get_smart_scores : string -> Message.t -> unit

val update_score_add : Event.ReactionAdd.t -> unit

val update_score_remove : Event.ReactionRemove.t -> unit

val update_cache : Message.t -> unit

val crunch_scores : Message.t -> unit

val rank_members : Message.t -> unit

val chance_of_delete : float -> Message.t -> unit

val delete_message : Message.t -> unit

val change_nick : Message.t -> unit

val send_dm : Message.t -> unit
