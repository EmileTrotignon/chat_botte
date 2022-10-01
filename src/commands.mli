open Disml.Models

val update_cached_members : Guild_id.t -> unit Lwt.t

val update_cached_roles : Guild_id.t -> unit Lwt.t

val get_score_of_author : Message.t -> unit Lwt.t

val get_scores_of_mentions : Message.t -> unit Lwt.t

val get_scores_of_everyone : Message.t -> unit Lwt.t

val get_smart_scores : string -> Message.t -> unit Lwt.t

val update_score_add : Event.ReactionAdd.t -> unit Lwt.t

val update_score_remove : Event.ReactionRemove.t -> unit Lwt.t

val update_cache : Message.t -> unit Lwt.t

val crunch_scores : Message.t -> unit Lwt.t

val stupid_message : Message.t -> unit Lwt.t

val rank_members : Message.t -> unit Lwt.t

val chance_of_delete : float -> Message.t -> unit Lwt.t

val delete_message : Message.t -> unit Lwt.t

val change_nick : Message.t -> unit Lwt.t

val send_dm : Message.t -> unit Lwt.t
