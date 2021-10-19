open Disml
open Models
open Async

val guild_of_id : Guild_id.t -> Guild.t

val message_of_id : Message_id.t -> Channel_id.t -> Message.t Deferred.t

val member_of_id : User_id.t -> Guild_id.t -> Member.t Deferred.t

val member_of_user : User.t -> Guild_id.t -> Models.Member.t Deferred.t