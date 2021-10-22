open Disml
open Async

val guild_of_id : Models.Guild_id.t -> Models.Guild.t

val message_of_id :
  Models.Message_id.t -> Models.Channel_id.t -> Models.Message.t Deferred.Or_error.t

val member_of_id : Models.User_id.t -> Models.Guild_id.t -> Member.t Deferred.Or_error.t

val member_of_user : Models.User.t -> Models.Guild_id.t -> Member.t Deferred.Or_error.t

val members_of_role_id :
  Models.Role_id.t -> Models.Guild_id.t -> Member.Set.t Deferred.t
