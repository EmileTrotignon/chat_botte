open Disml

val guild_of_id : Models.Guild_id.t -> Models.Guild.t

val message_of_id :
     Models.Message_id.t
  -> Models.Channel_id.t
  -> Models.Message.t Async.Deferred.Or_error.t

val member_of_id :
  Cache.UserMap.Key.t -> Models.Guild_id.t -> Member.t Async.Deferred.Or_error.t

val member_of_user :
  Models.User.t -> Models.Guild_id.t -> Member.t Async.Deferred.Or_error.t
