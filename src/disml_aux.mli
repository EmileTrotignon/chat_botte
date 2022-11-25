open Disml
open Async

val guild_of_id : Models.Guild_id.t -> Models.Guild.t

val message_of_id :
     Models.Message_id.t
  -> Models.Channel_id.t
  -> Models.Message.t Deferred.Or_error.t

val member_of_id :
  Models.Guild_id.t -> Models.User_id.t -> Member.t Deferred.Or_error.t

val member_of_user :
  Models.Guild_id.t -> Models.User.t -> Member.t Deferred.Or_error.t

val members_of_role_id :
  Models.Guild_id.t -> Models.Role_id.t -> Member.Set.t Deferred.t

val role_of_id :
  Models.Guild_id.t -> Models.Role_id.t -> Models.Role.t Deferred.Option.t

val user_is_admin : Models.Guild_id.t -> Models.User.t -> bool Deferred.t

val long_reply : Models.Message.t -> string -> unit Deferred.Or_error.t
