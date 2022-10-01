open Disml
open Base

val guild_of_id : Models.Guild_id.t -> Models.Guild.t Lwt.t

val message_of_id :
     Models.Message_id.t
  -> Models.Channel_id.t
  -> (Models.Message.t, string) Lwt_result.t

val member_of_id :
  Models.Guild_id.t -> Models.User_id.t -> (Member.t, string) Lwt_result.t

val member_of_user :
  Models.Guild_id.t -> Models.User.t -> (Member.t, string) Lwt_result.t

val members_of_role_id :
  Models.Guild_id.t -> Models.Role_id.t -> Member.Set.t Lwt.t

val role_of_id :
  Models.Guild_id.t -> Models.Role_id.t -> Models.Role.t option Lwt.t

val user_is_admin : Models.Guild_id.t -> Models.User.t -> bool Lwt.t

val logged_reply : Models.Message.t -> string -> unit