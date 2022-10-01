open Disml
open Models

val score_of_id : Guild_id.t -> User_id.t -> int Lwt.t

val score_of_user : Guild_id.t -> User.t -> int Lwt.t

val add_to_score : Guild_id.t -> User_id.t -> int -> unit Lwt.t

val set_score : Guild_id.t -> User_id.t -> int -> unit Lwt.t
