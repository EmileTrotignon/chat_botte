open Disml
open Models
open Core

val score_of_user : Guild_id.t -> User_id.t -> int Or_error.t

val add_to_score : Guild_id.t -> User_id.t -> int -> unit Or_error.t

val set_score : Guild_id.t -> User_id.t -> int -> unit Or_error.t
