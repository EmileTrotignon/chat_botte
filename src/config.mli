val token : string

val reacts : int Core.String.Map.t

val log_file : out_channel

val database_location : string

module Roles : sig
  val admins : int array

  val s : int

  val a : int

  val b : int

  val c : int

  val d : int

  val e : int

  val f : int

  val ranks : int array

  val warning : int

  val edit_punished : int

  val free_rename : int

  val free_ping : int
end

val command_prefix : string

val change_nick_cost : int

val dm_ping_cost : int

val bot_owner: int