open Async
open Core
open Disml
module TmpMember = Member
open Models
module Member = TmpMember

let rec iter_and_last ~f messages =
  match messages with
  | [] ->
      None
  | [message] ->
      f message ; Some message
  | message :: messages ->
      f message ; iter_and_last ~f messages

let rec iter_and_last_deferred ~f messages =
  match messages with
  | [] ->
      Deferred.return None
  | [message] ->
      let%map () = f message in
      Some message
  | message :: messages ->
      let%bind () = f message in
      iter_and_last_deferred ~f messages

let delete_tmp_message message =
  don't_wait_for
  @@ match%map Message.delete message with
     | Error e ->
         MLog.error_t "While deleting tmp message" e
     | Ok () ->
         ()

let rec iter_loop ~f channel message =
  let message_id = Message.(message.id) in
  match%bind
    Channel.get_messages ~limit:100 ~mode:`Before channel message_id
  with
  | Error e ->
      Deferred.return @@ MLog.error_t "While requesting messages" e
  | Ok messages -> (
    match iter_and_last ~f messages with
    | None ->
        Deferred.return ()
    | Some message ->
        f message ;
        iter_loop ~f channel message )

let iter ~f channel =
  match%bind
    Channel.send_message ~content:"Scanning this channel @.everyone" channel
  with
  | Error e ->
      Deferred.return (MLog.error_t "While sending scanning message" e)
  | Ok tmp_message -> (
      let tmp_message_id = Message.(tmp_message.id) in
      match%bind
        Channel.get_messages ~limit:100 ~mode:`Before channel tmp_message_id
      with
      | Error e ->
          delete_tmp_message tmp_message ;
          Deferred.return @@ MLog.error_t "While requesting messages" e
      | Ok messages -> (
          delete_tmp_message tmp_message ;
          match iter_and_last ~f messages with
          | None ->
              Deferred.return ()
          | Some message ->
              iter_loop ~f channel message ) )

let rec iter_loop_deferred ~f channel message =
  let message_id = Message.(message.id) in
  match%bind
    Channel.get_messages ~limit:100 ~mode:`Before channel message_id
  with
  | Error e ->
      Deferred.return @@ MLog.error_t "While requesting messages" e
  | Ok messages -> (
      match%bind iter_and_last_deferred ~f messages with
      | None ->
          Deferred.return ()
      | Some message ->
          iter_loop_deferred ~f channel message )

let iter_deferred ~f channel =
  match%bind
    Channel.send_message ~content:"Scanning this channel @.everyone" channel
  with
  | Error e ->
      Deferred.return (MLog.error_t "While sending scanning message" e)
  | Ok tmp_message -> (
      let tmp_message_id = Message.(tmp_message.id) in
      match%bind
        Channel.get_messages ~limit:100 ~mode:`Before channel tmp_message_id
      with
      | Error e ->
          delete_tmp_message tmp_message ;
          Deferred.return @@ MLog.error_t "While requesting messages" e
      | Ok messages -> (
          delete_tmp_message tmp_message ;
          match%bind iter_and_last_deferred ~f messages with
          | None ->
              Deferred.return ()
          | Some message ->
              iter_loop_deferred ~f channel message ) )
