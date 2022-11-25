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

let rec iter_and_last_deferred ~f messages : 'a option Deferred.Or_error.t =
  let open Letop.Deferred_or_error in
  match messages with
  | [] ->
      Deferred.Or_error.return None
  | [message] ->
      let* () = f message in
      Some message
  | message :: messages ->
      let+ () = f message in
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
      Deferred.return @@ MLog.error_t "While sending scanning message" e
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
  let open Letop.Deferred_or_error in
  let+ messages =
    Channel.get_messages ~limit:100 ~mode:`Before channel message_id
  in
  let+ r = iter_and_last_deferred ~f messages in
  match r with
  | None ->
      Deferred.return (Ok ())
  | Some message ->
      iter_loop_deferred ~f channel message

let iter_deferred ~f channel =
  let open Letop.Deferred_or_error in
  let+ tmp_message =
    Channel.send_message ~content:"Scanning this channel @everyone" channel
  in
  let tmp_message_id = Message.(tmp_message.id) in
  let+ messages =
    Channel.get_messages ~limit:100 ~mode:`Before channel tmp_message_id
  in
  delete_tmp_message tmp_message ;
  let+ r = iter_and_last_deferred ~f messages in
  match r with
  | None ->
      Deferred.return @@ Ok ()
  | Some message ->
      iter_loop_deferred ~f channel message
