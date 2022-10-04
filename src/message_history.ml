open Common
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
      Lwt.return None
  | [message] ->
      let+ () = f message in
      Some message
  | message :: messages ->
      let* () = f message in
      iter_and_last_deferred ~f messages

let delete_tmp_message message =
  let+ delete = Message.delete message in
  match delete with
  | Error e ->
      MLog.error_t "While deleting tmp message" e
  | Ok () ->
      ()

let rec iter_loop ~f channel message =
  let message_id = Message.(message.id) in
  let* messages =
    Channel.get_messages ~limit:100 ~mode:`Before channel message_id
  in
  match messages with
  | Error e ->
      Lwt.return @@ MLog.error_t "While requesting messages" e
  | Ok messages -> (
    match iter_and_last ~f messages with
    | None ->
        Lwt.return ()
    | Some message ->
        f message ;
        iter_loop ~f channel message )

let iter ~f channel =
  let* tmp_message =
    Channel.send_message ~content:"Scanning this channel @.everyone" channel
  in
  match tmp_message with
  | Error e ->
      Lwt.return @@ MLog.error_t "While sending scanning message" e
  | Ok tmp_message -> (
      let tmp_message_id = Message.(tmp_message.id) in
      let* messages =
        Channel.get_messages ~limit:100 ~mode:`Before channel tmp_message_id
      in
      let* () = delete_tmp_message tmp_message in
      match messages with
      | Error e ->
          Lwt.return @@ MLog.error_t "While requesting messages" e
      | Ok messages -> (
        match iter_and_last ~f messages with
        | None ->
            Lwt.return_unit
        | Some message ->
            iter_loop ~f channel message ) )

let rec iter_loop_deferred ~f channel message =
  let message_id = Message.(message.id) in
  let* messages =
    Channel.get_messages ~limit:100 ~mode:`Before channel message_id
  in
  match messages with
  | Error e ->
      Lwt.return @@ MLog.error_t "While requesting messages" e
  | Ok messages -> (
      let* messages = iter_and_last_deferred ~f messages in
      match messages with
      | None ->
          Lwt.return ()
      | Some message ->
          iter_loop_deferred ~f channel message )

let iter_deferred ~f channel =
  let* tmp_message =
    Channel.send_message ~content:"Scanning this channel @everyone" channel
  in
  match tmp_message with
  | Error e ->
      Lwt.return @@ MLog.error_t "While sending scanning message" e
  | Ok tmp_message -> (
      let tmp_message_id = Message.(tmp_message.id) in
      let* messages =
        Channel.get_messages ~limit:100 ~mode:`Before channel tmp_message_id
      in
      let* () = delete_tmp_message tmp_message in
      match messages with
      | Error e ->
          Lwt.return @@ MLog.error_t "While requesting messages" e
      | Ok messages -> (
          let* message = iter_and_last_deferred ~f messages in
          match message with
          | None ->
              Lwt.return ()
          | Some message ->
              iter_loop_deferred ~f channel message ) )
