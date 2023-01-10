open Core
(* open Disml_aux *)
(* open Disml.Models *)

let log channel content =
  Out_channel.output_string channel content ;
  Out_channel.newline channel ;
  Out_channel.flush channel

let log content =
  (* https://programmierfrage.com/items/using-discord-api-curl-to-send-a-discord-dm *)
  log Config.log_file content

let time () = Format.asprintf "%a" Time.pp (Time.now ())

let info content = log {%eml|[Info][<%- time () %>] <%- content %>|}

let error content = log {%eml|[Error][<%- time () %>] <%- content %>|}

let string_of_error e = Format.asprintf "%a" Error.pp e

let error_t reason e = error {%eml|<%- reason %> : <%- string_of_error e %>.|}
