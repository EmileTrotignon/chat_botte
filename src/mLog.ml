(* open Disml_aux *)
(* open Disml.Models *)

let log channel content =
  Out_channel.output_string channel content ;
  Out_channel.flush channel

let log content =
  (* https://programmierfrage.com/items/using-discord-api-curl-to-send-a-discord-dm *)
  log stdout content ;
  log Config.log_file content

let time () =
  Core.Time.pp Format.str_formatter @@ Core.Time.now () ;
  Format.flush_str_formatter ()

let info content = log ("[Info][" ^ time () ^ "] " ^ content ^ "\n")

let error content = log ("[Error][" ^ time () ^ "] " ^ content ^ "\n")

let string_of_error = Fun.id

let error_t reason e = error {%eml|<%- reason %> : <%- string_of_error e %>.|}
