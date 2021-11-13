open Core

let log channel content =
  Out_channel.output_string channel content ;
  Out_channel.flush channel

let log content =
  log stdout content ;
  log Config.log_file content

let time () =
  Time.pp Format.str_formatter @@ Time.now () ;
  Format.flush_str_formatter ()

let info content = log ("[Info][" ^ time () ^ "] " ^ content ^ "\n")

let error content = log ("[Error][" ^ time () ^ "] " ^ content ^ "\n")

let string_of_error e =
  Error.pp Format.str_formatter e ;
  Format.flush_str_formatter ()

let error_t reason e = error {%eml|<%- reason %> : <%- string_of_error e %>.|}
