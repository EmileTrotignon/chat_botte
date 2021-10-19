open Core

let log channel content =
  Out_channel.output_string channel content ;
  Out_channel.flush channel

let log content =
  log stdout content ;
  log Config.log_file content

let info content = log "[Info] " ; log content ; log "\n"

let error content = log "[Error] " ; log content ; log "\n"
