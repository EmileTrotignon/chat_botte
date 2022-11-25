open Core
open Bos

let result_conv rr =
  Rresult.(
    match rr with
    | Ok v ->
        Stdlib.Ok v
    | Error e ->
        Core.Error
          (Error.of_string
             (Format.asprintf "%a" Rresult.R.pp_msg e) (*|> Error.of_string*) ) )

let name_of_guild (`Guild_id id) = {%eml|guild_<%i- id %>|}

let name_of_user (`User_id id) = {%eml|user_<%i- id %>|}

let user_dir guild_id user_id =
  Fpath.(
    Config.database_location / name_of_guild guild_id / name_of_user user_id )

open Letop.Result

let score_of_user guild_id user_id =
  let path = Fpath.(user_dir guild_id user_id / "score") in
  let+ is_file = OS.File.exists path |> result_conv in
  if is_file then
    let+ content = OS.File.read path |> result_conv in
    match int_of_string_opt content with
    | None ->
        Error ({%eml|<%S-content%> is not a valid integer|} |> Error.of_string)
    | Some v ->
        Ok v
  else Ok 0

let set_score guild_id user_id score =
  let path = user_dir guild_id user_id in
  let+ _ = path |> OS.Dir.create |> result_conv in
  let () = () in
  let path = Fpath.(path / "score") in
  OS.File.write path (string_of_int score) |> result_conv

let add_to_score guild_id user_id additional_score =
  let+ score = score_of_user guild_id user_id in
  set_score guild_id user_id (score + additional_score)
