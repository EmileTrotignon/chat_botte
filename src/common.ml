let dont_wait_exn f = Lwt.dont_wait f (fun exn -> raise exn)

include Lwt.Syntax

module Result_syntax = struct
  let ( let+ ) r f = Result.map f r

  let ( let* ) = Result.bind
end

let combine_errors li =
  li
  |> Core.List.fold_left ~init:(Ok []) ~f:(fun acc ele ->
         match (ele, acc) with
         | Ok ele, Ok li ->
             Ok (ele :: li)
         | Error err_ele, Ok _ ->
             Error [err_ele]
         | Ok _, Error err_acc ->
             Error err_acc
         | Error err_ele, Error err_acc ->
             Error (err_ele :: err_acc) )
  |> Result.map_error (String.concat "\n")

let option_lwt_iter opt ~f =
  match opt with None -> Lwt.return_unit | Some v -> f v
