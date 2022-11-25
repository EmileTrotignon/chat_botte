open Core

module Result = struct
  module M = Result

  let ( let* ) v f = M.map ~f v

  let ( let+ ) v f = M.bind v ~f

  let ( let- ) v f = M.iter v ~f
end

module Option = struct
  let ( let* ) v f = Option.map ~f v

  let ( let+ ) v f = Option.bind v ~f

  let ( let- ) v f = Option.iter v ~f
end

module Deferred = struct
  open Async

  let ( let* ) v f = Deferred.map ~f v

  let ( let+ ) v f = Deferred.bind v ~f
end

module Deferred_or_error = struct
  open Async

  let ( let* ) v f = Deferred.Or_error.map ~f v

  let ( let+ ) v f = Deferred.Or_error.bind v ~f

  let ( let- ) v f =
    let* v = v in
    Core.Result.iter v ~f
end
