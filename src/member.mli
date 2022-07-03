open Core

include module type of Disml.Models.Member

val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t

val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

val ( = ) : t -> t -> bool

val compare : t -> t -> int

val visible_name : t -> string

val ping_text : t -> string

val has_role : t -> Disml.Models.Role_id.t -> bool

module Hashtbl : Hashtbl.S with type key = t