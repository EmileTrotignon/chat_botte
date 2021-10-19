open Core

include module type of Disml.Models.Member

val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t

val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

val ( = ) : t -> t -> bool

val compare : t -> t -> int

val visible_name : t -> string

module Set : Set.S with type Elt.t = t

module Map : Map.S with type Key.t = t
