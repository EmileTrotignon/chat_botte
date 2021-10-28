type id =
| Everyone
| Role of int
| User of int

type t =
| Id of id
| Or of t * t
| And of t * t