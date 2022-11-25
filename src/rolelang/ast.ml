type id = Everyone | Role of int | User of int

type t = Id of id | Not of t | Or of t * t | And of t * t
