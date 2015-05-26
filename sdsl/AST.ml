type var = string

module L = LogicSymbolicSet

type t =
  | Skip
  | Seq of t * t
  | Branch of t * t
  | Both of t * t
  | Loop of t
  | Kill of var
  | Rename of var * var
  | Assign of var * var L.e
  | Choose of var * var
  | Assume of var L.t
  | Assert of var L.t
