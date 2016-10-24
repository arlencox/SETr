type t =
  | Bool of bool
  | String of string
  | Int of int
  | Symbolic of (module SETr_Symbolic_Interface.S)
  | SymSing of (module SETr_SymSing_Interface.S)
  | Cardinality of (module SETr_Cardinality_Interface.S)
  | Numeric of (module SETr_Numeric_Interface.S)
  | NumericValue of (module SETr_Numeric_Value_Interface.S)

exception Build_error of string

let build_error msg =
  raise (Build_error msg)

let registered : (string, (t list -> t) * (string * string) * bool) Hashtbl.t
  = Hashtbl.create 29

let register name build args help =
  Hashtbl.replace registered name (build,(args,help), true)

let alias newname oldname =
  let (build, help, print) = Hashtbl.find registered oldname in
  Hashtbl.replace registered newname (build, help, false)

let is_registered name =
  Hashtbl.mem registered name
