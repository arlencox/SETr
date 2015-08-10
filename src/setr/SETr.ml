module type Domain = SETr_Interface.Domain
module Rename = SETr_Rename
(*module Select = SETr_Select*)
module DS = SETr_DS
module SymSing = SETr_SymSing
module Symbolic = SETr_Symbolic


type t = SETr_DomainBuilder.t =
  | Bool of bool
  | String of string
  | Int of int
  | Symbolic of (module SETr_Symbolic_Interface.S)
  | SymSing of (module SETr_SymSing_Interface.S)

let get : string -> t = SETr_Select.process
let help : string = SETr_DomainBuilder.help_string
