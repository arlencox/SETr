#ifdef PKG_CUDD
module Cudd = SETr_Symbolic_BDD_Cudd

let _ =
  let open SETr_DomainRegistrar in
  let build = function
    | [] ->
        Symbolic (module Cudd)
    | _ -> build_error "Expected no arguments"
  in
  let args = "" in
  let help = "Builds a symbolic domain from CU Decision Diagrams library" in
  register "symbolic.bdd.cudd" build args help

#endif
#ifdef PKG_MLBDD
module MLBDD = SETr_Symbolic_BDD_MLBDD

let _ =
  let open SETr_DomainRegistrar in
  let build = function
    | [] ->
        Symbolic (module MLBDD)
    | _ -> build_error "Expected no arguments"
  in
  let args = "" in
  let help = "Builds a symbolic domain from ML Binary Decision Diagrams library" in
  register "symbolic.bdd.mlbdd" build args help

#endif
module XBDD = SETr_Symbolic_BDD_XBDD

let _ =
  let open SETr_DomainRegistrar in
  let build = function
    | [] ->
        Symbolic (module XBDD)
    | _ -> build_error "Expected no arguments"
  in
  let args = "" in
  let help = "Builds a symbolic domain from Xavier Leroy's Binary Decision Diagram library" in
  register "symbolic.bdd.xbdd" build args help

#ifdef PKG_CUDD
module Default = Cudd.Make(struct let reorder = None end)
module Opt = SETr_Symbolic_Packer.Make(SETr_Symbolic_Remap.Make(Default))
#else
#ifdef PKG_MLBDD
module Default = MLBDD
module Opt = SETr_Symbolic_Equality.Make(SETr_Symbolic_Packer.Make(Default))
#else
module Default = XBDD
module Opt = SETr_Symbolic_Equality.Make(SETr_Symbolic_Packer.Make(Default))
#endif
#endif

let _ =
  let open SETr_DomainRegistrar in
  let build = function
    | [] ->
        Symbolic (module Opt)
    | _ -> build_error "Expected no arguments"
  in
  let args = "" in
  let help = "Builds a symbolic domain using a combinators and a BDD library chosen for performance" in
  register "symbolic.bdd.opt" build args help
