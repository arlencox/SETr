module BDD = SETr_Symbolic_BDD
module Debugger = SETr_Symbolic_Debugger
module Equality = SETr_Symbolic_Equality
module Logger = SETr_Symbolic_Logger
module Packer = SETr_Symbolic_Packer
#ifdef PKG_Z3
module QUICG = SETr_Symbolic_QUICG
module SMT = SETr_Symbolic_SMT

let _ =
  let open SETr_DomainRegistrar in
  let build = function
    | [] ->
        Symbolic (module QUICG)
    | _ -> build_error "Expected no arguments"
  in
  let args = "" in
  let help = "Builds a symbolic domain from the Quick Graphs library" in
  register "symbolic.quicg" build args help;
  alias "quicg" "symbolic.quicg";
  let build = function
    | [] ->
        Symbolic (module SMT)
    | _ -> build_error "Expected no arguments"
  in
  let args = "" in
  let help = "Builds a symbolic domain from an SMT solver" in
  register "symbolic.smt" build args help;
  alias "smt" "symbolic.smt"

#endif
module Remap = SETr_Symbolic_Remap
module Stats = SETr_Symbolic_Stats
module Tracer = SETr_Symbolic_Tracer