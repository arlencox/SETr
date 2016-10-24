module Interface = SETr_SymSing_Interface
module Lin = SETr_SymSing_Lin
module Logic = SETr_SymSing_Logic
module Sing = SETr_SymSing_Sing
module Card = SETr_SymSing_Card

let _ =
  let open SETr_DomainRegistrar in
  let build = function
    | [] ->
        SymSing (module Lin)
    | _ -> build_error "Expected no arguments"
  in
  let args = "" in
  let help = "Builds a symbolic with singletons domain from that uses a syntactic tracking of linear disjoint union constraints" in
  register "symsing.lin" build args help
