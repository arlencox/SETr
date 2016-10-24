module L = SETr_SymSing_Logic
module CL = SETr_Cardinality_Logic

module Make(D: SETr_Cardinality_Interface.S) : SETr_SymSing_Interface.S = struct
  type ctx = D.ctx

  type t = D.t

  type sym = int

  type cnstr = int L.t

  type output = int L.t

  type query = int L.q

  let init = D.init

  let top ctx = D.top ctx

  let bottom ctx = D.bottom ctx

  let symbols ctx t = D.symbols ctx t

  let rec of_symsing_e = function
    | L.Empty -> (CL.Empty, CL.True)
    | L.Universe -> (CL.Universe, CL.True)
    | L.DisjUnion (a, b) ->
      let a, ca = of_symsing_e a in
      let b, cb = of_symsing_e b in
      (CL.DisjUnion (a, b), CL.And(ca, cb))
    | L.Union (a, b) ->
      let a, ca = of_symsing_e a in
      let b, cb = of_symsing_e b in
      (CL.Union (a, b), CL.And(ca, cb))
    | L.Inter (a, b) ->
      let a, ca = of_symsing_e a in
      let b, cb = of_symsing_e b in
      (CL.Inter (a, b), CL.And(ca, cb))
    | L.Diff (a, b) ->
      let a, ca = of_symsing_e a in
      let b, cb = of_symsing_e b in
      (CL.Diff (a, b), CL.And(ca, cb))
    | L.Comp a ->
      let a, ca = of_symsing_e a in
      (CL.Comp a, ca)
    | L.Var v ->
      (CL.Var v, CL.True)
    | L.Sing v ->
      (CL.Var v, CL.NEq (CL.Card (CL.Var v), CL.Const 1))


  let rec of_symsing_t = function
    | L.Eq (a, b) ->
      let a, ca = of_symsing_e a in
      let b, cb = of_symsing_e b in
      CL.And (CL.Eq (a, b), CL.And (ca, cb))
    | L.SubEq (a, b) ->
      let a, ca = of_symsing_e a in
      let b, cb = of_symsing_e b in
      CL.And (CL.SubEq (a, b), CL.And (ca, cb))
    | L.In (v, b) ->
      let b, cb = of_symsing_e b in
      CL.And (CL.SubEq (CL.Var v, b),
              CL.And ( CL.NEq (CL.Card (CL.Var v), CL.Const 1),
                       CL.And (
                         CL.NLe (CL.Const 1, CL.Card b),
                         cb)))
    | L.And (a, b) ->
      CL.And (of_symsing_t a, of_symsing_t a)
    | L.Not a ->
      CL.Not (of_symsing_t a)
    | L.True -> CL.True
    | L.False -> CL.False

  let constrain ctx c t =
    let c = of_symsing_t c in
    D.constrain ctx c t

  let serialize ctx t =
    failwith "unimplemented"

  let sat ctx a c =
    let c = of_symsing_t c in
    D.sat ctx a c

  let join ctx a b = D.join ctx a b

  let widening ctx a b = D.widening ctx a b

  let meet ctx a b = D.meet ctx a b

  let le ctx a b = D.le ctx a b

  let is_bottom ctx a = D.is_bottom ctx a

  let is_top ctx a = D.is_top ctx a

  let forget ctx syms a = D.forget ctx syms a

  let rename_symbols ctx rename a = D.rename_symbols ctx rename a

  let query ctx a =
    let res = D.query ctx a in
    {
      L.get_eqs = res.CL.get_eqs;
      L.get_eqs_sym = res.CL.get_eqs_sym;
    }

  let combine ctx q a =
    let q = {
      CL.get_eqs = q.L.get_eqs;
      CL.get_eqs_sym = q.L.get_eqs_sym;
      CL.get_zeros = (fun () -> [])
    } in
    D.combine ctx q a

  let pp_print ctx pp ff t =
    D.pp_print ctx pp ff t

  let pp_debug ctx pp ff t =
    D.pp_debug ctx pp ff t


end

let _ =
  let open SETr_DomainRegistrar in
  let build = function
    | [Cardinality c] ->
        SymSing (module Make((val c)))
    | _ -> build_error "Expected a cardinality domain"
  in
  let args = "(<card>)" in
  let help = "Builds a symbolic with singletons domain from a cardinality domain <card>" in
  register "symsing.card" build args help
