module L = SETr_SymSing_Logic
module LS = SETr_Symbolic_Logic

module Make(D: SETr_Symbolic_Interface.S) : SETr_SymSing_Interface.S = struct

  module SS = Set.Make(struct
      type t = int
      let compare a b = b - a
    end)

  type ctx = D.ctx

  type t =
    {
      d : D.t;
      s : SS.t;
    }

  type sym = int

  type cnstr = int L.t

  type output = int L.t

  type query = int L.q

  let init = D.init

  let top ctx = {
    d = D.top ctx;
    s = SS.empty;
  }

  let bottom ctx = {
    d = D.bottom ctx;
    s = SS.empty;
  }

  let symbols ctx t = D.symbols ctx t.d

  let rec rewrite_e = function
    | L.Empty -> (LS.Empty, SS.empty)
    | L.Universe -> (LS.Universe, SS.empty)
    | L.DisjUnion(a,b) ->
      let (a,ai) = rewrite_e a in
      let (b,bi) = rewrite_e b in
      (LS.DisjUnion(a,b), SS.union ai bi)
    | L.Union(a,b) ->
      let (a,ai) = rewrite_e a in
      let (b,bi) = rewrite_e b in
      (LS.Union(a,b), SS.union ai bi)
    | L.Inter(a,b) ->
      let (a,ai) = rewrite_e a in
      let (b,bi) = rewrite_e b in
      (LS.Inter(a,b), SS.union ai bi)
    | L.Diff(a,b) ->
      let (a,ai) = rewrite_e a in
      let (b,bi) = rewrite_e b in
      (LS.Diff(a,b), SS.union ai bi)
    | L.Comp(a) ->
      let (a,ai) = rewrite_e a in
      (LS.Comp a, ai)
    | L.Var v ->
      (LS.Var v, SS.empty)
    | L.Sing v ->
      (LS.Var v, SS.singleton v)

  let rec rewrite is_pos t =
    let opt_n e =
      if is_pos then
        e
      else
        LS.Not e
    in
    match t, is_pos with
    | L.Eq(a,b), _ ->
      let (a,ai) = rewrite_e a in
      let (b,bi) = rewrite_e b in
      (opt_n (LS.Eq(a,b)), SS.union ai bi)
    | L.SubEq(a,b), _ ->
      let (a,ai) = rewrite_e a in
      let (b,bi) = rewrite_e b in
      (opt_n (LS.SubEq(a,b)), SS.union ai bi)
    | L.In (a,b), true ->
      let (b,bi) = rewrite_e b in
      (LS.SubEq(LS.Var a, b), SS.add a bi)
    | L.In (a,b), false ->
      let (b,bi) = rewrite_e b in
      (LS.Eq(LS.Inter(LS.Var a, b), LS.Empty), SS.add a bi)
    | L.And(a,b), _ ->
      let (a,ai) = rewrite true a in
      let (b,bi) = rewrite true b in
      (opt_n (LS.And(a,b)), SS.union ai bi)
    | L.Not a, n ->
      rewrite (not n) a
    | L.True, true -> (LS.True, SS.empty)
    | L.True, false -> (LS.False, SS.empty)
    | L.False, true -> (LS.False, SS.empty)
    | L.False, false -> (LS.True, SS.empty)

  let rewrite = rewrite true

  let rec rrewrite_e ss = function
    | LS.Empty -> L.Empty
    | LS.Universe -> L.Universe
    | LS.DisjUnion(a,b) ->
      let a = rrewrite_e ss a in
      let b = rrewrite_e ss b in
      L.DisjUnion(a,b)
    | LS.Union(a,b) ->
      let a = rrewrite_e ss a in
      let b = rrewrite_e ss b in
      L.Union(a,b)
    | LS.Inter(a,b) ->
      let a = rrewrite_e ss a in
      let b = rrewrite_e ss b in
      L.Inter(a,b)
    | LS.Diff(a,b) ->
      let a = rrewrite_e ss a in
      let b = rrewrite_e ss b in
      L.Diff(a,b)
    | LS.Comp(a) ->
      let a = rrewrite_e ss a in
      L.Comp a
    | LS.Var v ->
      if SS.mem v ss then
        L.Sing v
      else
        L.Var v

  let rec rrewrite ss t =
    match t with
    | LS.Eq(a,b) ->
      let a = rrewrite_e ss a in
      let b = rrewrite_e ss b in
      L.Eq(a,b)
    | LS.SubEq(a,b) ->
      let a = rrewrite_e ss a in
      let b = rrewrite_e ss b in
      begin match a with
        | L.Sing v -> L.In(v,b)
        | a -> L.SubEq(a,b)
      end
    | LS.And(a,b) ->
      let a = rrewrite ss a in
      let b = rrewrite ss b in
      L.And(a,b)
    | LS.Not a ->
      let a =rrewrite ss a in
      L.Not a
    | LS.True -> L.True
    | LS.False -> L.False

  let constrain ctx c {d;s} =
    let (c,ci) = rewrite c in
    let d = D.constrain ctx c d in
    let s = SS.union s ci in
    {d;s}

  let serialize ctx {d;s} =
    let o = D.serialize ctx d in
    rrewrite s o

  let sat ctx a c =
    let (c,_ci) = rewrite c in
    D.sat ctx a.d c

  let join ctx a b =
    {
      d = D.join ctx a.d b.d;
      s = SS.union a.s b.s;
    }

  let widening ctx a b =
    {
      d = D.widening ctx a.d b.d;
      s = SS.union a.s b.s;
    }

  let meet ctx a b =
    {
      d = D.meet ctx a.d b.d;
      s = SS.union a.s b.s;
    }

  let le ctx a b =
    D.le ctx a.d b.d

  let is_bottom ctx a =
    D.is_bottom ctx a.d

  let is_top ctx a =
    D.is_top ctx a.d

  let forget ctx syms a = 
    {
      d = D.forget ctx syms a.d;
      s = List.fold_left (fun s e -> SS.remove e s) a.s syms;
    }

  let rename_symbols ctx rename a =
    {
      d = D.rename_symbols ctx rename a.d;
      s = SS.fold (fun e s -> SS.add (SETr_Rename.get rename e) s) a.s SS.empty;
    }

  let query ctx a =
    let qs = D.query ctx a.d in
    {
      L.get_eqs = qs.LS.get_eqs;
      L.get_eqs_sym = qs.LS.get_eqs_sym;
    }

  let combine ctx q a = 
    let qs = {
      LS.get_eqs = q.L.get_eqs;
      LS.get_eqs_sym = q.L.get_eqs_sym;
    } in
    {a with d = D.combine ctx qs a.d}

  let pp_sym pp_sym t ff s =
    if SS.mem s t.s then
      Format.fprintf ff "{%a}" pp_sym s
    else
      pp_sym ff s

  let pp_print ctx pp ff t =
    D.pp_print ctx (pp_sym pp t) ff t.d

  let pp_debug ctx pp ff t =
    D.pp_print ctx (pp_sym pp t) ff t.d


end

let _ =
  let open SETr_DomainRegistrar in
  let build = function
    | [Symbolic s] ->
        SymSing (module Make((val s)))
    | _ -> build_error "Expected a symbolic domain"
  in
  let args = "(<sym>)" in
  let help = "Builds a symbolic with singletons domain from a symbolic domain <sym>" in
  register "symsing.sing" build args help
