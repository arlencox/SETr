module L = SETr_Cardinality_Logic
module SL = SETr_Symbolic_Logic
module NL = SETr_Numeric_Logic

module Make(S: SETr_Symbolic_Interface.S)(N: SETr_Numeric_Interface.S) : SETr_Cardinality_Interface.S = struct
  type ctx = {
    cs: S.ctx;
    cn: N.ctx;
  }

  type t = {
    s: S.t;
    n: N.t;
  }

  type sym = int

  type cnstr = sym L.t

  type output = cnstr

  type query = sym L.q

  let init () = {
    cs = S.init ();
    cn = N.init ();
  }

  let top ctx = {
    s = S.top ctx.cs;
    n = N.top ctx.cn;
  }

  let bottom ctx = {
    s = S.bottom ctx.cs;
    n = N.bottom ctx.cn;
  }

  module ISet = Set.Make(struct
      type t = int
      let compare = compare
    end)

  let set_of_list l = List.fold_left (fun r el -> ISet.add el r) ISet.empty l

  let symbols ctx t =
    let set_syms = S.symbols ctx.cs t.s in
    let num_syms = N.symbols ctx.cn t.n in
    set_of_list set_syms |>
    ISet.union (set_of_list num_syms) |>
    ISet.elements

  exception Unsupported

  let rec card_num_expr_to_num_expr a =
    match a with
    | L.Add (a,b) ->
      let a = card_num_expr_to_num_expr a in
      let b = card_num_expr_to_num_expr b in
      NL.Add(a,b)
    | L.Neg a ->
      let a = card_num_expr_to_num_expr a in
      NL.Neg a
    | L.Const i -> NL.Const i
    | L.Mul (a,b) ->
      let a = card_num_expr_to_num_expr a in
      let b = card_num_expr_to_num_expr b in
      NL.Mul(a,b)
    | L.Card (L.Var a) ->
      NL.Var a
    | L.Card _ ->
      raise Unsupported
    | L.NVar s ->
      NL.Var s

  let rec card_set_expr_to_set_expr a =
    match a with
    | L.Empty -> SL.Empty
    | L.Universe -> SL.Empty
    | L.DisjUnion (a,b) ->
      let a = card_set_expr_to_set_expr a in
      let b = card_set_expr_to_set_expr b in
      SL.DisjUnion (a,b)
    | L.Union (a,b) ->
      let a = card_set_expr_to_set_expr a in
      let b = card_set_expr_to_set_expr b in
      SL.Union (a,b)
    | L.Inter (a,b) ->
      let a = card_set_expr_to_set_expr a in
      let b = card_set_expr_to_set_expr b in
      SL.Inter (a,b)
    | L.Diff (a,b) ->
      let a = card_set_expr_to_set_expr a in
      let b = card_set_expr_to_set_expr b in
      SL.Diff (a,b)
    | L.Comp a ->
      let a = card_set_expr_to_set_expr a in
      SL.Comp a
    | L.Var a ->
      SL.Var a

  let rec split_constraint = function
    | L.And (a, b) ->
      let (sa, na) = split_constraint a in
      let (sb, nb) = split_constraint b in
      (SL.And(sa,sb), NL.And(na,nb))
    | L.Not a ->
      let (sa, na) = split_constraint a in
      ignore(na);
      (SL.Not sa, NL.True)
    | L.Eq (a, b) ->
      let a = card_set_expr_to_set_expr a in
      let b = card_set_expr_to_set_expr b in
      begin match a, b with
        | SL.Var sa, SL.Var sb ->
          (SL.Eq(a,b), NL.Eq(NL.Var sa, NL.Var sb))
        | _ ->
          (SL.Eq(a,b), NL.True)
      end
    | L.SubEq (a, b) ->
      let a = card_set_expr_to_set_expr a in
      let b = card_set_expr_to_set_expr b in
      begin match a, b with
        | SL.Var sa, SL.Var sb ->
          (SL.SubEq(a,b), NL.Le(NL.Var sa, NL.Var sb))
        | _ ->
          (SL.SubEq(a,b), NL.True)
      end
    | L.NLe (a, b) ->
      begin try
          let a = card_num_expr_to_num_expr a in
          let b = card_num_expr_to_num_expr b in
          (SL.True, NL.Le (a, b))
        with Unsupported ->
          (SL.True, NL.True)
      end
    | L.NLt (a, b) ->
      begin try
          let a = card_num_expr_to_num_expr a in
          let b = card_num_expr_to_num_expr b in
          (SL.True, NL.Lt (a, b))
        with Unsupported ->
          (SL.True, NL.True)
      end
    | L.NEq (a, b) ->
      begin try
          let a = card_num_expr_to_num_expr a in
          let b = card_num_expr_to_num_expr b in
          (SL.True, NL.Eq (a, b))
        with Unsupported ->
          (SL.True, NL.True)
      end
    | L.True ->
      (SL.True, NL.True)
    | L.False ->
      (SL.False, NL.False)


  let is_bottom ctx t =
    S.is_bottom ctx.cs t.s || N.is_bottom ctx.cn t.n


  let constrain ctx c t =
    (* split the constraint into set and numeric parts *)
    let (s,n) = split_constraint c in
    {
      s = S.constrain ctx.cs s t.s;
      n = N.constrain ctx.cn n t.n;
    }

  let sat ctx t c =
    is_bottom ctx (constrain ctx (L.Not c) t)


  let join ctx a b =
    {
      s = S.join ctx.cs a.s b.s;
      n = N.join ctx.cn a.n b.n;
    }

  let widening ctx a b =
    {
      s = S.widening ctx.cs a.s b.s;
      n = N.widening ctx.cn a.n b.n;
    }

  let meet ctx a b =
    {
      s = S.meet ctx.cs a.s b.s;
      n = N.meet ctx.cn a.n b.n;
    }

  let le ctx a b =
    S.le ctx.cs a.s b.s && N.le ctx.cn a.n b.n


  let is_top ctx t =
    S.is_top ctx.cs t.s && N.is_top ctx.cn t.n

  let forget ctx syms t =
    {
      s = S.forget ctx.cs syms t.s;
      n = N.forget ctx.cn syms t.n;
    }

  let rename_symbols ctx ren t =
    {
      s = S.rename_symbols ctx.cs ren t.s;
      n = N.rename_symbols ctx.cn ren t.n;
    }

  let query ctx t =
    let s_query = S.query ctx.cs t.s in
    {
      L.get_eqs = (fun () ->
          s_query.SL.get_eqs ()
        );
      L.get_eqs_sym = (fun sym ->
          s_query.SL.get_eqs_sym sym
        );
      L.get_zeros = (fun () ->
          let n_query = N.query ctx.cn t.n in
          n_query.NL.get_zeros ()
        );
    }

  let combine ctx q t =
    t

  let pp_debug ctx pp_sym ff t =
    let set_syms = Hashtbl.create 29 in
    List.iter (fun s ->
        Hashtbl.replace set_syms s ()
      ) (S.symbols ctx.cs t.s);
    let pp_n_sym ff s =
      if Hashtbl.mem set_syms s then
        Format.fprintf ff "|%a|" pp_sym s
      else
        Format.fprintf ff "%a" pp_sym s
    in
    Format.fprintf ff "%a /\\ %a"
      (S.pp_debug ctx.cs pp_sym) t.s
      (N.pp_debug ctx.cn pp_n_sym) t.n

  let pp_print ctx pp_sym ff t =
    let set_syms = Hashtbl.create 29 in
    List.iter (fun s ->
        Hashtbl.replace set_syms s ()
      ) (S.symbols ctx.cs t.s);
    let pp_n_sym ff s =
      if Hashtbl.mem set_syms s then
        Format.fprintf ff "|%a|" pp_sym s
      else
        Format.fprintf ff "%a" pp_sym s
    in
    Format.fprintf ff "%a /\\ %a"
      (S.pp_print ctx.cs pp_sym) t.s
      (N.pp_print ctx.cn pp_n_sym) t.n

  let rec of_symbolic_e = function
    | SL.Empty -> L.Empty
    | SL.Universe -> L.Universe
    | SL.DisjUnion (a, b) -> L.DisjUnion (of_symbolic_e a, of_symbolic_e b)
    | SL.Union (a, b) -> L.Union (of_symbolic_e a, of_symbolic_e b)
    | SL.Inter (a, b) -> L.Inter (of_symbolic_e a, of_symbolic_e b)
    | SL.Diff (a, b) -> L.Diff (of_symbolic_e a, of_symbolic_e b)
    | SL.Comp a -> L.Comp (of_symbolic_e a)
    | SL.Var s -> L.Var s

  let rec of_symbolic_t = function
    | SL.Eq (a, b) -> L.Eq (of_symbolic_e a, of_symbolic_e b)
    | SL.SubEq (a, b) -> L.SubEq (of_symbolic_e a, of_symbolic_e b)
    | SL.And (a, b) -> L.And (of_symbolic_t a, of_symbolic_t b)
    | SL.Not a -> L.Not (of_symbolic_t a)
    | SL.True -> L.True
    | SL.False -> L.False

  module NL = SETr_Numeric_Logic

  let rec of_numeric_e ss = function
    | NL.Add (a, b) -> L.Add (of_numeric_e ss a, of_numeric_e ss b)
    | NL.Mul (a, b) -> L.Mul (of_numeric_e ss a, of_numeric_e ss b)
    | NL.Neg a -> L.Neg (of_numeric_e ss a)
    | NL.Const i -> L.Const i
    | NL.Var s ->
      if ISet.mem s ss then
        L.Card (L.Var s)
      else
        L.NVar s

  let rec of_numeric_t ss = function
    | NL.Eq (a, b) -> L.NEq (of_numeric_e ss a, of_numeric_e ss b)
    | NL.Le (a, b) -> L.NLe (of_numeric_e ss a, of_numeric_e ss b)
    | NL.Lt (a, b) -> L.NLt (of_numeric_e ss a, of_numeric_e ss b)
    | NL.And (a, b) -> L.And (of_numeric_t ss a, of_numeric_t ss b)
    | NL.Not a -> L.Not (of_numeric_t ss a)
    | NL.True -> L.True
    | NL.False -> L.False

  let serialize ctx t =
    let ls = S.serialize ctx.cs t.s in
    let ls = of_symbolic_t ls in
    let set_syms = S.symbols ctx.cs t.s |> set_of_list in
    let ln = N.serialize ctx.cn t.n in
    let ln = of_numeric_t set_syms ln in
    match ls, ln with
    | L.True, o | o, L.True -> o
    | ls, ln -> L.And(ls,ln)

end


let _ =
  let open SETr_DomainRegistrar in
  let build = function
    | [Symbolic s; Numeric n] ->
        Cardinality (module Make((val s))((val n)))
    | _ -> build_error "Expected a symbolic domain and a numeric domain"
  in
  let args = "(<sym>,<num>)" in
  let help = "Builds a cardinality domain from a symbolic domain <sym> and a numeric domain <num>" in
  register "cardinality.combiner" build args help
  
