module L = SETr_Symbolic_Logic
module Rename = SETr_Rename

module Int = struct
  type t = int
  let compare = compare
  let equal = (=)
  let hash = Hashtbl.hash
  let pp = Format.pp_print_int
end

module B = SETr_Symbolic_BDD_XImp.Make(Int)

type ctx = B.ctx

type t = B.t

type sym = int

type cnstr = sym L.t

type output = cnstr

type query = sym L.q 

let init () = B.init ()

let top ctx = B.true_ ctx

let bottom ctx = B.false_ ctx

let symbols ctx t =
  B.support ctx t

let rec of_expr ctx = function
  | L.Empty ->
    (B.false_ ctx, B.true_ ctx)
  | L.Universe ->
    (B.true_ ctx, B.true_ ctx)
  | L.DisjUnion (a,b) -> 
    let (a,ac) = of_expr ctx a in
    let (b,bc) = of_expr ctx b in
    (* A ^ B = 0
       A /\ B = false
       ~(A /\ B) *)
    (B.or_ ctx a b, (B.and_ ctx (B.and_ ctx ac bc) (B.not_ ctx (B.and_ ctx a b))))
  | L.Union (a,b) ->
    let (a,ac) = of_expr ctx a in
    let (b,bc) = of_expr ctx b in
    (B.or_ ctx a b, B.and_ ctx ac bc)
  | L.Inter (a,b) ->
    let (a,ac) = of_expr ctx a in
    let (b,bc) = of_expr ctx b in
    (B.and_ ctx a b, B.and_ ctx ac bc)
  | L.Diff (a,b) ->
    let (a,ac) = of_expr ctx a in
    let (b,bc) = of_expr ctx b in
    (B.and_ ctx a (B.not_ ctx b), B.and_ ctx ac bc)
  | L.Comp a ->
    let (a,ac) = of_expr ctx a in
    (B.not_ ctx a, ac)
  | L.Var v ->
    (B.var_ ctx v, B.true_ ctx)

exception Unsupported

let rec of_cnstr is_pos is_over ctx c =
  let r is_inv = of_cnstr is_inv is_over ctx in
  match c, is_pos with
  | L.Eq (a,b), true ->
    let (a,ac) = of_expr ctx a in
    let (b,bc) = of_expr ctx b in
    B.and_ ctx (B.equiv_ ctx a b) (B.and_ ctx ac bc)
  | L.Eq _, false ->
    if is_over then
      B.true_ ctx
    else
      raise Unsupported
  | L.SubEq (a,b), true ->
    let (a,ac) = of_expr ctx a in
    let (b,bc) = of_expr ctx b in
    B.and_ ctx (B.imply_ ctx a b) (B.and_ ctx ac bc)
  | L.SubEq _, false ->
    if is_over then
      B.true_ ctx
    else
      raise Unsupported
  | L.And (a,b), true ->
    let a = r is_pos a in
    let b = r is_pos b in
    B.and_ ctx a b

  | L.And (a,b), false ->
    let a = r is_pos a in
    let b = r is_pos b in
    if is_over then
      B.or_ ctx a b
    else 
      raise Unsupported
  | L.Not a, _ ->
    r (not is_pos) a
  | L.True, false
  | L.False, true ->
    B.false_ ctx
  | L.False, false
  | L.True, true ->
    B.true_ ctx

let constrain ctx cnstr t =
  let c = of_cnstr true true ctx cnstr in
  B.and_ ctx c t


let rec bin_of_list emp op acc = function
  | h1::h2::rest -> bin_of_list emp op ((op h1 h2)::acc) rest
  | [h] -> bin_of_list emp op (h::acc) []
  | [] -> match acc with
    | [] -> emp
    | [h] -> h
    | rest -> bin_of_list emp op [] rest 

let bin_of_list emp op l = bin_of_list emp op [] l

let serialize t =
  failwith "serialize unimplemented"
  (*B.allprime (B.not_ t) |>
  List.map (fun el ->
     let (l,r) =
       el |>
       List.map (fun (b,v) -> (b, L.Var v)) |>
       List.partition fst in
      let l = List.map snd l in
      let l = bin_of_list L.Universe (fun a b -> L.Inter (a,b)) l in
      let r = List.map snd r in
      let r = bin_of_list L.Empty (fun a b -> L.Union (a,b)) r in
      L.SubEq (l,r)
    ) |>
  bin_of_list L.True (fun a b -> L.And (a,b))*)

let sat ctx t cnstr =
  try
    let c = of_cnstr true false ctx cnstr in
    B.is_true ctx (B.imply_ ctx t c)
  with Unsupported ->
    false


let join ctx a b = B.or_ ctx a b

let widening = join

let meet ctx a b = B.and_ ctx a b

let le ctx a b = B.is_true ctx (B.imply_ ctx a b)

let forget ctx syms t = B.exists_ ctx syms t

let is_bottom ctx t = B.is_false ctx t

let is_top ctx t = B.is_true ctx t



let rename_symbols ctx rename t =
  let v = {
    B.true_ = B.true_ ctx;
    B.false_ = B.false_ ctx;
    B.if_ = (fun v l r ->
        let v = Rename.get rename v in
        B.ite_ ctx (B.var_ ctx v) l r
      );
  } in
  B.visit ctx v t


module SymSymSet = Set.Make(struct
    type t = int * int
    let compare (a1,b1) (a2,b2) =
      let res = a1 - a2 in
      if res <> 0 then res
      else b1 - b2
  end)

module SymSet = Set.Make(struct
    type t = int
    let compare = (-)
  end)

let query ctx t =
  let get_eqs () = [] in
  let get_eqs_sym sym = [] in
  {
    L.get_eqs = get_eqs;
    L.get_eqs_sym = get_eqs_sym;
  }

let combine ctx q t =
  List.fold_left (fun t (s1, s2) ->
        constrain ctx (L.Eq (L.Var s1, L.Var s2)) t
    ) t (q.L.get_eqs ())

let rec pp_print pp_sym ff t =
  match t with
  | B.True -> Format.fprintf ff "true"
  | B.False -> Format.fprintf ff "true"
  | B.If(l,v,r,_id) -> Format.fprintf ff "ite(%a,%a,%a)" pp_sym v (pp_print pp_sym) l (pp_print pp_sym) r

let pp_print ctx pp_sym ff t =
  pp_print pp_sym ff t

let pp_debug = pp_print
