module L = SETr_Numeric_Logic

module Make(V: SETr_Numeric_Value_Interface.S) : SETr_Numeric_Interface.S =
struct
  module SMap = Map.Make(struct
      type t = int
      let compare a b = a - b
    end)

  type sym = int
  type query = sym L.q
  type cnstr = sym L.t
  type output = sym L.t

  type ctx = V.ctx

  type t = 
    | Bottom
    | Value of V.t SMap.t

  let init () =
    V.init ()

  let top c = Value SMap.empty

  let bottom c = Bottom

  let upper_bound op c a b =
    SMap.merge (fun i a b ->
        match a, b with
        | Some a, Some b -> Some (op c a b)
        | None, _ | _, None -> None
      ) a b

  let join c a b =
    match a, b with
    | Bottom, o | o, Bottom -> o
    | Value a, Value b -> Value (upper_bound V.join c a b)

  let widening c a b =
    match a, b with
    | Bottom, o | o, Bottom -> o
    | Value a, Value b -> Value (upper_bound V.widening c a b)

  exception Early_fail

  let meet c a b =
    SMap.merge (fun i a b ->
        match a, b with
        | Some a, Some b ->
          let res = V.meet c a b in
          if V.is_bottom c res then
            raise Early_fail
          else
            Some res
        | o, None | None, o -> o
      ) a b

  let meet c a b =
    match a, b with
    | Bottom, _ | _, Bottom -> Bottom
    | Value a, Value b ->
      try
        Value (meet c a b)
      with Early_fail ->
        Bottom


  let le c a b =
    SMap.merge (fun i a b ->
        match a, b with
        | _, None -> None
        | None, Some _ -> raise Early_fail
        | Some a, Some b ->
          if not (V.le c a b) then
            raise Early_fail;
          None
      ) a b

  let le c a b =
    match a, b with
    | Bottom, _ -> true
    | _, Bottom -> false
    | Value a, Value b ->
      try
        ignore(le c a b);
        true
      with Early_fail ->
        false

  let is_bottom c = function
    | Bottom -> true
    | _ -> false

  let is_top c = function
    | Bottom -> false
    | Value v ->
      SMap.for_all (fun i v -> V.is_top c v) v

  let symbols c = function
    | Bottom -> []
    | Value v -> SMap.fold (fun i _ l -> i::l) v []

  type atree =
    | A_bin of L.bop * atree * V.t * atree * V.t
    | A_un of L.uop * atree * V.t
    | A_cst of V.t
    | A_var of int * V.t

  let rec eval c m = function
    | L.Add (e1, e2) ->
      let a1,v1 = eval c m e1 in
      let a2,v2 = eval c m e2 in
      A_bin (L.BAdd,a1,v1,a2,v2), V.binary c L.BAdd v1 v2
    | L.Mul (e1, e2) ->
      let a1,v1 = eval c m e1 in
      let a2,v2 = eval c m e2 in
      A_bin (L.BMul,a1,v1,a2,v2), V.binary c L.BMul v1 v2
    | L.Neg e1 ->
      let a1,v1 = eval c m e1 in
      A_un (L.UNeg,a1,v1), V.unary c L.UNeg v1
    | L.Const i ->
      let v = V.const c i in
      A_cst v, v
    | L.Var v ->
      let r = try SMap.find v m with Not_found -> V.top c in
      A_var (v, r), r

  let rec refine c m a r =
    match a with
    | A_bin (op, a1, v1, a2, v2) ->
      let w1,w2 = V.bwd_binary c op r v1 v2 in
      refine c (refine c m a1 w1) a2 w2
    | A_un (op, a1, v1) ->
      let w1 = V.bwd_unary c op r v1 in
      refine c m a1 w1
    | A_var (var,v) ->
      let w = V.meet c v r in
      if V.is_bottom c w then raise Early_fail;
      SMap.add var w m
    | A_cst v ->
      if V.is_bottom c (V.meet c v r) then raise Early_fail;
      m

  let apply_compare c op e1 e2 t =
    match t with
    | Bottom -> Bottom
    | Value m ->
      try
        (* evaluate forward each expression *)
        let a1,v1 = eval c m e1 in
        let a2,v2 = eval c m e2 in
        (* apply comparison *)
        let r1,r2 = V.compare c op v1 v2 in
        (* perform backward refinement *)
        Value (refine c (refine c m a1 r1) a2 r2)
      with Early_fail ->
        Bottom


  let rec constrain c t pol = function
    | L.True -> if pol then t else Bottom
    | L.False -> if pol then Bottom else t
    | L.Not cnstr -> constrain c t (not pol) cnstr
    | L.And (a, b) ->
      if pol then
        constrain c (constrain c t pol a) pol b
      else
        let a = constrain c t pol a in
        let b = constrain c t pol b in
        join c a b
    | L.Eq (a, b) ->
      if pol then
        apply_compare c L.CEq a b t
      else
        constrain c t pol (L.And (L.Le (a, b), L.Le (b, a)))
    | L.Le (a, b) ->
      if pol then
        apply_compare c L.CLe a b t
      else
        apply_compare c L.CLt b a t
    | L.Lt (a, b) ->
      if pol then
        apply_compare c L.CLt a b t
      else
        apply_compare c L.CLe b a t

  let constrain c cnstr t =
    (* Format.fprintf Format.err_formatter "%a@."
      (L.pp (fun ff s -> Format.fprintf ff "v%d" s)) cnstr;*)
    constrain c t true cnstr

  let serialize c = function
    | Bottom ->
      L.False
    | Value v ->
      let res = SMap.bindings v |>
                List.rev_map (fun (s,v) -> V.serialize c (L.Var s) v) |>
                List.filter (function | L.True -> false | _ -> true) in
      match res with
      | [] -> L.True
      | h::r ->
        List.fold_left (fun r el -> L.And (r, el)) h r

  let sat c t cnstr =
    is_bottom c (constrain c (L.Not cnstr) t)

  let forget c syms = function
    | Bottom -> Bottom
    | Value v -> Value (List.fold_left (fun v s -> SMap.remove s v) v syms)

  let rename_symbols c ren = function
    | Bottom -> Bottom
    | Value v -> Value (SMap.fold (fun s v r ->
        SMap.add (SETr_Rename.get ren s) v r
      ) v SMap.empty)

  let query c t =
    {
      L.get_eqs = (fun () -> []);
      L.get_eqs_sym = (fun _s -> []);
      L.get_zeros = (fun () -> []);
    }

  let combine c q t = t

  let pp_debug c pp_sym ff t =
    L.pp pp_sym ff (serialize c t)

  let pp_print = pp_debug
end


let _ =
  let open SETr_DomainRegistrar in
  let build = function
    | [NumericValue s] ->
        Numeric (module Make((val s)))
    | _ -> build_error "Expected a value domain"
  in
  let args = "(<val>)" in
  let help = "Builds a non-relational numeric domain from a value domain <val>" in
  register "numeric.nonrelational" build args help
