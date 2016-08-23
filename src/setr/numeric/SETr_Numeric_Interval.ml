module L = SETr_Numeric_Logic

type ctx = unit
type sym = int
type cnstr = int L.t
type output = int L.t
type query = int L.q

module SMap = Map.Make(struct
    type t = int
    let compare a b = a - b
  end)

module Value = struct
  type lb =
    | LInt of int
    | LNInf
  type ub =
    | UInt of int
    | UInf
  type t = lb * ub

  let const i = (LInt i, UInt i)
  let join_lb a b =
    match a, b with
    | LNInf, _
    | _, LNInf -> LNInf
    | LInt i, LInt j -> LInt (min i j)

  let join_ub a b =
    match a, b with
    | UInf, _
    | _, UInf -> UInf
    | UInt i, UInt j -> UInt (max i j)

  let meet_lb a b =
    match a, b with
    | LNInf, o
    | o, LNInf -> o
    | LInt i, LInt j -> LInt (max i j)

  let meet_ub a b =
    match a, b with
    | UInf, o
    | o, UInf -> o
    | UInt i, UInt j -> UInt (min i j)

  let widening_lb a b =
    match a, b with
    | _, LNInf
    | LNInf, _ -> LNInf
    | LInt i, LInt j ->
      if i < j then
        LNInf
      else
        LInt i

  let widening_ub a b =
    match a, b with
    | _, UInf
    | UInf, _ -> UInf
    | UInt i, UInt j ->
      if i < j then
        UInf
      else
        UInt i

  let le_lb a b =
    match a, b with
    | LNInf, _ -> true
    | _, LNInf -> false
    | LInt i, LInt j -> i <= j

  let le_ub a b =
    match a, b with
    | _, UInf -> true
    | UInf, _ -> false
    | UInt i, UInt j -> i <= j

  let join (la,ua) (lb,ub) =
    (join_lb la lb, join_ub ua ub)

  let meet (la,ua) (lb,ub) =
    (meet_lb la lb, meet_ub ua ub)

  let widening (la,ua) (lb,ub) =
    (widening_lb la lb, widening_ub ua ub)

  let le (la,ua) (lb,ub) =
    le_lb la lb && le_ub ua ub

  let is_bottom = function
    | LInt i, UInt j -> i > j
    | _ -> false

  let is_top = function
    | LNInf, UInf -> true
    | _ -> false

  let serialize sym (l,u) =
    if is_bottom (l, u) then
      L.False
    else match (l,u) with
      | LInt i, UInt j ->
        L.And (L.Le (L.Const i, L.Var sym), L.Le (L.Var sym, L.Const j))
      | LNInf, UInt j ->
        L.Le (L.Var sym, L.Const j)
      | LInt i, UInf ->
        L.Le (L.Const i, L.Var sym)
      | LNInf, UInf ->
        L.True
end

type t = Value.t SMap.t option

let init () = ()

let top () = Some (SMap.empty)

let bottom () = None

let symbols () = function
  | None -> []
  | Some m -> SMap.bindings m |> List.rev_map fst

let serialize () = function
  | None -> L.False
  | Some m ->
    let res = 
      SMap.bindings m |>
      List.rev_map (fun (s,v) -> Value.serialize s v) |>
      List.filter (function
          | L.True -> false
          | _ -> true)
    in
    if List.exists (function L.False -> true | _ -> false) res then
      L.False
    else match res with
      | [] -> L.True
      | h::r ->
        List.fold_left (fun r el -> L.And (r, el)) h r

let is_bottom () = function
  | None -> true
  | Some m ->
    SMap.exists (fun k v -> Value.is_bottom v) m

let rec sat () t c =
  match c with
  | L.True -> true
  | L.False ->
    if is_bottom () t then
      true
    else
      false
  | L.And (c1, c2) ->
    sat () t c1 && sat () t c2

let constrain () c = function
  | None -> None
  

let upper_bound v_join () a b : t =
  match a, b with
  | None, o
  | o, None -> o
  | Some a, Some b ->
    Some (SMap.merge (fun s v1 v2 ->
        match v1, v2 with
        | Some v1, Some v2 -> Some (v_join v1 v2)
        | Some o, None
        | None, Some o -> Some o
        | None, None -> None
      ) a b)

let join = upper_bound Value.join

let widening = upper_bound Value.widening

let meet () a b =
  match a, b with
  | None, _
  | _, None -> None
  | Some a, Some b ->
    let res = Some (SMap.merge (fun s v1 v2 ->
        match v1, v2 with
        | Some v1, Some v2 -> Some (Value.meet v1 v2)
        | None, _
        | _, None -> None
      ) a b)
    in
    if is_bottom () res then
      None
    else
      res

exception Short_circuit

let le () a b =
  match a, b with
  | Some a, Some b ->
    begin try
        ignore (SMap.merge (fun s v1 v2 ->
            match v1, v2 with
            | Some a, Some b ->
              if Value.le a b then
                None
              else
                raise Short_circuit
            | Some a, None ->
              raise Short_circuit
            | _ ->
              None
          ) a b);
        true
      with Short_circuit ->
        false
    end
  | None, _ -> true
  | _, None -> false

let is_top () = function
  | None -> false
  | Some m ->
    SMap.for_all (fun k v -> Value.is_top v) m

let forget () ss = function
  | None -> None
  | Some m ->
    Some (List.fold_left (fun r s ->
        SMap.remove s r) m ss)

let rename_symbols () rn = function
  | None -> None
  | Some m ->
    Some (SMap.fold (fun k v r ->
        SMap.add (SETr_Rename.get rn k) v r) m SMap.empty)

let query () _t =
  {
    L.get_eqs = (fun () -> []);
    L.get_eqs_sym = (fun _s -> []);
  }

let combine () q t = t

let pp_debug () pp_sym ff t =
  L.pp pp_sym ff (serialize () t)

let pp_print = pp_debug
  

