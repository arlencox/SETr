module L = SETr_Numeric_Logic

type ctx = unit
type cnst = int
type uop = L.uop
type bop = L.bop
type cop = L.cop
type output = int L.t
type sexp = int L.e

let init () = ()

type bound = 
  | Int of Z.t
  | PInf
  | NInf

type t =
  | Itv of bound * bound
  | Bottom

let lift1 f x =
  match x with
  | Itv (a,b) -> f a b
  | Bottom -> Bottom

let lift2 f x y =
  match x, y with
  | Itv (a,b), Itv (c,d) -> f a b c d
  | Bottom, _ | _, Bottom -> Bottom

let bound_sign = function
  | Int i -> Z.sign i
  | PInf -> 1
  | NInf -> -1

let bound_neg = function
  | Int i -> Int (Z.neg i)
  | PInf -> NInf
  | NInf -> PInf

let bound_pred = function
  | Int i -> Int (Z.pred i)
  | PInf -> PInf
  | NInf -> NInf

let bound_succ = function
  | Int i -> Int (Z.succ i)
  | PInf -> PInf
  | NInf -> NInf

let bound_add a b =
  match a, b with
  | Int i, Int j -> Int (Z.add i j)
  | NInf, PInf | PInf, NInf -> invalid_arg "bound_add"
  | NInf, _ | _, NInf -> NInf
  | PInf, _ | _, PInf -> PInf

let bound_sub a b =
  bound_add a (bound_neg b)

let bound_mul a b =
  match a, b with
  | Int i, Int j -> Int (Z.mul i j)
  | _ ->
    let sgn = bound_sign a * bound_sign b in
    if sgn = 0 then
      Int Z.zero
    else if sgn > 0 then
      PInf
    else
      NInf

let bound_div a b = match a,b with
  | Int i, Int j -> Int (Z.div i j)
  | (NInf|PInf), (PInf|NInf) -> invalid_arg "bound_div" (* oo/oo *)
  | _,NInf | _,PInf -> Int Z.zero
  | _,Int z when z = Z.zero ->
    let res = bound_sign a in
    if res > 0 then PInf
    else if res < 0 then NInf
    else invalid_arg "bound_div" (* 0/0 *)
  | _ ->
    let res = bound_sign a * bound_sign b in
    if res > 0 then PInf
    else if res < 0 then NInf
    else Int Z.zero

let bound_cmp a b =
  match a, b with
  | Int i, Int j -> Z.compare i j
  | NInf, NInf | PInf, PInf -> 0
  | NInf, _ | _, PInf -> -1
  | PInf, _ | _, NInf -> 1

let bound_min a b =
  if bound_cmp a b <= 0 then a else b

let bound_max a b =
  if bound_cmp a b >= 0 then a else b

let top () = Itv (NInf, PInf)

let bottom () = Bottom

let const () i =
  let v = Int (Z.of_int i) in
  Itv (v, v)

let neg x =
  lift1 (fun a b -> Itv (bound_neg b, bound_neg a)) x

let add x y =
  lift2 (fun a b c d -> Itv (bound_add a c, bound_add b d)) x y

let sub x y =
  add x (neg y)

let mul x y =
  lift2 (fun a b c d ->
      let ac,ad = bound_mul a c, bound_mul a d in
      let bc,bd = bound_mul b c, bound_mul b d in
      Itv (bound_min (bound_min ac ad) (bound_min bc bd),
           bound_max (bound_max ac ad) (bound_max bc bd))
    ) x y

let div x y =
  lift2 (fun a b c d ->
      if bound_sign a >= 0 && bound_sign c > 0 then
        Itv (bound_div a d, bound_div b c)
      else top ()
    ) x y

let join () x y =
  match x, y with
  | Bottom, o | o, Bottom -> o
  | Itv (a,b), Itv(c,d) -> Itv (bound_min a c, bound_max b d)

let widening () x y =
  match x,y with
  | Bottom,o | o,Bottom -> o
  | Itv (a,b), Itv (c,d) ->
    Itv((if bound_cmp a c <= 0 then a else NInf),
        (if bound_cmp b d >= 0 then b else PInf))

let meet () x y =
  lift2
    (fun a b c d ->
       let ac = bound_max a c in
       let bd = bound_min b d in
       if bound_cmp ac bd <= 0 then Itv (ac, bd) else Bottom
    ) x y

let le () x y =
  match x, y with
  | Bottom, _ -> true
  | _, Bottom -> false
  | Itv (a,b), Itv (c,d) ->
    bound_cmp a c >= 0 && bound_cmp b d <= 0

let is_bottom () = function
  | Bottom -> true
  | Itv _ -> false

let is_top () = function
  | Bottom -> false
  | Itv (NInf,PInf) -> true
  | Itv _ -> false

let interval () l u =
  Itv (Int (Z.of_int l), Int (Z.of_int u))

let unary () op a =
  match op with
  | L.UNeg -> neg a

let binary () op a b =
  match op with
  | L.BAdd -> add a b
  | L.BMul -> mul a b

let eq x y =
  let res = meet () x y in
  res, res

let ge (x:t) (y:t) : t * t =
  match x, y with
  | Bottom, _ | _, Bottom -> Bottom, Bottom
  | Itv (a, b), Itv (c, d) ->
    let a = bound_max a c in
    let d = bound_min b d in
    if bound_cmp a b > 0 || bound_cmp c d > 0 then
      Bottom, Bottom
    else
      Itv (a, b), Itv (c, d)

let cle x y = ge y x

let gt x y =
  match x, y with
  | Bottom, _ | _, Bottom -> Bottom, Bottom
  | Itv (a, b), Itv (c, d) ->
    let a = bound_max a (bound_succ c) in
    let d = bound_min (bound_pred b) d in
    if bound_cmp a b > 0 || bound_cmp c d > 0 then
      Bottom, Bottom
    else
      Itv (a, b), Itv (c, d)

let lt x y = gt y x

let compare () op (a:t) (b:t) : t * t =
  match op with
  | L.CEq -> eq a b
  | L.CLe -> cle a b
  | L.CLt -> lt a b
  
let bwd_unary () op r a =
  match op with
  | L.UNeg -> meet () (neg r) a

let bwd_binary () op r a b =
  match op with
  | L.BAdd ->
    meet () a (sub r b), meet () b (sub r a)
  | L.BMul ->
    let contains_zero o = le () (const () 0) o in
    (if contains_zero b && contains_zero r then a else meet () a (div r b)),
    (if contains_zero a && contains_zero r then b else meet () b (div r a))


let serialize () s = function
  | Bottom -> L.False
  | Itv (a, b) ->
    let r1 = match a with
      | NInf -> None
      | PInf -> invalid_arg "serialize"
      | Int i -> Some (L.Le (L.Const (Z.to_int i), s))
    in
    let r2 = match b with
      | NInf -> invalid_arg "serialize"
      | PInf -> None
      | Int i -> Some (L.Le (s, L.Const (Z.to_int i)))
    in
    match r1, r2 with
    | None, None -> L.True
    | Some o, None | None, Some o -> o
    | Some a, Some b -> L.And (a, b)

let pp_print () pp_sym ff = function
  | Bottom -> Format.fprintf ff "bottom"
  | Itv (a, b) ->
    let r1 = match a with
      | NInf -> None
      | PInf -> invalid_arg "serialize"
      | Int i -> Some (Z.to_string i)
    in
    let r2 = match b with
      | NInf -> invalid_arg "serialize"
      | PInf -> None
      | Int i -> Some (Z.to_string i)
    in
    match r1, r2 with
    | None, None -> Format.fprintf ff "%a = top" pp_sym ()
    | Some a, None -> Format.fprintf ff "%s <= %a" a pp_sym ()
    | None, Some b -> Format.fprintf ff "%a <= %s" pp_sym () b
    | Some a, Some b -> Format.fprintf ff "%s <= %a <= %s" a pp_sym () b

let pp_debug = pp_print

