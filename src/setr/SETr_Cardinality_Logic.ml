module DS = SETr_DS
type 'sym e =
  | Empty
  | Universe
  | DisjUnion of 'sym e * 'sym e
  | Union of 'sym e * 'sym e
  | Inter of 'sym e * 'sym e
  | Diff of 'sym e * 'sym e
  | Comp of 'sym e
  | Var of 'sym

type 'sym n =
  | Add of 'sym n * 'sym n
  | Neg of 'sym n
  | Const of int
  | Mul of 'sym n * 'sym n
  | Card of 'sym e
  | NVar of 'sym

type 'sym t =
  | Eq of 'sym e * 'sym e
  | SubEq of 'sym e * 'sym e
  | And of 'sym t * 'sym t
  | Not of 'sym t
  | NLe of 'sym n * 'sym n
  | NLt of 'sym n * 'sym n
  | NEq of 'sym n * 'sym n
  | True
  | False
    
type 'sym q = {
  get_eqs: unit -> ('sym * 'sym) list;
  get_eqs_sym: 'sym -> 'sym list;
  get_zeros: unit -> 'sym list;
  (*get_representative: 'sym -> 'sym;*)
}

let prec_e = function
  | Union _ -> 60
  | Inter _ -> 70
  | Diff _ -> 80
  | DisjUnion _ -> 90
  | Comp _ -> 95
  | Var _
  | Empty
  | Universe -> 100

let rec pp_noparen_e ?parse:(parse=false) pp_sym ff t =
  let pprec = pp_e ~parse:(parse) ~prec:(prec_e t) pp_sym in
  match t with
  | Empty ->
    if parse then
      Format.fprintf ff "{}"
    else
      Format.fprintf ff "∅"
  | Universe ->
    if parse then
      Format.fprintf ff "~{}"
    else
      Format.fprintf ff "𝕌"
  | DisjUnion (a,b) ->
    if parse then
      Format.fprintf ff "%a U+ %a" pprec a pprec b
    else
      Format.fprintf ff "%a ⊎ %a" pprec a pprec b
  | Union (a,b) ->
    if parse then
      Format.fprintf ff "%a U %a" pprec a pprec b
    else
      Format.fprintf ff "%a ∪ %a" pprec a pprec b
  | Inter (a,b) ->
    if parse then
      Format.fprintf ff "%a ^ %a" pprec a pprec b
    else
      Format.fprintf ff "%a ∩ %a" pprec a pprec b
  | Diff (a,b) ->
    if parse then
      Format.fprintf ff "%a \\ %a" pprec a pprec b
    else
      Format.fprintf ff "%a ∖ %a" pprec a pprec b
  | Comp a -> Format.fprintf ff "~%a" pprec a
  | Var v -> pp_sym ff v
and pp_e ?parse:(parse=false) ?prec:(p=0) pp_sym ff t =
  if prec_e t < p then
    Format.fprintf ff "@[<hv 2>(%a)@]" (pp_noparen_e ~parse:parse pp_sym) t
  else
    pp_noparen_e ~parse:parse pp_sym ff t

let prec_n = function
  | Add _ -> 1
  | Neg _ -> 4
  | Const _ -> 10
  | Mul _ -> 2
  | Card _ -> 10
  | NVar _ -> 10

let rec pp_noparen_n ?parse:(parse=false) pp_sym ff t =
  let ppn = pp_n ~parse:parse ~prec:(prec_n t) pp_sym in
  let ppe = pp_e ~parse:parse pp_sym in
  match t with
  | Add(a,b) -> Format.fprintf ff "%a + %a" ppn a ppn b
  | Neg a -> Format.fprintf ff "-%a" ppn a
  | Const i -> Format.fprintf ff "%d" i
  | Mul(a,b) -> Format.fprintf ff "%a*%a" ppn a ppn b
  | Card e -> Format.fprintf ff "|%a|" ppe e
  | NVar s -> Format.fprintf ff "%a" pp_sym s
and pp_n ?parse:(parse=false) ?prec:(p=0) pp_sym ff t =
  if prec_n t < p then
    Format.fprintf ff "@[<hv 2>(%a)@]" (pp_noparen_n ~parse:parse pp_sym) t
  else
    pp_noparen_n ~parse:parse pp_sym ff t


let rec pp ?parse:(parse=false) pp_sym ff t =
  let ppe = pp_e ~parse:parse pp_sym in
  let ppn = pp_n ~parse:parse pp_sym in
  match t with
  | Eq (a,b) -> Format.fprintf ff "%a = %a" ppe a ppe b
  | SubEq (a,b) ->
    if parse then
      Format.fprintf ff "%a <= %a" ppe a ppe b
    else
      Format.fprintf ff "%a ⊆ %a" ppe a ppe b
  | And (a,b) ->
    if parse then
      Format.fprintf ff "%a /\\ %a" (pp ~parse:parse pp_sym) a (pp ~parse:parse pp_sym) b
    else
      Format.fprintf ff "%a ∧ %a" (pp ~parse:parse pp_sym) a (pp ~parse:parse pp_sym) b
  | Not a -> Format.fprintf ff "(not %a)" (pp ~parse:parse pp_sym) a
  | NLt (a,b) ->
    Format.fprintf ff "%a < %a" ppn a ppn b
  | NLe (a,b) ->
    if parse then
      Format.fprintf ff "%a <= %a" ppn a ppn b
    else
      Format.fprintf ff "%a ≤ %a" ppn a ppn b
  | NEq (a,b) ->
    if parse then
      Format.fprintf ff "%a == %a" ppn a ppn b
    else
      Format.fprintf ff "%a = %a" ppn a ppn b
  | True -> Format.fprintf ff "true"
  | False -> Format.fprintf ff "false"

