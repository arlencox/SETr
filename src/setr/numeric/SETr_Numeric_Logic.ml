type 'sym e =
  | Add of 'sym e * 'sym e
  | Neg of 'sym e
  | Const of int
  | Mul of 'sym e * 'sym e
  | Var of 'sym

type uop =
  | UNeg

type bop =
  | BAdd
  | BMul

type cop =
  | CEq
  | CLe
  | CLt

type 'sym t =
  | Eq of 'sym e * 'sym e
  | Le of 'sym e * 'sym e
  | Lt of 'sym e * 'sym e
  | And of 'sym t * 'sym t
  | Not of 'sym t
  | True
  | False

type 'sym q = {
  get_eqs: unit -> ('sym * 'sym) list;
  get_eqs_sym: 'sym -> 'sym list;
  get_zeros: unit -> 'sym list;
}

let prec_e = function
  | Add _ -> 1
  | Neg _ -> 4
  | Const _ -> 10
  | Mul _ -> 2
  | Var _ -> 10

let rec pp_noparen_e ?parse:(parse=false) pp_sym ff t =
  let ppe = pp_e ~parse:parse ~prec:(prec_e t) pp_sym in
  match t with
  | Add(a,b) -> Format.fprintf ff "%a + %a" ppe a ppe b
  | Neg a -> Format.fprintf ff "-%a" ppe a
  | Const i -> Format.fprintf ff "%d" i
  | Mul(a,b) -> Format.fprintf ff "%a*%a" ppe a ppe b
  | Var e -> Format.fprintf ff "%a" pp_sym e
and pp_e ?parse:(parse=false) ?prec:(p=0) pp_sym ff t =
  if prec_e t < p then
    Format.fprintf ff "@[<hv 2>(%a)@]" (pp_noparen_e ~parse:parse pp_sym) t
  else
    pp_noparen_e ~parse:parse pp_sym ff t

let rec pp ?parse:(parse=false) pp_sym ff t =
  let ppe = pp_e ~parse:parse pp_sym in
  match t with
  | And (a,b) ->
    if parse then
      Format.fprintf ff "%a /\\ %a" (pp ~parse:parse pp_sym) a (pp ~parse:parse pp_sym) b
    else
      Format.fprintf ff "%a ∧ %a" (pp ~parse:parse pp_sym) a (pp ~parse:parse pp_sym) b
  | Not a -> Format.fprintf ff "(not %a)" (pp ~parse:parse pp_sym) a
  | Lt (a,b) ->
    Format.fprintf ff "%a < %a" ppe a ppe b
  | Le (a,b) ->
    if parse then
      Format.fprintf ff "%a <= %a" ppe a ppe b
    else
      Format.fprintf ff "%a ≤ %a" ppe a ppe b
  | Eq (a,b) ->
    if parse then
      Format.fprintf ff "%a == %a" ppe a ppe b
    else
      Format.fprintf ff "%a = %a" ppe a ppe b
  | True -> Format.fprintf ff "true"
  | False -> Format.fprintf ff "false"
