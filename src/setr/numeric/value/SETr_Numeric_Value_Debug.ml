module L = SETr_Numeric_Logic

module Make(D: SETr_Numeric_Value_Interface.S) : SETr_Numeric_Value_Interface.S = struct
  type cnst = D.cnst (** type of constants *)
  type uop = D.uop (** type for unary operations *)
  type bop = D.bop (** type for binary operations *)
  type cop = D.cop (** type for comparison operations *)
  type output = D.output (** type for serialized constraints *)
  type sexp = D.sexp(** type for serialized expressions *)

  type ctx = D.ctx (** the context (mutable state) for managing lattice elements *)
  type t = D.t  (** the type of the lattice elements *)

  (** [init ()] creates a new context *)
  let init () = D.init ()

  (** [top ctx] creates a new top element from a context [ctx]. *)
  let top ctx = D.top ctx

  (** [bottom ctx] creates a new bottom element from a context [ctx]. *)
  let bottom ctx = D.bottom ctx

  (** {2 Binary Domain Operations} *)

  let pp ctx ff t =
    Format.fprintf ff "(%a)"
      (D.pp_print ctx (fun ff () -> Format.fprintf ff "v")) t

  let ppu ff = function
    | L.UNeg -> Format.fprintf ff "neg"

  let ppb ff = function
    | L.BAdd -> Format.fprintf ff "add"
    | L.BMul -> Format.fprintf ff "mul"

  let ppc ff = function
    | L.CEq -> Format.fprintf ff "eq"
    | L.CLe -> Format.fprintf ff "le"
    | L.CLt -> Format.fprintf ff "lt"

  (** [join a b] overapproximates the abstraction [a] or the
      abstraction [b].  *)
  let join ctx a b =
    let res = D.join ctx a b in
    Format.printf "%a = join %a %a@." (pp ctx) res (pp ctx) a (pp ctx) b;
    res

  (** [widening a b] overapproximates the abstraction [a] or the
      abstraction [b], guaranteeing stabilization. *)
  let widening ctx a b =
    let res = D.widening ctx a b in
    Format.printf "%a = widening %a %a@." (pp ctx) res (pp ctx) a (pp ctx) b;
    res

  (** [meet a b] overapproximates the abstraction [a] and the
      abstraction [b]. *)
  let meet ctx a b = 
    let res = D.meet ctx a b in
    Format.printf "%a = meet %a %a@." (pp ctx) res (pp ctx) a (pp ctx) b;
    res

  (** [le a b] determines if [a] is contained in [b] ([a] is less than
      or equal to [b]). *)
  let le ctx a b = 
    let res = D.le ctx a b in
    Format.printf "%b = le %a %a@." res (pp ctx) a (pp ctx) b;
    res

  (** [is_bottom t] returns true if [t] is the bottom element *)
  let is_bottom ctx a = 
    let res = D.is_bottom ctx a in
    Format.printf "%b = is_bottom %a@." res (pp ctx) a;
    res

  (** [is_top t] returns true if [t] is the top element *)
  let is_top ctx a = 
    let res = D.is_top ctx a in
    Format.printf "%b = is_top %a@." res (pp ctx) a;
    res

  (** [const c v] returns a value domain element representing only the constant
      [v] *)

  let const ctx c =
    let res = D.const ctx c in
    Format.printf "%a = const@." (pp ctx) res;
    res

  (** [interval c l u] returns a value domain element representing values in
      the range [l] to [u] inclusive *)
  let interval ctx a b =
    let res = D.interval ctx a b in
    Format.printf "%a = interval@." (pp ctx) res;
    res

  (** [unary c o t] returns the domain element after applying unary operation
      [o] to the domain element [t] *)
  let unary ctx o t =
    let res = D.unary ctx o t in
    Format.printf "%a = unary %a %a@." (pp ctx) res ppu o (pp ctx) t;
    res

  (** [binary c o a b] returns the domain element after applying the binary
      operations [o] to the domain elements [a] and [b] *)
  let binary ctx o a b = 
    let res = D.binary ctx o a b in
    Format.printf "%a = binary %a %a %a@." (pp ctx) res ppb o (pp ctx) a (pp ctx) b;
    res

  (** [compare c o a b] returns new [a', b'] strengthened by having applied the
      comparison between [a] and [b].  A sound implementation would be
      returning [a, b] *)
  let compare ctx o a b =
    let (res0,res1) = D.compare ctx o a b in
    Format.printf "(%a,%a) = compare %a %a %a@." (pp ctx) res0 (pp ctx) res1 ppc o (pp ctx) a (pp ctx) b;
    (res0,res1)

  (** [bwd_unary c o r t] applies a backward operation [o] to [r].  Result is a
      modified version of [t].  A sound implementation would be returning [t] *)
  let bwd_unary ctx o r t =
    let res = D.bwd_unary ctx o r t in
    Format.printf "%a = bwd_unary %a %a %a@." (pp ctx) res ppu o (pp ctx) r (pp ctx) t;
    res

  (** [bwd_binary c o r a b] applies a backward operation [o] to [r].  Result
      is modified versions of [a] and [b].  A sound implementaiton would be
      returning [a,b] *)
  let bwd_binary ctx o r a b = 
    let (res0,res1) = D.bwd_binary ctx o r a b in
    Format.printf "(%a,%a) = bwd_unary %a %a %a %a@." (pp ctx) res0 (pp ctx) res1 ppb o (pp ctx) r (pp ctx) a (pp ctx) b;
    (res0,res1)

  (** [pp_print c s t] pretty prints the value element [t] associated with the
      symbol printed by [s] *)
  let pp_print ctx s t = D.pp_print ctx s t

  (** [pp_debug c s t] pretty prints the value element [t] associated with the
      symbol printed by [s] in debug mode *)
  let pp_debug ctx s t = D.pp_debug ctx s t

  (** [serialize c s t] returns the constraint that represents [t] on [s] *)
  let serialize ctx s t = D.serialize ctx s t
end


let _ =
  let open SETr_DomainRegistrar in
  let build = function
    | [NumericValue d] ->
        NumericValue (module Make((val d)))
    | _ -> build_error "Debug takes a numeric value domain as an argument"
  in
  let args = "(<val>)" in
  let help = "Enables debugging of value domains" in
  register "numeric.value.debug" build args help
