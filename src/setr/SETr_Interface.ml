(** [Lattice] is a basic domain lattice.  It supports all of the basic lattice
    operations. *)
module type Lattice = sig
  type ctx (** the context (mutable state) for managing lattice elements *)
  type t   (** the type of the lattice elements *)

  (** [init ()] creates a new context *)
  val init : unit -> ctx

  (** [top ctx] creates a new top element from a context [ctx]. *)
  val top : ctx -> t

  (** [bottom ctx] creates a new bottom element from a context [ctx]. *)
  val bottom : ctx -> t

  (** {2 Binary Domain Operations} *)

  (** [join a b] overapproximates the abstraction [a] or the
      abstraction [b].  *)
  val join : ctx -> t -> t -> t

  (** [widening a b] overapproximates the abstraction [a] or the
      abstraction [b], guaranteeing stabilization. *)
  val widening : ctx -> t -> t -> t

  (** [meet a b] overapproximates the abstraction [a] and the
      abstraction [b]. *)
  val meet : ctx -> t -> t -> t

  (** [le a b] determines if [a] is contained in [b] ([a] is less than
      or equal to [b]). *)
  val le : ctx -> t -> t -> bool

  (** [is_bottom t] returns true if [t] is the bottom element *)
  val is_bottom: ctx -> t -> bool

  (** [is_top t] returns true if [t] is the top element *)
  val is_top: ctx -> t -> bool

end


(** [Domain] is a basic domain description.  It supports all basic domain
    operations including join, widening, inclusion check *)
module type Domain = sig
  include Lattice
  type sym    (** the type of symbols used in the domain *)
  type cnstr  (** the type of constraints used in the domain *)
  type output (** the type returned when serializing the abstract state *)
  type query  (** the type of query and constrain commands suitable for this domain *)

  (** [symbols t] returns the symbols constrained in [t] *)
  val symbols : ctx -> t -> sym list

  (** {2 Constraint Interfaces} *)

  (** [constrain ctx c t] abstracts constraint [c] and computes meet of the result
      with [t] *)
  val constrain : ctx -> cnstr -> t -> t

  (** [serialize ctx t] returns the constraint that represents [t] *)
  val serialize : ctx -> t -> output

  (** [sat ctx t c] checks if the abstract state [t] satisfies the constraints [c].
      If [c] is unrepresentable by the domain this will always be false.
  *)
  val sat : ctx -> t -> cnstr -> bool

  (** {2 Symbol Manipulation} *)

  (** [forget ctx syms t] forgets meaning of (projects out) the symbols [syms] in
      the domain [t]. *)
  val forget : ctx -> sym list -> t -> t

  (** [rename_symbols ctx f t] renames each symbol in [t] to the corresponding
      symbol returned by the rename structure [f].  Relationships are
      maintained, assuming the mapping does not reuse any symbols *)
  val rename_symbols : ctx -> sym SETr_Rename.t -> t -> t

  (** {2 Query interface} *)

  (** [query ctx t] returns a query datatype that is bound to the abstract state
      [t] *)
  val query: ctx -> t -> query

  (** [combine ctx q t] use a query from another abstract state (in a different
      domain) to strengthen this domain *)
  val combine: ctx -> query -> t -> t

  (** [pp_debug ctx s f t] pretty print the domain's internal structure using [s]
      to format symbols to the formatter [f] *)
  val pp_debug: ctx -> (Format.formatter -> sym -> unit) -> Format.formatter -> t -> unit

  (** [pp_print ctx s f t] pretty print the abstract state [s] to format symbols to
      the formatter [f] *)
  val pp_print: ctx -> (Format.formatter -> sym -> unit) -> Format.formatter -> t -> unit

end

module type DomainRenamable = sig
  include Domain

  (** [join ctx map a b] overapproximates the abstraction [a] or the
      abstraction [b].  The [map] is used to map symbols in [a] to symbols in
      [b] and the result.

      Each element in mapping [(syma, symb, symc)] cause [syma] to be matched
      with [symb] producing [symc].  The same symbol should not occur more
      than once. *)
  val join_map : ctx -> (sym * sym * sym) list -> t -> t -> t

  (** [widening ctx map a b] overapproximates the abstraction [a] or the
      abstraction [b], guaranteeing stabilization..  The [map] is used to map
      symbols in [a] to symbols in [b] and the result *)
  val widening_map : ctx -> (sym * sym * sym) list -> t -> t -> t

  (** [meet ctx map a b] overapproximates the abstraction [a] and the
      abstraction [b].  The [map] is used to map symbols in [a] to symbols in
      [b] and the result *)
  val meet_map : ctx -> (sym * sym * sym) list -> t -> t -> t

  (** [le ctx map a b] determines if [a] is contained in [b] ([a] is less
      than or equal to [b]) under mapping [map]. *)
  val le_map : ctx -> (sym * sym) list -> t -> t -> bool
end

(** [Sym] describes a modules for representing symbols.  Typical candidates
    would be integers and strings. *)
module type Sym = sig
  type t

  (** [compare a b] creates a total ordering on symbols.  Should return a
      negative number if [a] < [b], a positive number if [a] > [b], and 0 if
      [a] = [b] *)
  val compare: t -> t -> int

  (** [to_string t] converts a symbol to a string *)
  val to_string: t -> string

  (** [hash t] returns a hash code for a symbol, to be used in hash combinators
    or hash functions *)
  val hash: t -> int

  (** [fresh ()] returns a fresh symbol that is guaranteed to not conflict with
      any user-specified symbol or previously created fresh variable *)
  val fresh: unit -> t
end


module type Value = sig
  include Lattice

  type cnst (** type of constants *)
  type uop (** type for unary operations *)
  type bop (** type for binary operations *)
  type cop (** type for comparison operations *)
  type output (** type for serialized constraints *)
  type sexp (** type for serialized expressions *)

  (** [const c v] returns a value domain element representing only the constant
      [v] *)

  val const : ctx -> cnst -> t

  (** [interval c l u] returns a value domain element representing values in
      the range [l] to [u] inclusive *)
  val interval : ctx -> cnst -> cnst -> t

  (** [unary c o t] returns the domain element after applying unary operation
      [o] to the domain element [t] *)
  val unary: ctx -> uop -> t -> t

  (** [binary c o a b] returns the domain element after applying the binary
      operations [o] to the domain elements [a] and [b] *)
  val binary: ctx -> bop -> t -> t -> t

  (** [compare c o a b] returns new [a', b'] strengthened by having applied the
      comparison between [a] and [b].  A sound implementation would be
      returning [a, b] *)
  val compare : ctx -> cop -> t -> t -> t * t

  (** [bwd_unary c o r t] applies a backward operation [o] to [r].  Result is a
      modified version of [t].  A sound implementation would be returning [t] *)
  val bwd_unary : ctx -> uop -> t -> t -> t

  (** [bwd_binary c o r a b] applies a backward operation [o] to [r].  Result
      is modified versions of [a] and [b].  A sound implementaiton would be
      returning [a,b] *)
  val bwd_binary : ctx -> bop -> t -> t -> t -> t * t

  (** [pp_print c s t] pretty prints the value element [t] associated with the
      symbol printed by [s] *)
  val pp_print : ctx -> (Format.formatter -> unit -> unit) -> Format.formatter -> t -> unit

  (** [pp_debug c s t] pretty prints the value element [t] associated with the
      symbol printed by [s] in debug mode *)
  val pp_debug : ctx -> (Format.formatter -> unit -> unit) -> Format.formatter -> t -> unit

  (** [serialize c s t] returns the constraint that represents [t] on [s] *)
  val serialize : ctx -> sexp -> t -> output
  
end
