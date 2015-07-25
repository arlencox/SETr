# SETr Sets Abstract Domain Library

SETr is an interface for set abstractions.  It defines common infrastructure for abstracting set constraints for use in an abstract interpreter.  Built upon this interface, it provides a number of included abstractions usable as libraries.  These included abstractions are tuned for high-performance, symbolic (no known constants), relational (constraints over multiple variables) constraints.

For example, the *lin* abstract domain is tuned to efficiently handle constraints of the form:

> (W = {a} ⊎ Y ⊎ Z) ∧ (Z = A ⊎ B)

While there is no information about exactly what is in these sets, the *lin* abstract domain keeps track of the fact that the contents of W is the contents of Y and Z along with some element a, all of which are disjoint.  It can then track the combination of an arbitrary number of these kind of constraints.

Generally, SETr supports two logics for constraints.  Within these logics, different abstract domains more or less precisely represent constraints.  The first logic is the *Symbolic* logic, which solely supports set symbols.  The second logic is the *SymSing* logic, which supports set symbols in addition to singleton symbols.  The definition of these logics is the following:

*Symbolic*

```
E ::= {}     -- empty set
    | X      -- set symbols
    | ~E     -- complement
    | E ∪ E  -- union
    | E ∩ E  -- intersection
    | E ⊎ E  -- disjoint union
    | E \ E  -- set minus (relative complement)
    
S ::= True   -- true (top)
    | False  -- false (bottom)
    | S ∧ S  -- and/conjunction
    | E = E  -- set equality
    | E ⊆ E  -- non-strict subset
```

*SymSing*

```
E ::= {}     -- empty set
    | {a}    -- singleton set symbol
    | X      -- set symbols
    | ~E     -- complement
    | E ∪ E  -- union
    | E ∩ E  -- intersection
    | E ⊎ E  -- disjoint union
    | E \ E  -- set minus (relative complement)
    
S ::= True   -- true (top)
    | False  -- false (bottom)
    | S ∧ S  -- and/conjunction
    | E = E  -- set equality
    | E ⊆ E  -- non-strict subset
    | a ∈ E  -- element of/in
```

If a problem can be expressed within these logics, the SETr abstract domain library may solve the problem or will at least serve as a starting point for solving the problem.

## Quick Start

SETr is an [OCaml](https://ocaml.org) library intended to be used with `opam` and `ocamlfind`.  When used as such, enabling the library is simply a matter of adding `package(setr)` to your `_tags` file or adding `-package(setr)` to your `ocamlbuild` invocation.  The use of the library can be through two possible interfaces:

Set domain construction language:

```ocaml
let d = match SETr.get "lin" with
  | SETr.SymSing d -> d
  | _ -> failwith "Returned a set domain that is incompatible with SymSing"
in
let module D = (val d) in
let module L = SETr.SymSing.Logic in
```

Direct module access:

```ocaml
module D = SETr.SymSing.Lin
module L = SETr.SymSing.Logic
```

The choice of interface will be dictated by the application.  If a single domain suffices, there is no need to use the set domain construction language.  The module can be directly hard coded.  If the intent is to allow selection of the set domain based on a configuration file.  It may make sense to read a set construction language string from the configuration file.

Once a domain has been constructed, it can be used by first creating a context, which is responsible for any stateful domain information.  The extent to which this context is used depends on the specific domain that is instantiated.

```ocaml
let ctx = D.init () in
```

Now, domain operations can be performed.  Often this involves starting with no constraints:

```ocaml
let t0 = D.top ctx in
```

Now constraints can be added to that `t0` through the specific and compatible `Logic`:

```ocaml
let x,y,z = (0,1,2) in
let t1 = D.constrain ctx L.(Eq(Var(x),Var(y))) t0 in
let t2 = D.constrain ctx L.(Eq(Var(z),Var(y))) t1 in
```

This adds an appropriate overapproximation of the constraints `X = Y` and `Z = Y` to the abstract state `t0`, which results in new abstract stats `t1` and `t2`.  We can now query the abstract state `t2` to see if it can prove that `X = Z`, a constraint that was never directly written but is implied by the constraints we provided:

```ocaml
let r = D.sat ctx t2 L.(Eq(Var(X),Var(Z))) in
```

The result `r` is `true` if the proof succeeded, otherwise it is `false`. Other normal abstract domain operations are provided through the interface and are documented below.

## Interface

```ocaml
module type Domain = sig
  type ctx    (** the context (mutable state) for managing the domain *)
  type t      (** the type of the domain itself *)
  type sym    (** the type of symbols used in the domain *)
  type cnstr  (** the type of constraints used in the domain *)
  type output (** the type returned when serializing the abstract state *)
  type query  (** the type of query and constrain commands suitable for this domain *)

  (** [init ()] creates a new context *)
  val init : unit -> ctx

  (** [top ctx] creates a new top element from a context [ctx]. *)
  val top : ctx -> t

  (** [bottom ctx] creates a new bottom element from a context [ctx]. *)
  val bottom : ctx -> t
          
  (** [symbols t] returns the symbols constrained in [t] *)
  val symbols : ctx -> t -> sym list
         
  (** {2 Constraint Interfaces} *)

  (** [constrain c t] abstracts constraint [c] and computes meet of the result
      with [t] *)
  val constrain : ctx -> cnstr -> t -> t
    
  (** [serialize t] returns the constraint that represents [t] *)
  val serialize : ctx -> t -> output
    
  (** [sat t c] checks if the abstract state [t] satisfies the constraints [c].
      If [c] is unrepresentable by the domain this will always be false.
  *)
  val sat : ctx -> t -> cnstr -> bool

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

  (** {2 Symbol Manipulation} *)

  (** [forget syms t] forgets meaning of (projects out) the symbols [syms] in
      the domain [t]. *)
  val forget : ctx -> sym list -> t -> t

  (** [rename_symbols f t] renames each symbol in [t] to the corresponding
      symbol returned by the rename structure [f].  Relationships are
      maintained, assuming the mapping does not reuse any symbols *)
  val rename_symbols : ctx -> sym SETr_Rename.t -> t -> t

  (** {2 Query interface} *)

  (** [query t] returns a query datatype that is bound to the abstract state
      [t] *)
  val query: ctx -> t -> query

  (** [combine q t] use a query from another abstract state (in a different
      domain) to strengthen this domain *)
  val combine: ctx -> query -> t -> t

  (** [pp_debug s f t] pretty print the domain's internal structure using [s]
      to format symbols to the formatter [f] *)
  val pp_debug: ctx -> (Format.formatter -> sym -> unit) -> Format.formatter -> t -> unit

  (** [pp_print s f t] pretty print the abstract state [s] to format symbols to
      the formatter [f] *)
  val pp_print: ctx -> (Format.formatter -> sym -> unit) -> Format.formatter -> t -> unit
```

## Symbolic Abstract Domains

SETr provides a variety of abstract domains that implement the symbolic logic.  Here, each of the domains is described.  For each domain, the code to access that domain using direct module access is given and the comment contains the domain construction language equivalent.

All of the symbolic abstract domains leverage the fact that they are a Boolean algebra.  By replacing union with disjunction, intersection with conjunction, complement with negation, subset with implication, and equality with equivalence, any that can represent Boolean algebras, can represent sets.  For example, consider the following constraints:

> (W ⊆ X ∪ Y) ∧ (Z ∩ W = X)

These translate into the following Boolean algebra constraints:

> (W → X ∨ Y) ∧ (Z ∧ W ↔︎ X)

Which if simplified, becomes the following conjunctive normal form, using only the common *not* (¬), *and* (∧), and *or* (∨) operators:

> (¬W ∨ X ∨ Y) ∧ (¬Z ∨ ¬W ∨ X) ∧ (Z ∨ W ∨ ¬X)

From this, the challenge of representing constraints is one of efficiency.  Operations on this class of constraints are often in NP (3SAT).  Abstraction can avoid this cost through a combination of optimizing for common constraints, dropping constraints, and getting lucky with clever data structures.

### Binary Decision Diagrams

```ocaml
module D = SETr.Symbolic.BDD.Default (* bdd *)
module D = SETr.Symbolic.BDD.Opt     (* bdd-opt *)
module D = SETr.Symbolic.BDD.Cudd.Make(struct let reorder = None end)
                                     (* bdd-cudd *)
module D = SETr.Symbolic.BDD.MLBDD   (* bdd-mlbdd *)
module D = SETr.Symbolic.BDD.XBDD    (* bdd-xbdd *)
```

Binary decision diagrams (BDDs) are data structures for representing a Boolean algebra functions.  Each BDD is a directed acyclic graph.  Leaf nodes can be either *True* or *False* and non-leaf nodes are all *If-Then-Else* nodes that have the following behavior:

```
if X then N1 else N2
```

Where `N1` and `N2` are references to other nodes.  BDDs restrict that `N1` and `N2` must be different.  If they are the same, there is no point in this node at all and each reference to it could be replaced with `N1`.  It might be expensive to check if `N1` and `N2` were different, except that BDDs can easily be made canonical -- structural hashing can be used so that `N1` and `N2` can be referentially compared for equality.  To get this canonicity, a variable ordering is forced.  A total order over the variables is created and then the restriction is introduced that `X` must be less than any variable in `N1` or `N2`.

For example, the following constraints can be converted into a BDD:

> (¬W ∨ X ∨ Y) ∧ (¬Z ∨ ¬W ∨ X) ∧ (Z ∨ W ∨ ¬X)

First, a variable ordering is chosen: W ≺ X ≺ Y ≺ Z.  Then the BDD can be constructed by computing cofactors.  That is the formulas under the assumption that some variable is either true or false:

If W is false, the formula becomes:
> ¬W → Z ∨ ¬X

If W is true, the formula becomes:
> W → (X ∨ Y) ∧ (¬Z ∨ X)

This is represented by the graph node, where [] are used to represent future graph computations:

![Partial BDD construction](doc/img/partial-bdd.png)

Next we compute the cofactors of Z ∨ ¬X for X:

If X is false, the formula is True.

If X is true, the formula becomes:
> X → Z

Similarly, we compute the cofactors of (X ∨ Y) ∧ (¬Z ∨ X) for X:
I
If X is false, the formula becomes:
> ¬X → Y ∧ ¬Z

If X is true, the formula is True.

This process is continued until the following graph results.

![Full BDD construction](doc/img/full-bdd.png)

Notice that there is not a node for every single variable in every single branch.  This is, in addition to sharing, is the advantage of binary decision diagrams.  They are canonical and efficient to manipulate and they are highly compressed.

All of the BDD implementations provide complete precision for the symbolic logic.  Every operation is fully precise.  Therefore this can be used as a sound and complete decision procedure, though it is not optimized to be used as such.

There are a variety of implementations of BDDs provided that offer different performance tradeoffs.  It is always possible to use the default BDD implementation, but others may optionally be available depending on which libraries were installed.



#### Cudd

If the [mlcuddidl](http://pop-art.inrialpes.fr/people/bjeannet/mlxxxidl-forge/mlcuddidl/index.html) is installed, Cudd becomes available.  This is a powerful C library that offers many options.  As a result of these options, the interface is a functor rather than a domain directly.

```ocaml
module D = SETr.Symbolic.BDD.Cudd.Make(struct let reorder = None end)
```

The `reorder` option can be set to one of Cudd's reordering modes, which will enable automatic reordering.  In this case, when BDDs pass a certain threshold in size, a reordering is triggered, causing Cudd to search for a different ordering on the variables that is more compact.  The algorithms it can use are listed below (without explanation, see [Cudd](http://vlsi.colorado.edu/~fabio/CUDD/) for more information).

|Reordering Method         |
|--------------------------|
| `Some "same"`            |
| `Some "none"`            |
| `Some "random"`          |
| `Some "random_pivot"`    |
| `Some "sift"`            |
| `Some "sift_converge"`   |
| `Some "symm_sift"`       |
| `Some "symm_sift_conv"`  |
| `Some "window2"`         |
| `Some "window3"`         |
| `Some "window4"`         |
| `Some "window2_conv"`    |
| `Some "window3_conv"`    |
| `Some "window4_conv"`    |
| `Some "group_sift"`      |
| `Some "group_sift_conv"` |
| `Some "annealing"`       |
| `Some "genetic"`         |
| `Some "linear"`          |
| `Some "linear_converge"` |
| `Some "lazy_sift"`       |
| `Some "exact"`           |

Due to its years of tuning and careful implementation, this is likely the fastest option.  As such, it is selected as default when it is available on the system.  However, due to its use of native code, it is much more difficult to install than native code options and thus it may not be available.

Note that Cudd is not tolerant of large variable identifiers.  As a result, it may be necessary to use the [`Remap` functor](#remap), which will renumber large identifiers to the lowest unused identifier.  However, if there is simply a wide range of used identifiers, Cudd may not be an optimal choice.

#### MLBDD

The mlbdd library is an OCaml-native BDD implementation that uses data structures borrowed from Cudd to be more efficient than other OCaml-native BDDs.  It can be installed via `opam install mlbdd`.  Its hashing strategy uses a custom weak hash table to allow OCaml to garbage collect unused BDD nodes.  This also is tolerant of large numbers of identifiers and thus is a good choice in this situation.

When it is available, it is the second choice for default after Cudd.

#### XBDD

The xbdd implementation is borrowed from Xavier Leroy's experimental [BDD implementation](https://gforge.inria.fr/scm/viewvc.php/sodiac/attic/xlsat/).  It is reasonably fast, but it does not support garbage collection.  It is the last choice and the default only if no other libraries are installed.

#### Optimized

The `Opt` BDD is whichever BDD implementation was found to be experimentally optimal.  This includes the use of abstract domain modifiers to improve performance.  For example, this incorporates `Remap` into the `Cudd` to avoid performance issues with Cudd.

### Satisfiability Modulo Theories

```ocaml
module D = SETr.Symbolic.SMT (* smt *)
```

Satisfiability modulo theories (SMT) is the satisfiability decision procedure augmented with theories.  This manipulates Boolean formulas just like BDDs do except that it does not maintain a normal form.  When a query is made, the formulas are processed and simplified to attempt to construct a proof.

For example, the following domain operations result in the shown formulas:

| Command | Result |
|---------|--------|
| top     | true   |
| constrain (X = Y)  | X ↔︎ Y ∧ true |
| constrain (Z = Y)  | Z ↔︎ Y ∧ X ↔︎ Y ∧ true |

Note that there is no simplification going on or anything other than recording the constraints as they occur.  Recurring syntactic expressions are automatically shared by the SMT solver, however.  As a result, when a sat query is made, such as does this imply X = Z, the SMT solver must do whatever is required to prove this.

In this case, the SMT solver might learn the clauses X → Z and Z → X, which allow the proof to complete.  However, the SMT solver must decide if it wants to keep that information that it learned.  In many cases it will throw away that learned information when use of the domain proceeds.

The needs so far do not extend beyond SAT.  The reason SMT is required is the use of quantification.  Specifically, the forget operation generates quantifiers:

| Command | Result |
|---------|--------|
| forget Y | ∃Y. Z ↔︎ Y ∧ X ↔︎ Y ∧ true |

These quantifiers place significant load on the SMT solver.  Therefore, this abstraction is best suited to uses that do not often need to forget identifiers.  Additionally, because the accumulated formulas continually grow in size, which causes the complexity to continuously grow as use continues.

Like the BDDs, this is a sound and complete abstract domain.  All of the logic is precisely handled.


### Quantified Union/Intersection Constraint Graphs

```ocaml
module D = SETr.Symbolic.QUICG (* quicg *)
```

Quantified Union/Intersection Constraint Graphs (QUIC Graphs) are a graph-based representation of the underlying Boolean algebra for sets.  They build on conjunctive normal form constraints, where each constraint is of the form:

> X ∧ Y → W ∨ Z

Which is equivalent to the conjunctive normal form:

> ¬X ∨ ¬Y ∨ W ∨ Z

Each of these gets translated into an edge in a directed hypergraph:

![QUIC edge](doc/img/quic-edge.png)

The resulting data structure was designed for two purposes:

1. To be highly optimized for forget operations.
2. To be efficient for propagating information about nodes along the edges.  This is primarily used for handling constraints about constants.

A forget operation does pairwise merges of edges.  Each incoming edge to the node to be forgotten is paired with each outgoing edge to the node to be forgotten.

For example, if we were going to forget the variable W in the following graph, we would pair up these two edges.

![forgetting in a QUIC graph](doc/img/quic-forget.png)

The result merges the two edges in such a way that the variable W is eliminated:

![result of forgetting in a QUIC graph](doc/img/quic-forget-result.png)

This process corresponds to resolution between two conjunctive normal form clauses.

QUIC graphs differs from the previous abstractions in that it is not guaranteed to be precise.  It uses pattern matching heuristics to define join and widening operations.  These pattern matches commonly occurring logical facts and preserve them through joins.  This can result in losing unidentified patterns and can be slow depending on the number of patterns.

## Symbolic with Singletons Abstract Domains

The abstract domains that combine symbolic reasoning with singletons must involve more than simply the Boolean algebras.  They must also keep track of singleton information.  This information can be used to  increase precision of the analysis by ensuring that:

> {a} ⊈ ∅

This can add significant complexity to an abstract domain as it becomes much less well structured.

### Linear Set Constraints

```ocaml
module D = SETr.SymSing.Lin (* lin *)
```

The linear set constraints domain keeps track of linear relationships between sets.  Primarily it keeps track of constraints of the form:

> X = {a} ⊎ {b} ⊎ Y ⊎ Z

These constraints equate a variable such as X to a disjoint union of a number of singleton and non-singleton set symbols. Additionally, it keeps track of subset constraints between variables of the form:

> X ⊆ W

Other constraint forms are disallowed.  This abstracts significantly more than the above abstraction in that large classes of constraints are unsupported.  However, if the needed constraints are of the above form, this provides an efficient solution.



## Abstract Domain Modifiers

In addition to abstract domains, SETr provides abstract domain modifiers that consume abstract domains and produce abstract domains.  Some of them are created for performance and others are created for the purpose of understanding what is going on inside abstract domains.

### Equality

```ocaml
module D = SETr.Symbolic.Equality.Make(DI) (* eq(di) *)
```

The equality abstract domain modifier handles equalities of the following form outside of the underlying abstract domain.

> X = Y

Each constraint, before it is passed to the abstract domain is checked to see if it is an equality constraint.  Each time an equality is found, a representative is chosen and all equivalent symbols are rewritten using that representative.

In some abstract domains, equalities are not particularly efficiently represented.  By bringing these equalities out, they can be more efficiently handled.  The caveat is that rewriting the existing abstraction to use the representative becomes a frequent operation.  If the underlying domain does not efficiently support rewriting, the use of the equality domain modifier may be overly costly.

### Packing

```ocaml
module D = SETr.Symbolic.Packer.Make(DI) (* pack(di) *)
```
In many situations there are clusters of variables that are related.  If an abstraction has a theoretical cost of P(m), where m is the number of variables it tracks, packing splits that m into n clusters of o variables such that on average m = n × o.  This means that instead of the cost being P(n × o), the cost is n × P(o).  If P(o) is an exponential, or even a polynomial, this can be a significant benefit.

The packing domain modifier takes an abstract domain and intercepts all constraints sent to it.  An abstract state becomes a collection of abstract states in the underlying domain.  Each of those represents the abstract state for a single pack of variables.  Operations are then delegated to their appropriate pack.

If an operation involves variables from multiple packs, those packs are merged on the fly before delegating the operation.  This merging relies upon the meet operation.  If the underlying domain has an inefficient meet implementation, the packing operation can be expensive.

### Tracing

```ocaml
module C = struct
  let file = "trace.strace"
  let check = false
end
module D = SETr.Symbolic.Tracer.Make(C)(DI) (* trace(<false,> <'trace.strace',> di) *)
```

Tracing logs domain operations in a way that is compatible with the [strace format](#strace).  This allows re-executing domain operations for benchmarking later.

The tracing domain modifier takes a structure that configures tracing.  The `file` parameter is the file name to log the trace to.  The `check` parameter causes a sat check to occur for every constrain operation.  This can be useful in debugging if there are potentially problems in the constrain domain operation.

### Debugging

```ocaml
module D = SETr.Symbolic.Debugger.Make(DI) (* debug(di) *)
```

There are two printers that are available in the interface: `pp_print` shows a condensed output only displaying meaning of the constraints.  `pp_debug` shows a verbose output exposing the internal structure of the underlying abstraction.  The debugging domain modifier swaps `pp_print` for `pp_debug`.  This causes all printing to use the debug printer.  Since this can be embedded into a stack of domains, it can show internal structure at any level.

<a name="remap"></a>
### Remapping

```ocaml
module D = SETr.Symbolic.Remap.Make(DI) (* remap(di) *)
```

Remapping performs a global remapping of variables.  The first time a variable is used, it is added to a remapping table that increments a counter starting from 0.  As a result, all of the variables, no matter what their actual identifier, will be clustered near 0.  This is critical if the underlying abstract domain uses dense data structures such as arrays.

Remap could be more efficient than it is today because now it requires a global mapping rather than simply a live variable mapping.  However, the global mapping is often sufficient to fix problems, whereas the local mapping is much more likely to be needed only when there are other scalability problems.

### Statistics

```ocaml
module D = SETr.Symbolic.Stats.Make(DI) (* stats(di) *)
```

The statistics domain modifier collects statistics about the number of each type of domain operation that was performed underneath.  This information is printed along with the abstract state.

### Logging

```ocaml
module C = struct file = "domain.log" end
module D = SETr.Symbolic.Logger.Make(C)(DI) (* logger(<'domain.log',> di) *)
```

The logging domain modifier operates in a similar fashion to the tracer domain modifier.  However, in addition to logging every domain operation, it also shows the abstract state for each input and output before and after every domain operation.  This can be more useful than a screen dump of such information as it can be searched offline for critical information.

When using the string interface, the file name defaults to `domain.log`.

Note that this is a slow operation.  Even if printing is quite efficient, this performs a significant number of pretty printing operations and thus may significantly affect performance.  If performance is a concern, it may be useful to collect a trace using the tracer above, simplify the trace and then use the debugger on the simplified trace.

### Symbolic to SymSing

```ocaml
module D = SETr.SymSing.Sing.Make(DI) (* sing(di) *)
```

The `Sing` domain modifier converts a symbolic domain to a symbolic with singletons domain.  It does this by tracking which symbols are singleton and modifying constraints that flow through to the underlying domain.  In particular it handles a number of negation cases.

The translation of a non-negated constraint is typically straightforward.  For example:

> a ∈ X ⇒ A ⊆ X

However, in the negation, a direct translation is not possible because ⊈ is unsupported.  However, because a is a singleton, there is an alternative translation:

> ¬(a ∈ X) ⇒ A ∩ X = ∅

The sing domain modifier does not use the singleton information to strengthen proofs.  In theory this is possible, but as it seems rarely needed, this is not currently implemented functionality.

Additionally, the sing domain modifier does not perform relational cardinality reasoning.  For example, if {a} = X, the sing domain modifier will not infer that the cardinality of X is also 1.  It must be explicitly written as a constraint.


## Test language

With `make example`, a test interpreter is included.  This executes a test language that manipulates symbolic sets.  The language has the following syntax:

```
k ::= x = e
    | x = choose e               // overwrite x with an element of e
    | k; k                       // sequencing two commands
    | branch { k } else { k }    // non-deterministic branch
    | both { k } and { k }       // execute both branches in parallel
    | if ( c ) { k }             // conditional execution
    | if ( c ) { k } else { k }
    | while ( c ) { k }          // looping
    | loop { k }                 // non-deterministic loop
    | for ( x in e ) { k }       // loop over elements of e (e is mutable)
    | kill x                     // project out variable
    | rename x y                 // rename the variable x to be variable y
    | assume ( c )               // make an assumption about state
    | assert ( c )               // check property

e ::= {}                         // empty set
    |  v                         // set variable
    | { v }                      // singleton set
    | e U e                      // union
    | e U+ e                     // disjoint union
    | e \ e                      // set difference
    | e ^ e                      // intersection
    | ~e                         // set complement

c ::= e = e                      // set equality
    | e <= e                     // subset or equality
    | v in e                     // element containment
```

Examples of this language are contained in the `tests` directory.  These are
sample programs that exercise the built-in abstract interpreter.

## SDSL abstract interpreter usage

The included SDSL abstract interpreter is contained in the `sdsl` directory and
the corresponding `SDSL` module.  The static analyzer provides a number of
printing and usability options.

- `-step`  Prints each step of the program as it is interpreted and prints the
  abstract state after each step of the interpretation.
- `-final` Prints only the final abstract state
- `-brace` Prints braces around the abstract states (useful if abstract states
  require multiple to represent)
- `-color <color>` Shows abstract states in color to ease reading.  The
  `<color>` argument supports the following colors `black`, `red`, `green`,
  `yellow`, `blue`, `magenta`, `cyan`, and `white`.  Additionally each color can
  be converted to it's "bright" version by appending it with `!`.  For example,
  `red!` would be bright red.
- `-time` Times the analysis (excluding parse/prepreocess time) and reports the
  total time at the end of the analysis.

Options as above can be set on each command line.  However if they should
persist, they can be assigned through an environment variable.  Use the
variable `SDSLPARAMS` as an additional command-line place.  For example to make
step printing and bright red abstract states standard, execute the following
bash command:

```
$ export SDSLPARAMS='-step -color red!'
```

The command line implements a stack-based declarative language for selecting
and combining abstract domains.  By default the stack is empty, so running the
tool without specifying a domain will give an error:

```
$ ./Main.d.byte 
Error: Domain stack is empty, please specify an abstract domain
```

The abstract domain that the SDSL interpreter uses is determined by the `-dom` command-line flag.  The argument to `-dom` is a domain construction string as described above.  _The resulting domain must be a symbolic sets with singletons domain._

<a name="strace"></a>
## Trace inputs

To help in comparing abstract domains as used by other tools, the analyzer
supports the ability to replay traces.  Traces with the extension `*.strace`
and have the following format where `x` and `y` are variables that represent
abstract states, `v`, `v1`, etc are dimensions (integers) in the abstract
state.

```
k ::= let x = t                         // evaluate a transfer function
    | le x y                            // compare abstract states x and y
    | is_bottom x                       // check if abstract state x is bottom
    | is_top x                          // check if abstract state x is top
    | sat x c                           // check if abstract state satisfies constraint

t ::= top                               // return top
    | bottom                            // return top
    | constrain c x                     // return x constrained by c
    | join x y                          // overapproximate disjunction of abstract states x and y
    | widening x y                      // overapproximate disjunction of abstract states x and y (terminating)
    | meet x y                          // overapproximate conjunction of abstract states x and y
    | forget v1 v2 ... x                // drop dimensions v1, v2 etc from x
    | rename [v1 -> v2; v3 -> v4 ...] x // rename dimension v1 to v2, and v3 to v4 in x

e ::= {}                                // empty set
    |  v                                // set variable
    | { v }                             // singleton set
    | e U e                             // union
    | e U+ e                            // disjoint union
    | e \ e                             // set difference
    | e ^ e                             // intersection
    | ~e                                // set complement

c ::= e = e                             // set equality
    | e <= e                            // subset or equality
    | v in e                            // element containment
```

The result of running an analysis on a trace is statistics of the run:

```
$ ./Main.native -lin tests/test1.strace 

sat: 0/1
le : 0/0
bot: 1/1
top: 1/1
```

This shows the four kinds of query and the ratio of `true` results to total
results for those queries.

## Adding new abstract domains and combinators

The SETr library is designed to be easily extended with new abstract domains and abstract domain modifiers.

1. Create a file for the domain.  It should got into the appropriate directory depending on if the result of construct it is a `symbolic` or a `symsing` domain.  The name of the file should be `SETr_*_name`, where the * is `Symbolic` or `SymSing` depending on the result of the domain.
2. Add the domain to the appropriate collection module.  This would be either `SETr_Symbolic.ml` or `SETr_SymSing.ml`.  In here there should be a module definition of the form `module name = SETr_*_name`.
3. Add the string constructor to `SETr_DomainBuilder.ml`.  The name for the domain should be added and errors should be handled appropriately.  Additionally, this file contains the help information for the particular domain or domain modifier.

