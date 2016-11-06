let build_domain name args =
  let (build, help, _) = Hashtbl.find SETr_DomainRegistrar.registered name in
  build args

let max_width = 80
let help_indent = 8

let print_help ff () =
  Format.pp_open_vbox ff 2;
  Format.fprintf ff "  Domains can be constructed with the following commands:@,@,";
  let alias_table = Hashtbl.create 29 in
  let commands = Hashtbl.fold (fun name (_,_,help) l ->
      match help with
      | Some orig_name ->
        let upd = name::(try
            Hashtbl.find alias_table orig_name
          with Not_found -> []) in
        Hashtbl.replace alias_table orig_name upd;
        l
      | None ->
        name::l
    ) SETr_DomainRegistrar.registered [] in
  let commands = List.sort compare commands in

  List.iter (fun name ->
      let (_, (args, help), _) = Hashtbl.find SETr_DomainRegistrar.registered name in
      Format.pp_open_vbox ff help_indent;
      let aliases = try
          Hashtbl.find alias_table name |>
          List.sort compare |>
          String.concat " " |>
          (^) " - aliases: "
        with Not_found -> "" in
      Format.fprintf ff "%s%s%s@,@[<hov 0>" name args aliases;
      String.iter (function
          | ' ' -> Format.pp_print_space ff ()
          | '\n' -> Format.pp_print_space ff ()
          | '\r' -> ()
          | '\t' -> Format.fprintf ff "    "
          | c -> Format.pp_print_char ff c
        ) help;
      Format.pp_close_box ff ();
      Format.pp_close_box ff ();
      Format.pp_print_cut ff ()
    ) commands;
  Format.pp_close_box ff ();
  Format.pp_print_flush ff ()

let get_help () =
  let ff = Format.std_formatter in
  print_help ff ();
  Format.pp_print_newline ff ();
  exit 2

let help_string =
  let b = Buffer.create 1024 in
  let ff = Format.formatter_of_buffer b in
  print_help ff ();
  Buffer.contents b


(*let build_domain name args =
  match name, args with

(************************************************)
(**             Abstract Domains               **)
(************************************************)
#ifdef PKG_MLBDD
  | "bdd-mlbdd", [] ->
    Symbolic (module SETr_Symbolic_BDD_MLBDD)
  | "bdd-mlbdd", _ ->
    raise (Build_error "bdd-full domain does not accept any arguments")
#else
  | "bdd-mlbdd", _ ->
    raise (Build_error "BDD-based domains were disabled at compile time")
#endif
#ifdef PKG_CUDD
  | "bdd-cudd", [] ->
    Symbolic (module SETr_Symbolic_BDD_Cudd.Make(struct let reorder = None end))
  | "bdd-cudd", [String s] ->
    Symbolic (module SETr_Symbolic_BDD_Cudd.Make(struct let reorder = (Some s) end))
  | "bdd-cudd", _ ->
    raise (Build_error "bdd-full domain does not accept any arguments")
#else
  | "bdd-cudd", _ ->
    raise (Build_error "CUDD-based domains were disabled at compile time")
#endif
  | "bdd-xbdd", [] ->
    Symbolic (module SETr_Symbolic_BDD_XBDD)
  | "bdd-xbdd", _ ->
    raise (Build_error "bdd-x-full domain does not accept any arguments")

#ifdef PKG_CUDD
  | "bdd", [] ->
    Symbolic (module SETr_Symbolic_BDD_Cudd.Make(struct let reorder = None end))
  | "bdd-opt", [] ->
    let module D = SETr_Symbolic_BDD_Cudd.Make(struct let reorder = None end) in
    let module D = SETr_Symbolic_Remap.Make(D) in
    let module D = SETr_Symbolic_Packer.Make(D) in
    Symbolic (module D)
#else
#ifdef PKG_MLBDD
  | "bdd", [] ->
    Symbolic (module SETr_Symbolic_BDD_MLBDD)
  | "bdd-opt", [] ->
    let module D = SETr_Symbolic_BDD_MLBDD in
    let module D = SETr_Symbolic_Packer.Make(D) in
    let module D = SETr_Symbolic_Equality.Make(D) in
    Symbolic (module D)
#else
  | "bdd", [] ->
    Symbolic (module SETr_Symbolic_BDD_XBDD)
  | "bdd-opt", [] ->
    let module D = SETr_Symbolic_BDD_XBDD in
    let module D = SETr_Symbolic_Packer.Make(D) in
    let module D = SETr_Symbolic_Equality.Make(D) in
    Symbolic (module D)
#endif
#endif
  | "lin", [] ->
    SymSing (module SETr_SymSing_Lin)
  | "lin", _ ->
    raise (Build_error "lin domain does not accept any arguments")
#ifdef PKG_Z3
  | "smt", [] ->
    Symbolic (module SETr_Symbolic_SMT)
  | "smt", _ ->
    raise (Build_error "smt domain does not accept any arguments")
  | "quicg", [] ->
    Symbolic (module SETr_Symbolic_QUICG)
  | "quicg", _ ->
    raise (Build_error "quic domain does not accept any arguments")
#else
  | "smt", _
  | "quicg", _ ->
    raise (Build_error "SMT-based domains smt and quic were disabled at compile time")
#endif
#ifdef PKG_QBF
  | "qbf", l ->
    let arg = match l with
      | [] -> "depqbf"
      | [String s] -> s
      | _ ->
        raise (Build_error "qbf domain accepts an optional string argument specifying the solver")
    in
    let s = match arg with
      | "depqbf" -> (module QBF_SolverDepQBF : QBF.Solver)
      | s -> (Build_error ("QBF solver '"^s^"' is not supported"))
    in
    Symbolic (module QBF.Domain.Make((val s)))
#else
  | "qbf", _ ->
    raise (Build_error "QBF-based domain was disabled at compile time")
#endif

(************************************************)
(**            Domain Combinators              **)
(************************************************)

  | "logger", l ->
    let fname, d = match l with
      | [String fname; Symbolic d] -> (fname, d)
      | [Symbolic d] -> ("domain.log", d)
      | _ ->
        raise (Build_error "logger takes an optional file name and a set domain as arguments")
    in
    let module L = (struct
      let file = fname
    end) in
    let module D = (val d) in
    let module Log = SETr_Symbolic_Logger.Make(L)(D) in
    Symbolic (module Log)

  | "pack", [Symbolic d] ->
    Symbolic (module SETr_Symbolic_Packer.Make((val d)))
  | "pack", _ ->
    raise (Build_error "pack takes a set domain as an argument")

  | "eq", [Symbolic d] ->
    Symbolic (module SETr_Symbolic_Equality.Make((val d)))
  | "eq", _ ->
    raise (Build_error "eq takes a set domain as an argument")

  | "debug", [Symbolic d] ->
    Symbolic (module SETr_Symbolic_Debugger.Make((val d)))
  | "debug", _ ->
    raise (Build_error "debug takes a set domain as an argument")

  | "trace", l ->
    let default_name = "trace.strace" in
    let default_check = false in
    let c, s, d = match l with
      | [Bool c; String s; Symbolic d] -> (c, s, d)
      | [String s; Symbolic d] -> (default_check, s, d)
      | [Bool c; Symbolic d] -> (c, default_name, d)
      | [Symbolic d] -> (default_check, default_name, d)
      | _ ->
        raise (Build_error "trace takes an optional boolean, an optional file name, and a set domain")
    in
    let module L = (struct
      let file = s
      let check = c
    end) in
    Symbolic (module SETr_Symbolic_Tracer.Make(L)((val d)))

  | "sing", [Symbolic d] ->
    SymSing (module SETr_SymSing_Sing.Make((val d)))
  | "sing", _ ->
    raise (Build_error "sing takes a set domain as an argument")

  | "remap", [Symbolic d] ->
    Symbolic (module SETr_Symbolic_Remap.Make((val d)))
  | "remap", _ ->
    raise (Build_error "remap takes a set domain as an argument")

  | "stats", [Symbolic d] ->
    Symbolic (module SETr_Symbolic_Stats.Make((val d)))
  | "stats", _ ->
    raise (Build_error "stats takes a set domain as an argument")

(************************************************)
(**                Error Case                  **)
(************************************************)

  | s, _ ->
    raise (Build_error ("unrecognized domain '"^s^"'"))

let help_string =
"  The domain construction language has the following grammar:

  d ::= lin                  - A linear abstraction for disjoint unions
      | bdd-mlbdd            - A BDD-based abstraction for sets
      | bdd-cudd [('order')] - A high-performance implementation based the C-
                               native BDD library Cudd by Fabio Somenzi. 
                               Optional ordering enables dynamic, on-demand
                               reordering using the specified ordering.
                               Orderings are from CUDD.
      | bdd-xbdd             - An abstraction based on Xavier Leroy's BDD
                               implementation
      | bdd                  - default BDD implementation
      | bdd-opt              - A pre-built optimized combination of domains
      | smt                  - An SMT-based abstraction for sets
      | quicg                - A QUIC-graphs-based abstraction for sets

      | logger( ['file',] d) - Log domain operations to file.
                               Default file is domain.log.
      | pack(d)              - Use a separate abstract state for each group of
                               related variables.
      | eq(d)                - Track equality externally.
      | debug(d)             - Turn on debug printer for domain.
      | trace( [true,] ['file',] d )
                             - Generate strace file for domain operations.
                               If true is provided as first argument, produce a
                               sat check of each constraint after each
                               constrainquery.  Optional file name for trace
                               defaults to trace.strace.
      | sing(d)              - Syntactic handling of singleton sets.
      | stat(d)              - Collect domain statistics
      | remap(d)             - Remap symbols onto smallest natural numbers"

let get_help () =
  Format.printf "%s@." help_string;
  exit 2 *)
