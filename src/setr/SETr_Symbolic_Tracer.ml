module type L = sig
  val file: string
  val check: bool
end
module Rename = SETr_Rename

module S = SETr_Symbolic_Logic

module Make(L: L)(D: SETr_Symbolic_Interface.S) : SETr_Symbolic_Interface.S = struct

  type ctx = {
    fout: out_channel;
    ff: Format.formatter;
    ctx: D.ctx;
    counter: int ref;
  }

  let fresh_id ctx =
    let id = !(ctx.counter) in
    incr ctx.counter;
    id

  type t = {
    id: int;
    t: D.t;
  }

  type sym = int

  type cnstr = int S.t

  type output = int S.t

  type query = int S.q

  let pp ctx ff t =
    D.pp_print ctx Format.pp_print_int ff t
    (*S.pp Format.pp_print_int ff (D.serialize t.t)*)

  let ppc ff c =
    S.pp Format.pp_print_int ff c

  let init () = 
    let fout = open_out L.file in
    let ff = Format.formatter_of_out_channel fout in
    let ctx = D.init () in
    let counter = ref 0 in
    { fout; ff; ctx; counter }

  let top ctx =
    let id = fresh_id ctx in
    Format.fprintf ctx.ff "let x%d = top@." id;
    {
      id;
      t = D.top ctx.ctx;
    }

  let bottom ctx =
    let id = fresh_id ctx in
    Format.fprintf ctx.ff "let x%d = bottom@." id;
    {
      id;
      t = D.bottom ctx.ctx;
    }

  let symbols ctx t =
    D.symbols ctx.ctx t.t

  let constrain ctx cnstr t =
    let id = fresh_id ctx in
    Format.fprintf ctx.ff "let x%d = constrain %a x%d@." id (S.pp ~parse:true Format.pp_print_int) cnstr t.id;
    let dt = D.constrain ctx.ctx cnstr t.t in
    if L.check then Format.fprintf ctx.ff "sat x%d %a@." id (S.pp ~parse:true Format.pp_print_int) (D.serialize ctx.ctx dt);
    {t = dt; id}


  let serialize ctx t =
    D.serialize ctx.ctx t.t

  let sat ctx t cnstr =
    Format.fprintf ctx.ff "sat x%d %a@." t.id (S.pp ~parse:true Format.pp_print_int) cnstr;
    D.sat ctx.ctx t.t cnstr

  let join ctx a b =
    let id = fresh_id ctx in
    Format.fprintf ctx.ff "let x%d = join x%d x%d@." id a.id b.id;
    {t =D.join ctx.ctx a.t b.t; id}

  let widening ctx a b =
    let id = fresh_id ctx in
    Format.fprintf ctx.ff "let x%d = widening x%d x%d@." id a.id b.id;
    {t = D.widening ctx.ctx a.t b.t; id}

  let meet ctx a b =
    let id = fresh_id ctx in
    Format.fprintf ctx.ff "let x%d = meet x%d x%d@." id a.id b.id;
    {t = D.meet ctx.ctx a.t b.t; id}

  let le ctx a b =
    Format.fprintf ctx.ff "le x%d x%d@." a.id b.id;
    D.le ctx.ctx a.t b.t

  let is_bottom ctx t =
    Format.fprintf ctx.ff "is_bottom x%d@." t.id;
    D.is_bottom ctx.ctx t.t

  let is_top ctx t =
    Format.fprintf ctx.ff "is_top x%d@." t.id;
    D.is_top ctx.ctx t.t

  let forget ctx syms t =
    let id = fresh_id ctx in
    Format.fprintf ctx.ff "let x%d = forget %a x%d@." id
      (SETr_DS_List.pp_print ~pp_sep:(fun ff () -> Format.pp_print_string ff " ") Format.pp_print_int) syms
      t.id;
    {t = D.forget ctx.ctx syms t.t; id}

  let rename_symbols ctx map t =
    let id = fresh_id ctx in
    let l = Rename.to_assoc_list map in
    Format.fprintf ctx.ff "let x%d = rename [%a] x%d@." id
      (SETr_DS_List.pp_print ~pp_sep:(fun ff () -> Format.pp_print_string ff "; ") (fun ff (a,b) -> Format.fprintf ff "%d -> %d" a b)) l
      t.id;
    {t = D.rename_symbols ctx.ctx map t.t; id}

  let query ctx t =
    failwith "query unsupported in trace"
    (*D.query t.t*)

  let combine ctx q t =
    failwith "combine unsupported in trace"
    (*{t with t = D.combine q t.t}*)

  let pp_print ctx pp_sym ff t =
    D.pp_print ctx.ctx pp_sym ff t.t

  let pp_debug ctx pp_sym ff t =
    D.pp_print ctx.ctx pp_sym ff t.t


end


let _ =
  let open SETr_DomainRegistrar in
  let build args =
    let default_name = "trace.strace" in
    let default_check = false in
    let c, s, d = match args with
      | [Bool c; String s; Symbolic d] -> (c, s, d)
      | [String s; Symbolic d] -> (default_check, s, d)
      | [Bool c; Symbolic d] -> (c, default_name, d)
      | [Symbolic d] -> (default_check, default_name, d)
      | _ ->
        build_error "trace takes an optional boolean, an optional file name, and a set domain"
    in
    let module L = (struct
      let file = s
      let check = c
    end) in
    Symbolic (module Make(L)((val d)))
  in
  let args = "([t/f], [fname], <sym>)" in
  let help = "Builds an tracing symbolic domain that if the first argument is true adds checks to the trace. The trace file is generated to fname (default trace.strace" in
  register "symbolic.trace" build args help;
  alias "trace" "symbolic.trace"
