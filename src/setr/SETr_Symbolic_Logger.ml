module Rename = SETr_Rename
module type L = sig
  val file: string
end

module S = SETr_Symbolic_Logic

module Make(L: L)(D: SETr_Symbolic_Interface.S) : SETr_Symbolic_Interface.S
= struct

  type ctx = {
    fout: out_channel;
    ff: Format.formatter;
    ctx: D.ctx;
  }

  type t = D.t

  type sym = int

  type cnstr = int S.t

  type output = int S.t

  type query = int S.q

  let pp ctx ff t =
    D.pp_print ctx.ctx Format.pp_print_int ff t
    (*S.pp Format.pp_print_int ff (D.serialize t.t)*)

  let ppc ff c =
    S.pp Format.pp_print_int ff c

  let init () = 
    let fout = open_out L.file in
    let ff = Format.formatter_of_out_channel fout in
    let ctx = D.init () in
    { fout; ff; ctx }

  let top ctx =
    Format.fprintf ctx.ff "top@.";
    D.top ctx.ctx

  let bottom ctx =
    Format.fprintf ctx.ff "bottom@.";
    D.bottom ctx.ctx

  let symbols ctx t =
    Format.fprintf ctx.ff "symbols: %a@." (pp ctx) t;
    D.symbols ctx.ctx t

  let constrain ctx cnstr t =
    Format.fprintf ctx.ff "@[<v 2>constrain:@,";
    Format.fprintf ctx.ff "cnstr: %a@," ppc cnstr;
    Format.fprintf ctx.ff "pre  : %a@," (pp ctx) t;
    let t = D.constrain ctx.ctx cnstr t in
    Format.fprintf ctx.ff "post : %a" (pp ctx) t;
    Format.fprintf ctx.ff "@]@.";
    t


  let serialize ctx t =
    Format.fprintf ctx.ff "serialize: %a@." (pp ctx) t;
    D.serialize ctx.ctx t

  let sat ctx t cnstr =
    Format.fprintf ctx.ff "@[<v 2>sat:@,";
    Format.fprintf ctx.ff "pre  : %a@," (pp ctx) t;
    Format.fprintf ctx.ff "cnstr: %a@," ppc cnstr;
    let res = D.sat ctx.ctx t cnstr in
    Format.fprintf ctx.ff "res  : %b" res;
    Format.fprintf ctx.ff "@]@.";
    res

  let join ctx a b =
    Format.fprintf ctx.ff "@[<v 2>join:@,";
    Format.fprintf ctx.ff "a  : %a@," (pp ctx) a;
    Format.fprintf ctx.ff "b  : %a@," (pp ctx) b;
    let res = D.join ctx.ctx a b in
    Format.fprintf ctx.ff "res: %a" (pp ctx) res;
    Format.fprintf ctx.ff "@]@.";
    res

  let widening ctx a b =
    Format.fprintf ctx.ff "@[<v 2>widening:@,";
    Format.fprintf ctx.ff "a  : %a@," (pp ctx) a;
    Format.fprintf ctx.ff "b  : %a@," (pp ctx) b;
    let res = D.widening ctx.ctx a b in
    Format.fprintf ctx.ff "res: %a" (pp ctx) res;
    Format.fprintf ctx.ff "@]@.";
    res

  let meet ctx a b =
    Format.fprintf ctx.ff "@[<v 2>meet:@,";
    Format.fprintf ctx.ff "a  : %a@," (pp ctx) a;
    Format.fprintf ctx.ff "b  : %a@," (pp ctx) b;
    let res = D.meet ctx.ctx a b in
    Format.fprintf ctx.ff "res: %a" (pp ctx) res;
    Format.fprintf ctx.ff "@]@.";
    res

  let le ctx a b =
    Format.fprintf ctx.ff "@[<v 2>le:@,";
    Format.fprintf ctx.ff "a  : %a@," (pp ctx) a;
    Format.fprintf ctx.ff "b  : %a@," (pp ctx) b;
    let res = D.le ctx.ctx a b in
    Format.fprintf ctx.ff "res: %b" res;
    Format.fprintf ctx.ff "@]@.";
    res

  let is_bottom ctx t =
    Format.fprintf ctx.ff "@[<v 2>is_bottom:@,";
    Format.fprintf ctx.ff "pre: %a@," (pp ctx) t;
    let res = D.is_bottom ctx.ctx t in
    Format.fprintf ctx.ff "res: %b" res;
    Format.fprintf ctx.ff "@]@.";
    res

  let is_top ctx t =
    Format.fprintf ctx.ff "@[<v 2>is_top:@,";
    Format.fprintf ctx.ff "pre: %a@," (pp ctx) t;
    let res = D.is_top ctx.ctx t in
    Format.fprintf ctx.ff "res: %b" res;
    Format.fprintf ctx.ff "@]@.";
    res

  let forget ctx syms t =
    Format.fprintf ctx.ff "@[<v 2>forget:@,";
    Format.fprintf ctx.ff "syms: @[<h>%a@]@," (SETr_DS_List.pp_print Format.pp_print_int) syms;
    Format.fprintf ctx.ff "pre : %a@," (pp ctx) t;
    let res = D.forget ctx.ctx syms t in
    Format.fprintf ctx.ff "res: %a" (pp ctx) res;
    Format.fprintf ctx.ff "@]@.";
    res

  let rename_symbols ctx map t =
    Format.fprintf ctx.ff "@[<v 2>rename_symbols:@,";
    Rename.fold (fun a b () ->
        Format.fprintf ctx.ff "  %d -> %d@," a b
      ) map ();
    Format.fprintf ctx.ff "pre : %a@," (pp ctx) t;
    let res = D.rename_symbols ctx.ctx map t in
    Format.fprintf ctx.ff "res: %a" (pp ctx) res;
    Format.fprintf ctx.ff "@]@.";
    res

  let query ctx t =
    Format.fprintf ctx.ff "@[<v 2>query:@,";
    Format.fprintf ctx.ff "pre: %a" (pp ctx) t;
    Format.fprintf ctx.ff "@]@.";
    D.query ctx.ctx t

  let combine ctx q t =
    Format.fprintf ctx.ff "@[<v 2>combine:@,";
    Format.fprintf ctx.ff "pre: %a@," (pp ctx) t;
    let res = D.combine ctx.ctx q t in
    Format.fprintf ctx.ff "res: %a" (pp ctx) res;
    Format.fprintf ctx.ff "@]@.";
    res

  let pp_print ctx pp_sym ff t =
    D.pp_print ctx.ctx pp_sym ff t

  let pp_debug ctx pp_sym ff t =
    D.pp_print ctx.ctx pp_sym ff t


end

let _ =
  let open SETr_DomainRegistrar in
  let build args =
    let fname, d = match args with
      | [String fname; Symbolic d] -> (fname, d)
      | [Symbolic d] -> ("domain.log", d)
      | _ -> build_error "logger takes an optional file name and a set domain as arguments"
    in
    let module L = (struct
      let file = fname
    end) in
    let module D = (val d) in
    let module Log = Make(L)(D) in
    Symbolic (module Log)
  in
  let args = "([fname], <sym>)" in
  let help = "Builds an logging symbolic domain (to file fname default domain.log) from a symbolic domain" in
  register "symbolic.logger" build args help;
  alias "logger" "symbolic.logger"
