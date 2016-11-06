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