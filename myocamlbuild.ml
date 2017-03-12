open Ocamlbuild_plugin

(* enable cppo flags *)
(*let () =
  dispatch (fun hook -> Ocamlbuild_cppo.dispatcher hook; ) *)

(* make library accessible *)
let () =
  dispatch begin function
    | After_rules ->
      ocaml_lib "src/setr/libsetr"
    | _ -> ()
  end

  
