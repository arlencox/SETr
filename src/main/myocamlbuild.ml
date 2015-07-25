open Ocamlbuild_plugin
open Command

let _ =
  dispatch begin function
    | After_rules ->
      let dir = Sys.getenv "SETR_LIB" in
      ocaml_lib ~extern:true ~dir:dir "SETr";
    | _ -> ()
  end
