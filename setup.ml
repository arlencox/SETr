#use "topfind"
#require "findlib"
#require "str"


let mandatory_packages = [
  "ocamlbuild";
  "cppo_ocamlbuild";
]

let optional_packages = [
  "Z3";
  "cudd";
  "mlbdd";
]

let _ =
  Findlib.init ()

(*let if_expr = Str.regexp "#if[^#]#endif"*)
let if_split = Str.regexp "#ifdef \\([^ \r\n\t]+\\)[ \r\n\t]+\\([^#]*\\)[ \r\n\t]*#endif"

(* built-in uppercase function cannot be used due to changes in standard library 4.02 to 4.03 *)
let uppercase str =
  String.init (String.length str) (fun i ->
      let c = Char.code str.[i] in
      let c = if c >= 97 && c <= 122 then
          c - (97-65)
        else
          c in
      Char.chr c
    )

(* file reader *)
let read_file fn =
  let infile = open_in fn in
  let b = Buffer.create 1024 in
  try
    while true do
      Buffer.add_char b (input_char infile)
    done;
    assert false
  with End_of_file ->
    Buffer.contents b

(* Copy/preprocess utility *)
let preproc found_packages oldfile newfile =
  let flags = List.map (fun pkg -> "-D "^pkg) found_packages in
  let flags = String.concat " " flags in
  let cmd = Printf.sprintf "cppo -n %s -o '%s' '%s'" flags newfile oldfile in
  let res = Sys.command cmd in
  if res <> 0 then begin
    Printf.printf "Command %s ended with code %d" cmd res;
    exit 1
  end

let check_package pkg =
  try
    ignore (Findlib.package_directory pkg);
    true
  with Findlib.No_such_package _ ->
    false

let check_packages () =
  List.iter (fun pkg ->
      Printf.printf "Checking for package %s: " pkg;
      if check_package pkg then
        Printf.printf "found\n"
      else begin
        Printf.printf "not found\n";
        Printf.printf "Package %s is required and must be installed\n" pkg;
        exit 1
      end
    ) mandatory_packages;
  let pkgs = List.filter (fun pkg ->
      Printf.printf "Checking for package %s: " pkg;
      if check_package pkg then begin
        Printf.printf "found\n";
        true
      end else begin
        Printf.printf "not found\n";
        false
      end
    ) optional_packages in
  List.map (fun pkg ->
      "PKG_"^(uppercase pkg)
    ) pkgs


let _ =

  let found_packages = check_packages () in
  print_endline "Generating Makefile";
  preproc found_packages "Makefile.in" "Makefile";
  print_endline "Generating _tags";
  preproc found_packages "_tags.in" "_tags";
  print_endline "Generating META";
  preproc found_packages "META.in" "META";
  print_endline "Generating src/setr/libsetr.mllib";
  preproc found_packages "src/setr/libsetr.mllib.in" "src/setr/libsetr.mllib"
