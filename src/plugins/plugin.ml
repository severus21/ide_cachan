open Ocaml.Tl_ast

let () =
    let str = Utility.file_to_string "src/plugins/ocaml/tmp.ml" in

  let ast = string_to_ast str and tl_ast = quick_tl_ast str in
    print_ast ast;
    print_tl_ast tl_ast;


