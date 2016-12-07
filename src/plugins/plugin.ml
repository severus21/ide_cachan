open Ocaml.Frontend

let () =
    let str = Utility.file_to_string "src/plugins/ocaml/tmp.ml" in
    let str2 = c_ast_to_str (string_to_c_ast "tmp.ml" str) in

    Printf.printf "%s" str2;    
(*
    let ast = string_to_ast str and tl_ast = quick_tl_ast str in
    print_ast ast;
    print_tl_ast tl_ast;
    let core_ast = tl_ast_to_core "filename" tl_ast in
    print_c_ast core_ast;  
    print_tl_ast (c_ast_to_tl_ast core_ast);  
 *)  
    
