type language = Ocaml 

let load=function
    |Ocaml->Ocaml.Frontend.make_plg ()


let () =
    let plug = load Ocaml in

      
    let str = Utility.file_to_string "src/plugins/ocaml/compile.ml" in
    let str2 = plug#c_ast_to_str (plug#string_to_c_ast "tmp.ml" str) in
    
    Printf.printf "%s" str2;
    
    (*let fp = open_out "trollesque" in  
    Printf.fprintf fp "%s" str2;    
    close_out fp;*)
(*
    let ast = string_to_ast str and tl_ast = quick_tl_ast str in
    print_ast ast;
    print_tl_ast tl_ast;
    let core_ast = tl_ast_to_core "filename" tl_ast in
    print_c_ast core_ast;  
    print_tl_ast (c_ast_to_tl_ast core_ast);  
 *)  
    
