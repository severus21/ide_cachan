open Factory

let () =
    let plug = make_plg Ocaml in

      
    (*let str = Utility.file_to_string "src/plugins/extract.mli" in*)
    let _= plug#c_ast_to_folder "trollll" (plug#path_to_c_ast "src/plugins/ocaml/") in     
    ()
(*
    let ext1 = ["ml";"mli"] in
let ext2 = ["ml"] in 

let start_path = "/home/severus/Projects" in

let list3,list4 = main ext1 ext2 start_path in

 
    Printf.printf "liste commune :\n";  
    List.iter (fun x-> List.iter (fun y ->  Printf.printf "%s\n" y) x) list3; 
    Printf.printf "\nliste non commune :\n" ; 
    List.iter (fun x-> Printf.printf "%s\n" x) list4;  
    Printf.printf "\n"
 *)
    
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
 
