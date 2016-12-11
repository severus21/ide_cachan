open Factory

let () =
    let plug = make_plg Ocaml in

      
    (*let str = Utility.file_to_string "src/plugins/extract.mli" in*)
    let _= plug#c_ast_to_folder "trollll" (let t,_=plug#path_to_c_ast "src/plugins/ocaml/"in t) in     
    ()
 
