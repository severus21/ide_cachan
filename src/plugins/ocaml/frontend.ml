class plg_ocaml=object(self)
    method file_extensions = [
        ["ml";"mli"];
        ["ml"]
    ]

    method path_to_c_ast path=
        let res = Extract.extract_from_rules path (self#file_extensions) in 
        let project_path = (match Filename.dirname path with |"."->"" |p->p) in  
        let (tl_asts:Tl_ast.tl_ast list) = List.map (Ml_to_tl.entries_to_tl_ast project_path) (Utility.enumerate res) in
        Extract.print res;            
        Tl_to_c.tl_ast_to_c_ast (List.fold_left (fun acc x->x@acc) [] tl_asts)

    method string_to_c_ast str= 
        Tl_to_c.tl_ast_to_c_ast (Ml_to_tl.str_to_tl_ast str)
   
    method c_ast_to_folder (path:string) (ast:Core.Miscs.c_ast)=  
        Ml_to_tl.tl_ast_to_folder path (Tl_to_c.c_ast_to_tl_ast ast)
end

let make_plg ()= new plg_ocaml 
