(*extend some generic frontend*)

let string_to_c_ast filename str=
    Tl_to_c.tl_ast_to_core filename (Ml_to_tl.quick_tl_ast str)  

let c_ast_to_str ast =
   Ml_to_tl.tl_ast_to_str (Tl_to_c.c_ast_to_tl_ast ast)
