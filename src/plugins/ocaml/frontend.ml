let file_extensions = [
    ["ml";"mli"];
    ["ml"]
]

let string_to_c_ast filename str=
    Tl_to_c.tl_ast_to_c_ast filename (Ml_to_tl.str_to_tl_ast str)  

let c_ast_to_str ast =
   Ml_to_tl.tl_ast_to_str (Tl_to_c.c_ast_to_tl_ast ast)
