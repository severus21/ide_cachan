open Core.Miscs

type plug =
    <
    name:string;
    file_extensions:string list list;
    path_to_c_ast:string->c_ast * string list;
    string_to_c_ast:string->c_ast;
    c_ast_to_folder:string->c_ast->unit
    >

