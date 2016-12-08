(** This module contains functions to import/export tl_ast from/to caml ast*)

open Tl_ast

(** Opens a file and returns its ocaml ast
    @param path path of the ocaml file to convert
    @return caml ast*)
val file_to_ast : string ->  Parsetree.structure_item list

(** Transform a string containing some ml code into a caml ast
    @param str string representing a piece of caml code
    @return the caml ast related to str*)
val string_to_ast : string -> Parsetree.structure_item list

(** Prints a parsetree 
    @param ast parstree to print *)
val print_ast : Parsetree.structure_item list -> unit


(** Transform  a string containing some ml code into a tl_ast 
    @param str string representing a piece of caml code
    @return the tl_ast related to str*)
val str_to_tl_ast: string -> tl_ast

(** Transform  a string containing some ml code into a tl_struct,
    used mainly for testing
    @param str string representing a piece of caml code
    @return the first tl_structure of the tl_ast related to str*)
val str_to_tl_struct: string -> tl_struct 


(** Converts a top-level ast to a string
    @param ast a tl_ast
    @return the caml code related to ast*)                                    
val tl_ast_to_str : tl_ast -> string

(** Prints a tl_ast, for debbuging purposes 
    @param ast a tl_ast*)
val print_tl_ast : tl_ast -> unit


