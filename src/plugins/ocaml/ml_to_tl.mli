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

(** Transform a list of rule's entry to a tl_ast
    @param project_path, the parent dir of the project imported
    @param p as (rule_number, entries) where entries are a list of relation
        a relation is a list of files
    @return the tl_ast related to entries
  *)
val entries_to_tl_ast : string -> int * string list list -> tl_ast

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


(** Export a top-level ast into a caml folder
    @param path of the folder 
    @param ast a tl_ast, where roots are Tl_module,Tl_sig, and Tl_module_constrainte describing files*)
val tl_ast_to_folder : string->tl_ast -> unit

(** Prints a tl_ast, for debbuging purposes 
    @param ast a tl_ast*)
val print_tl_ast : tl_ast -> unit

(** Generate unittests for this module*)                               
val tests : unit -> OUnit2.test
