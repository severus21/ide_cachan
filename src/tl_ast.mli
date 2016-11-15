(** This module contains functions allowing to manipulate ocaml top-level ast*)

(** top-level structures*)
type tl_struct =  
|Tl_none (*to be removed*)
|Tl_open of string (*for the moment , we only extract  the complete line of the openning*)
|Tl_var of string * string
|Tl_fun of string * string
|Tl_exception of string * string
|Tl_type of (string * string) list

(** Top-level ast type*)
type tl_ast = tl_struct list


(** Returns the string containing the whole file f *)
val file_to_string : string -> string

(** Gets a parsetree of the ocaml file given in argument *)
val get_ast : string ->  Parsetree.structure_item list

(** Prints a parstree *)
val print_ast : Parsetree.structure_item list -> unit


(** Converts a parsetree into a tl_ast *)
val ast_to_tl_ast : string -> Parsetree.structure_item list -> tl_ast

(** Prints a tl_ast *)
val print_tl_ast : tl_ast -> unit




