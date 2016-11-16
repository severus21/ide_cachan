(** This module contains functions allowing to manipulate ocaml top-level ast*)

(** top-level structures*)
type tl_struct =  
|Tl_none (*to be removed*)
|Tl_open of string list * string (* the string list represents Module1.Module2. ... 
				 the secund parameter is the complete lign*)
|Tl_var of string * string
|Tl_fun of string * string
|Tl_exception of string * string
|Tl_type of string list * string

(** Top-level ast type*)
type tl_ast = tl_struct list


(** Returns the string containing the whole file f *)
val file_to_string : string -> string

(**Opens a file and returns its ocaml ast*)
val file_to_ast : string ->  Parsetree.structure_item list

(** Transform a string containing some ml code into a parsetree*)
val string_to_ast : string -> Parsetree.structure_item list

(** Prints a parstree *)
val print_ast : Parsetree.structure_item list -> unit


(** Converts a parsetree into a tl_ast *)
val ast_to_tl_ast : string  -> tl_ast

(** Prints a tl_ast *)
val print_tl_ast : tl_ast -> unit

(** Unit tests for the Tl_ast module*)
val unit_tests : unit -> unit


