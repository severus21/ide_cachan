open Tl_ast

(**Opens a file and returns its ocaml ast*)
val file_to_ast : string ->  Parsetree.structure_item list

(** Transform a string containing some ml code into a parsetree*)
val string_to_ast : string -> Parsetree.structure_item list

(** Prints a parsetree *)
val print_ast : Parsetree.structure_item list -> unit


(** Converts a parsetree into a tl_ast *)
val string_to_tl_ast : string  -> tl_ast

val tl_ast_to_str : tl_ast -> string

(** Prints a tl_ast *)
val print_tl_ast : tl_ast -> unit

(** Unit tests for the Tl_ast module*)
(*val unit_tests : unit -> unit*)
val quick_tl_ast: string -> tl_ast
val quick_tl_struct: string -> tl_struct 


