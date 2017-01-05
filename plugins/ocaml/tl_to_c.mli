(**This module contains functions to import/export tl_ast from/to c_ast *)

(** Convert a tl_ast to c_ast
    - param1 ast a tl_ast
    @return the c_ast related to ast
    @raise Bad_tl_ast if tl_ast represents a corrupted ocaml code*)
val tl_ast_to_c_ast : Tl_ast.tl_ast->Core.Miscs.c_ast

(** Convert a c_ast to tl_ast, it is the reverse operation of tl_ast_to_c_ast
    - param1 ast a c_ast
    @return the tl_ast related to ast
    @raise Not_define if internal_node represents a corrupted ocaml code*)
val c_ast_to_tl_ast : Core.Miscs.c_ast->Tl_ast.tl_ast

(** Generate unittests for this module*)               
val unittests : unit -> OUnit2.test 
