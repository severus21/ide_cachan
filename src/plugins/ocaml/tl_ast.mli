(** This module contains functions allowing to manipulate ocaml top-level ast*)
(***)

(** method visibility inside class*)
type tl_visibility = Tl_private | Tl_public

(** Top-level stucture*)
type tl_struct =  
|Tl_none (*to be removed*)
|Tl_open of string list * string (** handle caml open module
                                   - name list, code
                                   - ex: open A.B -> ([A,B], open A.B) *)
|Tl_var of string * string  (** handle variable declaration, 
                              if the left-pattern is only one identifier
                              - name, code
                              - ex: let a = 0 -> (a,let a =1) *)                   
|Tl_fun of string * string (**handle function declaration
                             - name, code
                             - ex: let f x=x -> (f, let f x=x)*)
|Tl_exception of string * string (** handle exception declaration
                                   - name, code 
                                   - ex: exception E of int -> (E, exception E of int)*)
|Tl_type of string list *string (** handle type/rec-type/and-type declaration
                                   - names of types, is_rec, code
                                   - ex: type a=int -> ([a],type a=int)
                                   - ex: type a=int and b=float -> ([a;b],  type a=int and b=float)*)

|Tl_module of string * tl_ast (** handle module(not rec-module)
                                -* name, ast of the module*)

|Tl_sign of string * tl_ast (** handle module signature(not rec)
                              - name, declarations of types*)
|Tl_constraint of string * tl_struct * tl_struct (** handle module with signature
                                             -name, module, signature*)
|Tl_functor of string * string* tl_ast (** handle functor(warning thay are currified)
                                            -name, header, body
                                            -ex module F (T:Alpha) = struct .. end -> ("F", "F (T:Alpha)=", "...")                                   
 *)
|Tl_recmodule of tl_ast * string (** handle rec-module
                                   -module list, code*)
|Tl_class of {name:string; header:string; virt:bool;self:string option; elmts:class_elmt list} (** handle class  declaration
             - with params, with self but without and-class, withour inheritance, without type coercion
             - name: name of the class 
             - header: name and params (ex: class a f1 f2=object ... end -> clas a f1 f2=object)
             - virt: flag indicate the class is virtual or not
             - self: (ex: object ... end -> None | object(c) ... end ->Some(c) )
             - elmts: class components
 *)
|Tl_class_and of tl_struct list*string
(** class components *)
and class_elmt=
|Cl_method of tl_struct * tl_visibility
|Cl_attribut of tl_struct                            
|Cl_init of string (** describe intializer
                     - code *) 

(** Top-level ast type*)
and tl_ast = tl_struct list


(**Opens a file and returns its ocaml ast*)
val file_to_ast : string ->  Parsetree.structure_item list

(** Transform a string containing some ml code into a parsetree*)
val string_to_ast : string -> Parsetree.structure_item list

(** Prints a parsetree *)
val print_ast : Parsetree.structure_item list -> unit


(** Converts a parsetree into a tl_ast *)
val string_to_tl_ast : string  -> tl_ast

(** Prints a tl_ast *)
val print_tl_ast : tl_ast -> unit

(** Unit tests for the Tl_ast module*)
(*val unit_tests : unit -> unit*)
val quick_tl_ast: string -> tl_ast
val quick_tl_struct: string -> tl_struct 

                               

