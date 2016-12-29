(** This module contains  top level ast types used to simplify caml ast*)
(***)

(** Used when we can not express caml ast in tl ast*)
exception Not_define of string

(** Used when tl ast is corrupted*)
exception Bad_tl_ast of string  

(** Panic function to raise Not_define*)                       
val not_define : string->'a

(** Panic function to raise bad_ast*)                       
val bad_tl_ast : string ->'a                           
   

(** method visibility inside class*)
type tl_visibility = Tl_private | Tl_public

(** Top-level stucture*)
type tl_struct =  
|Tl_none (*to be removed*)
|Tl_open of string list * string 
(** handle caml open module
- name list, code
- ex: open A.B -> ([A,B], open A.B) *)

|Tl_var of string * string  
(** handle variable declaration, 
if the left-pattern is only one identifier
- name, code
- ex: let a = 0 -> (a,let a =1) *)     

|Tl_constraint of string * string
(** handle inline constraint
- nam, constraint
- val t:int -> ("t", "int")i*)

|Tl_fun of string * string 
(**handle function declaration
- name, code
- ex: let f x=x -> (f, let f x=x)*)

|Tl_exception of string * string 
(** handle exception declaration
- name, code 
- ex: exception E of int -> (E, exception E of int)*)

|Tl_type of string list *string 
(** handle type/rec-type/and-type declaration
- names of types, is_rec, code
- ex: type a=int -> ([a],type a=int)
- ex: type a=int and b=float -> ([a;b],  type a=int and b=float)*)

|Tl_module of string * tl_ast 
(** handle module(not rec-module)
- name, ast of the module*)

|Tl_sign of string * tl_ast 
(** handle module signature(not rec)
- name, declarations of types*)

|Tl_module_constraint of string * tl_struct * tl_struct 
(** handle module with signature
- name, module, signature*)

|Tl_functor of string * string* tl_ast 
(** handle functor(warning thay are currified)
- name, header, body
- ex module F (T:Alpha) = struct .. end -> ("F", "F (T:Alpha)=", "...")*)     

|Tl_recmodule of tl_ast * string 
(** handle rec-module
- module list, code*)
                            
|Tl_class of {
    name:string; 
    header:string; 
    virt:bool;
    self:string option; 
    elmts:class_elmt list; 
    c_elmts:class_elmt list} 
(** handle class  declaration
- with params, with self but without and-class, withour inheritance, without type coercion
- name: name of the class 
- header: name and params (ex: class a f1 f2=object ... end -> clas a f1 f2=object)
- virt: flag indicate the class is virtual or not
- self: (ex: object ... end -> None | object(c) ... end ->Some(c) )
- elmts: class components
- c_elmts: constraint on class componnts      
*)

|Tl_class_and of tl_struct list*string

(** class components *)
and class_elmt=
|Cl_method of tl_struct * tl_visibility
(** describe a class method
- code, private/public*)  

|Cl_attribut of tl_struct   
(** describe a class attribut
- code*)

|Cl_init of string 
(** describe intializer
- code *)

|Cl_inherit of string * string option
(** describe class inheritance
- parent class, label 
ex: inherit Mordor as super -> Cl_inherit("Mordor", Some("super"))*)  

(** Top-level ast type*)
and tl_ast = tl_struct list

               
