exception Not_define of string
let not_define msg = raise (Not_define( "src/tl_ast.ml : "^msg^"\n" )) 
exception Bad_tl_ast of string
let bad_tl_ast msg = raise (Bad_tl_ast( "src/tl_ast.ml : "^msg^"\n" )) 

type tl_visibility = Tl_private|Tl_public
 
type tl_struct =  
|Tl_none 
|Tl_open of string list * string  
|Tl_var of string * string
|Tl_constraint of string * string
|Tl_fun of string * string
|Tl_exception of string * string
|Tl_type of string list * string
|Tl_module of string * tl_ast
|Tl_sign of string * tl_ast
|Tl_module_constraint of string * tl_struct* tl_struct
|Tl_functor of string * string * tl_ast                                      
|Tl_recmodule of tl_ast * string                                        
|Tl_class of {
    name: string; 
    header:string; 
    virt: bool; 
    self: string option; 
    elmts: class_elmt list; 
    c_elmts: class_elmt list}
|Tl_class_and of tl_struct list * string
and class_elmt=
|Cl_method of tl_struct * tl_visibility
|Cl_attribut of tl_struct                  
|Cl_init of string
|Cl_inherit of string * string option               
and tl_ast = tl_struct list

