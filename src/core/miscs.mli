open Gset

(** Define an abstract representation of a file
  - name of structure, header of structure, code of structure, and children*) 
type c_node = 
|Nil
|Node of {name:string;header:string;body:string ref;children:c_ast;meta:gset tags}
and c_ast = c_node list

exception Bad_cnode of string
val bad_cnode:string->'a                         

(** To reference a piece of c_ast, we can copy without duplicated part of ast 
*)
class ptr_ast : c_ast -> object
    val p_ast: c_ast ref
    
    method ast : c_ast   
end                      

val print_c_ast : c_ast->unit                              
