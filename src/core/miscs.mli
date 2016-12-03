(** Define an abstract representation of a file
  - name of structure, header of structure, code of structure, and children 
* *)
type c_node = Nil| Node of {name:string;header:string;body:string ref;children:c_ast}
and c_ast = c_node list

(** To reference a piece of c_ast, we can copy without duplicated part of ast 
*)
class ptr_ast : c_ast -> object
    val p_ast: c_ast ref
    
    method ast : c_ast   
end                      

val c_ast_to_str : c_ast -> string
val print_c_ast : c_ast->unit                              
