(** Define an abstract representation of a file
  - name of structure, header of structure, code of structure, and children 
* *)
type c_ast = Nil| Node of string * string * string * c_ast list


(** To reference a piece of c_ast, we can copy without duplicated part of ast 
*)
class ptr_ast : c_ast -> object
    val p_ast: c_ast ref
    
    method ast : c_ast   
end                      
                                          
