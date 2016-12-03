type c_ast = Nil| Node of string * string * string * c_ast list

class ptr_ast x: object
    val p_ast: c_ast ref
    
    method ast : c_ast   
end = object
    val p_ast = ref Nil
    method  ast = Nil             
end 
