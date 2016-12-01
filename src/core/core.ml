type c_ast = Nil | Node of string * string * string * c_ast list

class ptr_ast (_ast:c_ast)=object
    val p_ast = ref _ast
  
    method ast= !(p_ast)             
end                      
                                           
