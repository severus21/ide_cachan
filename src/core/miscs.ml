type c_node = Nil | Node of {name:string;header:string;body:string ref;children:c_ast}
and c_ast = c_node list                              

class ptr_ast (_ast:c_ast)=object
    val p_ast = ref _ast
  
    method ast= !(p_ast)             
end                      

let c_ast_to_str=function
|[] -> ""
|_-> "Node"

let print_c_ast ast= Printf.printf "%s\n" (c_ast_to_str ast)            

            
