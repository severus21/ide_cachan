open Gset

type c_node =
|Nil 
|Node of {name:string;header:string;body:string ref;children:c_ast;meta:gset metaData} 
and c_ast = c_node list                              

class ptr_ast (_ast:c_ast)=object
    val p_ast = ref _ast
  
    method ast= !(p_ast)             
end                      

let rec c_node_to_str tab=function
|Nil -> ""
|Node node ->(
    Format.sprintf "%sname:%s;header:%s;body:\n%s%s\nchildren:[\n%s%s]" tab node.name node.header tab !(node.body) tab (c_ast_to_str (tab^"\t") node.children )
)   
and c_ast_to_str tab ast=String.concat "" (List.map (c_node_to_str tab) ast)

let print_c_ast ast= Printf.printf "%s\n" (c_ast_to_str "" ast)            

            
