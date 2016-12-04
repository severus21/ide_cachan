open Gset

type c_node =
|Nil 
|Node of {name:string;header:string;body:string ref;children:c_ast;meta:gset metaData} 
and c_ast = c_node list                              

class ptr_ast (_ast:c_ast)=object
    val p_ast = ref _ast
  
    method ast= !(p_ast)             
end                      

exception Bad_cnode of string
let bad_cnode str = raise (Bad_cnode str)

let rec c_node_to_str tab=function
|Nil -> Format.sprintf "%sNil" tab
|Node node ->(
    Format.sprintf "\n%sNode({name:%s;header:%s;children:[\
        %s%s]\n%smeta:%s})\n" tab node.name node.header 
        (c_ast_to_str tab node.children ) (match node.children with|[]->""|_->tab) tab node.meta#to_string
)   
and c_ast_to_str tab ast=String.concat "" (List.map (c_node_to_str (tab^"\t")) ast)

let print_c_ast ast= Printf.printf "%s\n" (c_ast_to_str "" ast)            

            
