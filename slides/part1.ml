type node_internal =  {name:string;header:string;
    body:string ref;children:c_ast; meta :gset tags} 
and c_node =
|Nil 
|Node of node_internal
and c_ast = c_node list
