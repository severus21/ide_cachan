open Gset

let counter_miscs = ref 0 

type c_node =
|Nil 
|Node of {name:string;header:string;body:string ref;children:c_ast;meta:gset metaData} 
and c_ast = c_node list                              

class ptr_ast (_ast:c_ast)=object
    val p_ast = ref _ast
  
    method ast= !(p_ast)             

(*remettre les méthode pour string*)
    method to_file name_file =
      let rec to_file_aux =function
        |Nil -> output_string name_file ("Nil:\n");
        |Node node ->
          begin
            output_string name_file ("Node "^string_of_int( ! counter_miscs)^":\n");
            output_string name_file ("name:"^node.name^"\n");
            output_string name_file ("header:"^node.header^"\n");
            output_string name_file ("body:"^(! (node.body))^"\n");
            output_string name_file "children:\n";
            List.iter to_file_aux node.children;
            output_string name_file "tags_node:\n";
(*node.tags#to_file name_file*)
            output_string name_file ("FNode "^string_of_int( ! counter_miscs)^"\n");
          end
      in
      output_string name_file ("Ast:\n");
      List.iter to_file_aux (! p_ast);
      output_string name_file ("FAst\n")
           
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

            
let print a = Printf.printf "%s" a
  
let node1 = Nil
let body = ref "ok"
let node2 = Node {name= "bla";header="bla dodo";body = body;children=[node1]}
let node3 = new ptr_ast ([node1;node2])
let () = 
  let file = open_out "love" in
  node3#to_file file;
  close_out file;
  let file = open_in "love" in
  let a = pos_in file in
  print (input_line file);
  print (input_line file);
  seek_in file a;
  print (input_line file)
