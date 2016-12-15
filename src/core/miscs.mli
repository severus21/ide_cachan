open Gset

(** Define an abstract representation of a file
  - name of structure, header of structure, code of structure, and children*)



exception Fonction_not_exist
exception Not_compliant

 
type node_internal =
  {name:string;header:string;body:string ref;children:c_ast;meta:gset tags}
and c_node = 
|Nil
|Node of node_internal
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


class type table = object
  val table : (string,c_node ref) Hashtbl.t
  
  method fill_table : c_ast -> unit
   
(* Research all the c_node associated to the subword "name"*)       
  method potential_name : string -> (string, c_node ref) Hashtbl.t

(* Research all the c_node associated to the function "name"*)
  method research : string -> c_node ref list 

end


val to_file : string->c_ast->unit



val main_from_file : string -> ptr_ast 
 






