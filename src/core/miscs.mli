open Gset

(** It have differents functonalities :
    - Define an abstract representation of a file : 
    name of structure, header of structure, code of structure, and children
    - Write in a file a ast and recreate a ast from a file
    - Permit to make a research by name in the ast
*)


(** Exception that indicate the research function do not exist *)
exception Fonction_not_exist



type node_internal =
  {name:string;header:string;body:string ref;children:c_ast;meta:gset tags}
and c_node = 
|Nil
|Node of node_internal

(**Description of a c_ast*) 
and c_ast = c_node list




(** Class that contains a ast. 
    To reference a piece of c_ast, we can copy without duplicated part of ast 
*)
class ptr_ast : c_ast -> object
    val p_ast: c_ast ref
    
    method ast : c_ast   
end           

                                  

(** Class Table that contained the name of the node and the node associated *)
class type table = object
  
  (**Hashtable that contains a description of a ast *)
  val table : (string,c_node ref) Hashtbl.t
 
  (**  Return the value table of the class table*)
  method give_table : unit ->(string,c_node ref) Hashtbl.t 

  (**Take a ast and fill the table : for each node, 
     it adds the name of the node associated to the reference  of the node*)
  method fill_table : c_ast -> unit
   
(**  Take in enter a string, name and return a hashtable of all the nodes associated a subword of the name*)       
  method potential_name : string -> (string, c_node ref) Hashtbl.t

(** Take in enter a string, name and return a list of all the nodes associated to the name*)
  method research : string -> c_node ref list 

end

(** Function that take a name of a file and a ast and write in a file 
    the ast corresponding to the contains of the file.
*)
val to_file : string->c_ast->unit



(** Exception that indicate the file read do
    not have the suitable structure *)
exception Not_compliant

(** Function that take a name of a file and return
    the ast corresponding to the contains of the file.
    It fails with the exception Not_compliant if the file
    do not have the suitable structure  *)
val main_from_file : string -> ptr_ast 


(**Exception for the node not well built*)
exception Bad_cnode of string

(**Function to raise a exception Bad_node*)
val bad_cnode:string->'a           

(**Function that print the ast in the enter*)
val print_c_ast : c_ast->unit       




(**Unittests*)
val unittests : unit -> OUnit2.test




