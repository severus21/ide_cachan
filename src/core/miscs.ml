open Gset




(** Returns whether a is a subword of b *)
let is_subword a b =
    let current = ref 0 in
    let iter c =
        current := (String.index_from b !current c) + 1
    in
    try
        String.iter iter a;
        true
    with
    | Not_found -> false
;;



let counter_miscs = ref 0

exception Fonction_not_exist
exception Not_compliant




(**Description of a c_ast*)


type node_internal =  {name:string;header:string;body:string ref;children:c_ast; meta :gset tags} 
and c_node =
|Nil 
|Node of node_internal
and c_ast = c_node list 




(** Fill a hashtable with the name of the function and their c_node associated *)
let rec fill_table_node table = function
  |Nil -> ()
  |Node a as s->
    begin
      let b = ref s in
      Hashtbl.add table (a.name) b;
      fill_table_ast table (a.children) 
    end

and fill_table_ast table c_ast = List.iter (fill_table_node table) c_ast
   
                      
(**Class that contains the ast*)
class ptr_ast (_ast:c_ast) = object
  val p_ast = ref _ast
    
  method ast= !(p_ast)             

end   

                   

(**Table that contained the name of the node and the node associated *)
class table = object
  val table = Hashtbl.create 50
  method give_table () = table
  method fill_table c_ast = fill_table_ast table c_ast


   
(** Research all the c_node associated to the subword "name"*)       
  method potential_name subword = 
    let table1 = Hashtbl.copy table in 
    let aux x _ = 
      if not (is_subword subword x) then Hashtbl.remove table1 x
    in
    Hashtbl.iter aux table1;
    table1

(** Research all the c_node associated to the function "name"*)
  method research name =
    if Hashtbl.mem table name then raise Fonction_not_exist;
    Hashtbl.find_all table name 

end



(** Write into a File the description of the c_ast*)
let to_file name_file c_ast =
    let rec to_file_aux file =function
      |Nil -> Printf.fprintf file "Nil\n";
      |Node node ->
        begin
          Printf.fprintf file "Node %d\n" ( ! counter_miscs);
          Printf.fprintf file "name:\n";
          Printf.fprintf file "%S\n" node.name;
          Printf.fprintf file "header:\n";
          Printf.fprintf file "%S\n" node.header;
          Printf.fprintf file "body:\n"; 
          Printf.fprintf file "%S\n" (! (node.body));
          Printf.fprintf file "children:\n";
          Printf.fprintf file "Ast:\n";
          List.iter (to_file_aux file) node.children;
          Printf.fprintf file "FAst\n";
          Printf.fprintf file "Fchildren\n";
          Printf.fprintf file "FNode %d\n" ( ! counter_miscs);
        end
    in
    let file = open_out name_file in
    Printf.fprintf file ("Ast:\n");
    List.iter (to_file_aux file) c_ast;
    Printf.fprintf file ("FAst\n");
    close_out file




(** Write into a File the description of the c_ast*)

let from_file_name name name_file =
  let word = Scanf.bscanf name_file "%s " (fun x -> x) in
  if word <> name then raise Not_compliant;
  Scanf.bscanf name_file "%S " (fun x-> x)


(** Read in a File the description of the c_ast and create the c_ast associated *)



let rec from_file_children name_file fin =
  let word = Scanf.bscanf name_file "%s " (fun x->x) in
  if word <> "children:" then raise Not_compliant;
  let l = from_file_c_node name_file in
  let word,word2,word3 = Scanf.bscanf name_file "%s %s %d " (fun x y z->(x,y,z)) in
  if (word <>"Fchildren") ||(word2 <> "FNode")||(word3 <>fin) then raise Not_compliant;
  l



    
and from_file_node name_file fin =
  let name = (from_file_name "name:" name_file) in
  let header = (from_file_name "header:" name_file) in
  let body = ref (from_file_name "body:" name_file) in
  let children = from_file_children name_file fin in
  {name = name; header = header; body = body ;children = children; meta = new tags} 



and from_file_c_node name_file = 
  let rec aux l=
    let word = Scanf.bscanf name_file "%s " (fun x->x) in
    match word with
    |"FAst" -> List.rev l 
    | "Nil" -> aux (Nil::l)
    | "Node"->  
      begin
        let i = Scanf.bscanf name_file "%d " (fun x->x) in
        aux (( Node ( from_file_node name_file i)) :: l ) 
      end
    |_-> raise Not_compliant
  in  
  let word = Scanf.bscanf name_file "%s\n" (fun x->x) in
  if word = "Ast:" then aux []
  else raise Not_compliant




let main_from_file name_file =
  let file = Scanf.Scanning.open_in name_file in
  let l = try from_file_c_node file with Scanf.Scan_failure _ -> raise Not_compliant in
  Scanf.Scanning.close_in file;
  new ptr_ast (l)



(** Equality function on a c_ast*)


let rec c_equal c_ast1 c_ast2 =
  match c_ast1,c_ast2 with 
  |[],[]-> true
  |x::h,y::t-> (aux_equal x y) && (c_equal h t)
  |_,_->false

and aux_equal c_node1 c_node2 = 
  match c_node1, c_node2 with
  |Nil,Nil -> true
  |Node a, Node b -> (a.name = b.name) &&
    (a.header = b.header)&&
    (a.body = b.body)&&
    (c_equal (a.children) (b.children))&&
    (a.meta#equal (b.meta))
  |_,_-> false






(**Print the ast*)




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



(**Tests*)
open OUnit2

let test_fill_table () = 
  let node1 = Node {name = "a"; header = "a1"; body = ref "a2"; children = [Nil]; meta = new tags} in 
  let node2 = Node {name = "b"; header = "b1"; body = ref "b2"; children = [Nil]; meta = new tags} in 
  let node3 = Node {name = "c"; header = "c1"; body = ref "c2"; children = [node1; node2; Nil]; meta = new tags} in
  let table = new table in
  table#fill_table [node3];
  let table2 = Hashtbl.create 50 in
  Hashtbl.add table2 "c" (ref node3);
  Hashtbl.add table2 "a" (ref node1); 
  Hashtbl.add table2 "b" (ref node2);  
  assert_equal (table#give_table ()) table2 


let test_subword () =  
  let node1 = Node {name = "arbre"; header = "a1"; body = ref "a2"; children = [Nil]; meta = new tags} in 
  let node2 = Node {name = "feuille"; header = "b1"; body = ref "b2"; children = [Nil]; meta = new tags} in 
  let node3 = Node {name = "insecte"; header = "c1"; body = ref "c2"; children = [node1; node2; Nil]; meta = new tags} in
  let table = new table in
  table#fill_table [node3];
  let table2 = Hashtbl.create 50 in
  let table3 = Hashtbl.create 50 in
  Hashtbl.add table2 "arbre" (ref node1);
  Hashtbl.add table3 "feuille" (ref node2);
  Hashtbl.add table3 "insecte" (ref node3);
  assert_equal( table#potential_name "a" ) table2;
  assert_equal( table#potential_name "ie") table3

let test_subword2 () =  
  let node1 = Node {name = "arbre"; header = "a1"; body = ref "a2"; children = [Nil]; meta = new tags} in 
  let node2 = Node {name = "arbre"; header = "b1"; body = ref "b2"; children = [Nil]; meta = new tags} in 
  let node3 = Node {name = "insecte"; header = "c1"; body = ref "c2"; children = [node1; node2; Nil]; meta = new tags} in
  let table = new table in
  let table2 = Hashtbl.create 50 in
  table#fill_table [node3];
  Hashtbl.add table2 "arbre" (ref node1);
  Hashtbl.add table2 "arbre" (ref node2);
  assert_equal( table#potential_name "arbre") table2


let test_read_write_file () =
  let node1 = Node {name = "arbre"; header = "a1"; body = ref "a2"; children = [Nil]; meta = new tags} in 
  let node2 = Node {name = "feuille"; header = "b1"; body = ref "b2"; children = [Nil]; meta = new tags} in 
  let node3 = Node {name = "insecte"; header = "c1"; body = ref "c2"; children = [node1; node2; Nil]; meta = new tags} in 
  let c_ast = [node3;node1] in
  let name_file = "love" in
  to_file name_file c_ast;
  let c_ast2 = (main_from_file name_file)#ast in
  assert_equal (List.length c_ast2) 2;
  assert_equal (c_equal c_ast2 c_ast) true


let unittests ()=
  "Miscs">:::[
    "Test_fill_table:">::(function _-> test_fill_table());
    "Test_subword:">::(function _-> test_subword());
    "Test_subword2:">::(function _-> test_subword2());
    "Test_read_write_file:">::(function _-> test_read_write_file())
  ]
