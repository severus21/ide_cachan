open Parsetree

(** top-level structures*)
type tl_struct =  
|Tl_none (*to be removed*)
|Tl_open of string*string (*for the moment , we only extract the name of the module opened
			   *and we give the complete line of the openning*)

(** Top-level ast type*)
type tl_ast = tl_struct list


let file_to_string f = 
  let c = open_in f and s = ref "" and continue = ref true in
  while !continue do 
    try 
      s := !s ^ ((input_line c)^ "\n")
    with 
    |End_of_file -> continue := false
  done;
  !s


let get_ast f = 
  Parse.implementation (Lexing.from_channel (open_in f)) 


let print_ast ast = 
  Printast.implementation (Format.formatter_of_out_channel stdout) ast



(* ***BEGIN Miscallaneous functions on locations*)


(* ***END Miscallaneous functions on locations*)


(* ***BEGIN Conversion from ast to tl_ast*)


let struct_to_tl_struct ml  = function {pstr_desc = struct_item; pstr_loc = _} ->
  begin
    ignore ml;
    match struct_item with 
    |Pstr_open _ -> Tl_open("blub","blab")
    |_ -> Tl_none
  end

(*ml is a string containing the whole file from which ast was created *)
let ast_to_tl_ast ml ast = List.map (struct_to_tl_struct ml) ast


(* ***END Conversion from ast to tl_ast*)
  

(* ***BEGIN Printing of a tl_ast*)
let print_tl_struct tl_s = match tl_s with 
  |Tl_open(s1,s2)-> Printf.printf "%s %s\n" s1 s2
  |_ -> ()
    

let print_tl_ast tl = List.fold_left (fun () -> print_tl_struct) () tl


(* ***END Printing of a tl_ast*)
