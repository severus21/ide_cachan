open Parsetree

(** top-level structures*)
type tl_struct =  
|Tl_none (*to be removed*)
|Tl_open of string (*for the moment , we only extract  the complete line of the openning*)

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



(* ***BEGIN Miscellaneous functions on locations*)
(**Renvoie la sous chaîne de ml qui correspond à loc *)
let get_str_from_location ml = function {Location.loc_start = s ; loc_end = e; loc_ghost = _}  ->
  let cs = s.Lexing.pos_cnum and ce = e.Lexing.pos_cnum in 
  String.sub ml cs (ce - cs)

  


(* ***END Miscellaneous functions on locations*)


(* ***BEGIN Conversion from ast to tl_ast*)


let struct_to_tl_struct ml  = function {pstr_desc = struct_item; pstr_loc = loc} ->
  begin
    match struct_item with 
    |Pstr_open _-> Tl_open (get_str_from_location ml loc)
    |_ -> Tl_none
  end

(*ml is a string containing the whole file from which ast was created *)
let ast_to_tl_ast ml ast = List.map (struct_to_tl_struct ml) ast


(* ***END Conversion from ast to tl_ast*)
  

(* ***BEGIN Printing of a tl_ast*)

let tl_struct_to_str tl_s = match tl_s with 
  |Tl_open s-> Format.sprintf "%s\n" s
  |_ -> ""

let tl_ast_to_str tl = String.concat "" (List.map tl_struct_to_str tl)


    

let print_tl_ast tl = Printf.printf "%s\n" (tl_ast_to_str tl)

(* ***END Printing of a tl_ast*)
