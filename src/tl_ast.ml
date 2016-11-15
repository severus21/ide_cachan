open Parsetree

(** top-level structures*)
type tl_struct =  
|Tl_none (*to be removed*)
|Tl_open of string (*for the moment , we only extract  the complete line of the openning*)
|Tl_var of string * string
|Tl_fun of string * string
|Tl_exception of string * string
|Tl_type of (string * string) list

(** Top-level ast type*)
type tl_ast = tl_struct list


(*let file_to_string f = 
  let c = open_in f and s = ref "" and continue = ref true in
  while !continue do 
    try 
      s := !s ^ ((input_line c)^ "\n")
    with 
    |End_of_file -> continue := false
  done;
  !s*)

let file_to_string path=
	let input = open_in path in
	really_input_string input (in_channel_length input)

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
    |Pstr_value(rec_flag,  value::t)->((*pour l'instant on ne traite que la première*)
        match value.pvb_pat.ppat_desc  with 
        |Ppat_var loc->(
            let name = loc.txt and expr = (get_str_from_location ml 
                                             value.pvb_expr.pexp_loc) in
            match value.pvb_expr.pexp_desc with
            | Pexp_function _ | Pexp_fun _-> Tl_fun(name, expr)   
            | _ -> Tl_var(name, expr)   
        )(*comment faire avec les autrs patterns???*)
        |_->Tl_none                  
    )
    |Pstr_exception e-> Tl_exception( e.pext_name.txt, get_str_from_location ml e.pext_loc )                                   
    |Pstr_type  decls->Tl_type( List.map( 
        function decl ->
        (decl.ptype_name.txt, get_str_from_location ml decl.ptype_name.loc)
       ) decls )
    |_ -> Tl_none
  end

(*ml is a string containing the whole file from which ast was created *)
let ast_to_tl_ast ml ast = List.map (struct_to_tl_struct ml) ast


(* ***END Conversion from ast to tl_ast*)
  

(* ***BEGIN Printing of a tl_ast*)

let tl_struct_to_str tl_s = match tl_s with 
    |Tl_open s-> Format.sprintf "%s\n" s
    |Tl_var(name, expr)->Format.sprintf "let %s=%s\n" name expr 
    |Tl_fun(name, expr)->Format.sprintf "letfun %s %s\n" name expr
    |Tl_exception(name, values)->Format.sprintf "exception %s : %s" name values        |Tl_type values->(
        List.fold_left (fun head (name,value)-> Format.sprintf "%s type %s : %s\n" head name value) "" values   
    )
    |_ -> ""

let tl_ast_to_str tl = String.concat "" (List.map tl_struct_to_str tl)


    

let print_tl_ast tl = Printf.printf "%s\n" (tl_ast_to_str tl)

(* ***END Printing of a tl_ast*)