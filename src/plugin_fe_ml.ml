open Core
open Tl_ast

module Plugin_ml : Plugin_fe.Plugin = struct
  let file_extensions  = [["ml";"mli"];["ml"]]

  let tl_struct_to_set struc (father:gset) = match struc with 
    |Tl_none -> failwith "obsolete"
    |Tl_open(open_ls,lign) -> ignore lign;
      begin
	match father#meta_data_sys#get_value "dependencies" with 
	|Some values -> father#meta_data_sys#add_tag "dependencies" ((TDepend open_ls)::values)
	|None -> father#meta_data_sys#add_tag "dependencies" [TDepend open_ls] 
      end
    |Tl_var(_,_) -> failwith "not implemented"
    |Tl_fun(_,_) -> failwith "not implemented"
    |Tl_exception(_,_) -> failwith "not implemented"
    |Tl_type(_,_) -> failwith "not implemented"
    |Tl_module(_,_)|Tl_class _|Tl_class_and (_,_) -> failwith "not implemented"

  (** "blub.ml" -> "Blub" *)
  let file_name_to_module_name f = 
    String.uppercase_ascii (String.sub f 0 1) ^ (String.sub f 1 (String.length f - 3))

  let tl_ast_to_set ast file_name= 
    let s: gset  = new set(file_name_to_module_name file_name) in 
    let rec browse_ast ast' = match ast' with
      |[] -> ()
      |struc::q -> tl_struct_to_set struc s; 
	browse_ast q
    in
    browse_ast ast;
    s


  let string_to_set ext files = 
    if List.length ext <> List.length files then
      failwith "The number of strings does not match with the number of extensions (plugin_fe_ml.ml)"
    else
      begin
	match ext with
	|["ml"] ->
	  begin 
	    let file_name = List.hd files in 
	    let ast = string_to_tl_ast (file_to_string file_name) in  
	    tl_ast_to_set ast file_name
	  end
	|["ml";"mli"] -> failwith "not implemented"
	| _ -> failwith "The plugin ml cannot import these file extensions (plugin_fe_ml.ml)" 
      end

  let set_to_string set_a = 
    ignore set_a;
    ""
end
