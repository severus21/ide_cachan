open Core

module Plugin_ml : Plugin_fe.Plugin = struct
  let file_extensions  = [["ml";"mli"];["ml"]]

  let string_to_set ext files = 
    if List.length ext <> List.length files then
      failwith "The number of strings does not match with the number of extensions (plugin_fe_ml.ml)"
    else
      begin
	match ext with
	|["ml"] ->
	  begin 
	    let ml_str = List.hd files in 
	    let ast = Tl_ast.ast_to_tl_ast ml_str in 
	    ignore ast;
	    new set("blub")
	  end
	|["ml";"mli"] -> new set("blub")
	| _ -> failwith "The plugin ml cannot import these file extensions (plugin_fe_ml.ml)" 
      end

  let set_to_string set_a = 
    ignore set_a;
    ""
end
