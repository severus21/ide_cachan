open Tl_ast
open Plugin_fe_ml
open Dep_graph_build

let () =
 (* let tl = file_to_ast "src/test_ast.ml" and s = file_to_string "src/test_ast.ml" in 
  
  print_ast tl;
  print_tl_ast (string_to_tl_ast s );*)
  ignore (Plugin_ml.file_extensions);
  unit_tests ()
let () = build_graph []
