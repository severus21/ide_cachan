open Gui
open Tl_ast

let () =
  let tl = get_ast "src/ide.ml" and s = file_to_string "src/ide.ml" in 
  (*print_ast tl;*)
  print_tl_ast (ast_to_tl_ast s tl);
  gui ()
