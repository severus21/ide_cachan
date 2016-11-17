open Dep_graph_build
open Gui

let () =
    Printf.printf "%s\n" "*********************** IDE ***********************";
gui ()
let () = build_graph []
