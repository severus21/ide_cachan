(*open Dep_graph_build*)
open Gui.GuiMain
open Plugins.Factory

let () =
    load_plugins ();
    Printf.printf "%s\n" "*********************** IDE ***********************";
    main ()
(*let () = build_graph []*)
