(*open Dep_graph_build*)
open Gui.GuiMain
open Plugins.Factory

let () =
    load_plugins ();
    List.iter
        (fun plugin -> Printf.printf "%s plugin loaded\n%!" plugin#name)
        (get_plugins ());
    Printf.printf "%s\n" "*********************** IDE ***********************%!";
    main ()
(*let () = build_graph []*)
