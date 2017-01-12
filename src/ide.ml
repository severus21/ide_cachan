open Gui.GuiMain
open Plugins.Factory

let () =
    Printf.printf "%s\n%!" "*********************** IDE ***********************";
    load_plugins ();
    List.iter
        (fun plugin -> Printf.printf "%s plugin loaded\n%!" plugin#name)
        (get_plugins ());
    main ()
(*let () = build_graph []*)
