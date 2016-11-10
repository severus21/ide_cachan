open GMain
open GdkKeysyms

let locale = GtkMain.Main.init ();;

let notimp_callback = (fun () -> prerr_endline "Not Implemented");;

(* Pops up a "Do you want to quit" dialog and returns the answer *)
let confirm_quit () =
    let dialog = GToolbox.question_box
        "Confirm"
        ["Yes";"No"]
        "Do you really want to quit ?" in
    match dialog with
    | 0 | 2 -> false
    | 1 -> true
    | _ -> false
;;

let gui () =
    let window = GWindow.window ~resizable:true ~width:1280 ~height:720
                                    ~title:"Awesome Ocaml IDE"
                                    ~position:`CENTER () in
    let vbox = GPack.vbox ~packing:window#add () in
    ignore (
        window#event#connect#delete
        ~callback: (fun _ -> false(*not (confirm_quit ())*))
    );
    ignore (window#connect#destroy ~callback: Main.quit);

    (* Menu bar *)
    let menubar = GMenu.menu_bar ~packing:vbox#pack () in
    let factory = new GMenu.factory menubar in
    let accel_group = factory#accel_group in
    let file_menu = factory#add_submenu "File" in
    let test_menu = factory#add_submenu "Test" in

    (* File menu *)
    let factory = new GMenu.factory file_menu ~accel_group in
    ignore(factory#add_item "Import" ~key:_I ~callback: notimp_callback);
    ignore(factory#add_item "Export" ~key:_E ~callback: notimp_callback);
    ignore(factory#add_item "Quit" ~key:_Q
        ~callback: (fun () -> if confirm_quit () then Main.quit ()));

    (* Test menu *)
    let factory = new GMenu.factory test_menu ~accel_group in
    ignore(factory#add_item "Hello" ~key:_H
        ~callback: (fun () -> prerr_endline "Hello"));

    (* Navlist *)
    ignore(new GuiNavlist.navlist ~packing:vbox#add);

    (* Frame *)
    let frame = GBin.frame ~label:"Code" ~packing:vbox#add () in

    (* Text view *)
    ignore(GText.view ~packing:frame#add ());

    (* Display the windows and enter Gtk+ main loop *)
    window#add_accel_group accel_group;
    window#show ();
    Main.main ()
;;

let () =
    ignore(locale)
;;
