let plugins_folder = "plugins";;

let plugins : Plugin.plug list ref = ref [];;

let register_plugin plug =
    plugins := plug::!plugins
;;

let get_plugins () =
    !plugins
;;

let load_plugins () =
    (* Tries to load a file, fails silently if it can't *)
    let try_load file =
        let path = Filename.concat plugins_folder file in
        try
            if Filename.check_suffix path "cmxs" then
                begin
                    Printf.printf "Loading plugin %s\n%!" path;
                    Dynlink.loadfile path
                end
        with
        | Dynlink.Error error ->
                Printf.printf "Error loading plugin: %s\n%!"
                (Dynlink.error_message error)
    in
    try
        if Sys.is_directory plugins_folder then
            begin
                let files = Sys.readdir plugins_folder in
                Array.iter try_load files
            end
    with
    | Sys_error msg -> Printf.printf "No plugins folder, %s" msg
;;
