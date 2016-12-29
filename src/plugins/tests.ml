open OUnit2

open Factory  

let unittests ()= 
    load_plugins();  
    "Plugins">:::[
        "plugin_system">:::[
            Extract.unittests ();
        ];
        "plugs">:::
        List.map (fun plugin ->plugin#unittests ()) (get_plugins ())
    ]    
