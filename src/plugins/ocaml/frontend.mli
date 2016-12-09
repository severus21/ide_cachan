(** Export the Ocaml plugin API *)
                                         
(** Create a core plugin in ordre to interpret Ocaml
    @return the plugin corresponding to Ocaml*)                        
val make_plg : unit->Core.Miscs.plug
