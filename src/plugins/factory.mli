(** Define generic API over plugins*)

(** List all languages supported by the plugins*)
type language = (*TODO add version indication?*)
|Ocaml    

(** Create a core plugin in order to interpret a specific language
    @param the program language needed
    @return the plugin corresponding to the language *)
val make_plg : language -> Core.Miscs.plug   
