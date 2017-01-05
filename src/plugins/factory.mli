
(** Used by plugins to register themselves
    - param1 the plug reprensenting the plugin to register *)
val register_plugin : Plugin.plug -> unit

(** Returns the list of currently loaded plugins
    @return the list of loaded plugins *)
val get_plugins : unit -> Plugin.plug list

(** Searches for plugins and load them dynamically *)
val load_plugins : unit -> unit
