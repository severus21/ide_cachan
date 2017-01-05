# Plugin documentation
## Plugins API
First Plugins will only export two sub-modules :
1. Factory : export API
2. Tests : export all tests functions

### API exported (in module Factory)
* ```register_plugin : Plugin.plug -> unit``` used by plugins to register themselves
* ```load_plugins : unit -> unit``` loads plugins from the hardcoded folder in ```factory.ml```
* ```get_plugins : unit -> Plugin.plug list``` returns the list of currently loaded plugins

## Adding a new plugin for alpha language

### plugins/alpha

This folder contains all the code for the plugin except for modules from the IDE such as Factory or Core.
At least one of the source files should contain
```ocaml
let () =
  Factory.register_plugin yourplugin
;;
```

#### Makefile : long version
You will need a makefile with ```default```, ```clean``` and ```doc``` targets that output a
```plugin.cmxa``` file (with ```-shared``` passed to ```ocamlopt```).
#### TL;DR : copy Makefile from the ocaml plugin, should work as-is
