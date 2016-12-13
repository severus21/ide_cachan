# Plugin documentation
## Plugins API
First Plugins will only export two sub-modules :
1. Factory : export API
2. Tests : export all tests functions

### API exported by API
* Programming languages supported : ```type language```
* ̀A function to make a Plugin.plug from a supported language
```     val make_plg : language -> Plugin.plug``` 

## Adding a new plugin for alpha language

### src/plugins/alpha.mlpack
```
alpha/Frontend
alpha/Tests
```

### src/plugins/tests.ml
add ``̀`Alpha.Tests.unittests (); ``` into unittests' list

### src/plugins/factory.ml
1. add ```Alpha``` to language( add it into factory.mli too)
2. add ```Alpha->Alpha.Frontend.make_plg ()``` into make_plg

### Alpha module
Where plugin's sources files live

```
mkdir src/plugins/alpha
```

#### Alpha.Frontend module
In file : src/plugins/alpha/frontend.mli
```
(** Export the Alpha plugin API *)
                                         
(** Create a core plugin in ordre to interpret Alpha 
    @return the plugin corresponding to Alpha*)                        
    val make_plg : unit->Plugin.plug
```

and make the ml file related

### Alpha.Tests module
In file : src/plugins/alpha/tests.mli

```
(** Aggregate all the unittests of the Alpha plugin*)
val unittests : unit -> OUnit2.test
```

and make the ml file related

### Advices for Alpha submodules
Each of them should export one :
```val unittests : unit -> OUnit2.test```

which will be aggregated in the Alpha.Tests implementation

## TODO
* Some automatic script to generate new plugin
