open OUnit2

let unittests ()= "Plugins">:::[
    Extract.unittests ();
    Ocaml.Tests.unittests ();
]
