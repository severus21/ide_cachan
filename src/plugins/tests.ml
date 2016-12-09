open OUnit2

let tests ()= "Plugins">:::[
    Extract.tests ();
    Ocaml.Tests.tests ();
]
