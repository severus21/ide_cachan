type language = Ocaml 

let make_plg=function
    |Ocaml->Ocaml.Frontend.make_plg ()
