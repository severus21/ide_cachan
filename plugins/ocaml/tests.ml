open OUnit2

let unittests ()=
    "Ocaml">::: [Ml_to_tl.unittests(); Tl_to_c.unittests()]

