open OUnit2
open Tl_ast

let make_suite name suite =
    name >::: (List.map( function (name,ml,tl_struct)->
        name>::function _-> assert_equal (Tl_ast.quick_tl_struct ml) tl_struct
    ) suite)

let make_suites name suites =
    name >::: (List.map( function (name,suite)->  make_suite name suite)suites) 

let test_suites ()=
    let ml_forest_t = "type 'a tree=Nil|Node of 'a tree*'a tree*'a and \
    'a forest='a tree list" in
    let ml_hello_c =  "class hello = object(self) val hello:string=\"hello\" \
    end" in(*
    let ml_hello_m =  "module Hello = struct \
        let message = \"Hello\" \
        let hello () = print_endline message \
    end" in*)
    let ml_even_m = "module Even = struct \
        type t = Zero | Succ of int \
        let alpha = Zero \
        let hello () = print_endline \"Even\" \
    end" in
    let ml_even_s = "module type Even = sig \
        type t = Zero | Succ of int \
        val alpha : t \
    end" in             
    let ml_even_c = "module Even : sig \
        type t = Zero | Succ of int \
        val alpha : t \
    end = struct \
        type t = Zero | Succ of int \
        let alpha = Zero \
        let hello () = print_endline \"Even\" \
    end" in

[ 
    ("suite_open", [
        ("default", "open A.B", Tl_open(["A";"B"],"open A.B"))
    ]);
    ("suite_var", [
        ("default", "let a = 3", Tl_var("a","let a = 3"));
        ("ref", "let a = ref [\"test\"]", Tl_var("a", "let a = ref [\"test\"]"))
    ]);
    ("suite_fun", [
        ("default", "let f x = x", Tl_fun("f", "let f x = x"));
        ("function",  "let f = function x -> x", Tl_fun("f", 
            "let f = function x -> x"));
        ("fun", "let f = fun x y-> x+y", Tl_fun("f", "let f = fun x y-> x+y"))
    ]);
    ("suite_exception", [
        ("default", "exception E of string", Tl_exception("E", 
              "exception E of string"))
    ]);
    ("suite_type", [
        ("default", "type 'a tree =Nil |Node of 'a tree * 'a tree * 'a", 
        Tl_type(["tree"],"type 'a tree =Nil |Node of 'a tree * 'a tree * 'a"));
        ("multi rec", ml_forest_t, Tl_type(["tree";"forest"], ml_forest_t))
    ]);
    ("suite_class", [
        ("default", ml_hello_c, 
        Tl_class_and([
            Tl_class({
                name="hello"; 
                header="class hello = object"; 
                virt=false; 
                self=Some("self"); 
                elmts=[
                    Cl_attribut(Tl_var("hello", "hello:string=\"hello\""))
                ]
            })
        ], ml_hello_c))
    ]);
    ("suite_module", [
        ("default", ml_even_m, Tl_module( "Even", [
            Tl_type(["t"], "type t = Zero | Succ of int");
            Tl_var("alpha", "let alpha = Zero");
            Tl_fun("hello", "let hello () = print_endline \"Even\"")
        ]));
        ("signature", ml_even_s, Tl_sign("Even", [
            Tl_type(["t"], "type t = Zero | Succ of int");
            Tl_var("alpha", "val alpha : t")  
        ])); 
        ("constraint", ml_even_c, Tl_constraint("Even", Tl_module("Even", [
                Tl_type(["t"], "type t = Zero | Succ of int");
                Tl_var("alpha", "let alpha = Zero");
                Tl_fun("hello", "let hello () = print_endline \"Even\"")
            ]), Tl_sign("Even", [
                Tl_type(["t"], "type t = Zero | Succ of int");
                Tl_var("alpha", "val alpha : t")  
        ])))
    ]);
]

let test_structs = (make_suites "tl_ast" (test_suites()))
let unit_tests () = run_test_tt_main test_structs
