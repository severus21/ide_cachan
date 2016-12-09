open OUnit2

open Tl_ast  
open Ml_to_tl
open Tl_to_c  

let make_suite name suite =
    name >::: (List.map( function (name,ml,tl_struct)->
        name>::function _-> assert_equal (str_to_tl_struct ml) tl_struct
    ) suite)

let make_suites name suites =
    name >::: (List.map( function (name,suite)->  make_suite name suite)suites) 

let test_suites ()=
    let ml_forest_t = "type 'a tree=Nil|Node of 'a tree*'a tree*'a and \
    'a forest='a tree list" in
    let ml_hello_c =  "class hello = object(self) \
        val hello:string=\"hello\" \
        val alpha = 12 \
        val arf = ref true \
        inherit troll as super_meat_boy \
        method set (key:string) = 12 \
        initializer(arf:=false) \
    end" in
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
    let ml_even_odd_mr = "module rec Even : sig \
        type t = Zero | Succ of Odd.t \
    end = struct \
        type t = Zero | Succ of Odd.t \
    end \
    and Odd : sig \
        type t = Succ of Even.t \
    end = struct \
        type t = Succ of Even.t \
    end" in 
    (*let ml_comparable_s="module type Comparable = sig \
        type t \
        val compare : t -> t -> int \
    end" in*)                               
    let ml_compare_f = "module OrderList (T:Comparable) = struct \
        exception Empty \
        type content = T.t \
        type t = content list ref \
        let comp = T.compare \
    end" in

(*type c_ast = Nil| Node of string * string * string * c_ast list*)
    let ml_ptr_ast_c = "class ptr_ast x: object \
        val p_ast : c_ast ref \
        method ast : c_ast \
    end = object \
        val p_ast = ref Nil \
        method ast = Nil \
    end" in   
    
    let ml_class_type = "class type restricted_point_type = object \
        inherit dwarf \
        method get_x : int \
        method bump : unit \
    end" in 

    let ml_arrow_contraint ="val extract: string -> string list -> int list" in

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
                    Cl_attribut(Tl_var("hello", "hello:string=\"hello\""));
                    Cl_attribut(Tl_var("alpha", "alpha = 12"));
                    Cl_attribut(Tl_var("arf", "arf = ref true"));
                    Cl_inherit("troll", Some "super_meat_boy");
                    Cl_method(Tl_fun("set", "set (key:string) = 12"), Tl_public);
                    Cl_init("(arf:=false)")
                ];
                c_elmts=[];
            })
        ], ml_hello_c));
        ("constraint", ml_ptr_ast_c, 
        Tl_class_and([
            Tl_class({
                name="ptr_ast";
                header="class ptr_ast x:";
                virt=false;
                self=None;
                elmts=[
                    Cl_attribut(Tl_var("p_ast", "p_ast = ref Nil"));
                    Cl_method(Tl_fun("ast", "ast = Nil"), Tl_public);
                ];
                c_elmts=[
                    Cl_attribut(Tl_constraint("p_ast", "c_ast ref"));
                    Cl_method(Tl_constraint("ast", "c_ast"), Tl_public);
                ];
            })
        ], ml_ptr_ast_c));
        "class_type", ml_class_type, 
        Tl_class_and([
            Tl_class({
                name="restricted_point_type";
                header="";
                virt=false;
                self=None;
                elmts=[];
                c_elmts=[
                    Cl_inherit("dwarf", None);
                    Cl_method(Tl_constraint("get_x", "int"), Tl_public);
                    Cl_method(Tl_constraint("bump", "unit"), Tl_public);
                ];
            })
        ], ml_class_type)
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
        ("constraint", ml_even_c, Tl_module_constraint("Even", Tl_module("Even", [
                Tl_type(["t"], "type t = Zero | Succ of int");
                Tl_var("alpha", "let alpha = Zero");
                Tl_fun("hello", "let hello () = print_endline \"Even\"")
            ]), Tl_sign("Even", [
                Tl_type(["t"], "type t = Zero | Succ of int");
                Tl_var("alpha", "val alpha : t")  
        ])));
        ("rec", ml_even_odd_mr, Tl_recmodule([
            Tl_module_constraint("Even", Tl_module("Even",[
                Tl_type(["t"], "type t = Zero | Succ of Odd.t") 
            ]), Tl_sign("Even",[
                Tl_type(["t"], "type t = Zero | Succ of Odd.t") 
            ]));
            Tl_module_constraint("Odd", Tl_module("Odd",[
                Tl_type(["t"], "type t = Succ of Even.t") 
            ]), Tl_sign("Odd",[
                Tl_type(["t"], "type t = Succ of Even.t") 
            ]));

        ], ml_even_odd_mr));
        ("functor", ml_compare_f, Tl_functor("OrderList", 
            "OrderList (T:Comparable) =", [
                Tl_exception("Empty", "exception Empty");
                Tl_type(["content"], "type content = T.t");
                Tl_type(["t"], "type t = content list ref");
                Tl_var("comp", "let comp = T.compare")
            ]))
    ]);
    ("suite_constraint", [
        ("arrow", ml_arrow_contraint, Tl_constraint("extract", 
            "string -> string list -> int list")) 
    ]);
]

let test_suite2 ()=
    let bodies = [
        ("tree", "type 'a tree=Nil|Node of 'a tree*'a tree*'a and \
        'a forest='a tree list");

        ("helloc_c", "class hello = object(self) \
            val hello:string=\"hello\" \
            val alpha = 12 \
            val arf = ref true \
            method set (key:string) = 12 \
            initializer(arf:=false) \
        end"); 

        ("even_m", "module Even = struct \
            type t = Zero | Succ of int \
            let alpha = Zero \
            let hello () = print_endline \"Even\" \
        end");

        ("even_m_t", "module type Even = sig \
            type t = Zero | Succ of int \
            val alpha : t \
        end");             

        ("even_m_c", "module Even : sig \
            type t = Zero | Succ of int \
            val alpha : t \
        end = struct \
            type t = Zero | Succ of int \
            let alpha = Zero \
            let hello () = print_endline \"Even\" \
        end");

        ("even_odd", "module rec Even : sig \
            type t = Zero | Succ of Odd.t \
        end = struct \
            type t = Zero | Succ of Odd.t \
        end \
        and Odd : sig \
            type t = Succ of Even.t \
        end = struct \
            type t = Succ of Even.t \
        end"); 

        ("comparable", "module type Comparable = sig \
            type t \
            val compare : t -> t -> int \
        end");                                

        ("OrderList", "module OrderList (T:Comparable) = struct \
            exception Empty \
            type content = T.t \
            type t = content list ref \
            let comp = T.compare \
        end");

        ("ptr_ast", "class ptr_ast x: object \
            val p_ast : c_ast ref \
            method ast : c_ast \
        end = object \
            val p_ast = ref Nil \
            method ast = Nil \
        end");   
    ] in
    "export/import to c_ast" >:::(List.map (function name,body->(
        let tl_ast = str_to_tl_ast body in
        name>::function _-> assert_equal tl_ast (c_ast_to_tl_ast (tl_ast_to_c_ast tl_ast))
    )) bodies)                         

let test_structs = (make_suites "tl_ast" (test_suites()))
                     
let tests ()= "Ocaml">::: [test_structs; (test_suite2())]
