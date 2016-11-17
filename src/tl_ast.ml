open Parsetree
open Asttypes
open OUnit2

exception Not_define of string
let not_define msg = raise (Not_define( "src/tl_ast.ml : "^msg^"\n" )) 
(**
  * const ref de ml
  *
  *)

(** top-level structures*)
type tl_visibility = Tl_private | Tl_public  
type tl_struct =  
|Tl_none (*to be removed*)
|Tl_open of string list * string (* the string list represents Module1.Module2. ... 
				 the secund parameter is the complete lign*)
|Tl_var of string * string
|Tl_fun of string * string
|Tl_exception of string * string
|Tl_type of string list * string
|Tl_module of string * tl_ast (*TODO : signature, foncteur*)                  
|Tl_sign of string * tl_ast(*en fait une liste de type*)
|Tl_class of {name:string; header:string; virt:bool; self:string option; elmts:class_elmt list}(*name, header, vitual?, methods : (function, visibility), attribut*)
|Tl_class_and of tl_struct list * string 
and class_elmt=
|Cl_method of tl_struct * tl_visibility
|Cl_attribut of tl_struct                           
(** Top-level ast type*)
and tl_ast = tl_struct list


let file_to_string path=
	let input = open_in path in
	really_input_string input (in_channel_length input)

(**Opens a file and returns its ocaml ast*)
let file_to_ast f = 
  Parse.implementation (Lexing.from_channel (open_in f)) 

let string_to_ast str =
  Parse.implementation (Lexing.from_string str)

let print_ast ast = 
  Printast.implementation (Format.formatter_of_out_channel stdout) ast



(* ***BEGIN Miscellaneous functions on locations*)
(**Returns the substring corresponding to the location loc *)
let get_str_from_location ml = function {Location.loc_start = s ; loc_end = e; loc_ghost = _}  ->
  let cs = s.Lexing.pos_cnum and ce = e.Lexing.pos_cnum in 
  String.sub ml cs (ce - cs)

  


(* ***END Miscellaneous functions on locations*)


(* ***BEGIN Conversion from ast to tl_ast*)

(** Returns the string list ["Module1","Module2"] for the lign "open Module1.Module2"*)
let open_description_to_string_list od =
  let rec od_to_sl lid acc = match lid with 
    |Longident.Lident s -> s::acc
    |Longident.Ldot (lid',s) -> od_to_sl lid' (s::acc)
    |Longident.Lapply(_,_) -> failwith "I don't know what is this (Alice) (raised in tl_ast.ml)"
  in
  od_to_sl od.txt []

   
(**
  * @param class_expr of a class
  * @return class_struct, header
  *)
let rec get_class_struct (_:Lexing.position) = function 
  |Pcl_fun(_,_,pattern, child_ast)-> get_class_struct pattern.ppat_loc.Location.loc_end child_ast.pcl_desc(*on est entrain de parser les arguments de la classe, child => child ast*)
  |Pcl_structure(ast)-> ast.pcstr_self.ppat_loc.Location.loc_start, ast 
  |_-> not_define "Pcl_* not supported"                                 

(**
  * @param Pcl_structure c_struct.pcstr_fields.listfield
  * @return (attrs list, methods list)
  *)
let rec class_fields_to_attrs_methods ml elmts= function
  |[]-> List.rev elmts
  |{pcf_desc=Pcf_method(loc0, f_private, expr);pcf_loc=_;pcf_attributes=_}::t ->(
let loc1 = (match expr with |Cfk_virtual ct->ct.ptyp_loc |Cfk_concrete(_,expr1) ->expr1.pexp_loc) in

    let dmethod = Tl_fun(loc0.txt, get_str_from_location ml loc1) in
    let dvisibility =  (match f_private with|Private->Tl_private |_-> Tl_public) in 
      class_fields_to_attrs_methods ml ((Cl_method( dmethod, dvisibility) )::elmts) t
  )
  |{pcf_desc=Pcf_val(loc, _, expr);pcf_loc=_;pcf_attributes=_}::t ->(
     let loc1 = (match expr with |Cfk_virtual ct->ct.ptyp_loc |Cfk_concrete(_,expr1) ->expr1.pexp_loc) in
     let dattr = Tl_var(loc.txt, get_str_from_location ml loc1) in
    class_fields_to_attrs_methods ml ((Cl_attribut dattr)::elmts) t
  )
  |_-> not_define "Pcf_* not supported"  

let class_to_tl_class ml = function({pci_virt=virt; pci_params=_; 
                                    pci_name=name; pci_expr=expr; pci_loc=loc;
                                    pci_attributes=_}:class_declaration)->
  
  let header_end, struct_ast = get_class_struct expr.pcl_loc.Location.loc_start expr.pcl_desc in    
  let elmts = class_fields_to_attrs_methods ml [] struct_ast.pcstr_fields in  
  let self = begin
    match struct_ast.pcstr_self.ppat_desc with |Ppat_any -> None |Ppat_var str_loc ->Some(str_loc.txt)|_->not_define "Ppat_* not supported for self" end in   
 Tl_class {
    name=name.txt;

    header=get_str_from_location ml ({Location.loc_start=loc.Location.loc_start;Location.loc_end=header_end; Location.loc_ghost=false});
    virt=(virt == Virtual);
    self=self;
    elmts=elmts;
  }

(**
  * @param signature(signature_item list )
  *)
let rec sign_to_tl_sign ml types=function
  |[] -> List.rev types
  |{psig_desc=desc;psig_loc=_}::t->(
    match desc with
      |Psig_value {pval_name=name;pval_type=_;pval_prim=_;pval_attributes=_;pval_loc=loc}->(
         let tmp = Tl_type([name.txt], get_str_from_location ml loc) in
         sign_to_tl_sign ml (tmp::types) t  
      )
      |_-> not_define "Psig_* not defined"  
  )

let rec struct_to_tl_struct ml  = function {pstr_desc = struct_item; pstr_loc = loc} ->
  begin
    match struct_item with 
    |Pstr_open open_desc ->  Tl_open(open_description_to_string_list open_desc.popen_lid,
				     get_str_from_location ml loc)
    |Pstr_value(_,  value::_)->((*pour l'instant on ne traite que la première*)
        match value.pvb_pat.ppat_desc  with 
        |Ppat_var loc_var->(
            let name = loc_var.txt and expr = (get_str_from_location ml loc) in
            match value.pvb_expr.pexp_desc with
            | Pexp_function _ | Pexp_fun _-> Tl_fun(name, expr)   
            | _ -> Tl_var(name, expr)   
        )(*comment faire avec les autrs patterns???*)
        |_->Tl_none                  
    )
    |Pstr_exception e->Tl_exception( e.pext_name.txt, get_str_from_location ml loc)
    |Pstr_type(_,decls)->Tl_type((List.map (function d->d.ptype_name.txt) decls), 
       get_str_from_location ml loc)
    |Pstr_class decls->(
        let cls = (List.map (function d->class_to_tl_class ml d) decls) in
        Tl_class_and (cls, get_str_from_location ml loc)
    )
    |Pstr_module {pmb_name=loc; pmb_expr=expr; pmb_attributes=_; pmb_loc=_}->(
        match expr.pmod_desc with
        |Pmod_structure s-> Tl_module(loc.txt, ast_to_tl_ast ml s)
        |_-> not_define "Pmod_* not supported"                  
    )
    |Pstr_modtype {pmtd_name=name; pmtd_type=opt_m_type; pmtd_attributes=_; pmtd_loc=_}->(
        match opt_m_type with 
        |None -> Tl_sign( name.txt, [])   
        |Some m_type ->
            match m_type.pmty_desc with 
              |Pmty_signature s -> Tl_sign( name.txt, sign_to_tl_sign ml [] s)
              |_-> not_define "Pmty_* not supported"                        
     )
    |_ -> Tl_none
  end



(*ml is a string containing the whole file from which ast was created *)
and ast_to_tl_ast ml ast = List.map (struct_to_tl_struct ml) ast
    
let string_to_tl_ast ml = List.map (struct_to_tl_struct ml) (string_to_ast ml)


(* ***END Conversion from ast to tl_ast*)
  

(* ***BEGIN Printing of a tl_ast*)
let rec class_elmt_to_str head= function
    |Cl_attribut attr ->  Format.sprintf "%s\tval %s" head (tl_struct_to_str attr)  
    |Cl_method (m, f_v)->( Format.sprintf "%s\t%smethod %s" head 
        (match f_v with |Tl_private->"private "|_->"") 
        (tl_struct_to_str m)
    )     
and tl_struct_to_str =function 
    |Tl_open(_,s)-> Format.sprintf "%s\n" s
    |Tl_var(_, expr)->Format.sprintf "%s\n" expr 
    |Tl_fun(_, expr)->Format.sprintf "%s\n" expr
    |Tl_exception(_, values)->Format.sprintf "%s\n" values    
    |Tl_type(_,value)->Format.sprintf "%s\n" value
    |Tl_sign(name,ast)->Format.sprintf "module %s = sign\n%s\nend\n" name (tl_ast_to_str ast)
    |Tl_module(name, ast)-> Format.sprintf "module %s = struct\n%s\nend\n" name (tl_ast_to_str ast)                     
    |Tl_class cl ->(
       let elmts_str = List.fold_left (fun head elmt -> (class_elmt_to_str head elmt)) "" cl.elmts in
       let self_str = (match cl.self with |None->"" |Some(s)->"("^s^")" )in 
         Format.sprintf "%s%s\n%s\nend\n" cl.header self_str elmts_str  
    )
    |Tl_class_and(cls,_) -> List.fold_left (fun head cl -> Format.sprintf "%s\n%s" head  (tl_struct_to_str cl)) "" cls 
    |Tl_none -> ""

and tl_ast_to_str tl = String.concat "" (List.map tl_struct_to_str tl)


    

let print_tl_ast tl = Printf.printf "%s\n" (tl_ast_to_str tl)

(* ***END Printing of a tl_ast*)


(* ***BEGIN Some functions to test more easily *)

let ast_from_string s = Parse.implementation (Lexing.from_string s) 
let quick_tl_ast s = ast_to_tl_ast s (ast_from_string s)
let quick_tl_struct s = List.hd (quick_tl_ast s)

(* ***END Some functions to test more easily *)
(*
let test_open _ = 
  assert_equal (quick_tl_struct "open Module1.Module2") 
    (Tl_open(["Module1";"Module2"],"open Module1.Module2"))

let test_var _ =
  assert_equal (quick_tl_struct "let a = 3")
    (Tl_var("a","let a = 3"))

let test_var_ref _ =
  assert_equal (quick_tl_struct "let a = ref [\"test\"]")
    (Tl_var("a", "let a = ref [\"test\"]"))

let test_fun _ =
  assert_equal (quick_tl_struct "let f x = x")
    (Tl_fun("f", "let f x = x"))

let test_fun_function _ = 
  assert_equal (quick_tl_struct "let f = function x -> x")
    (Tl_fun("f", "let f = function x -> x"))

let test_fun_fun _ = 
  assert_equal (quick_tl_struct "let f = fun x y-> x+y")
    (Tl_fun("f", "let f = fun x y-> x+y"))

let test_exception _ =
  assert_equal (quick_tl_struct "exception E of string")
    (Tl_exception("E", "exception E of string"))

let test_type _ =
  let tdef= "type 'a tree =Nil |Node of 'a tree * 'a tree * 'a" in
  assert_equal (quick_tl_struct tdef) (Tl_type(["tree"], tdef))

let test_type_and _ =
  let tdef="type 'a tree=Nil|Node of 'a tree*'a tree*'a and 'a forest='a tree list" in
  assert_equal (quick_tl_struct tdef) (Tl_type(["tree";"forest"], tdef))

let test_class _ =
 let cdef="class test = object(self)\n\tval coucou:string=\"coucou\"\nend" in
 begin
   match  (quick_tl_struct cdef) with
   |Tl_class_and( (Tl_class c)::_, a) ->(
        Printf.printf "name #%s#\n" c.name;
        Printf.printf "Header : #%s#\n" c.header;
        Printf.printf "#%s#\n" a;
        Printf.printf "#%s#\n" cdef;
        Printf.printf "fuck!!!!!!!%s\n" (if a<>cdef then "ee" else "aa");
        match c.elmts with
          |Cl_attribut(Tl_var(n,v))::_->Printf.printf "attr #%s#%s#\n" n v
          |_->()
   )
   |_->()                                       
 end; 
   assert_equal (quick_tl_struct cdef) (Tl_class_and([Tl_class({
  name="test"; header="class test = object"; virt=false; self=Some("self"); elmts=[Cl_attribut(Tl_var("coucou", "coucou:string=\"coucou\""))]
  }) ], cdef))
(*
let test_class_and _ =
 let cdef="class test = object(self) val coucou:string=\"coucou\" end" in
 assert_equal (quick_tl_struct cdef) (Tl_class(["test"], cdef))
 *)
let test_module _ =
  let mdef = "module Hello = struct \
    let message = \"Hello\" \
    let hello () = print_endline message \
  end" in

  let mstruct = (Tl_module( "Hello", [
    (Tl_var("message", "let message = \"Hello\"")); 
    (Tl_fun("hello", "let hello () = print_endline message"))
  ])) in

  assert_equal (quick_tl_struct mdef) mstruct                
  *)
let make_suite name suite =
    name >::: (List.map( function (name,ml,tl_struct)->
        name>::function _-> assert_equal (quick_tl_struct ml) tl_struct
    ) suite)

let make_suites name suites =
    name >::: (List.map( function (name,suite)->  make_suite name suite)suites) 

let test_suites ()=
    let ml_forest_t = "type 'a tree=Nil|Node of 'a tree*'a tree*'a and \
    'a forest='a tree list" in
    let ml_hello_c =  "class hello = object(self) val hello:string=\"hello\" \
    end" in
    let ml_hello_m =  "module Hello = struct \
        let message = \"Hello\" \
        let hello () = print_endline message \
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
        ("default", ml_hello_m, Tl_module( "Hello", [
            Tl_var("message", "let message = \"Hello\""); 
            Tl_fun("hello", "let hello () = print_endline message")
        ]))      
    ]);
]

let test_structs =
  let l = (make_suites "tl_ast" (test_suites())) in

   l   (*i ["open">::test_open; "var">::test_var; "var_ref">::test_var_ref;
    "fun">::test_fun; "test_fun_function">::test_fun_function;
    "fun_function">::test_fun_function; "fun_fun">:: test_fun_fun;
    "exception">::test_exception; "test_type">::test_type; 
    "test_type_and">::test_type_and;
                                     "test_class">::test_class(*;
    "test_class_and">::test_class_and*);
    "">::test_module]*)
    
let unit_tests () = run_test_tt_main test_structs
