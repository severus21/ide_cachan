open Parsetree
open Asttypes

exception Not_define of string
let not_define msg = raise (Not_define( "src/tl_ast.ml : "^msg^"\n" )) 
exception Bad_ast of string
let bad_ast msg = raise (Bad_ast( "src/tl_ast.ml : "^msg^"\n" )) 
(** TODO
  *
  * extand class
  * inherit, class signature
  * basic-left pattern
  *
  * rewrite and factorize
  *)


type tl_visibility = Tl_private|Tl_public
 
type tl_struct =  
|Tl_none 
|Tl_open of string list * string  
|Tl_var of string * string
|Tl_constraint of string * string
|Tl_fun of string * string
|Tl_exception of string * string
|Tl_type of string list * string
|Tl_module of string * tl_ast
|Tl_sign of string * tl_ast
|Tl_module_constraint of string * tl_struct* tl_struct
|Tl_functor of string * string * tl_ast                                      
|Tl_recmodule of tl_ast * string                                        
|Tl_class of {name:string; header:string; virt:bool; self:string option; elmts:class_elmt list; c_elmts: class_elmt list}
|Tl_class_and of tl_struct list * string
and class_elmt=
|Cl_method of tl_struct * tl_visibility
|Cl_attribut of tl_struct                  
|Cl_init of string
and tl_ast = tl_struct list


(**Opens a file and returns its ocaml ast*)
let file_to_ast f = 
  Parse.implementation (Lexing.from_channel (open_in f)) 

let string_to_ast str =
  Parse.implementation (Lexing.from_string str)

let print_ast ast = 
  Printast.implementation (Format.formatter_of_out_channel stdout) ast



(* ***BEGIN Miscellaneous functions on locations*)
(**Returns the substring corresponding to the location loc *)
let get_str_from_location ml = function {Location.loc_start = s ; loc_end = e; 
                                         loc_ghost = _}  ->
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

let pexp_to_tl name body=function{pexp_desc=desc;_}->
    match desc with  
    | Pexp_function _ | Pexp_fun _-> Tl_fun(name, body)   
    | _ -> Tl_var(name, body)   

   
(**
  * @param class_expr of a class
  * @return caml class_struct, caml constaint struct, header
  *)

let compare_lexing_pos (pos1:Lexing.position) pos2=    
    assert (pos1.Lexing.pos_fname == pos2.Lexing.pos_fname);
    pos1.Lexing.pos_cnum < pos2.Lexing.pos_cnum 

let rec ptyp_to_tl name {ptyp_desc=desc;_}=
match desc with
|Ptyp_any -> Tl_none    
|Ptyp_var str->Tl_constraint(name, str)
|Ptyp_constr(lg_ident , next)->( 
   let constraints = List.map (function x->ptyp_to_tl "" x) next in
   let tmp = List.fold_left (fun str x->str^x) "" (Longident.flatten lg_ident.txt) in 
   let value = List.fold_left (fun (value:string) (tl:tl_struct)->(match tl with 
                     |Tl_constraint(_, v)->value^" "^v
                     |Tl_none -> value
                     |_->not_define "Bad ast")) "" constraints in                                  
   Tl_constraint( name, String.trim( value^" "^tmp)) 
)                                   
|_-> not_define "Ptyp_* not supported"

let pctf_to_tl {pctf_desc=desc;_}=
match desc with  
|Pctf_val(name, _, _, c_type)->Cl_attribut(ptyp_to_tl name c_type)
|Pctf_method(name, private_f, _, c_type)->(
    Cl_method(ptyp_to_tl name c_type, 
              match private_f with|Private->Tl_private|Public->Tl_public)
)   
|_-> not_define "Pctf_* not supported"

let pcty_to_tl {pcty_desc=desc;_}=
match desc with
|Pcty_signature cl_s->List.map pctf_to_tl cl_s.pcsig_fields
|_->not_define "Pcty_* not supported"   


let rec get_class_struct = function 
    |Pcl_fun(_,_,_, child_ast)->( 
        get_class_struct child_ast.pcl_desc(*on est entrain de parser les arguments de la classe, child => child ast*)
    )
    |Pcl_structure(ast)->(
        ast.pcstr_self.ppat_loc.Location.loc_start, ast, [] 
    )
    |Pcl_constraint(cl,cl_t)->(
        let pos1, cl_struct, _ = get_class_struct cl.pcl_desc in
        let pos2 = cl_t.pcty_loc.Location.loc_start in
        let header_end = if compare_lexing_pos pos1 pos2 then pos1 else pos2 in 
        header_end, cl_struct, pcty_to_tl cl_t   

    )   
    |_-> not_define "Pcl_* not supported"                                 

(**
  * @param Pcl_structure c_struct.pcstr_fields.listfield
  * @return (attrs list, methods list)
  *)
let class_fields_to_attrs_methods ml fields=
    let field_to_cl_field pcl_struct=
        match pcl_struct.pcf_desc with
        |Pcf_method(loc0, f_private, expr)->(
            let loc1 =( match expr with
                |Cfk_virtual ct->ct.ptyp_loc 
                |Cfk_concrete(_,expr1) ->expr1.pexp_loc
            ) in

            let dmethod = Tl_fun(loc0.txt, get_str_from_location ml loc1) in

            let dvisibility =( match f_private with
                |Private->Tl_private 
                |_-> Tl_public) in 

            Cl_method( dmethod, dvisibility) 
        )
        |Pcf_val(loc, _, expr)->(
            let loc1 =( match expr with 
                |Cfk_virtual ct->ct.ptyp_loc 
                |Cfk_concrete(_,expr1) ->expr1.pexp_loc
            ) in

            let loc2= {Location.loc_start=loc.loc.Location.loc_start; loc_end=loc1.Location.loc_end; loc_ghost=false} in

            let dattr = Tl_var(loc.txt, get_str_from_location ml loc2) in

            (Cl_attribut dattr)
        )
        |Pcf_initializer exp->(
             Cl_init (get_str_from_location ml exp.pexp_loc)
        )    
        |_-> not_define "Pcf_* not supported" 
    in
    List.map field_to_cl_field fields    

let class_to_tl_class ml = function{pci_virt=virt; pci_params=_; 
                                    pci_name=name; pci_expr=expr; pci_loc=loc;
                                    pci_attributes=_}->
    
    let header_end, struct_ast, constraints = get_class_struct expr.pcl_desc in    
    let elmts = class_fields_to_attrs_methods ml struct_ast.pcstr_fields in  
    
    (*detect if there is some _ste in class ...= object(_str) ... end*)
    let self = begin
        match struct_ast.pcstr_self.ppat_desc with 
            |Ppat_any -> None 
            |Ppat_var str_loc ->Some(str_loc.txt)
            |_->not_define "Ppat_* not supported for self" 
    end in   

    Tl_class {
        name = name.txt;
        header = String.trim (get_str_from_location ml (
            {Location.loc_start=loc.Location.loc_start; loc_end=header_end; 
            loc_ghost=false})
        );
        virt = (virt == Virtual);
        self = self;
        elmts = elmts;
        c_elmts = constraints;
    }

let class_type_to_tl_class = function{pci_virt=virt; pci_params=_; 
                                    pci_name=name; pci_expr=expr;_}-> 
    
    let constraints = pcty_to_tl expr in  

    Tl_class {
        name = name.txt;
        header = "";
        virt = (virt == Virtual);
        self = None;
        elmts = [];
        c_elmts = constraints;
    }
(*body : code des types, function is_rec*)      
let type_declarations_to_tl t_dcls body=
    Tl_type((List.map (function d->d.ptype_name.txt) t_dcls), body)

let value_description_to_tl_var ml value=
  Tl_var(value.pval_name.txt, get_str_from_location ml value.pval_loc)

(**
  * @param signature(signature_item list )
  *)
let sign_to_tl_sign ml signatures=
    let caml_to_tl {psig_desc=desc;psig_loc=loc}=
        match desc with
        |Psig_value value-> value_description_to_tl_var ml value(* val .. = ..*)
        |Psig_type(_, t_dcls)->((*type declaration in sign ... end*)
            type_declarations_to_tl t_dcls (get_str_from_location ml loc) 
        )   
        |_-> not_define "Psig_* not defined"  
    in 
    
    (List.map caml_to_tl signatures)  

let rec find_functor_header loc_end m_expr = function
|Pmod_functor(_,_, m_expr0)->(
    find_functor_header m_expr0.pmod_loc.Location.loc_start m_expr0 m_expr0.pmod_desc
)    
|_->loc_end, m_expr

let rec struct_to_tl_struct ml  = function{pstr_desc=struct_item;pstr_loc=loc}->
    let body = get_str_from_location ml loc in
    
    (* args Pmod_, name provient du parent* *)
    let rec pmod_to_tl ({txt=name;loc=loc} as args )  =function
        |Pmod_structure s-> Tl_module(name, ast_to_tl_ast ml s)
        |Pmod_constraint (m, mt)->(
                Tl_module_constraint(name, (pmod_to_tl args m.pmod_desc), 
                          (pmty_to_tl name (Some mt)))
        )
        |Pmod_functor({loc=_loc;_}, _, m_expr) as func->(
            let header_end, expr = find_functor_header _loc.Location.loc_end m_expr func in
            let header_loc = {Location.loc_start=loc.Location.loc_start; 
                              loc_end=header_end; loc_ghost=false} in
            let header = String.trim (get_str_from_location ml header_loc) in   
            
            let body =  (pmod_to_tl {txt=""; loc=loc}  expr.pmod_desc) in
            match body with 
            |Tl_module(_, m_body)->Tl_functor(name, header, m_body)
            |_->Tl_functor(name, header, [body])                         
        )   
        |_-> not_define "Pmod_* not supported"
    (* name, option(module_type) *)           
    and pmty_to_tl name =function
        |None -> Tl_sign( name, [])   
        |Some m_type ->(
            match m_type.pmty_desc with 
            |Pmty_signature s -> Tl_sign( name, sign_to_tl_sign ml s)
            |Pmty_ident{txt=lg_ident;_}->(*par extrapolation depuis les foncteurs*)
               Tl_type( [name], List.fold_left (fun str x->str^x) 
                              "" (Longident.flatten lg_ident))
            |_-> not_define "Pmty_* not supported" 
        )           
    in
      

    match struct_item with 
    |Pstr_open open_desc ->  
        Tl_open(open_description_to_string_list open_desc.popen_lid, body)
    |Pstr_value(_,  value::_)->((*pour l'instant on ne traite que la première*)
        match value.pvb_pat.ppat_desc  with 
        |Ppat_var {txt=name;_}-> pexp_to_tl name body value.pvb_expr
        |_->Tl_none                  
    )
    |Pstr_exception {pext_name={txt=name;_};_}->
        Tl_exception( name, body)
    |Pstr_type(_,t_dcls)-> type_declarations_to_tl t_dcls body
    |Pstr_class decls->(
        let cls = (List.map (function d->class_to_tl_class ml d) decls) in
        Tl_class_and(cls, body)
    )
    |Pstr_class_type decls->(
        let cls = (List.map (function d->class_type_to_tl_class d) decls) in
        Tl_class_and(cls, body)  
    )  
    |Pstr_module m -> pmod_to_tl m.pmb_name m.pmb_expr.pmod_desc
    |Pstr_modtype mt -> pmty_to_tl mt.pmtd_name.txt mt.pmtd_type
    |Pstr_recmodule m_list ->(
        Tl_recmodule(List.map (function m->pmod_to_tl m.pmb_name 
             m.pmb_expr.pmod_desc) m_list, body)
    )   
    |_ -> Tl_none


(**TODO : faire une nouvelle passe de simplification vers un nouvel ast et ajouter les types**)


(*ml is a string containing the whole file from which ast was created *)
and ast_to_tl_ast ml ast = List.map (struct_to_tl_struct ml) ast
    
let string_to_tl_ast ml = List.map (struct_to_tl_struct ml) (string_to_ast ml)


(* ***END Conversion from ast to tl_ast*)
  

(* ***BEGIN Printing of a tl_ast*)
let rec class_elmt_to_str head= function
    |Cl_attribut Tl_var(name, value) ->
        Format.sprintf "%s\tval %s = %s\n" head name value 
    |Cl_attribut Tl_constraint(name, value)->
        Format.sprintf "%s\tval %s : %s\n" head name value
    |Cl_method (m, f_v)->(
        let virtual_str = match f_v with |Tl_private->"private "|_->"" in
         match m with
        |Tl_fun(name, expr)->    
            Format.sprintf "%s\t%smethod %s = %s\n" head virtual_str name expr 
        |Tl_constraint(name, expr)->
            Format.sprintf "%s\t%smethod %s : %s\n" head virtual_str name expr
        |_-> not_define "bad tree class_elmt_to_str Cl_method"       
    )
    |Cl_init body-> Format.sprintf "%sinitializer %s" head body              
    |_->not_define "baf_tl_ast class_elmt_to_str"                  
and tl_struct_to_str =function 
    |Tl_open(_,s)-> Format.sprintf "%s\n" s
    |Tl_var(_, expr)->Format.sprintf "%s\n" expr 
    |Tl_constraint(_, expr)->Format.sprintf "%s\n" expr
    |Tl_fun(_, expr)->Format.sprintf "%s\n" expr
    |Tl_exception(_, values)->Format.sprintf "%s\n" values    
    |Tl_type(_, value)->Format.sprintf "%s\n" value
    |Tl_sign(name,ast)->Format.sprintf "module type %s = sig\n%send\n" name (tl_ast_to_str ast)
    |Tl_module(name, ast)-> Format.sprintf "module %s = struct\n%send\n" name (tl_ast_to_str ast) 
    |Tl_module_constraint(name, Tl_module(_,m), Tl_sign(_,mt))->(
        Format.sprintf "module %s : sig\n\
          %s\
          end = struct \n\
          %s\
          end\n" name (tl_ast_to_str mt) (tl_ast_to_str m)      
    )
    |Tl_recmodule(modules,_)->(
       let str = (List.fold_left (fun str x->(
            let _str = tl_struct_to_str x in
            let tmp = String.sub _str 7 (String.length _str -7) in (*removed module*)                 
            str^tmp^" and " 
        )) "module " modules) in

        String.sub str 0 (String.length str- 4)   
    )   
    |Tl_functor(_, header, next)->(
        Format.sprintf "module %s struct\n%send\n" header (tl_ast_to_str next)
    )
    |Tl_class cl ->(
       let elmts_str = List.fold_left (fun head elmt -> (class_elmt_to_str head elmt)) "" cl.elmts in
       let c_elmts_str = List.fold_left (fun head elmt -> (class_elmt_to_str head elmt)) "" cl.c_elmts in
         
       let self_str = (match cl.self with |None->"" |Some(s)->"("^s^")" )in 

       match cl.c_elmts, cl.elmts with
       |[], [] -> ""
       |_::_, []  -> Format.sprintf "class type %s = object\n%send\n" cl.name c_elmts_str   
       |[], _::_  -> Format.sprintf "%s%s\n%send\n" cl.header self_str elmts_str   
       |_, _   ->(Format.sprintf "%sobject\n%send = object%s\n%send\n" cl.header  
                c_elmts_str self_str elmts_str         
       )        
    )
    |Tl_class_and(cls,_) -> List.fold_left (fun head cl -> Format.sprintf "%s\n%s" head  (tl_struct_to_str cl)) "" cls 
    |Tl_none -> ""
    |_->failwith "Bad tree"               

and tl_ast_to_str tl = String.concat "" (List.map tl_struct_to_str tl)


    

let print_tl_ast tl = Printf.printf "%s\n" (tl_ast_to_str tl)

(* ***END Printing of a tl_ast*)


(* ***BEGIN Some functions to test more easily *)

let ast_from_string s = Parse.implementation (Lexing.from_string s) 
let quick_tl_ast s = ast_to_tl_ast s (ast_from_string s)
let quick_tl_struct s = List.hd (quick_tl_ast s)

(* ***END Some functions to test more easily *)


                          
open Core.Miscs
open Core.Gset                          
(* export to c_ast list*)   
(*il faut tagger l'ast sinon on ne sais plus qui est quoi *)
(* db: :(string, ref string) Hashtbl.t), np namespace*)
let ptr = let db = Hashtbl.create 1024 in fun (np:string) (x:string)->
    if (Hashtbl.mem db np) = false then Hashtbl.add db np (Hashtbl.create 1024);
    let tmp = Hashtbl.find db np in 
    
    if (Hashtbl.mem tmp x) = false then Hashtbl.add tmp x (ref x); 
    Hashtbl.find tmp x 



let rec cl_elmt_to_core (np:string) cl_elmt=
    let meta = new tags in 
    match cl_elmt with  
    |Cl_attribut tl_s->(
        meta#add_tag "plg_ast" [TStr "Cl_attribut"];
        match tl_s with
        |Tl_var(name, body)->(
            meta#add_tag "plg_desc" [ TStr "Tl_var"];
            [Node {
                name=name;
                header="";
                body= ptr np body;
                children=[];
                meta=meta;
            }]
        )
        |Tl_constraint(name, body)->(
            meta#add_tag "plg_desc" [ TStr "Tl_constraint"];
            [Node{
                name = name;
                header = "";
                body = ptr np body;
                children = [];
                meta = meta;
            }]
        )
        |_-> bad_ast "cl_elmt_to_core : Cl_attribut"
    )           
    |Cl_method(tl_s, tl_v)->(
        let header = match tl_v with|Tl_private->"private"|Tl_public->"public" in
        meta#add_tag "plg_ast" [ TStr "Cl_method"];
        match tl_s with
        |Tl_fun(name, body)->(
            meta#add_tag "plg_desc" [ TStr "Tl_fun"];
            [Node {
                name=name;
                header=header;
                body= ptr np body;
                children=[];
                meta=meta;
            }]
        )
        |Tl_constraint(name, body)->(
            meta#add_tag "plg_desc" [ TStr "Tl_constraint"];
            [Node{
                name = name;
                header = header;
                body = ptr np body;
                children = [];
                meta = meta;
            }]
        )
        |_->bad_ast "cl_elmt_to_core : Cl_method"                       
    )     
    |Cl_init(body)->(
        meta#add_tag "plg_ast" ([TStr "Cl_init"]:gset tag);
        [Node({
        name="";
        header="";
        body=ptr np body;
        children=[];
        meta=meta})]
    )      
and tl_struct_to_core np tl_struct=
    let meta = new tags in
    match tl_struct with
    |Tl_none -> []    
    |Tl_open(modules, body) ->( 
        meta#add_tag "plg_ast" [TStr "Tl_open"];
        [Node({
            name = List.fold_left (fun x y->x^"."^y) "" modules;
            header="";
            body=ptr np body;
            children=[];
            meta=meta})]
    )                               
    |Tl_var(name, body) ->(
        meta#add_tag "plg_ast" [TStr "Tl_var"];
        [Node({
            name =name;
            header="";
            body=ptr np body;
            children=[];
            meta=meta})]
    )                          
    |Tl_constraint(name, body) ->(
        meta#add_tag "plg_ast" [TStr "Tl_constraint"];    
        [Node({
            name = name;
            header="";
            body=ptr np body;
            children=[];
            meta=meta})]
    )      
    |Tl_fun(name, body) ->(
        meta#add_tag "plg_ast" [TStr "Tl_fun"];
        [Node({
            name =name;
            header="";
            body=ptr np body;
            children=[];
            meta=meta})]
    )      
    |Tl_exception(name, body) ->(
        meta#add_tag "plg_ast" [TStr "Tl_exception"];
        [Node({
            name = name;
            header="";
            body=ptr np body;
            children=[];
            meta=meta})]
    )      
    |Tl_type(names, body) ->(
        meta#add_tag "plg_ast" [TStr "Tl_type"];
        let meta_leaf = new tags in 
        meta_leaf#add_tag "plg_ast" [TStr "Tl_type_leaf"];
        [Node({
            name="";
            header="";
            body=ptr np body;
            children=List.map (function name-> Node({
                name =name;
                header="";
                body=ptr np body;
                children=[];
                meta=meta_leaf})) names;
            meta=meta})]  
    )       
    |Tl_module(name, ast) ->(
        meta#add_tag "plg_ast" [TStr "Tl_module"];
        [Node({
            name = name;
            header = "";
            body = ref "";
            children = tl_ast_to_core (np^"."^name) ast;
            meta=meta})]    
    )      
    |Tl_sign(name, ast) ->(
        meta#add_tag "plg_ast" [TStr "Tl_sign"];
        [Node({
            name = name;
            header = "";
            body = ref "";
            children = tl_ast_to_core (np^"."^name) ast;
            meta=meta})]  
    )      
    |Tl_module_constraint(name, m, m_t) ->(
        let fct value = List.iter( function
            |Nil->()
            |Node {meta=meta;_}->meta#add_tag "plg_desc" [TStr value]) in

        meta#add_tag "plg_ast" [TStr "Tl_module_constraint"];
        let m_t_children = (tl_struct_to_core np m_t) 
        and m_children = (tl_struct_to_core np m)in
        fct "module_type_item" m_t_children; 
        fct "module_item" m_children;
          
        [Node({
            name = name;
            header = "";
            body = ref "";
            children = m_t_children @ m_children; 
            meta=meta})]
    )      
    |Tl_functor(name, header, ast) ->(
        meta#add_tag "plg_ast" [TStr "Tl_functor"];
        [Node({
            name = name;
            header = header;
            body = ref "";
            children = tl_ast_to_core (np^"."^name) ast;
            meta=meta})]
    )      
    |Tl_recmodule(modules, body) ->(
        meta#add_tag "plg_ast" [TStr "Tl_recmodule"];
        [Node({
            name = "";
            header = "";
            body = ptr np body;
            children = tl_ast_to_core np modules;
            meta=meta})]
    )      
    |Tl_class cl->( 
        let fct value = List.iter( function
            |Nil->()
            |Node {meta=meta;_}->meta#add_tag "plg_desc" [TStr value]) in

        let fct1 items = List.concat( List.map (function x-> 
                                  cl_elmt_to_core (np^"#"^cl.name) x) items) in  
          
        meta#add_tag "plg_ast" [TStr "Tl_class"];
        meta#add_tag "plg_virt" [TStr (if cl.virt then "true" else "false")];
        meta#add_tag "plg_self" [TStr ((match cl.self with |None->""|Some(s)->s))];

        let c_t, c = fct1 cl.c_elmts, fct1 cl.elmts in
        fct "class_type_item" c_t;
        fct "class_item" c;

        [Node({
            name = cl.name;
            header = cl.header;
            body = ref "";
            children = (c_t @ c);
            meta=meta})]
    )                  
    |Tl_class_and(cls, body) ->(
        meta#add_tag "plg_ast" [TStr "Tl_class_and"];
        [Node({
            name = "";
            header = "";
            body = ptr np body;
            children = tl_ast_to_core np cls;
            meta=meta})]
    ) 
and tl_ast_to_core np = function tl_ast -> 
    List.concat (List.map (tl_struct_to_core np) tl_ast)


let c_type_to_tl_type =function
|Nil->not_define "Bad core node for type_leaf"
|Node node->(    
    match node.meta#get_value "plg_ast" with
    |Some [TStr "Tl_type_leaf"]->node.name
    |_->not_define "Bad core node for type_leafoo"
 )

let rec split_c_constraint prefix=function
|[]-> [], []   
|Nil::_-> not_define "not def"
|(Node child)::children->(
    let (t_items:c_node list), items = split_c_constraint prefix children in   
    let t_label, label = prefix^"_type_item", prefix^"_item" in 
      
    match child.meta#get_value "plg_desc" with
    |Some [TStr l] when l=t_label-> (Node child)::t_items, items
    |Some [TStr l] when l=label->t_items, (Node child)::items
    |_->not_define "not def"
)

let rec c_ast_to_cl_elmt=function
|Nil ->not_define "bas ast c_ast_to_cl_elmt" 
|Node node->(  
    match node.meta#get_value "plg_ast" with
    |Some [TStr "Cl_attribut"]->(
        match node.meta#get_value "plg_desc" with
        |Some [TStr "class_item"]->
            Cl_attribut(Tl_var(node.name, !(node.body)))
        |Some [TStr "class_type_item"]->
            Cl_attribut(Tl_constraint(node.name, !(node.body))) 
        |_->bad_cnode "c_ast_to_cl_elmt Cl_attribut"    
    )      
    |Some [TStr "Cl_method"]->(
        let f_visibility = match node.header with 
            |"public"->Tl_public
            |"private"->Tl_private
            |_->not_define "c_ast_to_cl_elmt Cl_method f_visibility"
        in               

        match node.meta#get_value "plg_desc" with
        |Some [TStr "class_item"]->
            Cl_method(Tl_fun(node.name, !(node.body)), f_visibility)  
        |Some [TStr "class_type_item"]->
            Cl_method(Tl_constraint(node.name, !(node.body)), f_visibility) 
        |_->bad_cnode "c_ast_to_cl_elmt Cl_method"                             
    )                                
    |Some [TStr "Cl_init"]->Cl_init(!(node.body))
    |_->bad_cnode "c_ast_to_cl_elmt"                             
)
and c_node_to_tl_ast=function
|Nil -> Tl_none
|Node node->(          
    match node.meta#get_value "plg_ast" with
    |None -> not_define "bad core tree"
    |Some [TStr "Tl_open"]->
        Tl_open(String.split_on_char '.' node.name, !(node.body))
    |Some [TStr "Tl_var"]->
        Tl_var(node.name, !(node.body))
    |Some [TStr "Tl_constraint"]->
        Tl_constraint(node.name, !(node.body))
    |Some [TStr "Tl_fun"]->
        Tl_fun(node.name, !(node.body))
    |Some [TStr "Tl_exception"]->
        Tl_exception(node.name, !(node.body))      
    |Some [TStr "Tl_type"]->
        Tl_type( List.map c_type_to_tl_type node.children, !(node.body))
    |Some [TStr "Tl_module"]->
        Tl_module(node.name, c_ast_to_tl_ast node.children)
    |Some [TStr "Tl_sign"]->
        Tl_sign(node.name, c_ast_to_tl_ast node.children)           
    |Some [TStr "Tl_module_constraint"]->(
        let m_t, m = match split_c_constraint "module" node.children with 
            |m_t::[], m::[]->m_t, m
            |_->not_define "kfsdh"
        in
        Tl_module_constraint(node.name, c_node_to_tl_ast m, c_node_to_tl_ast m_t)  
    )
    |Some [TStr "Tl_functor"]->
        Tl_functor(node.name, node.header, c_ast_to_tl_ast node.children)
    |Some [TStr "Tl_recmodule"]->
        Tl_recmodule(c_ast_to_tl_ast node.children, !(node.body))
    |Some [TStr "Tl_class"]->( 
        let f_virt = match (node.meta)#get_value "plg_virt" with
        |Some [TStr "true"]->true
        |Some [TStr "false"]->false
        |_->not_define "not def" in
        
        let self_v = match (node.meta)#get_value "plg_self" with
        |Some([TStr ""])->None
        |Some([TStr s])->Some s             
        |_->not_define "cnc" in

        let c_t, c = split_c_constraint "class" node.children in

        Tl_class({
            name=node.name;
            header=node.header;
            virt=f_virt;
            self=self_v;
            elmts=List.map c_ast_to_cl_elmt c;
            c_elmts=List.map c_ast_to_cl_elmt c_t;  
        })
    )
    |Some [TStr "Tl_class_and"]->
        Tl_class_and(c_ast_to_tl_ast node.children, !(node.body))
    |_->not_define "not yet"                                      
)
and c_ast_to_tl_ast x= List.map c_node_to_tl_ast x
