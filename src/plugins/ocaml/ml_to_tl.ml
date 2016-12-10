open Parsetree
open Asttypes

open Tl_ast
(* TODO When not_define afficher le nom de la structure non supportée*)  
(* ** BEGIN Miscellaneous functions** *)

(**Opens a file and returns its ocaml ast*)
let file_to_ast path = 
  Parse.implementation (Lexing.from_channel (open_in path)) 

let string_to_ast str =
  Parse.implementation (Lexing.from_string str)

let print_ast ast = 
  Printast.implementation (Format.formatter_of_out_channel stdout) ast

(**Returns the substring corresponding to the location loc *)
let get_str_from_location ml = function {Location.loc_start = s ; loc_end = e; 
                                         loc_ghost = _}  ->
  let cs = s.Lexing.pos_cnum and ce = e.Lexing.pos_cnum in 
  String.sub ml cs (ce - cs)

  


(* ***END Miscellaneous functions*)


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
|Ptyp_arrow (_,c_t1,c_t2)->((* print: x->y, c_t1 x and c_t2 y*)
    match ptyp_to_tl name c_t1, ptyp_to_tl name c_t2 with   
    |Tl_constraint(_,v1), Tl_constraint(_,v2)->Tl_constraint(name, v1^" -> "^v2)
    |_,_->not_define "ptyp_to_tl Ptyp_arrow"    
)
|Ptyp_tuple cts->(
    let constraints = List.map (ptyp_to_tl name) cts in
    Tl_constraint( name, (List.fold_left (fun v0 ->(function
        |Tl_constraint(_,v) ->if v0 <> "" then v0^" * "^v else v
        |_->not_define "ptyp_to_tl Ptyp_tuple")
    ) "" constraints)) 
)   
|_-> not_define "Ptyp_* not supported"

let pctf_to_tl {pctf_desc=desc;_}=
match desc with  
|Pctf_val(name, _, _, c_type)->Cl_attribut(ptyp_to_tl name c_type)
|Pctf_method(name, private_f, _, c_type)->(
    Cl_method(ptyp_to_tl name c_type, 
              match private_f with|Private->Tl_private|Public->Tl_public)
)
|Pctf_inherit c_t->(
    match c_t.pcty_desc with
    |Pcty_constr (lg_ident, _)->  
        Cl_inherit(List.fold_left (fun str x->str^x) 
            "" (Longident.flatten lg_ident.txt), None) 
    |_->not_define "pctf_to_tl Pctf_inherit"      
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
        |Pcf_inherit(_, expr, as_str)->(
            match expr.pcl_desc with
            |Pcl_constr(lg_ident, _)->    
                Cl_inherit(List.fold_left (fun str x->str^x) 
                    "" (Longident.flatten lg_ident.txt), as_str)                   
            |_->not_define "class_fields_to_attrs_methods Pcf_inherit"
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
    |Pstr_primitive {pval_name={txt=name;_};pval_type=c_t;_}->
        ptyp_to_tl name c_t
    |Pstr_attribute _->Tl_none      
    |_ -> not_define "Pstr_* not supported" 


(*ml is a string containing the whole file from which ast was created *)
and ast_to_tl_ast ml ast = List.map (struct_to_tl_struct ml) ast
    
(*let string_to_tl_ast ml = List.map (struct_to_tl_struct ml) (string_to_ast ml)*)


(* ***END Conversion from ast to tl_ast*)
  
(* ***BEGIN Printing of a tl_ast*)
let rec class_elmt_to_str head= function
    |Cl_attribut Tl_var(_, value) ->
        Format.sprintf "%s\tval %s\n" head value 
    |Cl_attribut Tl_constraint(_, value)->
        Format.sprintf "%s\tval %s\n" head value
    |Cl_method (m, f_v)->(
        let virtual_str = match f_v with |Tl_private->"private "|_->"" in
         match m with
        |Tl_fun(name, expr)->    
            Format.sprintf "%s\t%smethod %s = %s\n" head virtual_str name expr 
        |Tl_constraint(name, expr)->
            Format.sprintf "%s\t%smethod %s : %s\n" head virtual_str name expr
        |_-> not_define "bad tree class_elmt_to_str Cl_method"       
    )
    |Cl_init body-> Format.sprintf "%s\tinitializer %s\n" head body    
    |Cl_inherit (name,as_str)-> Format.sprintf "%s\tinherit %s %s\n" head name 
          (match as_str with |None->"" |Some s-> "as "^s)
    |_->not_define "baf_tl_ast class_elmt_to_str"                  
and tl_struct_to_str root =function (* root indicate that the element is a top level or insigne module*) 
    |Tl_open(_,s)-> Format.sprintf "%s\n" s
    |Tl_var(_, expr)->Format.sprintf "%s\n" expr 
    |Tl_constraint(name, expr)->(
        if root then Format.sprintf "val %s : %s\n" name expr
        else Format.sprintf "%s\n" expr
    )           
    |Tl_fun(_, expr)->Format.sprintf "%s\n" expr
    |Tl_exception(_, values)->Format.sprintf "%s\n" values    
    |Tl_type(_, value)->Format.sprintf "%s\n" value
    |Tl_sign(name,ast)->Format.sprintf "module type %s = sig\n%send\n" name (tl_ast_to_str ast)
    |Tl_module(name, ast)-> Format.sprintf "module %s = struct\n%send\n" name (tl_ast_to_str ast) 
    |Tl_module_constraint(name, Tl_module(_,m), Tl_sign(_,mt))->(
        Format.sprintf "module %s : sig\n\
          %s\
          end = struct\n\
          %s\
          end\n" name (tl_ast_to_str mt) (tl_ast_to_str m)      
    )
    |Tl_recmodule(modules,_)->(
       let str = (List.fold_left (fun str x->(
            let _str = tl_struct_to_str true x in
            let tmp = String.sub _str 7 (String.length _str -7) in (*removed module*)                 
            str^tmp^"and " 
        )) "module rec " modules) in

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
    |Tl_class_and(cls,_) -> List.fold_left (fun head cl -> Format.sprintf "%s%s" head  (tl_struct_to_str false cl)) "" cls 
    |Tl_none -> ""
    |_->failwith "Bad tree"               

and tl_ast_to_str tl = String.concat "" (List.map (tl_struct_to_str true) tl)

let name_to_path name= List.fold_left Filename.concat "" (String.split_on_char '@' name)

     (**all roots are files*)                         
let tl_ast_to_files tl=
    List.concat (List.map (function
        |Tl_module(name, ast) -> [(name_to_path name)^".ml", ast] 
        |Tl_sign(name, ast) -> [(name_to_path name)^".mli", ast]
        |Tl_module_constraint (name, Tl_module(_, ast0), Tl_sign(_,ast1))->(
            let path = name_to_path name in 
            [path^".ml",ast0;path^".mli",ast1]
        )
        |_->not_define "tl_ast_to_files"
    ) tl)

(*TODO make file*)
let write_file path (relative_path, ast)= 
    Printf.printf "Writing file %s|%s\n" relative_path path;
    let location = Filename.concat path relative_path in
    let dir = Filename.dirname location in
    
    Utility.mkdir dir 0o740;

    let fd = open_out location in            
    Printf.fprintf fd "%s\n" (tl_ast_to_str ast);
    close_out fd

let tl_ast_to_folder path tl_ast=
    Printf.printf "tl_ast_to_folder %s\n" path;
    let files = tl_ast_to_files tl_ast in
    Printf.printf "%d %d\n" (List.length tl_ast) (List.length files);  
    List.iter (write_file path) files

let print_tl_ast tl = Printf.printf "%s\n" (tl_ast_to_str tl)


(* ***END Printing of a tl_ast*)


(* ***BEGIN Some functions to test more easily *)

let ast_from_string s = Parse.implementation (Lexing.from_string s) 
let str_to_tl_ast s = ast_to_tl_ast s (ast_from_string s)
let str_to_tl_struct s = List.hd (str_to_tl_ast s)

(* ***END Some functions to test more easily *)

(** 
*   @param (rule number, entry)
* *)                  

let path_to_name project_path path=
    let rec _path_to_name path = 
    if path = "" || path = "." || path = "/" || path = "\\" then ""
    else( 
        let name = Filename.basename path and dir = Filename.dirname path in
          
        let prefix = (if dir <> path then _path_to_name dir else dir) in
        if name <> "" then(
            (if prefix <> "" then prefix^"@" else prefix)^name  
        )else prefix 
    ) in
    assert( not (String.contains path '@'));   

    let len0 = String.length project_path and len1 = String.length path in
    assert(project_path = (String.sub path 0 len0));  
    _path_to_name (Filename.remove_extension (String.sub path len0 (len1-len0)))  

    

let entry_to_tl_struct project_path=function(*il faudrait un truc comme un numero de rule*)
|0,ml::mli::[] ->(
    let name = path_to_name project_path ml in
    Tl_module_constraint(name, 
        Tl_module(name, str_to_tl_ast (Utility.file_to_string ml)), 
        Tl_sign(name, str_to_tl_ast (Utility.file_to_string mli)))
 )
|1,ml::[] ->(
    let name = path_to_name project_path ml and body= str_to_tl_ast(Utility.file_to_string ml) in
    Tl_module(name,body)

 ) 
|n,l when n<3 ->not_define (Printf.sprintf "%d %d" n (List.length l))              
|_->not_define "rule_to_tl_ast bad rule"   

     
let entries_to_tl_ast project_path (num_rule,entries)=
    List.map (fun e->entry_to_tl_struct project_path (num_rule,e)) entries    
            
open OUnit2
let tests_tl_ast_to_str = [
    ("constant", "let t = 1", "let t = 1\n");
    ("function", "let f x = x", "let f x = x\n");
    ("val", "val x : int", "val x : int\n");
    ("type", "type 'a tree = Nil | Node of 'a tree * 'a",
            "type 'a tree = Nil | Node of 'a tree * 'a\n");
    ("class", "class hello = object(self) \
                    val hello:string=\"hello\" \
                    val alpha = 12 \
                    val arf = ref true \
                    method set (key:string) = 12 \
                    initializer(arf:=false) \
                end",
                "class hello = object(self)\n\
                    \tval hello:string=\"hello\"\n\
                    \tval alpha = 12\n\
                    \tval arf = ref true\n\
                    \tmethod set = set (key:string) = 12\n\
                    \tinitializer (arf:=false)\n\
                end\n");
    ("module", "module rec Even : sig \
            type t = Zero | Succ of Odd.t \
        end = struct \
            type t = Zero | Succ of Odd.t \
        end \
        and Odd : sig \
            type t = Succ of Even.t \
        end = struct \
            type t = Succ of Even.t \
        end", "module rec Even : sig\n\
            type t = Zero | Succ of Odd.t\n\
            end = struct\n\
            type t = Zero | Succ of Odd.t\n\
            end\n\
            and Odd : sig\n\
            type t = Succ of Even.t\n\
            end = struct\n\
            type t = Succ of Even.t\n\
            end\n");
] 
let tests ()=
    "ml_to_tl">:::[
        "path_to_name">:::[
            "default">::(function _->( assert_equal
                (path_to_name "" "charlie.ml")
                "charlie"                        
            ));                            
            "file">::(function _->( assert_equal
                (path_to_name "" "alpha/tango/bravo/charlie.ml")
                "alpha@tango@bravo@charlie"                        
            ));
            "dir">::(function _->( assert_equal
                (path_to_name "" "alpha/tango/bravo/charlie")
                "alpha@tango@bravo@charlie"                     
            ));
            "absolu">::(function _->( assert_equal
                (path_to_name "" "/alpha/tango/bravo/charlie")
                "alpha@tango@bravo@charlie"
            ));
            "project">::(function _->( assert_equal
                (path_to_name "/alpha/tango" "/alpha/tango/bravo/charlie")
                "bravo@charlie"
            ));
            "project_files">::(function _->( assert_equal
                (path_to_name "alpha/tango/charlie.ml" "alpha/tango/charlie.ml")
                ""
            ));                                   
        ];
        "tl_ast_to_str">:::( List.map (function name, input, output->
            name>::(function _->(
                assert_equal (tl_ast_to_str (str_to_tl_ast input)) output
                (*assert_equal (let t=(tl_ast_to_str (str_to_tl_ast input)) in Printf.printf "#%s#\n" t;t) (let t=output in Printf.printf "@%s@\n" t;t)*)
            ))      
        ) tests_tl_ast_to_str)

        ]
