open Parsetree
open Asttypes

exception Not_define of string
let not_define msg = raise (Not_define( "src/tl_ast.ml : "^msg^"\n" )) 
(** TODO
  * const ref de ml
  * rec-module, and-module, foncteur
  * same for signature
  * extand class
  * basic-left pattern
  *
  * rewrite and factorize
  *)


type tl_visibility = Tl_private|Tl_public
 
type tl_struct =  
|Tl_none (*to be removed*)
|Tl_open of string list * string  
|Tl_var of string * string
|Tl_fun of string * string
|Tl_exception of string * string
|Tl_type of string list * string
|Tl_module of string * tl_ast (*TODO : foncteur*)
|Tl_sign of string * tl_ast(*en fait une liste de type*)
|Tl_constraint of string * tl_struct* tl_struct
|Tl_functor of string * string * tl_ast                                      
|Tl_recmodule of tl_ast * string                                        
|Tl_class of {name:string; header:string; virt:bool; self:string option; elmts:class_elmt list}(*name, header, vitual?, methods : (function, visibility), attribut*)
(* Not truly supported yep*)
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
  * @return caml class_struct, header
  *)
let rec get_class_struct (_:Lexing.position) = function 
    |Pcl_fun(_,_,pattern, child_ast)->( 
       let loc_end =  pattern.ppat_loc.Location.loc_end in
       get_class_struct loc_end child_ast.pcl_desc(*on est entrain de parser les arguments de la classe, child => child ast*)
    )
    |Pcl_structure(ast)->(
       ast.pcstr_self.ppat_loc.Location.loc_start, ast 
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

let class_to_tl_class ml = function({pci_virt=virt; pci_params=_; 
                                    pci_name=name; pci_expr=expr; pci_loc=loc;
                                    pci_attributes=_}:class_declaration)->
    
    let loc_start = expr.pcl_loc.Location.loc_start in
    let header_end, struct_ast = get_class_struct loc_start expr.pcl_desc in    
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
        header = get_str_from_location ml (
            {Location.loc_start=loc.Location.loc_start; loc_end=header_end; 
            loc_ghost=false}
        );
        virt = (virt == Virtual);
        self = self;
        elmts = elmts;
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
                Tl_constraint(name, (pmod_to_tl args m.pmod_desc), 
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
              (*TODO il faudra créer un autre type caml pour en capsuler les types et non utiliser tl_type : type defini à top level*)                             
            |_-> not_define "Pmty_* not supported" 
        )           
    in
      

    match struct_item with 
    |Pstr_open open_desc ->  
        Tl_open(open_description_to_string_list open_desc.popen_lid, body)
    |Pstr_value(_,  value::_)->((*pour l'instant on ne traite que la première*)
        match value.pvb_pat.ppat_desc  with 
        |Ppat_var {txt=name;_}-> pexp_to_tl name body value.pvb_expr
           (* match value.pvb_expr.pexp_desc with
            | Pexp_function _ | Pexp_fun _-> Tl_fun(name, body)   
            | _ -> Tl_var(name, body)   
            *)(*comment faire avec les autrs patterns???*)
        |_->Tl_none                  
    )
    |Pstr_exception {pext_name={txt=name;_};_}->
        Tl_exception( name, body)
    |Pstr_type(_,t_dcls)-> type_declarations_to_tl t_dcls body
    |Pstr_class decls->(
        let cls = (List.map (function d->class_to_tl_class ml d) decls) in
        Tl_class_and(cls, body)
    )
    |Pstr_module m -> pmod_to_tl m.pmb_name m.pmb_expr.pmod_desc
    |Pstr_modtype mt -> pmty_to_tl mt.pmtd_name.txt mt.pmtd_type
    |Pstr_recmodule m_list ->(
        Tl_recmodule(List.map (function m->pmod_to_tl m.pmb_name 
             m.pmb_expr.pmod_desc) m_list, body)

    )   
    |_ -> Tl_none


(**TODO : faire une nouvelle passe de simplification vers un nouvel ast **)


(*ml is a string containing the whole file from which ast was created *)
and ast_to_tl_ast ml ast = List.map (struct_to_tl_struct ml) ast
    
let string_to_tl_ast ml = List.map (struct_to_tl_struct ml) (string_to_ast ml)


(* ***END Conversion from ast to tl_ast*)
  

(* ***BEGIN Printing of a tl_ast*)
let rec class_elmt_to_str head= function
    |Cl_attribut attr ->  Format.sprintf "%sval %s|" head (tl_struct_to_str attr)  
    |Cl_method (m, f_v)->( Format.sprintf "%s%s method %s" head 
        (match f_v with |Tl_private->"private "|_->"") 
        (tl_struct_to_str m)
    )
    |Cl_init body-> Format.sprintf "%sinitializer |%s|" head body              
and tl_struct_to_str =function 
    |Tl_open(_,s)-> Format.sprintf "%s\n" s
    |Tl_var(_, expr)->Format.sprintf "%s\n" expr 
    |Tl_fun(_, expr)->Format.sprintf "%s\n" expr
    |Tl_exception(_, values)->Format.sprintf "%s\n" values    
    |Tl_type(_, value)->Format.sprintf "%s\n" value
    |Tl_sign(name,ast)->Format.sprintf "module type %s = sig\n%send\n" name (tl_ast_to_str ast)
    |Tl_module(name, ast)-> Format.sprintf "module %s = struct\n%send\n" name (tl_ast_to_str ast) 
    |Tl_constraint(name, Tl_module(_,m), Tl_sign(_,mt))->(
        Format.sprintf "module %s : sig\n\
          %s\
          end = struct \n\
          %s\
          end\n" name (tl_ast_to_str mt) (tl_ast_to_str m)      
    )
    |Tl_functor(_, header, next)->(
        Format.sprintf "module %s struct\n%send\n" header (tl_ast_to_str next)
    )
    |Tl_class cl ->(
       let elmts_str = List.fold_left (fun head elmt -> (class_elmt_to_str head elmt)) "" cl.elmts in
       let self_str = (match cl.self with |None->"" |Some(s)->"("^s^")" )in 
         Format.sprintf "%s%s\n%s\nend\n" cl.header self_str elmts_str  
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
