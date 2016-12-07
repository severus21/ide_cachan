open Cparse
open Genlab
open Printf
open Pervasives

(*
	TODO : réécrire environnement
*)

(*
	Convention sur les registres : 
		rax -
			valeur de retour des fonctions ( eax pour std )
			stocke temporarirement la valeur d'une expression lors 
			de son évaluation 
		r10 - utilisé pour les set ( intervenant comme registre 
			temporaire)
		r11 - reservé pour les set_array ( intervenant comme registre 
			temporaire)
		r12 - reservé pour l'addressage indirect ( intervenant comme 
			registre temporaire)
		r13 - identifiant de l'exception (0 pas d'exception) 
		r14 - valeur de l'exception
		r15 - 0 defaut, 1 il y a eu un return exécuté dans un code fils 
			d'un finally
*)

let listing = 
object
    val a_listings = ref [] (*code principal*)
    val b_listings = ref [] (*permet la rotation*)
	(*
		@param line - un ligne asm a ajouté au code principal
	*)
    method add (line:string) =(
		a_listings := line :: !a_listings
    )
    
    method rotate =(
		b_listings := !a_listings;
		a_listings := []; 
    )
    
    method link (line:string)=(
		b_listings := line :: !b_listings;
		a_listings := (!a_listings @ !b_listings ); (*une liste doublement chainée devrait être utilisée.....*)
		b_listings := [];
    )
    
    method no_return=(
		match !a_listings with
		| [] -> false
		| h::t when h = "  retq" -> false
		| _-> true
    )
    
    method write out=(
		let rec write_aux =function
			|[]-> ()
			|h::t->(
				fprintf out "%s\n" h;
				write_aux t;
			)
		in    
		write_aux (List.rev !a_listings)
    )
end;;

exception Const_descriptor ;;
exception NIL_founded ;;
exception Cfun_forbidden;;
exception Heap_already_declared;;
exception Not_an_array;;
exception No_inc;;
exception No_dec;;
exception Undefined;;
exception CompilationFailed;;

let locator2str (nom, fl, fc, ll, lc) = 
	"file="^nom^" first-line="^(string_of_int fl)^"  first-column="
	^(string_of_int fc)^" last-line="^(string_of_int ll)^" last-column="
	^(string_of_int fl)
;;

(*Written by ping*)
let print_error error_message (file, start_line, start_col, end_line, end_col) =
  printf "Error: %s" error_message;
  let chan = open_in file in

  let rec get_lines i list_of_lines =(
    let current_line = input_line chan in
	
	if (i < start_line) then get_lines (i+1) list_of_lines
	else if (i <= end_line) then(
		get_lines (i+1) (list_of_lines @ [current_line])
	)else list_of_lines
  ) in
  
  let lines = get_lines 1 [] in
  let number_of_lines = start_line - end_line + 1 in
  
  List.iteri (fun i line ->
    if (number_of_lines = 1) then
      printf "%d:\t%s\027[1;31m%s\027[0m%s" start_line (String.sub line 0 (start_col - 1))
      (String.sub line (start_col - 1) (end_col-1)) (String.sub line (end_col + 1) ((String.length line) - end_col - 1))
    else if (i = 0) then
      printf "%d:\t%s\027[1;31m%s\027[0m" start_line (String.sub line 0 (start_col - 1))
      (String.sub line (start_col - 1) ((String.length line) - start_col + 1))
    else if ((i+1) < number_of_lines) then
      printf "%d:\t\027[1;31m%s\027[0m" (start_line + i) line
    else
      printf "%d:\t\027[1;31m%s\027[0m%s" (start_line + i) (String.sub line 0 end_col)
      (String.sub line end_col ((String.length line) - end_col - 1))
  ) lines;
  printf "%s" ""
 ;;

(*Ajoute un commentaire ASM*)
let add_comm comm=
	listing#add  ("#" ^ comm )
;;

(* 
	@description - associe un message d'erreur aux exceptions personnalisés
*)
let excep_handler locator excep=
	let msg = 
	match excep with
	| Const_descriptor -> "it's an attempt to alter constant"
	| NIL_founded -> "empty argmunent not allowed  : f(1, ,3)"
	| Cfun_forbidden -> "bad function declaration"
	| Heap_already_declared -> "global var already declared"
	| Not_an_array -> "expr is not an array" 
	| No_inc -> "can not increment such an expr"
	| No_dec -> "can not decrement such an expr"
	| Not_found -> "var or cst not declared"	
	| _ -> raise excep
	in
	print_error msg locator;
	raise CompilationFailed;
;;

(* 
	@description - genère un nom unique
*)
let generate_name = let counter = ref 0 in fun () ->( 
	incr counter ; 
	"1_"^(string_of_int !counter ) 
);;

(*
	@param () -
	@description - retourne un nouveau label :
		( nom, header asm correspondant)
*)
let new_label = function _->(
	let name = generate_name () in
	(name, "."^name^":")
);;

(*
	stack( offset )	: 
		variable locale, offset de decalage par rapport à rbp
	stack_ind( offset ) : 
		variable locale, offset de decalage par rapport à rbp
		la valeur de la varariable est interprétée comme une adresse
		ne sert que pour S_INDEX
		
		Attention : 
			si set_value x  y avec y stack_ind : x aura la valeur de *y
		
	heap_str( name ) : 
		constante  globale représentant une chaîne de caractère
		name : nom du label
	heap_var : 
		varibale globale entière
	label( name ) : 
		un label
		name : nom du label
	register( name ) : 
		un registre
		name exemple : rax (et non %rax)
	value : 
		une valeur constante ( ex : 0, 51, 42)
	c_std( name ) : 
		une constante de la std
		name : son nom (ex : stdout)
	fct( name) :
		sert exclusivement pour l'optimisation
	ind_register:
		sert exclusivement pour l'optimisation
*)
type descriptor = NIL | C_STD of string | STACK of int 
| STACK_IND of int | HEAP_STR of string | HEAP_VAR of string 
| LABEL of string | REGISTER of string | VALUE of int
| FCT of string | IND_REGISTER of string;; 

class environnement ?(current_map=(Hashtbl.create 10) ) ()=
  object
    val map = current_map
    (*
		@param key - nom d'une variable( locale, ou globale)
		@description - retourne un descriptor décrivant le contenu de la
			variable
	*)
    method get (key:string) =(
		Hashtbl.find map key 

	)
	(*
		@param key - nom d'une variable( locale, ou globale)
		@param value - un descriptor décrivant le contenu de la
			variable
		@description - crée un nouvel environnement avec une nouvelle 
			entrée (key, value)
	*)
    method set (key:string) (value:descriptor) =(
		let tmp = Hashtbl.copy map in 
		Hashtbl.add tmp key value ;

		new environnement ~current_map: tmp ()
    )
    (*
		@param key - nom d'une variable( locale, ou globale)
		@param value - un descriptor décrivant le contenu de la
			variable
		@description - ajoute une nouvelle entrée (key, value) à 
			l'environnement courant
	*)
    method set2 (key:string) (value:descriptor) =(
		Hashtbl.add map key value ;     
    )
    initializer(
		Hashtbl.add map "NULL" (VALUE 0) ;
		Hashtbl.add map "EOF"  (VALUE (-1)) ;
		Hashtbl.add map "stdout" (C_STD "stdout") ;
		Hashtbl.add map "stderr" (C_STD "stderr") ;
		Hashtbl.add map "stdin"  (C_STD "stdin");
    )
end;;


let my_functions = 
  object
    val map = Hashtbl.create 10
    (*
		@param key - nom d'une fonction
		@description - retourne false si c'est une fonction de la std
								true sinon
	*)
    method get (key:string) =(
		try(
			let _ = Hashtbl.find map key in
			true
		)with
		|Not_found -> false
	)
	(*
		@param key - nom d'une fonction
		@description - ajoute la fonction (key, true) à l'objet courant
	*)
    method set (key:string)=(
		Hashtbl.add map key true ;     
    )
  end;;

let heap= (* décrit le contenu du tas*)
  object( self )
    val map_str = Hashtbl.create 10
    val map_int = Hashtbl.create 10
    val mutable buff = ""
    (*
		@param key : nom de la variable global
		@param value : chaîne de caractère associée
		@descritpion ajoute (key,value) au tas
    *)
    method add_str (key:string) (value:string) =(
		try(
		  let _ = Hashtbl.find map_str key in
			raise Heap_already_declared;
		)with
		|Not_found->Hashtbl.add map_str key value;
    )
    (*
		@param key - nom de la variable global
		@descritpion - ajoute (key,0) au tas
    *)
    method add_int (key:string) =( 
		try(
		  let _ = Hashtbl.find map_int key in
			raise Heap_already_declared;
		)with
		|Not_found->Hashtbl.add map_int key 0;
    ) 
    method private build_str_entry (key:string) (value:string)=(
		buff <- (buff^"."^key^":\n  .string \""^( String.escaped  value )
		^"\"\n  .align 8\n");
    )
    method private build_int_entry (key:string) (value:int)=(
		buff <- buff^".comm "^key^", 8, 8\n";
    )
    (*
		@description - génère le code asm représentant le tas
    *)
    method build =(
		Hashtbl.iter self#build_str_entry map_str;
		Hashtbl.iter self#build_int_entry map_int;
		buff
    )
  end;;

let e_table = (*table des exceptions*)
  object( self )
    val map = Hashtbl.create 10
    val mutable id	= 0
    (*
		@param name : nom de l'exception
		@descritpion retourne un identifiant unique et constant( pour chaque nom)
    *)
    method get (name:string) =(
		try(
			Hashtbl.find map name
		)with
		|Not_found->(
			id <- id + 1;
			Hashtbl.add map name id;
			id
		)
    )
end;;

(*
	@param descr_dest - un descriptor representant l'objet destination
	@param descr_source - un descriptor representant l'objet source
	@description destination = valeur_de_source
		
		Attention : 
		cette fonction suppose que (la source ou la destination) est un
		registre.
*)
let rec set_valid_value descr_dest descr_source=
	let source =(
		match descr_source with
		| NIL				-> "$1" (* permet le for(;;)*)
		| STACK( offset ) 	-> "-"^(string_of_int offset)^"(%rbp)"
		| STACK_IND( offset)->(
			set_value (REGISTER "r12") (STACK offset);
			"(%r12)" (*(%r12) possède la valeur de *p *)
		) 
		| C_STD( name )		-> name 
		| HEAP_STR( name )	-> "$."^name
		| HEAP_VAR( name ) 	-> name^"(%rip)" (*see gcc*)
		| LABEL( name )		-> "."^name
		| REGISTER( name )	-> "%"^name
		| VALUE( new_val )	-> "$"^(string_of_int new_val)
		|_-> raise Undefined
	) in

	let dest =(
		match descr_dest with
		| NIL				-> raise Const_descriptor
		| STACK( offset ) 	-> "-"^(string_of_int offset)^"(%rbp)"
		| STACK_IND( offset)->(
			set_value (REGISTER "r12") (STACK offset);
			"(%r12)"
		) 
		| C_STD( _ )		-> raise Const_descriptor
		| HEAP_STR( name )	-> raise Const_descriptor
		| HEAP_VAR( name ) 	-> name^"(%rip)" (*see gcc*)
		| LABEL( name )		-> raise Const_descriptor
		| REGISTER( name )	-> "%"^name
		| VALUE( new_val )	-> raise Const_descriptor
		| _ -> raise Undefined
	) in 
	
	listing#add ("  movq "^source^", "^dest);
	
(*
	@param descr_dest - un descriptor representant l'objet destination
	@param descr_source - un descriptor representant l'objet source
	@description pareil que set_valid_value mais ne nécessite pas que l'un
		des descripteurs soit un registre
*)
and set_value descr_dest descr_source=
	match descr_dest, descr_source with
	| REGISTER( name ), _ | _, REGISTER( name ) ->(
		set_valid_value descr_dest descr_source
	)
	| _, _ ->(
		set_valid_value (REGISTER "r10") descr_source;
		set_valid_value descr_dest (REGISTER "r10") ;
	);
;;

(*
	@param descr - un descripteur
	@description - empile la valeur du descripteur sur la pile		
		Remarque :
		cette fonction sert exclusivement au passage de parametres
*)
let push descr =
	let source =(
		match descr with
		| NIL				-> raise NIL_founded (*pas de f(1, ,2*)
		| STACK( offset ) 	-> "-"^(string_of_int offset)^"(%rbp)"
		| STACK_IND( offset)->(
			set_value (REGISTER "r12") (STACK offset); 
			"(%r12)"(*(%r12) possède la valeur de *p *)
		) 
		| C_STD( name )		-> name
		| HEAP_STR( name )	-> "$."^name
		| HEAP_VAR( name ) 	-> name^"(%rip)" (*see gcc*)
		| LABEL( name )		-> "."^name
		| REGISTER( name )	-> "%"^name
		| VALUE( new_val )	-> "$"^(string_of_int new_val)
		|_ -> raise Undefined
	) in
	
	listing#add ("  pushq "^source);
;;

(*
	@param env - l'environnement courant
	@param descr_root - descripteur décrivant le tableau
	@param descr_offset - descripteur décrivant la case 
	@param descr_value - descripteur décrivant la nouvelle valeur
	@description - root[ offset ] = value
*)
let set_array_value env descr_root descr_offset descr_value=
	(* %rax = r_offset*)
	set_value (REGISTER "rax") descr_offset;
	listing#add ("  imulq $8, %rax");
	
	(* %rax = r_offset + root_offset*)
	begin
	match descr_root with
	| STACK( _ ) | HEAP_VAR( _ ) | STACK_IND( _) ->(
		set_value (REGISTER "rbx") descr_root;
		listing#add "  addq %rax, %rbx";
	)
	| _	-> raise Not_an_array
	end;
	
	set_value (REGISTER "r11") descr_value;
	listing#add ("  movq %r11, (%rbx)");
;;

(*
	@param env 		- environnement du scope parent
	@param global	- si on est dans le scope principal( ou non)
	@param offset 	- offset de decalage par rapport à rbp( ie position 
		de la nouvelle écriture
	@param			- var_declaration list
	@description		- 
		resout les problèmes de portée avec l'environnement parent
	@return nouvel environnement, la queue de list non analysée( si fonction rencontrée), nouvel offset
*)
let rec hydrate_env (env:environnement) (global:bool) (offset:int) =function
	| []-> (env, offset, [])
	| CDECL(loc, name)::t 	->(
		if global then(
			heap#add_int name;
			hydrate_env ( env#set name (HEAP_VAR name) ) global offset t;
		)
		else (hydrate_env ( env#set name (STACK offset) ) global (offset+8) t); 
	)
	| (CFUN(_) as fct)::t ->(
		(env, offset, fct::t)
	)
;;

(*
	@param env 		- environnement du scope parent
	@param 			- arbre abstrait à partir de la première fct rencontrée
	@param			- var_declaration list
	@description	- initialise l'environnement avec les variables globales,
		d'après la sémantique du c-- on peut trouver les variables globales 
		utiles à une fontion après la déclaration de celle-ci
	@return nouvel environnement, liste des fcts
*)
let rec hydrate_global (env:environnement) acc=function
	|[]		-> (env, List.rev acc)
	|decls 	->(  
		let new_env, _, next_decls = (hydrate_env env true 8 decls) in
		
		begin
		match next_decls with
		| (CFUN(_,_,_,_) as fct)::t ->(
			hydrate_global new_env (fct::acc) t
		)
		| _ -> hydrate_global new_env acc next_decls
		end;
	)
;;

(*
	@param env - environnement du scope parent
	@param last_offset - offset de decalage par rapport à rbp( ie position 
		de la nouvelle écriture
	@param op_code - see.cparse.ml mon_op
	@param expr - see.cparse.ml expr
	@return - (un descripteur pointant vers la valeur de l'expression, 
		nouvel offset par rapport à rbp)
	@description - evalue une op1_expression, ajoute le code assembleur au listing
	@remarque - le scope est constant lors de l'éxecution de la fonction
*)
let rec compile_op1 env last_offset end_name op_code expr=
	let descr, offset	= ( compile_expr env last_offset end_name expr ) in
	set_value (REGISTER "rax") descr;
	
	match op_code with
	| M_MINUS	->(
		listing#add "  neg %rax";
		set_value (STACK offset ) (REGISTER "rax");
		( (STACK offset ) , offset+8);
	)
	| M_NOT		->(
		listing#add "  not %rax";
		set_value (STACK offset ) (REGISTER "rax");
		( (STACK offset ) , offset+8);
	)
	| M_POST_INC->(
		set_value (STACK offset ) (REGISTER "rax");
		listing#add "  addq $1, %rax";
		
		begin
		match descr with
			|STACK( _ ) | STACK_IND( _ ) | HEAP_VAR( _ )	-> set_value descr (REGISTER "rax")
			|_->raise No_inc
		end;
		
		(STACK( offset ), offset+8);
	)
	| M_POST_DEC->(
		set_value (STACK offset ) (REGISTER "rax");(*save valeur à retourner*)
		listing#add "  subq $1, %rax";
		
		begin
		match descr with
			|STACK( _ ) | STACK_IND( _ ) | HEAP_VAR( _ )	-> set_value descr (REGISTER "rax")
			|_->raise No_dec
		end;

		( (STACK offset ), offset+8);
	)
	| M_PRE_INC ->(
		listing#add "  addq $1, %rax";

		begin
		match descr with
			|STACK( _ ) | STACK_IND( _ ) | HEAP_VAR( _ )	-> set_value descr (REGISTER "rax")
			|_->raise No_inc
		end;
		
		set_value (STACK offset ) (REGISTER "rax");
		((STACK offset ), offset+8);
		
	)
	| M_PRE_DEC	->(
		listing#add "  subq $1, %rax";
		
		begin
		match descr with
			|STACK( _ ) | STACK_IND( _ ) | HEAP_VAR( _ )	-> set_value descr (REGISTER "rax")
			|_->raise No_inc
		end;
		
		set_value (STACK offset ) (REGISTER "rax");
		((STACK offset ), offset+8);
		
	)
	
(*
	@param env - environnement du scope parent
	@param last_offset - offset de decalage par rapport à rbp( ie position 
		de la nouvelle écriture
	@param op_code - see.cparse.ml bin_op
	@param expr1 - see.cparse.ml expr
	@param expr2 - see.cparse.ml expr
	@return - (un descripteur pointant vers la valeur de l'expression, 
		nouvel offset par rapport à rbp)
	@description - evalue une op2_expression, ajoute le code assembleur au listing
	@remarque - le scope est constant lors de l'éxecution de la fonction
*)	
and compile_op2 env last_offset end_name op_code expr1 expr2=
	let descr2, offset2	= ( compile_expr env last_offset end_name expr2 ) in
	let descr1, offset	= ( compile_expr env offset2 end_name expr1 ) in
	
	set_value (REGISTER "rax") descr1;
	set_value (REGISTER "rbx") descr2;
	
	let new_descr =( 
	match op_code with
		| S_MUL		->(
			listing#add "  imulq %rbx, %rax";
			set_value (STACK offset ) (REGISTER "rax");
			(STACK offset )
		)
		| S_DIV 	->(
			listing#add "  cqto";
			listing#add "  idivq %rbx";
			set_value (STACK offset ) (REGISTER "rax");
			(STACK offset )
		)
		| S_MOD 	->(
			listing#add "  cqto";
			listing#add "  idivq %rbx";
			
			set_value (REGISTER "rax") (REGISTER "rdx");
			set_value (STACK offset ) (REGISTER "rax");
			(STACK offset )
		)
		| S_ADD 	->(
			listing#add "  addq %rbx, %rax";
			set_value (STACK offset ) (REGISTER "rax");
			(STACK offset )
		)
		| S_SUB 	->(
			listing#add "  subq %rbx, %rax";
			set_value (STACK offset ) (REGISTER "rax");
			(STACK offset )
		)
		| S_INDEX	->(
			(* %rbx = r_offset*)
			set_value (REGISTER "rbx") descr2;
			listing#add ("  imulq $8, %rbx");
			
			(* %rbx = r_offset + root_offset*)
			begin
			match descr1 with
			| STACK( _ ) | HEAP_VAR( _ ) | STACK_IND ( _ ) ->(
				set_value (REGISTER "rax") descr1;(*rax = t[0] ie p*)
				listing#add "  addq %rbx, %rax";
			)
			| _	-> raise Not_an_array
			end;
			
			(*%rax = address de l'elmt du tableau*)
			set_value (STACK offset ) (REGISTER "rax");
			(STACK_IND offset)
		)
	)in
	(new_descr, offset+8)
	
(*
	@param env - environnement du scope parent
	@param last_offset - offset de decalage par rapport à rbp( ie position 
		de la nouvelle écriture
	@param op_code - see.cparse.ml cmp_op
	@param expr - see.cparse.ml expr
	@return - (un descripteur pointant vers la valeur de l'expression, 
		nouvel offset par rapport à rbp)
	@description - évalue une cmp_expression, ajoute le code assembleur au listing
	@remarque - le scope est constant lors de l'éxecution de la fonction
*)	
and compile_cmp env last_offset end_name op_code expr1 expr2=
	let descr2, offset2	= ( compile_expr env last_offset end_name expr2 ) in
	let descr1, offset	= ( compile_expr env offset2 end_name expr1 ) in
	
	set_value (REGISTER "rax") descr1;
	set_value (REGISTER "rbx") descr2;
	begin
		match op_code with
		| C_LT ->(
			listing#add "  cmpq %rbx, %rax";
			listing#add "  setl %al";
			listing#add "  movsbq %al, %rax";

		)
		| C_LE ->(
			listing#add "  cmpq %rbx, %rax";
			listing#add "  setle %al";
			listing#add "  movsbq %al, %rax";

		)
		| C_EQ ->(
			listing#add "  cmpq %rbx, %rax";
			listing#add "  sete %al";
			listing#add "  movsbq %al, %rax";
		)
	end;
	set_value (STACK offset ) (REGISTER "rax");
	((STACK offset ), offset+8)

(*
	@param env - environnement du scope parent
	@param last_offset - offset de decalage par rapport à rbp( ie position 
		de la nouvelle écriture
	@param expr - see.cparse.ml expr
	@return - (un descripteur pointant vers la valeur de l'expression, 
		nouvel offset par rapport à rbp)
	@description - évalue une expression, ajoute le code assembleur au listing
	@remarque - le scope est constant lors de l'éxecution de la fonction
*)	
and compile_expr env last_offset end_name=function
	|( locator, expr)->(
		try
		begin
		match expr with
		| VAR( name)->(
			(env#get name, last_offset)
		)
		| CST( i_value)->(
			(VALUE i_value, last_offset)
		)
		| STRING( s_value )->(		
			let heap_name = generate_name () in
			heap#add_str heap_name s_value;
			(HEAP_STR heap_name, last_offset)
		)
		| SET_VAR( name, value_expr)->(
			let (value_descr, new_offset) = ( compile_expr env last_offset end_name value_expr) in
			set_value (env#get name) value_descr;
			(value_descr, new_offset)
		)
		| SET_ARRAY( name, key_expr, value_expr)->(
			let (value_descr, new_offset1)	= ( compile_expr env last_offset  end_name value_expr ) in
			let (key_descr, new_offset2)	= ( compile_expr env new_offset1 end_name key_expr ) in
			
			set_array_value env (env#get name) key_descr value_descr;
			(value_descr, new_offset2)
		)
		| CALL(name, exprs)->( 
			let rec local_compile_exprs offset end_name descrs=function	 (*n° de l'argument*)
			|[], n	-> offset, descrs
			|h::t, n	->(
				let descr, offset1 = ( compile_expr env offset end_name h ) in
				let new_offset	= ref offset1 in
				
				local_compile_exprs !new_offset end_name (descr::descrs) (t, (n-1));
			) 
			in 
			
			let rec write_args = function
			|[], n	-> ()
			|descr::t, n	->(
				begin
				match n with
				|1-> set_value (REGISTER "rdi") descr;
				|2-> set_value (REGISTER "rsi") descr;
				|3-> set_value (REGISTER "rdx") descr;
				|4-> set_value (REGISTER "rcx") descr;
				|5-> set_value (REGISTER "r8") descr;
				|6-> set_value (REGISTER "r9") descr;
				|n-> push descr;
				end;
				
				write_args (t, (n-1));
			) 
			in
			
			let m = List.length exprs in

			(*si std*)
			if (my_functions#get name) = false then(
				set_value (REGISTER "rax") (VALUE 0);
			);		
			
			(*save r13,r14 *)
			set_value (STACK last_offset) (REGISTER "r13");
			set_value (STACK (last_offset+8)) (REGISTER "r14");
			set_value (STACK (last_offset+16)) (REGISTER "r15");
			
			(*arguments*)
			let new_offset, descrs = local_compile_exprs (last_offset+24) end_name [] ( List.rev exprs, m) in
			
			(*alignement*)
			listing#add "  pushq %rsp";
			listing#add "  pushq %rsp";
			listing#add "  andq $-16, %rsp";
			
			write_args (List.rev descrs,m);
			listing#add ("  call "^name);
	
		
			(*clean la pile*)
			if m>6 then(
				listing#add ("  addq $"^(string_of_int ((m-6)*8))^", %rsp");	
			);
			listing#add "  popq %rdi";(*pour simplifier l'optimisation*)
			listing#add "  popq %rsp";
	
			(*restaure r15*)
			set_value (REGISTER "r15") (STACK (last_offset+16));
	
			(* si std fct, restaure r13,r14, : dans tous les cas les exceptions n'existe pas en C*)
			if (my_functions#get name) = false then (
				set_value (REGISTER "r13") (STACK last_offset);
				set_value (REGISTER "r14") (STACK (last_offset+8));
				
				if name <> "malloc" then(
					listing#add "  movslq %eax, %rax"
				);
				
			)else(
				listing#add "  cmp $0, %r13";
				listing#add ("  jne ."^end_name);
			);
			set_value (STACK new_offset ) (REGISTER "rax");	

			(STACK new_offset, new_offset+8)
		)
		| OP1( op_code, expr1)->(
			compile_op1 env last_offset end_name op_code expr1;
		)
		| OP2( op_code, expr1, expr2)->(
			compile_op2 env last_offset end_name op_code expr1 expr2;
		) 
		| CMP( op_code,  expr1, expr2)->(
			compile_cmp env last_offset end_name op_code expr1 expr2;
		)
		| EIF( expr1, expr2, expr3)->(
			let (name_end, header_end) 	= new_label () in 
			let (name3, header3) 		= new_label () in 

		
			let descr1, offset1	= ( compile_expr env last_offset end_name expr1 ) in
			
			set_value (REGISTER "rax" ) descr1; 
			listing#add "  cmpq $0, %rax";
			listing#add ("  je ."^name3);
			
			(*si true*)
			let descr2, offset2	= ( compile_expr env offset1 end_name expr2 ) in
			set_value (STACK offset2 ) descr2; 

			listing#add ("  jmp ."^name_end);
			
			(*si false*)
			listing#add header3;
			let descr3, offset3	= ( compile_expr env (offset2+8) end_name expr3 ) in
			set_value (STACK offset2 ) descr3; 

			
			listing#add header_end;
			((STACK offset2), offset3)
		)
		| ESEQ(exprs)->(	
			begin 
			match exprs with
			|[]-> (NIL, last_offset)
			|h::t->(
				let descr, offset = (compile_expr env last_offset end_name h ) in
				
				begin
				match (compile_expr env offset end_name ( locator, ESEQ t) ) with
				|NIL, offset_end -> (descr, offset_end)
				|descr1, offset_end -> (descr1, offset_end)
				end
			)
			end
		)
	end with | excep -> excep_handler locator excep
	)
;;



(*
	@param env - environnement du scope parent
	@param last_offset - offset de decalage par rapport à rbp( ie position 
		de la nouvelle écriture
	@param finally_label - "" pas finally parent, "..." label du finally parent
	@param  - see.cparse.ml loc_code
	@description - evalution d'un bloc, ajoute le code assembleur au listing
	@return - nouvel offset par rapport à rbp
*)
let rec compile_code env last_offset finally_label=function
	|( locator, code)->(
		let end_name, end_header = new_label () in
		
		(*si exception, on saute le corps du block*)
		listing#add "  cmp $0, %r13";
		listing#add ("  jne ."^end_name);
	
		try
		begin
		let n_offset = 
		match code with
		|  CBLOCK(decls, codes)		->(		
			(*pas de déclaration de fonctions dans des blocs, donc 3eme retour est []*)
			let new_env, offset, _ = hydrate_env env false last_offset decls in
			compile_codes new_env offset finally_label codes;
		)
		| CEXPR( expr)			->(
			let _, offset = compile_expr env last_offset end_name expr in
			offset
		)
		| CIF( expr, code1, code2)->(
			let descr, offset = compile_expr env last_offset end_name expr in
			
			let (name2, header2) 	= new_label () in 
			
			set_value (REGISTER "rax" ) descr;
			
			listing#add "  cmpq $0, %rax";
			listing#add ("  je ."^name2);
			
			let offset1 = compile_code env offset finally_label code1 in
			listing#add ("  jmp ."^end_name);
			
			listing#add header2;
			compile_code env offset1 finally_label code2;
		)	
		| CWHILE( expr, code) ->(
			let (name_comp, header_comp) = new_label () in 
			let (name_end, header_end) 	= new_label () in 
			
			listing#add header_comp;
			let descr, offset = compile_expr env last_offset end_name expr in
			set_value (REGISTER "rax" ) descr;
			
			listing#add "  cmpq $0, %rax";
			listing#add ("  je ."^name_end);
			
			let offset_loop = compile_code env offset finally_label code in
			
			listing#add ("  jmp ."^name_comp);
			listing#add header_end;
			
			offset_loop
		)
		| CRETURN( opt )	->(
			let new_offset = 
			match opt with
			|None 			->(
				last_offset
			)
			|Some( expr )	->(
				let descr, offset = compile_expr env last_offset end_name expr in
				set_value (REGISTER "rax" ) descr;
				offset
			)
			in
			
			if finally_label = "" then(
				listing#add "  leave"; 
				listing#add "  retq";
			)else(
				set_value (REGISTER "r15") (VALUE 1);
				listing#add ("  jmp ."^finally_label);
			);
			
			new_offset
		)
		| CTHROW( string, expr)->(
			let descr, offset = compile_expr env last_offset end_name expr in
			let id	= e_table#get string in

			set_value (REGISTER "r13") (VALUE id);
			set_value (REGISTER "r14") descr;
			
			offset
		)
		| CTRY( c, try_list, final )->(
			let exit_label, exit_header = (
				match final with
				| None -> (end_name, end_header)
				| Some( _ ) -> new_label () 
			)in
			
			let new_finally_label = (if exit_label = end_name then "" else exit_label) in
			set_value (REGISTER "r15") (VALUE 0);
		
		
			let first_offset = compile_code env last_offset new_finally_label c in 
			let headers = List.rev ( generate_try_header [] try_list) in	
			listing#add ("  jmp ."^exit_label); (*si pas de catch valid*)
			
			let offset = generate_try_body env first_offset exit_label end_name finally_label (try_list, headers) in
			
			begin
			match final with
			| None -> offset
			| Some( f_c ) ->(
				listing#add exit_header;
				
				(*save exception *)
				set_value (STACK offset) (REGISTER "r13");
				set_value (STACK (offset+8)) (REGISTER "r14");
				set_value (STACK (offset+16)) (REGISTER "r15");
				set_value (STACK (offset+24)) (REGISTER "rax");
				
				set_value (REGISTER "r13") (VALUE 0);		
				set_value (REGISTER "r15") (VALUE 0);		
				let new_offset = compile_code env (offset+32) finally_label f_c in
				
				(*si exception dans le finally, on saute à la fin du block de try*)
 				listing#add "  cmp $0, %r13";
				listing#add ("  jne ."^end_name);
				
				(*on restaure exception précédante*)
				set_value (REGISTER "r13") (STACK offset);
				set_value (REGISTER "r14") (STACK (offset+8));
				
				(*on vérifie si le code du try retourne*)
				set_value (REGISTER "r15") (STACK (offset+16));
				set_value (REGISTER "rax") (STACK (offset+24));
				
				listing#add "  cmp $1, %r15";
				listing#add ("  jne ."^end_name);
				
				
				
				listing#add "  leave";
				listing#add "  retq";

				new_offset
			)
			end
		)
	in
	 
	listing#add end_header;
	n_offset
	
	end
	with | excep -> excep_handler locator excep
	)
	
(*
	
	@param acc - accumulateur des labels de chaque catch
	@param 	- catch list
	@description - retourne la liste inversée des labels correspondants aux 
		catchs
*)
and generate_try_header acc = function
	| [] -> acc
	| (exp_name, var_name, code)::t ->(
		let id = e_table#get exp_name in
		let l_name, l_header = new_label () in 
		
		listing#add ("  cmp $"^(string_of_int id )^", %r13");
		listing#add ("  je ."^l_name);
		
		generate_try_header ((l_header)::acc) t
	)
(* 
	@param env - environnement du scope parent
	@param last_offset - offset de decalage par rapport à rbp( ie position 
		de la nouvelle écriture
	@param exit_label - label de fin de block (ou finally si exitance )s
	@param end_label - label de fin de block
	@param finally_label - "" pas finally parent, "..." label du finally parent
	@param  - catch list
*)
and generate_try_body env last_offset exit_label end_label finally_label= function
	| [], [] -> last_offset
	| (_, var_name, code)::t1, l_header::t2 ->(
		listing#add l_header;
		set_value (REGISTER "r13") (VALUE (0)); (*reset exception*)
		
		let new_env = env#set var_name (STACK last_offset) in 
		set_value (new_env#get var_name) (REGISTER "r14");
		
		let offset = compile_code new_env (last_offset+8) finally_label code in
		
		(*garbage catch exception*)
		listing#add "  cmp $0, %r13";
		listing#add ("  jne ."^end_label);
		listing#add ("  jmp ."^exit_label);
		generate_try_body env offset exit_label end_label finally_label (t1, t2);
	) 
	| _ -> raise Undefined	
(*
	@param env - environnement du scope parent
	@param last_offset - offset de decalage par rapport à rbp( ie position 
		de la nouvelle écriture
	@param finally_label - "" pas finally parent, "..." label du finally parent
	@param  - lists de loc_code (see.cparse.ml loc_code)
	@description - ajoute le code assembleur au listing
	@return - nouvel offset par rapport à rbp
*)
and compile_codes env last_offset finally_label=function
	| [] -> last_offset
	| h::t ->(
		let offset = compile_code env last_offset finally_label h in
		compile_codes env offset finally_label t;
	);;

(*
	@param env - environnement du scope parent
	@param decls - liste des arguments de la fonction
	@param code	- le code de la fonction
	@description - evalue une fonction C, ajoute le code ASM au listing
*)
let rec compile_fun env decls code=
	listing#add "  pushq %rbp";
	listing#add "  movq %rsp, %rbp";
	set_value (REGISTER "r13") (VALUE 0);
	
	listing#rotate;
	
	(*pas de déclaration de fonctions dans des fonctions, donc 3eme retour est []*)
	let len_decls			= List.length decls in
	let offset				= (if len_decls> 6 then (len_decls-7)*8+16 else 8) in
	let local_env, offset, _ 	= hydrate_env env false offset decls in
	
	let rec local_get_decls=function (*n° de l'argument*)
		|[], n	-> ()
		|CDECL(locator, name)::t, n	->(	
			begin
			match n with
			|1->set_value (local_env#get name) (REGISTER "rdi")
			|2->set_value (local_env#get name) (REGISTER "rsi")
			|3->set_value (local_env#get name) (REGISTER "rdx")
			|4->set_value (local_env#get name) (REGISTER "rcx") 
			|5->set_value (local_env#get name) (REGISTER "r8")
			|6->set_value (local_env#get name) (REGISTER "r9")
			|n->(		
				listing#add ("  movq "^(string_of_int ((n-7)*8+16) )^"(%rbp), %rax");
				listing#add ("  movq %rax, -"^(string_of_int ((n-6)*8) )^"(%rbp)");
				local_env#set2 name  (STACK ((n-6)*8) )
			)
			end;
			
			local_get_decls (t, (n+1));
		)
		|_->raise Cfun_forbidden 
	in
		
	local_get_decls (decls, 1);

	let offset_block = compile_code local_env offset "" code in 
	
	let space_allocated = offset_block + ( if offset_block mod 16 =0 then 0 else  8 ) in
	
	listing#link ("  subq $"^(string_of_int space_allocated)^", %rsp");
	
	(*gestion du return; optionnel*)
	if listing#no_return then(
		listing#add "  leave"; 
		listing#add "  retq";
	);
;;

let rec compile_body =function
	|env, []-> ()
	|env, (CFUN(locator, name, fun_decls, code))::fcts->(
		try(
			my_functions#set name;
			listing#add ("\n.globl "^name);
			listing#add (name^":");
			
			compile_fun env fun_decls code;
			
			compile_body (env, fcts);
		)with | excep -> excep_handler locator excep
	)
	|_->()(*pour désactiver le warning, on n'entrera jamais dans ce bout de code*)
;;

let compile out decl_list =
	compile_body (hydrate_global (new environnement ()) [] decl_list);

	fprintf out "%s" heap#build;
	listing#write out;
;;
