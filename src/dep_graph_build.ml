(** Ce programme est appelé par la GUI avec comme argument la liste des modules pour lesquels l'utilisateur veut avoir des infos **)



(** Définitions de variables globales **)

let open_fic_out () = open_out "./graph.tex"

let header = "\
\\documentclass{article}\
\\usepackage[dvipsnames]{xcolor}\
\\usepackage[a4paper,text={16cm,22.7cm},centering]{geometry}\
\\usepackage{gastex}\
\\gasset{frame=false}\
\\parindent=0pt\
\
\\begin{document}\
\
\\begin{center}\
\\begin{gpicture}\
"

let ender = "\
\\end{gpicture}\
\\end{center}\
\
\\end{document}"

(*
class node ordre = object
	val mutable size = 0
	val mutable name = ""
	val mutable angle = 0.
	val mutable position = int * int
	val ordre = ordre
	
	method makeSize s = size <- s
	method makeName str = name <- str
	method makeAngle () = angle <- 8. *. atan(1.) *. (float_of_int ordre) /. (float_of_int size)
	method makePosition () = 	let abscisse = floor (80. *. (cos angle)) in
	let ordonnee = floor (80. *. (sin angle) -. 100.) in (int_of_float abscisse , int_of_float ordonnee)
	
  method print_me out = output_string out ("\t\\node(" ^ name ^ ")(" ^ (string_of_int (fst position)) ^ "," ^ (string_of_int (snd position)) ^ "){" ^ name ^ "}\n")
  (* affiche une flèche de modul à self *)
  method print_to modul =
*)



(** Fonctions pas encore codées **)

(* cette fonction va chercher (où ??) la liste des open faits dans le module en question *)
let get_open modul = if modul = modul then [["Blub";"Blab";"toto"] ; ["tyty";"Blob"];["titi"]] else [["Blub";"Blab";"toto"] ; ["tyty";"Blob"];["titi"]]


(** Construction des noeuds **)

(* renvoie la position du module de numéro index, sur un cercle de rayon 80 et d'origine 0,-100 *)
let give_one_pos size index =
	let angle = 8. *. atan(1.) *. (float_of_int index) /. (float_of_int size) in
	let abscisse = floor (80. *. (cos angle)) in
	let ordonnee = floor (80. *. (sin angle) -. 100.) in
	(int_of_float abscisse , int_of_float ordonnee)

(* renvoie la liste des positions correspondant à un graphe de dépendances à n modules *)
let rec give_positions size acc = match acc with
	|0 -> []
	|_ -> (give_one_pos size acc)::(give_positions size (acc-1))

(* affiche la définition des noeuds dans le .tex *)
let rec print_nodes modules liste_coord out = match liste_coord with
  |[] -> ()
  |(x,y)::q -> (let modul = List.hd modules in
    output_string out ("\t\\node(" ^ modul ^ ")(" ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ "){" ^ modul ^ "}\n") ;
    print_nodes (List.tl modules) q out)

(* affiche la partie 'définitions des noeuds' en entier *)
let print_nodes_complete modules out =
  output_string out "\t\\gasset{Nadjust=w,Nadjustdist=2,Nh=6,Nmr=1}\n" ;
  let size = List.length modules in
	let liste_coord = give_positions size size in
	print_nodes modules liste_coord out ;
	output_string out "\n"


(** Construction des arêtes **)

(* convertie ['Toto';'Plop'] en 'Toto.Plop' *)
let string_of_list l =
	let rec aux l = match l with
		|[] -> ""
		|t::q -> t ^ "." ^ aux q
	in
	let str = aux l in
	let str = String.sub str 0 (String.length str -1) in 
	str

(* renvoie le booléen correspondant à l'intersection de l1 et l2 *)
let rec l1_in_l2 l1 l2 = match l1 with
  |[] -> false
  |t::q -> (List.exists (fun x -> x=t) l2 || l1_in_l2 q l2)

(* renvoie la sous-liste de l1 dont les éléments sont les listes de l1 s'intersectant avec l2 *)
let rec big_l1_in_l2 l1 l2 = match l1 with
	|[] -> []
	|l::q -> if l1_in_l2 l l2 then l::(big_l1_in_l2 q l2) else big_l1_in_l2 q l2

(* parmis les dépendances d'un module, ne garde que celles concernant un autre module parmis ceux fournis *)
let get_useful_dep modul modules =
	let l1 = get_open modul in
	big_l1_in_l2 l1 modules
	
(* dessine la flèche correspondant à 'module 1 fait appel à module2' *)
let print_one_edge module1 module2 link out =
	output_string out ("\t\\drawedge(" ^ module2 ^ "," ^ module1 ^ "){" ^ link ^ "}\n")

(* prend une liste de dépendances d'un module, un autre module, et écrit les dépendances qui vont bien *)
let rec print_edges_two_mod dep_list m1 m2 out = match dep_list with
	|[] -> ()
	|l::q -> (if List.exists (fun x -> x=m2) l then print_one_edge m1 m2 (string_of_list l) out ; print_edges_two_mod q m1 m2 out	)

(* dessine toutes les flèches des dépendances de modul *)
let print_edge_module modul modules out =
	let open_list = get_useful_dep modul modules in
	let rec aux mod_list = match mod_list with
		|[] -> ()
		|m::q -> if m<>modul then (print_edges_two_mod open_list modul m out ; aux q) else aux q
	in
	aux modules

(* dessine toutes les flèches *)
let print_edges_complete modules out = 
	let rec aux mod_list = match mod_list with
		|[] -> ()
		|m::q -> (print_edge_module m modules out ; aux q)
	in
	aux modules

(** Partie écriture **)

let print_header out = output_string out header

let print_ender out = output_string out ender

let close_fic out = close_out out

let print_everything modules out =
	print_header out ;
	print_nodes_complete modules out ;
	print_edges_complete modules out ;
	print_ender out

let compile_tex () =
	let _ = Sys.command "latex graph.tex" in
	let _ = Sys.command "dvips graph.dvi" in
	let _ = Sys.command "ps2pdf graph.ps" in
	()

let build_graph modules =
  let out = open_fic_out () in
	print_everything modules out ;
	close_fic out ;
	compile_tex ()






