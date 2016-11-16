(** Ce programme est appelé par la GUI avec comme argument la liste des modules pour lesquels l'utilisateur veut avoir des infos **)



(** Définitions de variables globales **)

let open_fic_out () = open_out "./graph.tex"

let header = "
\\documentclass{article}
\\usepackage[dvipsnames]{xcolor}
\\usepackage[a4paper,text={16cm,22.7cm},centering]{geometry}
\\usepackage{gastex}
\\gasset{frame=false}
\\parindent=0pt

\\begin{document}

\\begin{center}
\\begin{gpicture}
"

let ender = "
\\end{gpicture}
\\end{center}

\\end{document}"




(** Fonctions utiles **)

(* cette fonction va chercher (où ??) la liste des open faits dans le module en question *)
let get_open nom_du_module = [[nom_du_module^"_Iterables";nom_du_module^"_Listes"];["Toto"]]

(** Construction des noeuds **)

(* renvoie la position du module de numéro index, sur un cercle de rayon 70 et d'origine 0,-100 *)
let give_one_pos size index =
	let angle = 8. *. atan(1.) *. (float_of_int index) /. (float_of_int size) in
	let abscisse = floor (70. *. (cos angle)) in
	let ordonnee = floor (70. *. (sin angle) -. 100.) in
	(int_of_float abscisse , int_of_float ordonnee)

(* renvoie la liste des positions correspondant à un graphe de dépendances à n modules *)
let rec give_positions size acc = match acc with
	|0 -> []
	|_ -> (give_one_pos size acc)::(give_positions size (acc-1))

(* affiche la définition des noeuds dans le .tex *)
let rec print_nodes modules liste_coord out = match liste_coord with
  |[] -> ()
  |(x,y)::q -> (let modul = List.hd modules in
    output_string out ("  \\node(" ^ modul ^ ")(" ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ "){" ^ modul ^ "}\n") ;
    print_nodes (List.tl modules) q out)

(* affiche la partie 'définitions des noeuds' en entier *)
let print_nodes_complete modules out =
  output_string out "  \\gasset{Nadjust=w,Nadjustdist=2,Nh=6,Nmr=1}\n" ;
  let size = List.length modules in
	let liste_coord = give_positions size size in
	print_nodes modules liste_coord out


	


(* build_graph est appelée par GUI
   modules est la liste des modules pour lesquels l'utilisateur veut des infos
   flag indique si l'utilisateur veut avoir des informations entre les éléments de liste, ou savoir TOUT sur chaque élément de liste (qui l'ouvre par exemple) (pour plus tard) *)







(** Partie écriture **)

let print_header out = output_string out header

let print_ender out = output_string out ender

let close_fic out = close_out out

let print_everything modules out =
	print_header out ;
	print_nodes_complete modules out ;
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
	
let _ = build_graph ["toto";"tutu";"tyty"]







