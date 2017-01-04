(** Ce programme est appelé par GUI avec pour argument la liste des modules pour lesquels l'utilisateur veut des infos **)
(** Pour la compilation avec ocamlc, faire 'ocamlc graphics.cma nom_du_fichier.ml' **)
open Graphics





(****** DEFINITIONS DE VARIABLES GLOBALES ******)

(* on définit les couleurs qu'on utilisera *)
let black = rgb 0 0 0 (* contour des rectangles *)
let red = rgb 238 0 0 (* texte *)
let violet = rgb 208 32 144 (* flèches *)
let lite_blue = rgb 240 248 255 (* intérieur des  rectangles *)

(* cette fonction sera appelée par la GUI probablement *)
let set_size w h = w,h

(* on définit la hauteur, la largeur, le diametre et le centre du cercle sur lequel seront positionnés les noeuds *)
let width_i,height_i = set_size 1200 750
let width_f = float_of_int width_i
let height_f = float_of_int height_i
let diam = float_of_int (min width_i height_i) -. 20.
let ray = diam /. 2.
let center = (width_f /. 2.) , (height_f /. 2.)

(* on construit les chaines de caractères correspondant à la largeur et à la hauteur *)
let width_s = string_of_int width_i
let height_s = string_of_int height_i





(****** FONCTIONS PAS ENCORE CODEES ******)

(* cette fonction va chercher (où ??) la liste des open faits dans le module en question *)
let get_open modul = 
    if modul = modul 
    then [["Blub";"Blab";"toto"] ; ["tyty";"Blob"];["titi"]] 
    else [["Blub";"Blab";"toto"] ; ["tyty";"Blob"];["titi"]]





(****** CONSTRUCTION DE LA LISTE DES ZONES QUI CONTIENNENT LES NOMS DE MODULE ******)

(* renvoie la position du module de numéro index, sur un cercle abstrait
 * cercle de rayon ray, de centre center, contenant size points *)
let give_one_pos size index center ray =
    let abs,ord = center in
	let angle = 8. *. atan(1.) *. (float_of_int index) /. (float_of_int size) in
	let abscisse = floor (ray *. (cos angle) +. abs) in
	let ordonnee = floor (ray *. (sin angle) +. ord) in
	(int_of_float abscisse , int_of_float ordonnee)

(* renvoie la liste des positions correspondant à un graphe de dépendances à size modules *)
let rec give_positions size acc center ray =
    match acc with
	|0 -> []
	|_ -> (give_one_pos size acc center ray)::(give_positions size (acc-1) center ray)

(* affiche dans la fenêtre les noms (en rouge) des modules et des rectangles (noirs) autour d'eux
 * renvoie dans une liste les coordonnées des zones clickables par l'utilisateur
 * (renvoie le nom du module et les coordonnées du centre de la boîte) *)
let rec give_area_list modules coord_list areas =
    match coord_list with
    |[] -> areas
    |(x,y)::q -> give_area_list (List.tl modules) q ((List.hd modules,(x,y))::areas)

(* suite *)
let give_areas modules center ray =
    let size = List.length modules in
    let coord_list = give_positions size size center ray in
    give_area_list modules coord_list []


    


(****** EXTRACTION DES DEPENDANCES REELLEMENT UTILES ******)

(* convertie ['Toto';'Plop'] en 'Toto.Plop' *)
let string_of_list l =
	let rec aux l =
        match l with
		|[] -> ""
		|t::q -> t ^ "." ^ aux q
	in
	let str = aux l in
	let str = String.sub str 0 (String.length str -1) in 
	str

(* renvoie le booléen correspondant à la non emptyness de l'intersection de l1 et l2 *)
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





(****** CONSTRUCTION DES ARETES ******)

(* fonction carré sur les float *)
let square x = x *. x

(* fonction addition sur les couples d'entiers *)
let tupl_sum (x,y) (a,b) = (x+a,y+b)

(* fonction opposé sur les couples d'entiers *)
let tupl_min (x,y) = (-x,-y)

(* dessine la flèche correspondant à 'module1 fait appel à module2' *)
let print_one_edge modul1 modul2 areas =
    set_color violet ;
    let area1 = List.find (fun (modul,(x,y)) -> modul=modul1) areas in
    let (x1,y1) = snd area1 in
    let area2 = List.find (fun (modul,(x,y)) -> modul=modul2) areas in
    let (x2,y2) = snd area2 in
    moveto x1 y1 ;
    lineto x2 y2 ;
    let a,b = (x1-x2) , (y1-y2) in (* le vecteur directeur de la fleche 2->1 *)
(*    let norm = int_of_float (sqrt ( square(float_of_int a) + (square(float_of_int b)))) in*)
    let norm = int_of_float (hypot (float_of_int a) (float_of_int b)) in
    let v = (a*10/norm) , (b*10/norm) in
    let n = (-b*6/norm) , (a*6/norm) in
    let two_third = (2*x1+x2)/3 , (2*y1+y2)/3 in
    let arrow = Array.make 3 (0,0) in
    arrow.(0) <- two_third ;
    arrow.(1) <- tupl_sum (two_third) (tupl_sum (tupl_min v) (n)) ;
    arrow.(2) <- tupl_sum (two_third) (tupl_sum (tupl_min v) (tupl_min n)) ;
    fill_poly arrow

(* prend une liste de dépendances d'un module, un autre module, et écrit les dépendances qui vont bien *)
let rec print_edges_two_mod dep_list m1 m2 areas = match dep_list with
	|[] -> ()
	|l::q -> (if List.exists (fun x -> x=m2) l then print_one_edge m1 m2 areas ; print_edges_two_mod q m1 m2 areas)

(* dessine toutes les flèches des dépendances de modul *)
let print_edge_module modul modules areas =
	let open_list = get_useful_dep modul modules in
	let rec aux mod_list = match mod_list with
		|[] -> ()
		|m::q -> if m<>modul then (print_edges_two_mod open_list modul m areas ; aux q) else aux q
	in
	aux modules

(* dessine toutes les flèches *)
let print_edges_complete modules areas = 
	let rec aux mod_list = match mod_list with
		|[] -> ()
		|m::q -> (print_edge_module m modules areas ; aux q)
	in
	aux modules





(****** CONSTRUCTION DES BOX ******)

let print_box area =
    let modul,(x,y) = area in
    let text_w,text_h = text_size modul in
    let x_botleft = (x-text_w/2 - 2) in
    let y_botleft = (y-text_h/2 - 2) in
    let rect_w = (text_w + 4) in
    let rect_h = (text_h + 4) in
    set_color lite_blue ; 
    fill_rect x_botleft y_botleft rect_w rect_h ;
    set_color black ;
    draw_rect x_botleft y_botleft rect_w rect_h ;
    set_color red ;
    moveto (x_botleft + 2) (y_botleft + 2) ;
    draw_string modul


let print_boxes areas = List.iter print_box areas





(****** GESTION DES CLICS UTILISATEURS ******)

(* cette exception est lancée quand l'utilisateur clique sur une zones n'étant pas dans une box *)
exception Not_in_a_box

(* renvoie vrai si pos est dans la zone définie par zone *)
let is_in pos area = 
    let x_mouse,y_mouse = pos in
    let modul,(x,y) = area in
    let text_w,text_h = text_size modul in
    (x_mouse >= (x-text_w/2 - 2)  &&  x_mouse <= (x+text_w/2 + 2)  &&  y_mouse >= (y+text_h/2 + 2)  &&  y_mouse <= (y+text_h/2 + 2))

(* si l'utilisateur a cliqué dans une box, renvoie le nom de la box, sinon lève l'exception Not_in_a_box *)
let rec get_module_name pos areas =
    match areas with
    |[] -> raise Not_in_a_box
    |area::q -> if is_in pos area then (fst area) else get_module_name pos q





(****** PARTIE ECRITURE ******)

let modules = ["toto";"titi";"tyty";"tete";"jojo";"jyjy"]

let dep_build modules width_i height_i =
    let width_f = float_of_int width_i in
    let height_f = float_of_int height_i in
    let diam = float_of_int (min width_i height_i) -. 20. in
    let ray = diam /. 2. in
    let center = (width_f /. 2.) , (height_f /. 2.) in
    let width_s = string_of_int width_i in
    let height_s = string_of_int height_i in

    let areas = give_areas modules center ray in


    open_graph (" " ^ width_s ^ "x" ^ height_s) ;
    print_edges_complete modules areas ;
    print_boxes areas ;
    let stat = wait_next_event [Button_down] in
    print_string "toto"

(*
let () = main ["toto";"titi";"tyty";"tete";"jojo";"jyjy"] 1200 750
*)
