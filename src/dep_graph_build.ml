(** Ce programme est appelé par la GUI avec comme argument la liste des modules pour lesquels l'utilisateur veut avoir des infos **)



(** Définitions de variables globales **)

let fic_out = open_out "./graph.tex"

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

(*let make_env ..........*)

(* cette fonction va chercher (où ??) la liste des open faits dans le module en question *)
let get_open nom_du_module = [[nom_du_module^"_Iterables";nom_du_module^"_Listes"];["Toto"]]

(*let make_nodes liste size =*)
	


(* main est appelée par GUI
   liste est la liste des modules pour lesquels l'utilisateur veut des infos
   flag indique si l'utilisateur veut avoir des informations entre les éléments de liste, ou savoir TOUT sur chaque élément de liste (qui l'ouvre par exemple) (pour plus tard) *)
   
let boudy_main liste flag = if liste = [] && flag = 0 then output_string fic_out "
  \\gasset{Nadjust=w,Nadjustdist=2,Nh=6,Nmr=1}
  \\node[Nmarks=i](A)(0,0){idle}
  \\node(B)(50,0){wait}
  \\node(C)(50,-20){wait}
  \\node[Nmarks=r](D)(0,-20){critical}
 
  \\drawedge(A,B){req1:=true}
  \\drawedge(B,C){turn:=2}
  \\drawedge[syo=-1,eyo=-1](C,D){turn=1?}
  \\drawedge[syo=1,eyo=1,ELside=r](C,D){req556666652=false?}
  \\drawedge(D,A){req1:=false}
"








(** Partie écriture **)

let () = output_string fic_out header

let () = boudy_main [] 0

let () = output_string fic_out ender

let close = close_out fic_out

let _ =
	let _ = Sys.command "latex graph.tex" in
	let _ = Sys.command "dvips graph.dvi" in
	Sys.command "ps2pdf graph.ps"









