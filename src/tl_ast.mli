(** Ce module contient des fonctions permettant de manipuler des ast top_level de ocaml*)




type tl_struct =  
|Tl_none (*cette valeur doit être supprimée*)
|Tl_open of string*string (*pour le moment, on se contente d'extraire le nom du module ouvert
			   *et on donne également la ligne complète de l'ouverture*)

(** Type des ast top level*)
type tl_ast = tl_struct list


(** Renvoie la chaîne de charactère contenant le fichier f complet *)
val file_to_string : string -> string

(** Permet d'obtenir un parsetree du fichier ocaml dont le nom est donné en argument*)
val get_ast : string ->  Parsetree.structure_item list

(** Affiche un parsetree*)
val print_ast : Parsetree.structure_item list -> unit


(** Conversion des ast de Parsetree vers les tl_ast *)
val ast_to_tl_ast : string -> Parsetree.structure_item list -> tl_ast

(** Affiche un tl_ast *)
val print_tl_ast : tl_ast -> unit




