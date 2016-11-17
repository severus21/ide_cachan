open Sys
open Unix
open Filename

(*Extension lists*)
let extension = ["ml";"mli"]

let is_there name ext =
Sys.file_exists (name^"."^ext)


let ls () = Sys.readdir (Filename.current_dir_name)

let filter f array = 
  let rec aux f = function
    |[]-> []
    |x::t -> if f x then x ::(aux f t) else aux f t
  in 
  (aux f  (Array.to_list array))

let get_dir array = 
  List.partition (fun x -> is_directory x) (Array.to_list array)

let end_of_word word tail_word length_tail_word=
  let n = String.length word in
  let f = ref true in
  let i = ref 0 in
  while !f && (!i < length_tail_word) do
    if word.[ n-length_tail_word + !i] <> tail_word.[!i] then f:= false;
    incr i
  done;
  !f



let rec get_ext ext length_ext = function
  |[]-> []
  |x::t -> if end_of_word x ext length_ext then x::(get_ext ext length_ext t) else (get_ext ext length_ext t)




let rec iterate ext =
  let array = ls () in
  let dir, file =  get_dir array in
  let f x = 
    Sys.chdir x;
    let a = iterate ext in
    Sys.chdir "../";
    a
  in
  let b = (get_ext ext (String.length ext) file) in
  List.concat [List.concat (List.map f dir) ; b]
  
(*let () = Sys.chdir "tentative"
let () = Sys.chdir "joke"
let () = Sys.chdir "../../"
let () = Printf.printf "directory : %s\n" (Sys.getcwd ())

let () = 
  List.iter (fun x -> Printf.printf "%s ;" x) (fst(get_dir (ls ())))*)

let () = 
  List.iter (fun x -> Printf.printf "%s ;" x) (iterate "ml");
  Printf.printf "directory : %s\n" (Sys.getcwd ());

(*il reste à récupérer les mli et ajouter le chemin et pas seulement le nom du fichier + commencer le fichier à partir de src*)
