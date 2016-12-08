open Sys
open Filename


let ls path = Sys.readdir path

  
let get_dir array path = 
  List.partition (fun x -> is_directory (Filename.concat path x)) (Array.to_list array)


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


let add_path list dir =
  List.map (fun x -> Filename.concat dir x) list 


let rec iterate ext start_path=
  let array = ls start_path in
  let dir, file =  get_dir array start_path in
  let f x =  
    let path = Filename.concat start_path x in
    add_path (iterate ext path) x
    
  in
  let b = (get_ext ext (String.length ext) file) in
  List.concat [List.concat (List.map f dir) ; b]




let supp_ext ext s =
  let i = String.length ext in
  let n = String.length s in
  String.sub s 0 (n - i) 

let add_ext s ext =
  String.concat "" [s; ext]

let reset_ext ext list = 
  List.map (fun y -> (List.map (fun x-> add_ext y x) ext)) list 
 
  

let inter list1 list2 list3 ext1 ext2=
  let list1a,list1b = List.partition (fun x-> List.mem x list2) list1 in
  let rec aux list = function
    |[] -> []
    |x::t-> List.concat [List.map (fun s-> add_ext x s) list; aux list t] 
  in 
  let list1b1 = aux ext1 list1b in
  let _,list2b = List.partition (fun x-> List.mem x list1a) list2 in
  let list2b1 = 
    begin
      match ext2 with 
      |None->[] 
      |Some a -> List.map (fun s-> add_ext s a) list2b 
    end
  in
  (list1a,List.concat [list1b1; list2b1; list3])



let prob ext1 ext2 start_path=
  let rec aux exta extb list1 list2=  
    match exta with
    |[]-> list1, list2
    |x::t-> 
      begin
        let lista = List.map (supp_ext x) (iterate x start_path) in 
        let f = List.mem x ext2 in
        let extb' = if f then x::extb else extb in
        let list1',list2' = if f then inter list1 lista list2 extb (Some x)
        else inter list1 lista list2 extb None in
        aux t extb' list1' list2'
      end 
  in
  let ext = List.hd ext1 in
  if List.mem ext ext2 
  then aux (List.tl ext1) [ext] (List.map (supp_ext ext) (iterate ext start_path)) []
  else aux (List.tl ext1) [] (List.map (supp_ext ext) (iterate ext start_path)) []

let main ext1 ext2 start_path= 
  
  let l1,l2 = prob ext1 ext2 start_path in
  let l1 = add_path l1 start_path in
  let l2 = add_path l2 start_path in
  let l1' = reset_ext ext1 l1 in
  l1',l2





let ext1 = ["ml";"mli"]
let ext2 = ["ml"]

let start_path = "/import/rzucchin/M1/GenieLog/ide_cachan/src/parcoursDossier/"

let list3,list4 = main ext1 ext2 start_path

let () =
  Printf.printf "liste commune :\n";  
  List.iter (fun x-> List.iter (fun y ->  Printf.printf "%s;" y) x) list3; 
  Printf.printf "\nliste non commune :\n" ; 
  List.iter (fun x-> Printf.printf "%s;" x) list4;  
  Printf.printf "\n"

