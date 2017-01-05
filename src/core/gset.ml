(**Define the tags and the sets *)


(**Type to create a tag element*)
type 'a tag_element =
| TStr of string
| TRef of 'a
| TDepend of string list;;

(**Type to create a tag *)
type 'a tag =  'a tag_element list;;



(** To print the object *)
class virtual toStringable = object
  method virtual to_string:string
end



(** Class tags 
    Methods : add a element, get a value, print a tags, equality between two tags*)
class ['a] tags = object(self)
  inherit toStringable


  val tag_htbl: (string,'a tag) Hashtbl.t = Hashtbl.create 4
  method get_tag = tag_htbl
  method get_value str =
    try
      Some (Hashtbl.find tag_htbl str)
    with
    |Not_found -> None

  method add_tag name values =
    Hashtbl.add tag_htbl name values

  method private to_string_aux values =
    List.fold_left (fun prec -> function
    | TStr(x) -> prec ^ ", " ^ "S : " ^ x
    | TRef(a) -> prec ^ ", " ^ "Ref : " ^ a#name
    | TDepend _ -> "" 
    ) "" values


  method to_string =
    let s = ref "" in
    Hashtbl.iter (fun _ values -> s := !s^(self#to_string_aux values)) tag_htbl;
    !s

  method private to_file_aux name values name_file =
    Printf.fprintf name_file "Name:\n";
    Printf.fprintf name_file "%S\n" name;
    List.iter (fun prec -> begin
    match prec with
    | TStr(x) ->  Printf.fprintf name_file "TStr: %S\n" x
    | _ -> ()  
    end
    ) values;
    Printf.fprintf name_file "FName\n"


  method to_file name_file =  
    Hashtbl.iter (fun name values -> (self#to_file_aux name values name_file)) tag_htbl

  method equal (tags:'a tags) = 
    tag_htbl = (tags#get_tag)
    

end

(** Class set 
    Methods : add a element, print a set*)
and ['a] set (name_tmp:string) = object(self)

  inherit toStringable

  val mutable children = []
  method children = children

  method add_child (child: 'a) = children <- child::children

  method name = name_tmp

  method to_string = match children with
  | [] -> "E(" ^ self#name ^ ")"
  | a::b -> "S:" ^ self#name ^ "(" ^ a#to_string ^ ((List.fold_left (fun a b -> a ^ "," ^ b#to_string) "" b)) ^ ")"
end



type gset = gset set


(**Tests*)
open OUnit2

let test_add_child_set ()=
  let set1 = new set("maison") in
  let set2 = new set ("piscine") in
  let set3 = new set("jardin") in
  let set4 = new set("vitre") in
  set1#add_child set4;
  set3#add_child set2;
  set1#add_child set3;
  assert_equal(set3#children) [set2];
  assert_equal(set1#children) [set3;set4]


let test_add_child_tags ()= 
  let tags = new tags in
  let tag1 = TStr "chat" in
  let tag2 = TStr "chien" in
  let tag3 = TStr "souris" in  
  let tag4 = TStr "chaise" in
  tags#add_tag "animaux" [tag1;tag2;tag3] ;
  tags#add_tag "meuble" [tag4];
  assert_equal(Hashtbl.find tags#get_tag  "animaux") [tag1;tag2;tag3] ;
  assert_equal(Hashtbl.length tags#get_tag) 2

let unittests ()=
  "Gset">:::[
    "Test_add_child_set:">::(function _-> test_add_child_set());
    "Test_add_child_tags:">::(function _-> test_add_child_tags())
  ]
