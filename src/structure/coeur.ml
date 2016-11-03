type 'a tag_element = 
  TStr of string
| TRef of 'a;;

type 'a tag = 'a tag_element list;;

class virtual toStringable = object
  method virtual to_string:string
end

class ['a] tags = object(self)
  inherit toStringable

  val mutable tag_list:'a tag list =[]
  method add_tag (tag:'a tag) = tag_list <- tag::tag_list

  method private tag_to_list z = 
    List.fold_left (fun prec -> function
  | TStr(x) -> prec ^ ", " ^ "S : " ^ x
  | TRef(a) -> prec ^ ", " ^ "Ref : " ^ a#name ) "" z

  method to_string = "[" ^ (List.fold_left (fun prec b ->
    prec ^ "; " ^ self#tag_to_list b) "" tag_list) ^ "]"

end

and ['a] metaData = object
  inherit ['a] tags
end

and ['a] set(name_tmp:string) = object(self)
  inherit toStringable
  inherit ['a] metaData

  val mutable children = []
  method add_child (child: 'a) = children <- child::children

  method name = name_tmp
  val mutable meta_data = new metaData

  method to_string = match children with
  | [] -> "E(" ^ self#name ^ ")"
  | a::b -> "S:" ^ self#name ^ "(" ^ a#to_string ^ ((List.fold_left (fun a b -> a ^ "," ^ b#to_string) "" b)) ^ ")"
end

type gset = gset set;;

let print = Printf.printf "%s\n";;

let () =
  let elt1 = new set("poussin") in
  let set1 = new set("poule") in
  let elt2 = new set("oeuf") in
  let set2 = new set("ferme") in
  set1#add_child elt1;
  set1#add_child elt2;
  set2#add_child set1;
  print set2#to_string
;;
