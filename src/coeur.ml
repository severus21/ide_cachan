type 'a tag_element =
    | TStr of string
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
  method children = children

  method name = name_tmp
  val mutable meta_data = new metaData

  method! to_string = match children with
  | [] -> Printf.sprintf "E(%s)" self#name
  | l ->
          begin
              let children_names = List.map (fun c -> c#to_string) l in
              let collected_children = String.concat "," children_names in
              Printf.sprintf "S:%s(%s)" self#name collected_children
          end

  val mutable name_class = ""
  method change_name_class name_c =
      name_class <-name_c;
    List.iter (fun a -> a#change_name_class(name_c)) children
    method name_class = name_class
end

type gset = gset set;;

let print = Printf.printf "%s\n";;




let () =
    let elt1 = new set("poussin") in
    let set1 = new set("poule") in
    let elt2 = new set("oeuf") in
    let set2 = new set("ferme") in
    (* let class1 = new classe("campagne") in*)
    set1#add_child elt1;
    set1#add_child elt2;
    set2#add_child set1;
    (*  class1#add_child set1;
    class1#give_name_class();*)
    print set2#to_string;
    Printf.printf "classe : %s\n" (elt1#name_class)
;;
