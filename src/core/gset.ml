type 'a tag_element =
| TStr of string
| TRef of 'a
| TDepend of string list;;


type 'a tag =  'a tag_element list;;




class virtual toStringable = object
  method virtual to_string:string
end





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

  method private to_file_aux values name_file =
    List.iter (fun prec -> begin
    match prec with
    | TStr(x) -> output_string name_file ("string :"^x^"\n")
    | _ -> () (*[Reb] TODO*)
    end
    ) values


  method to_file name_file =
    output_string name_file "tags :\n";
    Hashtbl.iter (fun _ values -> (self#to_file_aux values name_file)) tag_htbl

  method equal (tags:'a tags) = 
    tag_htbl = (tags#get_tag)
    

end

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



type gset = gset set;;



