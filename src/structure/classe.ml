open Coeur


exception No_child




class attr(name) = object(self)
  inherit set(name)
  method add_child _ = raise No_child
  method to_string =  "Attr(" ^ self#name ^ ")"
end

class meth(name) = object(self)
  inherit set(name) 
  method add_child _ = raise No_child
  method to_string = "Meth(" ^ self#name ^ ")"
end

type class_element = [`Attribute of attr | `Method of meth];;

class ['a] classe(name) = object(self)
  inherit class_element set(name)
  val mutable name_class = name
  method to_string = match children with
  | [] -> "C(" ^ self#name ^ ")"
  | a::b -> "C:" ^ self#name ^ "(" ^ a#to_string ^ ((List.fold_left (fun a b -> a ^ "," ^ b#to_string) "" b)) ^ ")" 
  method add_child (child: 'a) =
    child#change_name_class(name);
    children <- child::children
end

class ['a] class_holder(name) = object(self)
  inherit ['a classe] set(name)
end


(*'a set

abstract = set set;;

class class_element;;
classe = class_element set;;

module_element = [`module of module, `dec of dec]
and modul = module_element set;;

*)
