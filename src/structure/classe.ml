open Coeur


exception No_child

type class_elt = Attr of string| Meth of string

class virtual class_element(name) = object
  inherit [gset] set(name)
  method virtual as_attr: attr option
  method virtual as_meth: meth option 
  val mutable name_class = ""
  method virtual type_class_elt : class_elt
  method name_class = name_class
  method change_name_class name_c = 
    name_class <-name_c;
end

and attr(name) = object(self)
  inherit class_element(name)
  method add_child _ = raise No_child
  method as_attr = Some (self:>attr)
  method as_meth = None
  method type_class_elt = Attr name
  method to_string =  "Attr(" ^ self#name ^ ")"
end

and meth(name) = object(self)
  inherit class_element(name) 
  method add_child _ = raise No_child
  method as_attr = None
  method as_meth = Some (self:>meth)
  method type_class_elt = Meth name
  method to_string = "Meth(" ^ self#name ^ ")"
end


class ['a] classe(name) = object(self)
  inherit [class_element] set(name)
  val mutable name_class = name
  method to_string = match children with
  | [] -> "C(" ^ self#name ^ ")"
  | a::b -> "C:" ^ self#name ^ "(" ^ a#to_string ^ ((List.fold_left (fun a b -> a ^ "," ^ b#to_string) "" b)) ^ ")" 
  method add_child (child: 'a) =
    child#change_name_class(name);
    children <- child::children
end


(*'a set

abstract = set set;;

class class_element;;
classe = class_element set;;

module_element = [`module of module, `dec of dec]
and modul = module_element set;;

*)
