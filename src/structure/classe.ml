open Coeur


exception No_child

(*class virtual ['a] class_element(name) = object
  inherit ['a] set(name)
  method virtual as_attr: 'a attr option
  method virtual as_meth: 'a meth option
end*)

class ['a] attr(name) = object(self)
  inherit ['a] set(name)
  method add_child _ = raise No_child 
  method to_string = match children with
  | [] -> "Attr(" ^ self#name ^ ")"
  |_ -> raise No_child
end

class ['a] meth(name) = object(self)
  inherit ['a] set(name) 
  method add_child _ = raise No_child
  method to_string = match children with
  | [] -> "Meth(" ^ self#name ^ ")"
  |_ -> raise No_child
end


class ['a] classe(name) = object(self)
  inherit ['a] set(name)
  method to_string = match children with
  | [] -> "C(" ^ self#name ^ ")"
  | a::b -> "C:" ^ self#name ^ "(" ^ a#to_string ^ ((List.fold_left (fun a b -> a ^ "," ^ b#to_string) "" b)) ^ ")"
  method give_name_class () = 
    name_class <-name;
    List.iter (fun a -> a#change_name_class(name)) children

end




(*'a set

abstract = set set;;

class class_element;;
classe = class_element set;;

module_element = [`module of module, `dec of dec]
and modul = module_element set;;

*)
