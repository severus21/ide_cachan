open Coeur

(*
class classe(name) = object
  inherit class_element set(name)
  val mutable name_classe = name
  method transmit_name_classe =
    List.iter (fun x-> x#give_name_classe name) children
  
end

class virtual class_element(name) = object
  inherit elt(name)
  method virtual as_attr: option attr
  method virtual as_meth: option meth
end

class attr(name) = object
  inherit class_element(name)
end

class meth(name) = object
  inherit class_element(name)
end

'a set

abstract = set set;;

class class_element;;
classe = class_element set;;

module_element = [`module of module, `dec of dec]
and modul = module_element set;;
*)
