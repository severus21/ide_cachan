open Core;;

exception No_child;;

class cClassElement(name, parent) = object
  inherit [unit] cSet(name)
  val parent:cClass = parent
end
and cAttribute(name) = object(self)
  inherit cClassElement(name)
  method add_child _ = raise No_child
  method to_string =  "Attr(" ^ self#name ^ ")"
end
and cMethod(name) = object(self)
  inherit cClassElement(name) 
  method add_child _ = raise No_child
  method to_string = "Meth(" ^ self#name ^ ")"
end
and cClass(name) = object(self)
  inherit [[`Attribute of cAttribute | `Method of cMethod]] cSet(name)
  val mutable name_class = name
  method to_string = "C(" ^ self#name ^ ")"
end

and cClassHolder(name) = object
  inherit [cClass] cSet(name)
end


(*'a cSet

abstract = cSet cSet;;

class class_element;;
classe = class_element cSet;;

module_element = [`module of module, `dec of dec]
and modul = module_element cSet;;

*)
