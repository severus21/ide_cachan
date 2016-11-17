open Core;;
open Class;;

class ['a] cModule(name) = object
  inherit [[`Module of 'a cModule | `Class of cClass | `Else of 'a]] cSet(name)

  val mutable dependencies: string list list = []
  method add_dependency lst = dependencies <- lst::dependencies
    
end
