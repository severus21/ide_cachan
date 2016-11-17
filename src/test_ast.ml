(*class test aze ze= object(self)
  val ard:string ="coucou"
  val arf:int = 123

  method arg x=x                
end
and
  test2 name = object 
  method coucou = Printf.printf "coucou %s\n" name
end*)
let () = ()

module type  s = sig
  val hello : unit->unit
end

module Hello = 
struct
    let message = "Hello"
                      let hello () = print_endline message
end
(*
module Hello1 : 
sig
   val hello : unit -> unit
end = 
struct
    let message = "Hello"
                      let hello () = print_endline message
end

module type Hello_type =
sig
    val hello : unit -> unit
end
  
module Hello3 : Hello_type =
struct
  let hello () = print_endline message
end*)
