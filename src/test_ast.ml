class test aze ze= object(self)
  val ard:string ="coucou"
  val arf:int = 123

  method arg x=x                
end
and
  test2 name = object 
  method coucou = Printf.printf "coucou %s\n" name
end
(*
class type testa = object
  val coucou:string 
end

class virtual testi a b= object
    method f =a
  method g = b            
end*)
