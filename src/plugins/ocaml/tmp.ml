(*type even = Zero | Even_succ of odd and odd = Odd_succ of even*)

module Even : sig 
    type t = Zero | Succ of int
    val alpha:t                         
end = struct 
    type t = Zero | Succ of int
    let alpha = Zero                          
    let hello () = print_endline "Even"
end
module Even = struct 
        type t = Zero | Succ of int 
        let alpha = Zero
        let hello () = print_endline "Even"    
    end
(*module rec Even : sig 
    type t = Zero | Succ of Odd.t 
end = struct 
  type t = Zero | Succ of Odd.t 
end 
and Odd : sig 
    type t = Succ of Even.t 
end = struct 
   type t = Succ of Even.t 
end*) 
