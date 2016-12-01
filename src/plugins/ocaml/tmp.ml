module type Comparable =
sig
    type t
        val compare: t -> t -> int
end
 
module OrderedImpList (T : Comparable) (T2:Comparable)=
struct
    exception Empty
        type content = T.t
                           type t = content list ref
                                        let elem_comp = T.compare
                                                           
                                                            let rec insert x = function
                                                                      [] -> x::[]
                                                                                     | h::t as l when elem_comp x h < 0 -> x::l
                                                                                                                                    | h::t -> h::insert x t
                                                                                                                                                    
                                                                                                                                                     let push x s = s := insert x !s
                                                                                                                                                                            
                                                                                                                                                                             let pop s = match !s with
                                                                                                                                                                                       [] -> raise Empty
                                                                                                                                                                                                   | h::t -> s := t; h
                                                                                                                                                                                                                        
                                                                                                                                                                                                                         let is_empty s = !s = []
                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                   let empty () = ref []
end


