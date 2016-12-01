module type Comparable = sig 
        type t 
        val compare : t -> t -> int 
    end
module OrderList (T:Comparable) = struct 
        exception Empty 
        type content = T.t 
        type t = content list ref 
        let comp = T.compare 
    end
