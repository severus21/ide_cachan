let alpha = 11
let troll x = x + alpha              

class ptr_ast x: object 
    val p_ast : c_ast ref 
    method ast : c_ast 
end = object 
    val p_ast = ref Nil 
    method ast = Nil 
end

module OrderList (T:Comparable) = struct 
        exception Empty 
        type content = T.t 
        type t = content list ref 
        let comp = T.compare 
    end
