(** Returns the string containing the whole file f *)
val file_to_string : string -> string

(** TODO*)                                 
val enumerate : 'a list -> (int*'a) list                                 

(** Recrusive mkdi on unix plateform
    @param path
    @param mod*)                             
val mkdir : string -> Unix.file_perm->unit                            
