(** Provide facilities to scan recursively a dir with some collecting rules
    NOTA BENE : name of file is path/name_without extension, 
                filename is path/name_extension*)


(** Scan recursively a directory, and aggregate file according rules
    @param path - path of the parent dir 
    @param rules -
    @return entries, not_used_files
        entries :string list list list, 
            ieme elemt <->rule i => list of entry matching the rule i 
            but no rule lower than i
            an entry is relation between files ie [file1; ...; filen],
        not_used_files : string list,
            all the files which do not match any rules
    @exemple apply_rules ... [["mli";"ml"];["ml"]] will output something like  
        [[["a.mli";"a.ml"];["b.mli;b.ml"]];[["c.ml"];["d.ml"]]]*)
val extract_from_rules : string -> string list list -> string list list list * string list

(** Pretty printing for list of entries
    @param a list of entries*)      
val print : string list list list -> unit 

(** Generate unittests for this module*)                                       
val tests : unit -> OUnit2.test                                       

                                       
