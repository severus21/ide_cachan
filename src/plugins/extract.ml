(** Provide facilities to scan recursively a dir with some collecting rules
    NOTA BENE : name of file is path/name_without extension, 
                filename is path/name_extension*)


(** Return the subdirectories and the files in a directory
    @param items - a list of items in the directory
    @return subdirectories, files*) 
let dirs_files_of items=(* TODO check if files are valid, filter list*) 
    List.partition (Sys.is_directory) items

       
(** Return the name and the extension of a file
    @param filename - the name of the file
    @return the name without the ext, the ext if exists else ""*)
let ext_of filename =
    try
        let i = String.rindex filename '.' and len = String.length filename in
        String.sub filename 0 i, String.sub filename (i+1) (len-i-1)  
    with |Not_found-> filename,""    

(** Return all the files in a directory and in its subdirs(recursively)
    @param the path of the directory to scan
    @return a list of files*)
let scandir path=
    let rec _scandir =fun acc path->
        let items = List.map (Filename.concat path) (Array.to_list (Sys.readdir path))in
        let subdirs, files = dirs_files_of items in
        List.fold_left _scandir (acc@files) subdirs     
    in
    
    _scandir [] path

(** Scan recursively a directory, and aggregate file according rules
  * @param path - path of the parent dir 
    @param rules -
    @return string list list list, 
        ieme elemt <->rule i => list of entry matching the rule i but no rule lower than i
        an entry is relation between files ie [file1; ...; filen]   
    @exemple apply_rules ... [["mli";"ml"];["ml"]] will output somethin like  
        [[["a.mli";"a.ml"];["b.mli;b.ml"]];[["c.ml"];["d.ml"]]]
  *)
let apply_rules path rules=
    let files = scandir path in
    let n = List.length files in
    let htbl_files = Hashtbl.create n and closed_names = Hashtbl.create n in

    let names = (List.map (function filename ->(
        let name, _ = ext_of filename in
        Hashtbl.replace htbl_files filename true;
        name;
    )) files) in
    
    let match_rule rule name=
        if Hashtbl.mem closed_names name then []
        else(                                        
            match (List.filter (function ext-> not (Hashtbl.mem htbl_files (name^"."^ext)) ) rule ) with
            |[]->(
                Hashtbl.replace closed_names name true; (*Only the top ranked rule which match must be applied in a name*)
                List.map (function ext-> name^"."^ext) rule
            )      
            |_ -> []   ) 
    in

    List.rev (List.fold_left (fun acc0 rule -> (
        (*Here for a rule [ext1;...;extn] we harvest for all name in names, 
          [name.ext1;...;name.extn] who exists in files
          NB :  if a name match a rule it is add to closed_names to ensure that :
                a name is associated to the top ranked rule he match*)  
        List.fold_left (fun acc1 name -> 
            match match_rule rule name with 
            |[]-> acc1
            | l-> l::acc1 
        ) [] names
    )::acc0) [] rules)
(*TODO rules are functions*)
let affiche lll=
        Printf.printf "\nAffiche :";
        List.iter( function ll->(
            assert(ll <> []);
            Printf.printf "\n\tRule:";
            List.iter( function l->(
                assert(l<>[]);
                List.iter (function x->assert(x<>""))l;
                Printf.printf "\n\t\tEntry:\n\t\t\t|%s|" (String.concat "\n\t\t\t" l);
            )) ll 
        )) lll
     
