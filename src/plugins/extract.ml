(** Return the subdirectories and the files in a directory
    @param items - a list of items in the directory
    @return subdirectories, files*) 
let dirs_files_of items=
    try
        List.partition (Sys.is_directory) items
    with Sys_error _ -> [], [](*TODO log error*)

(** Return all the files in a directory and in its subdirs(recursively)
    @param the path of the directory to scan
    @return a list of files in asc order, 
       if path is a file then [path]*)(*TODO @exc throw excetion Sys_error*)
let scandir path=
    let rec _scandir =fun acc path->
        let items = List.map (Filename.concat path) (Array.to_list (Sys.readdir path))in
        let subdirs, files = dirs_files_of items in
        List.fold_left _scandir (acc@files) subdirs     
    in
    
    if Sys.is_directory path then  
        List.fast_sort String.compare (_scandir [] path)
    else [path]

let extract_from_rules path rules=
    let files = scandir path in
    let n = List.length files in
    let htbl_files = Hashtbl.create n and closed_files = Hashtbl.create n in

    List.iter (function filename ->(
        Hashtbl.replace htbl_files filename true
    )) files;
    
    let match_rule rule filename=
        if Hashtbl.mem closed_files filename then []
        else( 
            let name = Filename.remove_extension filename in
            match (List.filter (function ext-> not (Hashtbl.mem htbl_files (name^"."^ext)) ) rule ) with
            |[]->(
                (*Only the top ranked rule which match must be applied in a name*)
                List.map (function ext->(
                    Hashtbl.replace closed_files (name^"."^ext) true;
                    name^"."^ext
                )) rule
            )      
            |_ -> []   ) 
    in

    let entries = List.rev (List.fold_left (fun acc0 rule -> (
        (*Here for a rule [ext1;...;extn] we harvest for all filename such that 
          [name.ext1;...;name.extn] which exists
          NB :  if a filename match a rule it is add to closed_files to ensure that :
                a filename is associated to the top ranked rule he match*)  
        List.rev (List.fold_left (fun acc1 filename -> 
            match match_rule rule filename with 
            |[]-> acc1
            | l-> l::acc1 
        ) [] files)
    )::acc0) [] rules) in
    
    entries, List.filter (function filename->not (Hashtbl.mem closed_files filename)) files 

let print=
    List.iter( function r_entries->(
        Printf.printf "\nEntries:";
        List.iter( function entry->(
            Printf.printf "\n\tEntry:\n\t\t%s" (String.concat "\n\t\t" entry);
        )) r_entries 
    )) 

open OUnit2
let tests ()= 
    let path = "tests/data/plugins/extract/" in
    let location = function x->path^x in

    "Extract">:::[
        "scandir">::(function _->(assert_equal 
            (scandir path)
            (List.map location [
                 "a.aa";"a.b";"b.c";"d1/c.aa";"d1/c.c";"d1/d1.1/f.aa";
                 "d1/d1.1/f.r";"d2/a.aa";"d2/b.c"
            ])
        ));
        "extract_from_rules">::(function _->(assert_equal
            (extract_from_rules path [["aa";"b"];["b"];["c"]]) 
            ([
                [List.map location ["a.aa";"a.b"]];
                [];
                [[location "b.c"];[location "d1/c.c"];[location "d2/b.c"]];
            ], List.map location ["d1/c.aa"; "d1/d1.1/f.aa"; "d1/d1.1/f.r";
                "d2/a.aa"])
        ))
    ]
