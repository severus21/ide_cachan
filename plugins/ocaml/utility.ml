let file_to_string path=
	let input = open_in path in
	really_input_string input (in_channel_length input)

let enumerate l= 
    let i = ref (-1) in
    List.map (function x-> incr i; !i,x) l

let rec mkdir dir _mod=
    let flag =(
        try 
            not (Sys.is_directory dir)
        with |Sys_error _->true) 
    in
        
    let next = Filename.dirname dir in

    if flag then(
        if next <> "." && next <> ".." && next <> "/" then mkdir next _mod;
        Unix.mkdir dir _mod
    );
