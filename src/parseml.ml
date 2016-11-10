let test path = 
    let input = open_in path in
    let lbuff = Lexing.from_channel input in   
    let ptree = Parse.implementation lbuff in
    
    Printast.structure 0 (Format.formatter_of_out_channel stdout)  ptree;
    close_in input;
    ptree
;;
