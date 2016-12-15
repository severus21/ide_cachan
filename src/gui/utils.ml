
(** Returns whether a is a subword of b *)
let is_subword a b =
    let current = ref 0 in
    let iter c =
        current := (String.index_from b !current c) + 1
    in
    try
        String.iter iter a;
        true
    with
    | Not_found -> false
;;

