
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

(** Opens the search dialog to find an item by name *)
let find_dialog parent root =
    (* Window *)
    let window = GWindow.dialog
        ~parent
        ~destroy_with_parent:true
        ~title:"Find"
        ~position:`CENTER
        ~width:400
        ~height:200
        ~show:true ()
    in
    let packing = window#vbox#add in
    (* Entry *)
    let entry = GEdit.entry ~packing () in
    (* List *)
    let (column, model) =
        let columns  = new GTree.column_list in
        let column   = columns#add Gobject.Data.caml in
        let model    = GTree.list_store columns in
        let view     = GTree.view ~model ~packing () in
        let renderer = GTree.cell_renderer_text [] in
        let cell_data_func (model:GTree.model) iter =
            let set:Core.gset = model#get ~row:iter ~column in
            renderer#set_properties [`TEXT set#name]
        in
        let viewcol = GTree.view_column ~renderer:(renderer, []) () in
        viewcol#set_cell_data_func renderer cell_data_func;
        ignore(view#append_column viewcol);
        (column, model)
    in
    (* Fills the list with search results *)
    let fill_column () =
        model#clear ();
        let rec fill value =
            let subword = String.lowercase_ascii entry#text in
            let word = String.lowercase_ascii value#name in
            if is_subword subword word then
                let iter = model#append () in
                model#set ~row:iter ~column:column value;
            else ();
            List.iter fill value#children
        in
        fill root
    in
    fill_column ();
    (* Signal connection *)
    ignore(entry#connect#notify_text ~callback:(fun _ -> fill_column ()));
