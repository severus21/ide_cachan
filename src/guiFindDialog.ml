
let find_dialog parent root =
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
    let entry = GEdit.entry ~packing () in
    let (columns, column, model, view) =
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
        (columns, column, model, view)
    in
    ignore(root);ignore(columns);ignore(column);ignore(model);ignore(view);
    ignore(entry#connect#notify_text ~callback:(fun _ -> ()));
