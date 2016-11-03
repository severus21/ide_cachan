open Gobject.Data

class navlist ~packing =
    let hbox = GPack.hbox ~packing () in
    let _cols = new GTree.column_list in
    let _col1 = _cols#add string
    and _col2 = _cols#add string
    and _col3 = _cols#add string in
    object(self)
        val cols = _cols
        val col1 = _col1
        val col2 = _col2
        val col3 = _col3
        val pack = hbox#add

        method private make_model ~data ~col () =
            let store = GTree.list_store cols in
            let fill (value:string) =
                let iter = store#append () in
                store#set ~row:iter ~column:col value
                in
            List.iter fill data;
            store

        method private make_view ~model ~col () =
            let view = GTree.view ~model ~packing:pack () in
            (* Column 1 *)
            let rendererer = GTree.cell_renderer_text [], ["text", col] in
            let _col = GTree.view_column ~title:"hi" ~renderer:rendererer () in
            ignore(view#append_column _col);
            view

        initializer
            let model1 = self#make_model ~col:col1
                                         ~data:[("Hello");("World")] () in
            let model2 = self#make_model ~col:col2
                                         ~data:[("Hello");("World")] () in
            let model3 = self#make_model ~col:col3
                                         ~data:[("Hello");("World")] () in
            let view1 = self#make_view ~model:model1 ~col:col1 () in
            let view2 = self#make_view ~model:model2 ~col:col2 () in
            let view3 = self#make_view ~model:model3 ~col:col3 () in
            ignore(view1);
            ignore(view2);
            ignore(view3);
    end
;;
