open Gobject.Data
open Coeur

class navlist ~packing =
    let hbox = GPack.hbox ~packing () in
    let make_view model column =
        let view = GTree.view ~model ~packing:hbox#add () in
        let renderer = GTree.cell_renderer_text [] in
        let cell_data_func (model:GTree.model) iter =
            let set:gset = model#get ~row:iter ~column in
            let name = set#name in
            renderer#set_properties [`TEXT name]
        in
        let viewcol = GTree.view_column ~renderer:(renderer, []) () in
        viewcol#set_cell_data_func renderer cell_data_func;
        ignore(view#append_column viewcol);
        view
    in
    let _cols = new GTree.column_list in
    let _col1 = _cols#add caml
    and _col2 = _cols#add caml
    and _col3 = _cols#add caml in
    let _model1 = GTree.list_store _cols
    and _model2 = GTree.list_store _cols
    and _model3 = GTree.list_store _cols in
    let _view1 = make_view _model1 _col1
    and _view2 = make_view _model2 _col2
    and _view3 = make_view _model3 _col3 in
    object(self)
        val cols = _cols
        val col1 = _col1
        val col2 = _col2
        val col3 = _col3
        val model1 = _model1
        val model2 = _model2
        val model3 = _model3
        val view1 = _view1
        val view2 = _view2
        val view3 = _view3

        (** Sets the data in col *)
        method set_data ~col ~data () =
            let _set_data (model:GTree.list_store) column =
                model#clear ();
                let fill (value:gset) =
                    let iter = model#append () in
                    model#set ~row:iter ~column:column value
                in
                List.iter fill data
            in
            match col with
            | 1 -> _set_data model1 col1
            | 2 -> _set_data model2 col2
            | 3 -> _set_data model3 col3
            | _ -> failwith (Printf.sprintf "No such column (%i)" col)

        (** Changes the column title to title *)
        method set_column_title ~col ~title () =
            match col with
            | 1 -> (view1#get_column 0)#set_title title
            | 2 -> (view2#get_column 0)#set_title title
            | 3 -> (view3#get_column 0)#set_title title
            | _ -> failwith (Printf.sprintf "No such column (%i)" col)

        method private selection_changed1 selection () =
            let get path =
                let row = model1#get_iter path in
                model1#get ~row ~column:col1
            in
            match selection#get_selected_rows with
            | [p] -> self#set_data ~col:2 ~data:((get p)#children) ()
            | _ -> failwith "What"

        initializer
            let a = new set("A")
            and aa = new set("AA")
            and ab = new set("AB") in
            a#add_child aa;
            a#add_child ab;
            let b = new set("B")
            and ba = new set("BA") in
            b#add_child ba;
            self#set_data ~col:1 ~data:[a;b] ();
            ignore(view1#selection#connect#changed
                ~callback:(self#selection_changed1 view1#selection))
    end
;;
