open Core.Miscs

(** This module provides a navlist, a miller-columns-type gui item *)

(** This is an abstract class representing a list of subitems to be shown
  on the rightmost column of the GuiNavlist *)
class virtual item_list (root:c_ast) = object(self)
    (* The methods to implement *)
    (** Name of the list, displayed in the center column *)
    method virtual name : string

    (** Filters which subitems to show *)
    method virtual private filter : 'a -> bool

    (* The base methods *)
    (** Applies f to all items and subitems of root *)
    method private iter f =
        let rec _iter (item:c_node) =
            match item with
            | Nil -> f item
            | Node node ->
                    begin
                        match node.children with
                        | [] -> f item
                        | _  -> List.iter _iter node.children
                    end
        in
        List.iter _iter root

    (** Returns a list of the subitems to show *)
    method get =
        let items = ref [] in
        let _filter =
            fun item -> if self#filter item then items := item::!items in
        self#iter _filter;
        !items
end

(** This sublcass of item_list shows all subitems *)
class all_items (root:c_ast) = object
    inherit item_list root
    method name = "- All -"
    method filter _ = true
end

(** This is the miller column type gui item to navigate the hierarchy *)
class navlist ~packing =
    let hbox = GPack.hbox ~packing () in
    (* This function tells the column view how to render sets *)
    let c_node_data_func renderer column (model:GTree.model) iter =
        let (node:c_node) = model#get ~row:iter ~column in
        match node with
        | Nil -> renderer#set_properties [`TEXT "nil"]
        | Node n -> renderer#set_properties [`TEXT n.name]
    in
    (* This function tells the column view how to render an item_list *)
    let item_list_data_func renderer column (model:GTree.model) iter =
        let il:item_list = model#get ~row:iter ~column in
        renderer#set_properties [`TEXT il#name]
    in
    (* This is an helper function factoring the code to make a view *)
    let make_view model column data_func =
        let view = GTree.view ~model ~packing:hbox#add () in
        let renderer = GTree.cell_renderer_text [] in
        let cell_data_func = data_func renderer column in
        let viewcol = GTree.view_column ~renderer:(renderer, []) () in
        viewcol#set_cell_data_func renderer cell_data_func;
        ignore(view#append_column viewcol);
        view
    in
    let _cols = new GTree.column_list in
    let _col1 = _cols#add Gobject.Data.caml
    and _col2 = _cols#add Gobject.Data.caml
    and _col3 = _cols#add Gobject.Data.caml in
    let _model1 = GTree.tree_store _cols
    and _model2 = GTree.list_store _cols
    and _model3 = GTree.list_store _cols in
    let _view1 = make_view _model1 _col1 c_node_data_func
    and _view2 = make_view _model2 _col2 item_list_data_func
    and _view3 = make_view _model3 _col3 c_node_data_func in
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

        (** Sets the data in the hierarchy tree (leftmost column) *)
        method private fill_tree (data:c_ast) =
            model1#clear ();
            let rec fill ?(parent:Gtk.tree_iter option) (value:c_node) =
                let iter = model1#append ?parent () in
                model1#set ~row:iter ~column:col1 value;
                match value with
                | Nil -> ()
                | Node node -> List.iter (fun a -> fill ~parent:iter a)
                                node.children
            in
            List.iter fill data

        (** Sets the data in the method lists col (center column) *)
        method private fill_lists lists =
            model2#clear ();
            let fill (value:item_list) =
                let iter = model2#append () in
                model2#set ~row:iter ~column:col2 value
            in
            List.iter fill lists

        (** Sets the data in the methods col (rightmost column) *)
        method private fill_methods methods =
            model3#clear ();
            let fill (value:c_node) =
                let iter = model3#append () in
                model3#set ~row:iter ~column:col3 value
            in
            List.iter fill methods

        (** Changes the column title to title *)
        method set_column_title ~col ~title () =
            match col with
            | 1 -> (view1#get_column 0)#set_title title
            | 2 -> (view2#get_column 0)#set_title title
            | 3 -> (view3#get_column 0)#set_title title
            | _ -> failwith (Printf.sprintf "No such column (%i)" col)

        (** Callback on selection event on the leftmost column *)
        method private item_selected () =
            model3#clear ();
            let selection = view1#selection in
            let get path =
                let row = model1#get_iter path in
                model1#get ~row ~column:col1
            in
            (match selection#get_selected_rows with
            | [p] -> self#fill_lists [(new all_items [get p]:> item_list)]
            | []  -> ()
            | _   -> assert false);
            let first_list = model2#get_iter_first in
            match first_list with
            | Some first -> view2#selection#select_iter first
            | None -> ()

        (** Callback on selection event on the center column *)
        method private list_selected () =
            let selection = view2#selection in
            let get path =
                let row = model2#get_iter path in
                model2#get ~row ~column:col2
            in
            match selection#get_selected_rows with
            | [p] -> self#fill_methods ((get p)#get)
            | []  -> ()
            | _   -> assert false

        (** Callback on selection event on the rightmost column *)
        method private method_selected () =
            let selection = view3#selection in
            let get path =
                let row = model3#get_iter path in
                model3#get ~row ~column:col3
            in
            match selection#get_selected_rows with
            | [p] -> ignore(get p) (* Do smth *)
            | []  -> ()
            | _   -> assert false

        method set_root (root:c_ast) : unit =
            self#fill_tree root

        (** Fill in the leftmost column tree and connect events *)
        initializer
            ignore(view1#selection#connect#changed
                ~callback:self#item_selected);
            ignore(view2#selection#connect#changed
                ~callback:self#list_selected);
            ignore(view3#selection#connect#changed
                ~callback:self#method_selected);
    end
;;
