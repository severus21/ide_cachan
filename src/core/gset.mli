(**Define the tags and the sets : 
   - the sets correspond to a part of file
   - the tags indicate the attributs or information about a set
*)


(**Type to create a tag element*)
type 'a tag_element =
| TStr of string
| TRef of 'a
| TDepend of string list;;

(**Type to create a tag *)
type 'a tag =  'a tag_element list;;



(** Class to print a object *)
class virtual toStringable : object
  method virtual to_string:string
end



(** Class tags*)
class ['a] tags :object  
  inherit toStringable
  constraint 'a = < name : string; .. >
  val tag_htbl : (string, 'a tag) Hashtbl.t
  method add_tag : string -> 'a tag -> unit
  method equal : 'a tags -> bool
  method get_tag : (string, 'a tag) Hashtbl.t
  method get_value : string -> 'a tag option
  method to_file : out_channel -> unit
  method private to_file_aux :
    string -> 'a tag -> out_channel -> unit
  
  (**Print a tags*)
  method to_string : string
  method private to_string_aux : 'a tag -> string
end

(** Class set *)
and ['a] set : string -> object
  constraint 'a = < to_string : string; .. >
  val mutable children : 'a list
  method add_child : 'a -> unit
  method children : 'a list
  method name : string 
  
  (**Print a set*)
  method to_string : string
end


(**Type to create set with subset with type set *)
type gset = gset set


(**Unittests*)
val unittests : unit -> OUnit2.test
