open Gset

(** Define an abstract representation of a file
  - name of structure, header of structure, code of structure, and children*) 
type c_node = 
|Nil
|Node of {name:string;header:string;body:string ref;children:c_ast;meta:gset tags}
and c_ast = c_node list

exception Bad_cnode of string
val bad_cnode:string->'a                         

(** To reference a piece of c_ast, we can copy without duplicated part of ast 
*)
class ptr_ast : c_ast -> object
    val p_ast: c_ast ref
    
    method ast : c_ast   
end                      

type plug = <
file_extensions:string list list; (** 
    This list represents the file extensions that the plugin can import.
	The plugin can ask for several files of the same name but of different 
	extensions for the import (e.g. ["ml","mli"] means that the plugin 
	can import a Core.gset from the files my_file.ml my_file.mli).

	When the project has been explored for the first extension list, 
	we continue with the tail of the list but we ignore the files already
	imported (e.g. [["ml";"mli"];["ml"]]) *)

path_to_c_ast:string->c_ast * string list;(**
    Construct a c_ast from a file or a directory
    @param path, this is the path dir or the file location
    @return the c_ast related to path and a list of all files which are not used
        by the plugin*)

string_to_c_ast:string->c_ast;(**
    Construct a c_ast from file
    @param str string of file
    @return the c_ast related to file*) 

c_ast_to_folder:string->c_ast->unit(** 
    Export a c_ast into a folder, ready for compilation/execution 
    @param path : path of the folder                                       
    @param ast : c_ast to export
    @return the string(ocaml code) related to ast*)
>

val print_c_ast : c_ast->unit                              
