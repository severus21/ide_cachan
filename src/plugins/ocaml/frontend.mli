(**TODO extend some generic frontend*)

(** This list represents the file extensions that the plugin can import.
	The plugin can ask for several files of the same name but of different 
	extensions for the import (e.g. ["ml","mli"] means that the plugin 
	can import a Core.gset from the files my_file.ml my_file.mli).

	When the project has been explored for the first extension list, 
	we continue with the tail of the list but we ignore the files already
	imported (e.g. [["ml";"mli"];["ml"]]) *)
val file_extensions : string list list

(** Construct a c_ast from file
    @param filename full path
    @param str string of file
    @return the c_ast related to file*) 
val string_to_c_ast : string-> string -> Core.Miscs.c_ast

(** Export a c_ast to str
    @param ast : c_ast to export
    @return the string(ocaml code) related to ast*) 
val c_ast_to_str : Core.Miscs.c_ast -> string 
