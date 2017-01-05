open Core.Miscs

type plug = <
name:string; (**
    This string is the name of the plugin used for graphical
    interface output. *)

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
    - param1 path, this is the path dir or the file location
    @return the c_ast related to path and a list of all files which are not used
        by the plugin*)

string_to_c_ast:string->c_ast;(**
    Construct a c_ast from file
    - param1 str string of file
    @return the c_ast related to file*)

c_ast_to_folder:string->c_ast->unit;(**
    Export a c_ast into a folder, ready for compilation/execution
    - param1 path : path of the folder
    - param2 ast : c_ast to export
    @return the string(ocaml code) related to ast*)

unittests:unit->OUnit2.test(**
    Export unittests API for this plugin*)
>

