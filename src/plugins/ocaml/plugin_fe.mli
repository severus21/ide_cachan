
(** This module type is the signature that a plugin must provide to make
the ide compatible with some language (fe means front-end)*)

module type Plugin =
  sig

    (** This list represents the file extensions that the plugin can import.
	The plugin can ask for several files of the same name but of different 
	extensions for the import (e.g. ["ml","mli"] means that the plugin 
	can import a Core.gset from the files my_file.ml my_file.mli).

	When the project has been explored for the first extension list,
	we continue with the tail of the list but we ignore the files already
	imported (e.g. [["ml";"mli"];["ml"]]) *)
    val file_extensions :  (string list) list


    (** Example of use : string_to_set ["ml";"mli"] ["my_file.ml";"my_file.mli"]*)
    val string_to_set : string list -> string list -> Core.Gset.gset

    (**This function is used to export the project*)
    val set_to_string : Core.Gset.gset -> string
  end
