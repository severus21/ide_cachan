
(** This module type is the signature that a plugin must provide to make 
the ide compatible with some language (fe means front-end)*)
module type Plugin_fe = 
  sig
    (** this list represents the file extensions that the plugin can import
	the plugin can ask forseveral files of the same name but of different 
	extensions for the import (e.g. ["ml","mli"] means that the plugin 
	can import a Core.gset from the files my_file.ml my_file.mli 
   *)
    val file_extension :  (string list) list

    (** Example of use : string_to_set [("an ml string","ml"); ("an mli string","mli")]*)
    val string_to_set : ((string*string) list) -> Core.gset

    (**This function is used to export the project*)
    val set_to_string : Core.gset -> string
  end
