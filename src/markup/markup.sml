(* markup.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Markup =
  struct

    structure DC = DocCom

  (* information for val specs *)
    type val_info = {
	desc : DC.text_block list, 		(**< The description text. *)
	sees : (DC.see_ref * DC.text) list,	(**< The list of \@see tags. *)
	since : string option,			(**< The string in the \@since tag. *)
	befores : (string * DC.text) list,	(**< the version number and text in \@before tag *)
	deprecated : DC.text option,		(**< The text of the \@deprecated tag. *)
	params : DC.id_desc list,		(**< The list of parameter descriptions. *)
	raises : DC.id_desc list,		(**< The list of raised exceptions. *)
	return : DC.text option			(**< The description text of the return value. *)
      }

    datatype file = File of {
	src : string,		(* source file name *)
	dst : string		(* stem of destination file (usually the module name) *)
      }

  end
