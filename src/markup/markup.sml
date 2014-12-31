(* markup.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Markup =
  struct

    structure DC = DocCom

  (* information about a field in a record type *)
    type field_info = {
	desc : DC.text_block list,
	sees : (DC.see_ref * DC.text) list,	(**< The list of \@see tags. *)
	since : string option,			(**< The string in the \@since tag. *)
	befores : (string * DC.text) list,	(**< the version number and text in \@before tag *)
	deprecated : DC.text option		(**< The text of the \@deprecated tag. *)
      }

  (* information about a datatype or execption constructor *)
    type con_info = {
	desc : DC.text_block list,
	sees : (DC.see_ref * DC.text) list,	(**< The list of \@see tags. *)
	since : string option,			(**< The string in the \@since tag. *)
	befores : (string * DC.text) list,	(**< the version number and text in \@before tag *)
	deprecated : DC.text option		(**< The text of the \@deprecated tag. *)
      }

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

    type id = Id.id

    type tyvar = string

    datatype file = File of {
	src : string,		(* source file name *)
	dst : string		(* stem of destination file (usually the module name) *)
      }

  (* SML types *)
    and typ
      = VARty of tyvar
      | CONty of (typ list * id)
      | FUNty of (typ * typ)
      | TUPLEty of typ list
      | RECORDty of (id * typ * field_info) list
      | PARENty of typ

    and spec
      = VALspec of (id * typ * val_info)

    and con_spec
      = CONspec of id * typ option * con_info

    and where_spec
      = WHEREty of {
	    params : tyvar list,
	    id : id,
	    def : typ
	  }
      | WHEREstr of {
	    id : id,
	    def : id
	  }

  end
