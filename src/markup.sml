(** @file markup.sml
 *
 * @copy
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * @author John Reppy
 *
 * This module describes the representation of SMLdoc comments.
 *)

structure Markup =
  struct

  (** a block of text *)
    datatype text_block
      = TB_Text of text
      | TB_Style of Atom.atom * text_block list
      | TB_List of Atom.atom * text_block list list
      | TB_Blank

  (** text elements in a SMLdoc comment *)
    and text_elem
      = TXT_B of text
      | TXT_I of text
      | TXT_E of text
      | TXT_CODE of code_elem list
      | TXT_CHARS of string

  (** elements in a code string *)
    and code_elem
      = KW of string		(** keyword *)
      | PUNCT of string		(** punctuation *)
      | ID of string		(** identifier *)
      | WS of string		(** white space (tabs are expanded to spaces) *)
      | EOL			(** end-of-line *)
      | LIT of string		(** numeric, character, and string literals *)
      | COM of string		(** comment *)

    and tag
      = TAG_author of ??
      | TAG_date of ??
      | TAG_deprecated of ??
      | TAG_param of ??
      | TAG_raise of ??
      | TAG_return of ??
      | TAG_see of ??
      | TAG_since of ??
      | TAG_version of ??
      | TAG_instance of ??

    withtype text = text_elem list

  (** the representation of an SMLdoc comment. **)
    type comment = {
	pre : bool,		(**< true for comments that come before their item *)
	desc : text_block list,	(**< descriptive text *)
	tags : tag list		(**< optional tags *)
      }

  end
