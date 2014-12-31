(** @file doc-com.sml
 *
 * @copy
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * @author John Reppy
 *
 * This module describes the representation of SMLdoc comments.
 *)

structure DocCom =
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
      | TXT_EM of text
      | TXT_CODE of code_elem list
      | TXT_CHARS of string

  (** elements in a code string *)
    and code_elem
      = KW of Atom.atom		(**< keyword (including reserved symbols) *)
      | PUNCT of Atom.atom	(**< punctuation *)
      | ID of Atom.atom		(**< identifier *)
      | LIT of string		(**< numeric, character, and string literals *)
      | COM of string		(**< comment *)
      | WS of string		(**< white space (tabs are expanded to spaces) *)
      | EOL			(**< end-of-line *)

    and tag
      = TAG_author of string
      | TAG_before of string * text
      | TAG_copy of text
      | TAG_date of {year : int, month : int, day : int}
      | TAG_deprecated of text
      | TAG_instance of id_desc
      | TAG_param of id_desc
      | TAG_raise of id_desc
      | TAG_return of text
      | TAG_see of see_ref * text
      | TAG_since of string
      | TAG_version of string

  (** The different forms of references in \@see tags. *)
    and see_ref
      = SEE_url of string
      | SEE_file of string
      | SEE_doc of string

    withtype text = text_elem list
         and id_desc = {id : string, desc : text}

  (** the representation of an SMLdoc comment. **)
    type comment = {
	pre : bool,		(**< true for comments that come before their item *)
	desc : text_block list,	(**< descriptive text *)
	tags : tag list		(**< optional tags *)
      }

  end
