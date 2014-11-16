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

  (** @brief text elements in a SMLdoc comment *)
    datatype elem
      = Text of string		(** raw text *)
      | Bold of text		(** boldfaced text *)
      | Italic of text		(** italic text *)
      | Code of code_elem list	(** source code *)
      | Special of string	

  (**@brief elements in a code string *)
    and code_elem
      = KW of string		(** keyword *)
      | PUNCT of string		(** punctuation *)
      | ID of string		(** identifier *)
      | WS of string		(** white space (tabs are expanded to spaces) *)
      | EOL			(** end-of-line *)
      | LIT of string		(** numeric, character, and string literals *)
      | COM of string		(** comment *)

    withtype text = elem list

  (** @brief a block of text, possibly proceeded by a tag *)
    datatype block
      = NoTag of text
      | Author of string list
      | Brief of text
      | Copy of string list
      | Deprecated of text
      | File of string
      | Param of string * text
      | Raise of string * text
      | Return of text
      | Version of string

  (** @brief the representation of an SMLdoc comment.
   **
   ** An SMLdoc comment is organized into one or more tagged blocks.  A block is terminated
   ** by either a blank line or by the occurance of a new tag.
   **)
    type comment = block list

  end
