(* tokens.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

strstructure Tokens =
  struct

    datatype Tokens =
      = EOF		(* end-of-file *)
      | BLANK		(* blank line *)
      | WS		(* whitespace (other than line breaks) *)
      | EOL		(* end-of-line *)
    (* reserved IDs *)
      | KW_abstype | KW_and | KW_andalso | KW_as
      | KW_case
      | KW_datatype | KW_do
      | KW_else | KW_end | KW_eqtype | KW_exception
      | KW_fn | KW_fun
      | KW_functor | KW_handle
      | KW_if | KW_in | KW_include | KW_infix | KW_infixr
      | KW_let | KW_local
      | KW_nonfix
      | KW_orelse | KW_of | KW_op | KW_open
      | KW_raise | KW_rec
      | KW_sharing | KW_sig | KW_signature | KW_struct | KW_structure
      | KW_then | KW_type
      | KW_val | KW_where | KW_while | KW_with | KW_withtype
    (* reserved symbolic IDs *)
      | ASTERISK	(* '*' *)
      | ASSIGN		(* ':=' *)
      | AMPERSAND	(* '&' *)
      | BANG		(* '!' *)
      | SLASH		(* '/' *)
      | ARROW		(* '->' *)
      | BAR		(* '|' *)
      | COLON		(* ':' *)
      | COLONGT		(* ':>' *)
      | EQUALOP		(* '=' *)
      | DARROW		(* '=>' *)
      | HASH		(* '#' *)
    (* Punctuation *)
      | COMMA		(* ',' *)
      | LBRACE		(* '{' *)
      | RBRACE		(* '}' *)
      | LBRACKET	(* '[' *)
      | RBRACKET	(* ']' *)
      | SEMICOLON	(* ';' *)
      | LPAREN		(* '(' *)
      | RPAREN		(* ')' *)
      | DOTDOTDOT	(* '...' *)
    (* *)
      | ID of Atom.atom
      | INT of string
      | REAL of string
      | STRING of string
      | CHAR of string
    (* Documentation-comment tokens *)
      | TAG_author	(* '@author' *)
      | TAG_deprecated	(* '@deprecated' *)
      | TAG_param	(* '@param' *)
      | TAG_raise	(* '@raise' *)
      | TAG_return	(* '@return' *)
      | TAG_see		(* '@see' *)
      | TAG_since	(* '@since' *)
      | TAG_source	(* '@source' *)
      | TAG_before	(* '@before' *)
      | TAG_version	(* '@version' *)
      | TAG_instance	(* '@instance' *)
      | SECTION of int	(* '{0', '{1', '{2', ... *)
      | BOLD		(* '{b' *)
      | ITALIC		(* '{i' *)
      | EMPH		(* '{e' *)
      | CENTER		(* '{C' *)
      | LEFT		(* '{L' *)
      | RIGHT		(* '{R' *)
      | ITEMIZE		(* '{ul' *)
      | ENUMERATE	(* '{ol' *)
      | ITEM		(* '{-' *)
      | CLOSE		(* '}' *)
      | PRE_CODE	(* '{[' *)
      | CLOSE_PRE_CODE	(* ']}' *)
      | CODE		(* '[' *)
      | CLOSE_CODE	(* ']' *)

  end



