(* markup-tokens.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure MarkupTokens =
  struct

    datatype token
      = EOF		(* end-of-file *)
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
    (* identifiers and literals *)
      | ID of Atom.atom
      | INT of string
      | REAL of string
      | STRING of string
      | CHAR of string
    (* documentation comments *)
      | BLANKLN				(* blank line following/proceeding documentation comment *)
      | TEXT of string			(* non-whitespace text *)
      | WS of string			(* whitespace (other than line breaks) *)
      | EOL				(* end-of-line *)
    (* Text markup tokens *)
      | BEGIN of Atom.atom		(* '\begin{xxx}' *)
      | END of Atom.atom		(* '\end{xxx}' *)
      | ITEM				(* '\item' *)
      | BOLD				(* '\b{' *)
      | ITALIC				(* '\i{' *)
      | EMPH				(* '\e{' *)
      | CLOSE				(* '}' *)
      | CODE				(* '[' *)
      | CLOSE_CODE			(* ']' *)
    (* Documentation tags *)
      | TAG of Atom.atom		(* "@xxx" tag *)

  end



