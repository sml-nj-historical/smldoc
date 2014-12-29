(* markup-tokens.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure MarkupTokens =
  struct

  (* the tokens in a documentation comment *)
    datatype token
    (* Text markup tokens *)
      = BEGIN of Atom.atom		(* '\begin{xxx}' *)
      | END of Atom.atom		(* '\end{xxx}' *)
      | ITEM				(* '\item' *)
      | BOLD				(* '\b{' *)
      | ITALIC				(* '\i{' *)
      | EMPH				(* '\e{' *)
      | CLOSE				(* '}' *)
      | CODE				(* '[' *)
      | CLOSE_CODE			(* ']' *)
    (* Code markup tokens *)
      | KW of Atom.atom			(* reserved IDs *)
      | SYM of Atom.atom		(* reserved symbols (e.g., "*", ":=", ...) *)
      | PUNCT of Atom.atom		(* punctuation (e.g., ",", "{", "}", ...) *)
      | ID of Atom.atom			(* identifiers *)
      | INT of string			(* integer literals *)
      | WORD of string			(* word literals *)
      | REAL of string			(* real literals *)
      | STRING of string		(* string literals *)
      | CHAR of string			(* character literals *)
    (* documentation comments *)
      | BLANKLN				(* blank line following/proceeding documentation comment *)
      | TEXT of string			(* non-whitespace text *)
      | WS of string			(* whitespace (other than line breaks) *)
      | EOL				(* end-of-line *)
    (* Documentation tags *)
      | TAG of Atom.atom		(* "@xxx" tag *)

    fun toString (BEGIN env) = String.concat["\\begin{", Atom.toString env, "}"]
      | toString (END env) = String.concat["\\end{", Atom.toString env, "}"]
      | toString ITEM = "\\item"
      | toString BOLD = "\\b{"
      | toString ITALIC = "\\i{"
      | toString EMPH = "\\e{"
      | toString CLOSE = "}"
      | toString CODE = "["
      | toString CLOSE_CODE = "]"
      | toString tok = "<token>" (* FIXME *)

  end



