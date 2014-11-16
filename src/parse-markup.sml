(** @file parse-markup.sml
 *
 * @copy
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * @author John Reppy
 *
 * This module describes the representation of SMLdoc comments.
 *)

structure ParseMarkup : sig

  (** @brief parse the contents of an SMLdoc comment.
   **
   ** [parse errFn content] parses the string [content].
   ** @param errFn a callback function to report errors
   ** @param content the content of the SMLdoc comment to be parsed
   ** @return [SOME comment], where [comment] is the tree representation
   **   of the parsed comment, or else [NONE] if there was an error.
   **)
    val parse : (string -> unit) -> string -> Markup.comment option

  end = struct

    structure T = 
    structure SS = Substring

  (* split text into individual lines *)
    val splitLines = SS..tokens (fn #"\n" => true | #"\r" => true | _ => false)

  (* remove the prefix text, which is leading whitespace upto (and including) a sequence
   * of "*" characters and any trailing whitespace.
   *)
    val stripLine =
	  (SS.dropl (fn #"*" => true | _ => false)) o
	  (SS.dropl Char.isSpace) o
	  (SS.dropr Char.isSpace)

    fun parse errFn content = let
	(* split the content into individual lines and trim the prefix and any trailing whitespace *)
	  val lines = List.map stripLine (splitLines (SS.full content))
	  fun parseBlock [] =
	    | parseBlock (ln::rest) = (case SS.getc ln
		 of NONE => (* empty line *) parseBlock rest
		  | SOME(#"@", ss) => parseTag(ss, rest)
		  | SOME _ => parseNotag (ln::rest)
		(* end case *))
	  and parseTag (ln, lines) = let
		val (tag, rest) = SS.splitl Char.isAlphaNum ln
		in
		  case SS.string tag
		   of "author" =>
		    | "brief" =>
		    | "copy" =>
		    | "deprecated" =>
		    | "file" =>
		    | "param" =>
		    | "raise" =>
		    | "return" =>
		    | "version" =>
		    | "" =>
		    | tag =>
		  (* end case *)
		end
	  in
	  end

	(* parse the rest of an author tag: "@author" <ws> <string> *)

	(* parse the rest of a deprecated tag: "@author" <ws> <text> *)

	(* parse the rest of an instance tag: "@instance" <ws> <longid> <ws> <text> *)

	(* parse the rest of a parameter tag: "@param" <ws> <id> <ws> <text> *)

	(* parse the rest of a raise parameter tag: "@raise" <ws> <longid> <ws> <text> *)

	(* parse the rest of a return tag: "@return" <ws> <text> *)

	(* parse the rest of a see tag: "@see" <ws> [ <url> | <file> | <string> ] <ws> <text> *)

	(* parse the rest of a source tag: "@source" <ws> <string> *)

	(* parse the rest of a before tag: "@before" <ws> <version> <text> *)

	(* parse the rest of a version tag: "@version" <version> *)

	(* parse text *)

  end
