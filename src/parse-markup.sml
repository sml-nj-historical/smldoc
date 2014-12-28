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
   ** [parse errFn (lnum, content)] parses the documentation comment [content].
   ** @param errFn a callback function to report errors
   ** @param lnum the starting line number of the SMLdoc comment
   ** @param content the content of the SMLdoc comment to be parsed
   ** @return [SOME comment], where [comment] is the tree representation
   **   of the parsed comment, or else [NONE] if there was an error.
   **)
    val parse : (int * string -> unit) -> (int * MarkupTokens.token list) -> Markup.comment option

  end = struct

    structure T = MarkupTokens
    structure ASet = AtomSet

  (* text block environments *)
    val a_center = Atom.atom "center"
    val a_quote = Atom.atom "quote"

  (* list environments *)
    val a_enumerate = Atom.atom "enumerate"
    val a_itemize = Atom.atom "itemize"

    datatype block_kind = STYLE | LIST

    val kindMap = List.foldl AMap.insert' AMap.empty [
	    (a_center, STYLE),		(* "\begin{center}" *)
	    (a_enumerate, LIST),	(* "\begin{enumerate}" *)
	    (a_itemize, LIST),		(* "\begin{itemize}" *)
	    (a_quote, STYLE)		(* "\begin{quote}" *)
	  ]

  (* "@" tags *)
    val a_author = Atom.atom "author"
    val a_before = Atom.atom "before"
    val a_date = Atom.atom "date"
    val a_deprecated = Atom.atom "deprecated"
    val a_instance = Atom.atom "instance"
    val a_param = Atom.atom "param"
    val a_raise = Atom.atom "raise"
    val a_return = Atom.atom "return"
    val a_see = Atom.atom "see"
    val a_since = Atom.atom "since"
    val a_version = Atom.atom "version"

  (* a stream pairs a line number with the list of tokens *)
    type stream = (int * token list)

    fun skipWS (ts : stream) = (case ts
	   of (lnum, T.EOL :: r) => (lnum+1, r)
	    | (lnum, T.WS _ :: r) => (lnum, r)
	    | _ => ts
	  (* end case *))

    fun next (ts : stream) = (case ts
	   of (lnum, T.EOL :: r) => SOME(T.EOL, (lnum+1, r))
	    | (lnum, tok :: r) => SOME(tok, (lnum, r))
	    | _ => NONE
	  (* end case *))

    fun parse errFn (lnum, isPre, toks) = let
	  fun parseBlock (ts, blks) = (case next ts
		 of NONE => {pre = isPre, desc = List.rev blks, tags = []}
		  | T.BEGIN env :: r => (case AMap.find (kindMap, env)
		       of SOME STYLE => let
			    val (blk, lnum, rest) = parseStyleBlock (lnum, env, r)
			    in
			      parseBlock (lnum, rest, blk::blks)
			    end
			| SOME LIST => parseListBlock (lnum, env, r)
			| NONE => (* error *)
		      (* end case *))
		  | T.TAG _ :: _ => (* switch to parsing the tags *)
		      parseTags (lnum, toks, List.rev blks)
		  | T.END _ :: _ => (* error: unexpected \end{xxx} *)
		  | T.ITEM :: _ => (* error: unexpected \item *)
		  | T.BLANKLN :: rest => parseBlock (lnum+1, rest, blks)
		  | _ => ?? (* parse text *)
		(* end case *))
	  fun parseBlock ts = (case next ts
		 of NONE => NONE
		  | SOME(T.BEGIN env, ts') => (case AMap.find (kindMap, env)
		       of SOME STYLE => SOME(parseStyleBlock (env, ts'))
			| SOME LIST => SOME(parseListBlock (env, ts'))
			| NONE => (* error *)
		      (* end case *))
		  | SOME(T.BLANKLN, ts') => SOME(M.TB_Blank, ts')
		  | SOME(T.TAG _, _) => NONE
		  | SOME(T.END, _) => (* error: unexpected \end{xxx} *)
		  | SOME(T.ITEM, _) => (* error: unexpected \item *)
		  | _ => ?? (* parse text *)
		(* end case *))
	(* parse "\begin{style}" ... "\end{style}" *)
	  and parseStyleBlock (style, ts) = let
	      (* parse loop for block content *)
		fun parse (ts, blks) = (case next ts
		       of NONE => (* error: unexpected end-of-comment *)
			| SOME(T.END style', ts') => if Atom.same(style, style')
			    then (M.TB_Style(style, List.rev blks), ts')
			    else (* error: wrong close tag *)
			| _ => let
			    val (blk, ts') = parseBlock ts
			    in
			      parse (ts', blk::blks)
			    end
		      (* end case *))
		in
		  parse (skipWS ts, blks)
		end
	  and parseListBlock (list, ts) = let
	      (* parse loop for list content *)
		fun parse (ts, items) = (case next ts
		       of SOME(T.ITEM, ts') => let
			    val (blks, ts') = parseBlocks ts'
			    in
			      parse (ts', blks::items)
			    end
			| SOME(T.END style', ts') => if Atom.same(style, style')
			    then (M.TB_Style(style, List.rev blks), ts')
			    else (* error: wrong close tag *)
			| _ => (* error: expected either "\end" or "\item" *)
		      (* end case *))
		in
		  parse (skipWS ts, blks)
		end
	  and parseText ts = (case next ts
		 of SOME(T.BLANKLN, ts') =>
		  | SOME(T.TEXT s, ts') =>
		  | SOME(T.WS ws, ts') =>
		  | SOME(T.EOL, ts') =>
		  | SOME(T.BOLD, ts') =>
		  | SOME(T.ITALIC, ts') =>
		  | SOME(T.EMPH, ts') =>
		  | SOME(T.CLOSE, ts') =>
		  | SOME(T.CODE, ts') =>
		  | SOME(T.CLOSE_CODE, ts') =>
		(* end case *))
	  and parseCode ts =
	  and parseTags ts =
	  and parseTag (tag, ts) =
		if Atom.same(tag, a_author) then parseAuthor ts
		else if Atom.same(tag, a_before) then parseBefore ts
		else if Atom.same(tag, a_date) then parseDate ts
		else if Atom.same(tag, a_deprecated) then parseDeprecated ts
		else if Atom.same(tag, a_instance) then parseInstance ts
		else if Atom.same(tag, a_param) then parseParam ts
		else if Atom.same(tag, a_raise) then parseRaise ts
		else if Atom.same(tag, a_return) then parseReturn ts
		else if Atom.same(tag, a_see) then parseSee ts
		else if Atom.same(tag, a_since) then parseSince ts
		else if Atom.same(tag, a_version) then parseVersion ts
		else (* error *)
	(* parse the rest of an author tag: "@author" <ws> <string> *)
	  and parseAuthor ts =
	  and parseDate ts =
	  and parseDeprecated ts =
	(* parse the rest of an instance tag: "@instance" <ws> <longid> <ws> <text> *)
	  and parseInstance ts =
	(* parse the rest of a parameter tag: "@param" <ws> <id> <ws> <text> *)
	  and parseParam ts =
	(* parse the rest of a raise parameter tag: "@raise" <ws> <longid> <ws> <text> *)
	  and parseRaise ts =
	(* parse the rest of a return tag: "@return" <ws> <text> *)
	  and parseReturn ts =
	(* parse the rest of a see tag: "@see" <ws> [ <url> | <file> | <string> ] <ws> <text> *)
	  and parseSee ts =
	  and parseSince ts =
	(* parse the rest of a version tag: "@version" <version> *)
	  and parseVersion ts =
	  and parseComment ts = let
		val (blks, ts') = parseBlocks ts
		val (tags, ts') = parseTags ts
		in
(* QUESTION: do we need to check for end of comment? *)
		  {pre = isPre, desc = blks, tags = tags}
		end
	  in
	    parseComment (lnum, toks)
	  end

  end
