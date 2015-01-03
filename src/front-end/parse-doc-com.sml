(** @file parse-doc-com.sml
 *
 * @copy
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * @author John Reppy
 *
 * This module describes the representation of SMLdoc comments.
 *)

structure ParseDocCom : sig

    exception Error of (int * string)

  (** @brief parse the contents of an SMLdoc comment.
   **
   ** [parse (lnum, content)] parses the documentation comment [content].
   ** @param lnum the starting line number of the SMLdoc comment
   ** @param content the content of the SMLdoc comment to be parsed
   ** @return the tree representation of the parsed comment
   ** @raise [Error(lnum, msg)] for a parsing error.
   **)
    val parse : (int * bool * DCTokens.token list) -> DocCom.comment

  end = struct

    structure DC = DocCom
    structure T = DCTokens
    structure AMap = AtomMap

    exception Error of (int * string)

    fun error (lnum, msg) = raise Error (lnum, String.concat msg)

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
    type stream = (int * T.token list)

    fun expectedError ((lnum, []), s) =
	  error (lnum, ["expected ", s, ", but found nothing"])
      | expectedError ((lnum, tok::_), s) =
	  error (lnum, ["expected ", s, ", but found '", T.toString tok, "'"])

    fun unexpectedError ((lnum, []), cxt) =
	  error (lnum, ["unexpected end of comment in ", cxt])
      | unexpectedError ((lnum, tok::_), cxt) =
	  error (lnum, ["unexpected '", T.toString tok, "' in ", cxt])

    fun skipWS (ts : stream) = (case ts
	   of (lnum, T.EOL :: r) => (lnum+1, r)
	    | (lnum, T.WS _ :: r) => (lnum, r)
	    | _ => ts
	  (* end case *))

  (* eat expected whitespace *)
    fun eatWS (ts : stream) = (case ts
	   of (lnum, T.WS _ :: r) => skipWS(lnum, r)
	    | (lnum, T.EOL :: r) => skipWS(lnum+1, r)
	    | _ => expectedError (ts, "whitespace")
	  (* endd case *))

    fun next (ts : stream) = (case ts
	   of (lnum, T.EOL :: r) => SOME(T.EOL, (lnum+1, r))
	    | (lnum, tok :: r) => SOME(tok, (lnum, r))
	    | _ => NONE
	  (* end case *))

    fun parse (lnum, isPre, toks) = let
	  fun parseBlocks ts = let
		fun parse (ts, blks) = (case parseBlock ts
		       of NONE => (List.rev blks, ts)
			| SOME(blk, ts) => parse (ts, blk::blks)
		      (* end case *))
		in
		  parse (ts, [])
		end
	  and parseBlock ts = (case next ts
		 of NONE => NONE
		  | SOME(T.BEGIN env, ts') => (case AMap.find (kindMap, env)
		       of SOME STYLE => SOME(parseStyleBlock (env, ts'))
			| SOME LIST => SOME(parseListBlock (env, ts'))
			| NONE => error (#1 ts, ["unknown block kind '", Atom.toString env, "'"])
		      (* end case *))
		  | SOME(T.BLANKLN, ts') => SOME(DC.TB_Blank, ts')
		  | SOME(T.TAG _, _) => NONE
		  | SOME(tok as T.END _, _) => error (#1 ts, ["unexpected '", T.toString tok, "'"])
		  | SOME(T.ITEM, _) => error (#1 ts, ["unexpected '\\item'"])
		  | _ => (case parseText ts
		       of ([], _) => expectedError (ts, "text")
			| (txt, ts') => SOME(DC.TB_Text txt, ts')
		      (* end case *))
		(* end case *))
	(* parse "\begin{style}" ... "\end{style}" *)
	  and parseStyleBlock (style, ts) = let
	      (* parse loop for block content *)
		fun parse (ts, blks) = (case next ts
		       of NONE => unexpectedError (ts, concat["open '", Atom.toString style, "'"])
			| SOME(T.END style', ts') => if Atom.same(style, style')
			    then (DC.TB_Style(style, List.rev blks), ts')
			    else expectedError (ts, concat["'\\end{", Atom.toString style, "}'"])
			| _ => (case parseBlock ts
			     of SOME(blk, ts') => parse (ts', blk::blks)
			      | NONE => expectedError (ts, concat["'\\end{", Atom.toString style, "}'"])
			    (* end case *))
		      (* end case *))
		in
		  parse (skipWS ts, [])
		end
	  and parseListBlock (list, ts) = let
	      (* parse loop for list content *)
		fun parse (ts, items) = (case next ts
		       of SOME(T.ITEM, ts') => let
			    val (blks, ts') = parseBlocks ts'
			    in
			      parse (ts', blks::items)
			    end
			| SOME(T.END list', ts') => if Atom.same(list, list')
			    then (DC.TB_List(list, List.rev items), ts')
			    else expectedError (ts, concat["'\\end{", Atom.toString list, "}'"])
			| _ => expectedError (ts, concat[
			      "either '\\end{", Atom.toString list, "}' or '\\item'"
			    ])
		      (* end case *))
		in
		  parse (skipWS ts, [])
		end
	  and parseText ts = let
		fun parse (ts, elems) = (case parseTextElem ts
		       of NONE => (List.rev elems, ts)
			| SOME(elem, ts') => parse (ts', elem::elems)
		      (* end case *))
		in
		  parse (ts, [])
		end
	  and parseTextElem ts = let
		fun style con ts = let
		      val (elems, ts') = parseText ts
		      in
			case next ts'
			 of SOME(T.CLOSE, ts') => SOME(con elems, ts')
			  | _ => expectedError (ts', "'}'")
			(* end case *)
		      end
		fun parse (ts, txt) = (case next ts
		       of SOME(T.TEXT s, ts') => parse (ts', s::txt)
			| SOME(T.WS ws, ts') => parse (ts', ws::txt)
			| SOME(T.EOL, ts') => parse (ts', "\n"::txt)
			| _ => SOME(DC.TXT_CHARS(String.concat (List.rev txt)), ts)
		      (* end case *))
		in
		  case next ts
		   of SOME(T.TEXT s, ts') => parse (ts', [s])
		    | SOME(T.WS ws, ts') => parse (ts', [ws])
		    | SOME(T.EOL, ts') => parse (ts', ["\n"])
		    | SOME(T.BOLD, ts') => style DC.TXT_B ts'
		    | SOME(T.ITALIC, ts') => style DC.TXT_I ts'
		    | SOME(T.EMPH, ts') => style DC.TXT_EM ts'
		    | SOME(T.CLOSE, ts') => unexpectedError (ts, "text element")
		    | SOME(T.CODE, ts') => parseCode ts'
		    | SOME(T.CLOSE_CODE, ts') => unexpectedError (ts, "text element")
		    | _ => NONE
		  (* end case *)
		end
	  and parseCode ts = let
		fun parse (ts, toks) = (case next ts
		       of SOME(T.CLOSE_CODE, ts') => SOME(DC.TXT_CODE(List.rev toks), ts')
			| SOME(T.KW kw, ts') => parse (ts', DC.KW kw :: toks)
			| SOME(T.SYM s, ts') => parse (ts', DC.KW s :: toks)
			| SOME(T.PUNCT p, ts') => parse (ts', DC.PUNCT p :: toks)
			| SOME(T.ID id, ts') => parse (ts', DC.ID id :: toks)
			| SOME(T.INT n, ts') => parse (ts', DC.INT n :: toks)
			| SOME(T.WORD w, ts') => parse (ts', DC.WORD w :: toks)
			| SOME(T.REAL f, ts') => parse (ts', DC.REAL f :: toks)
			| SOME(T.STRING s, ts') => parse (ts', DC.STR s :: toks)
			| SOME(T.CHAR c, ts') => parse (ts', DC.CHR c :: toks)
			| SOME(T.COM s, ts') => parse (ts', DC.COM s :: toks)
			| SOME(T.WS ws, ts') => parse (ts', DC.WS ws :: toks)
			| SOME(T.EOL, ts') => parse (ts', DC.EOL :: toks)
			| _ => unexpectedError (ts, "SML code")
		      (* end case *))
		in
		  parse (ts, [])
		end
	  and parseTags ts = let
		fun parse (ts, tags) = (case next ts
		     of NONE => List.rev tags
		      | SOME(T.TAG tag, ts') => let
			  val (tag, ts') = parseTag (tag, ts')
			  in
			    parse (skipWS ts', tag::tags)
			  end
		      | _ => expectedError (ts, "documentation tag")
		    (* end case *))
		in
		  parse (skipWS ts, [])
		end
	(* parse the contents of the individual tag *)
	  and parseTag (tag, ts) = let
		val ts = eatWS ts
		in
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
		  else error (#1 ts, ["unknown tag '@", Atom.toString tag, "'"])
		end
	  and parseTagString (tagName, mkTag, ts) = let
		fun done (txt, ts') = (mkTag(String.concat(List.rev txt)), ts')
		fun parseString (ts, txt) = (case next ts
		       of NONE => done(txt, ts)
			| SOME(T.EOL, ts') => done (txt, ts')
			| SOME(T.WS s, ts') => parseString (ts', s::txt)
			| SOME(T.TEXT s, ts') => parseString (ts', s::txt)
			| _ => error (#1 ts, ["malformed '@", tagName, "' tag"])
		      (* end case *))
		in
		  parseString (ts, [])
		end
	(* parse a tag body of the form: <id> <ws> <text> *)
	  and parseTagIdText (tagName, mkTag, ts) = (case next ts
		 of SOME(T.TEXT id, ts') => let
		      val (desc, ts') = parseText ts'
		      in
			(mkTag {id = id, desc = desc}, ts')
		      end
		  | _ => error (#1 ts, ["malformed '@", tagName, "' tag"])
		(* end case *))
	(* parse a tag body of the form: <text> *)
	  and parseTagText (mkTag, ts) = let
		val (desc, ts') = parseText ts
		in
		  (mkTag desc, ts')
		end
	(* parse the rest of an author tag: "@author" <ws> <string> *)
	  and parseAuthor ts = parseTagString ("author", DC.TAG_author, ts)
	(* parse the rest of a before tag: "@before" <ws> <version> <text> *)
	  and parseBefore ts = raise Fail "@before unimplmented"
	(* parse the rest of a date tag: "@date" <ws> YYYY-MM-DD *)
	  and parseDate ts = (case next ts
		 of SOME(T.TEXT s, ts') => let
		      fun continue (tag, ts) = (case next ts
			     of NONE => (tag, ts)
			      | SOME(T.EOL, ts') => (tag, ts')
			      | SOME(T.WS _, ts') => continue (tag, ts')
			      | _ => error (#1 ts, ["malformed '@date' tag"])
			    (* end case *))
		      in
		      (* parse the date, which should be in YYYY-MM-DD format *)
			case List.map Int.fromString (String.fields (fn #"-" => true | _ => false) s)
			 of [SOME y, SOME m, SOME d] =>
			      continue (DC.TAG_date{year=y, month=m, day=d}, ts')
			  | _ => error (#1 ts, ["malformed '@date' tag"])
			(* end case *)
		      end
		  | _ => error (#1 ts, ["malformed '@date' tag"])
		(* end case *))
	(* parse the rest of a deprecated tag: "@deprecated" <ws> <text> *)
	  and parseDeprecated ts = parseTagText (DC.TAG_deprecated, ts)
	(* parse the rest of an instance tag: "@instance" <ws> <longid> <ws> <text> *)
	  and parseInstance ts = parseTagIdText ("instance", DC.TAG_instance, ts)
	(* parse the rest of a parameter tag: "@param" <ws> <id> <ws> <text> *)
	  and parseParam ts = parseTagIdText ("param", DC.TAG_param, ts)
	(* parse the rest of a raise parameter tag: "@raise" <ws> <longid> <ws> <text> *)
	  and parseRaise ts = parseTagIdText ("raise", DC.TAG_raise, ts)
	(* parse the rest of a return tag: "@return" <ws> <text> *)
	  and parseReturn ts = parseTagText (DC.TAG_return, ts)
	(* parse the rest of a see tag: "@see" <ws> [ <url> | <file> | <string> ] <ws> <text> *)
	  and parseSee ts = raise Fail "@see unimplemented"
	(* parse the rest of a since tag: "@since" <ws> <version> *)
	  and parseSince ts = parseTagString ("since", DC.TAG_since, ts)
	(* parse the rest of a version tag: "@version" <ws> <version> *)
	  and parseVersion ts = parseTagString ("version", DC.TAG_version, ts)
	  and parseComment ts = let
		val (blks, ts') = parseBlocks ts
		val tags = parseTags ts
		in
		  {pre = isPre, desc = blks, tags = tags}
		end
	  in
	    parseComment (lnum, toks)
	  end

  end
