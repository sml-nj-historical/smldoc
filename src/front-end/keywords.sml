(* keywords.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 *)

structure Keywords : sig

  (** Map alpha-numeric identifiers to parser tokens. *)
    val idToken : string -> SMLDocTokens.token
  (** Map symbolic identifiers to parser tokens. *)
    val symToken : string -> SMLDocTokens.token

  (** Map alpha-numeric identifiers to documentation comment tokens. *)
    val idToken' : string -> DCTokens.token
  (** Map symbolic identifiers to documentation comment tokens. *)
    val symToken' : string -> DCTokens.token

  end = struct

    structure T = SMLDocTokens
    structure DC = DCTokens

    val keywords = [
	    ("abstype",		T.KW_abstype),
	    ("and",		T.KW_and),
	    ("andalso",		T.KW_andalso),
	    ("as",		T.KW_as),
	    ("case",		T.KW_case),
	    ("datatype",	T.KW_datatype),
	    ("do",		T.KW_do),
	    ("else",		T.KW_else),
	    ("end",		T.KW_end),
	    ("eqtype",		T.KW_eqtype),
	    ("exception",	T.KW_exception),
	    ("fn",		T.KW_fn),
	    ("fun",		T.KW_fun),
	    ("functor",		T.KW_functor),
	    ("handle",		T.KW_handle),
	    ("if",		T.KW_if),
	    ("in",		T.KW_in),
	    ("include",		T.KW_include),
	    ("infix",		T.KW_infix),
	    ("infixr",		T.KW_infixr),
	    ("let",		T.KW_let),
	    ("local",		T.KW_local),
	    ("nonfix",		T.KW_nonfix),
	    ("orelse",		T.KW_orelse),
	    ("of",		T.KW_of),
	    ("op",		T.KW_op),
	    ("open",		T.KW_open),
	    ("raise",		T.KW_raise),
	    ("rec",		T.KW_rec),
	    ("sharing",		T.KW_sharing),
	    ("sig",		T.KW_sig),
	    ("signature",	T.KW_signature),
	    ("struct",		T.KW_struct),
	    ("structure",	T.KW_structure),
	    ("then",		T.KW_then),
	    ("type",		T.KW_type),
	    ("val",		T.KW_val),
	    ("where",		T.KW_where),
	    ("while",		T.KW_while),
	    ("with",		T.KW_with),
	    ("withtype",	T.KW_withtype),
	  (* symbolic reserved words *)
	    ("*",		T.ASTERISK),
	    (":=",		T.ASSIGN),
	    ("&",		T.AMPERSAND),
	    ("!",		T.BANG),
	    ("/",		T.SLASH),
	    ("->",		T.ARROW),
	    ("|",		T.BAR),
	    (":",		T.COLON),
	    (":>",		T.COLONGT),
	    ("=",		T.EQUALOP),
	    ("=>",		T.DARROW),
	    ("#",		T.HASH)
	  ]

  (* create a keyword lookup table *)
    local
      val find = let
            val tbl = AtomTable.mkTable (17, Fail "keywords")
            fun ins (id, tok) = AtomTable.insert tbl (Atom.atom id, tok)
            in
              List.app ins keywords;
              AtomTable.find tbl
            end
    in
    fun idToken id = let
	  val id = Atom.atom id
	  in
	    case find id
	     of NONE => T.ID id
	      | SOME kw => kw
	    (* end case *)
	  end
    fun symToken id = let
	  val id = Atom.atom id
	  in
	    case find id
	     of NONE => T.SYMID id
	      | SOME kw => kw
	    (* end case *)
	  end
  (* versions for documentation comments *)
    fun idToken' id = let
	  val id = Atom.atom id
	  in
	    case find id
	     of NONE => DC.ID id
	      | SOME _ => DC.KW id
	    (* end case *)
	  end
    fun symToken' id = let
	  val id = Atom.atom id
	  in
	    case find id
	     of NONE => DC.ID id
	      | SOME _ => DC.SYM id
	    (* end case *)
	  end
    end

  end
