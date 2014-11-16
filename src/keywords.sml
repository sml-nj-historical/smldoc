(* keywords.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 *)

structure Keywords : sig

  (** May identifiers (both alpha-numeric and symbolic) to tokens. *)
    val idToken : string -> SMLTokens.token

  end = struct

    structure T = SMLTokens

    val keywords = [
	    ("abstype",		KW_abstype),
	    ("and",		KW_and),
	    ("andalso",		KW_andalso),
	    ("as",		KW_as),
	    ("case",		KW_case),
	    ("datatype",	KW_datatype),
	    ("do",		KW_do),
	    ("else",		KW_else),
	    ("end",		KW_end),
	    ("eqtype",		KW_eqtype),
	    ("exception",	KW_exception),
	    ("fn",		KW_fn),
	    ("fun",		KW_fun),
	    ("functor",		KW_functor),
	    ("handle",		KW_handle),
	    ("if",		KW_if),
	    ("in",		KW_in),
	    ("include",		KW_include),
	    ("infix",		KW_infix),
	    ("infixr",		KW_infixr),
	    ("let",		KW_let),
	    ("local",		KW_local),
	    ("nonfix",		KW_nonfix),
	    ("orelse",		KW_orelse),
	    ("of",		KW_of),
	    ("op",		KW_op),
	    ("open",		KW_open),
	    ("raise",		KW_raise),
	    ("rec",		KW_rec),
	    ("sharing",		KW_sharing),
	    ("sig",		KW_sig),
	    ("signature",	KW_signature),
	    ("struct",		KW_struct),
	    ("structure",	KW_structure),
	    ("then",		KW_then),
	    ("type",		KW_type),
	    ("val",		KW_val),
	    ("where",		KW_where),
	    ("while",		KW_while),
	    ("with",		KW_with),
	    ("withtype",	KW_withtype),
	  (* symbolic reserved words *)
	    ("*",		ASTERISK),
	    (":=",		ASSIGN),
	    ("&",		AMPERSAND),
	    ("!",		BANG),
	    ("/",		SLASH),
	    ("->",		ARROW),
	    ("|",		BAR),
	    (":",		COLON),
	    (":>",		COLONGT),
	    ("=",		EQUALOP),
	    ("=>",		DARROW),
	    ("#",		HASH)
	  ]

  (* create a keyword lookup table *)
    local
      fun mkFind kws = let
            val tbl = AtomTable.mkTable (17, Fail "keywords")
            fun ins (id, tok) = AtomTable.insert tbl (Atom.atom id, tok)
            val find = AtomTable.find tbl
            fun idToken id = let
                  val id = Atom.atom id
                  in
                    case find id
                     of NONE => T.ID id
                      | SOME kw => kw
                    (* end case *)
                  end
            in
              List.app ins kws;
              idToken
            end
    in
  (* return either a keyword token or an ID token *)
      val idToken = mkFind keywords
    end

  end
