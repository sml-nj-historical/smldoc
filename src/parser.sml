(* parser.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Parser : sig

    val parseFile : string -> ATree.file option

  end = struct

    structure PT = ParseTree
    structure A = ATree

  (* glue together the lexer and parser *)
    structure SMLParser = SMLDocParseFn(SMLDocLexer)

  (* error function for lexers *)
    fun lexErr errStrm (pos, msg) = Error.errorAt(errStrm, (pos, pos), msg)

  (* error function for parsers *)
    val parseErr = Error.parseError tokToString

    fun parseFile inFile = if OS.FileSys.access(inFile, [OS.FileSys.A_READ])
	  then let
	    val inS = TextIO.openIn inFile
	    val errStrm = Error.mkErrStream inFile
	    fun get () = TextIO.input inS
	    val lexer = SMLDocLexer.lex (Error.sourceMap errStrm) (lexErr errStrm)
	    in
	      case SMLDocParser.parse lexer (SMLDocLexer.streamify get)
	       of (SOME pt, _, []) => (
		    TextIO.closeIn inS;
		    parseMarkup (errStrm, pt))
		| (_, _, errs) => (
		    TextIO.closeIn inS;
		    List.app (parseErr errStrm) errs;
		    NONE)
	      (* end case *)
	    end
	  else NONE

  (* convert the parse tree to an annotated tree by parsing the documentation comments *)
    and parseMarkup (errStrm, file) = let
	  fun cvtDoc doc = (List.map
		(fn (span, isPre, toks) => ParseMarkup.parse (?, isPre, toks))
		  doc
		) handle ParseMarup.Error(lnum, msg) => ??
	  fun cvtTyp (PT.VARty tv) = A.VARty tv
	    | cvtTyp (PT.CONty(tys, id)) = A.CONty(List.map cvtTyp tys, id)
	    | cvtTyp (PT.FUNty(ty1, ty2)) = A.FUNty(cvtTyp ty1, cvtTyp ty2)
	    | cvtTyp (PT.RECORDty flds) = let
		fun cvtFld (id, ty, doc) = (id, cvtTyp ty, cvtDoc doc)
		in
		  A.RECORDty(List.map cvtFld flds)
		end
	    | cvtTyp (PT.PARENty ty) = A.PARENty(cvtTyp ty)
	  fun cvtTop (PT.SIGdec decs) = let
		fun cvt (id, sigExp, whrSpecs) = (id, cvtSigExp sigExp, cvtWhereSpecs whrSpecs)
		in
		  A.SIGdef(List.map cvt decs)
		end
	  and cvtSigExp (PT.IDsigexp id) = A.IDsigexp id
	    | cvtSigExp (PT.SIGsigexp specs) = A.SIGsigexp(List.map cvtSpec specs)
	  and cvtSpec spec = (case spec
		 of PT.INCLspec specs =>
		      A.INCLspec(List.map (fn (id, doc) => (id, cvtDoc doc)) specs)
		  | PT.INCLWHEREspec(id, whrSpecs, doc) =>
		      A.INCLWHEREspec(id, cvtWhereSpecs whrSpecs, cvtDoc doc)
		  | PT.STRspec specs => let
		      fun cvt (id, sigExp, whrSpecs) = (id, cvtSigExp sigExp, cvtWhereSpecs whrSpecs)
		      in
			A.STRspec(List.map cvt specs)
		      end
		  | PT.SHAREspec(ids, doc) => A.SHAREspec(ids, cvtDoc doc)
		  | PT.SHARETYPEspec(ids, doc) => A.SHARETYPEspec(ids, cvtDoc doc)
		  | PT.TYspec{eq, specs} => let
		      fun cvt {params, id, def, doc} = {
			      params = params, id = id,
			      def = Option.map cvtTyp def,
			      doc = cvtDoc doc
			    }
		      in
			A.TYspec{eq = eq, specs = List.map cvt specs}
		      end
		  | PT.DTspec specs => let
		      fun cvt {params, id, cons, doc} = {
			      params = params, id = id,
			      cons = List.map cvtConSpec cons,
			      doc = cvtDoc doc
			    }
		      in
			A.DTspec(List.map cvt specs)
		      end
		  | PT.DTDEFspec{doc, id, def} =>
		      A.DTDEFspec{doc = cvtDoc doc, id = id, def = def}
		  | PT.EXNspec specs => A.EXNspec(List.map cvtConSpec specs)
		  | PT.VALspec specs => let
		      fun cvt (doc, id, ty) = (cvtDoc doc, id, cvtTyp ty)
		      in
			A.VALspec(List.map cvt specs)
		      end
		(* end case *))
	  and cvtConSpec (PT.CONspec(id, optTy, doc)) =
		A.CONspec(id, Option.map cvtTyp optTy, cvtDoc doc)
	  and cvtWhereSpecs whrSpecs = let
		fun cvt (PT.WHEREty{params, id, def}) =
		      A.WHEREty{params = params, id = id, def = cvtTyp def}
		  | cvt (PT.WHEREstr{id, def}) =
		      A.WHEREstr{id = id, def = def}
		in
		  List.map cvt whrSpecs
		end
	  val PT.FILE(doc, topDcls) = file
	  in
	    A.FILE(cvtDoc doc, List.map cvtTop topDcls)
	  end

  end
