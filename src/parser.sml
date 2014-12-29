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
    structure SMLParser = SMLDocParseFn(SMLDocLex)

  (* error function for lexers *)
    fun lexErr errStrm (pos, msg) = Error.errorAt(errStrm, (pos, pos), msg)

  (* error function for parsers *)
    val parseErr = Error.parseError tokToString

    fun parseFile inFile = if OS.FileSys.access(inFile, [OS.FileSys.A_READ])
	  then let
	    val inS = TextIO.openIn inFile
	    val errStrm = Error.mkErrStream inFile
	    fun get () = TextIO.input inS
	    val lexer = SMLDocLex.lex (Error.sourceMap errStrm) (lexErr errStrm)
	    in
	      case SMLDocParser.parse lexer (SMLDocLex.streamify get)
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
	  fun cvtDoc NONE = NONE
	    | cvtDoc (SOME(span, isPre, toks)) =
		SOME(ParseMarkup.parse (?, isPre, toks))
		  handle ParseMarup.Error(lnum, msg) => ??
	  fun cvtTyp (PT.VARty id) = A.VARty id
	    | cvtTyp (PT.CONty(tys, id)) = A.CONty(List.map cvtTyp tys, id)
	    | cvtTyp (PT.FUNty(ty1, ty2)) = A.FUNty(cvtTyp ty1, cvtTy ty2)
	    | cvtTyp (PT.RECORDty flds) = let
		fun cvtFld (id, ty, doc) = (id, cvtTyp ty, cvtDoc doc)
		in
		  A.RECORDty(List.map cvtFld flds)
		end
	    | cvtTyp (PT.PARENty ty) = A.PARENty(cvtTy ty)
	  fun cvtTop (PT.SIGdec(id, sigExp, whrSpecs)) =
		A.SIGdec(id, cvtSigExp sigExp, cvtWhereSpecs whrSpecs)
	  and cvtSigExp (PT.IDsigexp id) = A.IDsigexp id
	    | cvtSigExp (PT.SIGsigexp sigbody) = A.SIGsigexp(cvtSigBody sigbody)
	  and cvtSigBody (PT.SIGbody specs) =
		A.SIGbody(List.map (fn (doc, spec) => (cvtDoc doc, cvtSpec spec)) specs)
	  and cvtSpec spec = (case spec
		 of PT.INCLspec specs =>
		      A.INCLspec(List.map (fn (id, whrSpecs) => (id, cvtWhereSpecs whrSpecs)) specs)
		  | PT.STRspec(id1, id2, whrSpecs) =>
		      A.STRspec(id1, id2, cvtWhereSpecs whrSpecs)
		  | PT.STRSIGspec(id, sigBody) => A.STRSIGspec(id, cvtSigBody sigBody)
		  | PT.SHARINGspec specs => A.SHARINGspec(List.map cvtSharingSpec specs)
		  | PT.EXNspec specs => let
		      fun cvt (doc, id, optTy) = (cvtDoc doc, id, Option.map cvtTyp optTy)
		      in
		        A.EXNspec(List.map cvt specs)
		      end
		  | PT.TYspec specs => let
		      fun cvt {doc, eq, params, id, def} = {
			      doc = cvtDoc doc,
			      eq = eq, params = params, id = id,
			      def = Option.map cvtTyp def
			    }
		      in
			A.TYspec(List.map cvt specs)
		      end
		  | PT.DTspec specs => let
		      fun cvtCon (PT.CONspec(id, optTy, doc)) =
			    A.CONspec(id, Option.map cvtTyp optTy, cvtDoc doc)
		      fun cvt {doc, params, id, cons} = {
			      doc = cvtDoc doc,
			      params = params, id = id,
			      cons = List.map cvtCon cons
			    }
		      in
			A.DTspec(List.map cvt specs)
		      end
		  | PT.DTDEFspec specs => let
		      fun cvt {doc, id, def} = {doc = cvtDoc doc, id = id, def = def}
		      in
			A.DTDEFspec(List.map cvt specs)
		      end
		  | PT.VALspec specs => let
		      fun cvt (doc, id, ty) = (cvtDoc doc, id, cvtTyp ty)
		      in
			A.VALspec(List.map cvt specs)
		      end
		(* end case *))
	  and cvtSharingSpec (PT.STRshare ids) = A.STRshare ids
	    | cvtSharingSpec (PT.TYshare ids) = A.TYshare ids
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
