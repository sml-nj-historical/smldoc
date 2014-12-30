(* error.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Infrastructure for error reporting.
 *)

structure Error :> sig

  (* logical positions in the input stream *)
    type pos = AntlrStreamPos.pos
    type span = AntlrStreamPos.span

    type err_stream

  (* make an error stream from a filename. *)
    val mkErrStream : string -> err_stream

    val anyErrors : err_stream -> bool
    val sourceFile : err_stream -> string
    val sourceMap : err_stream -> AntlrStreamPos.sourcemap

  (* add error messages to the error stream *)
    val error : err_stream * string list -> unit
    val errorAt : err_stream * span * string list -> unit
    val errorAtLine : err_stream * int * string list -> unit

  (* add warning messages to the error stream *)
    val warning : err_stream * string list -> unit
    val warningAt : err_stream * span * string list -> unit

  (* add an ml-antlr parse error to the error stream *)
    val parseError : ('tok -> string)
	  -> err_stream
	    -> (pos * 'tok AntlrRepair.repair_action)
	      -> unit

  (* print the errors to an output stream *)
    val report : TextIO.outstream * err_stream -> unit

  (* a term marked with a source-map span *)
    type 'a mark = {span : span, tree : 'a}

  end = struct

    structure SP = AntlrStreamPos
    structure Repair = AntlrRepair
    structure F = Format

    type pos = SP.pos
    type span = SP.span

    datatype severity = WARN | ERR

  (* source-code locations *)
    datatype location
      = UNKNOWN
      | LINE of int
      | LOC of {l1 : int, c1 : int, l2 : int, c2 : int}

    type error = {
	kind : severity,
	loc : location,
	msg : string
      }

  (* an error stream collects the errors and warnings generated for
   * a compilation unit.
   *)
    datatype err_stream = ES of {
	srcFile		: string,
	sm		: SP.sourcemap,	(* the source map for mapping positions to *)
					(* source-file positions *)
	errors		: error list ref,
	numErrors	: int ref,
	numWarnings	: int ref
      }

  (* make an error stream. *)
    fun mkErrStream filename = ES{
	    srcFile = filename,
	    sm = SP.mkSourcemap' filename,
	    errors = ref [],
	    numErrors = ref 0,
	    numWarnings = ref 0
	  }

    fun anyErrors (ES{numErrors, ...}) = (!numErrors > 0)
    fun sourceFile (ES{srcFile, ...}) = srcFile
    fun sourceMap (ES{sm, ...}) = sm

    fun addErr (ES{errors, numErrors, ...}, loc, msg) = (
	  numErrors := !numErrors + 1;
	  errors := {kind=ERR, loc=loc, msg=String.concat msg} :: !errors)
	  
    fun addWarn (ES{errors, numWarnings, ...}, loc, msg) = (
	  numWarnings := !numWarnings + 1;
	  errors := {kind=WARN, loc=loc, msg=String.concat msg} :: !errors)

  (* make a location from a line number *)
    fun line (ES{sm, ...}, n) = LINE n

  (* make a location from a span *)
    fun location (ES{sm, ...}, (p1, p2) : span) =
	  if (p1 = p2)
	    then let
	      val {lineNo, colNo, ...} = SP.sourceLoc sm p1
	      in
		LOC{l1=lineNo, c1=colNo, l2=lineNo, c2=colNo}
	      end
	    else let
	      val {lineNo=l1, colNo=c1, ...} = SP.sourceLoc sm p1
	      val {lineNo=l2, colNo=c2, ...} = SP.sourceLoc sm p2
	      in
		LOC{l1=l1, c1=c1, l2=l2, c2=c2}
	      end

  (* make a location from a position *)
    fun position (ES{sm, ...}, p : pos) = let
	  val {lineNo, colNo, ...} = SP.sourceLoc sm p
	  in
	    LOC{l1=lineNo, c1=colNo, l2=lineNo, c2=colNo}
	  end

    fun parseError tok2str es (pos, repair) = let
	  val toksToStr = (String.concatWith " ") o (List.map tok2str)
	  val msg = (case repair
		 of Repair.Insert toks => ["syntax error; try inserting \"", toksToStr toks, "\""]
		  | Repair.Delete toks => ["syntax error; try deleting \"", toksToStr toks, "\""]
		  | Repair.Subst{old, new} => [
			"syntax error; try substituting \"", toksToStr new, "\" for \"",
			toksToStr old, "\""
		      ]
		  | Repair.FailureAt tok => ["syntax error at ", tok2str tok]
		(* end case *))
	  in
	    addErr (es, position (es, pos), msg)
	  end

  (* add error messages to the error stream *)
    fun error (es, msg) = addErr (es, UNKNOWN, msg)
    fun errorAt (es, span, msg) = addErr (es, location(es, span), msg)
    fun errorAtLine (es, lnum, msg) = addErr (es, line(es, lnum), msg)

  (* add warning messages to the error stream *)
    fun warning (es, msg) = addWarn (es, UNKNOWN, msg)
    fun warningAt (es, span, msg) = addWarn (es, location(es, span), msg)

  (* sort a list of errors by position in the source file *)
    val sort = let
	  fun gt (UNKNOWN, UNKNOWN) = false
	    | gt (UNKNOWN, _) = true
	    | gt (_, UNKNOWN) = false
	    | gt (LINE l1, LINE l2) = (l1 > l2)
	    | gt (LINE n, LOC{l1, ...}) = (n > l1)
	    | gt (LOC{l1, ...}, LINE n) = (l1 > n)
	    | gt (LOC loc1, LOC loc2) = (case Int.compare(#l1 loc1, #l1 loc2)
		 of LESS => false
		  | EQUAL => (case Int.compare (#c1 loc1, #c1 loc2)
		       of LESS => false
		        | EQUAL => (case Int.compare (#l2 loc1, #l2 loc2)
			     of LESS => false
			      | EQUAL => (#c2 loc1 > #c2 loc2)
			      | GREATER => true
			    (* end case *))
			| GREATER => true
		      (* end case *))
		  | GREATER => true
		(* end case *))
	  fun cmp (e1 : error, e2 : error) = gt(#loc e1, #loc e2)
	  in
	    ListMergeSort.sort cmp
	  end

    fun printError (outStrm, ES{srcFile, sm, ...}) = let
	  fun pr {kind, loc, msg} = let
		val kind = (case kind of ERR => "Error" | Warn => "Warning")
		val loc = (case loc
		       of UNKNOWN => concat["[", srcFile, "] "]
			| LINE n => F.format "[%s:%d] " [F.STR srcFile, F.INT n]
			| LOC{l1, c1, l2, c2} =>
			    if (l1 = l2)
			      then if (c1 = c2)
				then F.format "[%s:%d.%d] " [F.STR srcFile, F.INT l1, F.INT c1]
				else F.format "[%s:%d.%d-%d] " [
				    F.STR srcFile, F.INT l1, F.INT c1, F.INT c2
				  ]
			      else F.format "[%s:%d.%d-%d.%d] " [
				  F.STR srcFile, F.INT l1, F.INT c1, F.INT l2, F.INT c2
				]
		      (* end case *))
		in
		  TextIO.output (outStrm, String.concat [loc, kind, ": ", msg, "\n"])
		end
	  in
	    pr
	  end

    fun report (outStrm, es as ES{errors, ...}) =
	  List.app (printError (outStrm, es)) (sort (!errors))

  (* a term marked with a source-map span *)
    type 'a mark = {span : span, tree : 'a}

  end
