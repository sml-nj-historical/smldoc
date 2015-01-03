(* pp-code-fun-fn.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PP_CODE_DEV =
  sig

    structure PP : PP_STREAM

  (* begin/end an inline span of code *)
    val beginInline : PP.stream -> unit
    val endInline : PP.stream -> unit

  (* begin/end a block of code *)
    val beginBlock : PP.stream -> unit
    val endBlock : PP.stream -> unit

  (* pretty printing reserved identifiers, etc. *)
    val kw : string -> PP.stream -> unit
    val sym : string -> PP.stream -> unit
    val punct : string -> PP.stream -> unit

  (* pretty printing identifiers *)
    val id : PP.stream * Id.id -> unit

  (* pretty printing literals *)
    val strLit : PP.stream * string -> unit
    val chrLit : PP.stream * string -> unit
    val intLit : PP.stream * string -> unit
    val wordLit : PP.stream * string -> unit
    val realLit : PP.stream * string -> unit

  (* pretty printing comments *)
    val comment : PP.stream * string -> unit

  end

functor PPCodeFn (CodeDev : PP_CODE_DEV) : sig

    structure PP : PP_STREAM
      where type device = CodeDev.PP.device
      where type stream = CodeDev.PP.stream
      where type token = CodeDev.PP.token
      where type style = CodeDev.PP.style
      where type indent = CodeDev.PP.indent

    val ppInline : PP.stream * DocCom.code_elem list -> unit

    val ppBlock : PP.stream * DocCom.code_elem list -> unit

  (* TODO: pretty printing type specs *)
  end = struct

    structure Dev = CodeDev
    stucture PP = Dec.PP

    fun ppCodeElems isInline (ppS, code) = let
	  fun pp elem = (case elem
		 of KW kw => Dev.kw (Atom.toString kw) ppS
		  | PUNCT s => Dev.punct (Atom.toString s) ppS
		  | ID id => Dev.id (ppS, id)
		  | STR s => Dev.strLit (ppS, s)
		  | CHR s => Dev.chrLit (ppS, s)
		  | INT s => Dev.intLit (ppS, s)
		  | WORD s => Dev.wordLit (ppS, s)
		  | REAL s => Dev.realLit (ppS, s)
		  | COM s => Dev.comment (ppS, s)
		  | WS ws => if isInline
		      then PP.space ppS 1
		      else PP.space ppS (size ws)
		  | EOL => if isInline
		      then PP.space ppS 1
		      else PP.newline ppS
		(* end case *))
	  in
	    List.app pp code
	  end

    fun ppInline (ppS, code) = (
	  Dev.beginInline ppS;
	  ppCodeElems (true, code;
	  Dev.endInline ppS)

    fun ppBlock (ppS, code) = (
	  Dev.beginInline ppS;
	  ppCodeElems (false, code;
	  Dev.endInline ppS)

  end



