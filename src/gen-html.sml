(* gen-html.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure GenHTML =
  struct

    structure H = HTML4
    structure Dev = HTML4Dev

  (* HTML generation infrastructure *)
    structure Style =
      struct

	local
	  structure M = MLDocMarkup
	  structure E = MLDocElem
	in

	type style = Dev.style
	datatype token = TOK of {
	    sty : style,
	    id : string
	  }

	type context = {inSpec : bool, ctx : HTMLContext.context}

	datatype xref_kind = datatype HRefs.xref_kind

	val kwStyle = Dev.styleClass "kw"
	val punctStyle = Dev.styleClass "p"
	val sigidStyle = Dev.styleClass "sigid"
	val stridStyle = Dev.styleClass "strid"
	val tyidStyle = Dev.styleClass "tyid"
	val tyvarStyle = Dev.styleClass "tyvar"
	val conidStyle = Dev.styleClass "conid"
	val exnidStyle = Dev.styleClass "exnid"
	val validStyle = Dev.styleClass "valid"

	fun extendContext ({inSpec, ctx}, id) =
	      {inSpec = inSpec, ctx = HTMLContext.withSubstr (ctx, id)}

      (* identifiers that are possible cross references *)
	fun idToToken {inSpec, ctx} = let
	      val mkURL = HRefs.xrefURL ctx
	      fun toToken (MLSpec.ID id) = TOK{id = id, sty = idStyle}
		| toToken (MLSpec.TAG(M.ELEM{elem, body=[M.DATA id], ...})) = TOK{
    (* need to translate text here? *)
		      sty = case mkURL{xref = elem, id = id}
			   of (SOME tag) => HTMLDev.link tag
			    | NONE => idStyle
			  (* end case *),
		      id = id
		    }
	      in
		toToken
	      end

    (* NOTE: we also need to put a specification anchor here! *)
      (* identifiers that are forward links to descriptions *)
	fun descRef ({inSpec = true, ctx}, kind, id) = TOK{
		sty = HTMLDev.linkAnchor{
		    href = HRefs.descURL ctx {isRef = true, kind = kind, id = id},
		    name = HRefs.specURL ctx {isRef = false, kind = kind, id = id}
		  },
		id = id
	      }
	  | descRef (_, _, id) = TOK{sty=idStyle, id=id}

	fun mkToken (sty, s) = TOK{id = s, sty = sty}

	end (* local *)
      end

    structure Token : PP_TOKEN =
      struct
	type style = Style.style
	type token = Style.token

	fun string (Style.TOK{id, ...}) = id

	fun style (Style.TOK{sty, ...}) = sty

    (* if the text has been translated, then this will be wrong! *)
	fun size (Style.TOK{id, ...}) = String.size id

      end

    structure HTMLPPStrm = PPStreamFn (
      structure Token = Token
      structure Device = HTML4Dev)

    structure HTMLPPSpec = PPSpecFn(
      structure PPStrm = HTMLPPStrm
      structure Style = Style
      fun specBreak _ _ = ())

  end
