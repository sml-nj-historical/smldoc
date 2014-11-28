(* ppspec-sig.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PP_SPEC =
  sig

    structure PPStrm : PP_STREAM
    structure Style : ML_STYLE

    type device = PPStrm.device
    type stream = PPStrm.stream
    type token = PPStrm.token
    type context = Style.context

    val ppSignature : (stream * string) -> unit
    val ppStructure : (context * stream)
	  -> (string * bool * string option * ATree.where_ty list)
	    -> unit
    val ppFunctor : (context * stream)
	  -> (string * string option * bool * string option * ATree.where_ty list)
	    -> unit

    val ppSpecs : (context * stream * bool)
	  -> (ATree.spec * ATree.M.markup list) list -> unit
    val ppSpec : (context * stream * bool) -> ATree.spec -> unit
    val ppInclSpec : (context * stream * bool)
	  -> (ATree.tagged_id * ATree.where_ty list) list -> unit
    val ppStrSpec : (context * stream * bool)
	  -> (string * ATree.tagged_id * ATree.where_ty list) -> unit
    val ppStrSigSpec : (context * stream)
	  -> (string * (ATree.spec * ATree.M.markup list) list)
	    -> unit
    val ppWhereTys : (context * stream) -> ATree.where_ty list -> unit
    val ppSharingSpec : (context * stream) -> ATree.sharing_spec list -> unit
    val ppTySpec : (context * stream) -> {
	    eq : bool,
	    params : string option,
	    id : string,
	    def : ATree.typ option
	  } list -> unit
    val ppDTSpec : (context * stream) -> {
	    compact : bool, params : string option,
	    id : string,
	    cons : (string * ATree.typ option * ATree.M.markup list) list
	  } list -> unit
    val ppDTLHS : (context * stream) -> (string option * string) -> unit
    val ppConsSpec : (context * stream) -> {
	    isFirst : bool,
	    id : string,
	    ty : ATree.typ option
	  } -> unit
    val ppDTDefSpec : (context * stream) -> {
	    id : string,
	    def : ATree.tagged_id
	  } list -> unit
    val ppExnSpec : (context * stream)
	  -> (string * ATree.ATree.typ option) list -> unit
    val ppValSpec : (context * stream * bool)
	  -> (string * ATree.typ * MLDocMarkup.markup list) list -> unit

    val ppType : (context * stream) -> ATree.typ -> unit

 (* some utility print routines *)
    val ppTyParams : (stream * string option) -> unit
    val ppKW : (stream * string) -> unit
    val ppPunct : (stream * string) -> unit
    val ppPageBreak : (stream * bool) -> unit

  end
