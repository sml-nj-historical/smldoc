(* ml-spec-sig.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ML_STYLE =
  sig
    type token
    type style
    type context

    datatype xref_kind
      = SigRef | StrRef | FctRef | ShareRef | TyRef | ExnRef | ConRef | ValRef

  (* these styles correspond to the code highlighting styles in smldoc.css *)
    val kwStyle : style
    val punctStyle : style
    val sigidStyle : style
    val stridStyle : style
    val tyidStyle : style
    val tyvarStyle : style
    val conidStyle : style
    val exnidStyle : style
    val validStyle : style

  (* extend the context for the body of a nested substructure *)
    val extendContext : (context * string) -> context

  (* identifiers that are possible cross references *)
    val idToToken : context -> MLSpec.tagged_id -> token

  (* identifiers that are forward links to descriptions; this function is used
   * to wrap identifiers in specifications.
   *)
    val descRef : (context * xref_kind * string) -> token

  (* make a token with the given style (to support character escapes) *)
    val mkToken : (style * string) -> token

  end
