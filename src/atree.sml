(* atree.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* Annotated tree before parsing markup *)
structure ParseTree = ATreeFn (
    type doc_comment = AntlrStreamPos.span * bool * MarkupTokens.token list);

(* Annotated tree after parsing markup *)
structure ATree = ATreeFn (
    type doc_comment = Markup.comment);
