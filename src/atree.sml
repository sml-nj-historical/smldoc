(* atree.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * SML Abstract Syntax trees annotated with documentation comments.
 *)

structure ATree =
  struct

  (* raw documentation comment *)
    type doc = MarkupTokens.token list

  (* identifiers *)
    type id = string

  (* SML types *)
    datatype typ
      = VARty of string
      | CONty of (typ list * id)
      | FUNty of (typ * typ)
      | TUPLEty of typ list
      | RECORDty of (id * typ * doc) list
      | PARENty of typ

    datatype file
      = FILE of (doc * topdec list)

    and topdec
      = SIGdec of id * sigexp * where_spec list

    and sigexp
      = IDsigexp of id
      | SIGsigexp of sigbody

    and sigbody = SIGbody of (doc * spec) list

  (* SML specifications *)
    and spec
      = INCLspec of (id * where_spec list) list
      | STRspec of (id * id * where_spec list)
      | STRSIGspec of (id * sigbody)
      | SHARINGspec of sharing_spec list
      | EXNspec of (doc * id * typ option) list
      | TYspec of {
	    doc : doc,
	    eq : bool,
	    params : id list,
	    id : id,
	    def : typ option
	  } list
      | DTspec of {		(* a list of mutually recursive datatype specs *)
	    doc : doc,
	    params : id list,
	    id : id,
	    cons : (id * typ option * doc) list
	  } list
      | DTDEFspec of {		(* a definition of a datatype *)
	    doc : doc,
	    id : string,
	    def : id
	  } list
      | VALspec of (doc * id * typ) list

    and con_spec
      = CONspec of string * typ option * doc

    and sharing_spec
      = STRshare of string list
      | TYshare of string list

    and where_spec
      = WHEREty of {
	    params : string option,
	    id : string,
	    def : typ
	  }
      | WHEREstr of {
	    id : id,
	    def : id
	  }

  end
