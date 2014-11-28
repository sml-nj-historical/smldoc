(* atree.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * SML Abstract Syntax trees annotated with documentation comments.
 *)

structure ATree =
  struct

  (* documentation comment *)
    type doc = string option

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

    datatype sigbody = (doc * spec) list

  (* SML specifications *)
    and spec
      = INCLspec of (id * where_spec list) list
      | STRspec of (id * id * where_spec list)
      | STRSIGspec of (id * sigbody)
      | SHARINGspec of sharing_spec list
      | EXNspec of (id * typ option) list
      | TYspec of {
	    eq : bool,
	    params : id list,
	    id : id,
	    def : typ option
	  } list
      | DTspec of {		(* a list of mutually recursive datatype specs *)
	    params : id list,
	    id : id,
	    cons : (id * typ option * M.markup list) list
	  } list
      | DTDEFspec of {		(* a definition of a datatype *)
	    id : string,
	    def : id
	  } list
      | VALspec of (id * typ * doc) list

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

