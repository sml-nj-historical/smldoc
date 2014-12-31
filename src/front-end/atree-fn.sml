(* atree-fn.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * SML Abstract Syntax trees annotated with documentation comments.
 *)

structure Id =
  struct

  (* identifiers; we annotate them with a property list to allow
   * anchors and hrefs to be associated with them.
   *)
    datatype id = ID of {
	name : Atom.atom,
	props : PropList.holder
      }

    fun new name = ID{name = name, props = PropList.newHolder()}

    val bogus = new(Atom.atom "*bogus*")

  end

functor ATreeFn (type doc_comment) =
  struct

  (* documentation comments *)
    type doc = doc_comment list

    type id = Id.id

    type tyvar = string

  (* SML types *)
    datatype typ
      = VARty of tyvar
      | CONty of (typ list * id)
      | FUNty of (typ * typ)
      | TUPLEty of typ list
      | RECORDty of (id * typ * doc) list
      | PARENty of typ

    datatype file
      = FILE of (doc * topdec list)

    and topdec
      = SIGdec of (id * sigexp * where_spec list * doc) list

    and sigexp
      = IDsigexp of id
      | SIGsigexp of spec list

  (* SML specifications *)
    and spec
      = INCLspec of (id * doc) list
      | INCLWHEREspec of (id * where_spec list * doc)
      | STRspec of (id * sigexp * where_spec list * doc) list
      | SHAREspec of (id list * doc)
      | SHARETYPEspec of (id list * doc)
      | TYspec of {
	    eq : bool,
	    specs : {
		params : tyvar list,
		id : id,
		def : typ option,
		doc : doc
	      } list
	  }
      | DTspec of {		(* a list of mutually recursive datatype specs *)
	    params : tyvar list,
	    id : id,
	    cons : con_spec list,
	    doc : doc
	  } list
      | DTDEFspec of {		(* a definition of a datatype *)
	    id : id,
	    def : id,
	    doc : doc
	  }
      | EXNspec of con_spec list
      | VALspec of (id * typ * doc) list

    and con_spec
      = CONspec of id * typ option * doc

    and where_spec
      = WHEREty of {
	    params : tyvar list,
	    id : id,
	    def : typ
	  }
      | WHEREstr of {
	    id : id,
	    def : id
	  }

  end
