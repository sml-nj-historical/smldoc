(* html-gen.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure HTMLGen : sig

    val generate : {
	    dict : Index.index,
	    outDir : string,
	    
	  } -> unit

  end = struct

    structure DC = DocCom
    structure H = HTML4
    structure 

  (* generate 

  end



