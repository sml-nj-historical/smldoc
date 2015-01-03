(* main.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : sig

    val main : string * string list -> OS.Process.status

  end = struct

    fun doFile f = let
	  val (dir, stem) = let
		fun split base = let
		      val {dir, file} = OS.Path.splitDirFile base
		      in
			(dir, file)
		      end
		in
		(* Strip off the file extension, which we expect to be one
		 * of "cml", "pml", "fun", "sig", or "sml".  We ignore
		 * other extensions.
		 *)
		  case OS.Path.splitBaseExt f
		   of {base, ext=SOME "cml"} => split base	(* Concurrent ML *)
		    | {base, ext=SOME "pml"} => split base	(* Parallel ML *)
		    | {base, ext=SOME "fun"} => split base	(* Standard ML *)
		    | {base, ext=SOME "sig"} => split base	(* Standard ML *)
		    | {base, ext=SOME "sml"} => split base	(* Standard ML *)
		    | _ => split f
		  (* end case *)
		end
	  in
	    case Parser.parseFile f
	     of SOME tree => (case Convert.toMarkup tree
		   of [mTree] => () (* FIXME *)
		    | mTrees => () (* FIXME *)
		  (* end case *))
	      | NONE => ()
	    (* end case *)
	  end (* doFile *)

    fun main (cmdName, args) = (List.app doFile args; OS.Process.success)
	  handle ex => 
           err (concat [
                "uncaught exception ", General.exnName exn,
                " [", General.exnMessage exn, "]\n"
              ]);
            List.app (fn s => err (concat ["  raised at ", s, "\n"]))
              (SMLofNJ.exnHistory exn);
            OS.Process.failure)

  end
