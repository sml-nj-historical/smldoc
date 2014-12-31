(* convert.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Convert the ATree representation into markup.
 *)

structure Convert : sig

    val convert : (string * ATree.file) -> Markup.file list

  end = struct

    structure A = ATree
    structure DC = DocCom
    structure M = Markup

  (* the generic contents of a documentation comment *)
    type info = {
	desc : DC.text_block list, 		(**< The description text. *)
	authors : string list,			(**< The list of authors in \@author tags. *)
	version : string option, 		(**< The string in the \@version tag. *)
	date : {year : int, month : int, day : int} option,
						(**< The date in a \@date tag *)
	copy : DC.text option,			(**< copyright notice in \@copy tag *)
	sees : (DC.see_ref * DC.text) list,	(**< The list of \@see tags. *)
	since : string option,			(**< The string in the \@since tag. *)
	befores : (string * DC.text) list,	(**< the version number and text in \@before tag *)
	deprecated : DC.text option,		(**< The text of the \@deprecated tag. *)
	instances : DC.id_desc list,		(**< The modules in the \@instance tags. *)
	params : DC.id_desc list,		(**< The list of parameter descriptions. *)
	raises : DC.id_desc list,		(**< The list of raised exceptions. *)
	return : DC.text option			(**< The description text of the return value. *)
      }

    fun dcToInfo (coms : DC.comment list) : info = let
	  val desc = ref []
	  val authors = ref []
	  val version = ref NONE
	  val date = ref NONE
	  val copy = ref NONE
	  val sees = ref []
	  val since = ref NONE
	  val befores = ref []
	  val deprecated = ref NONE
	  val instances = ref []
	  val params = ref []
	  val raises = ref []
	  val return = ref NONE
	  fun doCom (comment : DC.comment) = let
		fun addOpt (tag, ref(SOME _), _) =
		      print(concat["ignoring duplicate @", tag, " tags\n"])
		  | addOpt (_, optRef, value) = optRef := SOME value
		fun addItem (listRef, value) = listRef := value :: !listRef
		fun doTag (DC.TAG_author arg) = addItem (authors, arg)
		  | doTag (DC.TAG_before arg) = addItem (befores, arg)
		  | doTag (DC.TAG_copy arg) = addOpt ("copy", copy, arg)
		  | doTag (DC.TAG_date arg) = addOpt ("date", date, arg)
		  | doTag (DC.TAG_deprecated arg) = addOpt ("deprecated", deprecated, arg)
		  | doTag (DC.TAG_instance arg) = addItem (instances, arg)
		  | doTag (DC.TAG_param arg) = addItem (params, arg)
		  | doTag (DC.TAG_raise arg) = addItem (raises, arg)
		  | doTag (DC.TAG_return arg) = addOpt ("return", return, arg)
		  | doTag (DC.TAG_see arg) = addItem (sees, arg)
		  | doTag (DC.TAG_since arg) = addOpt ("since", since, arg)
		  | doTag (DC.TAG_version arg) = addOpt ("version", version, arg)
		in
		  desc := List.revAppend(#desc comment, !desc);
		  List.app doTag (#tags comment)
		end
	  in
	    List.app doCom coms;
	    { desc	= List.rev (!desc)
	    , authors	= List.rev (!authors)
	    , version	= !version
	    , date	= !date
	    , copy	= !copy
	    , sees	= List.rev (!sees)
	    , since	= !since
	    , befores	= List.rev (!befores)
	    , deprecated = !deprecated
	    , instances	= List.rev (!instances)
	    , params	= List.rev (!params)
	    , raises	= List.rev (!raises)
	    , return	= !return
	    }
	  end

    fun notEmpty sel (info : info) = (case (sel info) of [] => false | _ => true)
    fun notNone sel (info : info) = (case (sel info) of NONE => false | _ => true)

  (* convert documentation comment to val-spec info *)
    fun toValInfo (id : Atom.atom, coms : DC.comment list) : M.val_info = let
	  val info = dcToInfo coms
	  fun ignoring tag = print(concat[
		  "ignoring '@", tag, "' in comment for val '", Atom.toString id, "'\n"
		])
	  in
	  (* warning messages for @-tags that don't belong in a val description *)
	    if notEmpty #authors info then ignoring "author" else ();
	    if notNone #version info then ignoring "version" else ();
	    if notNone #date info then ignoring "date" else ();
	    if notNone #copy info then ignoring "copy" else ();
	    if notEmpty #instances info then ignoring "instance" else ();
	    { desc = #desc info
	    , sees = #sees info
	    , since = #since info
	    , befores = #befores info
	    , deprecated = #deprecated info
	    , params = #params info
	    , raises = #raises info
	    , return = #return info
	    }
	  end

    fun convert (srcFile, content) = []

  end



