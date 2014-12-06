(* html4-dev.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A pretty printing device that uses HTML4 <span> elements to annotate
 * the test with style information.
 *)

structure HTML4Dev : sig

    include PP_DEVICE

  (* combine two styles into one *)
    val combineStyle : (style * style) -> style

  (* unstyled text *)
    val styleNONE : style

  (* a <span> element with the given class *)
    val styleClass : string -> style

  (* some standard HTML4 text styles *)
    val styleTT : style
    val styleI : style
    val styleB : style
    val styleU : style
    val styleSTRIKE : style
    val styleEM : style
    val styleSTRONG : style

  (* hyper-text links and anchors *)
    val link : string -> style
    val anchor : string -> style
    val linkAnchor : {name : string, href : string} -> style

    val openDev : {wid : int, textWid : int option, pre : bool} -> device
    val done : device -> HTML4.inline list

  end = struct

    datatype style
      = TT | I | B | U | STRIKE | EM | STRONG
      | SPAN of HTML4.attribute list
      | A of HTML4.attribute list
      | STYS of style list

    datatype device = DEV of {
	lineWid : int,
	textWid : int option,
	pre : bool,				(* if true, we don't need to convert #" " to "&nbsp;" *)
	emphStk	: (HTML4.inline list * style) list ref,
	txt : HTML4.inline list ref
      }

  (* return the current emphasis *)
    fun curEmph (DEV{emphStk, ...}) = (case !emphStk
	   of [] => STYS[]
	    | ((_, em)::r) => em
	  (* end case *))

  (* add an entity to the text list *)
    fun entity (DEV{txt, ...}, s) = txt := HTML4.CDATA[HTML4.ENTITY s] :: !txt

  (* add PCDATA to the text list *)
    fun pcdata (DEV{txt, ...}, s) = txt := HTML4.CDATA[HTML4.PCDATA s] :: !txt

  (* replace the sequence of CDATA elements at the head of the
   * txt list with its concatenation.  We also concatenate adjacent
   * PCDATA strings.
   *)
    fun concatTxt (DEV{txt, ...}) = let
	  fun gatherCData (HTML4.CDATA cdata :: r, cd) = gatherCData (r, cdata @ cd)
	    | gatherCData (txt, []) = txt
	    | gatherCData (txt, cd) = HTML4.CDATA(flatten cd) :: txt
	  and flatten cd = (case f cd
		 of ([], l) => l
		  | (strs, l) => HTML4.PCDATA(concat strs) :: l
		(* end case *))
	  and f [] = ([], [])
	    | f (HTML4.PCDATA s :: r) = let
		val (strs, l) = f r
		in
		  (s::strs, l)
		end
	    | f (cd :: r) = ([], cd :: flatten r)
	  in
	    gatherCData (!txt, [])
	  end

  (* are two styles the same? *)
    fun sameStyle (s1, s2) = (case (s1, s2)
	   of (TT, TT) => true
	    | (I, I) => true
	    | (B, B) => true
	    | (U, U) => true
	    | (STRIKE, STRIKE) => true
	    | (EM, EM) => true
	    | (STRONG, STRONG) => true
	    | (SPAN attrs1, SPAN attrs2) => true (* attrs will always be the same! *)
	    | (A attrs1, A attrs2) => let
		fun sameAttr ((a1, SOME(v1 : string)), (a2, SOME v2)) = Atom.same(a1, a2) andalso (v1 = v2)
		  | sameAttr ((a1, NONE), (a2, NONE)) = Atom.same(a1, a2)
		  | sameAttr _ = false
		in
		  ListPair.allEq sameAttr (attrs1, attrs2)
		end
	    | (STYS stys1, STYS stys2) =>
		ListPair.allEq sameStyle (stys1, stys2)
	    | _ => false
	  (* end case *))

    fun wrapStyle (sty, [], tl') = tl'
      | wrapStyle (STYS[], tl, tl') = List.revAppend(tl, tl')
      | wrapStyle (sty, tl, tl') = let
	  fun wrap (TT, t) = HTML4.TT([], t)
	    | wrap (I, t) = HTML4.I([], t)
	    | wrap (B, t) = HTML4.B([], t)
	    | wrap (U, t) = HTML4.U([], t)
	    | wrap (STRIKE, t) = HTML4.STRIKE([], t)
	    | wrap (EM, t) = HTML4.EM([], t)
	    | wrap (STRONG, t) = HTML4.STRONG([], t)
	    | wrap (SPAN attrs, t) = HTML4.SPAN(attrs, t)
	    | wrap (A attrs, t) = HTML4.A(attrs, t)
	    | wrap (STYS(sty::stys), t) =
		wrap (sty, List.foldr (fn (sty, t) => [wrap(sty, t)]) t stys)
	    | wrap (STYS[], _) = raise Fail "unexpected empty STYS list"
	  in
	    wrap(sty, List.rev tl) :: tl'
	  end

  (* push/pop a style from the devices style stack.  A pop on an
   * empty style stack is a nop.
   *)
    fun pushStyle (dev as DEV{emphStk, txt, ...}, sty) = (
	  emphStk := (concatTxt dev, sty) :: !emphStk;
	  txt := [])
    fun popStyle (DEV{emphStk as ref[], ...}) = ()
      | popStyle (dev as DEV{emphStk as ref ((tl, sty) :: r), txt, ...}) = (
	  txt := wrapStyle (sty, concatTxt dev, tl);
	  emphStk := r)
 
  (* the default style for the device (this is the current style,
   * if the style stack is empty).
   *)
    fun defaultStyle _ = STYS[]

  (* maximum printing depth (in terms of boxes) *)
    fun depth _ = NONE
  (* the width of the device *)
    fun lineWidth (DEV{lineWid, ...}) = SOME lineWid
  (* the suggested maximum width of text on a line *)
    fun textWidth (DEV{textWid, ...}) = textWid

    val nbsp = HTML4.ENTITY(Atom.atom "nbsp")

  (* output some number of spaces to the device *)
    fun space (DEV{pre, txt, ...}, n) = let
	  val sp = (case (pre, n)
		 of (true, 1) => [HTML4.PCDATA " "]
		  | (true, _) => [HTML4.PCDATA(String.implode(List.tabulate(n, fn _ => #" ")))]
		  | (false, 1) => [nbsp]
		  | (false, _) => List.tabulate(n, fn _ => nbsp)
		(* end case *))
	  in
	    txt := HTML4.CDATA sp :: !txt
	  end

    val nl = HTML4.CDATA[HTML4.PCDATA "\n"]

  (* output a new-line to the device *)
    fun newline (dev as DEV{pre, txt, ...}) =
	  if pre
	    then txt := nl :: !txt
	    else txt := nl :: HTML4.BR[] :: (concatTxt dev)

  (* output a string/character in the current style to the device *)
    val string = pcdata
    fun char (dev, c) = pcdata(dev, str c)

  (* flush is a nop for us *)
    fun flush _ = ()

    fun combineStyle (STYS l1, STYS l2) = STYS(l1 @ l2)
      | combineStyle (sty, STYS l) = STYS(sty::l)
      | combineStyle (sty1, sty2) = STYS[sty1, sty2]

    val styleNONE = STYS[]
    val styleTT = TT
    val styleI = I
    val styleB = B
    val styleU = U
    val styleSTRIKE = STRIKE
    val styleEM = EM
    val styleSTRONG = STRONG
    fun styleClass cls = SPAN[HTML4Attrs.class cls]
    fun link s = A[HTML4Attrs.href s]
    fun anchor s = A[HTML4Attrs.name s]
    fun linkAnchor {name, href} = A[HTML4Attrs.href href, HTML4Attrs.name name]

    fun openDev {wid, textWid, pre} = DEV{
	    txt = ref [],
	    emphStk = ref [],
	    pre = pre,
	    lineWid = wid,
	    textWid = textWid
	  }

    fun done (dev as DEV{emphStk = ref [], txt, ...}) = let
	  val l = concatTxt dev
	  in
	    txt := []; List.rev l
	  end
      | done _ = raise Fail "device is not done yet"

  end; (* HTMLDev *)

