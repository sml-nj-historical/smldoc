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

  (* color text (using FONT element) *)
    val color : string -> style

  (* hyper-text links and anchors *)
    val link : string -> style
    val anchor : string -> style
    val linkAnchor : {name : string, href : string} -> style

    val openDev : {wid : int, textWid : int option} -> device
    val done : device -> HTML4.text

  end = struct

    datatype style
      = TT | I | B | U | STRIKE | EM | STRONG
      | SPAN of HTML4.attribute list
      | A of HTML4.attribute list
      | STYS of style list

    datatype device = DEV of {
	lineWid : int,
	textWid : int option,
	emphStk	: (HTML4.text list * style) list ref,
	txt : HTML4.text list ref
      }

  (* return the current emphasis *)
    fun curEmph (DEV{emphStk, ...}) = (case !emphStk
	   of [] => STYS[]
	    | ((_, em)::r) => em
	  (* end case *))

  (* add PCDATA to the text list *)
    fun pcdata (DEV{txt, ...}, s) = txt := HTML4.PCDATA s :: !txt

  (* replace the sequence of PCDATA elements at the head of the
   * txt list with its concatenation.
   *)
    fun concatTxt (DEV{txt, ...}) = let
	  fun f ([], []) = []
	    | f (HTML4.PCDATA s :: r, l) = f (r, s::l)
	    | f (r, l) = HTML4.PCDATA(String.concat l) :: r
	  in
	    f (!txt, [])
	  end

  (* are two styles the same? *)
    fun sameStyle (s1 : style, s2) = (s1 = s2)

    fun wrapStyle (sty, [], tl') = tl'
      | wrapStyle (STYS[], tl, tl') = List.revAppend(tl, tl')
      | wrapStyle (sty, tl, tl') = let
	  val tl = List.rev tl
	  fun wrap (TT, t) = HTML4.TT([], t)
	    | wrap (I, t) = HTML4.I([], t)
	    | wrap (B, t) = HTML4.B([], t)
	    | wrap (U, t) = HTML4.U([], t)
	    | wrap (STRIKE, t) = HTML4.STRIKE([], t)
	    | wrap (EM, t) = HTML4.EM([], t)
	    | wrap (STRONG, t) = HTML4.STRONG([], t)
	    | wrap (SPAN attrs, t) = HTML4.SPAN(attrs, t)
	    | wrap (A{name, href}, t) = HTML4.A(attrs, t)
	    | wrap (STYS(sty::stys), t) =
		wrap (sty, List.foldr (fn (sty, t) => [wrap(sty, t)]) t stys)
	    | wrap (STYS[], _) = raise Fail "unexpected empty STYS list"
	  in
	    wrap(sty, t) :: tl'
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

  (* output some number of spaces to the device *)
    fun space (dev, n) =
	  pcdata(dev, concat(List.tabulate (n, fn _ => "&nbsp;")))

  (* output a new-line to the device *)
    fun newline (dev as DEV{txt, ...}) =
	  txt := HTML4.BR{clear=NONE} :: (concatTxt dev)

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
    fun span cls = SPAN[HTML4Attrs.class cls]
    fun link s = A[HTML4Attrs.href, s]
    fun anchor s = A[HTML4Attrs.name, s]
    fun linkAnchor {name, href} = A[HTML4Attrs.href href, HTML4Attrs.name name]

    fun openDev {wid, textWid} = DEV{
	    txt = ref [],
	    emphStk = ref [],
	    lineWid = wid,
	    textWid = textWid
	  }

    fun done (dev as DEV{emphStk = ref [], txt, ...}) = (case (concatTxt dev)
	   of [t] => (txt := []; t)
	    | l => (txt := []; HTML4.TextList(List.rev l))
	  (* end case *))
      | done _ = raise Fail "device is not done yet"

  end; (* HTMLDev *)

