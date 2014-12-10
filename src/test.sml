structure Test =
  struct

    local
      structure H = HTML4
    in

    structure TextToken =
      struct
	type token = string
	type style = HTML4Dev.style
	fun string t = t
	fun style t = HTML4Dev.styleTT
	fun size t = String.size t
      end

    structure PP = PPStreamFn (
      structure Token = TextToken
      structure Device = HTML4Dev)

    fun withPP (name, wid)  ppFn = let
	  val dev = HTML4Dev.openDev{wid = wid, textWid = NONE, pre = true}
	  val ppStrm = PP.openStream dev
	  val txt = (
		PP.openVBox ppStrm (PP.Abs 0);
		ppFn ppStrm;
		PP.newline ppStrm;
		PP.closeBox ppStrm;
		PP.closeStream ppStrm;
		HTML4Dev.done dev)
	  in
	    HTML4Print.prHTML {
		putc = fn c => TextIO.output1 (TextIO.stdOut, c),
		puts = fn s => TextIO.output (TextIO.stdOut, s)
	      } (H.HTML{
		version = NONE,
		head = [
		    H.Head_TITLE([], [H.PCDATA name])
		  ],
		content = H.BodyOrFrameset_BODY(H.BODY([], [H.BlockOrScript_BLOCK(H.PRE([], txt))]))
	      })
	  end

    fun kw strm s = (
	  PP.pushStyle(strm, HTML4Dev.styleClass "kw");
	  PP.string strm s;
	  PP.popStyle strm)
    fun punct strm s = (
	  PP.pushStyle(strm, HTML4Dev.styleClass "p");
	  PP.string strm s;
	  PP.popStyle strm)
    fun lit strm s = (
	  PP.pushStyle(strm, HTML4Dev.styleClass "lit");
	  PP.string strm s;
	  PP.popStyle strm)

    fun t40 () = withPP ("Test 40 [C code]", 20) (fn strm => (
	  PP.openHBox strm;
	    kw strm "if";
	    PP.space strm 1;
	    punct strm "("; PP.string strm "x < y"; punct strm ")";
	    PP.space strm 1;
	    punct strm "{";
	    PP.openHVBox strm (PP.Abs 4);
	      PP.space strm 1;
	      PP.string strm "stmt1"; punct strm ";"; PP.space strm 1;
	      PP.openHBox strm;
		kw strm "if";
		PP.space strm 1;
		punct strm "("; PP.string strm "w < z"; punct strm ")";
		PP.space strm 1;
		punct strm "{";
		PP.openHVBox strm (PP.Abs 4);
		  PP.space strm 1; PP.string strm "stmt2"; punct strm ";";
		  PP.space strm 1; PP.string strm "stmt3"; punct strm ";";
		  PP.space strm 1; PP.string strm "stmt4"; punct strm ";";
		PP.closeBox strm; PP.newline strm;
		punct strm "}";
	      PP.closeBox strm;
	      PP.space strm 1; PP.string strm "stmt5"; punct strm ";";
	      PP.space strm 1; PP.string strm "stmt6"; punct strm ";";
	    PP.closeBox strm; PP.newline strm;
	    punct strm "}";
	  PP.closeBox strm));

    end (* local *)
  end
