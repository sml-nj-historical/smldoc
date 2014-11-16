(* smldoc.lex
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

%name SMLDocLexer;

%let alphanum = [A-Za-z'_0-9]*;
%let alphanumId = [A-Za-z]{alphanum};
%let sym = [-!%&$+/:<=>?@~`\^|#*]|"\\";
%let symId = {sym}+;
%let id = {alphanumId}|{symId};
%let longid = {id}("."{id})*;		(* Q: should this be ({alphanumId}.)*{id} ? *)
%let ws = ("\012"|[\t\ ])*;
%let nrws = ("\012"|[\t\ ])+;
%let cr = "\013";
%let nl = "\010";
%let eol = ({cr}{nl}|{nl}|{cr});
%let num = [0-9]+;
%let frac = "."{num};
%let exp = [eE](\~?){num};
%let real = (\~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
%let hexDigit = [0-9a-fA-F];
%let hexnum = {hexDigit}+;

(* C - comments; DC - documentation comment; S - string; F - split strings *)
%states INITIAL C DC S F;

(**** Punctuation ****)
<INITIAL>","	=> (Tokens.COMMA);
<INITIAL>"{"	=> (Tokens.LBRACE);
<INITIAL>"}"	=> (Tokens.RBRACE);
<INITIAL>"["	=> (Tokens.LBRACKET);
<INITIAL>"]"	=> (Tokens.RBRACKET);
<INITIAL>";"	=> (Tokens.SEMICOLON);
<INITIAL>"("	=> (Tokens.LPAREN);
<INITIAL>")"	=> (Tokens.RPAREN);
<INITIAL>"..."	=> (Tokens.DOTDOTDOT);

<INITIAL>{id}	=> (Keywords.idToken yytext);

<INITIAL>{real}	=> (Tokens.REALyytext);
<INITIAL>{num}	=> (Tokens.INT yytext);
<INITIAL>"~"{num}
		=> (Tokens.INT yytext);
<INITIAL>"0x"{hexnum}
		=> (Tokens.INT yytext);
<INITIAL>"~0x"{hexnum}
		=> (Tokens.INT yytext);
<INITIAL>"0w"{num}
		=> (Tokens.WORD yytext);
<INITIAL>"0wx"{hexnum}
		=> (Tokens.WORD yytext);

<INITIAL>\"     => (charlist := [yytext]
                    ; stringStart := Source.getPos (source, Position.toInt yypos)
                    ; stringtype := true
                    ; YYBEGIN S
                    ; continue ());
<INITIAL>\#\"   => (charlist := [yytext]
                    ; stringStart := Source.getPos (source, Position.toInt yypos)
                    ; stringtype := false
                    ; YYBEGIN S
                    ; continue ());

(**** Comments ****)
<INITIAL>"(**)"	=> ();
<INITIAL>"(**""*"+")"	
	=> (continue());
<INITIAL>"(**"	=> (YYBEGIN DC; continue());
<INITIAL>"(*"   => (YYBEGIN C;
		    commentLevel := 1
                    commentStart := Source.getPos (source, Position.toInt yypos);
                    continue ());

<INITIAL>.      => (error (source, yypos, yypos + 1, "illegal token"); continue ());

<C>"(*"         => (inc commentLevel; continue ());
<C>\n           => (Source.newline (source, Position.toInt yypos) ; continue ());
<C>"*)"         => (dec commentLevel;
                    if 0 = !commentLevel then YYBEGIN INITIAL else ();
                    continue ());
<C>.            => (continue ());

(**** Documentation comments ****)

<DC>"@author"	=> (Tokens.TAG_author);
<DC>"@deprecated"
		=> (Tokens.TAG_deprecated);
<DC>"@param"	=> (Tokens.TAG_param);
<DC>"@raise"	=> (Tokens.TAG_raise);
<DC>"@return"	=> (Tokens.TAG_return);
<DC>"@see"	=> (Tokens.TAG_see);
<DC>"@since"	=> (Tokens.TAG_since);
<DC>"@source"	=> (Tokens.TAG_source);
<DC>"@before"	=> (Tokens.TAG_before);
<DC>"@version"	=> (Tokens.TAG_version);
<DC>"{[0-9]+"	=> (Tokens.SECTION(valOf(Int.fromString(String.extract(yytext, 1, NONE)))));
<DC>"{b"	=> (Tokens.BOLD);
<DC>"{i"	=> (Tokens.ITALIC);
<DC>"{e"	=> (Tokens.EMPH);
<DC>"{C"	=> (Tokens.CENTER);
<DC>"{L"	=> (Tokens.LEFT);
<DC>"{R"	=> (Tokens.RIGHT);
<DC>"{ul"	=> (Tokens.ITEMIZE);
<DC>"{ol"	=> (Tokens.ENUMERATE);
<DC>"{-"	=> (Tokens.ITEM);
<DC>"}"		=> (Tokens.CLOSE);
<DC>"{["	=> (Tokens.PRE_CODE);
<DC>"]}"	=> (Tokens.CLOSE_PRE_CODE);
<DC>"["		=> (Tokens.CODE);
<DC>"]"		=> (Tokens.CLOSE_CODE);

(***** Strings *****)
<S>"\""	=> (let
	    val s = String.concat (List.rev ("\"" :: !charlist))
	    val _ = charlist := nil
	    in
	      YYBEGIN INITIAL;
	      if !stringtype
		then Tokens.STRING s
		else Tokens.CHAR s
	    end);
<S>\\a          => (addChar #"\a"; continue ());
<S>\\b          => (addChar #"\b"; continue ());
<S>\\f          => (addChar #"\f"; continue ());
<S>\\n          => (addChar #"\n"; continue ());
<S>\\r          => (addChar #"\r"; continue ());
<S>\\t          => (addChar #"\t"; continue ());
<S>\\v          => (addChar #"\v"; continue ());
<S>\\\^[@-_]    => (addChar (Char.chr(Char.ord(String.sub(yytext, 2)) -Char.ord #"@"));
                    continue ());
<S>\\\^.
	=> (error (source, yypos, yypos + 2,
	      "illegal control escape; must be one of @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_");
	    continue ());

<S>\\[0-9]{3}
	=> (let
	    fun c (i, scale) = scale * (Char.ord(String.sub (yytext, i)) - Char.ord #"0")
	    val () = addOrd (IntInf.fromInt (c (1, 100) + c (2, 10) + c (3, 1)))
	    in
	      continue ()
	    end);
<S>\\u{hexDigit}{4}
	=> (addHexEscape (String.substring (yytext, 2, 4), source, yypos); continue ());
<S>\\U{hexDigit}{8}
	=> (addHexEscape (String.substring (yytext, 2, 8), source, yypos); continue ());
<S>"\\\""
	=> (addString "\""; continue ());
<S>\\\\
	=> (addString "\\"; continue ());
<S>\\{nrws}
	=> (YYBEGIN F; continue ());
<S>\\{eol}
	=> (Source.newline (source, (Position.toInt yypos) + 1) ; YYBEGIN F ; continue ());
<S>\\	=> (stringError (source, yypos, "illegal string escape"); continue ());
<S>{eol}
	=> (Source.newline (source, Position.toInt yypos);
	    stringError (source, yypos, "unclosed string")
	    continue ());
<S>.	=> (addString yytext; continue ());

<F>{eol}        => (Source.newline (source, Position.toInt yypos) ; continue ());
<F>{ws}         => (continue ());
<F>\\           => (YYBEGIN S
                    ; stringStart := Source.getPos (source, Position.toInt yypos)
                    ; continue ());
<F>.            => (stringError (source, yypos, "unclosed string")
                    ; continue ());
