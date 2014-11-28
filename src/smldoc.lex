(* smldoc.lex
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

%name SMLDocLexer;

$defs(
    structure T = Tokens
    structure MT = MarkupTokens

  (* markup token buffer for parsing documentation comments *)
    val markup : MT.token list ref = ref[]
    val addMarkup tok = (markup := tok :: !markup)
    val getMarkup () = (List.rev(!markup) before markup := [])
);

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

(* C - comments; S - string; F - split strings; DC - documentation comment; *)
%states INITIAL C S F DC;

(**** Punctuation ****)
<INITIAL>","	=> (T.COMMA);
<INITIAL>"{"	=> (T.LBRACE);
<INITIAL>"}"	=> (T.RBRACE);
<INITIAL>"["	=> (T.LBRACKET);
<INITIAL>"]"	=> (T.RBRACKET);
<INITIAL>";"	=> (T.SEMICOLON);
<INITIAL>"("	=> (T.LPAREN);
<INITIAL>")"	=> (T.RPAREN);
<INITIAL>"..."	=> (T.DOTDOTDOT);

<INITIAL>{id}	=> (Keywords.idToken yytext);

<INITIAL>{real}	=> (T.REALyytext);
<INITIAL>{num}	=> (T.INT yytext);
<INITIAL>"~"{num}
		=> (T.INT yytext);
<INITIAL>"0x"{hexnum}
		=> (T.INT yytext);
<INITIAL>"~0x"{hexnum}
		=> (T.INT yytext);
<INITIAL>"0w"{num}
		=> (T.WORD yytext);
<INITIAL>"0wx"{hexnum}
		=> (T.WORD yytext);

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
<INITIAL>"(**<"	=> (YYBEGIN DC; T.AFTER_COMMENT(continue()) before YYBEGIN INITIAL);
<INITIAL>"(**"	=> (YYBEGIN DC; T.COMMENT(continue()) before YYBEGIN INITIAL);
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

(***** Strings *****)
<S>"\""	=> (let
	    val s = String.concat (List.rev ("\"" :: !charlist))
	    val _ = charlist := nil
	    in
	      YYBEGIN INITIAL;
	      if !stringtype
		then T.STRING s
		else T.CHAR s
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

(**** Documentation comments ****
 *
 * This part of the scanner handles text that is inside a documentation comment.
 * We tokenize the contents of the comment and return the list of tokens as a single
 * SML parser token (wrapped with either COMMENT or AFTER_COMMENT).
 *)

(* the DC_BOL state handles prefixes at the beginning of a line *)
<DC_BOL>{ws}*"*"*{ws}*{eol}
			=> (addMarkup MT.BLANKLN; continue());
<DC_BOL>{ws}*"*"*	=> (YYBEGIN DC; continue());
<DC>{eol}		=> (addMarkup MT.NL; YYBEGIN DC_BOL; continue());

<DC,DC_BOL>"*)"		=> (getMarkup())
<DC>"@author"		=> (addMarkup MT.TAG_author; continue());
<DC>"@deprecated"	=> (addMarkup MT.TAG_deprecated; continue());
<DC>"@param"		=> (addMarkup MT.TAG_param; continue());
<DC>"@raise"		=> (addMarkup MT.TAG_raise; continue());
<DC>"@return"		=> (addMarkup MT.TAG_return; continue());
<DC>"@see"		=> (addMarkup MT.TAG_see; continue());
<DC>"@since"		=> (addMarkup MT.TAG_since; continue());
<DC>"@source"		=> (addMarkup MT.TAG_source; continue());
<DC>"@before"		=> (addMarkup MT.TAG_before; continue());
<DC>"@version"		=> (addMarkup MT.TAG_version; continue());


<DC>"{[0-9]+"		=> (MT.SECTION(valOf(Int.fromString(String.extract(yytext, 1, NONE)))));
<DC>"\b{"		=> (MT.BOLD);
<DC>"\i{"		=> (MT.ITALIC);
<DC>"\e{"		=> (MT.EMPH);
<DC>"\begin{center}"	=> (MT.CENTER);
<DC>"\end{center}"	=> (MT.CENTER);
<DC>"\begin{quote}"	=> (MT.LEFT);
<DC>"\end{quote}"	=> (MT.LEFT);
<DC>"{R"		=> (MT.RIGHT);
<DC>"\begin{itemize}"	=> (MT.ITEMIZE);
<DC>"\end{itemize}"	=> (MT.ITEMIZE);
<DC>"\begin{enumerate}"	=> (MT.ENUMERATE);
<DC>"\end{enumerate}"	=> (MT.ENUMERATE);
<DC>"\item"		=> (MT.ITEM);
<DC>"}"			=> (MT.CLOSE);
<DC>"["			=> (MT.CODE);
<DC>"]"			=> (MT.CLOSE_CODE);
