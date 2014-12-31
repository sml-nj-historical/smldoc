(* smldoc.lex
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

%name SMLDocLexer;

%arg (lexErr : AntlrStreamPos.pos * string list -> unit);

%defs(
    structure T = SMLDocTokens
    structure DC = DCTokens
    structure SS = Substring

    type lex_result = T.token

    fun eof () = T.EOF

    fun inc r = (r := !r + 1)
    fun dec r = (r := !r - 1)

  (* count nesting level of comments *)
    val commentLevel = ref 0;
    val commentStart : AntlrStreamPos.pos ref = ref 0

  (* buffer for scanning strings *)
    val stringBuf : string list ref = ref []
    val isChar = ref true	(* true for char, false for string *)
    fun addString s = (stringBuf := s :: !stringBuf)
    fun mkString () = let
	  val s = String.concat(List.rev(!stringBuf))
          in
            stringBuf := [];
            if !isChar then T.CHAR s else T.STRING s
          end

  (* token buffer for scanning documentation comments *)
    val tokens : DC.token list ref = ref[]
    val isAfter = ref false
    fun addToken tok = (tokens := tok :: !tokens)
    fun mkComment () = let
	  val toks = List.rev(!tokens)
	  in
	    tokens := [];
	    if !isAfter then T.AFTER_COMMENT toks else T.COMMENT toks
	  end

  (* counting nesting depth of "[" "]" brackets in code *)
    val codeLevel = ref 0
);

%let alphanum = [A-Za-z'_0-9]*;
%let alphanumId = [A-Za-z]{alphanum};
%let sym = [-!%&$+/:<=>?@~`\^|#*]|"\\";
%let symId = {sym}+;
%let id = {alphanumId}|{symId};
%let longid = {id}("."{id})*;		(* Q: should this be ({alphanumId}.)*{id} ? *)
%let ws = "\012"|[\t\ ];
%let cr = "\013";
%let nl = "\010";
%let eol = ({cr}{nl}|{nl}|{cr});
%let num = [0-9]+;
%let frac = "."{num};
%let exp = [eE](\~?){num};
%let real = (\~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
%let hexDigit = [0-9a-fA-F];
%let hexnum = {hexDigit}+;
%let dcChr = ([-a-zA-Z0-9_`~!#$%&*+=(){|;:'",.<>/?]|"]"|"^");  (* anything printable but @ [ } or \ *)

(* C		- comments
 * S		- string
 * F		- split strings
 * DC		- documentation comment
 * DC_BOL	- beginnning-of-line in documentation comment
 * CD		- code in documentation comment
 * CD_C		- comment in documentation comment code
 * CD_S		- string in documentation comment code
 * CD_F		- split string in documentation comment code
 *)
%states INITIAL C S F DC DC_BOL CD CD_C CD_S CD_F;

(**** Punctuation ****)
<INITIAL>","		=> (T.COMMA);
<INITIAL>"{"		=> (T.LBRACE);
<INITIAL>"}"		=> (T.RBRACE);
<INITIAL>"["		=> (T.LBRACKET);
<INITIAL>"]"		=> (T.RBRACKET);
<INITIAL>";"		=> (T.SEMICOLON);
<INITIAL>"("		=> (T.LPAREN);
<INITIAL>")"		=> (T.RPAREN);
<INITIAL>"..."		=> (T.DOTDOTDOT);

<INITIAL>{alphanumId}	=> (Keywords.idToken yytext);
<INITIAL>{symId}	=> (Keywords.symToken yytext);

<INITIAL>{real}		=> (T.REAL yytext);
<INITIAL>{num}		=> (T.INT yytext);
<INITIAL>"~"{num}	=> (T.INT yytext);
<INITIAL>"0x"{hexnum}	=> (T.INT yytext);
<INITIAL>"~0x"{hexnum}	=> (T.INT yytext);
<INITIAL>"0w"{num}	=> (T.WORD yytext);
<INITIAL>"0wx"{hexnum}	=> (T.WORD yytext);

<INITIAL>\"     	=> (stringBuf := [yytext];
			    isChar := false;
			    YYBEGIN S;
			    continue ());
<INITIAL>\#\"   	=> (stringBuf := [yytext];
			    isChar := true;
			    YYBEGIN S;
			    continue ());

(**** Comments ****)
<INITIAL>"(**)"		=> (skip());
<INITIAL>"(**""*"+")"	=> (skip());
<INITIAL>"(**<"		=> (YYBEGIN DC;
			    isAfter := true;
			    continue());
<INITIAL>"(**"		=> (YYBEGIN DC;
			    isAfter := false;
			    continue());
<INITIAL>"(*"   	=> (YYBEGIN C;
			    commentLevel := 1;
			    commentStart := yypos;
			    continue ());

<INITIAL>.      	=> (lexErr (yypos, ["bad input character '", String.toString yytext, "'"]);
			    continue ());

<C>"(*"         	=> (inc commentLevel; continue ());
<C>{eol}           	=> (continue ());
<C>"*)"         	=> (dec commentLevel;
			    if 0 = !commentLevel then YYBEGIN INITIAL else ();
			    continue ());
<C>.            	=> (continue ());

(***** Strings *****)
<S>"\""			=> (YYBEGIN INITIAL; mkString());
<S>\\[abfnrtv]		=> (addString yytext; continue ());
<S>\\\^[@-_]    	=> (addString yytext; continue ());
<S>\\\^.		=> (lexErr (yypos,
			      ["illegal control escape; must be one of @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"]);
			    continue ());
<S>\\[0-9]{3}		=> (addString yytext; continue ());
<S>\\u{hexDigit}{4}	=> (addString yytext; continue ());
<S>\\U{hexDigit}{8}	=> (addString yytext; continue ());
<S>"\\\""		=> (addString yytext; continue ());
<S>\\\\			=> (addString yytext; continue ());
<S>\\{ws}+		=> (YYBEGIN F; addString yytext; continue ());
<S>\\{eol}		=> (YYBEGIN F; 
			    addString yytext;
			    continue ());
<S>\\			=> (lexErr (yypos, ["illegal string escape"]); continue ());
<S>{eol}		=> (lexErr (yypos, ["unclosed string"]);
			    continue ());
<S>.			=> (addString yytext; continue ());

<F>({ws}|{eol})*	=> (addString yytext; continue ());
<F>\\           	=> (YYBEGIN S;
			    addString yytext; 
			    continue ());
<F>.            	=> (lexErr (yypos, ["unclosed string"]);
			    continue ());

(**** Documentation comments ****
 *
 * This part of the scanner handles text that is inside a documentation comment.
 * We tokenize the contents of the comment and return the list of tokens as a single
 * SML parser token (wrapped with either COMMENT or AFTER_COMMENT).
 *)

<DC>"*"*"*)"		=> (YYBEGIN INITIAL; mkComment());

(* the DC_BOL state handles prefixes at the beginning of a line *)
<DC>{eol}		=> (addToken DC.EOL; YYBEGIN DC_BOL; continue());
<DC_BOL>{ws}*"*"*{ws}*{eol}
			=> (addToken DC.BLANKLN; continue());
<DC_BOL>{ws}*"*"*"*)"	=> (YYBEGIN INITIAL; mkComment());
<DC_BOL>{ws}*"*"*	=> (YYBEGIN DC; continue());

<DC>"@"[a-zA-Z0-9]+	=> (addToken (DC.TAG(Atom.atom'(SS.slice(yysubstr, 1, NONE))));
			    continue());

<DC>"\\b{"		=> (addToken DC.BOLD; continue());
<DC>"\\i{"		=> (addToken DC.ITALIC; continue());
<DC>"\\e{"		=> (addToken DC.EMPH; continue());
<DC>"\\begin{"[a-zA-Z0-9]*"}"
			=> (addToken (DC.BEGIN(Atom.atom'(
			      SS.slice(yysubstr, 7, SOME(SS.size yysubstr - 8)))));
			    continue());
<DC>"\\end{"[a-zA-Z0-9]*"}"
			=> (addToken (DC.END(Atom.atom'(
			      SS.slice(yysubstr, 5, SOME(SS.size yysubstr - 6)))));
			    continue());
<DC>"\\item"		=> (addToken DC.ITEM; continue());
<DC>"}"			=> (addToken DC.CLOSE; continue());
<DC>"["			=> (YYBEGIN CD;
			    codeLevel := 1;
			    addToken DC.CODE;
			    continue());
<DC>{dcChr}+		=> (addToken (DC.TEXT yytext); continue());
<DC,CD>{ws}+		=> (addToken (DC.WS yytext); continue());

<CD>{eol}		=> (addToken DC.EOL; continue());
<CD>"["			=> (inc codeLevel;
			    addToken (DC.PUNCT(Atom.atom "["));
			    continue());
<CD>"]"			=> (let val n = !codeLevel - 1
			    in
			      if (n = 0)
				then (YYBEGIN DC; addToken DC.CLOSE_CODE)
				else addToken (DC.PUNCT(Atom.atom "]"));
			      continue()
			    end);
<CD>{alphanumId}	=> (addToken (Keywords.idToken' yytext); continue());
<CD>{symId}		=> (addToken (Keywords.symToken' yytext); continue());

<CD>{real}		=> (addToken (DC.REAL yytext); continue());
<CD>{num}		=> (addToken (DC.INT yytext); continue());
<CD>"~"{num}		=> (addToken (DC.INT yytext); continue());
<CD>"0x"{hexnum}	=> (addToken (DC.INT yytext); continue());
<CD>"~0x"{hexnum}	=> (addToken (DC.INT yytext); continue());
<CD>"0w"{num}		=> (addToken (DC.WORD yytext); continue());
<CD>"0wx"{hexnum}	=> (addToken (DC.WORD yytext); continue());

<CD>[,;(){}]		=> (addToken (DC.PUNCT(Atom.atom yytext)); continue());
<CD>"..."		=> (addToken (DC.PUNCT(Atom.atom yytext)); continue());
