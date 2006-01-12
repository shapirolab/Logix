/*

FCP tokenizer
Michael Hirsch, Marc Rosen, Muli Safra, Bill Silverman

Last update by		$Author: bill $
		       	$Date: 2006/01/12 21:46:28 $
Currently locked by 	$Locker:  $
			$Revision: 1.2 $
			$Source: /home/qiana/Repository/Logix/system/parse/tokenize.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([characters/2]).
-language([evaluate, compound, colon]).

procedure characters([Integer], [Token]).

Token ::= Number ; String ; Nil ;
	  string(String, Integer) ; funct(Functor, Integer) ;
	  Variable,
	  overflow(String).
Functor ::= Variable ; String.
Variable ::= {`"`", String} ; {`"?", String}.

characters(Chars,Tokens) :-
	monitor_functions # monitor_events(Done, Abort),
	tokenizer(Chars, Tokens, Done, Abort).

/* * * * * * * * * * * * * C O N S T A N T S * * * * * * * * * * * * * * * * */

Asterisk => 42.		% Asterisk := ascii("*").

OpenParen => 40.	% OpenParen := ascii("(").

Period => 46.		% Period := ascii(".").

QuestionMark => 63.	% QuestionMark := ascii("?").


/* * * * * * * * * * * * * * M A C R O S * * * * * * * * * * * * * * * * * */

Simple(Char) =>
   (Cs ? C, C =:= ascii(Char) :
      Tokens ! Char |
	self
).

Compound(Char) =>
   (Cs ? C, C =:= ascii(Char) :
      Tokens ! Token |
	get_special(Cs', Cs'', Special),
	construct_special(Cs'', [C | Special], Token),
	self
).

Quote(Char) =>
   (Cs ? C, C =:= ascii(Char) :
      Tokens ! Token |
	get_string(C, Cs', Cs'', QuotedCs),
	construct_q_symbol(Cs'', C, QuotedCs, Token),
	self
).

Numeric(Char) =>
   (Cs ? C, C =:= ascii(Char) :
      Tokens ! Token |
	get_digits(Cs', Cs1, List, Tail),
	get_number(Cs1, Cs'', [C | List], Tail, Token),
	self
).

Variable(Char) =>
   (Cs ? C, C =:= ascii(Char) :
      Tokens ! Token |
	get_alphalist(Cs', Cs'', VarCs),
	construct_variable(Cs'', [C | VarCs], Token),
	self
).

Symbol(Char) =>
   (Cs ? C, C =:= ascii(Char) :
      Tokens ! Token |
	get_alphalist(Cs', Cs'', SymCs),
	construct_symbol(Cs'', [C | SymCs], Token),
	self
).

AlphaNum(Char) =>
   (Cs1 ? C, C =:= ascii(Char) :
     Alphas ! C |
	self
).

Number(Char) =>
   (Cs1 ? C, C =:= ascii(Char) :
     NumberCs ! C |
	self
).

Special(Char) =>
   (Cs1 ? C, C =:= ascii(Char) :
     SpecialCs ! C |
	self
).

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


tokenizer(Cs, Tokens, Done, Abort) :-

    Simple("!");

    Quote('"');

    Compound("#");

    Symbol("$");

    Cs ? C, C =:= ascii("%") |				% "%"
	till_nl(Cs', Cs''),
	tokenizer;

    Compound("&");

    Quote("'");

    Simple("(");

    Simple(")");

    Simple("*");

    Compound("+");

    Simple(",");

    Compound("-");

    Cs ? C, C =:= ascii(".") :
      Tokens ! Token? |
	leading_point(Cs', Cs'', Token),
	tokenizer;

    Cs ? C, C =:= ascii("/"),
    Cs' ? Asterisk |			% "/*"
	skip_comment(Cs'', Cs'''),
	tokenizer;

    Cs ? C, C =:= ascii("/"),		% "/"
    Cs' =\= [Asterisk | _] :
      Tokens ! Token |
	get_special(Cs', Cs'', Special),
	construct_special(Cs'', [C | Special], Token),
	tokenizer;

    Numeric("0");

    Numeric("1");

    Numeric("2");

    Numeric("3");

    Numeric("4");

    Numeric("5");

    Numeric("6");

    Numeric("7");

    Numeric("8");

    Numeric("9");

    Compound(":");

    Compound(";");

    Compound("<");

    Compound("=");

    Compound(">");

    Compound("?");

    Compound("@");

    Variable("A");

    Variable("B");

    Variable("C");

    Variable("D");

    Variable("E");

    Variable("F");

    Variable("G");

    Variable("H");

    Variable("I");

    Variable("J");

    Variable("K");

    Variable("L");

    Variable("M");

    Variable("N");

    Variable("O");

    Variable("P");

    Variable("Q");

    Variable("R");

    Variable("S");

    Variable("T");

    Variable("U");

    Variable("V");

    Variable("W");

    Variable("X");

    Variable("Y");

    Variable("Z");

    Simple("[");

    Compound("\");

    Simple("]");

    Simple("^");

    Variable("_");

    Simple("`");

    Symbol("a");

    Symbol("b");

    Symbol("c");

    Symbol("d");

    Symbol("e");

    Symbol("f");

    Symbol("g");

    Symbol("h");

    Symbol("i");

    Symbol("j");

    Symbol("k");

    Symbol("l");

    Symbol("m");

    Symbol("n");

    Symbol("o");

    Symbol("p");

    Symbol("q");

    Symbol("r");

    Symbol("s");

    Symbol("t");

    Symbol("u");

    Symbol("v");

    Symbol("w");

    Symbol("x");

    Symbol("y");

    Symbol("z");

    Simple("{");

    Simple("|");

    Simple("}");

    Simple("~");

    Cs ? _,
    otherwise, unknown(Abort) |
	tokenizer;

    Cs = [] : Abort = _,
      Done = done,
      Tokens = [] ;

    known(Abort) : Cs = _,
      Done = done,
      Tokens = [] .


get_string(Type, Cs1, Cs2, QuotedCs) :-

    Cs1 ? Type, Cs1' ? Type :
      QuotedCs ! Type |
	get_string;

    Cs1 ? Type, Cs1' =\= [Type | _] :
      Cs1' = Cs2,
      QuotedCs = [] ;

    otherwise,
    Cs1 ? C :
      QuotedCs ! C |
	get_string.

get_string(_, [], []^, []^).


till_nl(Cs1, Cs2) :-

    Cs1 ? C,						% cr
    C =:= ascii(cr) :
      Cs1' = Cs2 ;

    Cs1 ? C,						% lf
    C =:= ascii(lf) :
      Cs1' = Cs2 ;

    otherwise,
    Cs1 ? _ |
	till_nl;

    Cs1 = [] :
	Cs2 = [] .


skip_comment(Cs1, Cs2) :-

    Cs1 ? Asterisk,
    Cs1' ? Slash, Slash =:= ascii('/') :
      Cs1'' = Cs2 ;

    otherwise,
    Cs1 ? _ |
	skip_comment;

    Cs1 = [] :
      Cs2 = [] .

			
get_alphalist(Cs1, Cs2, Alphas) :-

    AlphaNum("$");

    AlphaNum("'");

    AlphaNum("0");

    AlphaNum("1");

    AlphaNum("2");

    AlphaNum("3");

    AlphaNum("4");

    AlphaNum("5");

    AlphaNum("6");

    AlphaNum("7");

    AlphaNum("8");

    AlphaNum("9");

    AlphaNum("A");

    AlphaNum("B");

    AlphaNum("C");

    AlphaNum("D");

    AlphaNum("E");

    AlphaNum("F");

    AlphaNum("G");

    AlphaNum("H");

    AlphaNum("I");

    AlphaNum("J");

    AlphaNum("K");

    AlphaNum("L");

    AlphaNum("M");

    AlphaNum("N");

    AlphaNum("O");

    AlphaNum("P");

    AlphaNum("Q");

    AlphaNum("R");

    AlphaNum("S");

    AlphaNum("T");

    AlphaNum("U");

    AlphaNum("V");

    AlphaNum("W");

    AlphaNum("X");

    AlphaNum("Y");

    AlphaNum("Z");

    AlphaNum("_");

    AlphaNum("a");

    AlphaNum("b");

    AlphaNum("c");

    AlphaNum("d");

    AlphaNum("e");

    AlphaNum("f");

    AlphaNum("g");

    AlphaNum("h");

    AlphaNum("i");

    AlphaNum("j");

    AlphaNum("k");

    AlphaNum("l");

    AlphaNum("m");

    AlphaNum("n");

    AlphaNum("o");

    AlphaNum("p");

    AlphaNum("q");

    AlphaNum("r");

    AlphaNum("s");

    AlphaNum("t");

    AlphaNum("u");

    AlphaNum("v");

    AlphaNum("w");

    AlphaNum("x");

    AlphaNum("y");

    AlphaNum("z");

    otherwise :
      Cs1 = Cs2,
      Alphas = [] .


construct_variable(Next, VarCs, Result) :-

    Next ? Q, Q =:= ascii("?"),				% "?("
    Next' = [C | _], C := ascii("("),
    list_to_string(VarCs, VarName) :
      Result = funct(?VarName, 0) ;

    Next = [C | _], C := ascii("("),			% "("
    list_to_string(VarCs, VarName) :
      Result = funct(`VarName, 0) ;

    Next ? Q, Q =:= ascii("?"),
    Next' =\= [OpenParen | _],			% X? => _ro(X)
    Next' =\= [QuestionMark | _],		% X?? => (X)??
    list_to_string(VarCs,VarName) :
      Result = ?VarName ;

    Next ? Q, Q =:= ascii("?"),
    Next' = [QuestionMark, OpenParen | _],
    list_to_string(VarCs,VarName) :
      Result = funct(`VarName, 0) ;

    otherwise,					% X => _var(X)
    list_to_string(VarCs,VarName) : Next = _,
      Result = `VarName .


construct_symbol(Next, SymCs, Result) :-

    Next = [OpenParen | _],
    list_to_string(SymCs, SymName) :
      Result = funct(SymName, 0) ;

    otherwise,
    list_to_string(SymCs, Result^) : Next = _ .


construct_special(Next, SymCs, Result) :-
    known(Next),
    list_to_string(SymCs, Result^) |
	true.


construct_q_symbol(Next, Q, SymCs, Result) :-

    Next = [OpenParen | _],
    list_to_string(SymCs, SymName) :
      Result = funct(SymName, Q) ;

    Next =\= [OpenParen | _],
    list_to_string(SymCs, SymName) :
      Result = string(SymName, Q) ;

    Next = [OpenParen | _],
    SymCs = [] :
      Result = funct('', Q) ;

    otherwise : Next = _, SymCs = _,
      Result = string('', Q) .


leading_point(Cs1, Cs2, Token) :-

    Cs1 ? C,
    ascii("0") =< C, C =< ascii("9") |		% C in "0" ... "9"
 	get_number([Period, C | Cs1'], Cs2, Tail, Tail, Token);

    otherwise :
      Cs1 = Cs2,
      Token = ".".

get_number(Cs1, Cs2, Head, Tail, Number) :-

    Cs1 ? Period,
    Cs1' ? C,
    ascii("0") =< C, C =< ascii("9") :		% C in "0" ... "9"
      Tail = [Period, C | Tail'] |
	get_digits(Cs1'', Cs1''', Tail', []),
	list_to_string(Head, String),
	convert_real(String, Fraction),
	get_exponent(Cs1''', Cs2, EList),
	convert_float(String, Fraction, EList, Number);

    otherwise :
      Tail = [] |
	list_to_string(Head, String),
	convert_integer(String, Integer),
	get_exponent(Cs1, Cs2, EList),
	convert_float(String, Integer, EList, Number).	


get_exponent(Cs1, Cs2, EList) :-

    Cs1 ? E, E =:= ascii("e") :
      EList ! E |
	get_signed_exponent(Cs1', Cs2, EList');

    Cs1 ? E, E =:= ascii("E") :
      EList ! E |
	get_signed_exponent(Cs1', Cs2, EList');

    otherwise :
      Cs1 = Cs2,
      EList = [].

  get_signed_exponent(Cs1, Cs2, EList) :-

    Cs1 ? C,
    ascii("0") =< C, C =< ascii("9") :		% C in "0" ... "9"
      EList = [C | Tail] |
	get_digits(Cs1', Cs2, Tail, []);

    Cs1 ? Minus, Minus =:= ascii("-"),
    Cs1' ? C,
    ascii("0") =< C, C =< ascii("9") :		% C in "0" ... "9"
      EList = [Minus, C | Tail] |
	get_digits(Cs1'', Cs2, Tail, []);

    Cs1 ? Plus, Plus =:= ascii("+"),
    Cs1' ? C,
    ascii("0") =< C, C =< ascii("9") :		% C in "0" ... "9"
      EList = [Plus, C | Tail] |
	get_digits(Cs1'', Cs2, Tail, []);

    otherwise :
      Cs1 = Cs2,
      EList = [].


convert_integer(String, Result) :-

    convert_to_integer(String, Integer) :
      Result = Integer;

    String =?= "" :
      Result = 0;

    otherwise |
	numeric_overflow(String, Result).

    
convert_real(String, Result) :-

    convert_to_real(String, Real) :
      Result = Real;

    otherwise |
	numeric_overflow(String, Result).


convert_float(VString, Value, EList, Number) :-

    Value =?= overflow(_String) |
	real_overflow(VString, EList, Number);

    Value =\= overflow(_),
    EList = [] :
      VString = _,
      Value = Number;

    Value =\= overflow(_),
    EList ? _E |
	list_to_string(EList', EString),
	convert_integer(EString?, EValue),
	compute_scaled_value(VString, Value, EList, Number, EValue).


compute_scaled_value(VString, Value, EList, Number, EValue) :-

    EValue =?= overflow(_) :		% strange case!
      Value = _ |
	real_overflow(VString, EList, Number);
      
    EValue =\= overflow(_),
    convert_to_real(10, Ten) |
	Scaled := Value*pow(Ten, EValue),
	check_scaled_value(VString, Scaled, EList, Number).

  check_scaled_value(VString, Scaled, EList, Number) :-

    convert_to_string(Scaled, String),
    String =\= "inf" :
      EList = _,
      VString = _,
      Scaled = Number;
      
    otherwise :
      Scaled = _ |
	real_overflow(VString, EList, Number).


numeric_overflow(String, Result) :-
    true :
      Result = overflow(String) .


real_overflow(String, List, Result) :-
    string_to_dlist(String, List', List) :
      Result = overflow(String'?) |
	list_to_string(List', String').


get_digits(Cs1, Cs2, NumberCs, Tail) :-

    Number("0");

    Number("1");

    Number("2");

    Number("3");

    Number("4");

    Number("5");

    Number("6");

    Number("7");

    Number("8");

    Number("9");

    otherwise :
      Cs1 = Cs2,
      NumberCs = Tail .


get_special(Cs1, Cs2, SpecialCs) :-

    Special("#");

    Special("&");

    Special("+");

    Special("-");

    Special("/");

    Cs1 ? C,
    ascii(":") =< C, C =< ascii("@") :		% ":", ";", "<"
						% "=", ">","?", "@"
      SpecialCs ! C |
	get_special;

    Special("\");

    otherwise :
      Cs1 = Cs2,
      SpecialCs = [] .
