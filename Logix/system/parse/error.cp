/* $Header: /home/qiana/Repository/Logix/system/parse/error.cp,v 1.1.1.1 1999/07/09 07:03:09 bill Exp $ */
-export([prepare/5]).
-language(compound).
-mode(trust).

procedure prepare(String, [Any], [Any], [Any], String).

prepare(Error, Rest1,Rest2, Tokens, Text) :-
	construct_diagnostic(Rest1,Rest2, Tokens, FlatTerm),
	terms_to_string#
	    acyclic_grounded_terms_to_string([Error | FlatTerm], 0, 78, Text)
	.

construct_diagnostic(Rest1,Rest2, Tokens, FlatTerm) :-
    true :
      FlatTerm = ['
'		 | Till_Error] |
	extract_first(Tokens, Rest1, First),
	extract_last(Rest1, Rest2, Last),
	diagnostic(First, no, Till_Error,['
 **here**
  '				    | From_Error?]),
	diagnostic(Last, no, From_Error,['.']).


extract_first(Tokens, Rest, First) :-

    Tokens = Rest :
      First = [] |
	true;

    otherwise,
    Tokens ? Token :
      First ! Token |
    	extract_first.


extract_last(Rest1, Rest2, Last) :-

    Rest1 ? Token,
    Token =\= '.' :
      Last ! Token |
    	extract_last;

    Rest1 ? '.' :
      Rest1' = Rest2, Last = [] |
	true;

    otherwise :
      Rest1 = Rest2,
      Last = [] |
	true.


diagnostic(Tokens, PreSpace, Flats1, Flats2) :-

    Tokens ? Constant,
    constant(Constant) |
	diagnostic_constant(Constant, Tokens', PreSpace, Flats1,Flats2);

    Tokens ? Compound,
    compound(Compound) |
	diagnostic_compound(Compound, Tokens',Tokens'', PreSpace,PreSpace',
				Flats1,Flats1'
	),
	diagnostic;

    Tokens = [] : PreSpace = _,
      Flats1 = Flats2 |
	true.


diagnostic_constant(Constant, Tokens, PreSpace, Flats1,Flats2) :-

    string(Constant) |
	prespace_string(PreSpace, Constant, Flats1,Flats1'),
	postspace_string(Constant, PreSpace'),
	diagnostic(Tokens, PreSpace', Flats1',Flats2);

    number(Constant) |
	prespace(PreSpace, Constant, Flats1,Flats1'),
	diagnostic(Tokens, yes, Flats1',Flats2);

    otherwise : Constant = _ |
	diagnostic(Tokens, PreSpace, Flats1,Flats2).


diagnostic_compound(Compound, Tokens1,Tokens2, PreSpace1,PreSpace2,
				Flats1,Flats2
) :-

    Compound = `Name,
    string(Name) :
      PreSpace2 = yes |
	diagnostic_variable(Name,Variable,  Tokens1,Tokens2), 
	prespace(PreSpace1, Variable, Flats1,Flats2);

    Compound = ?Name,
    string(Name) :
      PreSpace2 = yes |
	diagnostic_variable(Name,Variable,  Tokens1,Tokens2), 
	prespace(PreSpace1, Variable, Flats1,Flats2);

    Compound = funct(Functor, Quote),
    string(Functor), integer(Quote),
    Tokens1 = [Open | Tokens2^],
    string(Open) :
      PreSpace2 = no |
	requote_string(Functor, Quote, ReQuoted),
	concatenate(ReQuoted, Open, FunctorOpen),
	prespace(PreSpace1, FunctorOpen, Flats1,Flats2);

    Compound = funct(`Varname,Quote),
    string(Varname), integer(Quote),
    Tokens1 = [Open | Tokens2^],
    string(Open)  :
      PreSpace2 = no |
	requote_string(Varname, Quote, ReQuoted),
	concatenate(ReQuoted, Open, FunctorOpen),
	prespace(PreSpace1, FunctorOpen, Flats1,Flats2);

    Compound = funct(?Varname,Quote),
    string(Varname), integer(Quote),
    Tokens1 = [Open | Tokens2^],
    string(Open)  :
      PreSpace2 = no |
	requote_string(Varname, Quote, ReQuoted),
	concatenate(ReQuoted, Open, FunctorOpen),
	prespace(PreSpace1, FunctorOpen, Flats1,Flats2);

    Compound = overflow(Digits),
    string(Digits) :
      Tokens1 = Tokens2,
      PreSpace2 = yes |
	prespace(PreSpace1, Digits, Flats1,Flats2);

    Compound = string(String, Quote),
    string(String), integer(Quote) :
      Tokens1 = Tokens2,
      PreSpace2 = yes |
	requote_string(String, Quote, ReQuoted),
	prespace(PreSpace1, ReQuoted, Flats1,Flats2);

    otherwise : Compound = _,
      Tokens1 = Tokens2,
      PreSpace1 = PreSpace2,
      Flats1 = Flats2 |
	true.


diagnostic_variable(Name, Variable, Flats1,Flats2) :-

    Flats1 = ['?' | Flats2^] |
	concatenate(Name, '?', Variable);

    otherwise :
      Name = Variable,
      Flats1 = Flats2 |
	true.


requote_string(String, Quote, ReQuoted) :-

    Quote =\= 0,
    string_to_dlist(String, Characters, []) |
	requote_list(Characters, Quote, QuotedList, Done),
	list_to_string(Done, [Quote | QuotedList], ReQuoted);

    Quote = 0 :
      String = ReQuoted |
	true.


requote_list(Chars, Quote, QuotedList, Done) :-

    Chars ? Quote:
      QuotedList ! Quote,
      QuotedList' ! Quote |
	requote_list;

    otherwise,
    Chars ? Char :
      QuotedList ! Char |
	requote_list;

    Chars = [] :
      QuotedList = [Quote],
      Done = done |
	true.


list_to_string(done, Chars, String) :-
    list_to_string(Chars, String^) |
	true.


prespace(yes, Token, [' ', Token | Flats]^,Flats).
prespace(no,  Token, [Token | Flats]^,Flats).


prespace_string(PreSpace,Token, Flats1,Flats2) :-

    PreSpace = yes,
    Token =\= ',', Token =\= ';',
    Token =\= ')', Token =\= ']', Token =\= '}' :
      Flats1 = [' ', Token | Flats2] |
	true;

    otherwise : PreSpace = _,
      Flats1 = [Token | Flats2] |
	true.


postspace_string(String, PreSpace) :-

    String =\= '(', String =\= '[', String =\= '{' :
      PreSpace = yes |
	true;

    otherwise : String = _,
      PreSpace = no |
	true.


concatenate(String1, String2, Concatenated) :-
    string_to_dlist(String1, D1, D2),
    string_to_dlist(String2, D2, []) |
	list_to_string(D1, Concatenated).
