/*

Convert Term to String

Ehud Shapiro & Michael Hirsch, 02-17-85
Revised By William Silverman, 05-08-85
Additions by Eli Biham & William Silverman, 07-03-85

Last update by		$Author: bill $
		       	$Date: 1999/11/28 12:38:51 $
Currently locked by 	$Locker:  $
			$Revision: 1.2 $
    			$Source: /home/qiana/Repository/Logix/system/terms_to_string.cp,v $

Copyright (C) 1986, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([acyclic_grounded_terms_to_string/3,
	 acyclic_grounded_terms_to_string/4]
).
-mode(failsafe).
-language(compound).

/*

INPUTS :
  acyclic_grounded_terms_to_string/3:

	Terms  - A (non-list) term or a list of grounded terms of bounded
		 depth.
	Options - 
		Options ::= {Indent, Length; Special}
		Special ::= hex ; melted ; none .
		   Indent & Length are described below,
		   hex indicates that non-displayable 4-byte strings should be
		   displayed in hex, inside angle brackets.
                   melted requests that frozen terms be melted to the string.

  acyclic_grounded_terms_to_string/4:
	Terms  - A (non-list) term or a list of grounded terms of bounded
		 depth.
	Indent - Number of blanks which will precede the  Terms  and each line
		 in String.
	Length - After Length characters String will be wrapped by
		 the insertion of a <new-line> character, followed by
		 Indent+  characters (see concatenate.c).

OUTPUT :

   String - A string representing Terms.

*/

procedure acyclic_grounded_terms_to_string(Any, {Integer, Integer, Special},
						String
).

Special ::= hex ; melted ; none.

acyclic_grounded_terms_to_string(Terms, {Indent, Length, Special}, String) :-
	acyclic_grounded_terms_to_string(Terms, Indent, Length, String,
					 Special
	).

procedure acyclic_grounded_terms_to_string(Any, Integer, Integer, String).

acyclic_grounded_terms_to_string(Terms, Indent, Length, String) :-
	acyclic_grounded_terms_to_string(Terms, Indent, Length, String,
						frozen
	).

acyclic_grounded_terms_to_string(Terms, Indent, Length, String, Special) :-
    Indent >= 0, Length > 0 :
      String = String'? |
	terms_to_tokens(Terms, Tokens, done, Done, Special),
	concatenate(Done, Tokens, Indent, Length, String').


terms_to_tokens(Terms, Tokens, Left, Right, Special) :-

    Terms ? Number, number(Number),
    convert_to_string(Number, ValueString) :
      Tokens ! ValueString |
	terms_to_tokens;

    Terms ? Term,
    otherwise |
	term_to_tokens({Left, Left'}, Term, {Tokens, Tokens'}, open, _,
			Special
	),
	terms_to_tokens;

    Terms = [] : Special = _,
      Tokens = [],
      Left = Right ;

    otherwise |
	term_to_tokens({Left, Right}, Terms, {Tokens, []}, open, _, Special).


concatenate(done, Tokens, Indent, Length, String) :-
	processor # link(execute(concatenate, {Tokens, String, Indent, Length}),
			 Reply
		    ),
	concatenate_reply(Reply, String).

concatenate_reply(true, _).
concatenate_reply(false, ' *** link#execute error *** '^).

/********************************************************************
The following code assumes that input is not self-referrant..
	Output is a list of strings and integers.
********************************************************************/

term_to_tokens(Control, Term, DTs, Context, Priority, Special) :-

    string(Term),
    Special =\= melted : Context = _ |
	string_or_module(Term, DTs, Priority, Control, Special);

    string(Term),
    Special = melted : Context = _,
      melt(Term, Term, []) |
	string_or_module(Term, DTs, Priority, Control, Special);

    otherwise,
    string(Term),
    Special = melted :
      melt(Term, Melted, Variables),
      Term' = 'MELTED'(Melted) |
	term_to_tokens,
	name_variables(Variables);

    number(Term),
    convert_to_string(Term, S) : Context =_, Special = _ |
	atomic_term(S, DTs, Priority, Control);

    list(Term) : Context = _,
      Priority = 0 |
	list_to_tokens(Control, Term, '[', DTs, Special);

    Term = [] : Context =_, Special = _ |
	atomic_term('[]', DTs, Priority, Control);

    channel(Term), arity(Term) =:= 1 : Context =_, Special = _ |
	atomic_term("<CHANNEL>", DTs, Priority, Control);

    vector(Term), arity(Term) > 1 : Context =_, Special = _ |
	atomic_term("<VECTOR>", DTs, Priority, Control);

    arg(1, Term, _) |	% tuple or vector
	which_fix(Term, Context, Fixity, Priority),
	start_tuple(Fixity, Control, DTs, Special);

    invalid(Term, Kind),
    string_to_dlist("INVALID[", Invalid, KL) : Context =_, Special = _,
      ascii(']', R) |
	string_to_dlist(Kind, KL, [R]),
	list_to_string(Invalid , Token),
	atomic_term(Token, DTs, Priority, Control);

    otherwise :				% Ease introduction of new types.
      Term = _, Context = _, Special = _ |
	atomic_term('??', DTs, Priority, Control).


string_or_module(String, DTs, Priority, Control, Special) :-

    module(String) : Special = _,
      DTs = {['MODULE' | Ts], Ts},
      Priority = 0,
      Control = {Done, Done} ;

    String = "" : Special = _,
      DTs = {Ts, Ts},
      Priority = 0,
      Control = {Done, Done} ;

    string_length(String) =:= 4, Special = hex,
    string_to_dlist(String, DL, []) :
      DTs = {[StringOut | Ts], Ts},
      Priority = 0 |
	hex_string(String, DL, StringOut, Control, Special);

    otherwise :
      DTs = {[StringOut | Ts], Ts},
      Priority = 0 |
	string(String, StringOut, Control, Special).

hex_string(String, DL, StringOut, Control, Special) :-

    DL = [C1, C2, C3, C4],
    ascii(' ') =< C1, C1 < ascii(del),
    ascii(' ') =< C2, C2 < ascii(del),
    ascii(' ') =< C3, C3 < ascii(del),
    ascii(' ') =< C4, C4 < ascii(del) |
	string(String, StringOut, Control, Special);

    otherwise |
	hex_string1(DL, String, DL, StringOut, Control, Special).

hex_string1(Chars, String, DL, StringOut, Control, Special) :-

    Chars ? C,
    ascii(' ') =< C, C < ascii(del) |
	self;

    Chars ? C,
    C =:= ascii(lf) |
	self;

    Chars = [] : DL = _ |
	string(String, StringOut, Control, Special);

    otherwise : Chars = _, String = _, Special = _,
      ascii('<', LB),
      ascii('>', RB) |
	convert_to_hex(DL, [LB | Hex], Hex, [RB], StringOut, Control).


string(String, StringOut, Control, Special) :-

    true : Special = _,
      melt(String, String, []),
      StringOut = String,
      Control = {Done, Done} ;

    Special =\= melted,
    otherwise : String = _,
      StringOut = 'FROZEN',
      Control = {Done, Done} ;

    Special = melted,
    otherwise : String = _,
      StringOut = 'FROZEN',
      Control = {Done, Done} |
	computation # display(term, failed - melt/3, prefix("[KERNEL]")).

convert_to_hex(DL, CL, Hex, Tail, StringOut, Control) :-

    DL ? C,
    C1 := (C /\ 255) / 16,
    C2 := C /\ 15 :
      Hex ! H1, Hex' ! H2 |
	convert_to_hex(C1, H1, CL, CL'),
	convert_to_hex(C2, H2, CL', CL''),
	self;

    DL = [] :
      Hex = Tail |
	list_to_string_control(CL, StringOut, Control).

convert_to_hex(C, H, L, R) :-

    C < 10,
    A := C + ascii('0') :
      H = A,
      L = R ;

    C >= 10,
    A := C - 10 + ascii('a') :
      H = A,
      L = R .

list_to_string_control(L, S, Control) :-
    list_to_string(L, S^) :
      Control = {Done, Done} .


name_variables(Variables) + (Index = 1) :-

    Variables ? Variable,
    convert_to_string(Index, String),
    string_to_dlist(String, List, [UnderScore]),
    Index' := Index + 1 :
      ascii('_', UnderScore) |
	list_to_string([UnderScore | List], Variable),
	name_variables;

    Variables = [] : Index = _ .


atomic_term(Token, {[Token | Ts], Ts}^, 0^, {Done, Done}^).


list_to_tokens(Control, Term, Prefix, DTs, Special) :-

    Term ? Car :
      Control = {L, R}, Control' = {M, R},
      DTs = {[Prefix | Ts], Ts2}, DTs' = {Ts1, Ts2},
      Prefix' = ', ' |
	term_to_tokens({L, M}, Car, {Ts, Ts1}, list, _, Special),
	list_to_tokens;

    Term =\= [], Term =\= [_|_] : Prefix = _,
      DTs = {[' | ' | Ts], Ts1} |
	term_to_tokens(Control, Term, {Ts, [']' | Ts1]}, list, _, Special).

list_to_tokens({Done, Done}^, [], _, {[']' | Ts], Ts}^, _Special).


start_tuple(Fixity, Control, DTs, Special) :-

    Fixity = infix(F, A, B, PLeft, PRight),
    known(F) :
      Control ={L, R},
      DTs = {Ts, Ts2} |
	term_to_tokens({L,  M1}, A, {TsA1, TsA2}, open, PA, Special),
	term_to_tokens({M1, M2}, B, {TsB1, TsB2}, open, PB, Special),
	infix_parentheses(PA?, PLeft, PRight, PB, Parentheses),
	infix_tuple({M2, R}, F, Parentheses,
		{TsA1, TsA2}, {TsB1, TsB2}, {Ts, Ts2}
	);

    Fixity = unfix(Tuple, F, N) :
      Control = {L, R},
      DTs = {Ts, Ts2} |
        bracket_tuple({L, M, N1}, N, F, {Ts, Ts1}, Close),
	N2 := N - N1,
	First := N2 + 1,
        tuple_to_tokens({M, R}, Tuple, First, N, {Ts1, [Close|Ts2]}, Special);

    Fixity = prefix(F, A, PRight) :
      Control = {L, R} |
	term_to_tokens({L, M}, A, {TsA1, TsA2}, open, PA, Special),
	prefix_tuple({M, R}, F, PRight, PA, {TsA1, TsA2}, DTs);

    Fixity = postfix(F, A, PLeft) :
      Control = {L, R} |
	term_to_tokens({L, M}, A, {TsA1, TsA2}, open, PA, Special),
	postfix_tuple({M, R}, F, PA, PLeft, {TsA1, TsA2}, DTs);

    Fixity = parenthesized(Tuple) :
      DTs = {['(' | Ts], Ts1},
      DTs' = {Ts, [')' | Ts1]} |
	which_fix(Tuple, open, Fixity', _),
	start_tuple.


which_fix(Tuple, Context, Fixity, Priority) :-

    Tuple = {F, A, B},
    string(F), string_to_dlist(F, Fs, [32]) |		% 32 =:= ascii(" ")
	infix_operator(F, PLeft, Pop, PRight, Result),
	infix_spacing(F, Fs, Result, Fsp),
	fixed_operation(Result, Context, {F,A,B},
			infix(Fsp, A, B, PLeft, PRight),
			Pop, Fixity, Priority
	);

    Tuple = {F,A},
    string(F), string_to_dlist(F, Fs, [32]) |	% 32 =:= ascii(' '),
	prefix_operator(F, Pop, PRight, Result),
	prefix_spacing(F, Fs, Result, Fsp),
	prefix_operation(Result, Context, {F,A}, prefix(Fsp, A, PRight),
			 Pop, Fixity, Priority
	);

    otherwise,
    arg(1, Tuple, F), N := arity(Tuple) : Context = _,
      Fixity = unfix(Tuple, F, N),
      Priority = 0 .


prefix_spacing(F, Fs, Result, Fsp) :-

    Result = true,
    Fs ? C,
    ascii(a) =< C, C =< ascii(z),	% Test for first character lower-case
    list_to_string(Fs, F') : F = _, Fs' = _,	% 32 =:= ascii(' ')
      Fsp = F' ;

    otherwise : Fs = _, Result = _,
      F = Fsp .


prefix_operation(Result, Context, Tuple, Prefix, Pop, Fixity, Priority) :-

    Result = true |
	fixed_operation(true, Context, Tuple, Prefix, Pop, Fixity, Priority);

    Result = false,
    Tuple = {F, A},
    string_to_dlist(F, Fn, []) : Prefix = _, Pop = _ |
	postfix_operator(F, PLeft, Pop', Result'),
	postfix_spacing(F, Fn, Result', Fsp),
	fixed_operation(Result', Context, Tuple, postfix(Fsp, A, Pop'), PLeft,
			Fixity, Priority
	).


postfix_spacing(F, Fn, Result, Fsp) :-

    Result = true,
    Fn ? C,
    ascii(a) =< C, C =< ascii(z),	% Test for first character lower-case
    list_to_string([32 | Fn], F') : F = _, Fn' = _,	% 32 =:= ascii(' ')
      Fsp = F' ;

    otherwise : Fn = _, Result = _,
      Fsp = F .


fixed_operation(true, open, _, Fixity, Priority, Fixity^, Priority^).
fixed_operation(true, list, Tuple, Fix, Pop, Fixity, Priority) :-
	parenthesize_in_list(Pop, Tuple, Fix, Fixity, Priority).
fixed_operation(false, _, Tuple, _, _, unfix(Tuple, F, N)^, 0^) :-
    arg(1,Tuple,F), N := arity(Tuple) |
	true.


parenthesize_in_list(Pop, Tuple, Fix, Fixity, Priority) :-
	infix_operator(',', _, _, Comma, true),	
	parenthesize_in_list(Pop, Comma, Tuple, Fix, Fixity, Priority).

parenthesize_in_list(Pop, Comma, Tuple, Fix, Fixity, Priority) :-

    Pop < Comma : Tuple = _,
      Fix = Fixity,
      Pop = Priority ;

    Pop >= Comma : Fix = _,
      Fixity = parenthesized(Tuple),
      Priority = 0 .


prefix_tuple(	{Done, Done}^,
		F, 
		PRight, 
		PA,
		{Ts, Ts2}, 
		{[F | Ts], Ts2}^
) :-
    PRight >= PA |
	true.
prefix_tuple(	{Done, Done}^,
		F, 
		PRight, 
		PA,
		{Ts, [')'|Ts2]}^,
		{[F, '(' | Ts], Ts2}^
) :-
    PRight < PA |
	true.


postfix_tuple(	{Done, Done}^,
		F, 
		0,
		_, 
		{Ts, [F | Ts2]}^, 
		{Ts, Ts2}
).
postfix_tuple(	{Done, Done}^,
		F, 
		PA,
		PLeft, 
		{Ts, [F | Ts2]}^, 
		{Ts, Ts2}
) :-
    PLeft >= PA |
	true.
postfix_tuple(	{Done, Done}^,
		F, 
		PA,
		PLeft,
		{Ts, [')', F | Ts2]}^,
		{['(' | Ts], Ts2}^
) :-
    PA =\= 0,
    PLeft < PA |
	true.


infix_spacing(F, Fs, Result, Fsp) :-

    Result = false : Fs = _,
      F = Fsp ;

    Result = true,
    F =\= ',', F =\= ';',
    list_to_string([32 | Fs], F') :		% 32 =:= ascii(' ')
      Fsp = F' .

infix_spacing(',', _, true, ', '^).
infix_spacing(';', _, true, '; '^).


infix_parentheses(PA, PLeft, PRight, PB, Parentheses) :-

    PLeft >= PA, PRight >= PB :
      Parentheses = neither ;

    PLeft < PA, PRight >= PB :
      Parentheses = left ;

    PLeft >= PA, PRight < PB :
      Parentheses = right ;

    PLeft < PA, PRight < PB :
      Parentheses = both .


infix_tuple(	{Done, Done}^,
		F,
		neither,
		{Ts, [F | Ts1]}^, 
		{Ts1, Ts2}, 
		{Ts, Ts2}^
) .
infix_tuple(	{Done, Done}^,
		F, 	
		left, 	
		{Ts, [')', F | Ts1]}^,
		{Ts1, Ts2}, 
		{['(' | Ts], Ts2}^
) .
infix_tuple(	{Done, Done}^,
		F, 	
		right, 	
		{Ts, [F, '(' | Ts1]}^, 
		{Ts1, [')' | Ts2]}^, 
		{Ts, Ts2}
) .
infix_tuple(	{Done, Done}^,
		F, 	
		both,
		{Ts, [')', F, '(' | Ts1]}^, 
		{Ts1, [')' | Ts2]}^, 
		{['(' | Ts], Ts2}^
) .


bracket_tuple(Control, N, F, DTs, Suffix) :-

    module(F) |
	general_tuple(Control, N, DTs, Suffix);

    string(F),
    otherwise,
    string_to_dlist(F, Flp, Llp) :
      ascii('(', Lp), Llp = [Lp] |
	functional_tuple(Control, N, F, Flp, DTs, Suffix);

    otherwise : F = _ |
	general_tuple(Control, N, DTs, Suffix).

functional_tuple(Control, N, F, Flp, DTs, Suffix) :-

    N > 1,
    Flp = [C1, C2, C3, C4, _RP],
    ascii(' ') =< C1, C1 < ascii(del),
    ascii(' ') =< C2, C2 < ascii(del),
    ascii(' ') =< C3, C3 < ascii(del),
    ascii(' ') =< C4, C4 < ascii(del),
    list_to_string(Flp, Funct),
    N1 := N - 1 :
      melt(F, F, []),
      Control = {Done, Done, N1},
      DTs = {[Funct | Ts], Ts},
      Suffix = ')' ;

    N > 1,
    Flp =\= [_,_,_,_,_],
    list_to_string(Flp, Funct),
    N1 := N - 1 :
      melt(F, F, []),
      Control = {Done, Done, N1},
      DTs = {[Funct | Ts], Ts},
      Suffix = ')' ;

    otherwise : F = _, Flp = _ |
	general_tuple(Control, N, DTs, Suffix).


general_tuple({Done, Done, N}^, N, {['{'|Ts], Ts}^, '}'^).


tuple_to_tokens(Control, Tuple, M, N, DTs, Special) :-

    M < N, arg(M, Tuple, A),
    M1 := M + 1,
    Control = {L, R},
    DTs = {Ts, Ts2} |
	term_to_tokens({L, Middle}, A , {Ts, [', ' | Ts1]}, list, _, Special),
	tuple_to_tokens({Middle, R}, Tuple, M1, N, {Ts1, Ts2}, Special);

    M = N,
    arg(N, Tuple, A) |
	term_to_tokens(Control, A, DTs, list, _, Special).


% operators - copied from parser * * * T E M P O R A R Y * * * 

infix_operator(Op, PLeft, Pop, PRight, Res) :-
    N := string_length(Op), N < 4 |
	infix_operator(N, Op, PLeft, Pop, PRight, Res).
infix_operator( _, _, _, _, false^) :-
    otherwise |
	true.

infix_operator(0, _, _, _, _, false^).
infix_operator(Length, Op, PLeft, Pop, PRight, Reply) :-

    Length = 1 |
	infix_operator_1(Op, PLeft, Pop, PRight, Reply);

    Length = 2 |
	infix_operator_2(Op, PLeft, Pop, PRight, Reply);

    Length = 3 |
	infix_operator_3(Op, PLeft, Pop, PRight, Reply).


infix_operator_1( ';' ,1069^, 1070^, 1070^, true^).
infix_operator_1( '|' ,1059^, 1060^, 1060^, true^).
infix_operator_1( ':' ,1049^, 1050^, 1049^, true^).
infix_operator_1( '&' , 999^, 1000^, 1000^, true^).	 
infix_operator_1( ',' , 999^, 1000^, 1000^, true^).	 
infix_operator_1( '!' , 899^,  900^,  900^, true^).	 
infix_operator_1( '?' , 900^,  900^,  899^, true^).	 
infix_operator_1( '=' , 799^,  800^,  799^, true^).	 
infix_operator_1( '@' , 720^,  720^,  719^, true^).
infix_operator_1( '#' , 709^,  710^,  710^, true^).	 
infix_operator_1( '<' , 699^,  700^,  699^, true^).	 
infix_operator_1( '>' , 699^,  700^,  699^, true^).	 
infix_operator_1( '+' , 500^,  500^,  499^, true^).
infix_operator_1( '-' , 500^,  500^,  499^, true^).	 
infix_operator_1( '*' , 400^,  400^,  399^, true^).	 
infix_operator_1( '/' , 400^,  400^,  399^, true^).	 
infix_operator_1( '\' , 299^,  300^,  299^, true^).	 
infix_operator_1(  _  ,  _ ,   _ ,   _ , false^) :-
    otherwise |
	true.

infix_operator_2( ':-' ,1199^, 1200^, 1199^, true^).
infix_operator_2( '<-' ,1189^, 1190^, 1189^, true^).
infix_operator_2( '=>' ,1009^, 1010^, 1009^, true^).
infix_operator_2( '@<' , 799^,  800^,  799^, true^).	 
infix_operator_2( ':=' , 699^,  700^,  699^, true^).	 
infix_operator_2( '+=' , 699^,  700^,  699^, true^).	 
infix_operator_2( '-=' , 699^,  700^,  699^, true^).	 
infix_operator_2( '==' , 699^,  700^,  699^, true^).	 
infix_operator_2( '#<' , 699^,  700^,  699^, true^).	 
infix_operator_2( '\=' , 699^,  700^,  699^, true^).
infix_operator_2( '=<' , 699^,  700^,  699^, true^).	 
infix_operator_2( '>=' , 699^,  700^,  699^, true^).	 
infix_operator_2( '\/' , 250^,  250^,  249^, true^).
infix_operator_2( '/\' , 240^,  240^,  239^, true^).
infix_operator_2(   _  ,  _ ,   _ ,   _ , false^) :-
    otherwise |
	true.

infix_operator_3( '::=',1199^,1200^,1199^, true^).	 
infix_operator_3( '<=>',1009^,1010^,1009^, true^).
infix_operator_3( '=?=' ,799^, 800^, 799^, true^).	 
infix_operator_3( '=\=' ,799^, 800^, 799^, true^).
infix_operator_3( '=:=' ,699^, 700^, 699^, true^).	
infix_operator_3(  div  ,400^, 400^, 399^, true^).
infix_operator_3(  mod  ,299^, 300^, 299^, true^).
infix_operator_3(   _   , _ ,  _ ,  _ , false^) :-
    otherwise |
	true.

prefix_operator( '#' ,710^, 710^, true^).
prefix_operator( '+' ,220^, 219^, true^).
prefix_operator( '-' ,220^, 219^, true^).
prefix_operator( '~' ,220^, 219^, true^).
prefix_operator( '`' ,210^, 210^, true^).
prefix_operator( '?' ,210^, 209^, true^).
prefix_operator( 'procedure', 100^, 99^, true^).
prefix_operator( _, _, _, false^) :-
    otherwise |
	true.

postfix_operator( '!', 99^, 100^, true^).
postfix_operator( '^', 205^, 204^, true^).
postfix_operator( '??', 200^, 199^, true^).
postfix_operator( '++', 200^, 199^, true^).
postfix_operator( '--', 200^, 199^, true^).
postfix_operator(  _, _, _, false^) :-
    otherwise |
	true.
