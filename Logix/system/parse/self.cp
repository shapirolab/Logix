/*

FCP parser
Michael Hirsch, Marc Rosen, Muli Safra, Bill Silverman

Last update by		$Author: bill $
		       	$Date: 2000/05/03 10:41:37 $
Currently locked by 	$Locker:  $
			$Revision: 1.3 $
			$Source: /home/qiana/Repository/Logix/system/parse/self.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([string/3, characters/3, tokens/3, tokenize/2]).
-mode(trust).
-language(compound).

List ::= [Any].

procedure string(String, List, [String]).

Errors ::= [String].

string(String, Terms, Errors) :-
    string_to_dlist(String, Characters, []) |
	characters(Characters, Terms, Errors).

procedure characters([Integer], List, Errors).

characters(Characters, Terms, Errors) :-
	tokenize # characters(Characters, Tokens),
	parser(Tokens, Terms, Errors).

procedure tokens(tokenize # Tokens, List, Errors).

tokens(Tokens, Terms, Errors) :-
	parser(Tokens, Terms, Errors).

% Relay operations

procedure tokenize((String ; [Integer]), tokenize # Tokens).

tokenize(Operand, Tokens) :-

    string(Operand),
    string_to_dlist(Operand, Chars, []) |
	tokenize # characters(Chars, Tokens);

    otherwise |
	tokenize # characters(Operand, Tokens).

% Parse  Tokens, producing Terms and Es (errors), using Os (syntax stack).

parser(Tokens, Terms, Es) + (Os = []) :-
    list(Tokens) |
        term(Tokens, Rest, Term, 1200, '.', Result, Os,Os1),
	if_error(Result, Rest,Tokens', Tokens, Es,Es', Term,
			Terms,Terms', Os1,Os'
	),
	parser.
parser([], []^, []^, []).
parser([], []^, []^, {[]^}).


if_error(Result, Rest1,Rest2, Tokens, Errors1,Errors2, Term,
			Terms1,Terms2, Os1,Os2
) :-

    Result = ok,
    Rest1 ? '.' : Tokens = _,
      Rest1' = Rest2,
      Errors1 = Errors2 |
	push_syntax(Term, Terms1, Terms2, Os1,Os2);

    Result = ok,
    Rest1 = [] : Tokens = _,
      Rest1 = Rest2,
      Errors1 = Errors2 |
	push_syntax(Term, Terms1, Terms2, Os1,Os2);

    Result = ok,
    Rest1 =\= ['.' | _ ], Rest1 =\= [] :
      Result' = incomplete_term |
	if_error;

    Result =\= ok : Term = _,
      Terms1 = Terms2,
      Errors1 = [Text? | Errors2?],
      Os1 = Os2 |
	error # prepare(Result, Rest1,Rest2, Tokens, Text).


push_syntax(Term, Terms1, Terms2, Os1,Os2) :-

    Term = -syntax(Id) :
      In = [prefix_operator(a/0, _, _, A) | In'],
      Terms1 = Terms2,
      Serve = Id # serve(In, Out) |
	computation_utils # call_list([Serve], Reply),
	push_syntax1(Reply, A, Out, Out', retry(Serve, In')),
	push_syntax2(In', Out', Os1,Os2);

    otherwise :
      Terms1 = [Term? | Terms2?],
      Os1 = Os2 |
	true.


push_syntax1(Reply, A, Out1, Out2, Continue) :-

    Out1 ? _ : Reply = _, A = _, Continue = _,
      Out1' = Out2 |
	true;

    A = true : Reply = _, Continue = _,
      Out1 = Out2 |
	true;

    Reply = false(Reason),
    Continue = retry(Serve, In) :
      Continue' = dont(Serve, In, Reason) |
	self # service_id(SId),
	computation_utils # call_list([SId # syntax # Serve], Reply'),
	push_syntax1;

    Reply = false(_),
    Continue = dont(Id#_, In, Reason) : A = _, Out1 = _,
      In = Out2 |
	computation # failed(Id # serve(_,_), Reason).

push_syntax2(In, Out, Os1, Os2) :-

    Os1 = [] :
      Os2 = {In} |
	serve_operators(Out);

    Os1 = {Out^} :
      Os2 = {In} |
	true.


term(Tokens, Rest, Term, Priority, Delimiter, Result, Os1,Os2) :-

    Tokens ? Token,
    string(Token) |
	prefix_term(Token, Tokens', Priority, Term0, Priority', Tokens'',
			Result0, Os1,Os1'
	),
	after_term(Result0, Delimiter, Priority, Term0, Priority', Tokens'',
			Term, Rest, Result, Os1',Os2
	);

    Tokens ? Token,
    tuple(Token) |
	tuple_term(Token, Delimiter, Tokens',
			Term0, Tokens'', Result0, Os1,Os1'
	),
	after_term(Result0, Delimiter, Priority, Term0, 0, Tokens'',
			Term, Rest, Result, Os1',Os2
	);

    Tokens ? Token,
    otherwise |				% Number (pass [] or list as token)
	after_term(ok, Delimiter, Priority, Token, 0, Tokens',
			Term, Rest, Result, Os1,Os2
	).

term([], []^, _, _, _, eof_encountered^, Os,Os^).


prefix_term('[', [']' | Tokens], _, []^, 0^, Tokens^, ok^, Os,Os^).
prefix_term(String, Tokens, Priority1, Term, Priority2, Rest,
			Result, Os1,Os2
) :-

    String = '[',
    otherwise : Priority1 = _,
      Priority2 = 0 |
	rlist(ok, [',' | Tokens], Rest, Term, Result, Os1,Os2);

    String = '(' : Priority1 = _,
      Priority2 = 0 |
	term(Tokens, Tokens', Term, 1200, '.', Result0, Os1,Os2),
	close_parenthesis(Tokens', Rest, Result0, Result);

    String = '{' : Priority1 = _,
      Priority2 = 0 |
	build_tuple('}', ',', 999, Tokens, Rest, Term, _Arg0, 0,
			Result, Os1,Os2
	);
/*
    String = '<<' : Priority1 = _,
      Priority2 = 0 |
	build_tuple('>>', '.', 1200, Tokens, Rest, Term, _Arg0, 0,
			Result, Os1,Os2
	);
*/
    String = '<<' : Priority1 = _,
      Priority2 = 0 |
	build_list(ok, ['.' | Tokens], Rest, Term, Result, Os1,Os2);

    otherwise,			% kluge to reduce look-ups
    string_length(String) > 2, String =\= 'procedure',
    Os1 = [] : Priority1 = _,
      String = Term,
      Tokens = Rest,
      Priority2 = 0,
      Result = ok,
      Os2 = [] |
	true;

    otherwise,
    Os1 = [] :
      Os2 = [] |
	prefix_operator(String, Pop, PRight, Reply),
	try_prefix(Reply, String, Tokens, Priority1, Term ,Priority2, Rest,
			Pop, PRight, Result, [],[]
	);

    otherwise,
    Os1 = {OS} :
      OS ! prefix_operator(String, Pop, PRight, Reply) |
	try_prefix(Reply, String, Tokens, Priority1, Term ,Priority2, Rest,
			Pop, PRight, Result, {OS'},Os2
	).


tuple_term(Tuple, Delimiter, Tokens, Term, Rest, Result, Os1,Os2) :-

    Tuple = funct(Functor, _),
    Tokens = ['?', '(' | Tokens''] : Delimiter = _ |
	build_tuple(')', ',', 999, Tokens'', Rest, Term, Functor, 1,
			Result, Os1,Os2
	);

    Tuple = funct(Functor,_),
    Tokens ? '(' : Delimiter = _ |
	build_tuple(')', ',', 999, Tokens', Rest, Term, Functor, 1,
			Result, Os1,Os2
	);

    Tuple = funct(Functor, _),
    Tokens = ['??', '(' | Tokens''] : Delimiter = _ |
	build_tuple(')', ',', 999, Tokens'', Rest, Term, {'??', Functor}, 1,
			Result, Os1,Os2
	);

    Tuple = ?Name,
    string(Name),
    Tokens ? '?' :
      Result = ok |
	look_ahead(Delimiter, Tokens', operator, NextType, Os1,Os2),
	read_only(NextType, Name, Term, Tokens', Rest);

    Tuple = ?Name,
    string(Name),
    Tokens =\= ['?' | _] : Delimiter = _,
      Term = `Name,
      Tokens = Rest,
      Result = ok,
      Os1 = Os2 |
	true;

    Tuple = `Name,
    string(Name) : Delimiter = _,
      Tuple = Term,
      Tokens = Rest,
      Result = ok,
      Os1 = Os2 |
	true;

    Tuple = string(String, _) : Delimiter = _,
      String = Term,
      Tokens = Rest,
      Result = ok,
      Os1 = Os2 |
	true;

    Tuple  = overflow(Digits) : Delimiter = _,
      Digits = Term,
      Tokens = Rest,
      Result = overflow,
      Os1 = Os2 |
	true;

    otherwise : Tuple = _, Term = _, Delimiter = _,
      Result = invalid_token,
      Tokens = Rest,
      Os1 = Os2 |
	true.


% kluge to discriminate suffix "?" from infix "?" when followed by string.

read_only(NextType, Name, Term, Tokens, Rest) :-

    NextType =\= operand :
      Term = ?Name,
      Tokens = Rest |
	true;

    otherwise : NextType = _,
      Term = `Name,
      Rest = ['?' | Tokens] |
	true.

close_parenthesis([')' | Rest], Rest^, Result, Result^).
close_parenthesis(Tokens, Tokens^, _, missing_right_parenthesis^) :-
    otherwise |
	true.


try_prefix(false, String, Tokens, _, String^, 0^, Tokens^, _, _, ok^, Os,Os^).
try_prefix(Diagnostic, String, Tokens, _, ''^, 0^, [String | Tokens]^,
		_, _, Diagnostic^, Os,Os^
) :-
    Diagnostic =\= true,
    Diagnostic =\= false |
	true.
try_prefix(true, '+', [Number | Tokens], _, Number^, 0^, Tokens^,
			_, _, ok^, Os,Os^
) :-
    number(Number) |
	true.
try_prefix(true, '-', [Number | Tokens], _, Negated, 0^, Tokens^,
			_, _, ok^, Os,Os^
) :-
    number(Number),
    Negated^ := - Number |
	true.
try_prefix(Reply, String, Tokens, Priority1, Term ,Priority2, Rest,
			Pop, PRight, Result, Os1,Os2
) :-

    otherwise,
    Reply = true,
    Pop =< Priority1 :
      Priority2 = Pop,
      Term = {String, Term'?} |
	look_ahead('.', Tokens, operand, NextType, Os1,Os1'),
	prefixed_operand(NextType, Tokens, Rest, Term', PRight, Result,
				Os1',Os2
	);

    otherwise : Reply = _, Priority1 = _, Pop = _, PRight = _,
      String = Term,
      Tokens = Rest,
      Priority2 = 0,
      Result = ok,
      Os1 = Os2 |
	true.

prefixed_operand(NextType, Tokens, Rest, Term, PRight, Result, Os1,Os2) :-

    NextType =\= operator, NextType =\= delimiter, NextType =\= sof |
	term(Tokens, Rest, Term, PRight, '.', Result, Os1,Os2);

    otherwise : NextType = _, PRight = _,
      Tokens = Rest,
      Term = '',
      Os1 = Os2 |
	diagnose_conflict(NextType, Result).

diagnose_conflict(operator, adjacent_operators^).
diagnose_conflict(_, missing_operand^) :-
    otherwise |
	true.

after_term(Result1, Delimiter, Priority1, Term1, Priority2, Tokens,
		Term2, Rest, Result2, Os1,Os2
) :-

    Result1 = ok,
    Tokens ? String,
    string(String),
    String =\= Delimiter,
    N := string_length(String), N < 4,
    Os1 = [] :
      Os2 = [] |
	infix_operator(N, String, PLeft, Pop, PRight, Reply),
	try_infix(Reply, String, Tokens', Rest, Term1, Term2, Priority1,
			Priority2, PLeft, Pop, PRight, Result2, [],[]
	);

    Result1 = ok,
    Tokens ? String,
    string(String),
    String =\= Delimiter,
    Os1 = {OS} :
      OS ! infix_operator(String, PLeft, Pop, PRight, Reply) |
	try_infix(Reply, String, Tokens', Rest, Term1, Term2, Priority1,
			Priority2, PLeft, Pop, PRight, Result2, {OS'},Os2
	);

    otherwise : Delimiter = _, Priority1 = _, Priority2 = _,
      Result1 = Result2,
      Term1 = Term2,
      Tokens = Rest,
      Os1 = Os2 |
	true.


try_infix(_, ')', Tokens, [')' | Tokens]^, Term, Term^, _, _, _, _, _,
		ok^, Os,Os^
).
try_infix(_, ']', Tokens, [']' | Tokens]^, Term, Term^, _, _, _, _, _,
		ok^, Os,Os^
).
try_infix(_, '}', Tokens, ['}' | Tokens]^, Term, Term^, _, _, _, _, _,
		ok^, Os,Os^
).
try_infix(_, '>>', Tokens, ['>>' | Tokens]^, Term, Term^, _, _, _, _, _,
		ok^, Os,Os^
).
try_infix(_, '.', Tokens, ['.' | Tokens]^, Term, Term^, _, _, _, _, _,
		ok^, Os,Os^
).
try_infix(Reply, String, Tokens, Rest, Term1, Term, Priority1,
		Priority2, PLeft, Pop, PRight, Result, Os1,Os2
) :-
    Pop =< Priority1, Priority2 =< PLeft : Reply = _ |
	look_ahead('.', Tokens, operand, NextType, Os1,Os1'),
	infixed_operand(NextType, String, Tokens, Rest, Term1, Term, Priority1,
			Priority2, Pop, PRight, Result, Os1',Os2
	);

    Reply = false,
    Os1 = [] : Reply = _, PLeft = _, Pop = _, PRight = _,
      Os2 = [] |
	postfix_operator(String, PLeft', Pop', Reply'),
	try_postfix(Reply', String, Tokens, Rest, Term1, Term, Priority1,
			Priority2, Pop', PLeft', Result, [],[]
	);

    Reply = false,
    Os1 = {OS} : Reply = _, PLeft = _, Pop = _, PRight = _,
      OS ! postfix_operator(String, PLeft', Pop', Reply') |
	try_postfix(Reply', String, Tokens, Rest, Term1, Term, Priority1,
			Priority2, PLeft', Pop', Result, {OS'},Os2
	);

     otherwise : Reply = _,
       Reply' = false |
	 try_infix.

infixed_operand(NextType, String, Tokens, Rest, Term1, Term, Priority1,
		Priority2, Pop, PRight, Result, Os1,Os2
) :-

    NextType = operand : Priority2 = _ |
	term(Tokens, Tokens', Term2, PRight, '.', Result0, Os1,Os1'),
	after_term(Result0, '.', Priority1, {String, Term1?, Term2?}, Pop,
			Tokens', Term, Rest, Result, Os1',Os2
	);

    otherwise : NextType = _, Pop = _, PRight = _ |
	try_infix(false, String, Tokens, Rest, Term1, Term, Priority1,
			Priority2, _, _, _, Result, Os1,Os2
	).

try_postfix(Reply, String, Tokens, Rest, Operand, Term, Priority1,
		Priority2, PLeft, Pop, Result, Os1,Os2
) :-
    Pop =< Priority1 , Priority2 =< PLeft : Reply = _ |
	after_term(ok, '.', Priority1, {String, Operand?}, Pop, Tokens,
			Term, Rest, Result, Os1,Os2
	).
try_postfix(false, String, Tokens, [String | Tokens]^, Term, Term^,
		_, _, _, _, ok^, Os,Os^
).
try_postfix(_, String, Tokens, [String | Tokens]^, Term, Term^,
		_, _, _, _, ok^, Os,Os^
) :-
    otherwise |
	true.

/*
** Inspect the next token and return its type.
**
** Input   Kind  in [operand, operator]
**
**	   For  Kind = operand , accept prefix operator as operand.
**	   For  Kind = operator , accept infix/suffix operator as operator.
**
** Output  NextType  in  [operand, operator, delimiter, sof]
**
*/

look_ahead(Delimiter, Tokens, Kind, NextType, Os1,Os2) :-

    Tokens = [String | _],
    string(String),
    String =\= Delimiter, String =\= ')', String =\= ']', String =\= '}',
    String =\= '>>', String =\= '.' |
	look_ahead1(String, Kind, NextType, Os1,Os2);

    Tokens = [String | _],
    string(String),
    otherwise : Delimiter = _, Kind = _,
      NextType = delimiter,
      Os1 = Os2 |
	true;

    Tokens =\= [_ | _] : Delimiter = _, Kind = _,
      NextType = sof,
      Os1 = Os2 |
	true;

    otherwise : Delimiter = _, Kind = _, Tokens = _,
      NextType = operand,
      Os1 = Os2 |
	true.

look_ahead1(String, Kind, NextType, Os1,Os2) :-

    Kind = operand,
    Os1 = [],
    SL := string_length(String) :	% kluge to reduce lookups (look_ahead2)
      Os2 = [] |
	prefix_operator(String, _, _, Prefix),
	look_ahead2(Prefix, SL, String, NextType);

    Kind = operator,
    Os1 = [] :
      Os2 = [] |
	lookup(infix_operator(String, _, _, _, Infix)),
	postfix_operator(String, _, _, Postfix),
	look_ahead4(Infix, Postfix, NextType);

    Kind = operand,
    Os1 = {OS1} :
      OS1 ! prefix_operator(String, _, _, Prefix) |
	look_ahead3(Prefix, String, NextType, OS1', Os2);

    Kind = operator,
    Os1 = {OS1} :
      OS1 = [infix_operator(String, _, _, _, Infix),
	     postfix_operator(String, _, _, Postfix) | OS1'''],
      Os2 = {OS1'''} |
	look_ahead4(Infix, Postfix, NextType).

look_ahead2(Prefix, SL, String, NextType) :-

    Prefix = false,
    SL = 1 |
	infix_operator_1(String, _, _, _, Infix),
	postfix_operator(String, _, _, Postfix),
	look_ahead4(Infix, Postfix, NextType);

    Prefix = false,
    SL = 2 |
	infix_operator_2(String, _, _, _, Infix),
	look_ahead4(Infix, false, NextType);

    Prefix = false,
    SL = 3 |
	infix_operator_3(String, _, _, _, Infix),
	look_ahead4(Infix, false, NextType);

    otherwise : Prefix = _, SL = _, String = _,
      NextType = operand |
	true.

look_ahead3(Prefix, String, NextType, OS, Os) :-

    Prefix = true : String = _,
      NextType = operand,
      Os = {OS} |
	true;

    otherwise : Prefix = _,
      OS = [infix_operator(String, _, _, _, Infix),
	    postfix_operator(String, _, _, Postfix) | OS''],
      Os = {OS''} |
	look_ahead4(Infix, Postfix, NextType).

look_ahead4(false, false, operand^).
look_ahead4(_, _, operator^) :-
    otherwise |
	true.

    

build_tuple(Right, Delimiter, Limit, Tokens, Rest, Tuple, Arg, N,
			Result, Os1,Os2
) :-

    Tokens ? Right : Delimiter = _, Limit = _,
      Tokens' = Rest, 
      Result = missing_argument,
      Os1 = Os2 |
	start_tuple(N, Tuple ,Arg);

    Tokens ? Delimiter : Right = _, Limit = _,
      Tokens' = Rest, 
      Result = missing_argument,
      Os1 = Os2 |
	start_tuple(N, Tuple ,Arg);

    Tokens = [Arg', Delimiter | Tokens''],
    Arg' = `Name,
    string(Name),					% Optimise common case
    N' := N + 1 |
	tuple_argument(N, Tuple', Arg, Tuple),
	build_tuple;

    Tokens = [`Name, Right | Tokens''],
    string(Name),					% Optimise common case
    N' := N + 1 : Delimiter = _, Limit = _,
      Tokens'' = Rest,
      Os1 = Os2,
      Result = ok |
	tuple_argument(N, Tuple', Arg, Tuple),
	start_tuple(N', Tuple', `Name);

    otherwise,
    N' := N + 1 |
	tuple_argument(N, Tuple', Arg, Tuple),
% Use Delimiter (instead of Right) to stop term, on the assumption that tuples
% have many arguments.
	term(Tokens, Tokens', Arg', Limit, Delimiter, Result0, Os1,Os1'),
	build_tuple1(Right, Delimiter, Limit, Tokens', Rest, Tuple',
			Arg', N', Result, Os1',Os2, Result0
	).


build_tuple1(Right, Delimiter, Limit, Tokens, Rest, Tuple,
		Arg, N, Result2, Os1,Os2, Result1
) :-

    Result1 = ok,
    Tokens ? Delimiter |
	build_tuple(Right, Delimiter, Limit, Tokens', Rest, Tuple,
			Arg, N, Result2, Os1,Os2
	);

    Result1 = ok,
    Tokens ? Right : Delimiter = _, Limit = _,
      Tokens' = Rest,
      Result2 = ok,
      Os1 = Os2 |
	start_tuple(N, Tuple, Arg).

build_tuple1(_, _, _, Rest, Rest^, Tuple, Arg, N, term_error^, Os,Os^, _) :-
    otherwise |
	start_tuple(N, Tuple, Arg).


start_tuple(N, Tuple, Arg) :-
    make_tuple(N, T),
    arg(N, T, A) :
      A = Arg?,
      T = Tuple |
	true.
start_tuple(0, []^, _).


tuple_argument(N, Tuple1, Arg, Tuple2) :-

    arg(N, Tuple1, A) :
      A = Arg?,
      Tuple1 = Tuple2 |
	true;

    N = 0 : Arg = _,
      Tuple1 = Tuple2 |
	true.


rlist(Result1, Tokens, Rest, Term, Result2, Os1,Os2) :-

    Result1 = ok,
    Tokens ? ',' :
      Term = [Car? | Term'?] |
% Use '|' (instead of ',' or ']') to stop term, on the assumption that lists
% are usually cdr'ed.
	term(Tokens', Tokens'', Car, 999, '|', Result1', Os1,Os1'),
	rlist;

    Result1 = ok,
    Tokens = [']' | Rest^] :
      Term = [],
      Result2 = ok,
      Os1 = Os2 |
	true;

    Result1 = ok,
    Tokens = ['|', Variable, ']' | Rest^],
    Variable = `Name,
    string(Name) :					% Optimise common case
      Term = Variable,
      Result2 = ok,
      Os1 = Os2 |
	true;

    otherwise,
    Result1 = ok,
    Tokens ? '|' |
	term(Tokens', Tokens'', Term, 1200, ']', Result1', Os1,Os2),
	rest_nil(Result1', Tokens'', Rest, Result2).

rlist(_, Tokens, Tokens^, _, list_error^, Os,Os^) :-
    otherwise |
	true.

rest_nil(ok, [']' | Rest], Rest^, ok^).
rest_nil(_, Tokens, Tokens^, term_error^) :-
    otherwise |
	true.


build_list(Result1, Tokens, Rest, Term, Result2, Os1,Os2) :-

    Result1 = ok,
    Tokens ? '.' :
      Term = [Car? | Term'?] |
	term(Tokens', Tokens'', Car, 1200, '.', Result1', Os1,Os1'),
	build_list;

    Result1 = ok,
    Tokens = ['>>' | Rest^] :
      Term = [],
      Result2 = ok,
      Os1 = Os2 |
	true.

build_list(_, Tokens, Tokens^, _, list_error^, Os,Os^) :-
    otherwise |
	true.


serve_operators(In) :-

    In ? Request |
	lookup(Request),
	serve_operators;

    In = [] |
	true.

/************* E m b e d d e d   O p e r a t o r   L o o k u p ***************/

% When adding operators, it may be necessary to adjust "% kluge"s scattered
% around the parser, relating to the length of the operator string. E.g. to
% add the prefix operator ":->" you must change 2 to 3 in prefix_term/9
% above, and 3 to 4 in lookup/1, below (or add another counter-kluge).

lookup(prefix_operator(A, B, C, D)) :-
    string_length(A) < 3 |			% kluge for speed
	prefix_operator(A, B, C, D).
lookup(prefix_operator('procedure', 100^, 99^, true^)).	% counter-kluge
lookup(prefix_operator(_, _, _, false^)) :-
    otherwise |
	true.
lookup(infix_operator(A, B, C, D, E)) :-
    N := string_length(A),			% kluge for speed
    N < 4 |
	infix_operator(N, A, B, C, D, E).
lookup(infix_operator(_, _, _, _, false^)) :-
    otherwise |
	true.
lookup(postfix_operator(A, B, C, D)) :-
    string_length(A) < 3 |			% kluge for speed
	postfix_operator(A, B, C, D).
lookup(postfix_operator(_, _, _, false^)) :-
    otherwise |
	true.
	
/************************** operators ****************************************/

infix_operator(Length, Op, PLeft, Pop, PRight, Reply) :-

    Length = 1 |
	infix_operator_1(Op, PLeft, Pop, PRight, Reply);

    Length = 2 |
	infix_operator_2(Op, PLeft, Pop, PRight, Reply);

    Length = 3 |
	infix_operator_3(Op, PLeft, Pop, PRight, Reply).

infix_operator(0, _, _, _, _, false^).

procedure infix_operator_1(Op, PLeft, Pop, PRight, Reply).

infix_operator_1( ';' ,1069^, 1070^, 1070^, true^).
infix_operator_1( '|' ,1059^, 1060^, 1060^, true^).
infix_operator_1( ':' ,1049^, 1050^, 1049^, true^).
infix_operator_1( '&' , 989^,  990^,  990^, true^).	 
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
prefix_operator( ')' ,_ , _, excess_right_parenthesis^).
prefix_operator( '}' ,_ , _, excess_right_brace^).
prefix_operator( ']' ,_ , _, excess_right_bracket^).
prefix_operator( '.' ,_ , _, missing_term^).
prefix_operator( '>>' ,_ , _, "excess '>>'"^).
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
