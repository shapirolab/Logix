/*

Utilities

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:02:52 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/utils.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([	append_strings/2, chars_to_lines/2, evaluate/2,
		ground/2, ground_stream/2, unparse/2,
		binary_merge/2,binary_sort_merge/2,
		binary_merge/3,binary_sort_merge/3,
		tuple_to_dlist/3, list_to_tuple/2,
		integer_to_dlist/3, list_length/2,
		freeze_term/3
	]
).
-mode(interrupt).
-language(compound).

send(Utility, Done, Value, Result) :-

    known(Done) : Utility = _,
      Result = Value ;

    otherwise : Done = _ |
	fail(Utility, Value =\= Result).

procedure chars_to_lines([Integer], [String]).

chars_to_lines(Chars, Lines) :-
	chars_to_lines(Chars, Lines1, Done),
	send(chars_to_lines, Done, Lines1, Lines).

chars_to_lines(Chars, Lines, Done) :-

    Chars ? C |
	chars_to_lines(C, Chars', Lines, Done);

    Chars = [] :
      Lines = [],
      Done = done ;

    otherwise :
      Lines = [],
      Done = done |
	fail(chars_to_lines, not_a_list(Chars)).

chars_to_lines(Char, Chars, Lines, Done) :-

    Char =:= ascii(cr) |
	chars_to_lines(Chars, Lines, Done);

    Char =:= ascii(lf) |
	chars_to_lines(Chars, Lines, Done);

    otherwise :
      Lines ! String? |
	one_line(Char, Chars, List, Chars', List, ClosedList),
	list_to_string(ClosedList, String),
	chars_to_lines(Chars', Lines', Done).

one_line(Chars, List, Chars1, OpenList, ClosedList) :-

    Chars ? Char |
	one_line(Char, Chars', List, Chars1, OpenList, ClosedList);

    Chars = [] :
      List = [],
      Chars1 = [],
      OpenList = ClosedList ;

    otherwise :
      List = [],
      Chars = Chars1,
      OpenList = ClosedList .

one_line(Char, Chars, List, Chars1, OpenList, ClosedList) :-

    -128 =< Char, Char =< 127 |
	one_char(Char, Chars, List, Chars1, OpenList, ClosedList);

    otherwise :
      ascii('?', Q), 
      List ! Q |
	fail(chars_to_lines, not_a_character(Char)),
	one_line(Chars, List', Chars1, OpenList, ClosedList).

one_char(Char, Chars, List, Chars1, OpenList, ClosedList) :-

    Char =\= 13 /* 13 =:= ascii(cr) */,
    Char =\= 10 /* 10 =:= ascii(lf) */:
      List ! Char |
	one_line(Chars, List', Chars1, OpenList, ClosedList);

    otherwise : Char = _,
      List = [],
      Chars = Chars1,
      OpenList = ClosedList .


procedure append_strings([StIn], String).

StIn ::= String ; Number.

append_strings(Strings, String) :-
	append_strings(Strings, List, List, String'),
	send(append_strings, String', String', String).

append_strings(Strings, List1, List2, String) :-
    Strings ? SN |
	append_strings(SN, Strings', List1, List2, String);

    Strings = [],
    list(List1) :
      List2 = [] |
	list_to_string(List1, String);

    Strings = [],
    List1 = List2 :
      String = '' ;    

    otherwise :
      Strings' = [Strings] |
	append_strings.

append_strings(SN, Strings, List1, List2, String) :-

    string(SN),
    string_to_dlist(SN, List2^, List3) |
	append_strings(Strings, List1, List3, String);
	
    number(SN),
    convert_to_string(SN, IS), string_to_dlist(IS, List2^, List3) |
	append_strings(Strings, List1, List3, String);

    SN = [] |
	append_strings(Strings, List1, List2, String);

    invalid(SN) : Strings = _, List1 = _, List2 = _,	% The result is
      String = SN ;	% invalid in the same way as the first invalid element.

    otherwise :
      ascii('?', Q),
      List2 ! Q |
	fail(append_strings, not_a_string(SN)),
	append_strings(Strings, List1, List2', String).

procedure ground_stream([Any], [Any]).

ground_stream([], []^).
ground_stream(Xs, Ys) :-

    Xs ? X |
	ground(X, done, Done),
	send(ground_stream, Done, [X | Ys'?], Ys),
	ground_stream;

    otherwise |
	ground(Xs, done, Done),
	send(ground_stream, Done, Xs, Ys).


procedure ground(Any, Any).

ground(Term, Grounded) :-
	ground(Term, done, Done),
	send(ground, Done, Term, Grounded).

ground(Term, Done1, Done2) :-

    Term ? Car |
	ground(Car, Done1, Done1'),
	ground;

    tuple(Term), arity(Term, N) |
	ground_args(Term, N, Done1, Done2);

    otherwise : Term = _,
      Done1 = Done2 .


ground_args(_, 0, Done, Done^).
ground_args(Term, N, Done1, Done2) :-
    N > 0,
    arg(N, Term, A),
    N' := N - 1 |
	ground(A, Done1, Done1'),
	ground_args.

procedure unparse(Any, Any).

unparse(Term,Result) :-
	unparse(Term, Unparsed, Unparsed, Grounded),
	send(unparse, Grounded, Grounded, Result).

unparse(Term, Unparsed, Left, Right) :-

    Term ? Car :
      Unparsed ! Car' |
	unparse(Car, Car', Left, Left'),
	unparse;

    tuple(Term) |
	unparse_tuple(Term, Unparsed, Left, Right);

    otherwise :
      Term = Unparsed,
      Left = Right .

unparse_tuple(Term, Unparsed, Left, Right) :-

    Term = `Name,
    string(Name) :
      Name = Unparsed,
      Left = Right ;

    Term = ?Name,
    string(Name),
    string_to_dlist(Name, List, [QM]) :
      ascii("?", QM),
      Left = Right |
	list_to_string(List, Unparsed);

   otherwise,
   N := arity(Term),
   make_tuple(N, Unparsed^) |
	unparse_args(N, Term, Unparsed, Left, Right).

unparse_args(N, Term, Tuple, Left, Right) :-
    N > 0,
    N' := N - 1,
    arg(N, Term, Arg),
    arg(N, Tuple, Arg') |
	unparse_args,
	unparse(Arg, Arg', Right', Right).
unparse_args(0, _, _, Done, Done^).


procedure evaluate(Any, Number).

evaluate(Expression, Result) :-
	compute_value(Expression, Value),
	send(evaluate, Value, Value, Result).

compute_value(Expression, Value) :-

    Expression = Expression' + Addend |
	self,
	compute_value(Addend, AValue),
	Value := Value' + AValue;

    Expression = Expression' - Subtrahend |
	self,
	compute_value(Subtrahend, SValue),
	Value := Value' - SValue;

    Expression = Expression' * Multiplicand |
	self,
	compute_value(Multiplicand, MValue),
	Value := Value' * MValue;

    Expression = Expression' / Divisor |
	self,
	compute_value(Divisor, DValue),
	Value := Value' / DValue;

    Expression = div(Expression', Divisor) |	% alias
	self,
	compute_value(Divisor, DValue),
	Value := Value' / DValue;

    Expression = Expression' \ Modulus |
	self,
	compute_value(Modulus, MValue),
	check_integer(Value', AInt),
	check_integer(MValue, BInt),
	Value := AInt \ BInt;

    Expression = mod(Expression', Modulus) |	% alias
	self,
	compute_value(Modulus, MValue),
	compute_value(Modulus, MValue),
	check_integer(Value', AInt),
	check_integer(MValue, BInt),
	Value := AInt \ BInt;

    Expression = Expression' /\ Andend |
	self,
	compute_value(Andend, BValue),
	check_integer(Value', AInt),
	check_integer(BValue, BInt),
	Value := AInt /\ BInt;

    Expression = Expression' \/ Orend |
	self,
	compute_value(Orend, OValue),
	check_integer(Value', AInt),
	check_integer(OValue, BInt),
	Value := AInt \/ BInt;

    Expression = ~Expression' |
	self,
	check_integer(Value', AInt),
	Value := ~AInt;

    Expression = bitwise_and(A, B) |	% alias
	compute_value(A /\ B, Value);

    Expression = bitwise_or(A, B) |	% alias
	compute_value(A \/ B, Value);

    Expression = bitwise_not(A) |	% alias
	compute_value(~A, Value);

    Expression = max(A, B) |
	compute_value(A, Aval),
	compute_value(B, Bval),
	Value := max(Aval, Bval);

    Expression = min(A, B) |
	compute_value(A, Aval),
	compute_value(B, Bval),
	Value := min(Aval, Bval);

    Expression = abs(A) |
	compute_value(A, Aval),
	Value := abs(Aval);

    Expression = round(A) |
	compute_value(A, Aval),
	Value := integer(real(1)/2 + Aval);

    Expression = real(A) |
	compute_value(A, Aval),
	Value := real(Aval);

    Expression = sin(A) |
	compute_value(A, Aval),
	Value := sin(Aval);

    Expression = cos(A) |
	compute_value(A, Aval),
	Value := cos(Aval);

    Expression = tan(A) |
	compute_value(A, Aval),
	Value := tan(Aval);

    Expression = sqrt(A) |
	compute_value(A?,Aval),
	Value := sqrt(Aval);

    Expression = asin(A) |
	compute_value(A?,Aval),
	Value := asin(Aval);

    Expression = acos(A) |
	compute_value(A?,Aval),
	Value := acos(Aval);

    Expression = atan(A) |
	compute_value(A?,Aval),
	Value := atan(Aval);

    Expression = ln(A) |
	compute_value(A?,Aval),
	Value := ln(Aval);

    Expression = log(A) |
	compute_value(A?,Aval),
	Value := ln(Aval);

    Expression = exp(A) |
	compute_value(A?,Aval),
	Value := exp(Aval);

    Expression = pow(A,B) |
	compute_value(A,Aval),
	compute_value(B,Bval),
	Value := pow(Aval?,Bval?);

    Expression = log(A,B) |
	compute_value(A,Aval),
	compute_value(B,Bval),
	Value := log(Aval?,Bval?);

    Expression = random |
	Value := random;

    Expression = integer(A) |
	compute_value(A, Aval),
	Value := integer(Aval);

    number(Expression) :
      Expression = Value ;

    Expression = +Expression' |
	self;

    Expression = -Expression' |
	self,
	Value := -Value';

    Expression = ascii(S),
    string(S) |
	ascii_value(S, Value);

    Expression = arity(T),
    Value^ := arity(T) |
	true;

    Expression = arity(L),
    list(L) :
      Value = 2 ;

    Expression = string_hash(S),
    Value^ := string_hash(S) |
	true;

    Expression = string_length(S),
    Value^ := string_length(S) |
	true;

    Expression = nth_char(N, S),
    nth_char(N, S, V) :
      Value = V ;

    Expression = length(S),
    string(S),
    Value^ := string_length(S) |
	true;

    Expression = length(L),
    list(L) |
	list_length(L, 0, Value);

    otherwise :
      Value = 0 |
	fail(evaluate,can't_evaluate(Expression)).

check_integer(AInt, AInt^) :-
    integer(AInt) |
	true.
check_integer(Error, 1^) :-
    otherwise |
	fail(evaluate, not_an_integer(Error)).

ascii_value(S, Value) :-

    string_to_dlist(S, [Value]^, []) |
	true;

    S = bel :
      ascii(bel, Value) ;

    S = bs :
      ascii(bs, Value) ;

    S = lf :
      ascii(lf, Value) ;

    S = cr :
      ascii(cr, Value) ;

    S = esc :
      ascii(esc, Value) ;

    S = del :
      ascii(del, Value) ;

    otherwise :
      Value = 0 |
	fail(evaluate,can't_convert_to_ascii(S)).


List ::= [Any].

procedure binary_merge([List], List).

binary_merge(In, Out) :-
	ordered_merger(Merger),
	binary_merge(In, Out, Merger).


Merger ::= [ordered_merge(List, List, List)].

procedure binary_merge([List], List, Merger).

binary_merge(In, Out, Merger) :-
    true :
      make_channel(CH, Merger) |
	level1_merge_server(In, Odds, Evens, PairList),
	binary_merger(Odds, Evens, PairList, Out, CH, done, Done),
	close_merger(Done, CH).

level1_merge_server(In, Odds, Evens, PairList) :-

    In ? Odds^ |
	level1_merge_server(In', Evens, PairList);

    otherwise : In = _,
      Odds = [], Evens = [],
      PairList = [] .

level1_merge_server(In, Evens, PairList) :-

    In ? Evens^ :
      PairList ! {One, Two} |
	level1_merge_server(In', One, Two, PairList');

    otherwise : In = _,
      Evens = [],
      PairList = [] .

procedure binary_sort_merge(List, List).

binary_sort_merge(In, Out) :-
	ordered_merger(Merger),
	binary_sort_merge(In, Out, Merger).

procedure binary_sort_merge(List, List, Merger).

binary_sort_merge(In, Out, Merger) :-
    true :
      make_channel(CH, Merger) |
	level1_sort_server(In, Odds, Evens, PairList),
	binary_merger(Odds, Evens, PairList, Out, CH, done, Done),
	close_merger(Done, CH).

level1_sort_server(In, Odds, Evens, PairList) :-

    In ? Odd :
      Odds = [Odd] |
	level1_sort_server(In', Evens, PairList);

    otherwise : In = _,
      Odds = [], Evens = [],
      PairList = [] .

level1_sort_server(In, Evens, PairList) :-

    In ? Even :
      Evens = [Even],
      PairList ! {One, Two} |
	level1_sort_server(In', One, Two, PairList');

    otherwise : In = _,
      Evens = [],
      PairList = [] .


binary_merger(In1, In2, PairList, Out, CH, Left, Right) :-

    list(In2) :
      write_channel(ordered_merge(In1, In2, In1'), CH) |
	binary_merger2(PairList, In2', PairList', CH, Left, Left'),
	binary_merger;

    In2 = [] : PairList = _, CH = _, 
      In1 = Out,
      Left = Right .

binary_merger1(PairList, UpList, CH, Left, Right) :-

    PairList ? {In1, In2} :
      UpList ! {Out1, Out2},
      write_channel(ordered_merge(In1, In2, Out1), CH) |
	binary_merger2(PairList', Out2, UpList', CH, Left, Right);

    PairList = [] : CH = _,
      UpList = [],
      Left = Right .

binary_merger2(PairList, Out, UpList, CH, Left, Right) :-

    PairList ? {In1, In2} :
      write_channel(ordered_merge(In1, In2, Out), CH) |
	binary_merger1(PairList', UpList, CH, Left, Right);

    PairList = [] : CH = _,
      Out = [],
      UpList = [],
      Left = Right .
/*
** Basic ordered merge - merge ground terms without duplication.
*/

ordered_merger(In) :-
    In ? ordered_merge(In1, In2, Out) |
	ordered_merge(In1, In2, Out),
	ordered_merger.
ordered_merger([]).

ordered_merge(In1, In2, Out) :-

    In1 ? I1, In2 = [I2 | _],
    I1 @< I2 :
      Out ! I1 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    I2 @< I1 :
      Out ! I2 |
	ordered_merge;

    In1 = [I | _], In2 ? I |
	ordered_merge;

    In1 = [] :
      In2 = Out ;

    In2 = [] :
      In1 = Out .

close_merger(done, CH) :-
    true :
      close_channel(CH) .

procedure integer_to_dlist(Integer, [Integer], Any).

integer_to_dlist(I, List, Tail) :-
    integer(I),
    convert_to_string(I, String),
    string_to_dlist(String, List^, Tail) |
	true.


procedure freeze_term(Any, String, [{String, Any}]).

freeze_term(Term, FrozenTerm, Dictionary) :-
    true :
      Dictionary = Head?,
      FrozenTerm = Term'? |
	processor # link(execute(freeze_term,
				 {Term, Term', 1000000, 1000000,
				 2, _, Head, [], _, 1,
				 2, '_var', '_ro', '_1000000_', '' }
			 )
		    ).

procedure list_length(List, Integer).

list_length(List, Length) :-
	list_length(List, 0, Total),
	send(list_length, Total, Total, Length).

list_length(List, Counter, Length) :-

    List ? _,
    Counter' := Counter + 1 |
	list_length;

    otherwise : List = _,
      Counter = Length .


procedure tuple_to_dlist(Tuple, List, Any).

tuple_to_dlist(Tuple, List, End) :-
    Arity := arity(Tuple) |
	tuple_to_dlist(0, Arity, Tuple, FinalList, {End, FinalList, List1}),
	send(tuple_to_dlist, List1, List1, List).

tuple_to_dlist(N, Arity, Tuple, List, Lists) :-

    N < Arity,
    N' := N + 1, arg(N', Tuple, Arg) :
      List ! Arg |
	tuple_to_dlist;

    N = Arity : Tuple = _,
      Lists = {List, FinalList, FinalList} .


procedure list_to_tuple(List, Tuple).

list_to_tuple(List,Tuple) :-
	list_length(List, 0, N),
	make_tuple(N, Tuple1),
	instantiate(List, Tuple1, 0, Tuple2),
	send(list_to_tuple, Tuple2, Tuple2, Tuple).

instantiate(List, Tuple1, N, Tuple2) :-

    List ? Arg,
    N' := N + 1,
    arg(N', Tuple1, Arg^) |
	instantiate;

    otherwise : List = _, N= _,			% usually List = []
      Tuple1 = Tuple2 .
