/*

Variable Dictionary Monitor

Yossie Lichenstein, Bill Silverman - 08/86

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:08 $
Currently locked by 	$Locker:  $
			$Revision: 1.1.1.1 $
			$Source: /home/qiana/Repository/Logix/system/dictionary_server/basic.cp,v $

Copyright (C) 1986, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([requests/1]).
-mode(trust).
-language(colon).

/*
	A dictionary server :

	Messages : - freeze, add, find, bindings, unbind, size.

	The dictionary is a difference-list, new entries are added to
	the tail. If the dictionary size exeeds MaxSize it is truncated
	(from the head) to a Treshhold. Truncation is done before freeze
	and add.
*/

requests(In) :-
	processor # link(lookup(freeze_term, Freeze)),
	processor # link(lookup(concatenate, Concatenate)),
	initialize(In, Freeze, Concatenate).

initialize(In, Freeze, Concatenate) :-
	integer(Freeze), integer(Concatenate) |
	serve(In, {Freeze, Concatenate}, H, H, 0, 0).

/***************************************************************************/

serve([Request|In], Offsets, H, T, Count, Next) :-
	serve(Request, In, Offsets, H, T, Count, Next).
serve(_, _, _, _, _, _) :-
	otherwise | true.

serve(	freeze(Term, Result, Depth, Length, Self_Reference, Variable_Format), 
	In, Offsets, H, T, Count, Next
) :-
	unify_without_failure(Frozen?, Result),
	self_reference(Self_Reference, SR),
	variable_format(Variable_Format, VF, VS, RS),
	truncate_and_serve(
		freeze(Term, Frozen, Depth, Length, SR, VF, VS, RS),
			In, Offsets, H, T, Count, Next
	).
serve(	ground(Term, {Result, []^}, {Depth, Length, Var_Format}),
	In, Offsets, H, T, Count, Next
) :-
	serve(freeze(Term, Result, Depth, Length, ignore, Var_Format),
		In, Offsets, H, T, Count, Next
	).
serve(	add(Name, Var, Response), In, Offsets, H, T, Count, Next) :-
	string(Name) |
	truncate_and_serve(
		add(Name, Var, Reply),
			In, Offsets, H, T, Count, Next
	),
	unify_without_failure(Reply?, Response).
serve(	find(Id, Var, Response), In, Offsets, H, T, Count, Next) :-
	find(Id, Var, H, T, Reply),
	unify_without_failure(Reply?, Response),
	serve(In, Offsets, H, T, Count, Next).
serve(	bindings(Out, Variable_Selector), In, Offsets, H, T, Count, Next) :-
	Offsets = {_, Concatenate} |
	bindings(H, T, Variable_Selector, Concatenate, Bindings),
	unify_without_failure(Bindings?, Out),
	serve(In, Offsets, H, T, Count, Next).
serve(	unbind(Id), In, Offsets, H, T, Count, Next) :-
	known(Id) |
	unbind(Id, H, T, New_H, T, Removed),
	New_Count := Count - Removed ,
	wait_to_serve(In, Offsets, New_H, T, New_Count, Next).
serve(	size(Var), In, Offsets, H, T, Count, Next) :-
	var(Var) |
	unify_without_failure(Count?, Var),
	serve(In, Offsets, H, T, Count, Next).
serve(	size(N), In, Offsets, H, T, Count, Next) :-
	N > 0  |
	truncate(N, H, T, New_H, New_T, Count, New_Count),
	wait_to_serve(In, Offsets, New_H, New_T, New_Count, Next).
serve(	unbind, In, Offsets, _H, _T, _Count, Next) :-
	serve(In, Offsets, New_H, New_H, 0, Next).
serve(	X, In, Offsets, H, T, Count, Next) :-
	otherwise |
	computation # error(dictionary, dont_understand, X),
	serve(In, Offsets, H, T, Count, Next).

/***************************************************************************/

truncate_and_serve(Service, In, Offsets, H, T, Count, Next) :-
	Count > 500 |						% Threshold
	truncate(300, H, T, New_H, New_T, Count, New_Count),	% Reset
	serve_now(Service, In, Offsets, New_H, New_T, New_Count, Next).
truncate_and_serve(Service, In, Offsets, H, T, Count, Next) :-
	Count =< 500 |						% Threshold
	serve_now(Service, In, Offsets, H, T, Count, Next).

/***************************************************************************/

variable_format(namevars, 1^, '_X'^, '_X'^).
variable_format(parsed, 2^, '_var'^, '_ro'^).
variable_format(_freeze, 0^, '_'^, '_?'^) :-
	otherwise | true.

self_reference(truncate, 1^).
self_reference(isomorphic, 2^).
self_reference(_ignore, 0^) :-
	otherwise | true.

/***************************************************************************/

serve_now(freeze(Term, Frozen, Depth, Length, SR, VF, VS, RS),
		In, Offsets, H, T, Count, Next
) :-
	known(Count), known(VF),
		Offsets = {Freeze, _} |
	execute(Freeze,	{Term, Frozen,
			Depth, Length, SR,
			H, T, New_T, Added, Next,
			VF, VS, RS, "...", cyclic_term
			}
	),
	New_Count := Count + Added,
	New_Next  := Next  + Added,
	serve(In, Offsets, H, New_T, New_Count, New_Next).
serve_now(add(Name, MyVar, Reply),
		In, Offsets, H, T, Count, Next
) :-
	known(Count) |
	add(Name, MyVar, H, T, {T, New_t, Added, Reply}),
	New_Count := Count + Added,
	wait_to_serve(In, Offsets, H, New_t, New_Count, Next).

/***************************************************************************/

% wait_to_serve synchronize on Count.

wait_to_serve(In, Offsets, H, T, Count, Next) :-
	known(Count) |
	serve(In, Offsets, H, T, Count, Next).

/***************************************************************************/

truncate(N, [_|Dic], T, New_H, New_T, Count, New_Count) :-	
	Count > N ,
	Count1 := Count - 1 |
	truncate(N, Dic, T, New_H, New_T, Count1, New_Count).
truncate(N, Head, T, Head^, T^, Count, Count^) :-
	Count =< N | true .

/***************************************************************************/

unbind(Id, [{Var, Id1}|H], T, [{Var, Id1}|H1]^, T^, Removed) :-
	Id =\= Id1 |
	unbind(Id, H, T, H1, T, Removed).
unbind(Id, [{_, Id}|H], T, H^, T^, 1^).
unbind(_, H, T, H^, T^, 0^) :-
	H = T | true.

/***************************************************************************/

bindings([Entry|H], T, VS, Concatenate, Out) :-
	binding(Entry, VS, Concatenate, Out, Out1),
	bindings(H, T, VS, Concatenate, Out1).
bindings(H, T, _, _, []^) :-
	unknown(H), H = T | true.
bindings(H, T, VS, Concatenate, Out) :-
	T = [_|T1] |
	bindings(H, T1, VS, Concatenate, Out).

binding({Var, _}, _, _, Out, Out^) :-
    unknown(Var) : true .
binding({Term, Name}, VS, Concatenate, Out, Out1) :-
    known(Term) |
	binding(VS, Name, Term, Concatenate, Out, Out1).

binding(0, Name, Term, _, [ Name = Term |Out]^, Out) :-
	string(Name) | true.
binding(1, Ix, Term, Concatenate, Out, Out1) :-
	integer(Ix) |
	execute(Concatenate, {['_X', Ix], Ident, 0, 1000}),
	binding(0, Ident, Term, Concatenate, Out, Out1).
binding(2, Id, Term, _, [ '_X'(Id) = Term | Out]^, Out).
binding(_, _, _, _, Out, Out^) :-
	otherwise | true.

/***************************************************************************/

find(Id, Value, [{_, Id1}|H], T, Reply) :-
	Id =\= Id1 |
	find(Id, Value, H, T, Reply).
find(Id, Value^, [{Value, Id}|_], _, true^).
find(_, _, H, T, false^) :-
	unknown(H), H = T | true.
find(Id, _, [{_, Id}|_], _, false(find_cant_unify)) :-
	otherwise | true.
find(Id, Value, H, T, Reply) :-
	T = [_|T1] |
	find(Id, Value, H, T1, Reply).

/***************************************************************************/

add(Name, Var, [{_, Id}|H], T, Control) :-
	Name =\= Id |
	add(Name, Var, H, T, Control).
add(Name, Var^, [{Var, Name}|_], _, {Old_T, Old_T, 0, old}^).
add(Name, _, [{_, Name}|_], _, {Old_T, Old_T, 0, false(add_cant_unify)}^) :-
	otherwise | true.
add(Name, Var, T, T, {[{Var, Name}|New_T], New_T, 1, new}^).

/*************************************************************************/

execute(Offset, Arguments) :-
    true : execute(Offset, Arguments) | true.
execute(Offset, Arguments) :-
	otherwise |
	computation #
	    error(dictionary_monitor, '*** execute ***', Offset, Arguments).
