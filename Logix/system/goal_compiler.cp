/*

Goal Compiler
Ehud Shapiro, 03-22-85

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:02:56 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
    			$Source: /home/qiana/Repository/Logix/system/goal_compiler.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([term/2, term/3, display/4]).
-mode(failsafe).
-language([compound, colon]).

term(Term, Compiled) :-
	computation # DictReqs?,
	wrap_requests(Requests?, DictReqs),
	term(Term, Compiled, Requests).

wrap_requests(Rs, DRs) :-

    Rs ? R :
      DRs ! dictionary(R) |
	self;

    Rs = [] :
      DRs = [] .

term(Term, Compiled, Requests) :-
	compile(Term, Goal, compile, {Goal, Requests}, {Compiled, []}).


display(Term, Compiled, Requests, Done) :-
	compile(Term, Compiled, display, done(Requests), Done([])).


% compile goal - replace each named variable by its value in a copied term.

compile(Term, Compiled, HatAct, Left, Right) :-
    Term ? Car :
      Compiled ! Car' |
	compile(Car, Car', HatAct, Left, Left'),
	compile;

    Term = `Variable |
	add_variable(Variable, Compiled, HatAct, Left, Right, '_var');

    Term = ?Variable :
      Compiled = Value? |
	add_variable(Variable, Value, HatAct, Left, Right, '_ro');

    HatAct = display,
    Term = {Hat, Term'},
    Hat = '^', 	%	{'^', X} == X^
    Term' = `Name :
      Left = {L, [display(Name, Compiled', Compiled) | Requests]},
      Left' = {L, Requests} |
	compile;

    otherwise,
    tuple(Term),
    arity(Term, N) |
	make_tuple(N, Compiled),
	compile_tuple(Term, Compiled, HatAct, Left, Right, N);

    otherwise : HatAct = _,
      Term = Compiled,
      Left = Right |
	true.

add_variable(Name, Compiled, HatAct, Left, Right, Functor) :-

    Name = '_' : Compiled = _, HatAct = _, Functor = _,
      Left = Right |
	true;

    string(Name), Name =\= '_' : HatAct = _, Functor = _,
      Left = {Done, [add(Name, Compiled) | Requests]},
      Right = {Done, Requests} |
	true;

    integer(Name),
    convert_to_string(Name, NS),
    string_to_dlist(NS, NL, []) :
      ascii('_', UnderScore) |
	list_to_string([UnderScore | NL], Name'),
	add_variable;

    otherwise :
      Compiled = Functor(Compiled') |
	compile(Name, Compiled', HatAct, Left, Right).

compile_tuple(Term, Tuple, HatAct, Left, Right, N) :-
    N > 0,
    arg(N, Term, Arg), arg(N, Tuple, CompiledArg),
    N' := N - 1 |
	compile(Arg, CompiledArg, HatAct, Left, Left'),
	compile_tuple.
compile_tuple(_, _, _, Done, Done^, 0).
