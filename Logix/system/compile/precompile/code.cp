/*

Precompiler for FCP - macro expansion

Michael Hirsch, 27 January, 1985
Bill Silverman, 5 September, 1985

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:36 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/system/compile/precompile/code.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([macro/7]).
-mode(trust).
-language(compound).

/*
macro(Terms, ProcIds, Program, EndProgram, Errors, EndErrors, Target).

Input    Terms			: a list of clauses.
Output   ProcIds		: A list of Idents - input stream for pregraph.
Output   Program, EndProgram	: a difference list of Procedures 
Output   Errors, EndErrors	: a difference list of error diagnostics.

Format   Procedure		: procedure(Ident, [Clause, ...])
					    Ident = Functor/Arguments
Format   Clause			: {H, G, B} - H = head
					    - G = guard list ([] => true)
					    - B = body list ([] => true)
				  For H and B-term, {String} replaces String
Format	Ident			: procedure(Ident, [GoalIdent, ... ])
*/

procedure macro([Any], [String/Integer], [Any], [Any], [Any], [Any], Target).

Target ::= wam ; dg.

macro(Terms, ProcIds, Program, EndProgram, Errors, EndErrors, Target) :-

	guardtable # dictionary(Guards),
	procedures(Terms, ProcIds, Program, EndProgram,
		     Errors, EndErrors, Target, Guards
	).

procedures(Terms, ProcIds, Procedures, Program, Errs1, Errs2, Target, Guards
) :-

    list(Terms) |
	enter_procedure(Ident, Clauses, GoalIds,
			ProcIds, ProcIds', Procedures, Procedures'
	),
	clauses(Terms, Ident, Terms', Clauses, Target,
		streams(GoalIds, [], Guards, Guards', Errs1, Errs1')
	),
	procedures.

procedures([], []^, SysCode^, SysCode, Errs^, Errs, _, []^).
	

enter_procedure(Ident, Clauses, GoalIds,
		ProcIds1, ProcIds2, Procedures1, Procedures2
) :-

    Ident = "procedure"/1 : Clauses = _,
      GoalIds = [],
      ProcIds1 = ProcIds2,
      Procedures1 = Procedures2 ;

    Ident = "::="/2 : Clauses = _,
      GoalIds = [],
      ProcIds1 = ProcIds2,
      Procedures1 = Procedures2 ;

    Ident = [] : Clauses = _, GoalIds = _,
      ProcIds1 = ProcIds2,
      Procedures1 = Procedures2 ;

    otherwise :
      ProcIds1 = [procedure(Ident, GoalIds) | ProcIds2],
      Procedures1 = [procedure(Ident, Clauses, GoalIds) | Procedures2] .


clauses(Terms1, OldIdent, Terms2, Clauses, Target, Streams) :-

    Terms1 = [Term | _] |
	clause_structure(Term, HeadTuple, Head, Clause),
	factor_goal(Head, HeadTuple, Functor, NewIdent),
	check_functor(Functor, HeadTuple, OldIdent, NewIdent, Reply),
	clause(Reply, Clause, Terms1, Terms2, Clauses, Target, Streams);

    Terms1 = [] : Target = _,
      Terms2 = [],
      Clauses = [],
      Streams = streams(C, C, G, G, E, E) |
	close_ident(OldIdent).


clause_structure((H :- (G | B)),HT, H^, {HT, G, B}^).
clause_structure((H :- (A : T)),HT, H^, {HT, (A : T), true}^).
clause_structure(Clause, HT, H, {HT, G, B}^) :-
    otherwise |
	clause_structure1(Clause, H, G, B).

clause_structure1((H :- B), H^, true^, B^).
clause_structure1(H, H^, true^, true^) :-
	otherwise | true.


factor_goal(Tuple, Tuple^, Functor^, ((Functor?)/Arguments)^) :-
    tuple(Tuple),
    arg(1, Tuple, Functor),
    Arguments := arity(Tuple) - 1 |
	true.
factor_goal(String, {String}^, String^, (String/0)^) :-
    string(String) |
	true.
factor_goal(Goal, {'?'}^, Goal^, ('?'/0)^) :-
    otherwise |
	true.


check_functor(Functor, Term, A, B, Reply) :-
    string(Functor),
    Arity := arity(Term) |
	check_functor1(Functor, Arity, Term, A, B, Reply).
check_functor(_, Term, A, _, {Term, A}^) :-
    otherwise |
	true.


check_functor1('_var', 2, Term, A, _, {Term, A}^).
check_functor1('_ro', 2, Term, A, _, {Term, A}^).
check_functor1(',', 3, Term, A, _, {Term, A}^).
check_functor1(';', 3, Term, A, _, {Term, A}^).
check_functor1('|', 3, Term, A, _, {Term, A}^).
check_functor1(':', 3, Term, A, _, {Term, A}^).
check_functor1(_, _, _, A, B, (A=B)^) :-
    otherwise |
	true.

close_ident([]^).
close_ident(_) :-
    otherwise |
	true.


clause(Reply, Clause, Terms1, Terms2, Clauses, Target, Streams) :-

    Reply = (Ident^ = Ident), 
    Clause = {H, G, B},
    N1 := arity(H), make_tuple(N1, H0),
    Terms1 ? _,
    Streams = streams(Calls1, Calls2, Guard1, Guard2, E1, E2) :
      Clauses ! {H1?, G1?, B1?} |
	copy_head(N1, H, H0, H1),
	precompile_guard(Target, G, G1, Guard1-Guard1', LastTemp, GE-BE),
	body(B, B1-[], Calls1-Calls1', temps(LastTemp, _), BE-[]),
	wrap_errors(Ident, GE, E1, E1'),
	clauses(Terms1', Ident, Terms2, Clauses', Target,
		streams(Calls1', Calls2, Guard1', Guard2, E1', E2)
	);

    Reply = {Term, OldIdent},
    Terms1 ? _,
    Streams = streams(C1, C2, G1, G2, E1, E2) : Clause = _,
      E1 ! "illegal clause head following"-OldIdent-Term |
	clauses(Terms1', OldIdent, Terms2, Clauses, Target,
		streams(C1, C2, G1, G2, E1', E2)
	).

clause(_, _, Terms, Terms^, []^, _, streams(C, C, G, G, E, E)^) :-
    otherwise |
	true.


copy_head(N, H, H1, H2) :-
    arg(N, H, A),
    arg(N, H1, A^),
    N' := N - 1 |
	copy_head.

copy_head(0, _, H, H^).


body(true, (NB-NB)^, (GI-GI)^, temps(T, T)^, (E-E)^).
body((`_ = `"_"), (NB-NB)^, (GI-GI)^, temps(T, T)^, (E-E)^).
body(Body, NewBody, GoalIds, Temps, Errs) :-

    Body = (OB1, OB2),
    NewBody = NB1-NB3,
    GoalIds = GI1-GI3,
    Temps = temps(TIn, TOut),
    Errs = E1-E3 |
	body(OB1, NB1-NB2, GI1-GI2, temps(TIn, TMid), E1-E2),
	body(OB2, NB2-NB3, GI2-GI3, temps(TMid, TOut), E2-E3);

    Body = (X:=Y),
    GoalIds = GI1-GI3 |
	static_eval(Y, OpY, Compiled, body, Temps, GI1-GI2, Errs),
	body_expression(Compiled, OpY, X, NewBody, GI2-GI3);

    Body = Goal@_ :
      GoalIds = ['@'/2|GoalIds1]-GoalIds2 |
	check_remote_goals(Goal, GoalIds1-GoalIds2, Reply),
	check_body(Reply, Body, NewBody, Errs, Temps);

    Body = `_ |
	meta_body(GoalIds, Body, NewBody, Errs, Temps);

    Body = ?_ |
	meta_body(GoalIds, Body, NewBody, Errs, Temps);

    arg(1, Body, `_) |
	meta_body(GoalIds, Body, NewBody, Errs, Temps);

    arg(1, Body, ?_) |
	meta_body(GoalIds, Body, NewBody, Errs, Temps);

    otherwise |
	factor_goal(Body, Call, Functor, Ident),
	check_functor(Functor, Call, GoalIds, [Ident | GI]-GI, Reply),
	check_body(Reply, Call?, NewBody, Errs, Temps).

meta_body(GoalIds, Body, NewBody, Errs, Temps) :-
    GoalIds = GI-GI',
    NewBody = NB-NB' :
      GI ! "#"/2,
      NB ! self#Body,
      Errs = E-E,
      Temps = temps(T, T) .

check_body((GIs=GIs^), Call, ([Call | NB]-NB)^, (E-E)^, temps(T, T)^).
check_body({Term, (GI-GI)^}, _, (NB-NB)^, ([illegal_goal-Term | E]-E)^,
		temps(T, T)^
).
check_body(dynamic, RemoteGoal, ([RemoteGoal | NB]-NB)^, (E-E)^, temps(T, T)^).

check_remote_goals(Goal, GIs, Reply) :-

    Goal = Goal' @ _ |
	check_remote_goals;

    otherwise |
	check_remote_goal(Goal, GIs, Reply).


check_remote_goal(Goal, GIs, Reply) :-

    Goal = `_ :
      GIs = GI-GI,
      Reply = dynamic ;

    Goal = ?_ :
      GIs = GI-GI,
      Reply = dynamic ;

    arg(1, Goal, `_) :
      GIs = GI-GI,
      Reply = dynamic ;

    arg(1, Goal, ?_) :
      GIs = GI-GI,
      Reply = dynamic ;

    Goal = _#_ :
      GIs = GI-GI,
      Reply = dynamic ;

    Goal = attributes(_) :
      GIs = GI-GI,
      Reply = dynamic ;

    Goal = service_id(_) :
      GIs = GI-GI,
      Reply = dynamic ;

    Goal = true :
      GIs = GI-GI,
      Reply = dynamic ;

    Goal = [] :
      GIs = GI-GI,
      Reply = dynamic ;

    Goal ? Goal'' :
      GIs = GI1-GI3 |
	check_remote_goals(Goal', GI1-GI2, Reply1),
	check_remote_goals(Goal'', GI2-GI3, Reply2),
	cumulate_replies(Reply1, Reply2, Reply);

    otherwise |
	factor_goal(Goal, Goal', Functor, Ident),
	check_functor(Functor, Goal', GIs, [Ident | GI]-GI, Reply).

cumulate_replies(R1, R2, R3) :-

    R1 = (A=B),
      A = B,
      R2 = R3 ;

    R2 = (A=B) :
      A = B,
      R1 = R3 .

cumulate_replies(R1, dynamic, R1^).
cumulate_replies(dynamic, R2, R2^).
cumulate_replies(R1, {_, GI-GI}^, R1^) :-
    otherwise |
	true.


body_expression(Compiled, Result, LHS, Goals, GIs) :-

    Compiled = (_, _) :
      GIs = GI-GI |
	body_expression(Compiled, Result, LHS, Goals);

    otherwise :
      Goals = NB1-NB2,
      GIs = [':='/2 | GI]-GI |
	body_expression(Compiled, [], [], NB1-[LHS? := Result? | NB2]).

body_expression(skip, _, _, (L-L)^).
body_expression((G1, G2), Result, LHS, L1-L3) :-
	body_expression(G1, Result, LHS, L1-L2),
	body_expression(G2, Result, LHS, L2-L3).
body_expression(Goal, Result, LHS, ([Goal1 | L]-L)^) :-
    otherwise |
	rename_result(Goal, Result, LHS, Goal1).

rename_result(Goal, Result, LHS, Goal1) :-

    Goal = {A, B, `Temp},
    Result = ?Temp :
      Goal1 = {A, B, LHS} ;

    Goal = {A, B, C, `Temp},
    Result = ?Temp :
      Goal1 = {A, B, C, LHS} ;

    Goal =\= {_,_,_}, Goal =\= {_,_,_,_},
    N-- := arity(Goal),
    arg(N,Goal,`Temp),
    Result = ?Temp,
    make_tuple(N, T),
    arg(N, T, AN) :
      AN = LHS |
	copy_head(N', Goal, T, Goal1);

    otherwise : Result = _, LHS = _,
      Goal1 = Goal.


precompile_guard(_, true, {[], []}^, (Guard-Guard)^, 0^, (E-E)^).
precompile_guard(Target, (GA:GT), {Ask?, Tell?}^, Guards-Guards2,
			LastTemp, Es1-Es3
) :-
	precompile_guard1(GA, ask, Target, Ask, Guards-Guards1,
				0, TellTemp, Es1-Es2
	),
	precompile_guard1(GT, tell, Target, Tell, Guards1-Guards2,
				TellTemp, LastTemp,  Es2-Es3
	).
precompile_guard(Target, G, {Ask?, []}^, Guards, LastTemp, Errs) :-
    otherwise |
	precompile_guard1(G, ask, Target, Ask, Guards, 0, LastTemp, Errs).

precompile_guard1(G, Kind, Target, G2, Guards, FirstTemp, LastTemp, Errs) :-
	hold_conditions(In, []),
	guard(G, Kind(Target, G1, []), Guards,
	      state(FirstTemp, LastTemp, In-[]), Errs
	),
	eliminate_true_guards(G1, G2).

hold_conditions(RCs, Held) :-
    RCs ? {Result, C} |
	hold_condition(C, Held, Held', Result),
	hold_conditions.
hold_conditions([], _).

hold_condition(C, Held1, Held2, Result) :-

    Held1 ? C :
      Held2 = [C | Held1'],
      Result = true ;

    otherwise,
    Held1 ? Other :
      Held2 ! Other |
	hold_condition;

    Held1 = [] :
      Held2 = [C],
      Result = false .


guard((OG1, OG2), Kind(Target, NG1, NG3), (Out1-Out3),
	state(Tin, Tout, H1-H3), E1-E3
) :-
	guard(OG1, Kind(Target, NG1, NG2), Out1-Out2,
		state(Tin, Tmid, H1-H2),  E1-E2
	),
	guard(OG2, Kind(Target, NG2, NG3), Out2-Out3,
		state(Tmid, Tout, H2-H3), E2-E3
	).
guard(otherwise, ask(_, [otherwise | NG]^, NG), (Out-Out)^, state(T, T, H-H)^,
		(E-E)^
).
guard(ascii(Name, Val), Kind(Target, [Unify? | NG?], NG)^, Out,
		state(T, T, Hold)^, E1-E3
) :-
	assigned_argument(integer, Val, Ascii, VX, E1, E2),
	ascii(Name, Ascii, E2, E3),
	ask_unify(Kind, Target, VX, Ascii, Unify),
	assert_post([known(Val), integer(Val), 0=<Val, Val=<255], Out, Hold).
guard((X := Y), ask(Target, NG1, NG2), Out, state(Tin, Tout, H1-H3), E1-E3) :-
	static_eval(Y, OpY, Compiled, guard, temps(Tin, Tout), _, E2-E3),
	guard_expression(Compiled?, NG1, [Unify? | NG2], H1, H2?),
	assigned_argument(number, X, OpY, ValueX, E1, E2?),
	ask_unify(ask, Target, ValueX?, OpY?, Unify),
	assert_post([known(X),number(X)], Out, H2-H3).

guard(OldGuard, NewGuard, Out, State, Errs) :-

    string(OldGuard), OldGuard =\= otherwise |
	guard1(guard, OldGuard, NewGuard, Out, State, Errs);

    OldGuard = {Opcode, Arg1, Arg2},
    Opcode =\= ',', Opcode =\= ascii, Opcode =\= ':=', string(Opcode),
    NewGuard = _(Target, _, _) |
	guard_relation(Opcode, Target, Arg1, Arg2, Reply, Operation),
	guard1(Reply, Operation, NewGuard, Out, State, Errs);

    otherwise,
    arg(1, OldGuard, Opcode), string(Opcode) |
	guard1(guard, OldGuard, NewGuard, Out, State, Errs).

guard(OG, Kind(_, NG, NG)^, (Out-Out)^, state(T, T, H-H)^,
	([Kind(illegal_guard)-OG|E]-E)^
) :-
    otherwise |
	true.


guard1(relation, {Opcode, Expression1, Expression2},
		ask(Target, NG1, NG2),
		Out,
		state(T1, T3, H1-H3),
		E1-E3
) :-
	static_eval(Expression1, Value1, Compiled1, guard, temps(T1, T2), _,
			E1-E2
	),
	static_eval(Expression2, Value2, Compiled2, guard, temps(T2, T3), _,
			E2-E3
	),
	evaluate_relation(Target, Opcode, Expression1, Expression2,
			  (Compiled1, Compiled2, {Opcode?, Value1?, Value2?}),
			  Relation
	),
	guard_expression(Relation, NG1, NG2?, H1, H2),
	assert_post(	[ known(Value1), known(Value2),
			  number(Value1), number(Value2) ],
			Out, H2-H3
	).
guard1(_, 	OldGuard,
		NewGuard,
  		([guard_analysis(Ident?, Analysis) | Out1]-Out2)^,
		State,
		Err
) :-
    otherwise |
	factor_goal(OldGuard, _, _, Ident),
	guard_r(OldGuard, NewGuard, Analysis, State, Out1-Out2, Err).

ascii(bel, 7^, E^, E).
ascii(bs, 8^, E^, E).
ascii(lf, 10^, E^, E).
ascii(cr, 13^, E^, E).
ascii(esc, 27^, E^, E).
ascii(del, 127^, E^, E).
ascii(Letter, IntLetter, E^, E) :-
    string_length(Letter) =:= 1,
    string_to_dlist(Letter, [IntLetter^], []) |
	true.
ascii(Code, 0^, [invalid_ascii(Code) | E]^, E) :-
    otherwise |
	true.

guard_expression(Goal, L1, L2, H1, H2) :-

    Goal = skip,
    L2 ? (Var = Val),
    number(Val) :
      L1 = [plus(Val, 0, Var) | L2'],	% kluge for ndg!
      H1 = H2 ;

    Goal = skip,
    otherwise :		% wam ?
      L1 = L2,
      H1 = H2 ;

    Goal =\= skip |
	guard_expression1.

guard_expression1(Goal, L1, L2, H1, H2) :-

    Goal = (Goal', Goal'') |
	guard_expression1(Goal', L1, L1', H1, H1'),
	self;

    Goal = skip :
      L1 = L2,
      H1 = H2 ;

    otherwise :
      H1 = [{Result, Goal} | H2] |
	guard_expression_end(Result, Goal, L1, L2).


guard_expression_end(true, _, T^, T).
guard_expression_end(false, Goal, [Goal | T]^, T).


guard_relation('<',     _, A, B, relation^, (A<B)^).
guard_relation('>',     _, A, B, relation^, (B<A)^).
/* When emulator support ">"/2, replace by:	******************************
guard_relation('>',   wam, A, B, relation^, (B<A)^).
guard_relation('>',    dg, A, B, relation^, (A>B)^).
*/
guard_relation('=<',    _, A, B, relation^, (A=<B)^).
guard_relation('>=',    _, A, B, relation^, (B=<A)^).
/* When emulator supports ">="/2, replace by:	******************************
guard_relation('>=',  wam, A, B, relation^, (B=<A)^).
guard_relation('>=',   dg, A, B, relation^, (A>=B)^).
*/
guard_relation('=:=', wam, A, B, relation^, (A=?=B)^).
guard_relation('=:=',  DG, A, B, relation^, (A=B)^) :-
    DG =\= wam | true.
guard_relation('=?=',  DG, A, B, guard^, (A=B)^) :-
    DG =\= wam | true.
guard_relation(Functor, _, A, B, guard^, {Functor, A, B}^) :-
    otherwise |
	true.

evaluate_relation(Target, Opcode, Expression1, Expression2, Rel1, Rel3) :-

    Target = wam : Opcode = _ |
	expression_is_variable(Expression1, Rel1, Rel2),
	expression_is_variable(Expression2, Rel2, Rel3);

    Opcode = "=?=",
    compound(Expression1), compound(Expression2) : Target = _ |
	expression_is_variable(Expression1, Rel1, Rel2),
	expression_is_variable(Expression2, Rel2, Rel3);

    otherwise : Target = _, Opcode = _, Expression1 = _, Expression2 = _, 
      Rel1 = Rel3 .


expression_is_variable(Expression, Rel1, Rel2) :-

    Expression = `_ :
      Rel2 = (number(Expression), Rel1) ;

    Expression = ?Expression :
      Rel2 = (number(Expression), Rel1) ;

    Expression = +Expression' |
	expression_is_variable;

    Expression =\= `_, Expression =\= ?_, Expression =\= +_ :
      Rel1 = Rel2 .


assigned_argument(_, X, _, X^, E^, E) :-
    X = `_ |
	true.
assigned_argument(_, X, _, X^, E^, E) :-
    X = ?_ |
	true.
assigned_argument(number, Y, {_, _}, Y^, E^, E) :-
    number(Y) |
	true.
assigned_argument(integer, Y, {_, _}, Y^, E^, E) :-
    integer(Y) |
	true.
assigned_argument(_, Y, Y, Y^, E^, E).
assigned_argument(_, Z, _, error^, [cannot_unify-Z | Err]^, Err) :-
    otherwise |
	true.


guard_r(Term, Kind1(_, NG1, NG2), Kind2(_, _, _, _), state(T, T, H-H)^,
	(Out-Out)^,  ([Kind1(illegal_guard)-Term | E]-E)^
) :-
    Kind2 =\= either, Kind1 =\= Kind2 :
      NG2 = NG1 .
guard_r(Term, Kind(Target, NG1, NG2), _(Term^, Pre, Post, Vars),
		state(Tin, Tout, Hold), Out, (E-E)^
) :-
    otherwise |
	ask_guard(Kind, Target, Pre, Pre', Vars, Vars'),
	assign_temps(Vars', Tin, Tout, Done),
	guard_s(Done, Pre', {NG1, NG2}, {Post, Out}, Hold).


ask_unify(Kind, Target, Var, Val, Unify) :-

    Kind = ask, Target = wam :
      Unify = (Val =?= Var) ;

    otherwise : Kind = _, Target = _,
      Unify = (Var = Val) .


ask_guard(Kind, Target, Pre1, Pre2, Vars1, Vars2) :-

    Kind = ask, Target = wam,
    Pre1 ? Goal,
    list(Pre1') :
      Pre2 ! Goal,
      Vars2 = Vars1 |
	replace_unify(Pre1', Pre2');

    Kind = ask, Target = dg,		% dg => (ndg OR nccdg) AND NOT cdg
    Pre1 ? Goal,
    list(Pre1') :
      Pre2 ! Goal |
	remove_unify(Pre1', Pre2', Vars1, Vars2);

    otherwise : Kind = _, Target = _,
      Pre1 = Pre2,
      Vars2 = Vars1 .

replace_unify(Pre1, Pre2) :-

    Pre1 ? (A = B) :
      Pre2 ! (A =?= B) |
	replace_unify;

    otherwise,
    Pre1 ? Other :
      Pre2 ! Other |
	replace_unify;

    Pre1 = [] :
      Pre1 = Pre2 .

remove_unify(Pre1, Pre2, Vars1, Vars2) :-

    Pre1 ? (A = B) :
      A = B |
	self;

    Pre1 ? Other, Other =\= (_ = _) :
      Pre2 ! Other |
	self;

    Pre1 = [] :
      Pre2 = [] |
	filter_unify_temps(Vars1, Vars2).

filter_unify_temps(In, Out) :-

    In ? T,
    known(T) |
	self;

    In ? T,
    unknown(T) :
      Out ! T |
	self;

    In = [] :
      Out = [] .


guard_s(done, [], {NG, NG}^, {Post, Out}, Hold) :-
	assert_post(Post, Out, Hold).
guard_s(done, [Cond | Pre], Guard, Post, ([{Res, Cond?} | H]-H1)^) :-
	cycle(Res, Cond, Pre, Guard, Post, H-H1).

cycle(Res, Cond, Pre, Guard, Post, Hold) :-

    Res = true : Cond = _ |
	guard_s(done, Pre, Guard, Post, Hold);

    Res = false :
      Guard = {[Cond | Guard1], Guard2} |
	guard_s(done, Pre, {Guard1, Guard2}, Post, Hold).

assign_temps([], Tin, Tin^, done^).
assign_temps(Temps, Tin1, Tin2, Done) :-
     Temps ? (`temp(Tin1?))^ |
	Tin1' := Tin1 + 1, 
	assign_temps.


assert_post([], (Out-Out)^, (H-H)^).
assert_post([X | Rest], Out, ([{Result, X} | H]-H1)^) :-
	cycle_post(Result, X, Rest, Out, H-H1).

cycle_post(Result, X, Rest, Out, Hold) :-

    Result = true : X = _ |
	assert_post(Rest, Out, Hold);

    Result = false :
      Out = ([guard_analysis(Ident?, _(X, _, Post, _)) | Out1]-Out3),
      Hold = H1-H3 |
	factor_goal(X, _, _, Ident),
	assert_post(Post, Out1-Out2, H1-H2),
	assert_post(Rest, Out2-Out3, H2-H3).


eliminate_true_guards(Gs, Gs1) :-

    Gs ? integer(I),
    integer(I) |
	eliminate_true_guards;

    Gs ? real(R),
    real(R) |
	eliminate_true_guards;

    Gs ? string(S),
    string(S) |
	eliminate_true_guards;

    Gs ? number(N),
    number(N) |
	eliminate_true_guards;

    Gs ? constant(C),
    constant(C) |
	eliminate_true_guards;

    Gs ? compound(C),
    compound(C), C =\= `_, C =\= ?_ |
	eliminate_true_guards;

    Gs ? list(L),
    list(L) |
	eliminate_true_guards;

    Gs ? known(V) |
	dont_wait_for_nonvar(V, Gs1, Gs1'),
	eliminate_true_guards;

    Gs ? (I < J),
    I < J |
	eliminate_true_guards;

    Gs ? (I =< J),
    I =< J |
	eliminate_true_guards;

    Gs ? (I = J) |
	suppress_redundant_unification(I, '=', J, Gs1, Gs1'),
	eliminate_true_guards;

    Gs ? (I =?= J) |
	suppress_redundant_unification(I, '=?=', J, Gs1, Gs1'),
	eliminate_true_guards;

    otherwise,
    Gs ? G :
      Gs1 ! G |
	eliminate_true_guards.

eliminate_true_guards([], []^).


dont_wait_for_nonvar(Var, Gs1, Gs2) :-

    Var = `_ :
      Gs1 = [known(Var) | Gs2] |
	true;

    Var = ?_ :
      Gs1 = [known(Var) | Gs2] |
	true;

    otherwise : Var = _,
      Gs1 = Gs2 |
	true.


suppress_redundant_unification(_, _, `'_', Gs, Gs^).
suppress_redundant_unification(`'_', _, _, Gs, Gs^).
suppress_redundant_unification(I, EQ, J, [{EQ, I, J} | Gs]^, Gs) :-
    otherwise |
	true.


wrap_errors(Id, In, Errs1, Errs2) :-

    In ? X :
      Errs1 ! (in_procedure-Id, X) |
        wrap_errors;

    In = [] : Id = _,
      Errs1 = Errs2 |
	true.


static_eval(Y, Y^, skip^, _, temps(T, T)^, (GI-GI)^, (E-E)^) :-
    number(Y) |
	true.
static_eval(Term, Var, Compute, Part, Temps, Gs, Es) :-

    string(Term) |
	static_eval_tuple(Term/1, {Term}, Var, Compute, Part, Temps, Gs, Es);

    tuple(Term),
    A := arity(Term),
    arg(1, Term, F), string(F) |
	tuple_operator(A, F, Kind),
	static_eval_tuple(Kind, Term, Var, Compute, Part, Temps, Gs, Es).

static_eval(X, 0^, skip^, _, temps(T, T)^, (GI-GI)^,
		([illegal_expression-X | E]-E)^
) :-
    otherwise |
	true.


static_eval_tuple(Kind, Term, Value, Compute, Part, Temps, GoalIds, Errs) :-

    Kind = unary(Procedure),
    Term = _(Y) |
	static_eval1(Procedure, Y, Value, Compute, Part, Temps, GoalIds, Errs);

    Kind = primary(Procedure),
    Part = guard,
    Term = _(Y, S),
    Temps = temps(Tin, Tout),
    Errs = E1-E2 :
      Value = `temp(Tin?),
      Compute = (ComputeY, {Procedure?, OpY?, S, Value}),
      GoalIds = [Procedure? / 3 | GI1]-GI2 |
	Tin1 := Tin + 1,
	static_eval(Y, OpY, ComputeY, Part, temps(Tin1, Tout), GI1-GI2, E1-E2);

    Kind = binary(Procedure),
    Part = guard,
    Term = _(Y, Z),
    Temps = temps(Tin, Tout),
    Errs = E1-E3 :
      Value = `temp(Tin?),
      Compute = (ComputeY, ComputeZ, {Procedure?, OpY?, OpZ?, Value}),
      GoalIds = [Procedure? / 3 | GI1]-GI3 |
	Tin1 := Tin + 1, 
	static_eval(Y, OpY, ComputeY, Part, temps(Tin1, Tmid), GI1-GI2, E1-E2),
	static_eval(Z, OpZ, ComputeZ, Part, temps(Tmid, Tout), GI2-GI3, E2-E3);

    Kind = primary(Procedure),
    Part = body,
    Term = _(Y, S),
    Temps = temps(Tin, Tout),
    Errs = E1-E2 :
      Value = ?temp(Tin?),
      Compute = (ComputeY, {Procedure?, OpY?, S, `temp(Tin?)}),
      GoalIds = [Procedure? / 3 | GI1]-GI2 |
	Tin1 := Tin + 1,
	static_eval(Y, OpY, ComputeY, Part, temps(Tin1, Tout), GI1-GI2, E1-E2);

    Kind = binary(Procedure),
    Part = body,
    Term = _(Y, Z),
    Temps = temps(Tin, Tout),
    Errs = E1-E3 :
      Value = ?temp(Tin?),
      Compute = (ComputeY, ComputeZ, {Procedure?, OpY?, OpZ?, `temp(Tin?)}),
      GoalIds = [Procedure? / 3 | GI1]-GI3 |
	Tin1 := Tin + 1, 
	static_eval(Y, OpY, ComputeY, Part, temps(Tin1, Tmid), GI1-GI2, E1-E2),
	static_eval(Z, OpZ, ComputeZ, Part, temps(Tmid, Tout), GI2-GI3, E2-E3);

    Kind = kernel(Kernel),
    Part = guard,
    Term = _(A),
    Temps = temps(Tin, Tout) :
      Value = `temp(Tin),
      Compute = Kernel(A, Value),
      GoalIds = [Kernel/2 | GI]-GI,
      Errs = E-E |
	Tout := Tin + 1;

    Kind = kernel(Kernel),
    Part = body,
    Term = _(A),
    Temps = temps(Tin, Tout) : Part = _,
      Value = ?temp(Tin),
      Compute = Kernel(A, `temp(Tin)),
      GoalIds = [Kernel/2 | GI]-GI,
      Errs = E-E |
	Tout := Tin + 1;

    Kind = F/A, 
    Part = body,
    N := A + 1, make_tuple(N, Call),
    Temps = temps(Tin, Tout),
    arg(1, Call, F), arg(N, Call, `temp(Tin?)) :
      Value = ?temp(Tin?),
      GoalIds = [Kind | GI1]-GI2 |
	Tin1 := Tin + 1, 
	static_eval_args(2, Term, Call, Compute, temps(Tin1, Tout), GI1-GI2,
				Errs
	);

    Kind = ascii,
    Term = ascii(Code) : Part = _,
      Compute = skip,
      Temps = temps(T, T),
      GoalIds = GI-GI,
      Errs = E1-E2 |
	ascii(Code, Value, E1, E2).	

static_eval_tuple(variable, X, X^, skip^, _, temps(T, T)^, (GI-GI)^, (E-E)^).

static_eval_tuple(_, X, 0^, skip^, _, temps(T, T)^, (GI-GI)^,
			([illegal_expression-X | E]-E)^
) :-
    otherwise |
	true.


static_eval1(Procedure, Y, Var, Compute, Part, Temps, GoalIds, Errs) :- 

    Procedure = null |
	static_eval(Y, Var, Compute, Part, Temps, GoalIds, Errs);

    Procedure = negate |
	static_eval(0-Y, Var, Compute, Part, Temps, GoalIds, Errs);

    otherwise,
    Part = guard,
    Temps = temps(Tin, Tout) :
      Var = `temp(Tin),
      Compute = (ComputeY, {Procedure, OpY?, Var}),
      GoalIds = [Procedure / 2 | GI1]-GI2 |
	Tin1 := Tin + 1, 
	static_eval(Y, OpY, ComputeY, Part, temps(Tin1, Tout), GI1-GI2, Errs);

    otherwise,
%   Part = body,
    Temps = temps(Tin, Tout) :
      Var = ?temp(Tin),
      Compute = (ComputeY, {Procedure, OpY?, `temp(Tin)}),
      GoalIds = [Procedure / 2 | GI1]-GI2 |
	Tin1 := Tin + 1, 
	static_eval(Y, OpY, ComputeY, Part, temps(Tin1, Tout), GI1-GI2, Errs).


static_eval_args(N, Tuple, Call, Args, Temps, GoalIds, Errs) :-
    arg(N, Tuple, Y),
    arg(N, Call, (OpY?)^),
    Temps =  temps(Tin, Tout),
    N' := N + 1 :
      Args = (ComputeY?, ComputeT?),
      GoalIds = GI1-GI3,
      Errs = E1-E3 |
	static_eval_args(N', Tuple, Call, ComputeT, temps(Tin', Tout), GI2-GI3,
				E2-E3
	),
	static_eval(Y, OpY, ComputeY, body, temps(Tin, Tin'), GI1-GI2, E1-E2).
static_eval_args(_N, _Tuple, Call, Call^, temps(T, T)^, (GI-GI)^, (E-E)^) :-
    otherwise | /* arity(Tuple, A), A > N */
	true.

tuple_operator(2, '_var', variable^).
tuple_operator(2, '_ro', variable^).
tuple_operator(3, '+', binary(plus)^).
tuple_operator(3, '-', binary(diff)^).
tuple_operator(3, '*', binary(times)^).
tuple_operator(3, '/', binary(div)^).
tuple_operator(3, '\', binary(mod)^).
tuple_operator(3, '/\', binary(bitwise_and)^).
tuple_operator(3, '\/', binary(bitwise_or)^).
tuple_operator(2, '+', unary(null)^).
tuple_operator(2, '-', unary(negate)^).
tuple_operator(2, '~', unary(bitwise_not)^).
tuple_operator(2, info, unary(info)^).
tuple_operator(2, real, unary(convert_to_real)^).
tuple_operator(2, integer, unary(convert_to_integer)^).
tuple_operator(2, arity, kernel(arity)^).
tuple_operator(2, string_length, kernel(string_length)^).
tuple_operator(2, string_hash, kernel(string_hash)^).
tuple_operator(3, nth_char, primary(nth_char)^).
tuple_operator(2, ascii, ascii^).
tuple_operator(Arity, Functor, (Functor/Arity)^) :-
    otherwise |
	true.
