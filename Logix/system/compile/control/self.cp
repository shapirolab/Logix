/*

Precompiler for FCP - add protection and control capabilities.
Michael Hirsch,  27 January 1985
Bill Silverman, 5 September 1985

Last update by		$Author: bill $
		       	$Date: 2002/06/07 12:11:12 $
Currently locked by 	$Locker:  $
			$Revision: 1.2 $
			$Source: /home/qiana/Repository/Logix/system/compile/control/self.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([boot/3,
	 system/3, user/3, monitor/5,
	 trust/3, failsafe/3, interrupt/3,
	 interpret/3
	]
).
-mode(trust).
-language([evaluate, compound, colon]).

/* This is only generated (so far) by biospi.  It might be derived from the
** language attribute - but then what about blocked input?
*/
SECOND_PHASE_SUFFIX => 39. % Prime (')

List ::= [Any].

procedure boot(Any, run#Procedures, run#Procedures).
procedure failsafe(IDT, Procedures, Procedures).
procedure interrupt(IDT, Procedures, Procedures).
procedure interpret(IDT, Procedures, Procedures).
procedure system(IDT, Procedures, Procedures).
procedure trust(IDT, Procedures, Procedures).
procedure user(IDT, Procedures, Procedures).
procedure monitor(IDT, Procedures, Procedures, MonitorMode, MonitorType).

IDT ::= {[Id], [Id]}.
Id ::= String/Integer.
Procedures ::= [procedure(Id, List)].
MonitorMode ::= trust ; failsafe.
MonitorType ::= user ; system.

boot(_Linkage, Precompiled, Protected) :-
	run # system(Precompiled, Protected).

system(Linkage, Precompiled, Protected) :-
	monitor(Linkage, Precompiled, Protected, trust, system).

user(Linkage, Precompiled, Protected) :-
	monitor(Linkage, Precompiled, Protected, trust, user).

failsafe(Linkage, Precompiled, Protected) :-
	protect(Linkage, Precompiled, Protected, failsafe).


interrupt(Linkage, Precompiled, Protected) :-
	protect(Linkage, Precompiled, Protected, interrupt).


trust(Linkage, Precompiled, Protected) :-
	protect(Linkage, Precompiled, Protected, trust).


interpret(Linkage, Precompiled, Protected) :-
	meta # interpret(Linkage, Precompiled, Protected).


/*
 * protect adds controls to every procedure, adding segments to the
 * termination circuit for spawned calls, local or remote.  It provides
 * a transfer vector and auxiliary control clauses to manage procedures
 * in response to control signals from the computation, and to produce
 * exceptions for unknown exported RPCs and failed local procedure calls.
 *
 *	Linkage = {ExportedIds,ImportingIds} -
 *	    ExportedIds = [ Ident, ... ],
 *		Ident  = Functor/Arguments  or  {Functor/Arguments,Alias};
 *	    ImportingIds  = [ Ident1, ... ],
 *		Ident1 = Functor/Arguments  or  Functor*Arguments;
 *	Program = [procedure(Ident,[Clause, ... ])];
 *	NewProgram = [procedure(Functor/Augmented,[NewClause, ... ])];
 *	Mode is in [trust, failsafe, interrupt];
 *
 *	    Clause = {H,{A,T},B} :	H is the clause head,
 *					A and T are guard-kernel lists,
 *					B is a body-goal list.
 */

protect(Linkage, Program, NewProgram, Mode) :-
	stream#hash_table(PIds?),
	reduce_mode(Mode, Linkage, Program, Mode'),
	start_new_program(Mode', Linkage, NewProgram, Procedures, PIds'),
	add_short_circuit(Mode', Program, Procedures, PIds, PIds'?).


monitor(Linkage, Program, NewProgram, Mode, Type) :-

    Program = [procedure(Id,[{ Functor(IN, ATTRIBUTES), {Ask, Tell}, Body}])
	      | Rest ] :

    NewProgram = [procedure(Id, [{ Functor(`'In', `'Controls'),
				   { Ask,
				     [ `'In' = export(attributes(ATTRIBUTES)),
				       `'Controls' = Type(IN,`control(0),
							     `control(R?),
							     `control(vector)
						     )
				     | Tell]
				   },
				   Body'
				 } 
				]
		      )
	             | Tail ] |
	reduce_mode(Mode, Linkage, [], Mode'),
	add_short_circuit(Mode', Rest, Tail, _, _),
	rewrite_bodies(Body, 0, Mode', Body', [], R).
	

reduce_mode(trust, {_, Importing}, _, trust(Importing)^).
reduce_mode(failsafe, _, _, failsafe^).
reduce_mode(interrupt, _, Program, interrupt(Program)^).


start_new_program(	Mode,
			{ExportedIds, _},
			[ procedure('_select' / 2, Selects),
			  procedure('_unknown' / 4,
			  	    [{ Unknown,
				       { [ known(`'V') ], [ Write ]},
				       []
			             }
				    ]
			  )
		        | More ]^,
			More,
			PIds
) :-
    true :
      Unknown = '_unknown'(`'Goal', `'L', `'R', `'V') |
	make_select(Mode, ExportedIds, Selects, Unknown, PIds),
	make_write_exception(unknown, `'Goal', Write).


make_select(Mode, Ids, Selects, Unknown, PIds) :-

    Mode = trust(Imps) :
      PIds = [] |
	select_trust(Ids, Imps, Selects, Unknown);

    Mode = failsafe :
      PIds = [] |
	select_failsafe(Ids, Selects, Unknown);

    Mode = interrupt(Resumes) |
	select_resume(Ids, Resumes, SelectIds),
	select_interrupt(SelectIds, Selects, Unknown, PIds).


select_resume(Ids, Resumes, SelectIds) :-

    Ids ? Id :
      SelectIds ! Id |
	reduce_resume(Id, Resumes, Resumes'),
	select_resume;

    Ids = [] :
      Resumes = SelectIds |
	true.


reduce_resume(Id, Resumes1, Resumes2) :-

    Resumes1 ? procedure(Id, _) :
      Resumes1' = Resumes2 |
	true;

    otherwise,
    Resumes1 ? Procedure :
      Resumes2 ! Procedure |
	reduce_resume;

    Resumes1 = [] : Id = _,
      Resumes2 = [] |
	true.


select_trust(Ids, Imps, Selects, Unknown) :-

    Ids ? Id :
      Selects ! {'_select'(export(External), `'Controls'),
			{ [], [`'Controls' = procedures(L, R , `'V')] },
			[ Internal' ]
		} |
	select_trust,
	factor_ident(Id, _Function, Functor, Alias, Arguments),
	importing(Alias/Arguments, Imps, Reply),
	generate_heads(trust, Functor,  Alias', Arguments,
			External, Internal),
	trust_internal(Reply, Alias, Internal, Alias', Internal', L, R);

    Ids = [] : Imps = _ |
	select_others(trust, Selects, Unknown).

trust_internal(Reply, Alias1, Internal1, Alias2, Internal2, L, R) :-

    Reply = true(_),
    string_to_dlist(Alias1, List, []) :
      Internal1 = Internal2,
      L = `'L',
      R = `'R' |
	list_to_string([32 | List], Alias2);	% 32 =:= ascii(' ')

    Reply = false,
    N := arity(Internal1) - 3,
    make_tuple(N, Goal) :
      Alias1 = Alias2,
      L = `'D',
      R = `'D' |
	copy_args(1, Internal1, Goal, Internal2).


select_failsafe(Ids, Selects, Unknown) :-

    Ids ? Id :
      Selects ! {'_select'(export(External), `'Controls'),
			{ [], [`'Controls' = procedures(`'L', `'R', `'V')] },
			[ Internal ]
		} |
	select_failsafe,
	factor_ident(Id, _Function, Functor, Alias, Arguments),
	generate_heads(failsafe, Functor, Alias, Arguments,
			External, Internal
	);

    Ids = [] |
	select_others(failsafe, Selects, Unknown).


select_interrupt(Ids, Selects, Unknown, PIds) :-
		 
    Ids ? SId :
      PIds ! member(Functor?, Value, Ok),
/**** Selects ! {'_select'({Function, External}, `'Controls'), ****/
      Selects ! {'_select'({`'_', External}, `'Controls'),
			{ [],
			  [`'Controls' = procedures(`'S', `'L', `'R', `'V') ]
			},
			[ Internal ]
		} |
	select_interrupt,
	factor_ident(SId, Function, Functor, _Alias, Arguments),
	call_alias(Ok?, Value?, Function?, Functor?, Alias,
			Arguments?, Arguments'),
	generate_heads(interrupt(_), Functor?, Functor?, Arguments?,
			External, _),
	generate_heads(interrupt(_), Alias?, Alias?, Arguments'?,
			_, Internal);

    Ids = [] :
      PIds = [] |
	select_others(interrupt, Selects, Unknown).

  call_alias(Ok, Value, Function, Functor, Alias, Arguments, AliasArguments) :-

    Ok =?= true,
    /* An exported procedure cannot be a communication procedure. */ 
    Function =?= reduce :
      Arguments = _,
      Functor = _,
      Value = Alias/AliasArguments;

    Ok =?= false :
      Function = _,
      Value = _,
      Alias = Functor,
      AliasArguments = Arguments.


select_others(Mode, Selects, Unknown) :-

    Mode = interrupt :
      Selects = [ {'_select'({`'_', `'Goal'}, `'Controls'),
			{ [ otherwise ], 
			  [`'Controls' = procedures(`'_', `'L', `'R', `'V') ]
			},
			[ Unknown ]
		  }
		] ;

    otherwise : Mode = _,
      Selects = [ { '_select'({`'_', `'Goal'},`'Controls'),
			{ [ otherwise ],
			  [`'Controls' = procedures(`'L', `'R', `'V') ]
			},
			[ Unknown ]
		  }
		] .


make_write_exception(Reason, Goal,
	write_vector(1,exception(Reason, Goal,`'L',`'R'),`'V',`'_')^
).


add_short_circuit(Mode, Program, NewProgram, PIds, TIds) :-

    Program ? procedure(Ident, Clauses) :
      NewProgram ! procedure(NewIdent, NewClauses),
      Value = Functor? / Arguments?,
      PIds ! lookup(SecondPhaseFunctor?, Value, Value, Status) |
/****/	add_lookup(Status?) = add_lookup(new), /** This should NOT fail. **/
	add_short_circuit,
	new_ident(Ident,  Mode, NewIdent, Reply),
	factor_ident(Ident, _Function, Functor, _Alias, Arguments),
	/*
	** If there is a matching functor, ending in the SECOND_PHASE_SUFFIX,
	** when that process is suspended it should resume in this procedure
	** (at the first phase).
	*/
	string_to_dlist(Functor?, FL, [SECOND_PHASE_SUFFIX]),
	list_to_string(FL?, SecondPhaseFunctor),
	clauses(Reply, Clauses, Arguments, Mode, NewClauses, LastClauses),
	second_last_clause(Mode, Functor, Arguments, LastClauses);

    Program = [] : Mode = _,
      NewProgram = [],
      PIds = TIds |
	true.


new_ident(Functor/Arguments, failsafe, (Functor/Augmented)^, Functor^) :-
    Augmented := Arguments + 3 |
	true.
new_ident(Functor/Arguments, interrupt(_), (Functor/Augmented)^, Functor^) :-
    Augmented := Arguments + 4 |
	true.
new_ident(Id, trust(Imps), NewId, Rewrite) :-
	importing(Id, Imps, Reply),
	new_ident1(Reply, Id, NewId, Rewrite).

new_ident1(false, Id, Id^, []^).
new_ident1(true(NewFunctor), _/Arguments, (NewFunctor/Augmented)^,
		NewFunctor^
) :-
    Augmented := Arguments + 3 |
	true.


clauses(Functor, Clauses, Arguments, Mode, NewClauses, Last) :-
    string(Functor),
    Clauses ? {H, G, B} :
      NewClauses ! {H1, G1, B1} |
	clauses,
	rewrite_head(Mode, Functor, Arguments, R, H, H1),
	rewrite_rhs(Mode, G, B, G1, B1, R).
clauses([], Clauses, _, _, Clauses^, _).
clauses(_, [], _, _, Last^, Last).


rewrite_head(Mode, Functor, Arguments, R, H, H1) :-
    Mode = interrupt(_) |
	augment_head(Functor, H,  Arguments, 5, `interrupt(signal), R, H1);

    otherwise : Mode = _ |
	augment_head(Functor, H, Arguments, 4, _, R, H1).

augment_head(Functor, H, Arguments, Add, Signals, R, H2) :-
    Arity := Arguments + Add,
    Right := Arity - 1,
    Left  := Arity - 2,
    Ss    := Arity - 3,
    make_tuple(Arity,H1),
    arg(1, H1, Functor),
    arg(Ss, H1, Signals^),
    arg(Left , H1, `control(0)),
    arg(Right, H1, `control(R)),
    arg(Arity, H1, `control(vector)) |
	copy_args(2, H, H1, H2).

rewrite_rhs(Mode, Guard1, Body1, Guard2, Body2, Right) :-

    Body1 = [],		% This case is an optimisation for terminal clauses!
    Guard1 = {Ask, Tell} :
      Guard2 = {Ask', [`control(0) = `control(1) | Tell]},
      Body2 = [],
      Right = 1 |
	rewrite_ask(Mode, Ask, Ask');

    Body1 =\= [],
    Mode = interrupt(_) :
      Guard2 = {[unknown(`interrupt(signal)) | Ask], Tell} |
	rewrite_rhs1(Mode, Guard1, Body1, {Ask, Tell}, Body2, Right);

    otherwise |
	rewrite_rhs1(Mode, Guard1, Body1, Guard2, Body2, Right).

rewrite_rhs1(Mode, Guard1, Body1, Guard2, Body2, Right1) :-
    Guard1 = {Ask, Tell} :
      Guard2 = {Ask, Tell2} |
	rewrite_bodies(Body1, Left, Mode, Body2, Calls, Right),
	rewrite_rhs2(Right, Right1, Tell1, Tell2),
	rewrite_tell(Calls, 0, Tell, Tell1, Left).

rewrite_rhs2(0, 1^, Tell, [`control(0) = `control(1) | Tell]^).
rewrite_rhs2(R, R^, Tell, Tell^) :-
    otherwise |
	true.


rewrite_ask(Mode, Ask1, Ask2) :-

    Mode =\= interrupt(_) :
      Ask1 = Ask2 |
	true;

    Mode = interrupt(_) |
      rewrite_if_otherwise(Ask1, Ask1, Ask2).

rewrite_if_otherwise(Ask, Ask1, Ask2) :-

    Ask ? otherwise : Ask' = _,
      Ask2 = [unknown(`interrupt(signal)) | Ask1] |
	true;

    Ask ? Other,
    Other =\= otherwise |
	rewrite_if_otherwise;

    Ask = [] :
      Ask1 = Ask2 |
	true.
      

rewrite_tell(Calls, Left1, Tell1, Tell2, Left2) :-

    Tell1 = [],
    Calls ? (Op # {Id, Call}),
    Left1' := Left1 + 1 :
      Tell2 ! write_vector(1,{Op,Id,Call,`control(Left1),`control(Left1')},
				`control(vector), `'_'
	      ) |
	rewrite_tell;

    otherwise,
    Tell1 ? Predicate :
      Tell2 ! Predicate |
	rewrite_tell;

    Calls = [] :
      Left1 = Left2,
      Tell1 = Tell2 |
	true.


rewrite_bodies(Body, Left, Mode, NewBody, Calls, Right) :-

    Body ? Goal,
    Goal =\= (_ # _),
    arg(1, Goal, Functor) :
      NewBody ! Goal' |
	rewrite_bodies,
	rewrite_goal(Mode, Functor, Goal, Left, Left', Goal');

    otherwise,
    Body ? RPC :
      Calls ! RPC |
	rewrite_bodies;

    Body = [] :
      NewBody = [],
      Calls = [] |
	complete_circuit(Left, Mode, Right).

complete_circuit(Left, Mode, Right) :-

    Left = 0,
    Mode =\= trust(_) :
      Right = 1 |
	true;

    otherwise : Mode = _,
      Left = Right |
	true.


rewrite_goal(Mode, Functor, Goal1, Left, Right, Goal2) :-

    Mode = interrupt(_) |
	augment_goal(Functor, Goal1, 4, `interrupt(signal),
			Left, Right, Goal2
	);

    Mode = failsafe |
	augment_goal(Functor, Goal1, 3, _, Left, Right, Goal2);

    Mode = trust(Imps),
    Arguments := arity(Goal1) - 1 |
	importing(Functor/Arguments, Imps, Reply),
	rewrite_goal1(Reply, Goal1, Left, Right, Goal2).

rewrite_goal1(Reply, Goal1, Left, Right, Goal2) :-

    Reply = true(NewFunctor) |
	augment_goal(NewFunctor, Goal1, 3, _, Left, Right, Goal2);

    Reply = false :
      Goal1 = Goal2,
      Left = Right |
	true.


augment_goal(Functor, Goal1, Add, Signals, Left1, Left2, Goal2) :-

    VX := arity(Goal1) + Add,
    Right := VX - 1,
    Left  := VX - 2,
    Ss    := VX - 3,
    make_tuple(VX, Goal),
    arg(1, Goal, Functor),
    arg(Ss, Goal, Signals^),
    arg(Left, Goal,`control(Left1)),
    arg(Right, Goal,`control(Left2)),
    arg(VX, Goal,`control(vector)) |
	Left2 := Left1 + 1,
	copy_args(2, Goal1, Goal, Goal2).


second_last_clause(trust(_), _, _, []^).
second_last_clause(Mode, Functor, Arguments, Last) :-
    otherwise :
      Last ! { Internal, { [ otherwise | Unknown ], [ Write ] }, [] } |
	generate_heads(Mode, Functor, Functor, Arguments,
			External, Internal),
	add_unknown(Mode, Unknown),
	make_write_exception(failed, External, Write),
	last_clause(Mode, Functor, Arguments, Last').

add_unknown(failsafe, []^).
add_unknown(interrupt(_), [unknown(`'S')]^).

last_clause(failsafe, _, _, []^).
last_clause(Mode, Functor, Arguments, Last) :-
    Mode = interrupt(_) :
      Last = [ {Internal, { [ known(`'S') ],
			     [ `'L' = {`'Done', [External | `'List']},
			     `'R' = {`'Done', `'List'}
			     ]
			   },
		[]
	       }
	     ] |
	generate_heads(interrupt(_), Functor, Functor, Arguments,
			External, Internal
	).


/* Mode = failsafe or interrupt(_) */
/* Arguments = number of arguments of declared procedure */
/* produces  External, Internal  */

generate_heads(Mode, Functor, Alias, Arguments, External, Internal) :-

    Arguments = 0 :
      Functor = External |
	call_head(Mode, 0, Alias, Internal);

    Arguments > 0,
    MHArity := Arguments + 1,
    make_tuple(MHArity, MH),
    arg(1, MH, Functor) |
	call_head(Mode, Arguments, Alias, CH1),
	generate_args(0, 1, MHArity, MH, External, CH1, Internal).


call_head(Mode, Arguments, Alias, CH1) :-

    Mode = interrupt(_),
    Arity := Arguments + 5,
    make_tuple(Arity, CH),
    Ss := Arity - 3,
    arg(Ss, CH, `'S') |
	call_head1(CH, Alias, CH1);

    otherwise,			% trust(Imps) | failsafe
    Arity := Arguments + 4,
    make_tuple(Arity, CH) : Mode = _ |
	call_head1(CH, Alias, CH1).

call_head1(CH1, Alias, CH) :-
    arity(CH1, VX),
    L := VX - 2,
    R := VX - 1,
    arg(1, CH1, (Alias)^),
    arg(L, CH1, (`'L')^),
    arg(R, CH1, (`'R')^),
    arg(VX, CH1,(`'V')^) :
      CH1 = CH |
	true.


generate_args(M0, M1, N, MH, MH1, CH, CH1) :-
    M1 < N,
    M2 := M1 + 1,
    Char1 := ascii('A') + ( M0 / 26 ),
    Char2 := ascii('A') + ( M0 \ 26 ),
    arg(M2, MH, (`Name?)^),
    arg(M2, CH, (`Name?)^) |
	generate_args(M1, M2, N, MH, MH1, CH, CH1),
	list_to_string([Char1, Char2], Name).
generate_args(_, N, N, MH, MH^, CH, CH^).


copy_args(N, H, H1, H2) :-
    arg(N, H, A),
    arg(N, H1, A^),
    N' := N + 1 |
	copy_args.
copy_args(_, _, T, T^) :-
    otherwise |
	true.


factor_ident(Functor/Arguments, (`'_')^, Functor^, Functor^, Arguments^).
factor_ident(procedure(Functor/Arguments, _), reduce^,
		Functor^, Functor^, Arguments^
).


importing(Id, Ids, Reply) :-

    Ids = [Id | _],
    Id = {_, Functor, _},
    string_to_dlist(Functor, List, []) :
      Reply = true(NewFunctor) |
	list_to_string([32 | List], NewFunctor);	% 32 =:= ascii(' ')

    otherwise,
    Ids ? _ |
	importing;

    Ids = [] : Id = _,
      Reply = false |
	true.
