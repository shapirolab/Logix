/*

Precompiler for FCP - meta-interpreter plus clause form
Bill Silverman, 20 May, 1987

Last update by		$Author: bill $
		       	$Date: 2004/07/22 16:26:09 $
Currently locked by 	$Locker:  $
			$Revision: 1.3 $
			$Source: /home/qiana/Repository/Logix/system/compile/control/meta.cp,v $

Copyright (C) 1987, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([interpret/3]).
-language([evaluate, compound, colon]).

/* 
**This is only generated (so far) by biospi.
*/
SECOND_PHASE_SUFFIX => 39. % Prime (')


/*
 * interpret transforms the procedures of Program, adding three arguments to
 * each.  It creates a boot-strap selection procedure, which transforms an
 * argument into a call to the corresponding transformed procedure.
 *
Input:	Linkage = {ExportedIds,ImportingIds} -
 *	    ExportedIds = [ Ident, ... ],
 *		Ident  = Functor/Arguments  or  {Functor/Arguments,Alias};
 *	    ImportingIds  = [ Ident1, ... ],
 *		Ident1 = Functor/Arguments  or  Functor*Arguments;
 *	Program  = [procedure(Ident,[Clause, ... ]), ... ];
Output:	Program' = [ControlCode ... procedure(Ident',[Clause', ... ]) ];
 *
 *	Ident' = Name/Arity'
 *
 *	Clause = {H,{A,T},B} :	H is the clause head, Name(Arguments);
 *				A is a guard-kernel list;
 *				T is a guard-kernel list;
 *				B is a body-goal list.
 *
 *	ControlCode is the selection procedure, which transforms calls to the
 *		    module.
 *
 *	Clause' = {H',{A,T},[]} : H' has the form :
 *
 *			Name(Arguments,B',I,T)
 *				B' is a body-goal conjunction
 *				   ( [] ~ true; [p,q] ~ (p,q) )
 *				I is the ordinal of the clause (input/output)
 *				  or anything else (input)
 *				T is the time of reduction (output), or
 *				  "failed" (output), or "abort" (input)
 */


List ::= [Any].

procedure interpret({[Id], [Id]}, Procedures, Procedures).

Id ::= String/Integer.
Procedures ::= [procedure(Id, List)].


interpret(ExportImport, Program1, Program2) :-
    ExportImport = {ExportedIds, _Importing} :
      Program2 = [ procedure('_select' / 2, Select) | Procedures] |
	select_resume(ExportedIds, Program1, SelectIds),
	select_resume(ExportedIds, Program1, SelectIds),
	reduce_suspend(Program1, Type),
	stream#hash_table(PIds?),
	select_meta(Type, SelectIds, Select, PIds, PIds'?, PIds'),
	transform(Program1, Procedures).


select_resume(ExportedIds, Resumes, SelectIds) :-

    ExportedIds ? Id :
      SelectIds ! Id |
	reduce_resume(Id, Resumes, Resumes'),
	select_resume;

    ExportedIds = [] :
      Resumes = SelectIds .


reduce_resume(Id, Resumes1, Resumes2) :-

    Resumes1 ? procedure(Id, _) :
      Resumes1' = Resumes2;

    otherwise,
    Resumes1 ? Procedure :
      Resumes2 ! Procedure |
	reduce_resume;

    Resumes1 = [] : Id = _,
      Resumes2 = [] .


reduce_suspend(Procedures, Type) :-

    Procedures ? procedure(Name/_Args, _),
    string_length(Name, N), nth_char(N, Name, SECOND_PHASE_SUFFIX) :
      Procedures' = _,
      Type = meta_suspend;

    Procedures ? _Procedure,
    otherwise |
	self;

    Procedures =?= [] :
      Type = meta.


select_meta(Type, SelectIds, Selects, SIds, TIds, PIds) :-

    SelectIds ? SId,
    Type =?= meta :
      Selects ! {'_select'({Function?, External?}, `'Controls'),
			{ [],
			  [`'Controls' = Type(`'Body',`'Ident',`'Result')]
			},
			[ Internal? ]
		} |
	select_meta,
	factor_ident(SId, Function, Functor, Alias, Arguments),
	generate_heads(Functor?, Alias?, Arguments?, External, Internal);

    SelectIds ? SId,
    Type =?= meta_suspend :
      PIds ! member(Functor?, Value, Ok),
      SIds ! lookup(SecondPhaseFunctor?, External?, External?, _Status),
/**** Selects ! {'_select'({Function?, External?}, `'Controls'), ****/
      Selects ! {'_select'({`'_', External?}, `'Controls'),
			{ [],
			  [`'Controls' = Type(`'Body',`'Ident',`'Result',
			  			Suspend?)
			  ]
			},
			[ Internal? ]
		} |
	select_meta,
	factor_ident(SId, _Function, Functor, Alias, Arguments),
	generate_heads(Functor?, Alias?, Arguments?, External, Internal),
	string_to_dlist(Functor?, FL, [SECOND_PHASE_SUFFIX]),
	list_to_string(FL?, SecondPhaseFunctor),
	suspend_alias(Ok?, External?, Value, Suspend);

    SelectIds = [] :
      SIds = TIds,
      PIds = [],
      Selects = [ {'_select'(`'_', `'Controls'),
			{ [ otherwise ], [ Unknown ] },
			[]
		  },
		  {'_select'(`'_', `'Controls'),
			{ [ Abort ], [] },
			[]
		  }
		]
      |
	select_failed(Type, unknown, Unknown),
	select_failed(Type, abort, Abort).

  suspend_alias(Ok, External, Value, Suspend) :-

    Ok =?= true :
    /* An exported procedure cannot be a communication procedure. */ 
      External = _,
      Value = Suspend;

    Ok =?= false :
      Value = _,
      Suspend = External.

  select_failed(meta, Signal,
		(`'Controls' = meta(`'_', `'_', Signal))^).
  select_failed(meta_suspend, Signal,
		(`'Controls' = meta_suspend(`'_', `'_', Signal, `'_'))^).


transform(Program, Procedures) :-
    Program ? procedure(Id, Clauses) :
      Procedures ! procedure(Alias? / Augmented?, Transformed?) |
	transform,
	factor_ident(Id, _Function, _Functor, Alias, Arguments),
	Augmented := Arguments + 3,
	clauses(Clauses, Alias?, Arguments, 1, Transformed).
transform([], []^).


clauses(Clauses, Functor, Arguments, N, NewClauses) :-

    Clauses ? {H, G, B},
    N' := N + 1 :
      NewClauses ! {H1?, G1?, []} |
	clauses,
	rewrite_head(Functor, Arguments, H, H1),
	conjunction(B, B1),
	rewrite_guard(G, B1, N, G1);

    Clauses = [] : N = _,
      NewClauses = [ { Internal?,
			{ [ otherwise ], [ `'Result' = failed ] },
			[]
		     },
		     { Internal?, { [ known(`'Result') ], [] }, [] }
		   ] |
	generate_heads(Functor, Functor, Arguments, _External, Internal).


rewrite_head(Functor, Arguments, H, H2) :-
    Body := Arguments + 2,
    Ident := Arguments + 3,
    Result := Arguments + 4,
    make_tuple(Result, H1),
    arg(1, H1, Functor),
    arg(Body, H1, `meta(body)),
    arg(Ident, H1, `meta(ident)),
    arg(Result, H1, `meta(result)) |
	copy_args(2, H, H1, H2).


conjunction(Goals, Conjunct) :-

    Goals ? Goal,
    Goals' =\= [], Goal =\= {_} :
      Conjunct = (Goal, Conjunct') |
	conjunction;

    Goals = [Goal], Goal =\= {_} :
      Conjunct = Goal ;

    Goals ? {Goal} :
      Goals'' = [Goal | Goals'] |
	conjunction;

    Goals = [] :
      Conjunct = true .


rewrite_guard(	{Ask, Tell}, Conjunct, N,
		{Ask1?, [`meta(time) = `meta(result),
			 `meta(ident) = N
			| Tell1?]
		}^
) :-
	append(Ask, [info(5, `meta(time))], Ask1),
	append(Tell, [`meta(body) = Conjunct],
		Tell1
	).


append(Head, End, Tail) :-

    Head ? G :
     Tail ! G |
	append;

    Head = [] :
      End = Tail .


/* Arguments = number of arguments of declared procedure */
/* produces  External, Internal  */

generate_heads(Functor, Alias, Arguments, External, Internal) :-

    Arguments = 0 :
      Functor = External |
	call_head(0, Alias, Internal);

    Arguments > 0,
    MHArity := Arguments + 1,
    make_tuple(MHArity, MH),
    arg(1, MH, Functor) |
	call_head(Arguments, Alias, CH1),
	generate_args(0, 1, MHArity, MH, External, CH1?, Internal).


call_head(Arguments, Alias, CH) :-
    Body := Arguments + 2,
    Ident := Arguments + 3,
    Result := Arguments + 4,
    make_tuple(Result, C),
    arg(1, C, Alias),
    arg(Body, C, `'Body'),
    arg(Ident, C, `'Ident'),
    arg(Result, C, `'Result') :
      C = CH .


generate_args(M0, M1, N, MH, MH1, CH, CH1) :-
    M1 < N,
    M2 := M1 + 1,
    Char1 := ascii('A') + ( M0 / 26 ),
    Char2 := ascii('A') + ( M0 \ 26 ),
    arg(M2, MH, (`Name?)^),
    arg(M2, CH, (`Name?)^) |
	generate_args(M1, M2, N, MH, MH1, CH, CH1),
	list_to_string([Char1,Char2], Name).
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
