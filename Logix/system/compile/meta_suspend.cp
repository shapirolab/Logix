/*

Precompiler for FCP - meta-interpreter plus clause form
Bill Silverman, 20 May, 1987

$Header: /home/qiana/Repository/Logix/system/compile/meta_suspend.cp,v 1.1.1.1 1999/07/09 07:03:35 bill Exp $

Copyright (C) 1987, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([interpret/3]).
-mode(trust).
-language(compound).

/*
 * interpret transforms the procedures of Program, adding three arguments to
 * each.  It creates a boot-strap selection procedure, which transforms an
 * argument into a call to the corresponding transformed procedure.
 *
Input:	Linkage = {ExportedIds,ImportingIds} -
 *	    ExportedIds = [ Ident, ... ],
 *		Ident  = Functor/Arguments
 *	    ImportingIds  = [ Ident1, ... ],
 *		Ident1 = Functor/Arguments
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
 *	Clause' = {H',{A,T},[]} : H' has the logical form :
 *
 *			Name(Arguments,B',I,R,S)
 *
 *				I is the ordinal of the clause (input/output)
 *				  or anything else (input)
 *				B' is a body-goal conjunction (output)
 *				   ( [] ~ true; [p,q] ~ (p,q) )
 *				R is the time of reduction (output), or
 *				  "failed" (output), or "abort" (input)
 *				S is the suspension indicator -
 *				  "suspend" or "reduce" (output)
 *
 *		Predicates to synchronize reduction with input/output
 *		arguments are added to the guard.  Output operations
 *              are implemented by tell predicates (=/2).
 */


List ::= [Any].

procedure interpret({[Id], [Id]}, Procedures, Procedures).

Id ::= String/Integer.
Procedures ::= [procedure(Id, List)].


interpret(ExportImport, Program1, Program2) :-
    ExportImport = {ExportedIds, _Importing} :
      Program2 = [ procedure('_select' / 2, Select) | Procedures] |
	select_resume(ExportedIds, Program1, SelectIds),
	select_meta(SelectIds,
		    { [], [`'Controls' = "meta-s"(`'Body',
		    				  {`'Suspense',`'Ident'},
						  `'Result'
					 )
			  ]
		    },
		    Select
	),
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
      Resumes1' = Resumes2 ;

    otherwise,
    Resumes1 ? Procedure :
      Resumes2 ! Procedure |
	reduce_resume;

    Resumes1 = [] : Id = _,
      Resumes2 = [] .


select_meta(SelectIds, Guard, Selects) :-

    SelectIds ? SId :
      Selects ! {'_select'({Function, External}, `'Controls'),
			Guard,
			[ Internal ]
		} |
	select_meta,
	factor_ident(SId, Function, Functor, Alias, Arguments),
	generate_heads(Functor, Alias, Arguments, External, Internal);

    SelectIds = [] : Guard = _,
      Selects = [ {'_select'(`'_', `'Controls'),
			{ [ otherwise ], 
			  [`'Controls' = "meta-s"(`'_', `'_', unknown) ]
			},
			[]
		  },
		  {'_select'(`'_', `'Controls'),
			{ [`'Controls' =?= {`'_', `'_', `'_', abort} ], [] },
			[]
		  }
		] .


transform(Program, Procedures) :-
    Program ? procedure(Id, Clauses) :
      Procedures ! procedure(Alias/Augmented, Transformed) |
	transform,
	factor_ident(Id, _Functor, _Function, Alias, Arguments),
	Augmented := Arguments + 4,
	clauses(Clauses, Alias, Arguments, 1, Transformed).
transform([], []^).


clauses(Clauses, Functor, Arguments, N, NewClauses) :-

    Clauses ? {H, G, B},
    N' := N + 1 :
      NewClauses ! {H1, G1, []} |
	clauses,
	rewrite_head(Functor, Arguments, H, H1),
	conjunction(B, B1),
	rewrite_guard(G, B1, N, G1);

    Clauses = [] : N = _,
      NewClauses = [ { Internal,
			{ [ otherwise ], [ `'Result' = failed ] },
			[]
		     },
		     { Internal,
			{ [], [`'Suspense' = suspend] },
			[Iterate]
		     },
		     { Internal, { [ known(`'Result'), known(`'Suspense') ],
				   []
				 },
			[]
		     }
		   ] |
	generate_heads(Functor, Functor, Arguments, _External, Internal),
	iterate_suspense(Internal, Iterate).


rewrite_head(Functor, Arguments, H, H2) :-
    Body := Arguments + 2,
    Ident := Arguments + 3,
    Result := Arguments + 4,
    Suspense := Arguments + 5,
    make_tuple(Suspense, H1),
    arg(1, H1, Functor),
    arg(Body, H1, `meta_suspend(body)),
    arg(Ident, H1, `meta_suspend(ident)),
    arg(Result, H1, `meta_suspend(result)),
    arg(Suspense, H1, `meta_suspend(suspense)) |
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
		{Ask1, [`meta_suspend(time) = `meta_suspend(result),
			`meta_suspend(ident) = N,
			`meta_suspend(suspense) = reduce
		       | Tell1]
		}^
) :-
	append(Ask, [info(5, `meta_suspend(time))], Ask1),
	append(Tell, [`meta_suspend(body) = Conjunct],
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
	generate_args(0, 1, MHArity, MH, External, CH1, Internal).


call_head(Arguments, Alias, CH) :-
    Body := Arguments + 2,
    Ident := Arguments + 3,
    Result := Arguments + 4,
    Suspense := Arguments + 5,
    make_tuple(Suspense, C),
    arg(1, C, Alias),
    arg(Body, C, `'Body'),
    arg(Ident, C, `'Ident'),
    arg(Result, C, `'Result'),
    arg(Suspense, C, `'Suspense') :
      C = CH .


iterate_suspense(Internal, Iterate) :-
    A := arity(Internal),
    make_tuple(A, It),
    arg(A, It, reduce) :
      Iterate = It |
	iterate_suspense(Internal, Iterate, A).

iterate_suspense(Internal, Iterate, A) :-

    A > 1,
    A' := A - 1,
    arg(A', Internal, X),
    arg(A', Iterate, X^) |
	iterate_suspense;

    A = 1 : Internal = _, Iterate = _ .


generate_args(M0, M1, N, MH, MH1, CH, CH1) :-
    M1 < N,
    M2 := M1 + 1,
    Char1 := ascii('A') + ( M0 / 26 ),
    Char2 := ascii('A') + ( M0 \ 26 ),
    arg(M2, MH, (`Name)^),
    arg(M2, CH, (`Name)^) |
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
