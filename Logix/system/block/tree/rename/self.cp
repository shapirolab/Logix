/* $Header: /home/qiana/Repository/Logix/system/block/tree/rename/self.cp,v 1.1 1999/07/09 07:03:13 bill Exp $ */
/*
 *  Rename Atoms for all procedural clauses of module.
 */

-export([clauses/6, goal/4]).
-language(compound).
-mode(trust).

/************************* C L A U S E S / 6 *********************************/

procedure clauses(Clauses, Lead, ProcIds, BlockedSource, BlockedSource, Calls).

% Rename all clauses and local goals in  Clauses , adding the
% results to  Block1 . Refer all remote procedure calls to  Calls .

clauses(Clauses, Lead, ProcIds, Blocked1, Blocked2, Calls) :-

	type_action(Lead, Type),
	parse_clauses(Type, Clauses, Blocked1, Blocked2, Changes),
	serve_clauses(Changes, Lead, ProcIds, Calls).

procedure type_action(String, TypeAction).

TypeAction ::= keep ; toss.

type_action(Lead, Type) :-

    Lead = '' :
      Type = keep ;

    otherwise : Lead = _,
      Type = toss .


Changes ::= [Change].
Change ::= head(Atom, Atom) ; goal(Atom, Atom) ; RPC.

procedure parse_clauses(TypeAction, Clauses, BlockedSource, BlockedSource,
			Changes
).
parse_clauses(Type, Clauses, Blocked1, Blocked2, Changes) :-

    Clauses ? Clause,
    Clause =\= (_ ::= _), Clause =\= (procedure _) :
      Blocked1 ! Transformed |
	clause # transform(Clause, Transformed, Changes, Changes'),
	parse_clauses;

    Clauses ? TypeClause,
    Type = keep,
    otherwise :
      Blocked1 ! TypeClause |
	parse_clauses;

    Clauses ? _TypeClause,
    Type = toss,
    otherwise |
	parse_clauses;

    Clauses = [] : Type = _,
      Blocked1 = Blocked2,
      Changes = [] .


procedure serve_clauses(Changes, String, ProcIds, Calls).

serve_clauses(Changes, Lead, ProcIds, Calls) :-

    Changes ? head(Head, Head') |
	local(true, Head, Lead, Head'),
	serve_clauses;

    Changes ? goal(Goal, Goal') |
	identifier(Goal, Id),
	auxils # member(Id, ProcIds, Answer),
	local(Answer, Goal, Lead, Goal'),
	serve_clauses;

    Changes ? Call,
    Call = call(_, _) :
      Calls ! Call |
	serve_clauses;

    Changes = [] : Lead = _, ProcIds = _,
      Calls = [] .


procedure goal(Atom, String, ProcIds, Atom).

% Transform externally referenced  Goal  of module.
      
goal(Goal, Lead, ProcIds, RenamedGoal) :-
	identifier(Goal, Id),
	auxils # member(Id, ProcIds, Answer),
	local(Answer, Goal, Lead, RenamedGoal).


procedure local((true ; false), Atom, Lead, Atom).

% If  Atom  refers to a locally defined procedure,
% prefix its functor with  Lead  in  RenamedAtom .

local(Answer, Atom, Lead, RenamedAtom) :-

    Answer = true,
    tuple(Atom),
    arg(1, Atom, Functor),
    string(Functor),
    Arity := arity(Atom),
    make_tuple(Arity, Tuple),
    string_to_dlist(Lead, RFL, FL),
    string_to_dlist(Functor, FL, []),
    arg(1, Tuple, RenamedFunctor) :
      RenamedAtom = Tuple |
	list_to_string(RFL, RenamedFunctor),
	copy_args(2, Atom, RenamedAtom);

    Answer = true,
    string(Atom) |
	auxils # append_strings(Lead, Atom, RenamedAtom);

    otherwise : Answer = _, Lead = _,
      Atom = RenamedAtom .

% copy the arguments of the original predicate to the renamed predicate

procedure copy_args(Integer, Tuple, Tuple).

copy_args(N, Tuple, RenamedTuple) :-

    arg(N, Tuple, Arg),
    arg(N, RenamedTuple, Arg^),
    N' := N + 1 |
	copy_args;

    otherwise : N = _, Tuple = _, RenamedTuple = _ .

procedure identifier(Atom, String/Integer).

% Construct the procedural identifier of  Atom .

identifier(Atom, Id) :-

    tuple(Atom),
    arg(1, Atom, Functor),
    string(Functor),
    N := arity(Atom) - 1 :
      Id = Functor/N ;

    string(Atom) :
      Id = Atom/0 ;

    otherwise : Atom = _,
      Id = '?'/0 .
