/*

colon - Colon language source-to-source transformer for Logix

Author  - Alon Kleinman, 02/88
Revised - Bill Silverman, 01/89

Convert a list of language(colon) clauses to a list of normal clauses.

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:17 $
Currently locked by 	$Locker:  $
			$Revision: 1.1.1.1 $
			$Source: /home/qiana/Repository/Logix/system/transform/colon.cp,v $

Copyright (C) 1988, Weizmann Institute of Science - Rehovot, ISRAEL

*/

%=============================================================================%
% Fcp language tranformation for  colon .
%
% Input format:
% 
%  Source    ::= [Clause]
%  Clause    ::= Any ; (Any :- Any) ; Any :- Guard | Any.
%  Guard     ::= Any ; (Ask : Tell)
%  Ask       ::= Any ; (Any , Ask)
%  Tell      ::= Ask 
%
%==============================================================================

-export([transform/5]).
-mode(trust).
-language(compound).

/*
** transform - export
*/

procedure transform(Attributes, Clauses, Attributes, Clauses, Output).

transform(Attributes1, Clauses1, Attributes2, Clauses2, Output) :-
    true :
      Attributes1 = Attributes2,
      Output = [] |
	clauses(Clauses1, Clauses2).

/*
** clauses - transform a list of clauses
*/

clauses(CIn, COut) :-

    CIn ? (procedure Name) :		% procedure definition in typed_fcp -
      COut ! (procedure Name) |		% leave it as is
	clauses;

    CIn ? (LHS ::= Type) :		% type declaration in typed_fcp -
      COut ! (LHS ::= Type) |		% leave it as is
	clauses;

    CIn ? (H :- G | B) :		% replace head, guard H, G 
      COut ! (H' :- RHS) |		% by transformed H', RHS
	head(H, H', HeadAsks, Asks, HeadTells, Tells, TellIndex),
	guard(G, Asks, Tells, TellIndex),
	right_hand_side(HeadAsks, HeadTells, B, RHS),
	clauses;

    CIn ?  (H :- B),			% replace head H by transformed H'
    B =\= (_ | _), B =\= (_ : _) :	% create guard, producing RHS
      COut ! (H' :- RHS) |
	create_guard(H, H', B, RHS),
	clauses;

    CIn ?  (H :- G),			% iterate with canonical clause
    G = (_ : _) :
      CIn'' = [(H :- G | true) | CIn'] |
	clauses;

    otherwise,				%		else
    CIn ? H :				% replace head H by transformed H'
      COut ! (H'? :- RHS) |		% create guard, producing RHS
	create_guard(H, H', true, RHS),
	clauses;

    CIn = [] :
      COut = [] .

/*
Predicate ::= Tuple ; String.

Head, Head1, Head2 ::= Predicate.
HeadAsks1, HeadAsks2 ::= Asks.
HeadTells1, HeadTells2 ::= Tells.
Asks ::= [AskPredicate].
Tells ::= [TellPredicate].
TellIndex ::= Integer.
*/

create_guard(Head1, Head2, Body, RHS) :-
	head(Head1, Head2, HeadAsks, [], HeadTells, [], _),
	right_hand_side(HeadAsks, HeadTells, Body, RHS).

/*
Terms ::= [Term].
Term ::= Any.
Conjunction ::= Term ; (Term, Conjunction).
*/

right_hand_side(Asks, Tells, Body, RHS) :-

    Asks = [],
    Tells = [] :
      RHS = Body ;

    Asks = [],
    Tells ? Tell,
    Body =\= true :
      RHS = (true : Conjunction | Body) |
	conjunction(Tells', Tell, Conjunction);

    Asks = [],
    Tells ? Tell,
    Body = true :
      RHS = (true : Conjunction) |
	conjunction(Tells', Tell, Conjunction);

    Asks ? Ask,
    Tells = [] :
      RHS = (Conjunction : true | Body) |
      	conjunction(Asks', Ask, Conjunction);

    Asks ? Ask,
    Tells ? Tell,
    Body =\= true :
      RHS = (AskConjunction : TellConjunction | Body) |
	conjunction(Asks', Ask, AskConjunction),
	conjunction(Tells', Tell, TellConjunction);

    Asks ? Ask,
    Tells ? Tell,
    Body = true :
      RHS = (AskConjunction : TellConjunction) |
	conjunction(Asks', Ask, AskConjunction),
	conjunction(Tells', Tell, TellConjunction).


conjunction(Terms, Term, Conjunction) :-

    Terms ? Term' :
      Conjunction = (Term, Conjunction') |
	conjunction;

    Terms = [] :
      Term = Conjunction . 

/*
** head - transform head of clause
*/

head(Head1, Head, HeadAsks1, HeadAsks2, HeadTells1, HeadTells2, TellIndex) :-

    tuple(Head1),
    Arity := arity(Head1),		% prepare an empty head Head1
    make_tuple(Arity, Head2),
    arg(1, Head1, Functor^),		% copy head functor
    arg(1, Head2, Functor) |
	head_terms(Arity, Head1, Head2, Head, HeadAsks1, HeadAsks2,
		   HeadTells1, HeadTells2, TellIndex
	);

    otherwise :				% string head (or error)
      Head1 = Head,
      HeadAsks1 = HeadAsks2,
      HeadTells1 = HeadTells2,
      TellIndex = 1 .

head_terms(HeadIndex, Head1, Head2, Head, HeadAsks1, HeadAsks2,
		 HeadTells1, HeadTells2, TellIndex) +
			(TIX = 1, ArgVars = _) :-

    HeadIndex > 1,
    arg(HeadIndex, Head1, Arg1),
    arg(HeadIndex, Head2, Arg2),
    HeadIndex' := HeadIndex - 1 :
      Arg2 = Arg2'? |
	head_argument(Arg1, HeadIndex', Arg2', HeadAsks1, HeadAsks1',
			HeadTells1, HeadTells1', TIX, TIX', ArgVars
	),
	head_terms;

    HeadIndex = 1 : Head1 = _, ArgVars = _,
      Head2 = Head,
      HeadAsks1 = HeadAsks2,
      HeadTells1 = HeadTells2,
      TellIndex = TIX .

/*
** head_argument - replace an argument by a created var if not first
**                 occurence of variable.
*/

head_argument(Arg1, HeadIndex, Arg2, HeadAsks1, HeadAsks2,
		HeadTells1, HeadTells2, TIX1, TIX2, ArgVars
) :-

    Arg1 = `'_' : HeadIndex = _, ArgVars = _,
      Arg2 = `'_',			% leave anonymous var as is
      HeadAsks1 = HeadAsks2,
      HeadTells1 = HeadTells2,
      TIX1 = TIX2 ;

    Arg1 = `Id, Id =\= '_' :
      Arg2 = `Id'?,			% replace variable if not first
      HeadTells1 = HeadTells2,
      TIX1 = TIX2 |
	head_variable(Id, HeadIndex, Id', HeadAsks1, HeadAsks2, ArgVars);

    Arg1 = {HAT, Term}, HAT = '^',
    TIX2^ := TIX1 + 1 : HeadIndex = _, ArgVars = _,
      HeadAsks1 = HeadAsks2,
      Arg2 = `colon_tell(TIX1),		% extract hat-annotated term to tell
      HeadTells1 = [(Arg2 = Term) | HeadTells2] ;

    Arg1 = ?Id,
    TIX2^ := TIX1 + 1 : HeadIndex = _, ArgVars = _,
      HeadAsks1 = HeadAsks2,
      Arg2 = `colon_tell(TIX1),		% extract read-only variable to tell
      HeadTells1 = [(`colon_tell(TIX1) = ?Id) | HeadTells2] ;

    otherwise : ArgVars = _,
      Arg2 = `colon_ask(HeadIndex),
      HeadAsks1 = [(`colon_ask(HeadIndex) =?= Term?) | HeadAsks2] |
	annotated(Arg1, Term, HeadTells1, HeadTells2, TIX1, TIX2).


head_variable(Id1, HeadIndex, Id2, HeadAsks1, HeadAsks2, ArgVars) :-

    ArgVars ? Idx, Id1 =\= Idx |
	head_variable;

    ArgVars ? Id1 : ArgVars' = _,
      Id2 = `colon_ask(HeadIndex),
      HeadAsks1 = [(`Id2 =?= `Id1) | HeadAsks2] ;

    true :
      ArgVars ! Id2?, ArgVars' = _, HeadIndex = _,
      HeadAsks1 = HeadAsks2 |
	Id1 = Id2.

/*
** guard - transform the guard into a list of un-annotated ask predicates,
** changing predicate "="/2 into "=?="/2, and a list of tell predicates.
**
*/

/*
Guard ::= (Ask : Tell) ; Ask.
Ask ::= AskUnify ; AskPredicate ; (Ask, Ask).
AskUnify ::= Any = Any.
Tell ::= TellPredicate ; (Tell, Tell).
AskPredicate, TellPredicate ::= Predicate.
EndAsk ::= Asks ; Tells.
*/

guard(Guard, Asks, Tells, TellIndex) :-

    Guard = (Ask : Tell) |			% there is Ask and Tell
	ask(Ask, Asks, AskTell, TellIndex),
	tell(Tell, Tells, AskTell);

    otherwise |					% only Ask, no Tell
	ask(Guard, Asks, Tells, TellIndex).


ask(Ask, Asks, Tells, TellIndex) + (EndAsk = [], EndTell = [], TIX = _) :-

    Ask = (Ask', Ask1) |
	ask,
	ask(Ask1, EndAsk', EndTell', TIX', EndAsk, EndTell, TIX);

    Ask = (Term1 = Term2) :
      Asks = [(Term1'? =?= Term2'?) | EndAsk] |
	annotated(Term1, Term1', Tells, Tells', TellIndex, TellIndex'),
	annotated(Term2, Term2', Tells', EndTell, TellIndex', TIX);

   Ask = true :
      Asks = EndAsk,
      Tells = EndTell,
      TellIndex = TIX ;

    otherwise :
      Asks = [Ask' | EndAsk] |
	annotated(Ask, Ask', Tells, EndTell, TellIndex, TIX).


tell(Tell, Tells, EndTell) :-

    Tell = (Tell'', Tell') |
	tell,
	tell(Tell', EndTell', EndTell);

    Tell = true :
      Tells = EndTell ;

    otherwise :
      Tells = [Tell | EndTell] .

/*
** annotated - transform all annotated terms to  Tell  unification
*/

annotated(Term1, Term2, Tells, EndTell, TIX1, TIX2) :-

    tuple(Term1) |
	annotated_tuple(Term1, Term2, Tells, EndTell, TIX1, TIX2);

    Term1 = [Term1' | CDR1] :
      Term2 = [Term2'? | CDR2?] |
	annotated,
	annotated(CDR1, CDR2, EndTell', EndTell, TIX2', TIX2);

    otherwise :
      Term1 = Term2,
      Tells = EndTell,
      TIX1 = TIX2 .

annotated_tuple(Term1, Term2, Tells, EndTell, TIX1, TIX2) :-

    Term1 = `_ :
      Term1 = Term2,
      Tells = EndTell,
      TIX1 = TIX2 ;

    Term1 = ?_,
    TIX2^ := TIX1 + 1 :
      Term2 = `colon_tell(TIX1),	% extract read-only variable to tell
      Tells = [(Term2 = Term1) | EndTell] ;

    Term1 = {HAT, Term}, HAT = '^',
    TIX2^ := TIX1 + 1 :
      Term2 = `colon_tell(TIX1),	% extract hat-annotated term to tell
      Tells = [(Term = Term2) | EndTell] ;

    otherwise,
    Arity := arity(Term1),
    make_tuple(Arity, Tuple) |
	annotated_args(Arity, Term1, Tuple, Term2, Tells, EndTell, TIX1, TIX2).

annotated_args(Index, Tuple1, Tuple2, Tuple, Tells, EndTell, TIX1, TIX2) :-
    Index > 0,
    arg(Index, Tuple1, Arg1),
    arg(Index, Tuple2, Arg2),
    Index' := Index - 1 :
      Arg2 = Arg2'? |
	annotated_args,
	annotated(Arg1, Arg2', Tells, Tells', TIX2', TIX2);

    Index = 0 : Tuple1 = _,
      Tuple2 = Tuple,
      Tells = EndTell,
      TIX1 = TIX2 .
