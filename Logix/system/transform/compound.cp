/*
Precompiler for compound procedures.

Bill Silverman, April 1987.

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:15 $
Currently locked by 	$Locker:  $
			$Revision: 1.1.1.1 $
			$Source: /home/qiana/Repository/Logix/system/transform/compound.cp,v $

Copyright (C) 1988, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([transform/5]).
-mode(trust).
-language(compound).

Source ::= [Any].

Term ::= Any.
Terms ::= [Term].
Errors ::= [Any].
TermList ::= [Any | Terms].

procedure transform(Any, Terms, Any, Terms, Errors).

transform(Attributes1, Source, Attributes2, Terms, Errors) :-
	Attributes1 = Attributes2,
	stream # hash_table(Hash?),
	clauses(Source, Terms, Errors, Hash, Refs, Refs).


Tree ::= {Any, Tree, Tree} ; [].
LookupTree ::= {String(Any), Any, Any}.
Server, AddClauses ::= true ; false.
PID ::= String/Integer.
Predicate ::= Tuple ; String.
Variable ::= {`"`", Any}.
Descriptor ::= {Integer, VTree}.
FindVReply ::= found(VData) ; [].
FindReply ::= FindVReply.
FindDescriptor ::= Descriptor ; [].
VTree ::= {String(VData), VTree, VTree} ; [].
VData ::= Integer.
Duped ::= unique(VTree) ; ambiguous(VTree).
Defs ::= Refs ; [lookup(String, Duped, Duped, (new ; old)) | Defs].
Refs ::= [member(String, Duped, (true ; false))].

procedure clauses(Terms, Terms, Errors, Defs, Refs, Refs).

clauses(Source, Terms, Errors, Defs, EndDefs, Refs) :-

    Source ? (P :- RHSS) |
	self,
	compound(P, RHSS, Terms, Terms', Errors, Errors',
		 Defs, Defs', Refs, Refs'
	);

    Source ? P,
    P =\= (_ :- _) |
	clauses,
	compound(P, true, Term, [], Errors, Errors',
		 Defs, Defs', Refs, Refs'
	),
	fact(Term, Terms, Terms');

    Source = [] :
      Terms = [],
      Errors = [],
      Defs = EndDefs,
      Refs = [] .

fact([(Fact :- true)], [Fact | Terms]^, Terms).
fact([Global, (Fact :- true)], [Global, Fact | Terms]^, Terms).


RHSS ::= RHS ; {`";", RHS, RHSS}.
RHS ::= CommaList ; (Guard | CommaList).
Guard ::= CommaList ; (CommaList : CommaList).
CommaList, Ask, Tell, Body ::= Any ; (Any , CommaList).


procedure compound(Any, RHSS, Terms, Terms, Errors, Errors,
			 Defs, Defs, Refs, Refs
).

compound(Head, RHSS, Terms, Terms1, Errors, Errors1,
	 Defs1, Defs2, Refs1, Refs2
) :-

    tuple(Head),
    Head =\= (_ + _),
    RHSS =\= (_ ; _) |
	head_arguments(Head, Args),
	compound_clause(Args, Head, RHSS, Terms, Terms1, Errors, Errors1,
	 		 Defs1, Defs2, Refs1, Refs2
	);

    Head = (Head' + Initial) :
      Terms = [(Head' :- Call?) | Terms'?] |
	factor_goal(Head', Functor, Arity),
	complete_definition(Functor, Arity, Head', Initial, Call,
				Definition1, FullArity
	),
	define(Functor, FullArity, Definition1, Definition2, Descriptor,
		Errors, Errors1, Defs1, Defs2
	),
	compound1(Descriptor, Definition2, RHSS, Terms', Terms1,
			Refs1, Refs2
	);

    otherwise |
	factor_goal(Head, Functor, Arity),
	define(Functor, Arity, Head, Definition, Descriptor,
		Errors, Errors1, Defs1, Defs2
	),
	compound1(Descriptor, Definition, RHSS, Terms, Terms1,
			Refs1, Refs2
	).


TRequests ::= [TRequest].
TRequest ::= iterate(Predicate) ; % call(Predicate, Predicate) ;
	     readonly(String) ;
	     variable(Any).
DRequests ::= [DRequest | DRequests] ; primed(VTree).
NDRequests ::= [] ; DRequests.
LNDRequests ::= NDRequests ; [Error | Nil].
Error ::= error(Any).
DRequest ::= enter(Any, Integer) ; Error.
LookupReply ::= old ; new(Any).

Args ::= writable ; other.


procedure head_arguments(Predicate, Args).
procedure head_arguments(Predicate, Args, Integer).

head_arguments(Head, Args) + (Index = 2) :-

    arg(Index, Head, `Variable),
    Variable =\= '_',
    Index' := Index + 1 |
	self;

    Index > arity(Head) :
      Args = writable ;

    otherwise : Head = _, Index = _,
      Args = other .


procedure compound_clause(Args, Predicate, RHSS, Terms, Terms, Errors, Errors).

compound_clause(Args, Head, RHSS, Terms, Terms1, Errors, Errors1,
 		 Defs1, Defs2, Refs1, Refs2
) :-
    Args = other :
      Terms = [(Head :- RHSS) | Terms1],
      Errors = Errors1,
      Defs1 = Defs2,
      Refs1 = Refs2 ;

    Args = other :
      PID = Functor? / Arity?,
      Terms = [(Head :- RHSS1?) | Terms1],
      Defs1 = Defs2 |
	factor_goal(Head, Functor, Arity),
	serve_plain(Ss?, PID, Errors, Errors1),
	transform_rhs(PID, RHSS, RHSS1, Ss, Refs1, Refs2);

    Args = writable |
	factor_goal(Head, Functor, Arity),
	define(Functor, Arity, Head, Definition, Descriptor,
		Errors, Errors1, Defs1, Defs2
	),
	make_clause(Descriptor, Definition, RHSS, Terms, Terms1,
			Refs1, Refs2
	).

procedure serve_plain(TRequests, PID, Errors1, Errors2).

serve_plain(Ss, PID, Errors1, Errors2) :-

    Ss ? iterate(true^) :
      Errors1 ! iterating_plain_clause - PID |
	self;

    Ss ? substitute(_, true^) :
      Errors1 ! iterating_plain_clause - PID |
	self;

    Ss ? alternate([]^) :
      Errors1 ! implicit_call_from_plain_clause - PID |
	self;

    Ss ? Other,
    Other =\= iterate(_), Other =\= substitute(_, _), Other =\= alternate(_) |
	self;

    Ss = [] : PID = _,
      Errors1 = Errors2 .


procedure compound1(Descriptor, Definition, RHSS, Terms, Terms,
			Refs, Refs
).

compound1(Descriptor, Definition, RHSS, Terms1, Terms2, Refs1, Refs2) :-

    RHSS = (RHS ; RHSS'),
    RHS = (_;_) |
	compound1(Descriptor, Definition, RHS, Terms1, Terms1', Refs1, Refs1'),
	self;

    RHSS = (RHS ; RHSS'),
    RHS =\= (_;_) |
	make_clause(Descriptor, Definition, RHS, Terms1, Terms1', Refs1, Refs1'),
	self;

    otherwise |
	make_clause(Descriptor, Definition, RHSS, Terms1, Terms2, Refs1, Refs2).


procedure make_clause(FindDescriptor, Definition, RHS, Terms, Terms,
			Refs, Refs
).
make_clause(Descriptor, Definition, RHS, Terms1, Terms2, Refs1, Refs2) :-
    Descriptor = {Arity, PVD},
    Definition = {Head, Predicate} :
      Terms1 = [(Header? :- RHS1?) | Terms2?] |
	serve_transform(Ss, PVD, Head, Predicate, Header),
	factor_goal(Predicate, Functor, _),
	transform_rhs(Functor / Arity, RHS, RHS1, Ss, Refs1, Refs2).

make_clause([], _, _, Terms^, Terms, Refs^, Refs).


procedure serve_transform(TRequests, VTree, Predicate, Predicate, Predicate).
procedure serve_transform(TRequests, VTree, Predicate, Predicate, Predicate,
			Predicate, Predicate, Predicate, TRequests, TRequests
).

serve_transform(TRequests, VTree, Head, Predicate, Header) :-

    string(Predicate) |
	serve_transform(TRequests, VTree, Head, Predicate, Header, _Iterate,
				Predicate, Predicate, Variables, Variables
	);

    A := arity(Predicate),
    arg(1, Predicate, Functor),
    make_tuple(A, AHead),
    arg(1, AHead, Functor),
    make_tuple(A, AIter),
    arg(1, AIter, Functor) |
	serve_transform(TRequests, VTree, Head, Predicate, Header, _Iterate,
				AHead, AIter, Variables, Variables
	).

serve_transform(TRequests, VTree, Head, Predicate, Header, Iterate,
		AHead, AIter, AVars, Vars
) :-

    TRequests ? iterate((Iterate?)^) |
	self;

    TRequests ? substitute(Associates, Goal) |
	associate(VTree, Associates, Associations),	% obsolescent code
	substitute(Associates, Associations, Iterate, Goal),
	self;

    TRequests ? Var, Var = variable(Name, Variable) :
      Vars ! Var |
	find_variable(VTree, Name, Result),
	found_variable(Result?, Name, Variable, AIter, AIter'),
	self;

    TRequests ? output(Variable) |
	head_variable(2, Predicate, Variable, AHead, AHead'),
	self;

    TRequests ? alternate(AVars^) |
	self;

    TRequests = [],
    string(Predicate) : VTree = _, AHead = _, AIter = _, AVars = _,
      Predicate = Iterate,
      Header = Head,
      Vars = [] ;

    TRequests = [],
    tuple(Predicate) : VTree = _, AVars = _,
      Vars = [] |
	copy_args(1, Head, AHead, Header),
	copy_args(1, Predicate, AIter, Iterate).


/* * * * * * * * * * * * Obsolescent code * * * * * * * * * * * * * * * * * */
/* Associate variable must have a string name. */

procedure associate(VTree, Associates, Associations).
associate(VTree, Associates, Associations) :-

    Associates = (`V, Associates') :
      Associates'' = (`V = `V, Associates') |
	self;

    Associates = `V :
      Associates'' = (`V = `V) |
	self;

    Associates = (`V = E, Associates'),
    string_to_dlist(V, VL, [Prime]) :
      ascii("'", Prime),
      Associations ! N(E) |
	list_to_string(VL, Primed),
	find_data(VTree, Primed, N),
	self;

    Associates = (`V = E),
    string_to_dlist(V, VL, [Prime]) :
      ascii("'", Prime),
      Associations = [N(E)] |
	list_to_string(VL, Primed),
	find_data(VTree, Primed, N);

    otherwise : VTree = _,
      Associations = [syntax_error(Associates)] .
/* * * * * * * * * * * End Obsolescent code * * * * * * * * * * * * * * * * */

procedure substitute(Associates, Associations, Iterate, Goal).
substitute(Associates, Associations, Iterate, Goal) :-
    A := arity(Iterate),
    make_tuple(A, Substitute) |
/* * * * * * * * * * * * Obsolescent code * * * * * * * * * * * * * * * * * */
	substitutes(Associates, Associations, Iterate, Goal, Substitute).

procedure substitutes(Associates, Associations, Predicate, Predicate, Predicate).

substitutes(Associates, Associations, Iterate, Goal, Substitute) :-

    Associations ? N(E), integer(N),
    arg(N, Substitute, (X?)^) |
	self,
	X = E? ;

    Associations = [Diagnostic(_) | _],
    string(Diagnostic),
    arg(1, Iterate, Functor) : Substitute = _,
      Goal = Functor+Associates ;

    Associations = [] : Associates = _ |
/* * * * * * * * * * * End Obsolescent code * * * * * * * * * * * * * * * * */
	copy_args(0, Iterate, Substitute, Goal).


procedure head_variable(Integer, Tuple, Tuple, Tuple, Tuple).

head_variable(Index, Predicate, Variable, From, To) :-

    Arity := arity(Predicate),
    Index =< Arity,
    arg(Index, Predicate, Variable),
    arg(Index, From, Enter),
    Variable = `Name :
      ?Name = Enter,
      From = To ;

    Arity := arity(Predicate),
    Index =< Arity,
    arg(Index, Predicate, Other),
    Variable =\= Other,
    Index' := Index + 1 |
	self;

    otherwise : Index = _, Predicate = _, Variable = _,
      From = To .

procedure copy_args(Integer, Tuple, Tuple, Tuple).

copy_args(Index, From, Via, To) :-

    Index' := Index + 1,
    arg(Index', From, Arg), arg(Index', Via, (Arg?)^) |
	self;

    A := arity(From),
    Index >= A :
      Via = To ;

    otherwise,
    Index' := Index + 1 |
	self.


procedure transform_rhs(PID, RHS, RHS, TRequests, Refs, Refs).

transform_rhs(PID, RHS1, RHS2, TRequests, Refs1, Refs2) :-

    RHS1 = (Ask | Body), Ask =\= (_ : _) :
      RHS2 = (Ask1? | Body1?) |
	transform_ask(Ask, Ask1, TRequests, TRequests1),
	transform_body(PID, Body?, Body1, TRequests1, Refs1, Refs2);

    RHS1 = (Ask : Tell | Body) :
      RHS2 = (Ask1? | Body1?) |
	transform_ask(Ask, Ask1, TRequests, TRequests1),
	transform_body(PID, (Tell, Body), Body1, TRequests1, Refs1, Refs2);

    RHS1 = (Ask : Tell | Body) :
      RHS2 = (Ask1? : Tell1? | Body1?) |
	transform_ask(Ask, Ask1, TRequests, TRequests1),
	transform_tell(Tell, Tell1, TRequests1, TRequests2),
	transform_body(PID, Body, Body1, TRequests2, Refs1, Refs2);

    RHS1 = (Ask : Tell) : PID = _,
      RHS2 = (Ask1? : Tell1?),
      Refs1 = Refs2 |
	transform_ask(Ask, Ask1, TRequests, TRequests1),
	transform_tell(Tell, Tell1, TRequests1, []);

    otherwise |
	transform_body(PID, RHS1, RHS2, TRequests, Refs1, Refs2).


procedure transform_ask(Ask, Ask, TRequests, TRequests).

transform_ask(Ask1, Ask2, TRequests1, TRequests2) :-

    Ask1 = (A1, Ask1') :
      Ask2 = (A2?, Ask2'?) |
	self+(Ask1 = A1, Ask2 = A2, TRequests1, TRequests2 = TRequests1'),
	self;

    Ask1 = (`V ? T),
    string_to_dlist(V, L, [Prime]) :
      ascii("'", Prime),
      Ask1' = (`V =?= [T | `(V1?)]) |
	list_to_string(L, V1),
	self;

    Ask1 = (Variable += T), Variable = `V,
    string_to_dlist(V, L, [Prime]) :
      ascii("'", Prime),
      Ask1' = (`(V1?) := Variable + T) |
	list_to_string(L, V1),
	self;

    Ask1 = (Variable -= T), Variable = `V,
    string_to_dlist(V, L, [Prime]) :
      ascii("'", Prime),
      Ask1' = (`(V1?) := Variable - T) |
	list_to_string(L, V1),
	self;

    Ask1 = {PP, Variable}, PP = "++", Variable = `V,
    string_to_dlist(V, L, [Prime]) :
      ascii("'", Prime),
      Ask1' = (`(V1?) := Variable + 1) |
	list_to_string(L, V1),
	self;

    Ask1 = {PP, Variable}, PP = "--", Variable = `V,
    string_to_dlist(V, L, [Prime]) :
      ascii("'", Prime),
      Ask1' = (`(V1?) := Variable - 1) |
	list_to_string(L, V1),
	self;

    otherwise |
	note_variables(Ask1, Ask1', TRequests1, TRequests1', Suffixes, []),
	filter_suffixes(Suffixes, Ask1', Ask2, TRequests1', TRequests2).

procedure transform_tell(Tell, Tell, TRequests, TRequests).

transform_tell(Tell1, Tell2, TRequests1, TRequests2) :-

    Tell1 = (T1, Tell1') :
      Tell2 = (T2?, Tell2'?) |
	self+(Tell1 = T1, Tell2 = T2, TRequests1, TRequests2 = TRequests1'),
	self;

    Tell1 = (Variable ! T), Variable = `V,
    string_to_dlist(V, L, [Prime]) :
      ascii("'", Prime),
      Tell1' = (Variable = [T | `(V1?)]) |
	list_to_string(L, V1),
	self;

    otherwise |
	note_variables(Tell1, Tell1', TRequests1, TRequests1', Suffixes, []),
	filter_suffixes(Suffixes, Tell1', Tell2, TRequests1', TRequests2).


procedure transform_body(PID, Body, Body, TRequests, Refs, Refs).

transform_body(PID, Body1, Body2, TRequests, Refs1, Refs2) :-
	transform_body(PID, Body1, Body1', TRequests, TRequests1',
		       Refs1, Refs2, Suffixes, []
	),
	filter_suffixes(Suffixes, Body1', Body2, TRequests1', []).

procedure transform_body(PID, Body, Body, TRequests, TRequests,
			 Refs, Refs, Suffixes, Suffixes
).

transform_body(PID, Body1, Body2, TRequests1, TRequests2,
		Refs1, Refs2, Suffixes1, Suffixes2
) :-
    Body1 = (B1, Body1') :
      Body2 = (B2?, Body2'?) |
	transform_body+(Body1 = B1, Body2 = B2,
			TRequests1, TRequests2 = TRequests1',
			Refs1, Refs2 = Refs1',
			Suffixes1, Suffixes2 = Suffixes1'
	),
	self;

    PID = Body1/Arity, Arity > 0 :
      Suffixes1 = Suffixes2,
      TRequests1 = [iterate(Body2) | TRequests2],
      Refs1 = Refs2 ;

    Body1 = self : PID = _,
      Suffixes1 = Suffixes2,
      TRequests1 = [iterate(Body2) | TRequests2],
      Refs1 = Refs2 ;

    PID = Functor/Arity, Arity > 0,			% obsolescent code!
    Body1 = Functor+Associates :
      Refs1 = Refs2 |
	note_added_variables(Associates, TRequests1,
		       [substitute(Associates, Body2) | TRequests2],
		       Suffixes1, Suffixes2
	);

    Body1 = self+Associates : PID = _,			% obsolescent code!
      Refs1 = Refs2 |
	note_added_variables(Associates, TRequests1,
		       [substitute(Associates, Body2) | TRequests2],
		       Suffixes1, Suffixes2
	);

    Body1 = (Variable ! T), Variable = `V,
    string_to_dlist(V, L, [Prime]) : PID = _,
      Refs1 = Refs2,
      ascii("'", Prime) |
	list_to_string(L, V1),
	note_variables(Variable = [T | `(V1?)], Body2, TRequests1, TRequests2,
		       Suffixes1, Suffixes2
	);

    Body1 = (Variable ! T), Variable = `V,
    string_to_dlist(V, L, [Prime]) : PID = _,
      Refs1 = Refs2,
      ascii("'", Prime),
      TRequests1 ! output(Variable) |
	list_to_string(L, V1),
	note_variables(Variable = [T | ?(V1?)], Body2, TRequests1', TRequests2,
		       Suffixes1, Suffixes2
	);

    Body1 = (Variable += T), Variable = `V,
    string_to_dlist(V, L, [Prime]) : PID = _,
      Refs1 = Refs2,
      ascii("'", Prime) |
	list_to_string(L, V1),
	note_variables(`(V1?) := Variable + T, Body2, TRequests1, TRequests2,
		       Suffixes1, Suffixes2
	);

    Body1 = (Variable -= T), Variable = `V,
    string_to_dlist(V, L, [Prime]) : PID = _,
      Refs1 = Refs2,
      ascii("'", Prime) |
	list_to_string(L, V1),
	note_variables(`(V1?) := Variable - T, Body2, TRequests1, TRequests2,
		       Suffixes1, Suffixes2
	);

    Body1 = {PP, Variable}, PP = "++", Variable = `_ :
      Refs1 = Refs2,
      Body1' = (Variable += 1) |
	self;

    Body1 = {PP, Variable}, PP = "--", Variable = `_ :
      Refs1 = Refs2,
      Body1' = (Variable -= 1) |
	self;

    Body1 ? CAR1 :
      Body2 ! CAR2? |
	transform_body+(Body1 = CAR1, Body2 = CAR2,
			TRequests1, TRequests2 = TRequests1',
			Refs1, Refs2 = Refs1',
			Suffixes1, Suffixes2 = Suffixes1'),
	self;

    PID = Functor/_,
    string(Body1),
    Body1 =\= Functor, Body1 =\= self, Body1 =\= true :
      Suffixes1 = Suffixes2,
      Refs1 ! member(Body1, Value, Reply), Refs1' = Refs2 |
	alternate(Body1, Body2, Suffixes1, Suffixes2, TRequests1, TRequests2,
			Value, Reply
	);

    PID = Functor/_,					% obsolescent code!
    Body1 = Alternate + Added,
    string(Alternate),
    tuple(Added),
    Alternate =\= Functor, Alternate =\= self :
      Refs1 ! member(Alternate, Value, Reply), Refs1' = Refs2 |
	alternate(Body1, Body2, Suffixes1, Suffixes2, TRequests1, TRequests2,
			Value, Reply
	);

    otherwise : PID = _,
      Refs1 = Refs2 |
	note_variables(Body1, Body2, TRequests1, TRequests2,
		       Suffixes1, Suffixes2
	).


alternate(Body1, Body2, Suffixes1, Suffixes2, TRequests1, TRequests2,
		Value, Reply
) :-
    Reply = true,
    Value = unique({Predicate, VTree}),
    string(Body1) :
      Suffixes1 = Suffixes2,
      TRequests1 ! alternate(Variables), TRequests1' = TRequests2 |
	transform_alternate(Body1, none, Body2, Predicate, VTree, Variables);
/*
      TRequests ! iterate(Body2), TRequests' = Variables |
	serve_transform(TRequests, VTree, {Body1}, Predicate, _Header);
*/
    Reply = true,					% obsolescent code!
    Value = unique({Predicate, VTree}),
    Body1 = Alternate + Added :
      TRequests1 ! alternate(Variables) |
	note_added_variables(Added, TRequests1', TRequests2,
		   	     Suffixes1, Suffixes2
	),
	transform_alternate(Alternate, Added, Body2, Predicate, VTree, Variables);
/*
      TRequests ! substitute(Added, Body2), TRequests' = Variables |
	serve_transform(TRequests, VTree, {Alternate}, Predicate, _Header);
*/
    otherwise : Reply = _, Value = _ |
	note_variables(Body1, Body2, TRequests1, TRequests2,
		       Suffixes1, Suffixes2
	).

transform_alternate(Alternate, Added, Body, Predicate, VTree, Variables) :-

    Added = none :
      TRequests ! iterate(Body), TRequests' = Variables |
	serve_transform(TRequests, VTree, {Alternate}, Predicate, _Header);

    Added =\= none :
      TRequests ! substitute(Added, Body), TRequests' = Variables |
	serve_transform(TRequests, VTree, {Alternate}, Predicate, _Header).

alternate_iterand(Args, Variables, Predicate1, Predicate2) :-

    Args-- > 1,
    arg(Args, Predicate1, `Name),
    arg(Args, Predicate2, Arg) |
	alternate_argument(Variables, Name, Arg),
	self;

    Args =< 1,
    arg(1, Predicate1, String),
    arg(1, Predicate2, Name) : Variables = _,
      Name = String .

alternate_argument(Variables, Name, Arg) :-

    Variables ? variable(Name, Arg^) : Variables' = _ ;

    Variables ? _,
    otherwise |
	self;

    Variables = [] :
      Arg = `Name .

note_added_variables(Added, TRequests1, TRequests2, Suffixes1, Suffixes2) :-

    Added = (`_, Added') |
	self;

    Added = (`_ = Term, Added') |
	note_variables(Term, _, TRequests1, TRequests1', Suffixes1, Suffixes1'),
	self;

    Added = (`_ = Term) |
	note_variables(Term, _, TRequests1, TRequests2, Suffixes1, Suffixes2);

    otherwise : Added = _,
      TRequests1 = TRequests2,
      Suffixes1 = Suffixes2 .


procedure filter_suffixes(Suffixes, Term, Term, TRequests, TRequests).

filter_suffixes(Suffixes, Term1, Term2, TRequests1, TRequests2) :-

    Suffixes ? {Op, SL, S},
    list_to_string(SL, S') :
      Term2 = (Term1, Term2'?),
      Term1' = (`(S') := {Op, `S, 1}),
      TRequests1 ! variable(S', `(S')) |
	self;

    Suffixes = [] :
      Term1 = Term2,
      TRequests1 = TRequests2 .


procedure note_variables(Term, Term, TRequests, TRequests, Suffixes, Suffixes).

note_variables(Term1, Term2, TRequests1, TRequests2, Suffixes1, Suffixes2) :-

    Term1 ? Car :
      Term2 ! Car'? |
	note_variables(Car?, Car', TRequests1, TRequests1', Suffixes1, Suffixes1'),
	self;

    Term1 = `V :
      Term1 = Term2,
      Suffixes1 = Suffixes2,
      TRequests1 = [variable(V, `(V?)) | TRequests2] ;

    Term1 = ?V :
      Term1 = Term2,
      Suffixes1 = Suffixes2,
      TRequests1 = [variable(V, `(V?)) | TRequests2] ;

    Term1 = {SuffixOp, _}, SuffixOp = "++" |
	note_operation(Term1, Term2, TRequests1, TRequests2,
		       Suffixes1, Suffixes2, "+"
	);

    Term1 = {SuffixOp, _}, SuffixOp = "--" |
	note_operation(Term1, Term2, TRequests1, TRequests2,
		       Suffixes1, Suffixes2, "-"
	);

    tuple(Term1), Term1 =\= `_,
    otherwise,
    A := arity(Term1),
    make_tuple(A, Tuple) |
	note_tuple(Term1, Term2, TRequests1, TRequests2,
		   Suffixes1, Suffixes2, Tuple, 1
	);

    otherwise :
      Term1 = Term2,
      Suffixes1 = Suffixes2,
      TRequests1 = TRequests2 .

note_operation(Term1, Term2, TRequests1, TRequests2, Suffixes1, Suffixes2,
		Operator
) :-

    Term1 = _(`S),
    string(S),
    string_to_dlist(S, SL, [Prime]) :
      ascii("'", Prime),
      Suffixes1 = [{Operator, SL, S} | Suffixes2],
      Term2 = `S,
      TRequests1 = TRequests2 ;

    Term1 = SuffixOp(Term1'),
    otherwise : Operator = _,
      Term2 = SuffixOp(Term2') |
	note_variables(Term1'?, Term2', TRequests1, TRequests2,
		       Suffixes1, Suffixes2
	).


procedure note_tuple(Tuple, Tuple, TRequests, TRequests,
		     Suffixes, Suffixes, Tuple, Integer
).
note_tuple(Tuple1, TupleOut, TRequests1, TRequests2,
	   Suffixes1, Suffixes2, Tuple2, Index
) :-

    arg(Index, Tuple1, Arg1),
    arg(Index, Tuple2, Arg2),
    Index' := Index + 1 :
      Arg2 = Arg? |
	note_variables(Arg1, Arg, TRequests1, TRequests1',
		       Suffixes1, Suffixes1'
	),
	self;

    otherwise : Index = _, Tuple1 = _,
      TupleOut = Tuple2,
      Suffixes1 = Suffixes2,
      TRequests1 = TRequests2 .


procedure factor_goal(Any, String, Integer).

factor_goal(Goal, Functor, Arity) :-

    A := arity(Goal),
    arg(1, Goal, Name), string(Name),
    Arity^ := A - 1 :
      Name = Functor ;

    string(Goal) :
      Goal = Functor,
      Arity = 0 ;

    otherwise : Goal = _,
      Functor = '_',
      Arity = 0 .


procedure complete_definition(String, Integer, Tuple, Tuple, CommaList,
				Tuple, Integer
).

complete_definition(Functor, Arity, Public, Private, Call, Tuple, FullArity) :-
	split_arguments(Private, Variables, Arguments, 1, Count),
	FullArity := Arity + Count,
	Max := FullArity + 1,
	copy_arguments(Functor, Arity, Public, Variables, Max, Tuple),
	copy_arguments(Functor, Arity, Public, Arguments, Max, Call).


procedure split_arguments(Tuple, TermList, TermList, Integer, Integer).

split_arguments(Private, Variables, Arguments, Index, Max) :-

    Private = (A, Private'),
    Index' := Index + 1 :
      Variables ! V?,
      Arguments ! T? |
	self,
	split_argument(A, V, T);

    otherwise |
	split_argument(Private, V, T),
	Variables = [V?],
	Arguments = [T?],
	Max = Index.


procedure split_argument(Any, Term, Term).

split_argument(V=T, V^, T^).
split_argument(V, V^, V^) :-
    otherwise |
	true.


procedure arg(Integer, Tuple, Any, Tuple).

arg(N, P, A, P^) :-
    arg(N,P,A^) |
	true.


procedure copy_arguments(String, Integer, Tuple, Terms, Integer, Tuple).

copy_arguments(Functor, Arity, Tuple, Terms, Index, To) :-
    make_tuple(Index, R0), M := Arity + 1,
    arg(1, R0, Functor) |
	copy_tuple(M, Tuple, R0, R1),
	copy_argument_list(Terms, M, R1?, To).


procedure copy_tuple(Integer, Tuple, Tuple, Tuple).

copy_tuple(Index, From, Via, To) :-

    Index > 1, arg(Index, From, A),
    Index' := Index - 1 |
	self,
	arg(Index, Via, A?, Via');

    Index = 1 : From = _,
      Via = To .

procedure copy_unanotated_variables(Integer, Tuple, Tuple, Tuple).

copy_unanotated_variables(A, From, Via, To) :-

    A-- > 1,
    arg(A, From, ?V),
    arg(A, Via, WE) :
      WE = `V |
	self;

    A-- > 1,
    arg(A, From, A1),
    A1 =\= ?_,
    arg(A, Via, A2) :
      A2 = A1 |
	self;

    A =< 1 : From = _,
      To = Via .

procedure copy_argument_list(Terms, Integer, Tuple, Tuple).

copy_argument_list(Terms, Index, Via, To) :-

    Terms ? A |
	copy_argument_list,
	Index' := Index + 1,
	arg(Index', Via, A, Via');

    Terms = [] : Index = _,
      Via = To .


procedure define(String, Integer, Definition, FindDescriptor, Predicate,
			Errors, Errors, Defs, Defs
).

define(Functor, Arity, Definition1, Definition2, Descriptor,
	Errors1, Errors2, Defs1, Defs2
) :-

    Functor =\= '_' :
      Descriptor = {Arity, VTree},
      Defs1 ! lookup(Functor, New, Old, Reply), Defs1' = Defs2 |
	serve_definition(NDRequests, Functor/Arity, VTree, Errors1, Errors2),
	primed_predicate(Definition1, Definition2, VTree, NDRequests),
	Definition2? = {_Head, Accumulate},
	complete_lookup(Reply, {Accumulate, VTree}, Old, New);

    Functor = '_' : Arity = _,
      Definition2 = {Definition1, Definition1},
      Descriptor = [],
      Errors1 = [invalid_definition - Definition1 | Errors2],
      Defs1 = Defs2 .


complete_lookup(Reply, PVTree, Old, New) :-

    Reply = new : Old = _,
      New = unique(PVTree) ;

    Reply = old,
    Old = unique(PVTree) :
      New = Old ;

    Reply = old,
    Old =\= unique(PVTree) :
      New = ambiguous(PVTree) .


procedure serve_definition(NDRequests, PID, VTree, Errors, Errors).

serve_definition(NDRequests, PID, VTree, Errors1, Errors2) :-

    NDRequests ? error(E) :
      Errors1 ! (PID, E) |
	self;

    NDRequests ? enter(S, N),
    string_to_dlist(S, L, [Prime]) :
      ascii("'", Prime) |
	list_to_string(L, V),
	lookup(VTree, V, Reply),
	enter_primed_variable(Reply?, S, N, NDRequests', NDRequests''),
	self;

    NDRequests ? enter(_, _),
    otherwise |			% ignore non-string variable id
      self;

    NDRequests = primed(VTree) : PID = _,
      Errors1 = Errors2 |
	close_tree(VTree);

    NDRequests = [] : PID = _, VTree = _,
      Errors1 = Errors2 .


procedure enter_primed_variable(LookupReply, String, Integer, Errors, Errors).

enter_primed_variable(old, S, N, Es, [error(duplicate_variable(N)-S) | Es]^).
enter_primed_variable(new(N1), _, N, Es^, Es) :-
    N1^ := N + 1 | true.


procedure primed_predicate(Predicate, Predicate, VTree, NDRequests).

primed_predicate(Head, Definition, VTree, NDRequests) :-

    N := arity(Head), arg(1, Head, P),
    make_tuple(N, Accumulate), arg(1, Accumulate, P) | 
	primed_variables(1, N, Head, Accumulate, Definition,
			 VTree, NDRequests
	);

    string(Head) :
      Definition = {Head, Head},
      VTree = [],
      NDRequests = [] .


procedure primed_variables(Integer, Integer, Predicate, Tuple, Predicate,
				VTree, DRequests
).

primed_variables(Index, Max, Head, Accumulate, Definition, VTree, DRequests) :-

    Index < Max,
    Index' := Index + 1,
    arg(Index', Head, A), arg(Index', Accumulate, (`V?)^) :
      DRequests ! DR? |
	primed_variables,
	primed_variable(A, V, Index, DR);

    Index >= Max : Head = _,
      Definition = {Head, Accumulate},
      DRequests = primed(VTree) .


procedure primed_variable(Variable, String, Integer, DRequest).

primed_variable(Var, Name, Index, DReq) :-

    Var = `Id :
      Name = Id,
      DReq = enter(Id, Index);

    otherwise :
      Name = '_',
      DReq = error(invalid_variable(Index)-Var) .


procedure close_tree(Tree).

close_tree(Tree) :-

    Tree = {_Entry, Tree', RightTree} |
	close_tree,
	close_tree+(Tree = RightTree).

close_tree([]^).


procedure lookup(LookupTree, String, LookupReply).

lookup(LookupTree, String, LookupReply) :-

    LookupTree = { Other(_), LookupTree', _ }, 
    String @< Other |
	self;

    LookupTree = { Other(_), _, LookupTree' }, 
    Other @< String |
	self;

    LookupTree = { String(_), _, _ } :
      LookupReply = old ;

    true :
      LookupTree  = { String(Data?), _, _ },
      LookupReply =  new(Data) .


/*
** find_variable - search for String or a leading-string of String (where
**		   difference between String and its leading-string is any
**		   [finite] number of trailing primes) in binary VTree,
**		   returning result in FindVReply.
*/


procedure find_variable(VTree, String, FindVReply).

find_variable(VTree, String, FindVReply) :-

    VTree = {High(_), VTree', _}, 
    String @< High |
	self;

    VTree = {Key(Data), _, Right}, 
    string_to_dlist(Key, H_Key, T_Key),
    string_to_dlist(String, L_string, []) :
      H_Key = L_string |
	all_primes(T_Key, Result),
	check_all_primes(Result?, Data, Right, String, FindVReply);

    otherwise,
    VTree = {_, _, VTree'} |
	self;

    VTree = [] : String = _,
      FindVReply = [] .

/*
** find_data - search for Name
**	       returning Integer Data or none in FindIReply.
*/

FindIReply ::= Integer ; false.

procedure find_data(VTree, String, FindIReply).

find_data(VTree, Name, Data) :-

    VTree = {High(_), VTree', _}, 
    Name @< High |
	self;

    VTree = {Name(Value), _, _} :
      Data = Value ;

    VTree = {Low(_), _, VTree'},
    Low @< Name |
	self;

    VTree = [] : Name = _,
      Data = none .

/*
** all_primes - check if List contains only "primes" (ascii("'")),
**              returning Result = true/false.
*/

CharList ::= [Integer].
Result ::= true; false.


procedure all_primes(CharList, Result).

all_primes(CharList, Result) :-
    CharList ? Prime,
    Prime =:= ascii("'") |
	self.

all_primes([], true^).

all_primes(_, false^) :-
    otherwise |
	true.

/*
** check_all_primes -
**	if Result is 'true' then return Data as a reply in FindVReply
**	if Result is 'false' then continue to find_variable in VTree.
*/

procedure check_all_primes(Result, VData, VTree, String, FindVReply).

check_all_primes(true, VData, _, _, found(VData)^).

check_all_primes(false, _, VTree, String, FindVReply) :-
	find_variable(VTree, String, FindVReply).

/*
** found_variable: if a variable was found - (i.e. 1st argument is found(N)),
**		   then replace argument N of From by Name ONLY
**		   if argument N is uninstantiated, or if Name is more primed
**		   than argument N.
**
** The revised tuple is returned in To.
**
** CONVENTION: To is instantiated only when the tuple is completely revised.
*/

procedure found_variable(FindVReply, String, Tuple, Tuple, Tuple).

found_variable(FindVReply, Name, Variable, From, To):-

    FindVReply = found(N),
    arg(N, From, _(Arg)),
    Arg @< Name,
    Arity := arity(From),
    make_tuple(Arity, Via) |
	replace_copy(From, Via, To, N, Variable, Arity);

    FindVReply = found(N),
    arg(N, From, (Variable)^) : Name = _,
      To = From ;

    otherwise : FindVReply = _, Name = _, Variable = _,
      To = From .

/*
** replace_copy - copy From to To, changing argument N to be Variable .
**		  Copy is to Via, which is unified with To at termination.
*/

procedure replace_copy(Tuple, Tuple, Tuple, Integer, Tuple, Integer). 


replace_copy(From, Via, To, N, Variable, Index) :-

    Index =\= N,
    arg(Index, From, Arg),
    Index' := Index - 1,
    arg(Index, Via, Arg^) |
	self;

    Index = N,
    Index' := Index - 1,
    arg(N, Via, Variable^) |
	self.

replace_copy(_, To, To^, _, _, _) :-
    otherwise |
	true.
