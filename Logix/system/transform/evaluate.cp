/* $Header: /home/qiana/Repository/Logix/system/transform/evaluate.cp,v 1.1.1.1 1999/07/09 07:03:15 bill Exp $ */
-language(compound).
-export([clauses/3, transform/5]).
-mode(trust).


procedure transform(Any, Procedures, Any, Procedures, Errors).

transform(Attributes1, Source, Attributes2, Procedures, Errors) :-
	Attributes1 = Attributes2,
	clauses(Source, Procedures, Errors).

clauses(Source, Procedures, Errors) :-
	hash_dictionary(In?, Errors'),
	extract(Source, Compound, Clauses, In, In'?, Errors, Errors'?),
	compile_entries(Compound?, In', Requests?, true),
	compile_clauses(Clauses?, Requests, Procedures).

extract(Source, Compound, Clauses, In1, In2, E1, E2) :-

    Source ? Assign,
    Assign = (`V => Term) |
	verify_variable(V, Term, In1, In1', E1, E1'),
	self;

    Source ? Replace,
    Replace = (Tuple => Term),
    Tuple =\= `_,
    tuple(Tuple) |
	verify_tuple(Tuple, Term, In1, In1', E1, E1'),
	self;

    Source ? Illegal,
    Illegal = (_ => _),
    otherwise :
      E1 ! illegal_declaration(Illegal) |
	self;

    Source ? Clause,
    otherwise :
      Clauses ! Clause |
	self;

    Source = [] :
      Clauses = [],
      E1 = E2,
      In1 = [compound(Compound, []) | In2] .

verify_variable(V, Term, In, In', Error, Error') :-

    V =\= _("_"), V =\= _(`_), V =\= _(?_) :
      Var = `V,
      In ! add(Var, Var, Term),
      Error = Error' ;

    otherwise : V = _, Term = _,
      In = In',
      Error ! illegal_assigned_variable(`V) .
   
verify_tuple(Tuple, Term, In1, In2, E1, E2) :-

    arg(1, Tuple, `V),
    V =\= "_", V=\= `_, V =\= ?_,
    A := arity(Tuple) :
      Id = A(V), I = 2, Ok =  true |
	tree_dictionary(As, Dups, []),
	verify_arguments;

    arg(1, Tuple, Functor),
    otherwise : Term = _,
      In1 = In2,
      E1 = [illegal_replacement_functor(Functor) | E2].

verify_arguments(Tuple, Term, In1, In2, E1, E2, Id, Dups, A, As, I, Ok) :-

    I++ =< A,
    arg(I, Tuple, Var), Var = `V,
    V =\= "_", V =\= `_, V =\= ?_ :
      As ! add(Var, Var, []) |
	self;

    I++ =< A,
    otherwise : Ok = _,
      Ok' = false,
      E1 ! illegal_argument(I, Tuple) |
	self;

    I > A :
      As = [] |
	duplicate_arguments.


duplicate_arguments(Tuple, Term, In1, In2, E1, E2, Id, Dups, Ok) :-

    Dups = [],
    Ok = true :
      In1 = [add(Id, Tuple, Term) | In2],
      E1 = E2 ;

    Dups = [],
    Ok =\= true : Tuple = _, Term = _, Id = _,
      In1 = In2,
      E1 = E2 ;

    Dups =\= [] : Term = _, Id = _, Ok = _,
      In1 = In2,
      E1 = [duplicate_arguments(Tuple) | E2] .


compile_entries(Compound, In, Requests, Changed) :-

    Compound ? (LHS => RHS) |
	tree_dictionary(Args?, _, []),
	compile_entry(LHS?, Id, RHS?, RHS', In, In'?, Args),
	compare_entries(RHS?, RHS'?, Id?, In', In''?, Changed, Changed'),
	self;

    Compound = [] :
      In ! compound(Compound', []) |
	complete_dictionary.

complete_dictionary(Compound, In, Requests, Changed) :-

    Changed = true :
      Changed' = false |
	compile_entries;

    Changed = false : Compound = _,
      In = Requests .

compare_entries(RHS1, RHS2, Id, In, In', Changed, Changed') :-

    RHS1 = RHS2 : Id = _,
      In = In',
      Changed' = Changed ;

    RHS1 =\= RHS2 : Changed = _,
      In ! replace(Id, RHS2),
      Changed' = true .


compile_entry(LHS, Id, RHS, RHS', D1, D2, Args) :-

    LHS = `_ :
      Id = LHS |
	rhs_compile(RHS, RHS', D1, D2, Args, []);

    arg(1, LHS, `V),
    A := arity(LHS) :
      Id = A(V) |
	macro_args(1, LHS, Args, Args'?),
	rhs_compile(RHS, RHS', D1, D2, Args', []).

macro_args(I, T, A1, A2) :-

   I++,
   arg(I', T, Var) :
     A1 ! add(Var, Var, I) |
	self;

   otherwise : I = _, T = _,
     A1 = A2 .


rhs_compile(T1, T2, D1, D2, A1, A2) :-

    constant(T1) :
      T1 = T2,
      D1 = D2,
      A1 = A2 ;

    T1 ? Car :
      T2 ! Car' |
	rhs_compile(Car, Car', D1, D1', A1, A1'),
	self;

    T1 = `"_" :
      T1 = T2,
      D1 = D2,
      A1 = A2 ;

    T1 = `V, V =\= "_" :
      A1 = [member(T1, Answer) | A2] |
	rhs_assign;

    T1 = ?V,
    V =\= "_", V =\= `_, V =\= ?_ :
      A1 = [member(`V, Answer) | A2] |
	rhs_assign;

    tuple(T1),
    arg(1, T1, `V), string(V), V =\= "_",
    A := arity(T1) :
      D1 ! member(A(V), Answer) |
	rhs_replace;

    tuple(T1),
    otherwise,
    A := arity(T1),
    make_tuple(A, T2^) |
	rhs_compile_arguments.

rhs_assign(T1, T2, D1, D2, Answer) :-

    Answer = false :
      D1 = [member(T1, Answer') | D2] |
	rhs_substitute;

    Answer =\= false :
      T2 = T1,
      D1 = D2 .

rhs_substitute(Answer, T1, T2) :-

    Answer = {_LHS, RHS},
    T1 = ?_, RHS = `V, V =\= `_, V =\= ?_ :
      T2 = ?V ;

    Answer = {_LHS, RHS},
    otherwise : T1 = _,
      T2 = RHS ;

    Answer =\= {_, _} :
      T2 = T1 .

rhs_replace(T1, T2, D1, D2, A1, A2, A, Answer) :-

    Answer =\= {_, _},
    make_tuple(A, T2^) |
	rhs_compile_arguments;

    Answer = {LHS, RHS} :
      D1 = D2,
      A1 = A2 |
	tree_dictionary(S1?, _, []),
	substitutions,
	rhs_replaces(RHS, T2, S2, []).

rhs_compile_arguments(T1, T2, D1, D2, A1, A2, A) :-

    A-- >= 1,
    arg(A, T1, T),
    arg(A, T2, T') |
	rhs_compile(T, T', D1, D1', A1, A1'),
	self;

    A < 1 : T1 = _, T2 = _,
      D1 = D2,
      A1 = A2 .

rhs_replaces(T1, T2, D1, D2) :-

    constant(T1) :
      T1 = T2,
      D1 = D2 ;

    T1 ? Car :
      T2 ! Car' |
	rhs_replaces(Car, Car', D1, D1'),
	self;

    T1 = `"_" :
      T1 = T2,
      D1 = D2 ;

    T1 = `V, V =\= "_" :
      D1 = [member(T1, Answer) | D2] |
	term_substitute;

    T1 = ?V, V =\= "_" :
      D1 = [member(`V, Answer) | D2] |
	term_substitute;

    tuple(T1),
    otherwise,
    A := arity(T1),
    make_tuple(A, T2^) |
	rhs_substitute_arguments.
	
rhs_substitute_arguments(T1, T2, D1, D2, A) :-

    A-- >= 1,
    arg(A, T1, T),
    arg(A, T2, T') |
	rhs_replaces(T, T', D1, D1'),
	self;

    A < 1 : T1 = _, T2 = _,
      D1 = D2 .


substitutions(A, LHS, T1, S1, S2) :-

    A > 1,
    arg(A, LHS, Var),
    arg(A--, T1, Arg) :
      S1 ! add(Var, Var, Arg) |
	self;

    A =< 1 : LHS = _, T1 = _,
      S1 = S2 .


term_compile(T1, T2, D1, D2) :-

    constant(T1) :
      T1 = T2,
      D1 = D2 ;

    T1 ? Car :
      T2 ! Car' |
	term_compile(Car, Car', D1, D1'),
	self;

    T1 = `"_" :
      T1 = T2,
      D1 = D2 ;

    T1 = ?"_" :
      T1 = T2,
      D1 = D2 ;

    T1 = `V, V =\= "_" :
      D1 = [member(T1, Answer) | D2] |
	term_substitute;

    T1 = ?V, V =\= "_" :
      D1 = [member(`V, Answer) | D2] |
	term_substitute;

    arg(1, T1, `V), V =\= "_", V =\= `_, V =\= ?_,
    A := arity(T1) :
      D1 ! member(A(V), Replace) |
	term_compile_tuple;

    otherwise,
    A := arity(T1) :
      Replace = false |
	term_compile_tuple.


term_substitute(Answer, T1, T2) :-

    Answer = {_LHS, RHS},
    T1 = ?_, RHS = `V, V =\= `_, V =\= ?_ :
      T2 = ?V ;

    Answer = {_LHS, RHS},
    otherwise : T1 = _,
      T2 = RHS ;

    Answer =\= {_, _} :
      T2 = T1 .

term_compile_tuple(T1, T2, D1, D2, A, Replace) :-

    Replace = {_, _} |
	term_replace;

    Replace =\= {_,_},
    make_tuple(A, T1') |
	term_compile_arguments(T1, T1', D1, D1'?, A),
	term_replacable(T1', D1', D1''?, Replace'),
	term_replace.
	
term_compile_arguments(T1, T2, D1, D2, A) :-

    A-- >= 1,
    arg(A, T1, T),
    arg(A, T2, T') |
	term_compile(T, T', D1, D1'?),
	self;

    A < 1 : T1 = _, T2 = _,
      D1 = D2 .


term_replacable(T, D, D', Replace) :-

    arg(1, T, `V),
    V =\= `_, V =\= ?_, V =\= "_",
    A := arity(T) :
      D ! member(A(V), Replace) ;

    otherwise : T = _,
      D = D',
      Replace = false .

term_replace(T1, T2, D1, D2, A, Replace) :-

    Replace =\= {_, _} : A = _,
      T2 = T1,
      D1 = D2 ;

    Replace = {LHS, RHS} |
	tree_dictionary(S1?, _, []),
	substitutions,
	term_compile(RHS, RHS', S2, []),
	term_compile(RHS'?, T2, D1, D2).

compile_clauses(Clauses, D1, Procedures) :-

    Clauses ? T1 :
      Procedures ! T2? |
	term_compile(T1, T2, D1, D1'?),
	self;

    Clauses = [] :
      D1 = [],
      Procedures = [] .


tree_dictionary(In, E1, E2) :-

    In ? add(Id, LHS, RHS) |
	variable(In', Id, LHS, RHS, E1, E1', Left, Right),
	tree_dictionary(Left, E1', E1''),
	tree_dictionary(Right, E1'', E2);

    In ? member(_, Answer) :
      Answer = false |
	self;

    In ? compound(List^, List) |
	self;

    In = [] :
      E1 = E2 .


variable(In, Id, LHS, RHS, E1, E2, Left, Right) :-

    In ? add(Id, L, R) :
      E1 ! duplicate_assignment((L => R)) |
	self;

    In ? replace(Id, RHS') : RHS = _ |
	self;

    In ? member(Id, {LHS, RHS}^) |
	self;

    In ? Request,
    arg(2, Request, Less),
    Less @< Id :
      Left ! Request |
	self;

    In ? Request,
    arg(2, Request, More),
    Id @< More :
      Right ! Request |
	self;

    In ? compound(List, List'''),
    compound(RHS) : Id = _,
      List ! (LHS => RHS),
      E1 = E2,
      Left ! compound(List', List''?),
      Right ! compound(List'', List''') |
	self;

    In ? compound(List, List''),
    constant(RHS) : Id = _,
      E1 = E2,
      Left ! compound(List, List'?),
      Right ! compound(List', List'') |
	self;

    In = [] : Id = _, LHS = _, RHS = _,
      E1 = E2,
      Left = [], Right = [] .


hash_dictionary(In, E1) :-
    true :
      E2 = [] |
	stream # hash_table(HD?),
	serve_dictionary.

serve_dictionary(In, E1, E2, HD) :-

    In ? add(`V, LHS, RHS) :
      HD ! lookup(V, New?, Old, Reply) |
	self,
	add_entry(LHS, RHS, Reply, Old, New, E1, E1');

    In ? add({N,V}, LHS, RHS), N =\= "_var" :
      HD ! lookup(V(N), New?, Old, Reply) |
	self,
	add_entry(LHS, RHS, Reply, Old, New, E1, E1');

    In ? member(`V, Answer) :
      HD ! member(V, Entry, Reply) |
	self,
	member_entry;

    In ? member({N,V}, Answer), N =\= "_var" :
      HD ! member(V(N), Entry, Reply) |
	self,
	member_entry;

    In ? replace(`V, NewRHS) :
      HD ! replace(V, New?, Old, _) |
	self,
	replace_entry_rhs;

    In ? replace({N,V}, NewRHS), N =\= "_var" :
      HD ! replace(V(N), New?, Old, _) |
	self,
	replace_entry_rhs;

    In ? compound(List1, List2) :
      HD ! entries(Entries) |
	self,
	compound_entries;

    In = [] :
      E1 = E2,
      HD = [] .

add_entry(LHS, RHS, Reply, Old, New, E1, E2) :-

    Reply = new : Old = _,
      New = {LHS, RHS},
      E1 = E2 ;

    Reply = old,
    Old = {LHS, RHS} :		% permit identical declarations
      New = Old,
      E1 = E2 ;

    Reply = old,
    otherwise :
      New = Old,
      E1 = [conflicting_declaration((LHS => RHS)) | E2] .

member_entry(Reply, Entry, Answer) :-

    Reply = true :
      Answer = Entry ;

    Reply = false : Entry = _,
      Answer = false .

replace_entry_rhs(NewRHS, Old, New) :-
    Old = {LHS, _RHS} :
      New = {LHS, NewRHS} .

compound_entries(Entries, List1, List2) :-

    Entries ? entry(_Key, {LHS, RHS}) :
      List1 ! (LHS => RHS) |
	self;

    Entries = [] :
      List1 = List2 .
