/* $Header: /home/qiana/Repository/Logix/system/transform/typed.cp,v 1.1.1.1 1999/07/09 07:03:16 bill Exp $ */
-language(compound).
-export([transform/5]).
-mode(trust).

Stream ::= [Any|Stream].

/* 
transform translates typed fcp programs into plain fcp programs, while
changing the attributes accordingly. The attributes may be of two kinds : 
The default language(typed), builds an autoamton which consists of all types
declared, and of the procedures declared in the export declaration
export(List). This automaton is frozen, and becomes an attribute with functor
'_type'. Alternatively, if types(Name,List') is declared, the automaton
built ignores the module's export list, and puts the procedures in List' into
an attribute with functor Name. multiple types(_,_) may be declared. This
option is used for special system files, and for monitors. The
transformation for retrieval by the type checker in case of a remote call to
one of these modules is provided by the module itself.
*/


procedure transform(Stream, Stream, Stream, Stream, Stream).
%***********************************************************

transform(Attributes1, Terms1, Attributes2, Terms2, Errs) :-

    true :
      Attributes2 = Attributes1,
      Terms2 = Terms1,
      Errs = [] ;

    otherwise |
	type_check #
		build_automaton(Terms1, Terms2, {Procs, DFA, Remote}, D1, D2, 
				Errs\Errs1),
	get_export_data(Attributes1, Attributes2, Type_attributes,
			Exports, Types
	),
	change(D2, D1, D3, Procs, Exported_procs, Exports, Types, Errs1),
	check_freeze(Exported_procs, DFA, Remote, D2, D3, Type_attributes).

get_export_data(List1, List2, Type_attributes, Exports, Types):-

    List1 ? X,
    X =\= export(_),
    X =\= types(_, _) :
      List2 ! X |
	get_export_data;

    List1 ? export(Exports^) :
      List2 ! export(Exports),
      Exports' = _ |
	get_export_data;

    List1 ? types(Name, List) :
      Types ! Name(List) |
	get_export_data;

    List1 = [] :
      List2 = Type_attributes,
      Exports = all,
      Types = [] |
	true.


change(Done, D1, D2, Procs, Exported, Exports, Types, Errs):- 

    Types =\= [] : Exports = _ |
	change_types(Types, Exported, Procs, Errs, Done, D1, D2);

    Exports = all,
    Types = [] : Done = _,
      D1 = D2,
      Procs = Exported,
      Errs = [] |
	true;

    Types = [],
    Exports =\= all | 
	change_export(Done, D1, D2, Procs, Exported, Exports, Errs, []).


change_types(Types, Exported, Procs, Errs, Done, D1, D2) :-

    Types ? Name(all) :
      Exported ! Name(Procs, done) |
	change_types;

    Types ? Name(List),
    List =\= all : 
      Exported ! Name(Export, D2) |
	change_export(Done, D1, D1', Procs, Export, List, Errs, Errs'),
	change_types;

    Types = [] : Procs = _, Done = _,
      Exported = [],
      Errs = [],
      D1 = D2 |
	true.


%procedure change_export(done, done, done, Tree, Tree, [Id], Stream, Stream).
%****************************************************************************

change_export(Done, D1, D2, Tree, New_tree, List, Errs1, Errs2) :-

    List ? F/Args :
      Id = F/A |
	A := Args + 1, 
	type_check # automaton #
		lookup_tree(Id, Proc, Tree, D1\D1', Reply, Done?),
	check(Reply, Id, Proc, New_tree, D1', D1'', Errs1, Errs1'),
	change_export;

    otherwise : Done = _, Tree = _, New_tree = _, List = _,
      D1 = D2,
      Errs1 = Errs2 |
	true.

check(Reply, Id, State, New_tree, D1, D2, Errs1, Errs2) :-

    Reply = add,
    Id = F/A,
    make_tuple(A, Atom),
    arg(1, Atom, F),
    N := A - 1 :
      Errs1 = [undeclared_procedure(F/N) | Errs2],
      State = functor(Atom,  F/A) |
	fill_with_Any(Atom, 2, A, D1, D1'),
	type_check # automaton #
		lookup_tree(Id, State, New_tree, D1'\D2, _, done);

    Reply = find :
      Errs1 = Errs2 |
	type_check # automaton #
		lookup_tree(Id, State, New_tree, D1\D2, _, done).

fill_with_Any(Atom, N, A, D1, D2):-

    N =< A, 
    arg(N, Atom, [basic('Any')]^) | 
    N' := N + 1, 
	fill_with_Any;

    N > A : Atom = _,
      D1 = D2 |
	true.


check_freeze(Export, DFA, Remote, D1, D2, Type_attributes) :-

    D1 = done, D2 = done,
    arity(Export, 4) :
      Type_attributes = ['_type'(Frozen_DFA)] | 
	freeze({Export, DFA, Remote}, Frozen_DFA, _);

    D1 = done, D2 = done :
      Export = [],
      Type_attributes = ['_type'(Frozen_DFA)] | 
	freeze({_, DFA, Remote}, Frozen_DFA, _);

    Export ? Name(Tree, Done) : D2 = _,
      Type_attributes ! Name(Frozen_DFA) |
	unify(Done, D1, D3),
	freezer({Tree, DFA, Remote}, D3, Frozen_DFA),
	check_freeze1(Export', DFA, Remote, D1, Type_attributes').


check_freeze1(Export, DFA, Remote, D1, Type_attributes) :-

    Export ? Name(Tree, D0) :
      Type_attributes ! Name(Frozen_DFA) |
	unify(D0, D1, D2),
	freezer({Tree, DFA, Remote}, D2, Frozen_DFA),
	check_freeze1;

    Export = [] : DFA = _, Remote = _, D1 = _,
      Type_attributes = [] |
	true.


freezer(Term, done, FrozenTerm) :-
	freeze(Term, FrozenTerm, _).


unify(X, X, X^).
