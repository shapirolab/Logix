-export([tree/3,nfa_to_dfa/2,process_args/5,find_if_includes_String_or_Any/3]).
-mode(trust).
-language([colon,typed]).

YesNo ::= yes ; no ; do_not_know.

procedure tree(Proc_tree,Proc_tree,S_circuit).

tree(Tree,Tree',D\D1):- 
	Tree = {Key,Data,Left,Right}:
	Tree' = {Key,Data'?,Left',Right'}|
	nfa_to_dfa(Data,Data'),
	tree(Left,Left',D\D2),
	tree(Right,Right',D2\D1).
tree(Leaf,_,(D\D)^) :-%what happens if Proc_tree is not defined completely yet?
	unknown(Leaf) | true.

 % the unknown leaf means this is an unistantiated tree node - the
 % tree's end. the new tree is left unistantiated, we might want to
 % add undefined procedures afterwards.  


procedure nfa_to_dfa(State,State).
nfa_to_dfa(Atoms,DFA_state) :-
        flatten_lists(Atoms?,Atoms?,Atoms'\[]),
	sort(Atoms'?,Atoms''),
	nfa_to_dfa1(Atoms''?,DFA_state,_).

%		?           ^
nfa_to_dfa1(List,[functor(Term',Functor/Arity)|Terms']^,Dictionary) :-
	List = [X|Terms],
	X = functor(Term,Functor/Arity),
	make_tuple(Arity,Term'):
	arg(1,Term',Functor)|
	extract_unifiable_functor_terms(Functor/Arity,
					Terms?,
					Term_list,
					Terms''),
	process_args(Arity,2,[Term|Term_list],Term',Dictionary),
	nfa_to_dfa1(Terms''?,Terms',Dictionary).

nfa_to_dfa1(List,[compound(Term',Arity/Inc_funcs)|Terms']^,Dictionary) :-
	List = [X|Terms],
	X = compound(Term,Arity/_),
	arg(1,Term,Arg1):
	make_tuple(Arity,Term'),
	arg(1,Term',DFA_state) |
        find_if_includes_String_or_Any(Arg1?,no\Include_functors,_\_),
	extract_unifiable_compound_terms(Arity,
					 Include_functors? \ Inc_funcs,
					 Terms?,
					 Term_list,
					 Terms''),
        process_term(Inc_funcs?,Arity,[Term|Term_list],Terms'''\Terms'',Arg'\[]),
	sort(Arg'?,NFA_state),
	look_up(NFA_state,Dictionary,DFA_state,Dictionary),
	process_args(Arity,2,[Term|Term_list],Term',Dictionary),
	sort(Terms'''?,Terms4),
	nfa_to_dfa1(Terms4?,Terms',Dictionary).

nfa_to_dfa1(List,[Term'?|Terms']^,Dictionary) :-
	List = [Term|Terms],
	Term = list(_,_):
	Term' = list(_,_) |
	extract_unifiable_list_terms(Terms?,Term_list,Terms''),
	process_args(3,2,[Term|Term_list],Term',Dictionary),
	nfa_to_dfa1(Terms''?,Terms',Dictionary).

nfa_to_dfa1(List,[Term|Terms']^,Dictionary) :-
	List = [Term|Terms],
	Term = constant(_) |
	extract_unifiable_constants(Term,Terms?,Terms''),
	nfa_to_dfa1(Terms''?,Terms',Dictionary).

nfa_to_dfa1(List,[Term|Terms']^,Dictionary) :-
	List = [Term|Terms],
	Term = basic(Basic_type) |
	extract_basic_terms(Basic_type?,Terms?,Terms''),
	nfa_to_dfa1(Terms''?,Terms',Dictionary).

%nfa_to_dfa1([Term|Terms],Terms',Dictionary) :-
%	list(Term) |
%        flatten_lists([Term|Terms],Terms''\[]),
%	qsort(Terms''?,Terms'''\[]),
%	nfa_to_dfa1(Terms'''?,Terms',Dictionary).

nfa_to_dfa1([Term|Terms],Terms',Dictionary) :-
	Term = [] |
	nfa_to_dfa1(Terms?,Terms',Dictionary).

%nfa_to_dfa1([Term|Terms],[Term|Terms'],Dictionary) :-
%	Term? = '_undefined'(_) |
%	nfa_to_dfa1(Terms?,Terms',Dictionary).

nfa_to_dfa1([],[]^,_).


flatten_lists(Old_list,New_list,Terms1\Terms3) :-
	New_list = [Term | Terms],
	list(Term),
	Old_list =\= Term |
        flatten_lists(Term,Term?,Terms1\Terms2),
	flatten_lists(Old_list,Terms?,Terms2\Terms3).

flatten_lists(Old_list,New_list,Terms1\Terms2) :-
	New_list = [Term | Terms],
	list(Term),
	Old_list = Term |
	flatten_lists(Old_list,Terms?,Terms1\Terms2).

flatten_lists(Old_list,[Term|Terms],Terms1\Terms2) :-
	Term = [] |
        flatten_lists(Old_list,Terms?,Terms1\Terms2).

flatten_lists(_,[],Terms^\Terms).

flatten_lists(Old_list,[Term|Terms],[Term?|Terms1?]^\Terms2) :-
	otherwise |
        flatten_lists(Old_list,Terms?,Terms1\Terms2).


extract_unifiable_functor_terms(Name,
	                        List,
				[Term|Term_list]^,
				Terms'
) :-
	List = [X|Terms],
	X = functor(Term,Name'),
	Name = Name' |
        extract_unifiable_functor_terms(Name,Terms?,Term_list,Terms').

extract_unifiable_functor_terms(_,[],[]^,[]^).
	
extract_unifiable_functor_terms(Name,
			      [Term|Terms],
			      Term_list,
			      [Term|Terms']^
) :-
	otherwise |
        extract_unifiable_functor_terms(Name,Terms?,Term_list,Terms').
	

extract_unifiable_compound_terms(Arity,
				 Include_functors\Inc_funcs,
				 List,
				 [Term|Term_list]^,
				 Terms'
) :-
	List = [X|Terms],
	X = compound(Term,Arity'/_),
	Include_functors = no,
	Arity = Arity',
	arg(1,Term,Arg1) |
        find_if_includes_String_or_Any(Arg1?,no\Include_functors',_\_),
        extract_unifiable_compound_terms(Arity,
					Include_functors'? \Inc_funcs,
					Terms?,
					Term_list,
					Terms').

extract_unifiable_compound_terms(Arity,
				 Include_functors\yes^,
				 List,
				 [Term|Term_list]^,
				 Terms'
) :-
	List = [X|Terms],
	X = compound(Term,Arity'/_),
	Include_functors = yes,
	Arity = Arity' |
        extract_unifiable_compound_terms(Arity,
					 yes\yes,
					 Terms?,
					 Term_list,
					 Terms').

extract_unifiable_compound_terms(Arity,
				 yes\yes^,
				 List,
				 [Term'|Term_list]^,
				 Terms'
) :-
	List = [X|Terms],
	X = functor(Term,_/Arity'),
	Arity = Arity' :
	make_tuple(Arity,Term'),
	arg(1,Term',[basic('String')]) |
        copy_arguments(2,Arity,Term,Term'),
       extract_unifiable_compound_terms(Arity,yes\yes,Terms?,Term_list,Terms').

extract_unifiable_compound_terms(_,I\I^,[],[]^,[]^).
	
extract_unifiable_compound_terms(Arity,
	                         Include_functors\Inc_funcs,
				 [Term|Terms],
				 Term_list,
				 [Term|Terms']^
) :-
	otherwise |
        extract_unifiable_compound_terms(Arity,
                                         Include_functors\Inc_funcs,
					 Terms?,
					 Term_list,
					 Terms').
	


copy_arguments(N,Arity,Term,Term') :-
	N =< Arity,
	arg(N,Term,ArgN):
	arg(N,Term',ArgN),
	N1 := N + 1 |
        copy_arguments(N1,Arity,Term,Term').

copy_arguments(N1,Arity,_,_) :-
	Arity < N1 |
        true.


extract_unifiable_list_terms(List,[Term|Term_list]^,Terms') :-
	List = [Term|Terms],
	Term = list(_,_) |
	extract_unifiable_list_terms(Terms?,Term_list,Terms').

extract_unifiable_list_terms([],[]^,[]^).

extract_unifiable_list_terms([Term|Terms],Term_list,[Term|Terms']^) :-
	otherwise | 
	extract_unifiable_list_terms(Terms?,Term_list,Terms').


extract_unifiable_constants(Term,List,Terms'):-
	List = [Term' | Terms],
	Term = Term'|
	extract_unifiable_constants(Term,Terms?,Terms').

extract_unifiable_constants(Term,List,[Term'|Terms']^):-
	List? = [Term' | Terms],
	Term =\= Term'|
	extract_unifiable_constants(Term,Terms?,Terms').

extract_unifiable_constants(_,[],[]^).


process_term(no,Arity,[Term|Term_list],Terms\Terms',Arg\Arg1) :-
	arg(1,Term,Arg') |
        remove_strings(Arg'?,Arity,Term,Terms\Terms'',Arg\Arg2),
	process_term(no,Arity,Term_list?,Terms''\Terms',Arg2\Arg1).

process_term(yes,_,[Term'|Term_list],Terms^\Terms,Arg\Arg1) :-
	arg(1,Term',Arg') |
        remove_strings1(Arg'?,Arg\Arg2),
	process_term(yes,_,Term_list?,_\_,Arg2\Arg1).

process_term(_,_,[],Terms^\Terms,Arg^\Arg).


remove_strings(List,
               Arity,
	       Term,
	       [functor(Term',X/Arity)|Terms]^\Terms',
	       Args
) :-
	List = [A1|Arg'],
	A1 = constant(X),
	string(X),
	make_tuple(Arity,Term'):
	arg(1,Term',X) |
        copy_arguments(2,Arity,Term,Term'),
	remove_strings(Arg'?,Arity,Term,Terms\Terms',Args).

remove_strings(List,Arity,Term,Terms\Terms',Arg\Arg1) :-
	List = [X|Arg'],
	list(X) |
	remove_strings(X?,Arity,Term,Terms\Terms'',Arg\Arg2),
	remove_strings(Arg'?,Arity,Term,Terms''\Terms',Arg2\Arg1).

remove_strings([],_,_,Terms^\Terms,Arg^\Arg).

remove_strings([X|Arg'],Arity,Term,Terms\Terms',[X|Arg]^\Arg1) :-
	otherwise |
        remove_strings(Arg'?,Arity,Term,Terms\Terms',Arg\Arg1).


remove_strings1([constant(X)|Arg'],Args) :-
	string(X) |
	remove_strings1(Arg'?,Args).

remove_strings1([X|Arg'],Arg\Arg1) :-
	list(X) |
	remove_strings1(X?,Arg\Arg2),
	remove_strings1(Arg'?,Arg2\Arg1).

remove_strings1([],Arg^\Arg).

remove_strings1([X|Arg'],[X|Arg]^\Arg1) :-
	otherwise |
        remove_strings1(Arg'?,Arg\Arg1).


procedure process_args(Integer,Integer,[Tuple],Tuple,Dictionary).

process_args(Arity,N,Term_list,Term',Dictionary) :-
	N =< Arity:
	N1 := N + 1,
	arg(N,Term',DFA_state) |
	make_new_state(N,Term_list?,State),
	sort(State?,NFA_state),
	look_up(NFA_state,Dictionary,DFA_state,Dictionary),
	process_args(Arity,N1,Term_list,Term',Dictionary).

process_args(Arity,N,_,_,_) :-
	N > Arity |
        true.

make_new_state(N,[Term|Term_list],[Arg?|State?]^) :-
	arg(N,Term,Arg) |
        make_new_state(N,Term_list?,State).

make_new_state(_,[],[]^).


look_up(X,List,Y,Dictionary) :-
    List = [(X1,_)|Dict],
    X =\= X1 |
        look_up(X,Dict,Y,Dictionary).

look_up(X,List,Y^,_) :-
    List = [(X,Y)|_] |
        true.

look_up(X,(List?)^,Y,Dictionary) :-
	List = [(X,Y)|_],
	flatten_lists(X,X?,X'\[]),
	sort(X'?,X''),
	nfa_to_dfa1(X''?,Y,Dictionary).
%	sort(Y'?,Y).
	
/*
                                %not the same Dictionary as above.
append_lists([List|Lists],List',Dictionary) :-
	append_lists(Lists?,List'',Dictionary'),
	append(List?,List'',List',Dictionary,Dictionary').

append_lists([],[],_).

%                           ?           ^
append([X|Xs],Ys,[X?|Zs],Dictionary,Dictionary') :-
	tuple(X) |
	append(Xs?,Ys,Zs,Dictionary,Dictionary').

append([X|Xs],Ys,Zs,Dictionary,Dictionary') :-
	list(X) |
	member(X,Dictionary?,Answer),
	continue_appending(Answer?,X,Xs,Ys,Zs,Dictionary,Dictionary').

append([],Ys,Ys,Dictionary,Dictionary).


member(X,Dict,Answer) :-
	Dict? = [X1|Dictionary],
	X =\= X1 |
        member(X,Dictionary?,Answer).

member(X,Dict,yes) :-
	Dict? = [X1|_],
        X =?= X1 |
        true.

member(_,[],no).


continue_appending(yes,_,Xs,Ys,Zs,Dictionary,Dictionary') :-
	append(Xs?,Ys,Zs,Dictionary,Dictionary').

continue_appending(no,X,Xs,Ys,Zs,Dictionary,Dictionary') :-
	append(X?,Xs,Xs',[X|Dictionary],Dictionary''),
	append(Xs'?,Ys,Zs,Dictionary'',Dictionary').
*/

sort(Xs,Ys) :-
	pair(Xs?,In),
	msort1(In?,Ys).

msort1([],[]^).
msort1([Xs],Xs^).
msort1(Xs,Ys) :-
	otherwise | 
	msort(Xs?,Zs),
	msort1(Zs?,Ys).

pair([X1|Xs1],[[X1,X2]|Ys?]^) :-
	Xs1 = [X2|Xs], X1@<X2 |
	pair(Xs?,Ys).
pair(Xs,Ys) :-
	Xs = [X1|Xs1],
	Xs1 = [X2|_],
	X1 = X2 :
	Ys = Ys'? |
	pair(Xs1?,Ys').
pair([X1,X2|Xs],[[X2,X1]|Ys?]^) :-
	otherwise |
	pair(Xs?,Ys).
pair([X|Nil],[[X]]^) :-
	Nil = [] |
	true.
pair([],[]^).


msort([Xs1,Xs2|XXs],[Ys|YYs?]^) :-
	merge(Xs1?,Xs2?,Ys),
	msort(XXs?,YYs).
msort([Xs],[Xs]^).
msort([],[]^).

merge([X|Xs],[Y|Ys],[X|Zs]^) :-
	X@<Y |
	merge(Xs?,[Y|Ys],Zs).
merge(L1,L2,[Y|Zs]^) :-
	L1 = [X|Xs],   
        L2 = [Y|Ys],
        Y = X |
	merge(Xs?,Ys?,Zs).
merge([X|Xs],[Y|Ys],[Y|Zs]^) :-
	otherwise |
	merge([X|Xs],Ys,Zs).
merge([],Ys,Ys^).
merge(Xs,[],Xs^).

extract_basic_terms('Integer',Terms,Terms') :-
	extract_integer_terms(Terms?,Terms').
	
extract_basic_terms('Real',Terms,Terms') :-
	extract_real_terms(Terms?,Terms').
	
extract_basic_terms('Vector',Terms,Terms'):- 
	extract_vector_terms(Terms,Terms').

extract_basic_terms('String',Terms,Terms') :-
	extract_string_terms(Terms?,Terms').
	
extract_basic_terms('Tuple',Terms,Terms') :-
	extract_tuple_terms(Terms?,Terms').
	
extract_basic_terms('List',Terms,Terms') :-
	extract_list_terms(Terms?,Terms').
	
extract_basic_terms('Any',_,[]^).
	

extract_integer_terms(List,Terms') :-
	List = [Term|Terms],
	Term = constant(X),
	integer(X) |
	extract_integer_terms(Terms?,Terms').

extract_integer_terms(List,Terms') :-
	List = [Term|Terms],
	Term = basic('Integer') |
	extract_integer_terms(Terms?,Terms').

extract_integer_terms([],[]^).

extract_integer_terms([Term|Terms],[Term|Terms']^) :-
	otherwise |
	extract_integer_terms(Terms?,Terms').


extract_real_terms(List,Terms') :-
	List = [Term|Terms],
	Term = constant(X),
	real(X) |
	extract_real_terms(Terms?,Terms').

extract_real_terms(List,Terms') :-
	List = [Term|Terms],
	Term = basic('Real') |
	extract_real_terms(Terms?,Terms').

extract_real_terms([],[]^).

extract_real_terms([Term|Terms],[Term|Terms']^) :-
	otherwise |
	extract_real_terms(Terms?,Terms').


extract_vector_terms(List,Terms') :-
    List = [basic('Vector') | Terms] |
	extract_vector_terms(Terms?, Terms').

extract_vector_terms([],[]^).

extract_vector_terms([Term|Terms],[Term|Terms']^) :-
	otherwise |
	extract_vector_terms(Terms?,Terms').


extract_string_terms(List,Terms') :-
	List = [Term|Terms],
	Term = constant(X),
	string(X) |
	extract_string_terms(Terms?,Terms').

extract_string_terms(List,Terms') :-
	List = [Term|Terms],
	Term = basic('String') |
	extract_string_terms(Terms?,Terms').

extract_string_terms([],[]^).

extract_string_terms([Term|Terms],[Term|Terms']^) :-
	otherwise |
	extract_string_terms(Terms?,Terms').


extract_tuple_terms(List,Terms') :-
	List = [Term|Terms],
	Term = functor(_,_) |
	extract_tuple_terms(Terms?,Terms').

extract_tuple_terms(List,Terms') :-
	List? = [Term | Terms],
	Term = compound(_,_) |
	extract_tuple_terms(Terms?,Terms').

extract_tuple_terms(List,Terms') :-
	List = [Term | Terms],
	Term = basic('Tuple') |
	extract_tuple_terms(Terms?,Terms').

extract_tuple_terms([],[]^).

extract_tuple_terms([Term|Terms],[Term|Terms']^) :-
	otherwise |
	extract_tuple_terms(Terms?,Terms').


extract_list_terms(List,Terms') :-
	List = [Term|Terms],
	Term = list(_,_) |
	extract_list_terms(Terms?,Terms').

extract_list_terms(List,Terms') :-
	List = [Term | Terms],
	Term = basic('List') |
	extract_list_terms(Terms?,Terms').

extract_list_terms([],[]^).

extract_list_terms([Term|Terms],[Term|Terms']^) :-
	otherwise |
	extract_list_terms(Terms?,Terms').



procedure find_if_includes_String_or_Any(State,YesNo\YesNo,done\done). 
find_if_includes_String_or_Any(_,yes\yes^,(D\D)^). 

find_if_includes_String_or_Any(List,no\yes^,(D\D)^) :-
	List = [Term|_],
	Term = basic('String') |
        true.
	
find_if_includes_String_or_Any(List,no\yes^,(D\D)^) :-
	List = [Term | _],
	Term = basic('Any') |
        true.

find_if_includes_String_or_Any(List,Left\Right,D\D1) :-
	List = [Term | Terms],
	Term = [_|_] |
        find_if_includes_String_or_Any(Term?,Left? \ Middle,D\D2),
	find_if_includes_String_or_Any(Terms?,Middle? \ Right,D2\D1).

find_if_includes_String_or_Any([],L^\L,(D\D)^). 
	
find_if_includes_String_or_Any([_|Terms],no\Answer,Done) :-
	otherwise |
        find_if_includes_String_or_Any(Terms?,no\Answer,Done).
	
