/*
Warning : Works only if both states are ground 
-------
This module checks equivalence of two states in the set of terms that they
represent.
*/
-export([equivalent/3,rev/2]).
-mode(trust).
-language([colon,typed]).

Any_list ::= [Any].

procedure equivalent(State,State,Any).
%*************************************

equivalent(State_1,State_2,yes^) :-
	State_1 = State_2 |
        true.

equivalent(State_1,State_2,Answer) :- 
	otherwise |
	equivalent1([(State_1,State_2,[])],
	            [{State_1,Leader},{State_2,Leader}|_],
		    Answer).


equivalent1([(State_1,State_2,Trace)|LIST],Equivalence_table,Answer):-
	collision(State_1?,State_2?,Equivalence_table,LIST1\LIST,Trace),
	equivalent1(LIST1?,Equivalence_table,Answer).

equivalent1([],_,yes^).

equivalent1([Answer|_],_,Answer^) :-
		otherwise | true.


collision(List,State_2,Equivalence_table,Left\Right,Trace) :-
	List = [Term_1|Terms_1],
	Term_1 =\= basic(_) |
  match(Term_1?,State_2?,Terms_2',Equivalence_table,Left\Middle,Trace),
  collision(Terms_1?,Terms_2'?,Equivalence_table,Middle\Right,Trace).

collision(List,State_2,Equivalence_table,Left\Right,Trace) :-
	List = [Term_1|Terms_1],
	Term_1 = basic(Basic_type) |
  match_basic(Basic_type?,State_2?,Terms_2',Left\Middle,Trace),
  collision(Terms_1?,Terms_2'?,Equivalence_table,Middle\Right,Trace).

collision([],_,_,(L\L)^,_).


% match(Term_1,List_2,List_21,_,LIST) ---
%		Finds a unifiable clause for clause_1 in List_2. 
%		List_21 is the same as List_2 after deleting the unifiable
%		clause for clause_1. LIST is being updated with the next
%		equivalence relations to be performed.

match(Term_1,List,Terms_2^,_,(L\L)^,_) :-
	List = [Term_2|Terms_2],
	Term_1 = Term_2 |
        true.

match(functor(Term_1,Functor_1/Arity_1),
      [functor(Term_2,Name_2)|Terms_2],
      Terms_2^,
      Equivalence_table,
      LIST,
      Trace
) :-
	Name_2 = Name/Arity_2,
	Functor_1 = Name,
        Arity_1 = Arity_2:
	Arity := Arity_1 - 1 |
	functor_add_to_LIST(Term_1?,
	                    Term_2?,
			    2,
			    Arity_1,
			    Equivalence_table,
			    LIST,
			    [Functor_1/Arity|Trace]).

match(compound(Term_1,Arity_1/_),
      [compound(Term_2,Arity_2/_)|Terms_2],
      Terms_2^,
      Equivalence_table,
      LIST,
      Trace
) :-
	Arity_1 = Arity_2 |
	add_to_LIST(Term_1?,
	            Term_2?,
		    1,
		    Arity_1,
		    Equivalence_table,
		    LIST,
		    ['Tuple'/Arity_1|Trace]).

match(compound(Term_1,Arity_1/Includes_functors),
      [functor(Term_2,_/Arity_2)|Terms_2],
      Terms_2',
      Equivalence_table,
      LIST,
      Trace
) :-
	Arity_1 = Arity_2,
	Includes_functors = yes:
	make_tuple(Arity_1,Term),
	arg(1,Term,[basic('String')]) |
	add_to_LIST(Term_1?,
                    Term?,
		    1,
		    Arity_1,
		    Equivalence_table,
		    LIST,
                    ['Tuple'/Arity_1|Trace]),
	extract_functors(Arity_1,Terms_2?,Terms,Terms_2'),
        nfa_to_dfa#process_args(Arity_1,2,[Term_2|Terms],Term,_).

match(Term_1,List,Terms_2^,Equivalence_table,LIST,Trace) :-
	Term_1 = list(_,_),
	List = [Term_2 | Terms_2],
	Term_2 = list(_,_) |
	functor_add_to_LIST(Term_1?,
                            Term_2?,
			    2,
			    3,
			    Equivalence_table,
			    LIST,
                            ['List'|Trace]).

match(Term_1,[],[]^,_,LIST,Trace) :-
        super#redundant([Term_1],[],Redundant),
	update_list(Redundant?,Term_1,LIST,Trace).

match(Term_1,[Term_2|Terms_2],[Term_2|Terms_2']^,Equivalence_table,LIST,Trace
) :-
	otherwise |
	match(Term_1,Terms_2,Terms_2',Equivalence_table,LIST,Trace).
	
	
match_basic(Term_1,[Term_2|Terms_2],Terms_2^,(L\L)^,_) :-
        Term_2 = basic(Term),
	Term_1 = Term|
        true.

match_basic('Integer',List,Terms,(L\L)^,_) :-
	List = [Term_2|Terms_2],
	Term_2 = constant(X),
	integer(X)|
	filter_integers(Terms_2?,Terms).

match_basic('Real',List,Terms,(L\L)^,_) :-
	List = [Term_2|Terms_2],
	Term_2 = constant(X),
	real(X) |
	filter_reals(Terms_2?,Terms).

match_basic('String',List,Terms,(L\L)^,_) :-
	List = [Term_2|Terms_2],
	Term_2 = constant(X),
	string(X) |
	filter_strings(Terms_2?,Terms).

match_basic('Tuple',List,Terms,(L\L)^,_) :-
	List = [Term_2|Terms_2],
	Term_2 = functor(_,_) |
	filter_tuples(Terms_2?,Terms).

match_basic('Tuple',List,Terms,(L\L)^,_) :-
	List = [Term_2|Terms_2],
	Term_2 = compound(_,_) |
	filter_tuples(Terms_2?,Terms).

match_basic('Any',_,[]^,(L\L)^,_).

match_basic(Term_1,[],[]^,([no(Trace',Term_1)|L]\L)^,Trace) :-
	rev(Trace?,Trace').

match_basic(Term_1,[Term_2|Terms_2],[Term_2|Terms_2']^,LIST,Trace) :-
	otherwise |
	match_basic(Term_1,Terms_2,Terms_2',LIST,Trace).


add_to_LIST(Term_1,Term_2,N,Arity,Equivalence_table,L\R,Trace) :-
	N =< Arity,
        arg(N,Term_1,State_1),
        arg(N,Term_2,State_2):
   	N1 := N + 1 |
        find_State(State_1,Equivalence_table,Leader_1,Done_1),
        find_State(State_2,Equivalence_table,Leader_2,Done_2),
        add_if_necessary(State_1,
                         Leader_1,
			 State_2,
			 Leader_2,
			 Done_1?,
			 Done_2?,
			 L\M,
			 [N|Trace]),
        add_to_LIST(Term_1,Term_2,N1,Arity,Equivalence_table,M\R,Trace).

add_to_LIST(_,_,N,Arity,_,(L\L)^,_) :-
	N > Arity |
        true.


functor_add_to_LIST(Term_1,Term_2,N,Arity,Equivalence_table,L\R,Trace) :-
	N =< Arity,
        arg(N,Term_1,State_1),
        arg(N,Term_2,State_2):
	N1 := N + 1,
	N2 := N - 1 |
        find_State(State_1,Equivalence_table,Leader_1,Done_1),
        find_State(State_2,Equivalence_table,Leader_2,Done_2),
        add_if_necessary(State_1,
                         Leader_1,
			 State_2,
			 Leader_2,
			 Done_1?,
			 Done_2?,
			 L\M,
			 [N2|Trace]),
       functor_add_to_LIST(Term_1,Term_2,N1,Arity,Equivalence_table,M\R,Trace).

functor_add_to_LIST(_,_,N,Arity,_,(L\L)^,_) :-
	N > Arity |
        true.


find_State(State,SlT,Leader,Done) :-
    true :
      SlT = SlT'?,
      Done = done |
	SlT' = [{State,Leader}|_].
find_State(State,[{State,Leader}|_],Leader^,done^).
find_State(State,[{State',_}|Equivalence_table],Leader,Done) :-
	State =\= State' |
	find_State(State,Equivalence_table,Leader,Done).


add_if_necessary(_,Leader_1,_,Leader_2,done,done,(L\L)^,_) :-
	Leader_1 == Leader_2 |
        true.

add_if_necessary(State_1,
	         Leader,
		 State_2,
		 Leader^,
		 done,
		 done,
		 ([(State_1,State_2,Trace)|L]\L)^,
		 Trace
) :-
	otherwise |
        true.


update_list(redundant,_,(L\L)^,_).

update_list(not_redundant,Term,([no(Trace',Term')|L]\L)^,Trace) :-
	toplevel(Term?,Term'),
	rev(Trace?,Trace').


filter_integers(List,Ys) :-
	List = [constant(Z)|Xs],
	integer(Z) |
       	filter_integers(Xs?,Ys).

filter_integers([],[]^).

filter_integers([X|Xs],[X|Ys]^) :-
	otherwise |
       	filter_integers(Xs?,Ys).


filter_reals(List,Ys) :-
	List = [constant(Z)|Xs],
	real(Z) |
       	filter_reals(Xs?,Ys).

filter_reals([],[]^).

filter_reals([X|Xs],[X|Ys]^) :-
	otherwise |
       	filter_reals(Xs?,Ys).


filter_strings(List,Ys) :-
	List = [constant(Z)|Xs],
	string(Z) |
       	filter_strings(Xs?,Ys).

filter_strings([],[]^).

filter_strings([X|Xs],[X|Ys]^) :-
	otherwise |
       	filter_strings(Xs?,Ys).


filter_tuples(List,Ys) :-
	List = [X|Xs],
	X = functor(_,_) |
       	filter_tuples(Xs?,Ys).

filter_tuples(List,Ys) :-
	List = [X|Xs],
	X = compound(_,_) |
       	filter_tuples(Xs?,Ys).

filter_tuples([],[]^).

filter_tuples([X|Xs],[X|Ys]^) :-
	otherwise |
       	filter_tuples(Xs?,Ys).

extract_functors(Arity,List,[Term|Terms]^,Terms_2') :-
	List = [X|Terms_2],
	X = functor(Term,_/Arity'),
	Arity = Arity' |
	extract_functors(Arity,Terms_2?,Terms,Terms_2').

extract_functors(Arity,List,Terms,[X|Terms_2']^) :-
	List = [X|Terms_2],
	X = functor(_,_/Arity'),
	Arity =\= Arity' |
	extract_functors(Arity,Terms_2?,Terms,Terms_2').

extract_functors(_,[],[]^,[]^).

extract_functors(_,Terms_2,[]^,Terms_2^) :-
	otherwise |
	true.


procedure rev(List,List).
%************************

rev(Xs,Ys) :-
	rev(Xs?,[],Ys).

procedure rev(List,List,List).
%*****************************

rev([X|Xs],Acc,Ys) :-
	rev(Xs?,[X|Acc],Ys).
rev([],Ys,Ys^).



toplevel(Term,Term'^) :-
	Term = Constant(Term'),
        Constant = constant |
         true.

toplevel(Term,['_'|'_']^) :-
	Term = list(_,_) |
        true.

toplevel(Term,Term') :-
	Term = compound(_,Arity/_):
	make_tuple(Arity,Term') |
        fill_with(Term',1,Arity,'_').

toplevel(Term,Term') :-
	Term = functor(_,F/Arity) :
	make_tuple(Arity,Term'),
	arg(1,Term',F?)|
        fill_with(Term',2,Arity,'_').

fill_with(Term,N,Arity,Arg):- 
	N =< Arity:
	arg(N,Term,Arg?) |
   	N1 := N + 1,
    	fill_with(Term,N1,Arity,Arg).
fill_with(_,N,A,_):- 
	N > A | 
        true.

