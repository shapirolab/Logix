-export([intersect/3]).
-mode(trust).
-language([colon,typed]).

Table ::= [{State,[{State,State}]}].


procedure intersect(State,State,State).
%**************************************

intersect(State_1,State_2,State_1^) :-
	State_1 = State_2 |
        true.

intersect(State_1,State_2,State) :-
	otherwise |
	member(State_1?,State_2?,State,Intersection_table,Intersection_table).
 

procedure member(State,State,State,Table,Table).
%***********************************************

member( State_1,
	State_2,
	State,
	[{State_1'?,[{State_2?,State}|_]}|_]^,
	Intersection_table
) :-
	State_1 = State_1',
	collision(State_1?,State_2?,State\[],Intersection_table).

member( State_1,State_2,State,Table,Intersection_table) :-
	Table = [Argument|_],
	Argument = {State_1',States},
	State_1 = State_1' |
        member1(State_1,State_2,State,States,Intersection_table).

member( State_1,State_2,State,Table,Intersection_table) :-
	Table = [Argument|States],
	Argument = {State_1',_},
	State_1 =\= State_1' |
        member(State_1,State_2,State,States,Intersection_table).


procedure member1(State,State,State,[{State,State}],Table).
%**********************************************************

member1(State_1,State_2,State,[{State_2'?,State}|_]^,Intersection_table) :-
	State_2' = State_2,
	collision(State_1?,State_2?,State\[],Intersection_table).

member1(_,State_2,State^,Table,_) :-
	Table = [Argument|_],
	Argument = {State_2',State},
	State_2 = State_2' |
        true.

member1(State_1,State_2,State,Table,Intersection_table) :-
	Table = [Argument|States],
	Argument = {State_2',_},
	State_2 =\= State_2' |
        member1(State_1,State_2,State,States,Intersection_table).


procedure collision(State,State,State\State,Table).
%**************************************************

collision(List,State_2,State\Rest,Intersection_table) :-
	List = [Term_1|Terms_1],
	Term_1 = basic(Basic_type) |
	intersect_basic(Basic_type?,State_2?,State_2',State\State'),
	collision(Terms_1?,State_2'?,State'\Rest,Intersection_table).

collision(List,State_2,State\Rest,Intersection_table) :-
	List = [Term_1|Terms_1],
	Term_1 =\= basic(_) |
	match(Term_1?,State_2?,State_2',State\State',Intersection_table),
	collision(Terms_1?,State_2'?,State'\Rest,Intersection_table).

collision([],_,(State\State)^,_).

% match(Term_1,List_2,List,List_21,_,_) ---
%		Finds a unifiable term for Term_1 in List_2. 
%		List_21 is the same as List_2 after deleting the unifiable
%		term for Term_1. A unifiable term is added to List.


procedure match(Element,State,State,State\State,Table).
%******************************************************

match(Term_1,[Term_1|Terms_2],Terms_2^,([Term_1|Terms]\Terms)^,_).

match(  functor(Term_1,Functor_1/Arity_1),
	[functor(Term_2,Functor_2/Arity_2)|Terms_2],
	Terms_2^,
	([functor(Term,Functor_1/Arity_1)|Terms]\Terms)^,
	Intersection_table
) :-
	Functor_1 = Functor_2,
	Arity_1 = Arity_2:
	make_tuple(Arity_1,Term),
	arg(1,Term,Functor_1)|
        intersect_arguments(2,
	                    Arity_1,
			    Term_1,
			    Term_2,
			    Term,
			    Intersection_table).

match(  functor(Term_1,Functor_1/Arity_1),
	[compound(Term_2,Arity_2/Include_functors)|Terms_2],
	[compound(Term_2,Arity_2/Include_functors)|Terms_2]^,
	([functor(Term,Functor_1/Arity_1)|Terms]\Terms)^,
	Intersection_table
) :-
	Arity_1 = Arity_2,
	Include_functors = yes:
	make_tuple(Arity_1,Term),
	arg(1,Term,Functor_1) |
        intersect_arguments(2,
	                    Arity_1,
			    Term_1,
			    Term_2,
			    Term,
			    Intersection_table).

match(  compound(Term_1,Arity_1/Include_functors),
	[functor(Term_2,Functor_2/Arity_2)|Terms_2],
	Terms_2',
	([functor(Term,Functor_2/Arity_1)|Terms]\Terms')^,
	Intersection_table
) :-
	Arity_1 = Arity_2,
	Include_functors = yes :
	make_tuple(Arity_1,Term),
	arg(1,Term,Functor_2) |
        intersect_arguments(2,Arity_1,Term_1,Term_2,Term,Intersection_table),
	match(compound(Term_1,Arity_1/Include_functors),
              Terms_2?,
	      Terms_2',
	      Terms\Terms',
	      Intersection_table).

match(  compound(Term_1,Arity_1/Include_functors_1),
	[compound(Term_2,Arity_2/Include_functors_2)|Terms_2],
	Terms_2^,
	([compound(Term,Arity_1/Include_functors)|Terms]\Terms)^,
	Intersection_table
) :-
	Arity_1 = Arity_2:
	make_tuple(Arity_1,Term)|
        intersect_arguments(1,Arity_1,Term_1,Term_2,Term,Intersection_table),
	intersect_include_functors(Include_functors_1?,
                                   Include_functors_2?,
				   Include_functors).

match(  Term_1,
	List,
	Terms_2^,
	([Term|Terms]\Terms)^,
	Intersection_table
) :-
	Term_1 = list(_,_),
	List = [Term_2|Terms_2],
	Term_2 = list(_,_):
	Term =list(_,_) |
	intersect_arguments(2,3,Term_1,Term_2,Term,Intersection_table).

match(Term_1,List,Terms,State,Intersection_table) :-
	List = [Term_2|Terms_2],
	Term_2 =  basic(Basic_type)|
        intersect_term_with_basic(Term_1?,
	                          Basic_type?,
				  Terms_2,
				  Terms,
				  State,
				  Intersection_table).

match(_,[],[]^,(Terms\Terms)^,_).

match(  Term_1,
	[Term_2|Terms_2],
	[Term_2|Terms_2']^,
	Terms,
	Intersection_table
) :-
	otherwise |
	match(Term_1,Terms_2?,Terms_2',Terms,Intersection_table).
	

YesNo ::= yes;no.

procedure intersect_include_functors(YesNo,YesNo,YesNo).
%*******************************************************

intersect_include_functors(yes,yes,yes^).

intersect_include_functors(no,_,no^).

intersect_include_functors(_,no,no^).


procedure intersect_term_with_basic(Element,
                                    Basic,
				    State,
				    [basic(Basic)|State],
				    State\State,
				    Table).
%*********************************************************

intersect_term_with_basic(constant(Term_1),
                          'Integer',
			  Terms_2,
			  [basic('Integer')|Terms_2]^,
			  ([constant(Term_1)|State]\State)^,
			  _
) :-
	integer(Term_1)|
        true.

intersect_term_with_basic(constant(Term_1),
                          'Real',
			  Terms_2,
			  [basic('Real')|Terms_2]^,
			  ([constant(Term_1)|State]\State)^,
			  _
) :-
	real(Term_1)|
        true.

intersect_term_with_basic(constant(Term_1),
                          'String',
			  Terms_2,
			  [basic('String')|Terms_2]^,
			  ([constant(Term_1)|State]\State)^,
			  _
) :-
	string(Term_1)|
        true.

intersect_term_with_basic(functor(Term_1,Name),
                          'Tuple',
			  Terms_2,
			  [basic('Tuple')|Terms_2]^,
			  ([functor(Term_1,Name)|State]\State)^,
			  _).

intersect_term_with_basic(compound(Term_1,Name),
                          'Tuple',
			  Terms_2,
			  [basic('Tuple')|Terms_2]^,
			  ([compound(Term_1,Name)|State]\State)^,
			  _).

intersect_term_with_basic(Term_1,
                          'Any',
			  [],
			  [basic('Any')]^,
			  ([Term_1|State]\State)^,
			  _).

intersect_term_with_basic(Term_1,
                          Basic,
			  Terms_2,
			  [basic(Basic)|Terms]^,
			  State,
			  Intersection_table
) :-
	otherwise |
        match(Term_1,Terms_2?,Terms,State,Intersection_table).

procedure intersect_basic(Basic,State,State,State\State).
%********************************************************

intersect_basic('Integer',Terms_2,Terms,State) :-
	intersect_integer(Terms_2?,Terms,State).

intersect_basic('Real',Terms_2,Terms,State) :-
	intersect_real(Terms_2?,Terms,State).

intersect_basic('String',Terms_2,Terms,State) :-
	intersect_string(Terms_2?,Terms,State).

intersect_basic('Vector',Terms_2,Terms,State) :-
	intersect_vector(Terms_2?,Terms,State).

intersect_basic('Tuple',Terms_2,Terms,State) :-
	intersect_tuple(Terms_2?,Terms,State).

intersect_basic('Any',Terms_2,[]^,(Terms_2\[])^).%special case.should be checked


procedure intersect_integer(State,State,State\State).
%****************************************************

intersect_integer(List,Terms,([Term_2|State]\State')^) :-
	List = [Term_2|Terms_2],
	Term_2 = constant(X),
	integer(X) |
        intersect_integer(Terms_2?,Terms,State\State').

intersect_integer(List,Terms_2^,([Term_2|State]\State)^) :-
	List = [Term_2|Terms_2],
	Term_2 = basic('Integer') |
        true.

intersect_integer([Term_2],[Term_2]^,([basic('Integer')|State]\State)^) :-
	Term_2 = basic('Any') |
        true.

intersect_integer([],[]^,(State\State)^).

intersect_integer([Term_2|Terms_2],[Term_2|Terms]^,States) :-
	otherwise|
        intersect_integer(Terms_2?,Terms,States).


procedure intersect_real(State,State,State\State).
%*************************************************

intersect_real(List,Terms,([Term_2|State]\State')^) :-
	List = [Term_2|Terms_2],
	Term_2 = constant(X),
	real(X) |
        intersect_real(Terms_2?,Terms,State\State').

intersect_real(List,Terms_2^,([Term_2|State]\State)^) :-
	List = [Term_2|Terms_2],
	Term_2 = basic('Real') |
        true.

intersect_real([Term_2],[Term_2]^,([basic('Real')|State]\State)^) :-
	Term_2 = basic('Any') |
        true.

intersect_real([],[]^,(State\State)^).

intersect_real([Term_2|Terms_2],[Term_2|Terms]^,States) :-
	otherwise|
        intersect_real(Terms_2?,Terms,States).


procedure intersect_vector(State,State,State\State).
%***************************************************

intersect_vector(List,Terms_2^,([Term_2|State]\State)^) :-
	List = [Term_2|Terms_2],
	Term_2 = basic('Vector') |
        true.

intersect_vector([Term_2],[Term_2]^,([basic('Vector')|State]\State)^) :-
	Term_2 = basic('Any') |
        true.

intersect_vector([],[]^,(State\State)^).

intersect_vector([Term_2|Terms_2],[Term_2|Terms]^,States) :-
	otherwise|
        intersect_vector(Terms_2?,Terms,States).


procedure intersect_string(State,State,State\State).
%***************************************************

intersect_string(List,Terms,([Term_2|State]\State')^) :-
	List = [Term_2|Terms_2],
	Term_2 = constant(X),
	string(X) |
        intersect_string(Terms_2?,Terms,State\State').

intersect_string(List,Terms_2^,([Term_2|State]\State)^) :-
	List = [Term_2|Terms_2],
	Term_2 = basic('String') |
        true.

intersect_string([Term_2],[Term_2]^,([basic('String')|State]\State)^) :-
	Term_2 = basic('Any') |
        true.

intersect_string([],[]^,(State\State)^).

intersect_string([Term_2|Terms_2],[Term_2|Terms]^,States) :-
	otherwise|
        intersect_string(Terms_2?,Terms,States).


procedure intersect_tuple(State,State,State\State).
%**************************************************
intersect_tuple(List,Terms,([Term_2|State]\State')^) :-
	List = [Term_2|Terms_2],
	Term_2 = functor(_,_)|
        intersect_tuple(Terms_2?,Terms,State\State').

intersect_tuple(List,Terms,([Term_2|State]\State')^) :-
	List = [Term_2|Terms_2],
	Term_2 = compound(_,_)|
        intersect_tuple(Terms_2?,Terms,State\State').

intersect_tuple(List,Terms_2^,([Term_2|State]\State)^) :-
	List = [Term_2|Terms_2],
	Term_2 = basic('Tuple') |
        true.

intersect_tuple([Term_2],[Term_2]^,([basic('Tuple')|State]\State)^) :-
	Term_2 = basic('Any') |
        true.

intersect_tuple([],[]^,(State\State)^).

intersect_tuple([Term_2|Terms_2],[Term_2|Terms]^,States) :-
	otherwise|
        intersect_tuple(Terms_2?,Terms,States).

procedure intersect_arguments(Integer,Integer,Tuple,Tuple,Tuple,Table).
%**********************************************************************

intersect_arguments(N,
	            Arity,
		    Term_1,
		    Term_2,
		    Term,
		    Intersection_table
) :-
	N =< Arity,
	arg(N,Term_1,State_1),
	arg(N,Term_2,State_2),
	arg(N,Term,State):
	N1 := N + 1|
	intersect(State_1?,State_2?,State,Intersection_table),
	intersect_arguments(N1,
	                    Arity,
			    Term_1,
			    Term_2,
			    Term,
			    Intersection_table).
	
intersect_arguments(N,Arity,_,_,_,_) :-
	N > Arity | 
        true.


procedure intersect(State,State,State,Table).
%********************************************

intersect(State_1,State_2,State_1^,_) :-
	State_1 =?= State_2 |
        true.

intersect(State_1,State_2,State,Intersection_table) :-
	otherwise |
	member(State_1?,State_2?,State,Intersection_table,Intersection_table).
