-language([colon,typed]).
-export([build_automaton/6,lookup_tree/6]).
-mode(trust).

/*

build automaton/6 provides the automaton - {Procedures,Types,Remote}. The
flags are used for termination detection.

lookup_tree/6 provides tree search operation. The additional flags are used
for write access.

*/

Definition ::= (LHS ::= RHS).

LHS ::= Var_ro(Name) ; (Var_ro(Name),LHS) ; (LHS, LHS).

Name ::= String.


procedure build_automaton(List,List,Automaton,done,done,DList).
%**************************************************************

build_automaton(Terms,(Terms'?)^,{Procs,Declared-Not_declared,Remote}^,
		(D1?)^,(D3?)^,Outs
):- 
	extract_types_and_declaration(Terms?,
	                              Terms',
				      Decs,
				      Procs,
				      Remote,
				      Not_declared,
				      Done?,     % type tree closed	
				      done\D,    % type tree finished.
				      done\D1,   % procedure tree finished.
				      Outs),
	canonical(D? \Done,
	               Decs,D\Done,Declared - Not_declared,Decs,Remote,D1\D2),
	close(Remote,D2?,D?,D3).
% D is enough (not Done) since Done is dependent on D and D2.

procedure extract_types_and_declaration(List,List,Def_dict,Proc_tree,Remote,
	                                DFA,done,S_circuit,S_circuit,DList).
%******************************************************************************

 % builds the procedure tree using decs and returning remote and
 % undeclared in procs, and builds the decs that will be turned into DFA

extract_types_and_declaration([procedure(Type)|Terms],
	                      Terms',
			      Decs,
			      Procs,
			      Remote,
			      Undeclared,
			      Dn,
			      S_circuit,
			      (D\D1)^,
			      (Out1\Out3)^

) :-
    tuple(Type),
    arg(1,Type,Func),
    string(Func),
    arity(Type,Arity),
    make_tuple(Arity,Type'),
    arg(1,Type',Func)|
        process_arguments(Dn?,Type?,Type'-Undeclared,2,Arity,Decs,Remote,D\D2),
        lookup_tree(Func/Arity,[functor(Type',Func/Arity)],Procs,D2\D3,R,Dn?),
	check_duplicate(R, Func/Arity, Out1, Out2),
% the done should be last - a(X,X?)					<-
	extract_types_and_declaration(Terms?,
                                      Terms',
				      Decs,
				      Procs,
				      Remote,
				      Undeclared,
				      Dn,
				      S_circuit,
				      D3\D1,
				      Out2\Out3).

extract_types_and_declaration([procedure(Func)|Terms],
	                      Terms',
			      Decs,
			      Procs,
			      Remote,
			      Undeclared,
			      Dn,
			      S_circuit,
			      (D\D1)^,
			      Outs
) :-
    string(Func),
    make_tuple(1,Type),
    arg(1,Type,Func) |
	extract_types_and_declaration([procedure(Type) | Terms?],
                                      Terms',
				      Decs,
				      Procs,
				      Remote,
				      Undeclared,
				      Dn,
				      S_circuit,
				      D\D1,
				      Outs).

extract_types_and_declaration([{'::=',Any,RHS}|Terms],
	                      Terms',
			      Decs,
			      Procs,
			      Remote,
			      Undeclared,
			      Done,
			      (D\D1)^,
			      Short_circuit,
			      (Out1\Out3)^
) :-
	check_Lhs(Any,RHS,Decs,D\D2,Out1,Out2), 
	extract_types_and_declaration(Terms?,
	                              Terms',
				      Decs,
				      Procs,
				      Remote,
				      Undeclared,
				      Done,
				      D2\D1,
				      Short_circuit,
				      Out2\Out3).

extract_types_and_declaration([Term|Terms],
	                      [Term|Terms'?]^,
			      Decs,
			      Procs,
			      Remote,
			      Undeclared,
			      Done,	
			      Short_circuit1,
			      Short_circuit2,
			      Outs
) :-
	otherwise |
	extract_types_and_declaration(Terms?,
	                              Terms',
				      Decs,
				      Procs,
				      Remote,
				      Undeclared,
				      Done,
				      Short_circuit1,
				      Short_circuit2,
				      Outs).

extract_types_and_declaration([],[]^,_,_,_,_,_,(D\D)^,(D1\D1)^,(Out\Out)^).

procedure check_Lhs(LHS,RHS,Def_dict,S_circuit,List,List).
%*********************************************************

check_Lhs(`Name,RHS,Dict,D\D1,Out1,Out2):- 
	lookup_tree(Name,Data,Dict,D\D2,Reply,done),
	check_double(Reply?,Name,RHS,Data,D2\D1,Out1,Out2).
check_Lhs((X,Y),RHS,Dict,(D\D1)^,Out1,Out3):- 
	check_Lhs(X?,RHS,Dict,D\D2,Out1,Out2),
	check_Lhs(Y?,RHS,Dict,D2\D1,Out2,Out3).
check_Lhs(X,_,_,(D\D)^,[invalid_type_name(X) | Out]^,Out):- 
    otherwise |
	true.

procedure check_double(Reply,Name,RHS,{State,RHS},S_circuit,List,List).
%**********************************************************************

check_double(add,_,RHS,{_,RHS}^,(D\D)^,Out^,Out).
check_double(_,Name,_,_,(D\D)^,[multiply_defined(Name) | Out]^,Out):- 
    otherwise |
	true.

procedure check_duplicate(Reply, Procedure_name,List,List).
%**********************************************************

check_duplicate(add, _, Out^, Out).
check_duplicate(_, PName, [multiply_declared(PName) |Out]^, Out) :-
    otherwise |
	true.

/*
 
 canonical/7 gets Declarations as a tree of elements of the form
 {State_name,{State,RHS},Left,Right} where each variable on the RHS is
 represented by the tuple {_var,'Variable_name'}. It transforms Declarations
 into canonical representation, and builds the automaton.
 The automaton is of the form ({Name,State,L,R}-{Name,State,L,R},Remote).
 where the difference list consists of the automaton with pointers for 
 states of undefined types which are put in the second part of the differrence
 list to be found later. Remote is a tree of Module_names and appropriate DFA.

*/

procedure canonical(S_circuit,(Def_dict ; []),S_circuit,DFA-DFA,
	                                            Def_dict,Remote,S_circuit).
%******************************************************************************

 % The short circuit in the first argument starts as soon as the type
 % tree is filled up , and it detects the time when it is closed. this
 % should be known, since looking up a type and not finding it may
 % result in adding an unknown type to the tree. the third argument
 % feeds process with the final 'done'

canonical((Done\Closed)^,
	  Def_dict,
	  (C\C1)^,
	  ({Name?,State?,Left',Right'}-DFA)^,
	  Old_Decs,
	  Remote,
	  (D\D1)^
) :-
    Def_dict = {Name,{State,RHS},Left,Right}|
	process(Closed?,RHS?,State-DFA,Old_Decs,Remote,D\D2),
	canonical(Done? \Closed,Right,C\C2,Right'-DFA,Old_Decs,Remote,D2\D3),
	canonical(Done? \Closed,Left,C2\C1,Left'-DFA,Old_Decs,Remote,D3\D1).

canonical(done\_,[]^,(D1\D1)^,_,_,_,(D\D)^).
 % the definition dictionary is closed when the tree is completed.

procedure process(done,RHS,State-DFA,Def_dict,Remote,S_circuit).
%***************************************************************

process(Done,(Term;Terms),([Term'?|Terms'?]-DFA)^,Old_Decs,Remote,D\D1) :-
	process1(Done,Term?,Term'-DFA,Old_Decs,Remote,D\D2),
	process(Done,Terms?,Terms'-DFA,Old_Decs,Remote,D2\D1).

process(Done,`Name,State,Old_Decs,Remote,Short_circuit) :-
	process_var_ro(Done,Name,State,Old_Decs,Remote,Short_circuit).

process(Done,?Name,State,Old_Decs,Remote,Short_circuit) :-
	process_var_ro(Done,Name,State,Old_Decs,Remote,Short_circuit).

process(_,Remote_name,(State-_)^,_,Remote,Short_circuit) :-
    Remote_name = (_#_) |
	break(Remote_name,Path,Name,Ok),
	look_up1(Remote,Path,Name,State,Ok?,Short_circuit).

process(Done,Term,((State?)-DFA)^,Old_Decs,Remote,Short_circuit) :-
    Term = [Term'] |
	State = [constant([]),list(State1?,State)],
        process(Done,Term'?,State1-DFA,Old_Decs,Remote,Short_circuit).

process(Done,Term,([{F?,Term'?,N?}]-DFA)^,Old_Decs,Remote,Short_circuit) :-
    tuple(Term),
    Term =\= (_;_),
    Term =\= (_#_),
    Term =\= `_,
    Term =\= ?_ ,
    arity(Term,Arity),
    make_tuple(Arity,Term') |
        process_tuple(Done,
		      Term,
                      {F,Term',N}-DFA,
		      Arity,
		      Old_Decs,
		      Remote,
		      Short_circuit).

process(_,Term,([constant(Term)]-_)^,_,_,(D\D)^) :-
    constant(Term) |
        true.
process(Done,List,([list(X'?,Xs'?)]-DFA)^,Old_Decs,Remote,(D\D1)^) :-
    List = [X|Xs],
    Xs =\= [] |
        process(Done,X?,X'-DFA,Old_Decs,Remote,D\D2),
        process(Done,Xs?,Xs'-DFA,Old_Decs,Remote,D2\D1).


procedure process1(done,Any,Element-DFA,Def_dict,Remote,S_circuit).
%******************************************************************

process1(Done,Term,({F?,Term'?,N?}-DFA)^,Old_Decs,Remote,Short_circuit) :-
    tuple(Term),
    Term =\= (_#_),
    Term =\= `_,
    Term =\= ?_ ,
    arity(Term,Arity),
    make_tuple(Arity,Term') |
        process_tuple(Done,
		      Term,
                      {F,Term',N}-DFA,
		      Arity,
		      Old_Decs,
		      Remote,
		      Short_circuit).

process1(Done,List,(list(X'?,Xs'?)-DFA)^,Old_Decs,Remote,D\D1) :-
    List = [X|Xs],
    Xs =\= [] |
        process(Done,X?,X'-DFA,Old_Decs,Remote,D\D2),
        process(Done,Xs?,Xs'-DFA,Old_Decs,Remote,D2\D1).

process1(Done,`Name,State,Old_Decs,Remote,Short_circuit) :-
  process_var_ro(Done,Name,State,Old_Decs,Remote,Short_circuit).

process1(Done,?Name,State,Old_Decs,Remote,Short_circuit) :-
  process_var_ro(Done,Name,State,Old_Decs,Remote,Short_circuit).

% here, the basic(_) can only be nested inside a state, since we can get
% states with more than one element

process1(_,Remote_name,(State-_)^,_,Remote,Short_circuit) :-
    Remote_name = (_#_) |
	break(Remote_name?,Path,Name,Ok),
	look_up1(Remote,Path,Name,State,Ok?,Short_circuit).

process1(Done,Term,(State-DFA)^,Old_Decs,Remote,Short_circuit) :-
    Term = [Term'] :
	State = [constant([]),list(State1?,State)] |
        process(Done,Term'?,State1-DFA,Old_Decs,Remote,Short_circuit).

process1(_,Term,(constant(Term)-_)^,_,_,(D\D)^) :-
    constant(Term) | 
        true.


procedure process_tuple(done,
			Tuple,
	                Tuple_element-DFA,
			Arity,
			Def_dict,
			Remote,
			S_circuit).
%*******************************************

process_tuple(Done,
	      Term,
	      (functor(Term',(Functor/Arity))-DFA)^,
	      Arity,
	      Old_Decs,
	      Remote,
	      Short_circuit
) :-
    arg(1,Term,Functor),
    string(Functor) |
	arg(1,Term',Functor),
        process_arguments(Done,Term?,Term'-DFA,
	                   2,Arity,
			   Old_Decs,
			   Remote,
			   Short_circuit).
process_tuple(Done,
	      Term,
	      (functor(Term',Quoted/Arity)-DFA)^,
	      Arity,
	      Old_Decs,
	      Remote,
	      Short_circuit
) :-
    arg(1,Term,BackQuoted),
    BackQuoted = Bq(Quoted),
    Bq = '`' |
	arg(1,Term',Quoted),
        process_arguments(Done,Term?,Term'-DFA,
	                   2,Arity,
			   Old_Decs,
			   Remote,
			   Short_circuit).
process_tuple(Done,
	      Term,
	      (compound(Term',Arity/do_not_know)-DFA)^,
	      Arity,
	      Old_Decs,
	      Remote,
	      D\D1
) :-
    otherwise |
        process_arguments(Done,Term?,Term'-DFA,
	                   1,Arity,
			   Old_Decs,
			   Remote,
			   D\D1).


procedure process_arguments(done,
			    Tuple,
	                    Element-DFA,
			    Integer,
			    Arity,
			    Def_dict,
			    Remote,
			    S_circuit).
%****************************************

process_arguments(Done,Term,
	          Term'-DFA,
	          A,
		  Arity,
		  Old_Decs,
		  Remote,
		  (D\D1)^
) :-
    A =< Arity,
    arg(A,Term,Arg),
    arg(A,Term',State),
    A1 := A + 1 |
	process(Done,Arg?,State-DFA,Old_Decs,Remote,D\D2),
	process_arguments(Done,Term,Term'-DFA,A1,Arity,Old_Decs,Remote,D2\D1).

process_arguments(_,_,_,A,Arity,_,_,(D\D)^) :-
    A > Arity |
        true.


procedure process_var_ro(done,Any,State-DFA,Def_dict,Remote,S_circuit).
%**********************************************************************

process_var_ro(Done,Name,State-Undeclared,Old_Decs,_,(D\D1)^) :-
    known(Name) |
	lookup_tree(Name,{State,_},Old_Decs,D\D2,Reply,Done?),
	check(Reply?,Name,State-Undeclared,D2\D1).


procedure check(RestrictedReply,Name,State-DFA,S_circuit).
%***********************************************

RestrictedReply ::= find ; not_found.

check(not_found,Name,State,Sc):-
	look_up_Basic_types(Name,State,Sc). % state == state - undeclared
check(find,_,_,(D\D)^).

procedure look_up_Basic_types(String,State-DFA,S_circuit).
%*********************************************************

look_up_Basic_types('Integer',([basic('Integer')]-_)^,(D\D)^).
look_up_Basic_types('Real',([basic('Real')]-_)^,(D\D)^).
look_up_Basic_types('String',([basic('String')]-_)^,(D\D)^).
look_up_Basic_types('Tuple',([basic('Tuple')]-_)^,(D\D)^).
look_up_Basic_types('Vector',([basic('Vector')]-_)^,(D\D)^).
look_up_Basic_types('Any',([basic('Any')]-_)^,(D\D)^).
look_up_Basic_types('Number',([basic('Integer'),basic('Real')]-_)^,(D\D)^).
look_up_Basic_types('Constant',([basic('Integer'),basic('Real'),
                                 basic('String'),constant([])]-_)^,(D\D)^).
look_up_Basic_types('Compound',([basic('Tuple'),List,basic('Vector')]-_)^,
			(D\D)^
):- 
	List = list([basic('Any')],[basic('Any')]).
look_up_Basic_types('Nil',([constant([])]-_)^,(D\D)^).
look_up_Basic_types('None',([constant('_any')]-_)^,(D\D)^).
look_up_Basic_types(Name,State-DFA,Short_circuit) :-
      otherwise |
	lookup_tree(Name,State,DFA,Short_circuit,_,done).

procedure look_up1(Remote,Path,Any,State,True,S_circuit).
%********************************************************

look_up1(_,Path,NonVar,[basic('Any')]^,false,(D\D)^) :-
/***/	computation#event(invalid_path(Path#NonVar)).
 %either path or name was illegal 
 %put basic(any), tell nothing???
look_up1(_,Path,NonVar,[basic('Any')]^,true,(D\D)^) :-
    NonVar =\= `_, NonVar =\= ?_ |
/***/	computation#event(invalid_type(Path#NonVar)).
look_up1(Remote,Path,{_,Name},State,true,(D\D1)^):-
    otherwise |
	lookup_tree(Path,DFA,Remote,D\D2,_,done),
	look_up2(Name,State,DFA,D2\D1).

procedure look_up2(Name,State,DFA,S_circuit).
%********************************************

look_up2(Name,State,DFA,D2\D1) :-
    known(D2) |
	   lookup_tree(Name,State,DFA,D2\D1,_,done).

procedure unify(Any,Any,S_circuit).
%**********************************

unify(X^,X,(D\D)^).
unify(_,_,(D\D)^) :-
    otherwise |
	true.

procedure close(Remote,done,done,done).
%**************************************

close(Remote,done,done,Done1) :-
	close(Remote,done\Done1).


procedure close((Remote ; []),S_circuit).
%****************************************

close([]^,(D\D)^).
close({_,DFA,Left,Right},(D\D1)^) :-
        close1(DFA,D\D2),
	close(Left,D2\D3),
	close(Right,D3\D1).

procedure close1(Tree,S_circuit).
%********************************

close1([]^,(D\D)^).
close1({_,_,Right,Left},(D\D1)^) :-
        close1(Right,D\D2),
	close1(Left,D2\D1).

Remote_name ::= (Tuple ; String).
True ::= true ; false.

procedure break(Remote_name,Remote_name,Tuple,True).
%***************************************************

 % break is used instead of computation_utils#call_context_goal/4 because
 % the module might not exist, yet the path might be a possible one.

break(Remote,(First#Answer)^,Name,(Ok?)^):-
	Remote = (First # Rest),
	Rest = (_#_),
	string(First)|
                break(Rest,Answer,Name,Ok). % no vars or tuples allowed
break(Remote,First^,Last^,true^):- 
	Remote = (First # Last),
	Last =\= (_#_),
	string(First)|
                true.
break(_,_,_,false^):- 
	otherwise | 
                true.

procedure lookup_tree(Key,Data,Tree,S_circuit,Reply,done).
%*********************************************************

lookup_tree(Key,Data,Tree,S_circuit,Reply,Ready):-
    Tree = {Key',_,Left,_},
    Key @< Key' |
	lookup_tree(Key,Data,Left,S_circuit,Reply,Ready).
lookup_tree(Key,Data,Tree,S_circuit,Reply,Ready):-
    Tree = {Key',_,_,Right},
    Key' @< Key |
	lookup_tree(Key,Data,Right,S_circuit,Reply,Ready).
lookup_tree(Key,Data,Tree,S_circuit,find^,_):-
    Tree = {Key',_,_,_},
    Key = Key' |
	unify(Tree, {Key,Data,_,_}, S_circuit). 
lookup_tree(Key,Data,Tree,(D\D)^,find^,_):-
    Tree = {Key',Data',_,_},
    Key = Key',
    Data =\= Data' |
	true.
lookup_tree(Key,Data,Tree,S_circuit,Reply,done):-
    true :
      Tree = Tree'?,
      Reply = add |
	unify(Tree',{Key,Data,_,_},S_circuit).
lookup_tree(_,_,[],(D\D)^,not_found^,done).
