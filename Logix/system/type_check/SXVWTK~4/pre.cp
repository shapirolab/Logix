-language(typed).
-mode(trust).
-export([precom/4, append/3]).

Procedures ::= [Procedure].
Procedure ::= {String/Int,Clauses}.
Clauses ::= [Clause].
Clause ::= Atom ; (Atom :- Atoms).
Atoms ::= Atom ; (Atom,Atoms) ; (Atoms | Atoms).
Atom ::= Tuple ; String.
Bnd_list ::=  [(Var,Any)].
Ns ::= Int\Int.
List ::= [Any].
Var ::= {Var_name,Name}.
Var_name ::= '_var'.
Ss ::= ServiceId\ServiceId.

/*
   pre#precom receives as input its first argument - Clauses . it pre
   processes the given clauses and returns procedures as a list of two argument
   tuples - procedure name and relevant procedures, while translating Fcp into
   a regular logic program and substituting variables according to
   assignments in the guard and in the body.  
*/

procedure  precom(Clauses,Procedures,Dict,Ss).
%*********************************************

precom(Clauses,[{Id?,Procedure?}|Procedures],Dictionary,Ss) :-
	Clauses? = [Clause|_],
	tuple(Clause) |
          procedure_name(Clause?,Id),
	  precom(Clauses?,Id?,Procedure,Procedures,Dictionary,Ss).

precom(Clauses,[{Id,Procedure?}|Procedures],Dictionary,Ss) :-
	Clauses? = [Name|_],
	string(Name),
	  Id = Name/1 |
	  precom(Clauses?,Name/1,Procedure,Procedures,Dictionary,Ss).

precom([],[],_,SID\SID).

procedure precom(Clauses,String/Int,Clauses,Procedures,Dict,Ss).
%***************************************************************

/* 
   checks whether the given clause has the same procedure name as the
   previous one, and if it does it calls bindings/3 to remove = and =?=
   calls from the guard and body, while creating a binding list . the
   binding list is applied to the head using process_arguments/4 .
   find_remote_calls/4 finds r_c's and puts them in the Dictionary.
*/

precom([Head|Clauses],
       Procedure_name,
       [Head'? | Procedure],
       Procedures,
       Dictionary,
       Ss
) :-
     Head? =\= ( _ :- _ ),
     arg(1,Head,Name),
     arity(Head,Arity),
     (Name? / Arity? ) =?= Procedure_name,
     make_tuple(Arity,Tuple) |
          process_arguments(Head,Tuple,Head',1,Arity,1\_,[]),
	  precom(Clauses?,Procedure_name,Procedure,Procedures,Dictionary,Ss).

precom([Name|Clauses],
       Procedure_name,
       [Name|Procedure],
       Procedures,
       Dictionary,
       Ss
) :-
     (Name / 1) =?= Procedure_name |
	  precom(Clauses?,Procedure_name,Procedure,Procedures,Dictionary,Ss).

precom( [Clause|Clauses],
	Procedure_name,
	[Clause'|Procedure],
	Procedures,
	Dictionary,
        SID\SID1
) :-
    Clause? = (Head :- RHS),
    arg(1,Head,Name),
    arity(Head,Arity),
    (Name? / Arity? ) =?= Procedure_name,
    make_tuple(Arity,Tuple),
    Clause' = (New_head? :- New_RHS?) |
        process_arguments(Head?,Tuple,New_head,1,Arity?,1\N,Final?),
        precom(Clauses?,Procedure_name?,Procedure,Procedures,Dictionary,
		SID\SID2),
	transform_rhs(RHS?, N?, New_RHS, Dictionary, Final,SID2\SID1).

precom( [Clause|Clauses],
	Procedure_name,
	[Clause'|Procedure],
	Procedures,
	Dictionary,
	SID\SID1
) :-
    Clause? = (Name :- RHS),
    (Name? / 1) =?= Procedure_name,
    Clause' = (Name :- New_RHS?) |
        precom(Clauses?,Procedure_name?,Procedure,Procedures,Dictionary,
		SID\SID2),
	transform_rhs(RHS?, 1, New_RHS, Dictionary, _,SID2\SID1).

precom(Clauses,_,[],Procedures,Dictionary,Ss) :-
   otherwise |
          precom(Clauses?,Procedures,Dictionary,Ss).


procedure transform_rhs(Atoms,Int,Atoms,Dict,Bnd_list,Ss).
%*********************************************************

transform_rhs(RHS,N,New_RHS,Dictionary,Final,SID\SID1) :-
    RHS =?= (Guard | Body),
      New_RHS = (New_guard? | New_body?) |
	simplify_guard(Guard?,Guard'),
	transform(Guard'?,New_guard,[],Bindings,N\N1,_,Final?,SID\SID2),
	transform(Body?,New_body,[],Bindings1,N1\_,Dictionary,Final?,
			SID2\SID1),
        process_bindings(Bindings?,Bindings?,Bindings_t),
	process_bindings(Bindings_t?,Bindings1?,Bindings_k),
	find_and_process_tuple_and_list(Bindings_k?,Final,Bindings_k?).
transform_rhs(RHS,N,New_RHS,Dictionary,Final,SIDL) :-
    RHS =?= (_ : _) |
	transform_rhs((RHS | true),N,New_RHS,Dictionary,Final,SIDL).
transform_rhs(Body,N,New_body,Dictionary,Final,Ss) :-
    Body =\= (_ | _) |
        transform(Body?,New_body,[],Bindings,N\_,Dictionary,Final?,Ss),
	process_bindings(Bindings?,Bindings?,Bindings1),
	find_and_process_tuple_and_list(Bindings1?,Final,Bindings1?).

procedure simplify_guard(Any, Any).
%**********************************

simplify_guard(Compound, (Ask?, Guard?)) :-
    Compound =?= ((Ask, Asks) : Tell) |
	simplify_guard((Asks : Tell), Guard).
simplify_guard(Guard, (Ask?, Tell?)) :-
    Guard =?= (Ask : Tell),
    Ask =\= (_ , _) |
	true.
simplify_guard(Guard, Guard) :-
    Guard =\= (_ : _) |
	true.
	
/*
The binding list.
transform_rhs/7 creates it. using strip_bindings/4,it brings the bindings
to the form of _var(X) = any_term before adding them to the list.
then, the bindings are processed, bringing the binding list to a state
where there is no case of the same variable on the left hand side and
(free) in the right hand side (except when part of a list or a tuple).
those last cases are dealt with by find_and_process_tuple/3 which
evaluates the tuples and lists on the rhs (X,f(Y)),(Y,T)->(X,f(T)),(Y,T) ,
while dealing with circular terms, so that (X=f(Y),Y=f(X)) won't cause a loop.
*/

procedure transform(Atoms,Atoms,Bnd_list,Bnd_list,Ns,Dict,Bnd_list,Ss).
%**********************************************************************

/*
transform/8 processes clauses. in each clause, a list of variable
bindings is created, returned as an agument to precom/5 which sends it
to be processed, and returned again - a final binding list as the
seventh argument. this list is applied to each variable by process/4,
which, in addition, removes ro's and numerotates unknown vars.
it also finds remote procedure calls, and adds them to the dictionary after
processing them ( removing ro's and naming unknown vars process / 4),
unrecognized remote calls (with vars in them) are substituted by 'true'. 
*/

transform(Input,Atoms',Bindings,Bindings'',N\N3,Dict,Final,Ss):- 
                        Input =?= (Atom,Atoms),
			Atom? = {Equal,X,Y},
                        Equal? = '='|
	process(X?,X',N\N1,[]),
	process(Y?,Y',N1\N2,[]),
	strip_binding(X'?,Y'?,Bindings,Bindings'),
	transform(Atoms?,Atoms',Bindings',Bindings'',N2\N3,Dict,Final,Ss).

transform(Input,Atoms',Bindings,Bindings'',N\N3,Dict,Final,Ss):- 
                        Input =?= (Atom,Atoms),
			Atom? = {Equal,X,Y},
                        Equal? = '=?='|
	process(X?,X',N\N1,[]),
	process(Y?,Y',N1\N2,[]),
	strip_binding(X'?,Y'?,Bindings,Bindings'),
	transform(Atoms?,Atoms',Bindings',Bindings'',N2\N3,Dict,Final,Ss).

transform(Input,(Atom',Atoms'),Bindings,Bindings',N\N2,Dict,Final,Ss):- 
                        Input =?= (Atom,Atoms),
                        Atom? =\= (_ = _),
                        Atom? =\= (_ =?= _),
			Atom? =\= (_ # _)|
	process(Atom,Atom',N\N1,Final),
	transform(Atoms?,Atoms',Bindings,Bindings',N1\N2,Dict,Final,Ss).

transform(Input,(Atom''?,Atoms'),Bindings,Bindings',N\N2,Dict,Final,
		SID\SID1
):- 
                        Input =?= (Atom,Atoms),
                	Atom =?= (_#_) |
	process(Atom?,Atom',N\N1,Final),
	transform_rpc(Atom'?,Dict,Atom'',SID\SID2),
	transform(Atoms?,Atoms',Bindings,Bindings',N1\N2,Dict,Final,SID2\SID1).

transform(Atom,Atom',Bindings,Bindings,N\N1,_,Final,SID\SID):- 
                        Atom =\= (_#_),
			Atom =\= (_, _),
			Atom =\= (_ = _),
			Atom =\= (_ =?= _)|
	process(Atom,Atom',N\N1,Final).

transform(Atom,Atom'',Bindings,Bindings,N\N1,Dict,Final,Ss):-
                	Atom =?= (_#_) |
	process(Atom?,Atom',N\N1,Final),
	transform_rpc(Atom'?,Dict,Atom'',Ss).

transform(Atom,true,Bindings,Bindings',N\N2,_,_,SID\SID):- 
                        Atom? = {Equal,X,Y},
                        Equal? = '='|                        
	process(X?,X',N\N1,[]),
	process(Y?,Y',N1\N2,[]),
	strip_binding(X'?,Y'?,Bindings,Bindings').

transform(Atom,true,Bindings,Bindings',N\N2,_,_,SID\SID):- 
                        Atom? = {Equal,X,Y},
                        Equal? = '=?='|
	process(X?,X',N\N1,[]),
	process(Y?,Y',N1\N2,[]),
	strip_binding(X'?,Y'?,Bindings,Bindings').

transform(Atom,Atom,Bindings,Bindings,N\N,_,_,SID\SID):- 
	                 string(Atom) | 
	true.

procedure transform_rpc(Atom,Dict,Atom,Ss).
%******************************************

transform_rpc(Computation,_,true,SID\SID) :-
    Computation =?= computation # _ |
	true.
transform_rpc(Self,_,Atom',SID\SID) :-
    Self =?= self # Atom,
      Atom = Atom' |
	true.
transform_rpc(Super,Dict,Atom',Ss) :-
    Super =?= super # Atom |
	hier#identify_module_name(Atom,Dict,Atom',Ss).
transform_rpc(Atom,Dict,Atom',Ss) :-
	otherwise |
	hier#identify_module_name(Atom,Dict,Atom',Ss).

procedure procedure_name(Clause,String/Int).
%*******************************************

 % finds procedure name 

procedure_name(Clause,Name? / Arity?):-
   Clause =\= ( _ :- _ ),
   arg(1,Clause,Name),
   arity(Clause,Arity) |
	   true.
procedure_name(X,X? / 1):- 
   string(X) |
           true.	
procedure_name(Clause,Name? / Arity?):-
   Clause? = (Head :- _),
   arg(1,Head,Name),
   arity(Head,Arity) |
           true.
procedure_name(Clause,Head? / 1):-
   Clause? = (Head :- _),
   string(Head)|
             true.

procedure process(Any,Any,Ns,Bnd_list).
%**************************************

/*
   processes the given term by removing ro's and substituting appropriate
   terms in the head using the binding list. in case a tuple is found,
   process_arguments/6 is invoked, alter/3 does the substitution of terms. the
   last argument (the binding list) is nil when the processed expression is
   not from the head of a clause
*/
 
process(Term,Term',Ns,Binding_list) :-
	tuple(Term),
        Term =\= `_,
        Term =\= ?_,
	arity(Term,Arity),
	make_tuple(Arity,Tuple) |
        process_arguments(Term,Tuple,Term',1,Arity,Ns,Binding_list).

process(Y,Y',N\N1,Binding_list) :-
        Y? = [X|Xs],
        Y' = [X'?|Xs'?] |
        process(X?,X',N\N2,Binding_list),
        process(Xs?,Xs',N2\N1,Binding_list).

process(Expr,`Name?,N\N1,_) :-
	Expr =?= ?'_' |
	N1 := N + 1,
	make_unique_name(N, Name).

process(Expr,`Name?,N\N1,_) :-
	Expr =?= `'_' |
	N1 := N + 1,
	make_unique_name(N, Name).

process(Expr,New_expression,N\N,Binding_list) :-
	Expr =?= ?Name,
	Name =\= '_'  |	
	alter(Name, Binding_list?, New_expression).

process(Expr,New_expression,N\N,Binding_list) :-
	Expr =?= `Name,
        Name =\= '_'  |	
	alter(Name, Binding_list?, New_expression).

process(Term,Term,N\N,_) :-
	constant(Term) | 
        true.

procedure make_unique_name(Integer, String).
%*******************************************

make_unique_name(N, Name) :-
    convert_to_string(N, S),
    string_to_dlist(S, D, []),
    ascii('_', UnderScore),
    list_to_string(0, [UnderScore | D], Name) |
	true.

procedure alter(Any,Bnd_list,Any).
%*********************************

 % searches for variable in the binding_list. if found - returns equivalent
 % expression , otherwise the variable itself is returned.

alter(Name, [(Variable, Equivalent) | _], Equivalent):-
    Variable =?= `Name |
	true.
alter(Name, [_ | Rest], Result):- 
    otherwise | 
	alter(Name, Rest?, Result).
alter(Name, [], `Name).

procedure process_arguments(Tuple,Tuple,Tuple,Int,Int,Ns,Bnd_list).
%************************************************************

% special case for process - expression is a tuple 

process_arguments(Term,Term',Tuple,A,Arity,N\N1,Binding_list) :-
	A =< Arity,
	arg(A,Term,Arg),
	arg(A,Term',Arg'?),
	A1 := A + 1 |
        process(Arg?,Arg',N\N2,Binding_list?),
	process_arguments(Term,Term',Tuple,A1,Arity,N2\N1,Binding_list).
process_arguments(_,Tuple,Tuple,A,Arity,N\N,_) :-
	A > Arity | true.

procedure strip_binding(Any,Any,Bnd_list,Bnd_list).
%**************************************************

strip_binding(X,Y,Binding_list,Binding_list') :-
	tuple(X),
	tuple(Y),
        X =\= `_,
	Y =\= `_,
	X == Y,
        arity(X,Arity) |
        process_tuple(X,Y,1,Arity,Binding_list,Binding_list').

strip_binding(X',Y',Binding_list,Binding_list'') :-
             X'? = [X|Xs],
             Y'? = [Y|Ys]   | 
        strip_binding(X,Y,Binding_list,Binding_list'),
        strip_binding(Xs?,Ys?,Binding_list',Binding_list'').
strip_binding(X,Term,Binding_list,[(X,Term)|Binding_list]) :-
	 X =?= `_ | 
                 true.
strip_binding(Term,Y,Binding_list,[(Y,Term)|Binding_list]) :-
	 Y =?= `_ |
                 true.
strip_binding(X,Y,Binding_list,Binding_list):- 
               constant(X),
	       X =?= Y| 
               true.
strip_binding(X,Y,Binding_list,Binding_list):- 
         otherwise | 
               screen#display(ununifiable(X,Y),type(ground)).
                
procedure process_tuple(Tuple,Tuple,Int,Int,Bnd_list,Bnd_list).
%**************************************************************

process_tuple(_,_,Current,Arity,Binding_list,Binding_list):- 
        Current > Arity |
                   true.

process_tuple(X,Y,Current,Arity,Binding_list,Binding_list'):- 
	Current =< Arity,
	arg(Current,X,Xm),
        arg(Current,Y,Ym)  |
                   Next := Current + 1,
                   strip_binding(Xm,Ym,Binding_list,Binding_list''),
                   process_tuple(X,Y,Next,Arity,Binding_list'',Binding_list').

procedure process_bindings(Bnd_list,Bnd_list,Bnd_list).
%******************************************************

process_bindings([],Binding_list,Binding_list).
process_bindings([(X,Y)|Rest],Binding_list,Binding_list_f):- 
      iterate1(X,Y,Binding_list?,Binding_list',New_subs),
      process_bindings(New_subs?,Binding_list'?,Binding_list''),
      process_bindings(Rest?,Binding_list'',Binding_list_f).

procedure iterate1(Var,Any,Bnd_list,Bnd_list,Bnd_list).
%*******************************************************

iterate1(X,Y,Binding_list,Binding_list,[]):- X =?= Y | true.
iterate1(X,Y,Binding_list,Binding_list',New_subs):- 
                     otherwise |
	     iterate(X?,Y,Binding_list?,Binding_list',New_subs).

procedure iterate(Var,Any,Bnd_list,Bnd_list,Bnd_list).
%*****************************************************

iterate(X,Y,[],[(X,Y)],[]).
iterate(`X,Y,[(Z,W)|Rest],
            [(Z,Y)|Rest'],
            [(Z,Y)|New_subs]):- 
                        W? = Var(Name),
                        Var? = '_var',
			Name? = X? |			
           iterate(`X?,Y?,Rest?,Rest',New_subs).
iterate(`X,Y,[(Z,W)|Rest],Rest'',New_subs):-
                  Z? = {Var,Z'},
		  Var? = '_var',
		  X? = Z'?  |  
      check_equal(`X,Y,W,Rest'?,Rest'',New_subs'?,New_subs),
      iterate(`X,Y,Rest,Rest',New_subs').
iterate(X,Y,[Expression|Rest],[Expression|Rest'],New_subs):- 
                otherwise |     
           iterate(X,Y,Rest?,Rest',New_subs).

procedure check_equal(Var,Var,Var,Bnd_list,Bnd_list,Bnd_list,Bnd_list).
%**********************************************************************

check_equal(_,Y,W,Rest,Rest,New,New):- Y =?= W | 
                         true. 

check_equal(X,Y,W,Rest,Rest',New_bindings,New_bindings'):- 
               otherwise |
                   strip_binding(Y,W,[],Bindings),
                   append([(X,Y)|Rest?],Bindings?,Rest'),
                   append([(X,Y)|New_bindings?],Bindings?,New_bindings').
                       
procedure find_and_process_tuple_and_list(Bnd_list,Bnd_list,Bnd_list).
%*********************************************************************

find_and_process_tuple_and_list([],[],_).
find_and_process_tuple_and_list([(X,Y)|Rest],
                                [(X,Y'?)|Rest'],
                                Binding_list):- 
      tuple(Y),
      arg(1,Y,N),
      N? =\= '_var',
      arity(Y,A),
      make_tuple(A,Y') |
              tuple_alter([X],1,A,Y,Y',Binding_list),
              find_and_process_tuple_and_list(Rest?,Rest',Binding_list).
find_and_process_tuple_and_list([(X,Y)|Rest],
                                [(X,Y')|Rest'],
                                Binding_list):- 
      Y? = [Z|Zs],
      Y' = [Z'?|Zs'?] | 
              list_alter([X],Z?,Z',Binding_list),
              list_alter([X],Zs?,Zs',Binding_list),
              find_and_process_tuple_and_list(Rest?,Rest',Binding_list).
find_and_process_tuple_and_list([(X,Y)|Rest],
                                [(X,Y)|Rest'],
                                Binding_list):- 
      otherwise | 
              find_and_process_tuple_and_list(Rest?,Rest',Binding_list).

procedure list_alter(List,Any,Any,Bnd_list).
%*******************************************

list_alter(L,Z,Z',Binding_list):- 
	Z? = [X|Xs],
        Z' = [X'?|Xs'?] |
                 list_alter(L,X?,X',Binding_list),
                 list_alter(L,Xs?,Xs',Binding_list).
list_alter(L,Z,Z',Binding_list):- 
      tuple(Z),
      Z =\= `_,
      arity(Z,A), 
      make_tuple(A,Z') |
              tuple_alter(L,1,A,Z,Z',Binding_list).
list_alter(_,Z,Z,_):- 
      constant(Z) | true.
list_alter(L,Z,Z',Binding_list):- 
	Z =?= `_ |
              search(L,Z,Z',Binding_list?,Binding_list).

procedure tuple_alter(List,Int,Int,Tuple,Tuple,Bnd_list).
%********************************************************

tuple_alter(_,Current,Arity,_,_,_):- 
          Current > Arity |
                true.
tuple_alter(List,Current,Arity,Y,Y',Binding_list):- 
          Current =< Arity,
          arg(Current,Y,Org),
          arg(Current,Y',New_value?),
          tuple(Org),
          arg(1,Org,F),
          F? =\= '_var',
          arity(Org,Arity1),
          make_tuple(Arity1,New_value)| 
                tuple_alter(List,1,Arity1,Org,New_value,Binding_list),
                Next := Current + 1,
                tuple_alter(List,Next,Arity,Y,Y',Binding_list).
tuple_alter(List,Current,Arity,Y,Y',Binding_list):- 
          Current =< Arity,
          arg(Current,Y,Org),
          arg(Current,Y',Org),
          constant(Org) | 
                 Next := Current + 1,         
                 tuple_alter(List,Next,Arity,Y,Y',Binding_list).
tuple_alter(List,Current,Arity,Y,Y',Binding_list):- 
	Current =< Arity,
        arg(Current,Y,Z),
        arg(Current,Y',Z'),
        Z? = [X|Xs],
        Z' = [X'?|Xs'?] |
                 list_alter(List,X?,X',Binding_list),
                 list_alter(List,Xs?,Xs',Binding_list),
                 Next := Current + 1,         
                 tuple_alter(List,Next,Arity,Y,Y',Binding_list).       
tuple_alter(List,Current,Arity,Y,Y',Binding_list):- 
          Current =< Arity,
          arg(Current,Y,Org),
          arg(Current,Y',New_value?),
          Org? =  {Var,_},
	  Var? = '_var'| 
                 search(List,Org?,New_value,Binding_list?,Binding_list),
                 Next := Current + 1,         
                 tuple_alter(List,Next,Arity,Y,Y',Binding_list).

procedure search(List,Var,Any,Bnd_list,Bnd_list).
%************************************************

% if found in binding list attempt to alter in regard to restricted 
% variables (List) else, return original

search(_,Org,Org,[],_).
search(List,Org,New_value,[(`X,Subs)|_],Binding_list):- 
             Org =?= `X? | 
       search_vars(List?,List,Subs?,Org,New_value,Binding_list).
search(List,Org,New_value,[_|Rest],Binding_list):- 
            otherwise |
                search(List,Org,New_value,Rest?,Binding_list).

procedure search_vars(List,List,Any,Var,Any,Bnd_list).
%*****************************************************

 % if var is in restricted var list then return var else add to
 % restricted variables list and check to see what Subs is

search_vars([],List,Subs,Org,New_value,Binding_list):-
               search_vars1([Org|List],Subs,New_value,Binding_list).
search_vars(List,_,_,Org,Org,_):- 
                List? = [X|_],
                X =?= Org | true.
search_vars([_|Xs],List,Subs,Org,New_value,Binding_list):- 
                otherwise | 
         search_vars(Xs?,List,Subs,Org,New_value,Binding_list).
  
procedure search_vars1(List,Any,Any,Bnd_list).
%*********************************************

 % if new_expr (Subs) is not a variable then, process on, else return
 % new value 
 
search_vars1(List,Subs,New_value,Binding_list):- 
	tuple(Subs),
        Subs =\= `_,
        arity(Subs,Arity),
        make_tuple(Arity,New_value) | 
               tuple_alter(List,1,Arity,Subs,New_value,Binding_list).  
search_vars1(List,Subs,New_value,Binding_list):- 
	Subs? = [X|Xs],
        New_value = [X'?|Xs'?] |
               list_alter(List,X?,X',Binding_list),
               list_alter(List,Xs?,Xs',Binding_list).  
search_vars1(_,New_value,New_value,_):-
	otherwise |
                      true.

procedure append(List,List,List).
%********************************

append([X|Xs],Ys,[X|Zs]):-
                append(Xs?,Ys?,Zs).
append(Xs,[Y|Ys],[Y|Zs]):- 
                append(Xs?,Ys?,Zs).
append([],Zs,Zs).
append(Zs,[],Zs).
