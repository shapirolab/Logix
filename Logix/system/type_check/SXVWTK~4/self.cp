-language([colon, typed]).
-export([module/2, build_automaton/6,
	 put_Any_in_arguments/3, guard_types/3,
	 message/3, redundant/3]
).
-mode(trust).

SystemReply ::= true ; false(Any).
ServiceId ::= [String].

Answer ::= [{Procedure_name, Diagnose}].
Procedure_name ::= Name/Arity.
Diagnose ::= [Dgns].
Dgns ::= No ; Rdn.
No ::= no(Trace, Any).
Rdn ::=  redundant(Int, (Trace ; String)).
Trace ::= ['List', Int, String/Arity].

Ok ::= Any.

Reply ::= add ; found.

Proc_tree ::= {Name/Arity, Procedure, Proc_tree, Proc_tree} ; [].

Procedure ::= [Functor_element | Nil].

Remote ::= {String, DFA, Remote, Remote} ; [].

DFA ::= {Name,State,DFA,DFA} ; [].

Name ::= String.

State ::= [Element].

Element ::= constant(Constant) ; Tuple_element;
            list(State,State) ; Basic_element ; State.

RHS ::= Any.

Tuple_element ::= Functor_element;
	          compound(Tuple, Arity/do_not_know).

Functor_element ::= functor(Tuple, String/Arity).

YesNo ::= yes ; no ; do_not_know.

Basic_element ::= basic(Basic).

Basic ::= 'Integer' ; 'Real' ; 'String' ; 'Vector' ; 'Tuple' ; 'Any'.

Dictionary, Dict ::= {String, DFA, Dict, Dict} ; [].

Definition ::= (Var_ro(Name) ::= RHS).

Def_dict ::= {Name, {State,RHS}, Def_dict, Def_dict}.

Var_ro ::= '_var' ; '_ro'.

Path ::= {`('#'),String,Path} ; String. % String#Path ; String

Arity ::= Integer.

S_circuit ::= Any\Any.

Automaton ::= {Proc_tree, DFA-DFA, Remote}.

List ::= [Any].

DList ::= List \ List.

Redundant ::= redundant ; not_redundant.

Int ::= Integer.
procedure build_automaton(List,List,Automaton,done,done,DList).
%**************************************************************

build_automaton(Terms1, Terms2, Automaton, Done1, Done2, Outs) :-
	automaton #
		build_automaton(Terms1, Terms2, Automaton, Done1, Done2, Outs).


procedure module(Any,List).
%**************************

module(Path,(Output?)^):-
	computation_utils#call_id_goal(self#Path,SID,Module_name,Ok),
	type_check2(Ok?,SID,Module_name,Output).

procedure type_check2(SystemReply,Any,Any,List).
%***********************************************
type_check2(true,SID,Module_name,Output):-
	get_source#context(SID,Module_name,[],Result,{Output,Output1}),
	type_check3(Result?,SID,Module_name,Output1).

type_check2(false(Other),_,_,[Other]^).

procedure type_check3(Any,Any,Any,List).
%***************************************

type_check3(module(Options,Attributes,Procs),SID,Module_name,Output):-
    true : Dictionary = {String,{Type,DFA},_,_} |
	transform#languages(Options,typed,Attributes,_,Procs,Terms,
				 Output,Output1
		  ),					% proc_tree,everything
	automaton#build_automaton(Terms?,Terms',Automaton,PD,TD,Errs\Errs1),
	utils#binary_sort_merge(Errs,Output1),
	Automaton? = {Type',DFA - Rest,Remote},
	nfa_to_dfa(PD?,Type',Type,DD),
	hier#id_to_string([Module_name | SID],String),
	pre#precom(Terms'?,Terms'',Dictionary,SID\SID1),
	hier#get(Module_name?,SID1,true,Rest,Dictionary,TD?),
	extend_automaton(DD?,Remote?,Dictionary,SID),
	type_check(Terms''?,Type,Errs1,DD?).

type_check3(library(_,_,_),_,_,[library]^).

type_check3(false(Remark),_,_,[Remark]^).

procedure extend_automaton(done,Remote,Dictionary,ServiceId).
%************************************************************

extend_automaton(done,Remote,Dictionary,SID) :-
	hier#extend_automaton(Remote,Dictionary,SID).

/*
all the following missing portion has been made a separate module - 
hier. that module takes care of hierarchical type declaration retrieving
*/

procedure type_check(pre#Procedures,State,List,done).
%****************************************************

type_check(Input,
	Type,
	Diagnostics,
	D
):-
    Input = [{Name / Arity,Procedure} | Procedures],
    Arity > 1 :
      N := Arity - 1 |
	lookup_tree(Name/Arity,Type',Type,R,D?), % <- here
 	check_proc(R?,Name/Arity,Type'),
	procedure_type_check(Procedure?,
	                     Type?,
			     Type'?,
			     Answer1,
			     Diagnostic,
			     D),
	filter_yes_answers(Name/N,
	                   Answer1?,
			   Diagnostic?,
			   Diagnostics\Diagnostics1
		           ),
	type_check(Procedures?,Type,Diagnostics1,D).

 % -> we do not wish to add by ourselves procs that can be added
 % otherwise, so we wait for automaton to finish building the tree
					     
type_check(Input,Type,Diagnostics,D):-
    Input = [{Name/1,Procedure}|Procedures]|
	procedure_type_check(Procedure?,Type?,[],Answer1,Diagnostic,D),
	filter_yes_answers(Name/0,Answer1?,Diagnostic?,
				Diagnostics\Diagnostics1
	),
	type_check(Procedures?,Type,Diagnostics1,D).

type_check([],_,[]^,_).


filter_yes_answers(_,yes,[],(Diagnostics\Diagnostics)^).

filter_yes_answers(Name,
	           yes,
	           Diagnostic,
		   ([{Name,Diagnostic}|Diagnostics]\Diagnostics)^
):-
    list(Diagnostic) |
        true.

filter_yes_answers(Name,
	           No,
	           Diagnostic,
		   ([{Name,[No|Diagnostic]}|Diagnostics]\Diagnostics)^
):-
	tuple(No)|
	true.


procedure_type_check(Procedure,Type,Type',Answer1,Diagnostic,D):-
	infer_type(Procedure?,1,Type,Inferred_type,D),
	filter_redundant_messages(Inferred_type,Inferred,Diagnostic\[]),
	nfa_to_dfa#nfa_to_dfa(Inferred?,Inferred'),
	intersection#intersect(Inferred'?,Type'?,Inferred''),
	equivalent#equivalent(Type'?,Inferred''?,Answer1).


filter_redundant_messages([X|Inferred_type],
                          Inferred,
			  ([X|Diagnostic]\Diagnostic1)^
):-
	arg(1,X,Functor),
	Functor = redundant |
	filter_redundant_messages(Inferred_type,
                                  Inferred,
				  Diagnostic\Diagnostic1).

filter_redundant_messages([],[]^,(Diagnostic\Diagnostic)^).

filter_redundant_messages([X|Inferred_type],
                          [X|Inferred]^,
			  Diagnostic):-
	otherwise|
	filter_redundant_messages(Inferred_type,
                                  Inferred,
				  Diagnostic).

procedure infer_type(Element,Integer,State,State).
%*************************************************

infer_type(	[Clause|Clauses],
	        Clause_number,
		Type,
		[Clause_inferred_type|Inferred_type]^,
		D
):-
	true:
	Clause_number' := Clause_number + 1 |
	clause_parts(Clause?,Head,Ask,Tell,Body),
        table_of_var_occurrence_type_p(	Head?,
					Type,
					Occurrence_type_table,
					done\Done,
					D),
	table_of_var_occurrence_type_c(	Ask?,
					Type,
					Occurrence_type_table,
					Done\Done1,
					D),
	table_of_var_occurrence_type_c(	Tell?,
					Type,
					Occurrence_type_table,
					Done1\Done2,
					D),
        table_of_var_occurrence_type_c(	Body?,
					Type,
					Occurrence_type_table,
					Done2\Done3,
					D),
	table_of_vars_type(Done3?,Occurrence_type_table,Type_table),
        clause_type(Head,Clause_number,Type_table,Clause_inferred_type),
	infer_type(Clauses?,Clause_number',Type,Inferred_type,D).

infer_type([],_,_,[]^,_).

clause_parts(Clause, Head1, Ask1, Tell1, Body1) :-
    Clause = (Head :- Ask : Tell | Body) :
      Head = Head1, Ask = Ask1, Tell = Tell1, Body = Body1 |
	true.
clause_parts(Clause, Head1, Guard1, true^, Body1) :-
    Clause = (Head :- Guard | Body),
    Guard =\= (_:_) :
      Head = Head1, Guard = Guard1, Body = Body1 |
	true.
clause_parts(Clause, Head1, true^, true^, Body1) :-
    Clause =?= (Head :- Body),
    Body =\= (_|_) :
      Head = Head1, Body = Body1 |
	true.
clause_parts(Head, Head1, true^, true^, true^) :-
    Head =\= (_:-_) :
      Head = Head1 |
	true.


table_of_var_occurrence_type_c(true,_,_,(Done\Done)^,_).
table_of_var_occurrence_type_c(	Conjunct,
				Type,
				Occurrence_type_table,
				Done\Done2,
				D):-
	Conjunct = (Atom,Rest) |
	table_of_var_occurrence_type_g(	Atom?,
					Type,
					Occurrence_type_table,
					Done\Done1,
					D),
	table_of_var_occurrence_type_c(	Rest?,
					Type,
					Occurrence_type_table,
					Done1\Done2,
					D).
table_of_var_occurrence_type_c(	Atom,
				Type,
				Occurrence_type_table,
				Done,
				D):-
    otherwise |
	table_of_var_occurrence_type_g(	Atom,
					Type,
					Occurrence_type_table,
					Done,
					D).

table_of_var_occurrence_type_g(Remote,_,Occurrence_type_table,Done,_):-
    Remote = (Type(Path) # Atom) |
	nfa_to_dfa#nfa_to_dfa([Type?],Type'),
	insert(Atom?,Type'?,Occurrence_type_table,[Path#''],Done).

table_of_var_occurrence_type_g(Atom,Type,Occurrence_type_table,Done,D) :-
    otherwise |
	table_of_var_occurrence_type_p(Atom,Type,Occurrence_type_table,Done,D).

table_of_var_occurrence_type_p(	Atom,
				_,
				_,
				(Done\Done)^,
				_):-
    Atom = `_ |
	true.

table_of_var_occurrence_type_p(Atom,_,_,(Done\Done)^,_):-
    string(Atom) |
	true.

table_of_var_occurrence_type_p(	Atom,
				Type,
				Occurrence_type_table,
				Done,
				D):-
    otherwise,
    arg(1,Atom,Name),
    arity(Atom,Arity) |
	lookup_tree(Name / Arity,Type',Type,R,D?),
	check_proc(R?,Name/Arity,Type'),
	insert(Atom?,Type'?,Occurrence_type_table,[],Done).


insert(Term,State,Occurrence_type_table,Trace,Done):-
	tuple(Term),
	Term =\= `_ |
	insert_compound_or_functor(Term,State?,Occurrence_type_table,
					Trace,Done
	).

insert(Term,State,Occurrence_type_table,Trace,Done):-
	Term = [Car|Cdr] |
	insert_list(Car,Cdr,State,Occurrence_type_table,Trace,Done).

insert(Term,State,Occurrence_type_table,Trace,Done):-
	constant(Term) |
	insert_constant(Term,State?,Occurrence_type_table,Trace,Done).

insert(Variable,State,Occurrence_type_table,_,Done):-
	Variable = `Term |
	put(Term?,State,Occurrence_type_table,Done).



insert_compound_or_functor(Term,State,Occurrence_type_table,Trace,Done):-
	arg(1,Term,Functor),
	string(Functor),
	arity(Term,Arity) |
	insert_functor(Term,Functor,Arity,State?,
	                                     Occurrence_type_table,Trace,Done).

insert_compound_or_functor(Term,State,Occurrence_type_table,Trace,Done):-
	arg(1,Term,Functor),
	Functor = `_,
	arity(Term,Arity) |
      extract_compound_and_functors(State?,Arity,Compounds,New_functor,Basics),
      make_new_state(Arity,Compounds?,New_functor?,Basics?,State'), 
      insert_compound(Term,Arity,State'?,Occurrence_type_table,Trace,Done).

insert_compound_or_functor(Term,State,Occurrence_type_table,Trace,Done):-
	otherwise,
	arity(Term,Arity) |
	insert_compound(Term,Arity,State?,Occurrence_type_table,Trace,Done).


extract_compound_and_functors([Element|_],_,[]^,[]^,[Element]^):-
	Element = basic('Tuple') |
        true.

extract_compound_and_functors([Element|_],_,[]^,[]^,[Element]^):-
	Element = basic('Any') |
        true.

extract_compound_and_functors(States,
	                      Arity,
			      [Term|Compounds]^,
			      [Functor|New_functor]^,
			      Basics
):-
	States = [Element|State],
	Element = compound(Term,Arity'/_),
	Arity = Arity',
	arg(1,Term,Functor) |
        extract_compound_and_functors(State?,
	                              Arity,
				      Compounds,
				      New_functor,
				      Basics).

extract_compound_and_functors(States,
	                      Arity,
			      [Term|Compounds]^,
			      [constant(Functor)|New_functor]^,
			      Basics
):-
	States = [Element|State],
	Element = functor(Term,Functor/Arity'),
	Arity = Arity' |
      extract_compound_and_functors(State?,Arity,Compounds,New_functor,Basics).

extract_compound_and_functors([],_,[]^,[]^,[]^).

extract_compound_and_functors([_|State],Arity,Compounds,New_functor,Basics):-
	otherwise |
      extract_compound_and_functors(State?,Arity,Compounds,New_functor,Basics).


make_new_state(Arity,Compounds,New_functor,[],[compound(New_Term,Arity/_)]^):-
	Compounds =\= [] :
	make_tuple(Arity,New_Term),
	arg(1,New_Term,New_functor') |
         nfa_to_dfa#nfa_to_dfa(New_functor?,New_functor'),
	 nfa_to_dfa#process_args(Arity,2,Compounds?,New_Term,_). 

make_new_state(_,_,_,[Basic],[Basic]^).

make_new_state(_,[],[],[],[]^).


insert_functor(Term,
	       Functor,
	       Arity,
	       [functor(Term',F' / A')|_],
	       Occurrence_type_table,
	       Trace,
	       Done
):-
        Functor = F',
	Arity = A' :
	N := Arity - 1 |
	insert_functor_arguments(Term,Term',2,Arity,Occurrence_type_table,
	                                              [Functor/N |Trace],Done).
insert_functor(Term,
	       Functor,
	       Arity,
	       [compound(Term',Arity'/Include_functors)|_],
	       Occurrence_type_table,
	       Trace,
	       Done
):-
	Arity = Arity',
	Include_functors = yes :
	N := Arity - 1 |
	insert_functor_arguments(Term,Term',2,Arity,Occurrence_type_table,
	                                              [Functor/N |Trace],Done).


insert_functor(Term,_,Arity,[Term'|_],Occurrence_type_table,_,Done):-
	Term' = basic('Tuple') |
        insert_arguments_in_Any(Term,2,Arity,Occurrence_type_table,Done).

insert_functor(Term,_,Arity,[Term'|_],Occurrence_type_table,_,Done):-
	Term' = basic('Any') |
        insert_arguments_in_Any(Term,2,Arity,Occurrence_type_table,Done).

insert_functor(Term,_,_,[],Occurrence_type_table,Trace,Done):-
	put((Term,Trace),[],Occurrence_type_table,Done).

insert_functor(Term,Functor,Arity,[_|State],Occurrence_type_table,Trace,Done
):-
	otherwise |
        insert_functor(Term,Functor,Arity,State?,Occurrence_type_table,Trace,
	                                                                 Done).



insert_compound(Term,
                Arity,
		[compound(Term',Arity'/_)|_],
		Occurrence_type_table,
		Trace,
		Done
):-
	Arity = Arity'|
	insert_arguments(Term,Term',1,Arity,Occurrence_type_table,
	                                           ['Tuple'/Arity|Trace],Done).

insert_compound(Term,Arity,[Term'|_],Occurrence_type_table,_,Done):-
	Term' = basic('Tuple') |
        insert_arguments_in_Any(Term,1,Arity,Occurrence_type_table,Done).

insert_compound(Term,Arity,[Term'|_],Occurrence_type_table,_,Done):-
	Term' = basic('Any') |
        insert_arguments_in_Any(Term,1,Arity,Occurrence_type_table,Done).

insert_compound(Term,_,[],Occurrence_type_table,Trace,Done):-
	put((Term,Trace),[],Occurrence_type_table,Done).

insert_compound(Term,Arity,[_|State],Occurrence_type_table,Trace,Done):-
	otherwise |
        insert_compound(Term,Arity,State?,Occurrence_type_table,Trace,Done).
	

insert_list(Car,Cdr,[Term'|_],Occurrence_type_table,Trace,D\D1):-
	Term' = list(Car',Cdr') |
	insert(Car?,Car'?,Occurrence_type_table,['List'/1|Trace],D\D2),
	insert(Cdr?,Cdr'?,Occurrence_type_table,['List'/2|Trace],D2\D1).

insert_list(Car,Cdr,[Term'|_],Occurrence_type_table,_,D\D1):-
	Term' = basic('Any') |
        insert(Car?,[basic('Any')],Occurrence_type_table,_,D\D2),
        insert(Cdr?,[basic('Any')],Occurrence_type_table,_,D1\D2).

insert_list(Car,Cdr,[],Occurrence_type_table,Trace,Done):-
	put(([Car|Cdr],Trace),[],Occurrence_type_table,Done).

insert_list(Car,Cdr,[_|State],Occurrence_type_table,Trace,Done):-
	otherwise |
        insert_list(Car,Cdr,State?,Occurrence_type_table,Trace,Done).
	

insert_constant(Term,[Term'|_],_,_,(D\D)^):-
	Term' = constant(Term2),
	Term2 = Term |
        true.

insert_constant(Term,[Term'|_],_,_,(D\D)^):-
	integer(Term),
	Term' = basic('Integer') |
        true.

insert_constant(Term,[Term'|_],_,_,(D\D)^):-
	real(Term),
	Term' = basic('Real') |
        true.

insert_constant(Term,[Term'|_],_,_,(D\D)^):-
	string(Term),
	Term' = basic('String') |
        true.

insert_constant(_,[Term'|_],_,_,(D\D)^):-
	Term' = basic('Any') |
        true.

insert_constant(Term,[],Occurrence_type_table,Trace,Done):-
	put((Term,Trace),[],Occurrence_type_table,Done).

insert_constant(Term,[_|State],Occurrence_type_table,Trace,Done):-
	otherwise |
        insert_constant(Term,State?,Occurrence_type_table,Trace,Done).


insert_functor_arguments(Term,Term',N,Arity,Occurrence_type_table,Trace,D\D1
):-
	N =< Arity :
	N2 := N - 1,
	N1 := N + 1 |
	arg(N,Term,Arg),
	arg(N,Term',Arg'),
	insert(Arg?,Arg'?,Occurrence_type_table,[N2|Trace],D\D2),
        insert_functor_arguments(Term,Term',N1,Arity,
	                                    Occurrence_type_table,Trace,D2\D1).

insert_functor_arguments(_,_,N,Arity,_,_,(D\D)^):-
	N > Arity |
	true.	



insert_arguments(Term,Term',N,Arity,Occurrence_type_table,Trace,D\D1
):-
	N =< Arity :
	N1 := N + 1 |
	arg(N,Term,Arg),
	arg(N,Term',Arg'),
	insert(Arg?,Arg'?,Occurrence_type_table,[N|Trace],D\D2),
        insert_arguments(Term,Term',N1,Arity,
	                                    Occurrence_type_table,Trace,D2\D1).

insert_arguments(_,_,N,Arity,_,_,(D\D)^):-
	N > Arity |
	true.	


insert_arguments_in_Any(Term,N,Arity,Occurrence_type_table,D\D1):-
	N =< Arity,
	arg(N,Term,Arg) :
	N1 := N + 1 |
	insert(Arg?,[basic('Any')],Occurrence_type_table,_,D\D2),
	insert_arguments_in_Any(Term,N1,Arity,Occurrence_type_table,D2\D1).

insert_arguments_in_Any(_,N,Arity,_,(D\D)^):-
	N > Arity |
	true.	


put(Var_name,State,(Table?)^,Done):-
	Table = [{Var_name?,States}|_],
	put_state(State,States,Done).
put(Var_name,State,Table,Done):-
	Table = [{Var_name,States}|_] |
        put_state(State,States,Done).
put(Var_name,State,Table,Done):-
	Table = [{Var_name1,_}|Occurrence_type_table],
	Var_name1 =\= Var_name |
	put(Var_name,State,Occurrence_type_table,Done).


put_state(State,States,S_circuit):-
    true :
      States = States'?,
      S_circuit = D\D |
	States' = [State|_].

put_state(State,List,(D\D)^):-
    List = [State|_] |
	true.

put_state(State,List,Done):-
    List = [State1|States],
    State =\= State1 |
	put_state(State,States,Done).


table_of_vars_type(done,Occurrence_type_table,Type_table):-
	table_of_vars_type(Occurrence_type_table,Type_table).


table_of_vars_type([{Var_name,States}|Occurrence_type_table],
		   [{Var_name,Var_type?}|Type_table]^
):-
	table_of_vars_type(Occurrence_type_table,Type_table),
	intersect(States?,Var_type).
table_of_vars_type([]^,[]^).



intersect([State]^,State'):-
        remove_redundant_terms(State?,State').

intersect([X,X1|Xs],Var_type):-
	intersection#intersect(X?,X1?,X2'),
        remove_redundant_terms(X2'?,X2),
	intersect([X2|Xs],Var_type).


remove_redundant_terms([Term|State'],State):-
        redundant1(Term?,[],Answer),
	remove_redundant_terms(Answer?,Term,State',State).

remove_redundant_terms([],[]^).


remove_redundant_terms(redundant,_,State',State):-
	remove_redundant_terms(State'?,State).

remove_redundant_terms(not_redundant,Term,State',[Term|State]^):-
	remove_redundant_terms(State'?,State).


procedure redundant(State,[(State)],Redundant).
%**********************************************
 
redundant([Term|Terms],Ancesstors,Redundant):-
	redundant1(Term?,Ancesstors,Answer),
	check_answer(Answer?,Terms,Ancesstors,Redundant).
redundant([],_,redundant^).


check_answer(redundant,Terms,Ancesstors,Redundant):-
	redundant(Terms?,Ancesstors,Redundant).

check_answer(not_redundant,_,_,not_redundant^).


procedure redundant1(Element,[(State)],Redundant).
%*************************************************

redundant1(Term,_,not_redundant^):-
	Term = constant(_) |
        true.

redundant1(Term,_,not_redundant^):-
	Term = basic(_) |
        true.

redundant1([],_,redundant^).

redundant1(State,Ancesstors,Redundant):-
	list(State)|
        member(State?,Ancesstors?,Answer),
	continue_checking(State,_,3,2,Ancesstors,Redundant,Answer?).
	
redundant1(Term,Ancesstors,Answer):-
	Term = functor(Term', _/Arity)|
        redundant_arguments(Term'?,2,Arity,Ancesstors,Answer).
	
redundant1(Term,Ancesstors,Answer):-
	Term = compound(Term', Arity/_)|
        redundant_arguments(Term'?,1,Arity,Ancesstors,Answer).
	
redundant1(Term,Ancesstors,Answer):-
	Term = list(_,_)|
        redundant_arguments(Term?,2,3,Ancesstors,Answer).


redundant_arguments(Term,N,A,Ancesstors,Redundant):-
	N =< A|
	arg(N,Term,State),
        member(State?,Ancesstors?,Answer),
	continue_checking(State,Term,N,A,Ancesstors,Redundant,Answer?).

redundant_arguments(_,N,A,_,not_redundant^):-
	N > A |
        true.


continue_checking(_,_,_,_,_,not_redundant^,member).

continue_checking(State,Term,N,A,Ancesstors,Redundant,not_member):-
	redundant(State?,[State|Ancesstors],Answer),
	continue_checking1(Term,N,A,Ancesstors,Redundant,Answer?).


continue_checking1(_,_,_,_,redundant^,redundant).

continue_checking1(Term,N,A,Ancesstors,Redundant,not_redundant):-
	true :
	N1 := N + 1 |
        redundant_arguments(Term,N1,A,Ancesstors,Redundant).


member(X,List,member^):-
	List = [X|_] |
        true.

member(X,List,Member):-
	List = [Y|Xs],
        X =\= Y |
	member(X,Xs?,Member).

member(_,[],not_member^).


clause_type(Head,Clause_number,Type_table,Inferred_type):-
	redundancy_clause_check(Type_table?,Answer),
	clause_type(Answer?,Head,Clause_number,Type_table,Inferred_type).      


redundancy_clause_check([{_,Type}|Type_table],Answer):-
	Type =\= [] |
	redundancy_clause_check(Type_table?,Answer).

redundancy_clause_check([{Term,Type}|_],{Trace',Term',redundant}^):-
	Type = [],
	Term = (Term',Trace) |
        equivalent#rev(Trace?,Trace').

redundancy_clause_check([{Term,Type}|_],{Term,redundant}^):-
	Type = [],
	Term =\= (_,_) |
	true.

redundancy_clause_check([],not_redundant^).


clause_type({Term,redundant},_,Clause_num,_,redundant(Clause_num,Term)^).
clause_type({Trace,_,redundant},_,Clause_n,_,redundant(Clause_n,Trace)^).
clause_type(not_redundant,
	    Head,
	    _,
	    Type_table,
	    functor(Inferred_type,Functr/Arity)^
):-
	arg(1,Head,Functr),
	arity(Head,Arity) :
	make_tuple(Arity,Inferred_type),
	arg(1,Inferred_type,Functr) |
        process_arguments(Head?,Inferred_type,2,Arity,Type_table).

clause_type(not_redundant,Name,_,_,constant(Name)^):-
	string(Name) | true.

process(Term,[{F?,Term'?,N?}]^,Type_table):-
	tuple(Term),
	Term =\= `_,
	arity(Term,Arity) :
	make_tuple(Arity,Term') |
        process_tuple(Term,{F,Term',N},Arity,Type_table).

process([X|Xs],[list(X',Xs')]^,Type_table):-
        process(X?,X',Type_table),
        process(Xs?,Xs',Type_table).

process(VarName,Var,Type_table):-
    VarName = `Name |
	look_up(Type_table?,Name?,Var).

process(Term,[constant(Term)]^,_):-
	constant(Term) | 
        true.


process_arguments(Term,Term',A,Arity,Type_table):-
	arg(A,Term,Arg),
	arg(A,Term',Var) :
	A1 := A + 1 |
        process(Arg?,Var,Type_table),
	process_arguments(Term,Term',A1,Arity,Type_table).

process_arguments(_,_,A,Arity,_):-
	A > Arity | true.


process_tuple(Term,functor(Term',Functor/Arity)^,Arity,Type_table):-
	arg(1,Term,Functor),
	string(Functor) :
	arg(1,Term',Functor) |
        process_arguments(Term?,Term',2,Arity,Type_table).
			   
process_tuple(Term,compound(Term',Arity/Include_functors)^,Arity,Type_table):-
	otherwise |
	arg(1,Term',Arg1),
	nfa_to_dfa#find_if_includes_String_or_Any(Arg1?,
						  no\Include_functors,
						  _\_
			),
        process_arguments(Term?,Term',1,Arity,Type_table).
			   

look_up(Type_table,Name1,State^):-
	Type_table = [Argument|_],
        Argument = {Name1,State}|
         true.

look_up(Type_table,Name,Var):-
	Type_table = [Argument|Type_table'],
        Argument = {Name1,_},
	Name1 =\= Name |
	 look_up(Type_table'?,Name,Var).


procedure guard_types(Functor_element,Name/Arity,String).
%********************************************************

/*
** Guard Kernels
*/

guard_types(functor([basic('Any')] = [basic('Any')],'=' / 3)^,'=' / 3,_).
guard_types(functor([basic('Any')] \= [basic('Any')],'\=' / 3)^,'\=' / 3,_).
guard_types(functor([basic('Any')] =?= [basic('Any')],'=?=' / 3)^,'=?=' / 3,_).
guard_types(functor([basic('Any')] =\= [basic('Any')],'=\=' / 3)^,'=\=' / 3,_).
guard_types(functor([basic('Any')] == [basic('Any')],'==' / 3)^,'==' / 3,_).
guard_types(functor(Expression? =:= Expression?,'=:=' / 3)^, '=:=' / 3,_):-
	expression_type(Expression).
guard_types(functor(Expression? > Expression?, '>' / 3)^, '>' / 3,_):-
	expression_type(Expression).
guard_types(functor(Expression? >= Expression?, '>=' / 3)^, '>=' / 3,_):-
	expression_type(Expression).
guard_types(functor(Expression? < Expression?, '<' / 3)^, '<' / 3,_):-
	expression_type(Expression).
guard_types(functor(Expression? =< Expression?, '=<' / 3)^, '=<' / 3,_):-
	expression_type(Expression).
guard_types(functor([basic('Any')] @< [basic('Any')], '@<' / 3)^,'@<' / 3,_).
guard_types(functor([basic('String'),basic('Tuple')] # 
                            [basic('String'),basic('Tuple')],
		    '#' / 3)^,
            '#' / 3,_
).
guard_types(functor((Number := Expression?), ':=' / 3)^, ':=' / 3,_):-
    true : Number = [basic('Integer'), basic('Real')] |
	expression_type(Expression).
guard_types(functor(ascii([basic('String')],[basic('Integer')]), ascii / 3)^,
            ascii / 3,_
).
guard_types(functor(integer([basic('Integer')]), integer/2)^, integer/2,_).
guard_types(functor(real([basic('Real')]),real/2)^,real/2,_).
guard_types(functor(number([basic('Integer'),basic('Real')]), number/2)^,
            number/2,_
).
guard_types(functor(string([basic('String')]),string/2)^,string/2,_).
guard_types(functor(vector([basic('Vector')]),vector/2)^,vector/2,_).
guard_types(functor(channel([basic('Vector')]),channel/2)^,channel/2,_).
guard_types(functor(constant([basic('Integer'),basic('Real'),
                              basic('String'),constant([])]),
                    constant/2)^,
            constant/2,_
).
guard_types(functor(tuple([basic('Tuple')]),tuple/2)^,tuple/2,_).
guard_types(functor(list(List),list/2)^,list/2,_):- 
    true : List = [list([basic('Any')],[basic('Any')])] | true.
guard_types(functor(compound([List,basic('Tuple'),basic('Vector')]),
                    compound/2)^,
	    compound/2,_
):-
    true : List = [list([basic('Any')],[basic('Any')])] | true.
guard_types(functor(known([basic('Any')]),known/2)^,known/2,_).
guard_types(functor(unknown([basic('Any')]),unknown/2)^,unknown/2,_).
guard_types(functor(arity([basic('Tuple'),List,basic('Vector')],
                          [basic('Integer')]),
	            arity/3)^,
            arity/3,_
) :-
    true : List = list([basic('Any')],[basic('Any')]) | true.
guard_types(functor(arg([basic('Integer')],[basic('Tuple'),List],
                        [basic('Any')]),
                    arg/4)^,
            arg/4,_
):-
    true : List = list([basic('Any')],[basic('Any')]) | true.
guard_types(functor(string_length([basic('String')],[basic('Integer')]), 
                    string_length/3)^,
            string_length/3,_
).
guard_types(functor(string_hash([basic('String')],[basic('Integer')]), 
                    string_hash/3)^,
            string_hash/3,_
).
guard_types(functor(nth_char([basic('String')], 
                             [basic('Integer')],
                             [basic('String')]),
                    nth_char/4)^,
            nth_char/4,_
).
guard_types(functor(length([basic('String'),List],[basic('Integer')]),
                    length/3)^,
            length/3,_
) :-
    true : List = [constant([]),list([basic('Any')],List)] | true.
guard_types(functor(make_tuple([basic('Integer')],[basic('Tuple')]),
	            make_tuple/3)^,
            make_tuple/3,_
).
guard_types(functor(copy_skeleton(Compound,Compound),copy_skeleton/3)^,
            copy_skeleton/3,_
) :-
    true : Compound = [list([basic('Any')],[basic('Any')]),basic('Tuple')] | true.
guard_types(functor(string_to_dlist([basic('String')],[basic('Any')],
				    [basic('Any')]),
		    string_to_dlist/4)^,
            string_to_dlist/4,_
).
guard_types(functor(list_to_string([basic('Integer')],List,[basic('String')]),
	            list_to_string/4)^,
            list_to_string/4,_
):-
    true : List = [list([basic('Integer')],[constant([]),List])] | true.
guard_types(functor(list_to_string(List,[basic('String')]),
	            list_to_string/3)^,
            list_to_string/3,_
):-
    true : List = [list([basic('Integer')],[constant([]),List])] | true.
guard_types(functor(list_to_tuple(List,[basic('Tuple')]),list_to_tuple/3)^,
            list_to_tuple/3,_
):-
    true : List = [list([basic('Any')],[constant([]),List])] | true.
guard_types(functor(tuple_to_dlist([basic('Tuple')],List,[basic('Any')]),
                    tuple_to_dlist/4)^,
            tuple_to_dlist/4,_
):-
    true : List = [list([basic('Any')],[basic('Any')])] | true.
guard_types(functor(convert_to_integer([basic('Integer'),basic('Real'),
                                        basic('String'),constant([])],
                                       [basic('Integer')]),
                    convert_to_integer/3)^,
            convert_to_integer/3,_
).
guard_types(functor(convert_to_string([basic('Integer'),basic('Real'),
                                       basic('String'),constant([])],
                                      [basic('String')]),
                    convert_to_string/3)^,
            convert_to_string/3,_
).
guard_types(functor(convert_to_real([basic('Integer'),basic('Real'),
                                     basic('String'),constant([])],
                                    [basic('Real')]),
                    convert_to_real/3)^,
            convert_to_real/3,_
).
guard_types(functor(close_vector([basic('Integer')],[basic('Vector')]),
                    close_vector/3)^,
            close_vector/3,_
).
guard_types(functor(write_vector([basic('Integer')],[basic('Any')],
                                 [basic('Vector')],[basic('Vector')]),
                    write_vector/5)^,
            write_vector/5,_
).
guard_types(functor(write_vector([basic('Integer')],[basic('Any')],
                                 [basic('Vector')]),
                    write_vector/4)^,
            write_vector/4,_
).
guard_types(functor(store_vector([basic('Integer')],[basic('Any')],
                                 [basic('Vector')],[basic('Vector')]),
                    store_vector/5)^,
            store_vector/5,_
).
guard_types(functor(store_vector([basic('Integer')],[basic('Any')],
                                 [basic('Vector')]),
                    store_vector/4)^,
            store_vector/4,_
).
guard_types(functor(read_vector([basic('Integer')],[basic('Vector')],
                                [basic('Any')]),
                    read_vector/4)^,
            read_vector/4,_
).
guard_types(functor(make_vector([basic('Integer')],[basic('Vector')],
                                [basic('Tuple')]),
                    make_vector/4)^,
            make_vector/4,_
).
guard_types(functor(close_channel([basic('Vector')]),close_channel/2)^,
            close_channel/2,_
).
guard_types(functor(write_channel([basic('Any')],[basic('Vector')],
                                  [basic('Vector')]),
                    write_channel/4)^,
            write_channel/4,_
).
guard_types(functor(write_channel([basic('Any')],[basic('Vector')]),
                    write_channel/3)^,
            write_channel/3,_
).
guard_types(functor(make_channel([basic('Vector')],List),
                    make_channel/3)^,
            make_channel/3,_
) :-
    true : List = [list([basic('Any')],List),constant([])] | true.
guard_types(functor(close_context([basic('Vector')]),close_context/2)^,
            close_context/2,_
).
guard_types(functor(freeze([basic('Any')],[basic('String')],List),freeze/4)^,
            freeze/4,_
) :-
    true : List = [list([basic('Any')],List),constant([])] | true.
guard_types(functor(freeze([basic('Any')],Limits,[basic('String')],List),
			   freeze/5)^,
            freeze/5,_
) :-
    true : List = [list([basic('Any')],List),constant([])],
	Limits = [constant([]),basic('Tuple')] | true.
guard_types(functor(freeze([basic('Any')],Limit,Limit,Limit,
			   [basic('String')],List),
                    freeze/7)^,
            freeze/7,_
):-
    true :
      Limit = [basic('Integer'),constant([])],
      List = [list([basic('Any')],List),constant([])] | true.
guard_types(functor(melt([basic('Any')],[basic('Any')],List),melt/4)^,
            melt/4,_
) :-
    true : List = [list([basic('Any')],List),constant([])] | true.
guard_types(functor(melt([basic('Any')],[basic('Any')],List),melt/4)^,
            melt/4,_
) :-
    true : List = [list([basic('Any')],List),constant([])] | true.
guard_types(functor(execute([basic('Integer')],[basic('Tuple')]),execute/3)^,
            execute/3,_
).
guard_types(functor(activate([basic('String')],[basic('Any')],[basic('Any')]),
                    activate/4)^,
            activate/4,_
).
guard_types(functor(request([basic('Tuple')],[constant([])]),request/3)^,
            request/3,_
).
guard_types(functor(info([basic('Integer')],[basic('Integer')]),info/3)^,
            info/3,_
).
guard_types(functor(link(NameList,[basic('Integer')]),link/3)^,link/3,_) :-
    true : NameList = [list([basic('String')],[constant([]),NameList])]
	| true.
guard_types(functor(code_info([basic('Vector')],[basic('Tuple')]),
			code_info/3)^,
            code_info/3,_
).
guard_types(functor(make_module([basic('String')],List,[basic('String')]),
                    make_module/4)^,
            make_module/4,_
) :-
    true : List = [list([basic('String')],List),constant([])] | true.
guard_types(functor(make_procedure([basic('Tuple')],List,List,
				   [basic('String')]),
		    make_procedure/5)^,
            make_procedure/5,_
) :-
    true : List = [list([basic('Integer')],List),constant([])] | true.

guard_types(functor(ttyput_byte([basic('Integer')]),ttyput_byte/2)^,
            ttyput_byte/2,_
).
guard_types(functor(ttyput_integer([basic('Integer')]),ttyput_integer/2)^,
            ttyput_integer/2,_
).
guard_types(functor(ttyput_string([basic('String')]),ttyput_string/2)^,
            ttyput_string/2,_
).
guard_types(functor(machine_output(List),machine_output/2)^,
            machine_output/2,_
) :-
    true : List = [list([basic('Any')],List),constant([])] | true.
guard_types(functor(exceptions(List),exceptions/2)^,exceptions/2,_) :-
    true : List = [list([basic('Any')],List),constant([])] | true.
guard_types(functor(priority([basic('String'),constant([])],
			     [basic('String'),constant([])]),
		    priority/3)^,
	    priority/3,_).
guard_types(functor({fail},fail/1)^,fail/1,_).
guard_types(functor({deschedule}^,deschedule/1),deschedule/1,_).
guard_types(functor(make_shared_vars([basic('String')],List,List),
		    make_shared_vars/4)^,
	    make_shared_vars/4,_
) :-
    true : List = [list([basic('Any')],List),constant([])] | true.
guard_types(functor(get_from_shared_var([basic('String')],[basic('Any')],
					[basic('Any')],[basic('Any')]),
		    get_from_shared_var/5)^,
	    get_from_shared_var/5,_
).
guard_types(functor(send_to_shared_var([basic('Any')],[basic('Any')],
					[basic('Any')]),
		    send_to_shared_var/4)^,
	    send_to_shared_var/4,_
).
guard_types(functor(eventual_unify([basic('Any')],[basic('Any')]),
		    eventual_unify/3)^,
	    eventual_unify/3,_
).
guard_types(functor(update_logical_time([basic('Integer')],[basic('Integer')]),
		    update_logical_time/3)^,
	    update_logical_time/3,_).

/*
** Primitive Procedures
*/

guard_types(functor(fail([basic('Any')])^,fail/2),fail/2,_).
guard_types(functor(fail([basic('Any')],[basic('Any')]),fail/3)^,fail/3,_).
guard_types(functor(unify_without_failure([basic('Any')],[basic('Any')]),
		    unify_without_failure/3)^,
	    unify_without_failure/3,_
).
guard_types(functor(Atom,Name/Arity)^,Name/Arity,Path):- 
   otherwise :
      make_tuple(Arity,Atom) |
	      put_Any_in_arguments(Atom,2,Arity),
	      message(Atom,Name/Arity,Path).

expression_type(Expression):-
    true : Expression =	[basic('Integer'), basic('Real'),
			 functor(+ Expression, '+'/2),
			 functor(- Expression, '-'/2),
			 functor(abs(Expression), abs/2),
			 functor(real(Expression), real/2),
			 functor(round(Expression), round/2),
			 functor(integer(Expression), integer/2),
			 functor(arity([basic('Tuple'),List,basic('Vector')]),
				 arity/2),
			 functor(string_hash([basic('String')]),
				 string_hash/2),
			 functor(string_length([basic('String')]),
			 	 string_length/2),
			 functor(ascii([basic('String')]), ascii/2),
			 functor(~Expression, '~'/2),
			 functor(bitwise_not(Expression), bitwise_not/2),
			 functor(Expression + Expression, '+'/3),
			 functor(Expression - Expression, '-'/3),
			 functor(Expression * Expression, '*'/3),
			 functor(Expression / Expression, '/'/3),
			 functor(Expression \ Expression, '\'/3),
			 functor(Expression /\ Expression, '/\'/3),
			 functor(Expression \/ Expression, '\/'/3),
			 functor(Expression div Expression, div/3),
			 functor(Expression mod Expression, mod/3),
			 functor(max(Expression, Expression), max/3),
			 functor(min(Expression, Expression), min/3),
			 functor(bitwise_or(Expression, Expression),
				 bitwise_or/3),
			 functor(bitwise_and(Expression, Expression),
				 bitwise_and/3),
			 functor(sin(Expression), sin/2),
			 functor(cos(Expression), cos/2),
			 functor(tan(Expression), tan/2),
			 functor(sqrt(Expression), sqrt/2),
			 functor(asin(Expression), asin/2),
			 functor(acos(Expression), acos/2),
			 functor(atan(Expression), atan/2),
			 functor(log(Expression), log/2),
			 functor(exp(Expression), exp/2),
			 constant(random)
			],
      List = [list([basic('Any')],List),constant([])] | true.

check_proc(add,Name,[State?]^):-
	guard_types(State,Name?,'').
check_proc(found,_,_).

procedure put_Any_in_arguments(Tuple,Integer,Integer).
%*****************************************************
put_Any_in_arguments(Atom,N,Arity):-
	N =< Arity :
	N1 := N + 1,
	arg(N,Atom,[basic('Any')]) |
        put_Any_in_arguments(Atom,N1,Arity).

put_Any_in_arguments(_,N,Arity):-
	N > Arity |
	true.

procedure message(Tuple,String/Integer,String).
%**********************************************

message(Atom,Name/Arity,Path):-
	true :
	N := Arity - 1,
	arg(1,Atom,Name)|
	screen#display(['UNDECLARED PROCEDURE ',Path,'#',Name,'/',N,'
'],
			[list,type(ground)]).


nfa_to_dfa(done,Tree,Tree',Done):- nfa_to_dfa#tree(Tree,Tree',done\Done).


Tree ::= {Key, Data,Tree, Tree} ; [].
Key, Data ::= Any.

procedure lookup_tree(Key,Data,Tree,Reply,done).
%***********************************************

lookup_tree(Key,Data,Tree,Reply,Ready):-
    Tree = {Key',_,Left,_},
    Key @< Key' |
	lookup_tree(Key,Data,Left,Reply,Ready).
lookup_tree(Key,Data,Tree,Reply,Ready):-
    Tree = {Key',_,_,Right},
    Key' @< Key |
	lookup_tree(Key,Data,Right,Reply,Ready).
lookup_tree(Key,Data^,Tree,found^,_):-
    Tree = {Key,Data,_,_} |
	true.
lookup_tree(Key,Data,Tree,Reply,Ready):-
    Ready = done :
      Tree = Tree'?,
      Reply = add |
	Tree' = {Key,Data,_,_}.
