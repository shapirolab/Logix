-language([compound, colon, typed]).
-mode(trust).
-export([extend_automaton/3, identify_module_name/4, get/6, id_to_string/2]).

True ::= true ; false.
New_r ::= {`'#', {Procedure, Path}, Tuple} ; true.
ProcTreeDFA ::= {Proc_tree, DFA}.
LimitedReply ::= find ; add.

procedure extend_automaton(Remote, Dict, ServiceId).
%***************************************************

extend_automaton({ModuleName, Names, Left, Right}, Dictionary, SID) :-
	computation_utils # path_id(SID # ModuleName, PID, Ok), 
	check_path(ModuleName, PID, Names, Dictionary, Ok), 
	extend_automaton(Left, Dictionary, SID), 
	extend_automaton(Right, Dictionary, SID).
extend_automaton([], _, _).

procedure check_path(String, ServiceId, DFA, Dict, Any).
%**************************************************************

 % checks the given remote path and, if existent, links to it.
 % otherwise, it adds nothing to the dictionary, and gives appropriate
 % error messages. 

check_path(Rel, SID, Names, Dictionary, true):-
	id_to_string(SID, AID), 
	automaton # lookup_tree(AID, ProcTreeDFA, Dictionary, _, R, done), 
	dict_type(R, SID, Names, ProcTreeDFA, Rel, Dictionary).

check_path(Rel, _, Names, _, _):- 
     otherwise |
%	screen # display(unrecognized_path(Rel), type(ground)), 
	error_fill_with_Any(Names, Rel).

procedure dict_type(LimitedReply, ServiceId, DFA, ProcTreeDFA, String,
			Dict
).
%****************************************************************************

 % after search for a type in the dict: if path is not found add path to
 % dict and look on for undeclared types; if found, attempt to look on
 % for types which were not found in this module.

dict_type(Reply, SID, Names, ProcTreeDFA, Rel, Dict):- 

    Reply = add |
	get_module_type(SID, Rel, Names, ProcTreeDFA, Dict);

    Reply = find,
    ProcTreeDFA = {_, DFA} |
	find_names(Names, DFA, Not_found, done\Done), 
	computation_utils # path_id(SID # super, NewID, Ok), 
	get(Rel, NewID, Ok, Not_found, Dict, Done).

procedure identify_module_name(Tuple, Dict, New_r, S_circuit).
%*************************************************************

 % called from pre. it gets a remote call and returns true or
 % {path, procs} # goal for the call, to avoid another search later.

identify_module_name(Atom, Dictionary, NewAtom, Ss):-
    true :
      Ss = SID\_SID1 |
	computation_utils # call_id_goal(SID # Atom, NewID, Goal, Ok), 
	check_path_and_goal(Atom, Dictionary, NewAtom, Ss, NewID, Goal, Ok).

procedure check_path_and_goal(Tuple, Dict, New_r, S_circuit, ServiceId,
				Any, Any
).
%********************************************************************

 % if the path is an existent one, a link is made, and the goal
 % itself is checked. if the path does not exist - true is returned, 
 % and appropriate error messages are given.

check_path_and_goal(Atom, Dictionary, NewAtom, Ss, NewID, Goal, Ok) :-

    Ok = true |
	 check_goal(Atom, Dictionary, NewAtom, Ss, NewID, Goal);

    Ok =\= true : Dictionary = _, NewID = _,
      NewAtom = true,
      Ss = SID\SID |
	  relative_path(Atom, Relative, Reply), 
	  check_atom_no(Reply, Relative, Goal).

procedure check_goal(Tuple, Dict, New_r, S_circuit, ServiceId, Tuple).
%*********************************************************************

 % check goal - a tuple . for a string there is no need to look up the
 % type. if both path and goal are ok - a search in the automaton is made

check_goal(Atom, Dictionary, NewAtom, Ss, NewID, Goal) :-
    tuple(Goal), 
    arg(1, Goal, Functor), 
    arity(Goal, Arity), 
    string(Functor), 
    Arity > 1, 
    Goal =\= `_, 
    Goal =\= ?_ :
      NewAtom = {State, Rel} # Goal,
      Ss = SID\SID1 |
	id_to_string(NewID, AID), 
	relative_path(Atom, Rel, _), 
	automaton # lookup_tree(AID, ProcTreeDFA, Dictionary, SID\SID2, R,
				done
			), 
	tree_proc(R, NewID, ProcTreeDFA, Rel, Dictionary), 
	look_for_atom(ProcTreeDFA?, Functor? / Arity?, State, Rel, SID2\SID1).

check_goal(_, _, true^, (SID\SID)^, _, _):- 
    otherwise | 
	true.

procedure tree_proc(LimitedReply, ServiceId, ProcTreeDFA, Path, Dict).
%*********************************************************************

 % path found - process DFA and procedure decs.

tree_proc(find, _, _, _, _).
tree_proc(add, SID, ProcTreeDFA, Rel, Dict):-
	get_module_type(SID, Rel, [], ProcTreeDFA, Dict).

procedure check_atom_no(True, Path, Tuple).
%******************************************

 % if the path is possible and the goal normal warn the user that the
 % procedure is not declared.
 
check_atom_no(true, Relative, Goal):-
    tuple(Goal), 
    arg(1, Goal, Functor), 
    arity(Goal, Arity), 
    string(Functor), 
    Arity > 1|
	super # message(Goal, Functor/Arity, Relative).
check_atom_no(_, _, _) :-
    otherwise |
	true.

procedure look_up_type([{Any, Any}], Any).
%*****************************************

 % finds frozen type or returns no

look_up_type(Attributes, Answer) :-

    Attributes ? Attribute,
    Attribute =\= {'_type', _} |
        look_up_type;

    Attributes = [{'_type', Answer^} | _] |
          true;

    Attributes = [] :
      Answer = no |
	true.

procedure get_module_type(ServiceId, String, DFA, ProcTreeDFA, Dict).
%***************************************************************************

get_module_type(SID, Rel, Names, ProcTreeDFA, Dict) :-
	SID # attributes(Attributes), 
	computation_utils # path_id(SID # super, NewID, Ok), 
	look_up_type(Attributes, Answer), 
	get_module_type1(Answer, SID, Names, ProcTreeDFA, Dict, Rest, Done), 
	get(Rel, NewID, Ok, Rest, Dict, Done).

procedure get_module_type1(Any, ServiceId, DFA, ProcTreeDFA, Dict,
				DFA, done
).
%*********************************************************************

get_module_type1(no, _, Names, {_, DFA}^, _, Names^, Done) :-
	find_names(Names, DFA, _, done\Done).
get_module_type1(Frozen_type, SID, Names, ProcTreeDFA, Dict, Not_local, Done):-
 
    melt(Frozen_type, Melted_type, _) :
      ProcTreeDFA = {ProcTree, DFA},
      Melted_type = {ProcTree, DFA-Not_local, Remote} |
	find_names(Names, DFA, Not_local, done\Done), 
	extend_automaton(Remote?, Dict, SID);

    otherwise : Dict = _,	% probably recompiled since last accessed
      ProcTreeDFA = {ProcTree, DFA} |
	freeze((ProcTree | DFA), [], FPD, _),
	SID # path(P), 
	computation # failed('_type'(P), changed(Frozen_type, FPD)), 
	find_names(Names, DFA, Not_local, done\Done).

procedure find_names(DFA, DFA, DFA, S_circuit).
%**********************************************
 % The first DFA contains the remote types to be found.  The second DFA
 % contains the given types in the remote module.  The third DFA contains
 % those not found, to iterate the search in the super.

find_names([], _, _, (D\D)^).
find_names(Names, DFA, Not_local, Dones):-

    Names = {Name, State, Left, Right}, 
    DFA =  {Name', _, DFA', _}, 
    Name @< Name' :
      Dones = D\D1, Dones' = D\D2,
      Names' = {Name, State, Left, []} |
        find_names,
	find_names(Right, DFA, Not_local, D2\D1);

    Names = {Name, State, Left, Right}, 
    DFA =  {Name', _, _, DFA'}, 
    Name' @< Name :
      Dones = D\D1, Dones' = D\D2,
      Names' = {Name, State, [], Right} |
	find_names,
	find_names(Left, DFA, Not_local, D2\D1);

    Names = {Name, State, Names', Right}, 
    DFA =  {Name, State', DFA', Right'} :
      Dones = D\D1, Dones' = D\D2,
      State = State'? |
        	find_names,
		find_names(Right, Right', Not_local, D2\D1);

    Names = {_, _, _, _} :
      DFA = NewDFA?,
      Dones = D\D1 |
	add_names(Names, DFA1, Not_local, {DFA1, D} \ {NewDFA, D1});

/* Temporary Kluge */

    DFA = [] |
	add_names(Names, _, Not_local, Dones).

procedure add_names(DFA, DFA, DFA, S_circuit).
%*********************************************

add_names([], _, _, (D\D)^).

add_names({Name, State, Left, Right}, {Name, State, L, R}^, Not_local,
		(D\D1)^
):-
	automaton # lookup_tree(Name?, State, Not_local, D\D2, _, done), 
	add_names(Left, L, Not_local, D2\D3), 
	add_names(Right, R, Not_local, D3\D1).


procedure get(String, ServiceId, Any, DFA, Dict, done).
%******************************************************

 % if types were missing this level, and next level is defined, look
 % for them . no next level - warn user if types were missing.
 % otherwise, just finish and do nothing.

get(Rel, SID, Ok, Names, Dict, Done):- 

    Done = done,
    Ok = true,
    Names = {_, _, _, _} |
	id_to_string(SID, PathString), 
	automaton # lookup_tree(PathString, ProcTreeDFA, Dict, _, R, done), 
	close_names(Names, R, Ready), 
	dict_type(Ready, SID, Names, ProcTreeDFA, Rel, Dict);

    Done = done,
    Ok =\= true : SID = _, Dict = _ |
	error_fill_with_Any(Names, Rel).
get(_, _, true, []^, _, done).

procedure close_names(DFA, Any, Any).
%************************************

close_names(Names, R1, R2) :-

    Names = {_, _, Names', Right} |
	close_names,
	close_names(Right, R2', R2);

    true :
      Names = [],
      R1 = R2 |
	true.

BasicTree ::= [] ; {Any, [basic('Any') | Nil], BasicTree, BasicTree}.

procedure error_fill_with_Any(BasicTree, String).
%************************************************

error_fill_with_Any([]^, _).
error_fill_with_Any({Name, [basic('Any')]^, Left, Right}, Path):- 
	error_fill_with_Any(Left, Path), 
	error_fill_with_Any(Right, Path), 
	path_to_list(Path?, PList, ['# ', Name]), 
	utils # append_strings(['UNDECLARED TYPE ' | PList], O), 
	screen # display(O, type(ground)).

procedure id_to_string(ServiceId, String).
%*****************************************

id_to_string(SID, String):-

    SID ? S |
	id_to_path_list(SID', [S], Strings), 
	utils # append_strings(Strings?, String);

    SID = [] :
      String = '[]' |
	true.

StringList ::= [String | [String]].

procedure id_to_path_list([String], StringList, StringList).
%***********************************************************

id_to_path_list(Ss, Part, Strings) :-

    Ss ? S :
      Part' = [S, '#' | Part] |
	id_to_path_list;

    Ss = [] :
      Part = Strings |
	true.

procedure path_to_list(Path, StringList, [String]).
%**************************************************

path_to_list(Path, Left, Right) :-

    Path = A # Path' |
	path_to_list(A, Left, ['#' | Left']), 
	path_to_list;

    string(Path) :
      Left = [Path | Right] |
	true.

procedure relative_path(Tuple, Path, Any).
%*****************************************

relative_path(Atom, Relative, Reply) :-

    Atom = First # Atom',
    string(First),
    Atom' = (_#_) :
      Relative = First # Relative' |
        relative_path;

    Atom = Last # Tail,
    string(Last),
    Tail =\= (_#_) :
      Relative = Last,
      Reply = true |
	true;

    otherwise : Atom = _,
      Relative = '?',
      Reply = false |
	true.

procedure look_for_atom(ProcTreeDFA, Procedure_name, Procedure, Path,
			S_circuit
).
%********************************************************************

look_for_atom({ProcTree, _}, Id, State, Module_name, Ss) :-
	look_for_atom1(Id, ProcTree, State, Module_name, Ss).

procedure look_for_atom1(Procedure_name, Proc_tree, Procedure, Path,
				S_circuit
).
%*******************************************************************

look_for_atom1(Id, Tree, Procedure, Module_name, Ss):-	

    Tree = {LId, _, Tree', _}, 
    Id @< LId |
	look_for_atom1;

    Tree = {RId, _, _, Tree'}, 
    RId @< Id |
	look_for_atom1;

    Tree = {Id, Procedure^, _, _} : Module_name = _,
      Ss = SID\SID |
	true;

    true :
      Tree = New_branch?,
      Ss = SID\SID,
      Procedure = [Proc?] |
	New_branch = {Id, Procedure, _, _}, 
	super # guard_types(Proc, Id, Module_name).
