/* the operator for lexical logic programs with types, written in Compound .
   names parts are separated by '-'
 */


/* This file defines the operator which transforms a typed lexical logic 
    program (disjunctive clauses, in fututre with implicit variables)
    to a flat program.
 */


-language(compound).
-mode(interrupt).
-export([flat_prog/3, build_dict/4, transform/5]).

transform(Attributes1, Clauses1, Attributes2, Clauses2, Output) :-
	Attributes2 = Attributes1,
	flat_prog(Clauses1, Clauses2, Output).

/* flat_prog: 
   flattening a nested program Nest to Flat. Errors in Err.
   internal call.
 */

flat_prog(Nest, Flat, Err):-
    flat_pr(Nest, Flat, Subs, Subs, [], Err, [], 1).



/* flat_pr: flatenning clauses in Nest to list of parents Parents-TParents
            and flattened sub-clauses FSubs-TSubs. Errors in Err-Err_t,
            each clasue is given an index beginning with Index.
 */


flat_pr(Nest, Parents, TParents, FSubs, TSubs, Err, Err_t, Index):-

    Nest ? (Head :- Bodies <- Subs),
    Bodies =\= [], Bodies =\= [_|_],
    Index' := Index + 1 |
        make_path(Head, Index, Path),
        rename_clause_vars((Head:-Bodies), Path, Parent, RVars, Err, Err'),
        flat_proc(Parent?, Subs?, Path?, RVars, _,
                                         FParent, FSubs, FSubs', Err', Err''),
        Parents ! FParent,
        flat_pr;

    Nest = [] : Index = _,
      Parents = TParents,
      FSubs = TSubs,
      Err = Err_t ;

    Nest ? (Head:-Bodies),
    Bodies =\= (_ <- _),
    Bodies =\= [_|_], Bodies =\= [],
    Index' := Index + 1 :
      Parents ! (Head :- Bodies')|
        rm_locals(Bodies, Bodies'),
        flat_pr;

    Nest ? Head,
    Head =\= (_ <- _),
    Head =\= (_ :- _),
    Head =\= (procedure _),
    Head =\= (_ ::= _),
    Index' := Index + 1 :
      Parents ! Head|
        flat_pr;

    Nest ? (procedure Definition):
      Parents ! (procedure Definition)|
      flat_pr;

    Nest ? (Type ::= Definition):
      Parents ! (Type ::= Definition)|
      flat_pr;

    otherwise,
    Nest ? Err_clause:
      Err ! (illegal_clause - Err_clause)|
        flat_pr.

	



/* make_path:
   creates a prefix Path  for the clauses which are nested in a clause 
   with index Index and head Head.
 */


make_path(Head, Index, Path):-

    convert_to_string(Index, IString)|
        proc_name(Head?, Proc, _),
        utils#append_strings(['_lex-', Proc, '-', IString], Path).




/* proc_name(Head, Proc, Arity): the name of the procedure whose clause
   has the head Head is Proc and it defines a predicate with arity Arity.
 */

proc_name(Head, Proc, Arity):-

    string(Head):
      Arity = 0,
      Head = Proc;

    arg(1, Head, A1),
    Args := arity(Head) - 1 :
      Proc = A1,
      Arity = Args .



flat_proc(Parent, Subs, Path, Var_list, Dictionary,
                          FParent, Flat_code, Tail, Errors, Err_tail):-

      build_vtree(Var_list, VTree, Done1),
      flat_subs(Subs, Path, Flats, FSubs, FSubs, [], Dict_subs, 
                                        Types, Errors, Err_tail),
      call_combine(Done1, Parent, Flats, Path, VTree, 
                   Dict_subs?, Dictionary, Types?, FParent, 
                   Flat_code, Tail).






call_combine(done,  Parent, Flat_subs, Path, VTree, Dict_subs, Dictionary,
                               Dict_types, FParent, Flat_code, Tail):-
      combine(Parent, Flat_subs, Path, VTree, Dict_subs, Dictionary,
                               Dict_types, FParent, Flat_code, Tail).


/* build_vtree(Var_list, Tree, Done):
   builds an ordered binary tree Tree which contains the elements
   in Var_list.
   Done is instantiated to done when the building is over and Tree has
    its finite value.
 */

build_vtree(Var_list, Tree, Done)+(Tmp = _, L=done, R=_):-

    Var_list ? Var|
      insert(Tmp, Var, Tmp', L, M),
      build_vtree(Var_list', Tree, Done, Tmp'?, M, R);

    Var_list = []:
      (R, Tree) = (L,Tmp)|
        Done = R.


insert(Tree, Element, New_tree, L, R):-

    Tree = {Var, Tree', Right},
    Element @< Var:
      New_tree = {Var, New_tree'?, Right}|
        insert;

    Tree = {Var, Left, Tree'},
    Var @< Element:
      New_tree = {Var, Left, New_tree'?}|
        insert;

    true:
      Tree = {Element?, _, _}|
        (New_tree, L) = (Tree, R).





/* closing Tree gives the result Closed. In addition, an open copy is produced
    in Open. A short-circuit is used to detect the termination of the 
    production of the open tree.
 */


close_tree(Tree, Open, Closed, L, R):-

    Tree = {Var, Left, Right}:
    Closed = {Var, LClose?, RClose?},
    Open = {Var, LOpen, ROpen}|
      close_tree(Left, LOpen,  LClose, L, M),
      close_tree(Right, ROpen, RClose, M, R);

    true: Tree = []| 
      Closed = [],
      (L, Open) = (R,_).






/* rename_clause: renames the procedure-name, arguments and local variables in 
   the clause Clause to get Rclause. The renamed arguments and 
   local variables are accumulated in the list RVars. 
   Variables and arguments are renamed by being conveted to a tuple
    VPath(Variable).
   Procedure names are renamed by adding PPath as a prefix.
 */

rename_clause(Clause, PPath, VPath, Rclause, RVars, Errors, Errors_tail):-

    Clause = (Head:-Var_list ; Rest),
    list(Var_list):
      Rclause = (RHead? :- RVList? ; Rest)|
        rename_head(Head, PPath, VPath, RHead, RVars, Tail_Vars),
        rename_vars(Var_list, VPath, RVList, Tail_Vars, Errors, Errors_tail);


    Clause =\= (_ :- _)|
      Errors = Errors_tail,
      rename_head(Clause?, PPath, VPath, Rclause, RVars, []);

    otherwise,
    Clause = (Head:- Rest):
      Errors = Errors_tail,
      Rclause = (RHead:-Rest)|
        rename_head(Head?, PPath, VPath, RHead, RVars, []).





/* rename_clause_vars: renaming only the argumetns and local variables of
   Clause. (not the procedure name), using the same technique as rename_clause.
 */

rename_clause_vars(Clause, Path, Rclause, RVars, Errors, Errors_tail):-

    Clause = (Head:-Var_list; Rest),
    list(Var_list):
      Rclause = (RHead? :- RVList? ; Rest)|
        rename_head_vars(Head, Path, RHead, RVars, Tail_Vars),
        rename_vars(Var_list, Path, RVList, Tail_Vars, Errors, Errors_tail);

    Clause =\= (_ :- _)|
      Errors = Errors_tail,
      rename_head_vars(Clause?, Path, Rclause, RVars, []);

    otherwise,
    Clause = (Head:- Rest):
      Errors = Errors_tail,
      Rclause = (RHead:-Rest)|
        rename_head_vars(Head?, Path, RHead, RVars, []).




/* concat_proc_path: concatenates Prefix and Old_path to the path Path of the form:
    _lexPrefix-Old_path to denote a procedure-name.
 */

concat_proc_path(Prefix, Old_path, Path):-
    utils#append_strings([Prefix, '-', Old_path], Path).
      
    





/* rename_head: renaming the procedure name in Head and all its
    variables by adding a prefix PPath to the procedure name, and a prefix
    Vpath to the variables. RHead is the renamed head, Rvars is a list of
    the renamed variables with tail Tail_vars.
 */

rename_head(Head, PPath, VPath, RHead, RVars, Tail_Vars):-

    string(Head) : VPath = _,
      RVars = Tail_Vars|
        concat_proc_path(PPath, Head, RHead);


    arg(1, Head, Proc_name),
    Arity := arity(Head),
    make_tuple(Arity, RH),
    arg(1, RH, RP) :
      RHead = RH,
      RP = RProc? |
        concat_proc_path(PPath, Proc_name, RProc),
        rename_args(Head, Arity, VPath, RHead, RVars, Tail_Vars).






/* rename_head_vars: renaming only the variables in Head
     by adding  a prefix Vpath to the variables. 
     RHead is the renamed head, Rvars is a list of
    the renamed variables with tail Tail_vars.
 */

rename_head_vars(Head, VPath, RHead, RVars, Tail_Vars):-

    string(Head) : VPath = _,
      RVars = Tail_Vars,
      RHead = Head;

    arg(1, Head, Proc_name),
    Arity := arity(Head),
    make_tuple(Arity, RH),
    arg(1, RH, Proc_name) :
      RHead = RH |
        rename_args(Head, Arity, VPath, RHead, RVars, Tail_Vars).




rename_args(Head, Index, VPath, RHead, Args, Tail_Args):-

    Index > 1,
    arg(Index, Head, Arg)|
      rename_head_arg(Arg, VPath, RArg, Args, Args'),
      arg(Index, RHead, RArg?),
      Index' := Index - 1,
      rename_args;

    Index = 1 : Head = _, RHead = _, VPath = _,
      Args = Tail_Args.





rename_head_arg(Arg, VPath, RArg, Args, Tail_Args):-

    Arg = `Var,
    Var =\= '_'|
        rename_var(VPath, Var, RVar),
        RArg = `(RVar?),
        Args = [[RVar?, arg]|Tail_Args];

    Arg = ?Var,
    Var =\= '_' |
        rename_var(VPath, Var, RVar),
        RArg = ?(RVar?),
        Args = [[RVar?, arg]|Tail_Args];

    Arg ? A :
      RArg ! RA? |
	rename_head_arg(A, VPath, RA, Args, Args'),
	rename_head_arg;

    Arg = [] : VPath = _,
      RArg = [],
      Args = Tail_Args;

    Arg = (A, Arg') :
      RArg = (RA?, RArg') |
	rename_head_arg(A, VPath, RA, Args, Args'),
	rename_head_arg;

    tuple(Arg),
    otherwise,
    arg(1, Arg, A),
    Arity := arity(Arg),
    make_tuple(Arity, RT),
    arg(1, RT, R) :
      RArg = RT,
      R = RA? |
	rename_head_arg(A, VPath, RA, Args, Args'),
	rename_args(Arg, Arity, VPath, RArg, Args', Tail_Args);

    otherwise : VPath = _,
      Arg = RArg,
      Args = Tail_Args .






/* rename_var: Renaming thee name Var to RVar by: RVar = Prefix(Var).
 */

rename_var(Prefix, Var, RVar):-
      utils#append_strings([Prefix, '-', Var], RVar).




/* rename_vars: renaming the variables-list Vars to RVars by adding
   the prefix VPath to each variable name. Accumulating the variables
   names in RNames.
 */


rename_vars(Vars, VPath, RVars, RNames, Errors, Errors_tail):-

    Vars ? `Var,
    Var =\= '_' :
      RVars ! `(RVar?),
      RNames ! [RVar?,local] |
	rename_var(VPath, Var, RVar),
	rename_vars;

    Vars = [?Var| Vars'],
    Var =\= '_' :
      RVars ! ?(RVar?),
      RNames ! [RVar?, local] |
	rename_var(VPath, Var, RVar),
	rename_vars;

    Vars = [] : VPath = _,
      RVars = [],
      RNames = [], 
      Errors = Errors_tail;
      
    otherwise,
    Vars ? Var :
      Errors ! (illegal_local_variable - Var) |
	rename_vars.













/* flat_subs: Parents-TParents is a list which contains all the parent-clauses
   of Subs, and FSubs-TFSubs contains the nested  clauses of Subs
   flattened. Dict_subs contains all the (new) names of all the procedures 
   in Subs, and their renamed variables. 
   Initial variable for Dict is dummy.
 */

flat_subs(Subs, Path, Parents, TParents, FSubs, TFSubs, Dict_subs, 
          Dict_types, Err, Err_t)+
          (Index=1, Dict={('_lex_-', 0, _,_),_,_}, ITypes = {'_lex_-', _,_}):-


    arg(Index, Subs, (Head :- Bodies <- SSubs)),
    Bodies =\= [_|_], Bodies =\= [],
    Index' := Index + 1 :
      Parents ! FParent |
        rename_clause((Head:-Bodies), Path, Path_sub, RParent, RVars, Err, Err'),
        get_new_path(Path, Head, Index, Path_sub), 
        flat_proc(RParent, SSubs, Path_sub, RVars, Dictionary, 
                               FParent, FSubs, FSubs', Err', Err''),
        merge_d(Dictionary?, Dict, Dict', Err'', Err'''), 
        flat_subs(Subs, Path, Parents', TParents, FSubs', TFSubs, Dict_subs, 
                     Dict_types, Err''', Err_t, Index', Dict'?, ITypes);

    arg(Index, Subs, Clause),
    Clause =\= (_ :- _ <- _),
    Clause =\= (_ <- _),
    Clause =\= (_::=_), Clause =\= (procedure _),
    Clause =\= (_ :- []), Clause =\= (_ :- [_|_]),
    Index' := Index + 1 :
      Parents ! (Path_sub, RClause) |
        rename_clause(Clause, Path, Path_sub, RClause, RVars, Err, Err'),
        get_new_path(Path, Head, Index, Path_sub),
        head(Clause, Head),
        insert_dict(RClause, RVars, Dict, Dict', Err', Err''),
        flat_subs(Subs, Path, Parents', TParents, FSubs, TFSubs, Dict_subs, 
                     Dict_types, Err'', Err_t, Index', Dict'?, ITypes);


    arg(Index, Subs, (procedure Definition)),
    Index' := Index + 1 :
      Parents ! (procedure RDefinition?) |
        concat_proc_path(Path, Proc?, RProc),
        proc_name(Definition, Proc, Arity),
        replace_proc_name(Definition, Arity, RProc, RDefinition),
        flat_subs;

    arg(Index, Subs, (`Type ::= Definition)),
    Index' := Index + 1 :
      Parents ! (`RType ::= Definition) |
        rename_var(Path, Type, RType),
        insert(ITypes, RType?, ITypes', _, _),
        flat_subs(Subs, Path, Parents', TParents, FSubs, TFSubs, Dict_subs, 
                     Dict_types, Err, Err_t, Index', Dict, ITypes'?);

    arg(Index, Subs, Clause),
    otherwise,
    Index' := Index + 1 :
      Err ! (illegal_clause - Clause) |
        flat_subs;

    otherwise : Index = _, Path = _, Subs = _,
      Parents = TParents,
      FSubs = TFSubs,
      Err = Err_t,
      Dict =  Dict_subs,
      Dict_types = ITypes.





/* repalce_proc_name: replacing the first argument of Definition with
     Arity arguments from Proc to RProc, resulting in RDefinition.
 */

replace_proc_name(Definition, Arity, RProc, RDefinition):-

    string(Definition) : Arity = _,
      RDefinition = RProc;

    tuple(Definition),
    Ar := Arity + 1,
    make_tuple(Ar, RD),
    arg(1, RD, RProc) :
      RDefinition = RD |
        copy_args(Definition, Ar, RDefinition).



/* copy_args: copies the arguments of Tuple to RTuple from position Index 
       downwards.
 */

copy_args(Tuple, Index,  RTuple):-

    Index > 1,
    arg(Index, Tuple, A),
    arg(Index, RTuple, B),
    Index' := Index - 1 :
      B = A |
	copy_args;

    Index = 1 : Tuple = _, RTuple = _ .




Dictionary ::= {(Proc_name, VTree, Add_tree), Dictionary, Dictionary}.
Proc_name ::= String.
Add_tree ::= VTree.

/* insert_dict: inserts to dictionary Dict the variables Vars of Clause.
   the result it New_dict.
    redefined procedure is informed n Err with tail Err_t
 */

insert_dict(Clause, Vars, Dict, New_dict, Err, Err_t):-
    head(Clause, Head),
    proc_name(Head, Proc_name, Arity),
    insert_to_d(Proc_name?, Arity?, Vars,  Dict, New_dict, Err, Err_t).





/* insert_to_d: inserts to dictionary Dict under the entry Proc
     the variables Vars in the variables tree, resulting in New_dict.
     redefintion errors in Err with tail Err_t.
     Arity is the arity of the inserted procedure.
 */

insert_to_d(Proc, Arity, Vars, Dict, New_dict, Err, Err_t):-

    Dict = {(Proc, Ar, VTree, Add), Left, Right} :
      New_dict = {Entry?, Left, Right} |
	assign(Done, Entry, (Proc, Ar, NVTree, Add)),
	check_redefined(Proc, Ar, Arity, Err, Err_t),
	build_vtree(Vars, NVTree, Done, VTree, done, _RDone);

    Dict = {(P, Ar, V, A), Dict', Right},
    Proc @< P :
      New_dict = {(P, Ar, V, A), New_dict'?, Right} |
	insert_to_d;
      
    Dict = {(P, Ar, V, A), Left, Dict'},
    P @< Proc :
      New_dict = {(P, Ar, V, A), Left, New_dict'?} |
	insert_to_d;

    true :
      Dict=New_dict?,
      Err = Err_t |
	assign(Done, New_dict, {(Proc, Arity, VTree, _), _, _}),  
	build_vtree(Vars, VTree, Done).




/* insert_d(Entry, Dictionary, Dict, Err, Err_t): inserts Entry 
    (whose add-lists are unistantiated)
    to the completely open dictionary Dictionary to get Dict.
    informs about redefinitions errors in Err with tail Err_t
 */

insert_d(Entry, Dictionary, Dict, Err, Err_t):-

    Entry = (String1,_, _,_),
    Dictionary = {(String2, Ar, T2, A2), Dictionary', Right},
    String1 @< String2:
      Dict = {(String2, Ar, T2, A2), Dict'?, Right}|
        insert_d;


    Entry = (String1, _,_,_),
    Dictionary = {(String2, Ar2, T2, A2), Left, Dictionary'},
    String2 @< String1:
      Dict = {(String2, Ar2, T2, A2), Left, Dict'?}|
        insert_d;

    Entry = (String, Ar1, T1, A1),
    Dictionary = {(String, Ar2, T2, A2), Left, Right} :
      A1 = A2,
      Dict = {E?, Left, Right} |
        check_redefined(String, Ar1, Ar2, Err, Err_t),
        merge_trees(T1, T2, Tree, Done),
        assign(Done, E, (String, Ar1, Tree, A2));

    true :
      Dictionary = {Entry?, _, _},
      Err = Err_t,
      Dict = Dictionary .





/* merge_d: merging the closed dictionary Dict1 with the open Dict2 to Dict.
 */

merge_d(Dict1, Dict2, Dict, Err, Err_tail):-
    inorder(Dict1, Dict_list),
    build_dict(Dict_list, Dict, Err, Err_tail, Dict2).





build_dict(Entries, Dict, Err, Err_t)+(Tmp = _):-

    Entries ? Entry|
      insert_d(Entry, Tmp, Tmp', Err, Err'),
      build_dict(Entries', Dict, Err', Err_t, Tmp'?);



    Entries = []:
      Err = Err_t,
      Tmp = Dict .




/* merge_trees: merges two different trees (with no common elements) Tree1 
    and Tree2 to Tree.
   Done gets the value done when the merging is over, and Tree has the
     right value.
 */

merge_trees(Tree1, Tree2, Tree, Done):-
    close_tree(Tree1, _, CTree1,_,_),
    inorder(CTree1, List1),
    build_vtree(List1, Tree, Done, Tree2, done, _RDone).






head(Clause, Head):-

    Clause = (H :- _Body) :
      Head = H ;

    otherwise :
      Head = Clause .





/* get_new_path: changing Old_path to New_path which is: Old_path(Proc(Index
      where Proc is the procedure name in Head.
 */

get_new_path(Old_path, Head, Index, New_path):-
    convert_to_string(Index, SIndex)|
      proc_name(Head, Proc,_),
      utils#append_strings([Old_path, '-', Proc, '-', SIndex], New_path).





/* combine: combines the code of the parent_clause Parent with the flat code
     of its sub-procedures Flat_subs to New_parent and Flats-Tail in which 
     all the arguments and local variables of the parent-clause and its 
     sub-procedure are renamed.
     The variables of the parent-procedure in VTree are merged with
     Dict_subs which includes the variables of the sub-procedures to 
     Dictionary.
     Path is the path upto the parent-clause.
 */

combine(Parent, Flat_subs, Path, VTree, Dict_subs, 
                   Dictionary, Dict_types, New_parent, Flats, Tail):-

    close_tree(VTree, OVTree, CVTree, done, Done),
    close_tree(Dict_types, _, CTypes, _, _),
    close_dict(Dict_subs, CDict_subs),
    transform_clauses(Flat_subs, Path, CVTree, CDict_subs, CTypes, Flats, 
                      Tail, Call_list, Heads, Proc_types, Init_add_list),
    add_to_dict(CDict_subs, Init_add_list, Dict),
    serve_call(Call_list, Dict?, New_dict),
    complete_heads(Heads, New_dict),
    complete_procs(Proc_types, New_dict),
    transform_parent(Parent, Path, CVTree, New_dict, New_parent),
    call_merge_dict(Done, New_dict, OVTree, New_parent, Dictionary).

 

call_merge_dict(done, New_dict, OVTree, New_parent, Dictionary):-
    merge_dict(New_dict?, OVTree, New_parent, Dictionary).



/* close_dict: closes the uninstantiated variables in the entries of Open_dict
     and the variables trees in Open_dict. Returns an opent dicionary ODict
     and a close dictionary CDict.
 */


close_dict(Open_dict, CDict):-

    Open_dict = {(String, Arity, VTree, Add), Open_dict', Right}|
      CDict = {(String, Arity, CVTree?, Add), CDict'?, CRight?},
      close_dict,
      close_dict(Right, CRight),
      close_tree(VTree, _,  CVTree, _, _);

    true:
      Open_dict = []|
        CDict = [].
      

Predicate ::= Tuple; String.
Call_list ::= [call(Predicate, Predicate, Predicate)|Call_list]; [].

Heads ::= [head(Predicate, Predicate)|Heads]; [].

Init_add ::= [add(String, Variable)|Init_add]; [].
Variable::= `String; ?String.


transform_clauses(Clauses, Path, VTree, Dict_subs, Dict_types,
                Flat_code, Tail, Call_list, Heads, Proc_types, Init_add)+
                (Tail_call=[]):-

    Clauses ? Clause,
    Clause =\= (_ ::= _),
    Clause =\= (procedure _):
      Flat_code ! RClause,
      Heads ! Head,
      Init_add ! Add_vars|
        transform_clause(Clause,  Path, VTree, Dict_subs, RClause,
                      Add_vars, Head, Call_list, Call_list'),
        transform_clauses;

    Clauses ? (Type ::= Definition):
      Flat_code ! (Type ::= RDefinition)|
        rename_types(Definition, Path, Dict_types, RDefinition),
        transform_clauses;

    Clauses ? (procedure Definition):
      Flat_code ! (procedure RDefinition)|
        rename_proc_args(Definition, Path, Dict_types, Definition1),
        Proc_types ! (proc(Definition1, RDefinition)),
        transform_clauses;
        


    Clauses = [] : Path = _, VTree = _, Dict_subs = _, Dict_types = _,
      Init_add = [],
      Heads = [],
      Proc_types = [],
      Call_list = Tail_call,
      Flat_code = Tail.
   







/* transform_clause: renaming the variables which appear in the body
     of Clause and are either arguments and local variables of Clause
     or appear in VTree. RClause is Clause renamed. 
     the variables of VTree found in Clause are accumulated in the list
     Add_vars. All calls to procedure which appear in Dict are accumulated
     in Call_list with tail Tail_call.
     Head is of the form head(Head1, Head2) where Head1 is the head of
     Clause, Head2 is the head of RClause.
     Path is the path for the variables of the parent-procedure of Clause.
     CPath is the path of Clause, no-local if there are no local variables
       and arguments of Clause.
 */

transform_clause(Clause, Path, VTree, Dict, RClause,
                      Add_vars, Head, Call_list, Tail_call):-

    Clause = (CPath, (H :- RHS)):
      Head = head(H, RHead),
      Add_vars = add(Proc, Vars),
      RClause = (RHead :- RHS1)|
        find_entry(Dict, Proc?, CTree),
        proc_name(H, Proc,_),
        transform_rhs(RHS, Path, CPath, VTree, CTree, Dict, RHS1, Vars, 
                      Call_list, Tail_call);
		% CTree should contain also arguments.

    Clause = (_CPath, H),
    H =\= (_ :- _) : Path = _, Dict = _, VTree = _,
      Head = head(H, RClause),
      Call_list = Tail_call,
      Add_vars = add(Proc, [])|
        proc_name(H, Proc,_);

    Clause = (H :- RHS):
      Head = head(H, RHead),
      Add_vars = add(Proc, Vars),
      RClause = (RHead :- RHS1)|
        find_entry(Dict, Proc?, CTree),
                  % finds the tree togather with the procedure name
        proc_name(H, Proc,_),
        transform_rhs(RHS, Path, renamed, VTree, CTree, Dict, RHS1, Vars, 
                      Call_list, Tail_call);

    otherwise : Dict = _, Path = _, VTree = _,
      Head = head(Clause, RClause),
      Call_list = Tail_call,
      Add_vars = add(Proc, [])|
        proc_name(Clause, Proc, _). 








/* find_entry: returns in Entry the entry in Dict which corresponds
      to the procedure-name Proc.
      the required entry must appear in Dict.
 */

find_entry(Dict, Proc, Entry):-

    Dict = {(Proc, Ar, V, A), _, _} :
      Entry = (Proc, Ar, V, A) ;

    Dict = {(P, _, _, _), Dict', _},
    Proc @< P|
      find_entry;

    Dict = {(P, _, _, _), _, Dict'},
    P @< Proc |
      find_entry;
	
    Dict = [] : Proc = _,
      Entry = [].






/* transform_rhs: changing RHS of a clause to RHS1 by renaming the local variables
    and arguments (in Loc_tree) of the clause to include the full pathname,
    renaming also variables of RHS which appear in VTree and accumulating 
    them in the list Vars.
    For each call to a procedure in the dictionary Dict, a request
    call(_,_,_) is added to Call_list with tail Tail_call.
    Path is the path upto the parent-procedure of the clause.
    The list of local variables of each clause is omitted.
 */

transform_rhs(RHS, Path, CPath, VTree, Loc_tree, Dict, RHS1, Vars, 
                         Call_list, Tail_call)+(Tail_vars=[]):-
    rm_locals(RHS, RHS'),
    transform_rest_rhs(RHS', Path, CPath, VTree, Loc_tree, Dict, RHS1, Vars, 
                         Call_list, Tail_call, Tail_vars).


transform_rest_rhs(RHS, Path, CPath, VTree, Loc_tree, Dict, RHS1, Vars, 
                         Call_list, Tail_call, Tail_vars):-

   RHS = (Guard|Body):
      RHS1 = (Guard1| Body1)|
        transform_guard(Guard, Path, CPath, VTree, Loc_tree, Guard1, Vars,
                        Tvars),
        transform_body(Body, Path, CPath, VTree, Loc_tree, Dict, 
                       Body1, Tvars, Tail_vars, Call_list, Tail_call);

   RHS = (_Ask : _Tell) : Dict = _,
     Call_list = Tail_call |
        transform_guard(RHS, Path, CPath, VTree, Loc_tree, RHS1, Vars, 
                                                            Tail_vars);

    RHS =\= (_ ; _),
    RHS =\= (_ | _),
    RHS =\= (_ : _) |
      transform_body(RHS,  Path, CPath, VTree, Loc_tree, Dict, RHS1, 
                     Vars, Tail_vars, Call_list, Tail_call);

    RHS = (RHS' ; RHSs):
      RHS1 = (RHS1' ; RHS1s)|
      transform_rest_rhs(RHSs, Path, CPath, VTree, Loc_tree, Dict, RHS1s, 
                    Vars, Call_list, Call_list', Vars'),
      transform_rest_rhs.




/* transform_guard: transforms Guard to Guard1, by adding CPath to
     local variables and arguments (in Loc_tree), and PPath to global variables
     of the parent (stored in VTree). Accumulate in Vars with tail Tail_vars
     the variables of the parent in Guard.
 */

transform_guard(Guard, PPath, CPath, VTree, Loc_tree, Guard1, Vars, 
                                                           Tail_vars):-


    Guard = (Ask : Guard'):
      Guard1 = (Ask1 : Guard1')|
        transform_guard(Ask, PPath, CPath, VTree, Loc_tree, Ask1, Vars, Vars'),
        transform_guard;

    Guard = (G, Guard'):
      Guard1 = (G1, Guard1')|
        rename_guard_atom(G, PPath, CPath, VTree, Loc_tree, G1, Vars, Vars'),
        transform_guard;

    otherwise|
      rename_guard_atom(Guard, PPath, CPath, VTree, Loc_tree, Guard1, Vars, Tail_vars).








/* transform_body: renaming local variables and arguments of Body 
           (in Loc_tree) and variables of the parent (in VTree, accumulated
           in Vars with tail Tail_vars) to Body1. 
           calls to procedures in Dict are updated using Call_list with
           Tail_call. 
           PPath: path upto the parent. CPath: path of the clause.
 */
      
transform_body(Body, PPath, CPath, VTree, Loc_tree, Dict, 
                         Body1, Vars, Tail_vars, Call_list, Tail_call):-

    Body = (B, Body'),
    B = (_,_):
      Body1 = (B1, Body1')|
	transform_body(B, PPath, CPath, VTree, Loc_tree, Dict,
			B1, Vars, Vars', Call_list, Call_list'),
	transform_body;

    Body = (B, Body'),
    B =\= (_,_):
      Body1 = (B1, Body1')|
        rename_body_atom(B, PPath, CPath, VTree, Loc_tree, Dict, 
                         B1, Vars, Vars', Call_list, Call_list'),
        transform_body;

    otherwise|
        rename_body_atom(Body, PPath, CPath, VTree, Loc_tree, Dict, 
                         Body1, Vars, Tail_vars, Call_list, Tail_call).









/* rename_guard_atom: renames Atom to Atom1 (of guard) by adding to each local 
     variable and argument (in  Loc_tree) a prefix CPath and to each 
     parent-variable (in VTree) PPath. accumulating the variables from 
     VTree in Vars\Tail_vars.
 */

rename_guard_atom(Atom, PPath, CPath, VTree, Loc_tree, Atom1, Vars, 
                                                         Tail_vars):-

    Loc_tree = (_, _, Tree, _),
    arg(1, Atom, Arg),
    Arity := arity(Atom),
    make_tuple(Arity, Atom'),
    arg(1, Atom', Arg) |
        rename_rhs_args(2, Arity, Atom, PPath, CPath, VTree, Tree, Atom', 
                        Atom1, Vars, Tail_vars);

    otherwise : PPath = _, CPath = _, VTree = _, Loc_tree = _,
      Atom = Atom1,
      Vars = Tail_vars.






/* rename_body_atom: renames Atom of a body to Atom1, by adding to each
    local variable (in Loc_tree) the prefix CPath, to each parent-variable
    (in VTree) PPath. Renaming sub-procedures (in Dict) by adding PPath. 
    Accumulating the parent-variables in Vars\Tail_vars. Accumulating calls 
    to sub-procedures in Call_list\Tail_call.
    
 */

rename_body_atom(Atom, PPath, CPath, VTree, Loc_tree, Dict,
                    Atom1, Vars, Tail_vars, Call_list, Tail_call):-

    Loc_tree = (Proc, _, Tree, _),
    Arity := arity(Atom),
    arg(1, Atom, Call_name),
    make_tuple(Arity, Atom') |
        rename_rhs_args(2, Arity, Atom, PPath, CPath, VTree, Tree, Atom',
                        Atom'', Vars, Tail_vars),
        get_call_name(Call_name, Proc, PPath, Dict, RCall),
        arg(1, Atom'', RCall, Atom'''),
        complete_atom(Proc, RCall, Atom''', Call_list, Tail_call, Atom1);

    Loc_tree = (Proc,_,_,_),
    string(Atom) : CPath = _, VTree = _,
      Tail_vars = Vars |
	get_call_name(Atom, Proc, PPath, Dict, RCall),
	complete_atom(Proc, RCall, RCall, Call_list, Tail_call, Atom1);

    otherwise : PPath = _,  CPath = _, VTree = _, Loc_tree = _, Dict = _,
      Tail_vars = Vars,
      Tail_call = Call_list,
      Atom1 = Atom .





arg(Index, Term, Arg, RTerm):-
    arg(Index, Term, A) :
      Arg = A,
      RTerm = Term.



get_call_name(Call_name, Self, PPath, Dict, RCName):-
    renamed_proc(Call_name, Reply),
    rename_call_name(Reply, Call_name, Self, PPath, Dict, RCName).




renamed_proc(Call_name, Reply):-

    string_to_dlist(Call_name, LC, []),
    string_to_dlist('_lex-', HLex, _),
    HLex = LC :
      Reply = renamed ;

    otherwise : Call_name = _,
      Reply = not_renamed .




  

rename_call_name(Renamed, Call_name, Self, PPath, Dict, RCName):-

    Renamed = renamed : Self = _, PPath = _, Dict = _,
      Call_name = RCName ;
    
    Renamed = not_renamed |
	concat_proc_path(PPath, Call_name, RCall),
	check_renamed(Call_name, RCall, Self, Dict, RCName).


check_renamed(Old_cname, RCall, Self, Dict, RCName):-

    Self = RCall : Old_cname = _, Dict = _,
      RCName = RCall ;

    Self =\= RCall |
      check_renamed_dict(Old_cname, RCall, Dict, RCName).


check_renamed_dict(Old_cname, Rcall, Dict, RCName):-
    find_in_dict(Dict, Rcall, Reply),
    found_in_dict(Reply, Old_cname, Rcall, RCName).




/* find_in_dict: returns in Reply found or not_found according to the
    case wheather there is or not an entry Proc_name in Dict.
 */

find_in_dict(Dict, Proc_name, Reply):-

    Dict = [] : Proc_name = _,
      Reply = not_found;

    Dict = {(Proc_name, _, _,_),_,_}:
      Reply = found;

    Dict = {(P,_,_,_), Dict', _},
    Proc_name @< P|
      find_in_dict;

    Dict = {(P,_,_,_), _, Dict'},
    P @< Proc_name|
      find_in_dict.
    


found_in_dict(found, _, RCall, RCName):-
    RCName = RCall.

found_in_dict(not_found, Old, _, RCName):-
    RCName = Old.




    
complete_atom(Self, Call_name, RCall, Call_list, Tail_call, Final_call):-
    renamed_proc(Call_name, Reply),
    finish_complete(Reply, Self, Call_name, RCall, Call_list, Tail_call, 
                                                               Final_call).
    




finish_complete(Reply, Caller, Call_name, Call, Call_list, Tail_call, 
                                                           Final_call):-

    Reply = renamed,
    Caller =\= Call_name:
      Call_list = [call(Caller, Call, Final_call)|Tail_call];

    Reply = renamed,
    Caller = Call_name,
    tuple(Call):
      Call_list = [self_call(Caller, Call, Final_call)|Tail_call];
 
    otherwise : Reply = _, Caller = _, Call_name = _,
      Call_list = Tail_call,
      Final_call = Call .
      













        



/* rename_rhs_args: renaming arguments of Atom to RAtom using Via,
       beginning from Index, ending with Arity.
       PPath is the prefix of variables in VTree which are accumulated in
       the list Vars with tail Tail_vars.
       CPath is the prefix of variables in Loc_tree which is the tree of the
       arguments and local variables of the predicate that Atom is a part
       of its definition.
 */

rename_rhs_args(Index, Arity, Atom, PPath, CPath, VTree, Loc_tree,
                        Via, RAtom, Vars, Tail_vars):-

    Index > Arity : Atom = _, PPath = _, CPath = _, VTree = _, Loc_tree = _,
      Via = RAtom,
      Vars = Tail_vars;

    Index =< Arity,
    arg(Index, Atom, Arg),
    arg(Index, Via, RArg),
    Index' := Index + 1 |
        rename_arg(Arg, PPath, CPath, VTree, Loc_tree, RArg, Vars, Vars'),
        rename_rhs_args.
      






rename_arg(Arg, PPath, CPath, VTree, Loc_tree, RArg, Vars, TVars):-

    Arg = `Name:
      RArg = `RName|
        find_name(Name, PPath, CPath, VTree, Loc_tree, RName, Vars, TVars);

    Arg = ?Name:
      RArg = ?RName|
        find_name(Name, PPath, CPath, VTree, Loc_tree, RName, Vars, TVars);

    Arg ? A1:
      RArg ! RA1|
        rename_arg(A1, PPath, CPath, VTree, Loc_tree, RA1, Vars, Vars'),
        rename_arg;

    Arg = [] : PPath = _, CPath = _, VTree = _, Loc_tree = _,
      RArg = [],
      Vars = TVars ;

    Arg = (A1, Arg') :
      RArg  = (RA1, RArg')|
        rename_arg(A1, PPath, CPath, VTree, Loc_tree, RA1, Vars, Vars'),
        rename_arg;

    tuple(Arg),
    otherwise,
    Arity := arity(Arg),
    make_tuple(Arity, Via) |
        rename_rhs_args(1, Arity, Arg, PPath, CPath, VTree, Loc_tree,
                        Via, RArg, Vars, TVars);

    otherwise : PPath = _, CPath = _, VTree = _, Loc_tree = _,
      Arg = RArg,
      Vars = TVars .




find_name(Name, PPath, CPath, VTree, Loc_tree, RName, Vars, TVars):-

    CPath =\= renamed |
      rename_var(CPath, Name, Local),
      find_loc_var(Local?, Loc_tree, Reply),
      found_local(Reply, PPath, Name, VTree, RName, Vars, TVars);

    CPath = renamed : Loc_tree = _ |
      found_local(non_found, PPath, Name, VTree, RName, Vars, TVars).






found_local(found(Var), _, _, _, RVar, Vars, TVars):-
      Vars = TVars,
      Var = RVar.

found_local(Found, PPath, Name, VTree, RName, Vars, TVars):-

    Found = non_found,
    string_length(Name) > 5,
    string_to_dlist('_lex-', H, _),
    string_to_dlist(Name, HN, []),
    H =\= HN |                      %    Name =\= '_lex-'_
      rename_var(PPath, Name, RVar),
      find_var(RVar, VTree, Reply),
      found_global(Reply, Name, RName, Vars, TVars);

    Found = non_found,
    string_length(Name) =< 5 |      %    Name =\= '_lex-'_
      rename_var(PPath, Name, RVar),
      find_var(RVar, VTree, Reply),
      found_global(Reply, Name, RName, Vars, TVars).

found_local(non_found, _, Name, _, RName, Vars, TVars):-
    string_to_dlist('_lex', H, _),
    string_to_dlist(Name, HN, []),
    H = HN :                        %    Name = '_lex-'_
      RName = Name,
      Vars = TVars .


found_global(found(Var), _, RVar, Vars, TVars):-
    true :
      Vars = [`RVar?|TVars],
      Var = RVar .

found_global(non_found, Name, RName, Vars, TVars):-
    true :
      Name = RName,
      Vars = TVars.






find_var(Name, VTree, Reply):-

    VTree = [] : Name = _,
      Reply = non_found;

    VTree = {[_,_], _,_} |
      find_prog_var(Name, VTree, Reply);

    otherwise |
      find_type(Name, VTree, Reply).





find_prog_var(Name, VTree, Reply):-

    VTree = {[Name,_], _,_}:
      Reply = found(Name);

    VTree = [] : Name = _,
      Reply = non_found;

    VTree = {[Var,_], VTree' ,_},
    Name @< Var|
      find_prog_var;

    VTree = {[Var,_], _, VTree' },
    Var @< Name|
      find_prog_var.





find_type(Name, VTree, Reply):-

    VTree = {Name, _,_}:
      Reply = found(Name);

    VTree = [] : Name = _,
      Reply = non_found;

    VTree = {Var, VTree' ,_},
    Name @< Var|
      find_type;

    VTree = {Var, _, VTree' },
    Var @< Name|
      find_type.


/* find_loc_var: finds a local variable whose name is Name in VTree.
   returns in Reply found(Name) or non_found.
   for arguments (variables whose entry in VTree is [V, arg]) the reply
   found(Name) is returned also if Name is more primed than the entry
   in VTree.
 */

find_loc_var(Name, VTree, Reply):-

    VTree = {[Name,_], _,_}:
      Reply = found(Name);

    VTree = [] : Name = _,
      Reply = non_found;

    VTree = {[Var,_], VTree' ,_},
    Name @< Var|
      find_loc_var;

    VTree = {[Var,arg], _, VTree' },
    Var @< Name|
      check_primed(Var, Name, VTree', Reply);

    VTree = {[Var,local], _, VTree' },
    Var @< Name|
      find_loc_var.




/* check_primed: if the difference between the original variables names
     in Var1 and Var2 is only
     primes, then returning Reply as found(Var2).
     otherwise: continue looking for Var2 in VTree and returning Reply. 
 */

check_primed(Var1, Var2, VTree, Reply):-

    string_to_dlist(Var1, H1, T1),
    string_to_dlist(Var2, H2, []),
    H1 = H2 |
	all_primes(T1, Res),
	check_all_primes(Res, Var2, VTree, Reply);

    otherwise : Var1 = _ |
	find_loc_var(Var2, VTree, Reply).





check_all_primes(true, Name, _, Reply):-
    Reply = found(Name).

check_all_primes(false, Name, VTree, Reply):-
    find_loc_var(Name, VTree, Reply).





all_primes(CharList, Result):-

    CharList ? Prime,
    Prime =:= ascii("'") |
	all_primes;

    CharList = [] :
      Result = true;

    otherwise : CharList = _,
      Result = false.







rename_proc_args(Procdef, Path, Dict_types, RDef):-
    arg(1, Procdef, Proc),
    arity(Procdef, Arity),
    make_tuple(Arity, Via),
    arg(1, Via, Proc)|
        rename_rhs_args(2, Arity, Procdef, Path, renamed, Dict_types, _,
                        Via, RDef, _,_);

    otherwise : Path = _, Dict_types = _,
      Procdef = RDef.




rename_types(Def_types, Path, Dict_types, RDef_types):-

    Def_types = (Definition ; Def_types'):
      RDef_types = (RDefinition ; RDef_types')|
        rename_arg(Definition, Path, renamed, Dict_types, _, RDefinition, _,_),
        rename_types;


    Def_types =\= (_ ; _)|
        rename_arg(Def_types, Path, renamed, Dict_types, _, RDef_types, _,_).




/* insert_add_list: inserts the variables in Addlist to the add-list
    field ot the procedure Proc in Dict, resultin in NDict. 
 */

insert_add_list(Addlist, Proc, Dict, NDict):-

    Dict = {(Proc, Arity, VTree, Add), Left, Right}|
      NDict = {Entry?, Left, Right},
      assign(Done, Entry, (Proc, Arity, VTree, NAdd)),
      build_vtree(Addlist, NAdd, Done, Add, done, _RDone);

    Dict = {(P, Ar, V, A), Dict', Right},
    Proc @< P|
      NDict = {(P, Ar, V, A), NDict'?, Right},
      insert_add_list;
      
    Dict = {(P, Ar, V, A), Left, Dict'},
    P @< Proc|
      NDict = {(P, Ar, V, A), Left, NDict'?},
      insert_add_list.





/* add_to_dict: adds the addings in Init_add_list to the adding-lists
    in dictionary Dict. the result is ADict.
 */

add_to_dict(Dict, Init_add_list, ADict):-

    Init_add_list ? add(Proc, Vars)|
      insert_add_list(Vars, Proc, Dict, Dict'),
      add_to_dict(Dict'?, Init_add_list', ADict);

    Init_add_list = []:
      Dict = ADict.










serve_call(Call_list, Dict, New_dict):-
    close_add_lists(Dict, ODict, CDict),
    acc_additions(Call_list, CDict, Additions),
    complete_serve(Additions, Call_list, ODict?, CDict?, New_dict).





/* close_add_lists: closes the add-list tree in all the entries of Dict,
     returning a closed dictionary CDict and a new open dictionary ODict.
 */

close_add_lists(Dict, ODict, CDict):-

    Dict = {(P, Ar, V, A), Left, Right}|
      close_tree(A, OA, CA, done, Done),
      CDict = {(P, Ar, V, CA?), CLeft?, CRight?},
      assign(Done, Entry, (P, Ar, V, OA)),
      ODict = {Entry?, OLeft?, ORight?},
      close_add_lists(Left, OLeft, CLeft),
      close_add_lists(Right, ORight, CRight);

    Dict = []|
      CDict = [],
      ODict =[].




      

/* acc_aditions: accumulates in Additions all the required additions of global
    variables because of calls in Calls, using the add-lists in Dict.
 */

acc_additions(Calls, Dict, Additions):-

    Calls ? call(Caller, Call, _RCall) |
      proc_name(Call, Called,_),
      proc_name(Caller, Caller_name,_),
      global_contained(Called?, Caller_name?, Dict, Add_list),
      add_to_list(Add_list, Caller_name, Additions, Additions'),
      acc_additions;

    Calls ? self_call(_Caller, _Call, _RCall) |
      acc_additions;

    Calls = [] : Dict = _,
      Additions = [].





/* global_contained: accumulating in Add_list the variables which have to
   be added to the add-list of P2 in Dict in order to make the add-list
   of P1 contained in the add-list of P2.
 */

global_contained(P1, P2, Dict, Add_list):-
    find_entry(Dict, P1, (_, _, _, A1)),
    find_entry(Dict, P2, (_, _, _, A2)),
    inorder(A1, L1),
    inorder(A2, L2),
    list_contained(L1, L2, Add_list).






/* inorder: traversing Tree  inorderly yields the list List with tail Tail.
 */

inorder(Tree, List) + (Tail = []):-

    Tree = {Value, Left, Right}|
      inorder(Left, List, [Value|Mid]),
      inorder(Right, Mid, Tail);

    Tree = []|
      List = Tail.




/* list_contained: accumulates in Add_list the variables which are needed
     to be added to List2 in order to make List1 contained in List2.
      all lists are sorted.
 */

list_contained(List1, List2, Add_list):-

    List1 ? Value,
    List2 ? Value|
      list_contained;

    List1 ? Value1,
    List2 = [Value2|_],
    Value1 @< Value2:
      Add_list ! Value1|
        list_contained;

    List1 = [Value1|_],
    List2 ? Value2,
    Value2 @< Value1|
      list_contained;

    List1 = [] : List2 = _,
      Add_list = [];

    List2 = [] :
      Add_list = List1.




/* add_to_list: adds the construct: add(Proc, Additions) to List with
    tail Rest only if Additions is not empty.
 */

add_to_list(Additions, Proc, List, Rest):-

     Additions = [] : Proc = _,
      List = Rest;

      otherwise :
        List = [add(Proc, Additions)|Rest].




/* complete_serve: if there are Additions to add to ODict, then adding
     them and continue serve_call with the updated dictionary. 
       otherwise: updating the calls in Call_list to have a final
     value, using the information in CDict. 
     New_dict is the final value obtained for an open dictionary.
 */ 

complete_serve(Additions, Call_list, ODict, CDict, New_dict):-

    Additions =\= [] : CDict = _ |
	add_to_dict(ODict, Additions, ODict'),
	serve_call(Call_list, ODict'?, New_dict);

    Additions = [] : ODict = _,
      CDict =  New_dict |
	complete_calls(Call_list, CDict).

      






/* complete_calls: complete the calls in Calls (the third argument of each
    call) according to the information in Dict.
 */

complete_calls(Calls, Dict):-

    Calls ? call(_Proc, Old_call, New_call) |
      complete_call(Old_call, Dict, New_call),
      complete_calls;

    Calls ? self_call(_Proc, Old_call, New_call) |
      complete_call(Old_call, Dict, New_call),
      complete_calls;

    Calls = [] : Dict = _ .

      




complete_call(Old_call, Dict, New_call):-

    proc_name(Old_call, Proc, _),
    find_entry(Dict, Proc?, (_, _, _, Add)),
    inorder(Add, Add_list),
    add_to_args_list(Old_call, Add_list, New_call).





list_to_tuple_or_atom([String], Atom):-
    Atom = String.

list_to_tuple_or_atom(List, Tuple):-
    otherwise|
      list_to_tuple(List, Tuple).



   


/* completes each Old_head to New_head in the list of head(Old_head, New_head)
    by adding the global variables of the procedure in Old_head.
 */

complete_heads(Heads, Dict):-

    Heads ? head(Head, RHead)|
      complete_call(Head, Dict, RHead),
      complete_heads;

    Heads = [] : Dict = _ .





complete_procs(Procs, Dict):-

    Procs ? proc(Old, New)|
      proc_name(Old, Proc,_),
      find_entry(Dict, Proc, Entry),
      complete_proc(Old, Entry, New),
      complete_procs;

    Procs = [] : Dict = _ .





complete_proc(Old_proc, Entry, New_proc):-

    Entry = (_, _, _, Add)|
      inorder(Add, Alist),
      make_any_list(Alist, Any_list),
      add_to_args_list(Old_proc, Any_list, New_proc);

    Entry = []|
      Old_proc = New_proc.
    





add_to_args_list(Call, Args, New_call):-

    tuple(Call) |
      tuple_to_dlist(Call, TList, Args),
      list_to_tuple(TList, New_call);

    string(Call)|
      list_to_tuple_or_atom([Call|Args], New_call).






make_any_list(Alist, Any_list):-
    Alist ? _ :
      Any_list ! `'Any'|
        make_any_list;

    Alist = [] :
      Any_list = [].





/* transform_parent: changing the calls in Parent to its sub-procedures
    in Dict to include the renamed names of the sub-procedures (by
    adding the prefix Path) and the global arguments which are local
    in Parent and are added in Dict.
    In addition, the local variables of Parent (in VTree) are renamed.
    The result is New_parent.
 */

transform_parent(Parent, Path, VTree, Dict, New_parent):-

    Parent = (Head:- RHS) :
      New_parent = (Head :- RHS1) |
	proc_name(Head, PName,_),
	transform_p_rhs(RHS, Path, PName?, VTree, Dict, RHS1);

    otherwise : Path = _, VTree = _, Dict = _,
      Parent = New_parent .






/* transform_p_rhs: changing Rhs of the parent-clause whose procedure-name is
    PName and has a path Path to Rhs1, using information about its sub-procedures
    in Dict.
    Local variables (in VTree) are renamed.
 */

transform_p_rhs(Rhs, Path, PName, VTree, Dict, Rhs1):-
    rm_locals(Rhs, Rhs'),
    transform_rest_p_rhs(Rhs', Path, PName, VTree, Dict, Rhs1).


transform_rest_p_rhs(Rhs, Path, PName, VTree, Dict, Rhs1):-

    Rhs = (Guard|Rhs') :
      Rhs1 = (Guard1|Rhs1') |
	transform_rest_p_rhs(Guard, Path, PName, VTree, Dict, Guard1),
	transform_rest_p_rhs;

    Rhs = (Ask:Rhs') :
      Rhs1 = (Ask1:Rhs1') |
	transform_rest_p_rhs(Ask, Path, PName, VTree, Dict, Ask1),
	transform_rest_p_rhs;

    Rhs = (Atom, Rhs') :
      Rhs1 = (Atom1, Rhs1') |
	transform_rest_p_rhs,
	transform_p_atom(Atom, Path, PName, VTree, Dict, Atom1);

    Rhs =\= (_ , _),
    Rhs =\= (_ | _),
    Rhs =\= (_ ; _),
    Rhs =\= (_ : _) |
	transform_p_atom(Rhs, Path, PName, VTree, Dict, Rhs1);

    Rhs = (R; Rhs') :
      Rhs1 = (R1; Rhs1') |
	transform_rest_p_rhs,
	transform_rest_p_rhs(R, Path, PName, VTree, Dict, R1).






/* transform_p_atom: if the procedure-name Name of Atom refers to a 
     sub-procedure (i.e. Path(Name is a name of a procedure in Dict) then 
     renames Name to Path(Name and adds the added arguments from Dict.
     In addition, local variables (variabls Var for which Path(Var are
     in VTree) are renamed to Path(Var.
     The result is Atom1.
 */

transform_p_atom(Atom, Path, PName, VTree, Dict, Atom1):-

    arg(1, Atom, Name),
    Arity := arity(Atom),
    make_tuple(Arity, Atom')|
        get_call_name(Name, PName, Path, Dict, Name1),
        arg(1, Atom', Name1, Atom''),
        rename_rhs_args(2, Arity, Atom, '', Path, [], VTree,
                        Atom'', RAtom, _, _),
        add_args(Name, Name1, RAtom, Dict, Atom1);

    string(Atom) : VTree = _ |
	get_call_name(Atom, PName, Path, Dict, Name1),
	add_args(Atom, Name1, Name1, Dict, Atom1);

    otherwise : Path = _, PName = _, VTree = _, Dict = _,
      Atom = Atom1 .






/* add_args: if Name = RName, then leaving Atom1 as Atom.
    otherwise (which means that Atom is a call to a sub-procedure in Dict),
     adding the added arguments of Atom from Dict. The result is Atom1.
 */

add_args(Name, RName, Atom, Dict, Atom1):-

    Name = RName : Dict = _,
      Atom = Atom1;

    Name=\=RName |
        complete_call(Atom, Dict, Atom1).











/* merge_dict: merging close Dict and the variables-tree VTree of Parent 
    to Dictionary (closed), where add-lists are unistantiated, and
    the local trees of the sub-procedures (from Dict) are replaced by
    an unistantiated variable.
    using the fact that the name of Parent is @< than the name of all the
    procedures in Dict (because the name of the parent is a partial path
    of the name of these procedures).
 */

merge_dict(Dict, VTree, Parent, Dictionary):-
    head(Parent, Head),
    proc_name(Head, PName, Arity),
    Dictionary = {(PName?, Arity?, VTree, _), [], RDict?},
    shrink_dict(Dict, RDict).




/* shrink_dict: unistantiates the variable-trees and the
      add-lists in Dict. The result is SDict.
 */

shrink_dict(Dict, SDict):-

    Dict = {(S, A, _, _), Dict', Right}|
      SDict = {(S, A, _, _), SDict'?, SRight?},
      shrink_dict,
      shrink_dict(Right, SRight);

    Dict = []|
      SDict = [].









/* assign: Assigning Val to Var only after the first argument is instantiated.
 */

assign(done, Var, Val):-
    true: Var=Val.




/* rm_locals: removing the list of local variables from a clause rhs (if exists)
 */

rm_locals(Rhs, Rhs1):-

    Rhs = (Locals; Rhs'),
    list(Locals)|
      Rhs' = Rhs1;

    Rhs = (Locals; Rhs'),
    Locals = []|
      Rhs' = Rhs1;

    otherwise|
      Rhs = Rhs1.






/* check_redefined: checks if there is a redefinition conflict by defining
   Proc once with arity Ar1 and once with Ar2. informs the result in the
   error-list Err with tail Err_t
*/

check_redefined(Proc, Ar1, Ar2, Err, Err_t):-

    Ar1 = Ar2 : Proc = _,
      Err = Err_t;

    Ar1 =\= Ar2:
      Err =[(procedure_redefined - Proc)|Err_t].

