/* $Header: /home/qiana/Repository/Logix/system/lint/cross.cp,v 1.2 2002/11/16 11:35:22 bill Exp $ */
/*
 *
 * lint_cross module is the one that do the globals checks on the dictionary
 * and the checks per goal.
 * It has two exported procedures:
 *	- goal_internal/4 check for every goal it internal data.
 *	- modulewide/5 check the global data.
 *
 */

-export([clause_internal/4, proc_wide/5, modulewide/5]).
%-mode(trust).
-language(compound).

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

procedure clause_internal(String/Integer, [VarRef], Any, Ci_options).

/*
 *
 * clause_internal/4 check each clause in the module for internal
 * consistency. Currently it checks if a variable appears once in the
 * clause or if it appears in read-only mode only. If option  varoncenusc
 * is set, it checks if each variable whose first character is underscore
 * appears more than once in the clause.
 *
 */

clause_internal(Id, Gvi, _Gpi, Ci_options) :-

    Ci_options ? Var_once_gi, Var_once_gi =\= false :  Ci_options' = _ | 
	gi_check_vars(Id, Gvi, Var_once_gi);

    otherwise : Id = _, Gvi = _, Ci_options = _ .

/*
 *
 * gi_check_vars/3 implements the variable checks for each variable.
 *
 *
 */


gi_check_vars(Id, Gvi, Var_once_gi) :-

    Gvi ? varref(Name, List) |
	first_char(Name, '', First_char),
	gi_check_var_occurs(Id, Name, First_char, List, Var_once_gi),
	gi_check_ro_occurs(Id, Name, List),
	gi_check_vars;

    otherwise : Id = _, Gvi = _, Var_once_gi = _ .

/*
 *
 * gi_check_var_occurs/5 checks appearance of variables according
 * to the options in use.
 *
 */

gi_check_var_occurs(Id, Var, First_char, List, Var_once_gi) :-

    List = [_One_appearance],
    Var_once_gi = once_non_underscored,
    First_char =\= '_', First_char =\= '$' |
	check_generated_id_end;

    First_char = '_',
    Var_once_gi = once_non_underscored,
    List = [_, _ | _] |
	computation # diagnostic((Id, underscored_variable - Var
					- appears_multiply)
		 );

    List = [_One_appearance],
    Var_once_gi = true : First_char = _ |
	check_generated_id_end;

    otherwise : Id = _, Var = _, First_char = _, List = _, Var_once_gi = _ .

check_generated_id_end(Id, Var) :-

    Id = Name/_Arity - _Index,
    NL := string_length(Name),
    nth_char(NL--, Name, I),
    ascii('0') =< I, I =< ascii('9') |
	check_generated_id;

    Id = Name/_Arity,		% For proc-wide test
    NL := string_length(Name),
    nth_char(NL--, Name, I),
    ascii('0') =< I, I =< ascii('9') |
	check_generated_id;

    otherwise |
	computation # diagnostic((Id, variable - Var - appears_only_once)).

check_generated_id(Id, Var, NL, Name) :-

    nth_char(NL--, Name, I),
    ascii('0') =< I, I =< ascii('9') |
	self;

    nth_char(NL, Name, Dollar),
    Dollar =:= ascii('$') : Id = _, Var = _ ;

    otherwise : NL = _, Name = _ |
	computation # diagnostic((Id, variable - Var - appears_only_once)).

/*
 *
 * gi_check_ro_occurs/3 checks if all the variables in the goal are in ro 
 * mode or suspended and cant get a value.
 *
 */

gi_check_ro_occurs(Id, Name, List) :-


    List ? at(_Id1, var, _Part) : Id = _, Name = _, List' = _ ;

    otherwise,
    List ? at(_Id1, _RoSuspend, _Part) |
	gi_check_ro_occurs;

    List = [] |
	computation # diagnostic((Id, variable - Name -
				only_in_read_only_mode)).


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

procedure proc_wide(String/Integer, Pvi_list, Pci_list, Any, Pi_options).

/*
 * proc_wide/5 ...
 */

proc_wide(Id, Pvi_list, Pci_list, _Ppi_list, Pi_options) :-
    Pi_options = [Prccasechk, Prcvaronce | _] |
	prc_case_check(Id, Pvi_list, Pci_list, Prccasechk),
	prc_check_vars_occurs(Id, Pvi_list, Prcvaronce).

/*
 *
 * prc_check_var_occurs/3, prc_check_var_occurs/2 & prc_check_var/2 check 
 * if every variable in the procedure appears at least twice.
 * Underscored  variables and generated ('$'...) variables are ignored.
 *
 */

prc_check_vars_occurs(Id, Pvi_list, Prcvaronce) :-

    Prcvaronce = false : Id = _, Pvi_list = _, Prcvaronce = _ ;

    Prcvaronce = true |
	prc_check_vars_occurs(Id, Pvi_list).


prc_check_vars_occurs(Id, Pvi_list) :-

    Pvi_list ? varref(Name, List) |
	prc_check_vars_occurs,
	first_char(Name, '_', First_char),
	prc_check_var(Id, Name, First_char, List);

    Pvi_list = [] : Id = _ .


prc_check_var(Id, Var, First_char, List) :-

    First_char = '_' : Id = _, Var = _, List = _ ;

    First_char = '$' : Id = _, Var = _, List = _ ;

    List = [_,_|_] : Id = _, Var = _, First_char = _ ;

    List = [_],
    First_char =\= '_', First_char =\= '$' |
	check_generated_id_end.

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

procedure modulewide(Ref_list, Vr, Cr, Pr, Options).

/*
 *
 * modulewide/5 does all of the global checks/actions for the module.
 *
 * The checks are:
 *	- Undefined/Uncalled procedures.
 *	- Check if vars/consts with similar names appears.
 *
 * The actions are:
 *	- cross-reference, by 3 separate options:
 *		constants, variables, procedures
 *	- Give a list of the defined procedures and the number
 *	  of clauses in each.
 *	- Give a list of procedures imported by the module.
 *
 */

modulewide(Ref_list, Vr, Cr, Pr, Options) :-

    Options = [Constref, Varsref, Procref, Undef, Deadcode, Casecheck,
		Proclist, Imported | _] |
	check_procs(Pr, Undef, Deadcode),
	case_check(Vr, Cr, Casecheck),
	const_ref(Cr, Constref),
	vars_ref(Vr, Varsref),
	proc_ref(Pr, Procref),
	proc_list(Ref_list, Proclist),
	imported_procs(Pr, Imported).

/*
 *
 * check_proc/3 checks if there are undefined procedures and/or uncalled ones.
 *
 */

check_procs(Pr, Undef, Deadcode) :-

    Undef = false,
    Deadcode = false : Pr = _ ;

    otherwise |
	check_procs1(Pr, Undef, Deadcode, Deads, Undefs),
	check_undefs(Undefs, Undef, Code),
	check_deads(Deadcode, Deads, Code).


check_deads(Deadcode, Deads, Code) :-

    Deadcode = true,
    list(Deads) |
	rqs_generator(Code, DicReqs),
	compile # deadcode(DicReqs, DeadList),
	convert_list(DeadList, DeadList'),
	insert(DeadList', DeadLib, done, Done, _),
	check_deads1(Done, Deads, DeadLib);

    otherwise : Deadcode = _, Deads = _, Code = _ .


rqs_generator(Code, DicReqs) :-

    Code ? procedure(Id, _, Xrefs) :
      DicReqs ! procedure(Id, Xrefs) |
	rqs_generator;

    Code = [] :
      DicReqs = [] .


convert_list(In, Out) :-

    In ? Entry :
      Out ! {_, Entry} |
	convert_list;

    In = [] :
      Out = [] .


check_undefs(List, Flag, SysCode) :-

    Flag = true,
    list(List) |
	check_list(List, List1),
	make_queries(List1, List2),
	computation # library(query, List2, [], SysCode),
	check_queries(List2, List1);

    otherwise : List = _, Flag = _,
      SysCode = [] .


check_list(Procs, NewList) :-

    Procs ? Proc |
	predefined_procedure(Proc, Reply),
	sieve(Reply, Proc, NewList, NewList'),
	check_list;

    Procs = [] :
      NewList = [] .


sieve(Reply, Proc, NewList1, NewList2) :-

    Reply = 0 :
      NewList1 = [Proc | NewList2] ;

    otherwise : Reply = _, Proc = _,
      NewList1 = NewList2 .


make_queries(Procs, Queries) :-

    Procs ? {_, Id} :
      Queries ! query(Id, _) |
	make_queries;

    Procs = [] :
      Queries = [] .


check_queries(Queries, Procs) :-

    Queries ? query(_, false),
    Procs ? Proc |
	display_proc(undefined_procedure, undefined_guard_predicate, Proc),
	check_queries;

    Queries ? query(_, true),
    Procs ? _ |
	check_queries.

check_queries([],[]).


insert(Procs, Lib, Left, Right, List) :-

    Procs ? Proc,
    arg(2, Proc, Id) |
	insert1(Proc, Id, Lib, Left(List), Left'(List')),
	insert;

    Procs = [] : Lib = _,
      Left = Right,
      List = [] .


insert1(Proc, Id, Lib, Left, Right) :-

    Lib = {Proc1, _, _}, 
    arg(2, Proc1, Id) : Proc = _,
      Left = Right ;

    Lib = {Proc1, Lib', _}, 
    arg(2, Proc1, Id1),
    Id @< Id1 |
	insert1;

    Lib = {Proc1, _, Lib'}, 
    arg(2, Proc1, Id1),
    Id1 @< Id |
	insert1;

    true : Id = _,
      Lib = Leaf?,
      Left = Done(List),
      List ! Proc,
      Done(List') = Right |
	Leaf = {Proc, _, _}.


check_deads1(Done, Deads, Lib) :-
    Done = done |
	insert(Deads, Lib, _, _, List),
	pre_display_check(List).

pre_display_check(List) :-

    List ? Proc,
    Proc = user(Name/_Arity),
    NL := string_length(Name),
    nth_char(NL--, Name, I),
    ascii('0') =< I, I =< ascii('9') |
	check_generated_proc;

    List ? Proc,
    otherwise |
	display_proc(unreferenced_procedure, '*** error ***', Proc),
	self;

    List = [] |
	true.

check_generated_proc(List, Proc, NL, Name) :-

    nth_char(NL--, Name, I),
    ascii('0') =< I, I =< ascii('9') |
	self;

    nth_char(NL, Name, Dollar),
    Dollar =:= ascii('$') : Proc = _ |
	pre_display_check;

    otherwise : NL = _, Name = _ |
	display_proc(unreferenced_procedure, '*** error ***', Proc),
	pre_display_check.

check_procs1(Pr, Undef, Deadcode, Deads,Undefs) :-

    	Pr ? prc(Proc, List) |
	check_proc_occurs(Proc, List, _, 
				Undef, Deadcode,
					Undefs(Deads), Undefs'(Deads')
	),
	check_procs1;

    Pr = [] : Undef = _, Deadcode = _,
      Deads = [],
      Undefs = [] .

/*
 *
 * check_proc_occurs/7 checks if the procedure occurs as declared only
 * or only as used. The Cmd argument is in  [declare, use, none].
 * 'none' means initial mode (that we didn't look in the list yet).
 *
 */

check_proc_occurs(Proc, List, Cmd, Undef, Deadcode, Left, Right) :-

    List ? at(_Id, Cmd^, _Part) |
	check_proc_occurs;
/*

    Cmd = none,
    List ? at(_Id, Cmd', _Part) |
	check_proc_occurs;

*/

    List = [], Cmd = use, Undef = true : Deadcode = _,
      Left = Undefs(Deads),
      Undefs ! Proc,
      Right = Undefs'(Deads) ;
      
    List = [], Cmd = declare : Undef = _, Deadcode = _,
      Left = Undefs(Deads),
      Deads ! Proc,
      Right = Undefs(Deads') ;
      
    otherwise : Proc = _, List = _, Cmd = _, Undef = _, Deadcode = _,
      Left = Right . 


display_proc(MssU, MssG, Proc) :-

    Proc = user(Id),
    Id =\= '#'/2, Id =\= '@'/2 : MssG = _ |
	computation # diagnostic(MssU - Id);

    Proc = guardp(Id) : MssU = _ |
	computation # diagnostic(MssG - Id);

    otherwise : MssU = _, MssG = _, Proc = _ .
				% including user(#/2), user(@/2), external(_)

/*
 *
 * const_ref/2 sends the constant references to the screen.
 *
 */

const_ref(Cr, Constref) :-

    Constref = false : Cr = _ ;

    Constref = true |
	computation # Stream,
	comment_stream(Cr, Stream).

comment_stream(Cr, Stream) :-

    Cr ? Comment :
      Stream ! comment(Comment) |
	self;

    Cr = [] :
      Stream = [] .

/*
 *
 * vars_ref/2 sends to the screen the variables reference list.
 *
 */

vars_ref(Vr, Varsref) :-

    Varsref = false : Vr = _ ;

    Varsref = true |
	computation # Stream,
	comment_stream(Vr, Stream).

/*
 *
 * proc_ref/2 sends to the screen the procedures reference lists.
 * Also it says if the procedure is marked predefined or not.
 *
 */

proc_ref(Pr, Procref) :-

    Procref = true |
	make_list(Pr, Procs, Lists),
	make_queries(Procs, Queries),
	computation # library(query, Queries, [], _),
	proc_refs(Queries, Procs, Lists);

    Procref = false : Pr = _ .


make_list(Pr, Procs, Lists) :-

    Pr ? prc(Proc,List) :
      Procs ! Proc,
      Lists ! List |
	make_list;

    Pr = [] :
      Procs = [],
      Lists = [] .


proc_refs(Queries, Procs, Lists) :-

    Queries ? query(_, Res) :
      Procs ! Proc,
      Lists ! List |
	proc_refs1(Res, Proc, List),
	proc_refs;

    Queries = [] :
      Procs = [],
      Lists = [] .

proc_refs1(Res, Proc, List) :-

    Res = false |
	computation # comment((proc_ref - Proc : List));

    Res = true |
	computation # comment((proc_ref - Proc, predefined : List)).


/*
 *
 * proc_list/2 sends to the display the declared precedure list with the 
 * number of goals defined for each procedure.
 *
 */

proc_list(Ref_list, Proclist) :-

    Proclist = false : Ref_list = _ ;

    Proclist = true |
	computation # display(stream, Ref_list, prefix(proc_list)).

/*
 *
 * imported_procs/2 outputs the imported procedures that are called from the
 * module.
 *
 */

imported_procs(Pr, Imported) :-

    Imported = false : Pr = _ ;

    Imported = true |
	imported_procs1(Pr, []).

imported_procs1(Pr, Imports) :-

    Pr ? prc(external(Proc), _List) :
      Imports' = [Proc | Imports] |
	imported_procs1;

    otherwise,
    Pr ? _ |
	imported_procs1;

    Pr = [] |
	computation # comment((imported_procs : Imports)).

/*
 *
 * case_check/3 and prc_case_check/4 check for similarity of
 * costants/variables names.
 * They have 2 modes:
 *	- casecheck: checks for difference in letter case only.
 *	- similarity: as above but ignores special characters(except for
 *	  lettters, and digits)
 *
 */

case_check(Vr, Cr, Casecheck) :-

    Casecheck = false : Vr = _, Cr = _ ;

    otherwise |
	stream # merger([merge(Vr) | Cr], Refs),
	lowercase(Casecheck, Refs, T_msgs, [tree_to_list(Lowers)]),
	tree # binary_tree(T_msgs, lowers),
	duplications(modulewide, Lowers).

prc_case_check(Id, Pci_list, Pvi_list, Casecheck) :-

    Casecheck = false : Id = _, Pvi_list = _, Pci_list = _ ;

    otherwise |
	stream # merger([merge(Pvi_list) | Pci_list], Refs),
	lowercase(Casecheck, Refs, T_msgs, [tree_to_list(Lowers)]),
	tree#binary_tree(T_msgs, lowers),
	duplications(Id, Lowers).

/*
 *
 * lowercase/3 translates any name to its lowercase version. If similarity
 * option set, omit special characters in the lowercase version.
 *
 */

lowercase(Casecheck, Refs, Msgs1, Msgs2) :-

    Refs ? {_VCref, Name, _VCat} :
      Msgs1 ! add(Y, Name) |
	lowercase_word(Casecheck, Name, Y),
	lowercase;

    Refs = [] : Casecheck = _,
      Msgs1 = Msgs2 .


lowercase_word(Casecheck, X, Y) :-

    string_to_dlist(X, List, []) |
	lowercase_list(Casecheck, List, LList, Done),
	list_to_string(Done, LList, Y);

    otherwise : Casecheck = _, X = _,
      Y = '_' .

lowercase_list(Casecheck, Is, NIs, Done) :-

    Is ? I,
    ascii('A') =< I, I =< ascii('Z'),
    NI := I - ascii('A') + ascii('a') :
      NIs ! NI |
	lowercase_list;

    Is ? I,
    ascii('a') =< I, I =< ascii('z') :
      NIs ! I |
	lowercase_list;

    Is ? I,
    ascii('0') =< I, I =< ascii('9') :
      NIs ! I |
	lowercase_list;

    otherwise,
    Casecheck = casecheck,
    Is ? I :
      NIs ! I |
	lowercase_list;

    otherwise,
    Is ? _ |
	lowercase_list;

    Is = [] : Casecheck = _,
      NIs = [],
      Done = true .


list_to_string(true, [], '_'^).
list_to_string(true, List, Y) :-
    List =\= [],
    list_to_string(List, Y^) |
	true.


first_char(Name, Default, First_char) :-

    string_to_dlist(Name, [First | _], []),
    list_to_string([First], First_char^) : Default = _ ;

    otherwise : Name = _,
      First_char = Default .

/*
 *
 * duplications/2 get a list of names and their lowercase version and diagnose
 * names whose lowercase version occurs more than once.
 *
 */

duplications(Id, Refs) :-

    Refs ? lowers(Lower, List),
    Lower =\= '_' |
	output_dup(Id, List),
	duplications;

    Refs ? lowers('_', _List) |
	duplications;

    Refs = [] : Id = _ .

output_dup(Id, Dup) :-

    Dup = [_] : Id = _ ;

    otherwise |
	computation # diagnostic((Id, case_duplication(Dup))).


predefined_procedure(guardp(true/0), true^).
predefined_procedure(guardp(ascii/2), true^).
predefined_procedure(guardp(':='/2), true^).
predefined_procedure(guardp(Proc), Index) :-
    otherwise |
        guardtable # dictionary([guard_index(Proc, Index)]).
predefined_procedure(user(true/0), true^).
predefined_procedure(user('#'/2), true^).
predefined_procedure(user('@'/2), true^).
predefined_procedure(user(':='/2), true^).
predefined_procedure(_, 0^) :-
    otherwise |
	true.
