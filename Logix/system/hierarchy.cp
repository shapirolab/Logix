/*

Hierarchy utilities
Bill Silverman  02/88

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:02:54 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/system/hierarchy.cp,v $

Copyright (C) 1988, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([update/1, compile/1, lint/1, type_check/1,
	 update/2, compile/2, lint/2,
	 source_tree/3]
).
-mode(interrupt).
-language(compound).

SystemReply ::= true ; false(Any).
FileKind ::= director ; import.

Action ::= compile([Any]) ; lint([Any]); type_check ; update([Any]).

/*
** Base is a computation relative path to the root director of the sub-
** hierarchy to be processed.
*/

/*
** Options below may include options for get_source (syntax/1, precompile/1).
** Residual options are referred to the designated process (e.g. lint).
** Options for compile and update may include an option for get_module
** (auto, query).  For auto option, binary is not written for a module
** which has compilation errors.
*/

/*
** update/1 & update/2 - update(Base) & update(Base, Options)
**
** Compare the date of modification of each source module in the sub-hierarchy
** to the date of compilation of the corresponding binary module.  If the
** source module's date is more recent, compile it with given (default)
** options.
**
** Default Options for update/1 is [auto].
*/

procedure update(Any).
procedure update(Any, Any).

update(Base) + (Options = [auto]) :-
	open(Base, update(Options)).

/*
** compile/1 & compile/2 - compile(Base) & compile(Base, Options)
**
** Compile each source module in the sub-hierarchy with given (default)
** Options.  The user is queried whether to write ("use") the binary of
** a module which has compilation errors, unless Options include "auto".
*/

procedure compile(Any).
procedure compile(Any, Any).

compile(Base) + (Options = []) :-
	open(Base, compile(Options)).

/*
** lint/1  & lint/2 - lint(Base) & lint(Base, Options)
**
** Check each module in the sub-hierarchy using the lint analyzer with
** given (default) Options.  The user is queried whether to analyze the
** precompiled source, if there are precompilation errors and Options
** includes "query".
*/

procedure lint(Any).
procedure lint(Any, Any).

lint(Base) + (Options = auto) :-
	open(Base, lint(Options)).

/*
** type_check/1 - type_check(Base)
**
** Type-Check each module in the sub-hierarchy using the type_check analyzer.
*/

procedure type_check(Any).

type_check(Base) :-
	open(Base, type_check).

/*
** source_tree/3
**
** Prepare a tree describing the sub-hierarchy and its source modules.
**
** Return  ServiceId, the service identifier of the root node, and Tree.
**
**   Each node of Tree includes a list of SubTrees and a list of Module
**   names - the first module name in the list is "self" iff there is
**   such a source module within the corresponding director.
*/

RootTree ::= {[], Names, Trees}.
Tree ::= {ServiceId, Names, Trees}.
Names ::= [Name].
Name ::= String.
ServiceId ::= [String].
Trees ::= [Tree].


procedure source_tree(Any, ServiceId, RootTree).

source_tree(Base, ServiceId, RootTree) :-
    true : RootTree = {[], Modules?, SubTrees?} |
        computation_utils #
		call_list([Base#service_id(ServiceId)
			  | known(ServiceId,
				  [ServiceId # attributes(Attributes)]
			    )
			  ],
			  SystemReply1
		),
	source_tree1(Attributes, ServiceId, SystemReply1, SystemReply2),
	source_tree2(Base, ServiceId, Modules, SubTrees, SystemReply2).

Attributes ::= [Any].

procedure source_tree1(Attributes, ServiceId, SystemReply, SystemReply).

source_tree1(_, [], true, true^).
source_tree1([director | _], _, _, true^).
source_tree1(Attributes, ServiceId, Reply1, Reply2) :-
    Attributes ? Other,
    Other =\= director |
	source_tree1.
source_tree1([], [Name | Scope], _, Reply) :-
	file # execute_in_context(Scope, isdirectory(Name, Reply)).
source_tree1(_, ServiceId, Reply, Reply^) :-
    Reply =\= true |
	unify_without_failure(ServiceId, []).


procedure source_tree2(Any, ServiceId, Names, Trees, SystemReply).

source_tree2(Base, ServiceId, Modules, SubTrees, SystemReply) :-
    SystemReply = true : Base = _ |
	tree_director(ServiceId, Modules, SubTrees, Done),
	complete(Done).
source_tree2(Base, _, []^, []^, false(0)) :-
	computation # event(not_director(Base)).
source_tree2(Base, _, []^, []^, false(Reason)) :-
    Reason =\= 0 |
	computation # event(Reason(Base)).


Ready ::= ready.

procedure tree_director(ServiceId, Names, Trees, Ready).
procedure tree_director(ServiceId, Names, Trees, Ready, RelId, Ready).

tree_director(ServiceId, Modules, Trees, Done)
		+ (RelId = [], Ready = ready) :-
	catalog(ServiceId, Names, Modules, Ready, Ready'),
	tree_directors(ServiceId, Names, DirNames),
	sub_trees(ServiceId, RelId, DirNames, Trees, Ready', Done).


procedure sub_trees(ServiceId, String, Names, Trees, Ready, Ready).

sub_trees(ServiceId, RelId, DirNames, Trees, Ready, Done) :-

    DirNames ? DirName :
      DirRelId = [DirName | RelId],
      Trees ! {DirRelId, Modules?, SubTrees?} |
	sub_trees,
	tree_director([DirName | ServiceId], Modules, SubTrees, Ready',
			DirRelId, Ready
	);

    DirNames = [] : ServiceId = _, RelId = _,
      Trees = [],
      Ready = Done .


procedure tree_directors(ServiceId, Names, Names).

tree_directors(ServiceId, Names, DirNames) :-

    Names ? Name,
    Name =\= 'Bin', Name =\= bin |
	file # execute_in_context(ServiceId, isdirectory(Name, Reply)),
	director_file(Reply, Name, DirNames, DirNames'),
	tree_directors;

    Names ? _,
    otherwise |
	tree_directors.

tree_directors(_, [], []^).


procedure director_file(SystemReply, Name, Names, Names).

director_file(Reply, Name, DirNames, MoreNames) :-

    Reply = true :
      DirNames = [Name | MoreNames] ;

    otherwise : Reply = _, Name = _,
      DirNames = MoreNames .

/************************** interface to unix ********************************/

procedure complete(Ready).

complete(ready) :-
	logix_names_interface(["rm " | PLN], PLN, Ok),
	complete_rm(Ok).

complete_rm(Ok) :-

    Ok = true | true ;

    Ok = false(Reason) |
	computation # event("UNIX: rm failed"(Reason)).


StringList ::= [String | [String]].

procedure catalog(ServiceId, Names, Names, Ready, Ready).

catalog(ServiceId, Names, Modules, Ready, Done) :-
    Ready = ready |
	ServiceId # '_unique_id'(PD),
	logix_names_interface(["ls -1 ", PD, " > " | PLN], PLN, Ok),
	get_names(Ok, FileNames, Done),
	logix_files(FileNames, Names, Modules).

Strings2 ::= [String, '.logix_names' | Nil].

procedure logix_names_interface(StringList, Strings2, SystemReply).

logix_names_interface(Strings, Strings2, Ok) :-
    true : Strings2 = [PC, '.logix_names'] |
	computation # (self # '_unique_id'(PC)),
	utils # append_strings(Strings, Command),
	processor # interface(unix_call(Command?), Ok).


procedure get_names(True, ServiceId, StringList, Ready).

get_names(Ok, Strings, Ready) :-

    Ok = true |
	file # execute_in_context(self, get_file('.logix_names', NS, [], Ok1)),
	string_to_dlist(NS, NCS, []),
	utils # chars_to_lines(NCS?, Strings),
	when(Ok1, Ready);

    Ok = false(Reason) :
      Strings = [],
      Ready = ready |
	computation # event("UNIX: ls failed"(Reason)).

procedure when(Any, Ready).

when(Ok, ready^) :-
   known(Ok) |
	true.


procedure logix_files(Names, Names, Names).
procedure logix_files(Names, Names, Names, Names, Names).

logix_files(FileNames, DirNames, Modules) + (Self = Rest, Rest) :-
    FileNames ? Name,
    string_to_dlist(Name, NChars, []) |
	partition_files(NChars,
			{DirNames, Self, Rest}, {DirNames', Self', Rest'}
	), 
	logix_files.
logix_files([], []^, Self^, Self, []^).

Chars ::= [Integer].
Heads, Tails ::= {[String], [String], [String]}.

procedure partition_files(Chars, Heads, Tails).
procedure partition_files(Chars, Heads, Tails, Chars, Chars).

partition_files(NChars, Heads, Tails) + (FChars = AChars, AChars) :-

    NChars ? Dot, Dot =:= ascii('.') :
      AChars = [] |
	module(FChars, NChars', Heads, Tails);

    NChars ? Char,
    otherwise :
       AChars ! Char |
	partition_files;

    NChars = [] :
      Heads = {[DirName | DirNames], Self, Rest},
      Tails = {DirNames, Self, Rest},
      AChars = [] |
	list_to_string(FChars, DirName).

procedure module(Chars, Chars, Heads, Tails).

module(FChars, NChars, Heads, Tails) :-

    FChars =\= [],
    list_to_string(NChars, cp),
    list_to_string(FChars, Module) |
	add_module(Module, Heads, Tails);

    otherwise : FChars = _, NChars = _,
      Heads = Tails .


procedure add_module(String, Heads, Tails).

add_module(String, Heads, Tails) :-

    String =\= self, String =\= super, String =\= computation,
    Heads = {DirNames, Self, Rest} :
      Rest ! String,
      Tails = {DirNames, Self, Rest'} ;

    String = self,
    Heads = {DirNames, Self, Rest} :
      Tails = {DirNames, [self | Self], Rest} ;

    otherwise : String = _,
      Heads = Tails .

/*****************************************************************************/

% Open Base and start walk in its director for update/1, lint/1, lint/2,
% compile/1, compile/2.

procedure open(Any, Action).

open(Base, Action) :-
	source_tree(Base, Root, {_, Modules, Trees}),
	root_path(Base, RelPath),
	director(RelPath, Action, Root, Modules, Trees).


procedure root_path(Any, String).

root_path(Base, RelPath) :-

    string(Base), Base =\= self, Base =\= super, Base =\= computation,
    freeze(Base # _, RelPath^, _) |
	true;

    otherwise : Base = _ |
	freeze(_, RelPath, _).

/*************************** hierarchy walk **********************************/

procedure director(String, Action, ServiceId, Names, Trees).
procedure director(String, Action, ServiceId, Names, Trees, Ready).

director(RelPath, Action, ServiceId, Modules, Trees) + (Ready = _) :-
	modules(RelPath, Action, ServiceId, Modules, Ready'),
	directors(Ready', Trees, RelPath, Action, ServiceId, Ready''),
	close_node(Ready'', Action, ServiceId, Ready).

procedure close_node(Ready, Action, ServiceId, Ready).

close_node(Ready, Action, ServiceId, Ready1) :-

    Action =\= update(_), Action =\= compile(_) : ServiceId = _,
      Ready = Ready1 ;

    otherwise,
    Ready = ready : Action = _ |
	computation # shell(close, [ServiceId], Ok),
	when(Ok, Ready1).


procedure directors(Ready, Trees, String, Action, ServiceId, Ready).

directors(Ready, Trees, RelPath, Action, ServiceId, Ready1) :-

    Ready = ready,
    Trees ? {[DirName | _], Modules, SubTrees} |
	dir_name_path(DirName, RelPath, DirRelPath),
	director(DirRelPath, Action, [DirName | ServiceId], Modules,
			SubTrees, Ready'
	),
	directors;

    otherwise : Trees = _, RelPath = _, Action = _, ServiceId = _,
      Ready = Ready1 .


procedure dir_name_path(Name, String, String).

dir_name_path(Name, RelPath, DirRelPath) :-

    true :
      melt(RelPath, MeltedPath, [Name # _]) |
	freeze(MeltedPath, DirRelPath, _).


procedure modules(String, Action, ServiceId, Names, Ready).
procedure modules(String, Action, ServiceId, Names, Ready, Ready).

modules(RelPath, Action, ServiceId, Modules, Ready) + (Done = ready) :-

    Done = ready,
    Modules ? Module |
	display_path(Action, RelPath, Module, Action'),
	action(Action', ServiceId, Module, Ok, Call),
	when(Ok, Done'),
	computation_utils # Call,
	modules;

    Done = ready,
    Modules = [] : RelPath = _, Action = _, ServiceId = _,
      Ready = ready .

procedure display_path(Action, String, String, Action).

display_path(Action1, RelPath, String, Action2) :-

    Action1 =\= compile(_), Action1 =\= update(_) :
      melt(RelPath, Melted, [String]) |
	computation # display(term, Melted, close(Action1, Action2));

    otherwise : RelPath = _, String = _,
      Action1 = Action2 .

procedure action(Action, ServiceId, Name, Any, Tuple).

action(Action, ServiceId, Name, Ok, Call) :-

    Action = compile(Options) :
      Call = call_output([identifier(compile(Name)),
			  get_module # compile_module([Name | ServiceId], 
						      Options, Output, _
					)
			 ],
			 Output,
			 [prefix(' -'), type(ground)],
			 Ok
		) ;

    Action = type_check :
      Call = call_output([type_check #
				module(ServiceId # Name, Output),
			  identifier(type_check(Name))
			 ],
			 Output,
			 [prefix(' -'), type(ground)],
			 Ok
		) ;

    Action = lint(Options), Options =\= [] :
      Call = call_output([identifier(lint(Name)),
			  lint # lint(context(ServiceId, Name), Options)],
			 [], [], Ok);

    Action = lint([]) :
      Call = call_output([identifier(lint(Name)),
			  lint # lint(context(ServiceId, Name))],
			 [], [], Ok);

    Action = update(Options) :
      Call = call_output([identifier(compile(Name)),
			  get_module # update(ServiceId, Name, 
					      Options, Output, _
					  )
			 ],
			 Output,
			 [prefix(' -'), type(ground)],
			 Ok
		) .
