/* $Header: /home/qiana/Repository/Logix/system/block/self.cp,v 1.1.1.1 1999/07/09 07:03:11 bill Exp $ */
/*
 *  This module is the (self of the) root of the blocker
 *  It  processs the options of the initial call.
 *
 *  This module contains the declaration of all types that are used
 *  throughout the block-compiler.
 */


-export([compose/1, compose/2]).
-language(compound).
-mode(trust).

/******* TYPE DEFINITIONS ****************************************************/

% GENERAL :

Name ::= String.
Node ::= Name ; super ; self ; file ; computation.
Nodes ::= [Node].

Path ::= Node ; {`"#", Node, Path}.			% Node ; Node#Path.
ServiceId ::= [Node].
NodeId ::= [Node | ServiceId].

Lead ::= String.					% Node1$...$NodeN$

Answer ::= true ; false.

% MODULES :

Term ::= Attribute ; TypeDeclaration ; ProcedureHeading ; Clause.
Terms, BlockedSource ::= [Term].

Attribute ::= Atom.
Attributes ::= [Attribute].

TypeDeclaration ::= (TypeName ::= Any).
TypeName ::= Name.

ProcedureHeading ::= {'procedure', Atom}.

Clause ::= Fact ; Rule ; Declaration.
Clauses ::= [Clause].

Fact ::= Atom.
Atom ::= ProcedureName ; Tuple.
ProcedureName ::= Name.
ProcId ::= ProcedureName/Integer.
ProcIds ::= [ProcId].

Rule ::= (Head :- RightSide).
Head ::= Atom.
RightSide ::= Body ; (Guard | Body) ; (Guard : Guard).
Guard ::= Any.
Body ::= (Body, Body) ; [Body] ; Goal.
Goal ::= true ; (Any = Any) ; (Any := Any)  ; RPC ; Link ; Atom.
RPC ::= {`"#", Any, Body}.
Link ::= Body @ LinkName.
LinkName ::= Name.

Procedure ::= Any.					% [ProcedureHeading,
							%  Clauses]
Declaration ::= {`"::=", Any, Any} ; procedure Atom .

% REPORT :

Report ::= [Message].

Message ::= (Path : Result).
Result ::= included ; excluded - Reason ;
	   parsing_errors(String) ; transformation_errors([Any]).
Reason ::= library ; monitor ; empty ; error(not_found) ; no_service.

ParsingErrors ::= [ParsingError].
ParsingError ::= Any.

% OPTIONS :

Option ::= name(Name) ; source([Any]) ; text(Var) ; report(Var) ; Residual ;
	   no_name ; no_source ; no_text ; no_report ; Nil ; scope(ServiceId).
							% Nil (=[]) is the
							%  default residue
Options ::= [Option] ; Option.
OptionsTuple ::= {Option, Option, Option, Option, Option, DiffResidueList}.
DiffResidueList ::= ResidueList\ResidueList.
ResidueList ::= [Residual].
Var, Residual ::= Any.


/******************* COMPOSE/1 & COMPOSE/2 ***********************************/
			
procedure compose(Any).
procedure compose(Any, Options).

% Block-compile the tree whose root is determined by Path, according to
% given or default options.

compose(Path) + (Options = []) :-
	scan_options,
	computation_utils # path_id(Path, RootId, Reply),
	arg(5, OptionsTuple, ScopeOption),
	block_scope,
	tree # load(ScopeId, RootId, Tree, BlockedSource, Report),
	report,
	source.

block_scope(Reply, ScopeOption, RootId, ScopeId, Tree) :-

    Reply = true |
	file # isdirectory(RootId, IsDirectory),
	self_scope;

    Reply = false(_) : ScopeOption = _,
      Tree = Reply,
      ScopeId = [],
      RootId = [] .

self_scope(IsDirectory, ScopeOption, RootId, ScopeId, Tree) :-

    IsDirectory = true |
	get_source # file([self | RootId], [], Result, _),
	self_source;

    IsDirectory =\= true : ScopeOption = _, RootId = _,
      ScopeId = [],
      Tree = false(not_directory).

self_source(Result, ScopeOption, RootId, ScopeId, Tree) :-

    Result = module(_, Attributes, _),
    ScopeOption = no_scope |
	auxils # member(scope(Scope), Attributes, Answer),
	validate_scope;

    Result = module(_, _, _),
    ScopeOption = scope(Scope) :
      Answer = true |
	validate_scope;

    Result = library(_, _, _) : ScopeOption = _, RootId = _,
      ScopeId = [],
      Tree = false(self_library);

    Result =\= module(_,_,_), Result =\= library(_,_,_) :
      ScopeOption = _, RootId = _,
      ScopeId = [],
      Tree = Result.

validate_scope(Answer, Scope, RootId, ScopeId, Tree) :-

    Answer = false : Scope = _,
      ScopeId = RootId |
	computation # hierarchy # source_tree(RootId, _, Tree);

    Answer = true : RootId = _ |
	computation_utils # path_id(RootId # Scope, ScopeId, Reply),
	file # isdirectory(ScopeId, IsDirectory),
	validate_scope_directory.

validate_scope_directory(Reply, IsDirectory, ScopeId, Tree) :-

    Reply = true,
    IsDirectory = true |
	computation # hierarchy # source_tree(ScopeId, _, Tree);

    Reply = true,	
    IsDirectory =\= true : ScopeId = _,
      Tree = false(scope_not_directory);

    Reply =\= true : IsDirectory = _, ScopeId = _,
      Tree = Reply .


/***************************** SCAN_OPTIONS **********************************/

procedure scan_options(Options, OptionsTuple).

% Scans Options (which is one option or a list of options), ordering the
% options into OptionsTuple, filling in default-values for unspecified
% options.

scan_options(Options, OptionsTuple) :-

    Options ? Option |
	self,
	set_option(Option, OptionsTuple', OptionsTuple);

    Options = [] :
      OptionsTuple = {no_name, no_source, no_text, no_report, no_scope,
		      ResidueList\ResidueList};

    otherwise :					% Option is one option,
	Options' = [Options] |
    	self.					%  not a list

/****************************** SET_OPTION ***********************************/

procedure set_option(Option, OptionsTuple, OptionsTuple).

set_option(Option, OptionsTuple1, OptionsTuple) :-

    Option = name(_) :
      OptionsTuple1 = {_, Source, Text, Report, Scope, Residue},
      OptionsTuple = {Option, Source, Text, Report, Scope, Residue};

    Option = source(_) :
      OptionsTuple1 = {Name, _, Text, Report, Scope, Residue},
      OptionsTuple = {Name, Option, Text, Report, Scope, Residue};

    Option = text(_) :
      OptionsTuple1 = {Name, Source, _, Report, Scope, Residue},
      OptionsTuple = {Name, Source, Option, Report, Scope, Residue};

    Option = report(_) :
      OptionsTuple1 = {Name, Source, Text, _, Scope, Residue},
      OptionsTuple = {Name, Source, Text, Option, Scope, Residue};

    Option = scope(_) :
      OptionsTuple1 = {Name, Source, Text, Report, _, Residue},
      OptionsTuple = {Name, Source, Text, Report, Option, Residue};

    otherwise :
      OptionsTuple1 = {Name, Source, Text, Report, Scope,
    			 Residue\Residue'},
      OptionsTuple = {Name, Source, Text, Report, Scope,
    			[-Option|Residue]\Residue'}.

/********************************* SOURCE ************************************/

procedure source(OptionsTuple, ServiceId, BlockedSource).

% Waits till the 2nd argument of the OptionsTuple is
% instantiated, then pretty-prints the composed module, if requested.

source(OptionsTuple, RootId, BlockedSource) :-

    arg(2, OptionsTuple, no_source) |
	pretty_printer # pretty(RootId, BlockedSource, OptionsTuple);

    arg(2, OptionsTuple, source(Source)),
    arg(6, OptionsTuple, Residue\Residue') : RootId = _,
      Source = Residue,
      BlockedSource = Residue' .

/******************************** REPORT *************************************/

% Waits till the 4th argument of the OptionsTuple is
% instantiated, then returns the report or displays it

procedure report(OptionsTuple, Report). 

report(OptionsTuple, Report) :-

    arg(4, OptionsTuple, no_report) |
	computation # Events?,
	report_events(Report, Events);

    arg(4, OptionsTuple, report(OutputReport)) :
      OutputReport = Report .

report_events(Report, Events) :-

    Report ? Event, Event =\= (_:_), Event =\= comment(_) :
      Events ! diagnostic(Event) |
	self;

    Report ? Comment, Comment = comment(_) :
      Events ! Comment |
	self;

    Report ? Comment, Comment = (_:_) :
      Events ! comment(Comment) |
	self;

    Report = [] :
      Events = [] .

/******* (END OF MODULE) *****************************************************/
