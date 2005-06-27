/*

Source preprocessor

William Silverman

Last update by		$Author: bill $
		       	$Date: 2005/06/27 04:20:50 $
Currently locked by 	$Locker:  $
			$Revision: 1.2 $
			$Source: /home/qiana/Repository/Logix/system/get_source.cp,v $

Copyright (C) 1988, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([characters/4, context/5, file/4, parsed/4, string/4]).
-mode(trust).
-language(compound).

procedure context(file#Path, String, List, Result, {List, List}).
procedure file(file#Path, List, Result, {List, List}).
procedure characters([Integer], List, Result, {List, List}).
procedure string(String, List, Result, {List, List}).
procedure parsed(List, List, Result, {List, List}).

List ::= [Any].
Result ::= library(List, List, List) ;
	   module(List, List, List) ;
	   false(Any).

context(Path, Name, Options, Result, Outs):-
	prepare(context(Path, Name), Options, Result, Outs).

file(Path, Options, Result, Outs):-
	file # Requests?,
	resolve_path(Path, Requests, File),
	prepare(File, Options, Result, Outs).

characters(Chars, Options, Result, Outs) :-
	prepare(characters(Chars), Options, Result, Outs).

string(String, Options, Result, Outs) :-
	prepare(string(String), Options, Result, Outs).

parsed(Terms, Options, Result, Outs) :-
	prepare(parsed(Terms), Options, Result, Outs).


resolve_path(Path, Requests, File) :-

    Path ? Name :
      File = file(Path', Name, Requests) ;
    
    otherwise |
	computation_utils # call_id_goal(computation#Path, Id, Goal, Reply),
	check_path(Reply, Requests, File, Id, Goal).

check_path(Reply, Requests, File, Id, Goal) :-

    Reply = true :
      File = file(Id, Goal, Requests) ;

    Reply = false(_) : Id = _, Goal = _,
      Requests = [],
      File = Reply .


prepare(Source, Options, Result, Outs) :-
    true : Outs = {O, O'} |
	monitor_functions # choke_stream(Done, Terms1, Terms2, Abort),
	prepare_controls(Source, Options, Abort, Done,
			 SourceControl, ScanControl
	),
	get_source_terms(Abort, SourceControl, Source, Reply, Terms1, {O, O1}),
	scan_terms(Reply, ScanControl, Terms2, Result, {O1, O'}).


prepare_controls(Source, Options, Abort, Done, SourceControl, ScanControl) :-
    true :
      SourceControl = {Syntax, Option_Attributes, Residue}, 
      ScanControl = {Done, Option_Attributes, Context', Includes'} |
	scan_options(Abort, Options, {Syntax, Context, Includes, Residue}),
	prepare_context(Source, Context, Context', Includes, Includes').

prepare_context(Source, Option, Context, Included, Includes) :-

    Source = file(Context^, _Name, _Requests),
    Included =\= included : Option = _,
      Includes = Included ;

    Source = file(Context^, Name, _Requests),
    Included = included : Option = _,
      Includes = Included(Name) ;

    Source = context(Context^, _Name) : Option = _,
      Includes = Included ;

    otherwise : Source = _,
      Context = Option,
      Includes = Included .


get_source_terms(Abort, Control, Source, Reply, Terms, Outs) :-

    Source = string(String),
    string(String) |
	close_source_control(Control, Syntax),
	tokenize_source(Abort, found, Syntax, String, Reply, Terms, Outs);

    Source = characters(Chars),
    list(Chars) |
	close_source_control(Control, Syntax),
	tokenize_source(Abort, found, Syntax, Chars, Reply, Terms, Outs);

    Source = parsed(Parsed),
    list(Parsed) :
      Outs = {O, O} |
	close_source_control(Control, _),
	module_kind(Abort, Parsed, Reply, Terms);

    Source = file(Context, Name, Requests) :
      Requests ! execute_in_context(Context, isdirectory(Name, IsDirectory)) |
	get_file_source(Abort, Control, IsDirectory, Reply, Terms, Outs,
			Context, Name, Requests'
	);

    Source = context(Context, Name),
    string(Name) :
      Control = {Syntax, [source(Absolute), date(Date)|As], As} |
	file # execute_in_context(Context, 
				get_source(Name, Chars, Status, Absolute, Date)
	     ),
	tokenize_source(Abort, Status, Syntax, Chars, Reply, Terms, Outs);

    otherwise : Abort = _, Source = _,
      Reply = false(invalid_source),
      Terms = [],
      Outs = {O, O} |
	close_source_control(Control, _);

    Abort = abort : Source = _,
      Reply = false(aborted),
      Terms = [],
      Outs = {O, O} |
	close_source_control(Control, _).


get_file_source(Abort, Control, IsDirectory, Reply, Terms, Outs,
			Context, Name, Requests
) :-

    IsDirectory =\= true :
      Requests ! execute_in_context(Context,
			get_source(Name, Chars, Status, Absolute, Date)
		 ) |
	check_file_status(Abort, Control, Status, Reply, Terms, Outs,
				Context, Name, Requests',
				Chars, Absolute, Date
	);

    IsDirectory = true : Abort = _, Context = _, Name = _,
      Reply = false(director),
      Terms = [],
      Outs = {O, O},
      Requests = [] |
	close_source_control(Control, _);

    Abort = abort : IsDirectory = _, Context = _, Name = _,
      Reply = false(aborted),
      Terms = [],
      Outs = {O, O},
      Requests = [] |
	close_source_control(Control, _).

check_file_status(Abort, Control, Status, Reply, Terms, Outs,
			Context, Name, Requests, Chars, Absolute, Date
) :-

    Status = not_found,
    Name =\= self,
    Context ? _ : Chars = _, Absolute = _, Date = _,
      Source = file(Context', Name, Requests) |
	get_source_terms(Abort, Control, Source, Reply, Terms, Outs);

    Status = not_found,
    Name =\= self,
    channel(Context) : Chars = _, Absolute = _, Date = _,
      write_channel(service_id(Context'), Context) |
	self;

    otherwise : Context = _, Name = _,
      Control = {Syntax, [source(Absolute), date(Date) | As], As},
      Requests = [] |
	tokenize_source(Abort, Status, Syntax, Chars, Reply, Terms, Outs);

    Abort = abort :
      Status = _, Context = _, Name = _, Chars = _, Absolute = _, Date = _,
      Reply = false(aborted),
      Terms = [],
      Outs = {O, O},
      Requests = [] |
	close_source_control(Control, _).


close_source_control({Syntax, As, As}^, Syntax).


tokenize_source(Abort, Status, Syntax, Source, Reply, Terms, Outs) :-

    Status = found |
	parse # tokenize(Source, Tokens),
	parse_source(Abort, Syntax, Tokens, Reply, Terms, Outs);

    Status =\= found, Status =\= false(_) : Abort = _, Syntax = _, Source = _,
      Reply = false(error(Status)),
      Terms = [],
      Outs = {O, O} ;

    Status = false(Reason) : Abort = _, Syntax = _, Source = _,
      Reply = false(Reason),
      Terms = [],
      Outs = {O, O} ;

    Abort = abort : Status = _,
      Abort' = _,
      Status' = false(aborted) |
	tokenize_source.


parse_source(Abort, Syntax, Tokens, Reply, Terms, Outs) :-

    true :
      Outs = {O1, O2} |
	syntax_names(Abort, Syntax, AllTokens, Tokens),
	parse # tokens(AllTokens, Parsed, ParseErrors),
	parser_errors(Abort, ParseErrors, O1, O2),
	module_kind(Abort, Parsed, Reply, Terms).


syntax_names(Abort, Syntax, Tokens1, Tokens2) :-

    Syntax = [] : Abort = _,
      Tokens1 = Tokens2 ;

    Syntax ? SyntaxName :
      Tokens1 = ['-', funct(syntax, 0), '(', SyntaxName, ')', '.' | Tokens1'] |
	syntax_names;

    Abort = abort : Syntax = _, Tokens2 = _,
      Tokens1 = [] .


parser_errors(Abort, Errors, Out1, Out2) :-

    Errors ? Error :
      Out1 ! Error |
	parser_errors;

    Errors = [] : Abort = _,
      Out1 = Out2 ;

    Abort = abort : Errors = _,
      Out1 = Out2 .


module_kind(Abort, Terms1, Kind, Terms2) :-

    Terms1 ? library : Abort = _,
      Kind = library,
      Terms1' = Terms2 ;

/* Add other Kind detection here. */

    otherwise :
      Abort = _,
      Kind = module,
      Terms1 = Terms2 ;

    Abort = abort : Terms1 = _,
      Kind = false(aborted),
      Terms2 = [] .


scan_terms(Kind, ScanControl, Terms, Result, Outs) :-

    Kind = library,
    ScanControl = {Done, Options, Context, Includes} :
      Result = library(Options, Attributes, Terms') |
	scan_attributes(Terms, Attributes, Terms', Done, Outs,
			Context, Includes
	);

    Kind = module,
    ScanControl = {Done, Options, Context, Includes} :
      Result = module(Options, Attributes, Terms') |
	scan_attributes(Terms, Attributes, Terms', Done, Outs,
			Context, Includes
	);

    Kind = false(_) : ScanControl = _, Terms = _,
      Kind = Result,
      Outs = {O, O} .

scan_attributes(Terms1, Attributes, Terms2, Done, Outs, Context, Includes)
		+ (Includable = true, Is = Head, As = Head) :-

    Terms1 ? -A,
    A = include(Include),
    Includes =\= included(_),	% This is the primary module!
    string(Include) : Includable = _, Is = _, As = _ |
	member(Include, Includes, Reply),
	include_file(Terms1', Attributes, Terms2, Done, Outs,
			Context, Includes, Reply
	);

    Terms1 ? -include(Include),
    Includes =\= included(_),	% This is the primary module!
    Include ? I :
      Terms1'' = [-include(I), -include(Include') | Terms1'] |
	self;

    Terms1 ? -include([]) |
	self;

    Terms1 ? -I,
    I = include(_),
    Includes = included(_) :	% This is an included module!
      As ! I |
	self;

    Terms1 ? -A,
    A = includable(true) : Includable = _,
      Includable' = true |
	self;

    Terms1 ? -A,
    A = includable(false) : Includable = _,
      Includable' = false |
	self;

    otherwise,
    Terms1 ? -A,
    Includes =\= included(_) :
      Attributes ! A |
	self;

    otherwise,
    Terms1 ? -_,
    Includes = included(_) |
	self;

    Terms1 =\= [-_ | _] : Context = _,
    Includes = included(_),
    Includable = true,
      As = [],
      Attributes = Is,
      Terms1 = Terms2,
      Done = done,
      Outs = {O, O} ;

    Terms1 =\= [-_ | _],
    Includes = included(Name),
    Includable = false : Context = _, Terms1 = _, As = _, Is = _,
      Attributes = [],
      Terms2 = [],
      Done = done,
      Outs = {O, O'},
      O ! cant_include(Name) ;

    Terms1 =\= [-_ | _],
    Includes =\= included(_) : Context = _, Includable = _, As = _, Is = _,
      Attributes = [],
      Terms1 = Terms2,
      Done = done,
      Outs = {O, O} .

member(Member, Members, Reply) :-

    Members ? Member : Members' = _,
      Reply = old ;
      
    Members ? Other,
    Member =\= Other |
	self;

    Members = [] :
      Reply = new(Member) .


include_file(Terms1, Attributes, Terms2, Done, Outs,
		Context, Includes, Reply
) :-

    Reply = old |
	scan_attributes(Terms1, Attributes, Terms2, Done, Outs,
			Context, Includes
	);

    Reply = new(_),
    Context = computation |
	computation # service_id(Context'),
	include_file;

    Reply = new(Name),
    Context =\= computation,
    Outs = {O1, O2} :
      FileId = [Name | Context],
      Includes' = [FileId | Includes] |
	self # file(FileId, "_included", Result, {O1, O1'}),
	check_include(Result, Name, Terms1', Terms1,
			Terms2', Terms2, O1', O1''
	),
	scan_attributes(Terms1', Attributes, Terms2', Done, {O1'', O2},
			Context, Includes'
	).


check_include(Result, Name, Attributes1, Attributes2,
		Terms1, Terms2, O1, O2
) :-
/*
    Result = module([source(Absolute), date(Date)], As, Ts) |
	computation # comment((included : Absolute - Date)),
	self + (Result = module([], As, Ts));
*/
    Result = module(_, As, Ts),
    As ? I, I = include(_) :
      Attributes1 ! -I |
	self + (Result = module([], As', Ts));

    Result = module(_, As, Ts),
    As ? A, A =\= include(_) |
	self + (Result = module([], As', Ts));

    Result = module(_, [], Ts),
    Ts ? Term :
      Terms2 ! Term |
	self + (Result = module([], [], Ts'));

    Result = module(_, [], []) : Name = _,
      Attributes1 = Attributes2,
      Terms1 = Terms2,
      O1 = O2;

    otherwise : Result = _,
      Attributes1 = Attributes2,
      Terms1 = Terms2,
      O1 = [diagnostic(cant_include-Name) | O2] .


scan_options(Abort, Options, Replies) :-

    Options ? Option |
	set_option(Abort, Option, Replies, Replies'),
	scan_options;

    Options = [] : Abort = _,
      Replies = {[], computation, [], []} ;

    otherwise :
      Options' = [Options] |
	scan_options;

    Abort = abort : Options = _,
      Replies = {[], [], [], []} .


set_option(Abort, Option, Replies1, Replies2) :-

    Option = syntax(ModName),
    string(ModName) : Abort = _,
      Replies1 = {Syntax, Context, Included, Residue},
      Syntax ! Option,					% cumulate options
      Replies2 = {Syntax', Context, Included, Residue} ;

    Option = "_included" : Abort = _,
      Replies1 = {Syntax, Context, included, Residue},	% set flag
      Replies2 = {Syntax, Context, _Included, Residue} ;

    Option = service_id(ServiceId),
    ServiceId ? _  : Abort = _,
      Replies1 = {Syntax, ServiceId', Included, Residue},
      Residue ! Option,					% pass it along
      Replies2 = {Syntax, _Context, Included, Residue'} ;% prefer first

    otherwise : Abort = _,
      Replies1 = {Syntax, Context, Included, Residue},
      Residue ! Option,					% cumulate options
      Replies2 = {Syntax, Context, Included, Residue'} ;

    Abort = abort : Option = _,
      Replies1 = Replies2 .

