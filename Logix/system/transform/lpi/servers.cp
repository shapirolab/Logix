/* $Header: /home/qiana/Repository/Logix/system/transform/lpi/servers.cp,v 1.1.1.1 1999/07/09 07:03:18 bill Exp $ */

/*
Services for lpi transformation - logic programs with inheritance.
Bill Silverman - November 1991
*/


-language(compound).
-export([diagnostics/2, dictionary/1, ids/3, terminal/1]).
-mode(trust).

/***
* Serve module dictionaries via channel.
***/

ids(IdC, ModuleName, Superid) :-
	make_channel(IdC, Ids),
	stream # merger(Ms, Ms'),
	make_channel(MsC, Ms),
	terminal(Dict),
	serve_module(Ids, ModuleName, Dict, MsC),
	dictionary(Sources),
	serve_main(Ms', Superid, Sources, MsC).

dictionary(In) :-

    In ? find(_, notfound^) |
	dictionary;

    In ? lookup(F, Value, new^) |
	entry(In', F, Value, In'', InR),
	dictionary,
	dictionary(InR);

    In = [] | true;

    In = names(L, R) :
      L = R ;

    In = ids(L, R) :
      L = R .

entry(In, F, Value, L, R) :-

    In ? lookup(F, Value', Reply) :
      Reply = old(Value) |
	entry;

    In ? find(F, Reply):
      Reply = found(Value) |
	entry;

    In ? Action, arg(2, Action, Other), Other @< F :
      L ! Action |
	entry;

    In ? Action, arg(2, Action, Other), F @< Other :
      R ! Action |
	entry;

    In = [] : F = _, Value = _,
      L = [],
      R = [] ;

    In = names(Ps, Ps''') : Value = _,
      Ps' ! F,
      L = names(Ps, Ps'),
      R = names(Ps'', Ps''') ;

    In = ids(Ps, Ps''') :
      Ps' ! F/Value,
      L = ids(Ps, Ps'),
      R = ids(Ps'', Ps''') .


terminal(In) :-

    In ? find(_, notfound^) |
	terminal;

    In ? lookup(F, N, notfound^) |
	node(In', F, [N], In'', InR),
	terminal,
	terminal(InR);

    In = [] | true;

    In = names(L, R) :
      L = R ;

    In = ids(L, R) :
      L = R .

node(In, F, Ns, L, R) :-

    In ? lookup(F, N, Reply) |
	search_values(N, Ns, Ns', Reply),
	node;

    In ? find(F, Reply),
      Ns = [_,_|_] :
      Reply = found(Ns) |
	node;

    In ? find(F, Reply),
      Ns = [S] :
      Reply = found(S) |
	node;

    In ? Action, arg(2, Action, Other), Other @< F :
      L ! Action |
	node;

    In ? Action, arg(2, Action, Other), F @< Other :
      R ! Action |
	node;

    In = [] : F = _, Ns = _,
      L = [],
      R = [] ;

    In = names(Ps, Ps''') : Ns = _,
      Ps' ! F,
      L = names(Ps, Ps'),
      R = names(Ps'', Ps''') ;

    In = ids(Ps, Ps''') :
      Ps' ! F/Ns,
      L = ids(Ps, Ps'),
      R = ids(Ps'', Ps''') .


search_values(N, Ns, Ts, Reply) :-

    Ns ? N : Ns' = _,
      Ts = Ns,
      Reply = found(N) ;

    Ns ? A, A =\= N :
      Ts ! A |
	search_values;

    Ns = [] :
      Ts = [N],
      Reply = notfound .


serve_module(Ids, ModuleName, Dict, MsC) :-

    Ids ? M, M = name(ModuleName^) |
	serve_module;

    Ids ? M, M = get_module(_,_,_,_) :
      write_channel(M, MsC, MsC') |
	serve_module;

    Ids ? M, M = lookup(_,_,_) :
      Dict ! M |
	serve_module;

    Ids ? M, M = find(_,_) :
      Dict ! M |
	serve_module;

    Ids = [] : ModuleName = _,
      Dict = [],
      close_channel(MsC) ;

    Ids ? NAMES, NAMES = names(_,_) : ModuleName = _, Ids' = _,
      Dict = NAMES,
      close_channel(MsC) ;

    Ids ? IDS, IDS = ids(_,_) : ModuleName = _, Ids' = _,
      Dict = IDS,
      close_channel(MsC) .


serve_main(Ms, SuperId, Sources, MsC) :-

    Ms ? get_module(Name, Status, Errors, Errors'), Name =\= super :
      Sources ! find(Name, Found) |
	get_module(Ms', SuperId, Sources', MsC, Found, Status, scope,
			[_ | SuperId], Name, Errors, Errors'
	);

    Ms ? get_module(Name, Status, Errors, Errors'), Name = super,
    SuperId = [SName | _] :
      Sources ! find(SName, Found) |
	get_module(Ms', SuperId, Sources', MsC, Found, Status, super,
			SuperId, SName, Errors, Errors'
	);

    Ms ? get_module(Name, false(no_super)^, Errors^, Errors), Name = super,
    SuperId = [] |
	serve_main;

    Ms = [] : SuperId = _,
      close_channel(MsC),
      Sources = [] .


get_module(Ms, SuperId, Sources, MsC, Found, Status, Kind,
		NextId, ServiceName, Errors1, Errors2
) :-

    Found = found(Status^) : Kind = _, NextId = _, ServiceName = _,
      Errors1 = Errors2 |
	serve_main(Ms, SuperId, Sources, MsC);

% Get source and parse it.
    Found = notfound,
    Kind = scope,
    NextId ? _ |
	file # execute_in_context(NextId', isdirectory(ServiceName, Reply)),
	get_source(Reply, ServiceName, NextId', Parsed, Errors1, Errors1'),
	next_service_id(ServiceName, NextId', NextId''),
	analyse_source(Parsed, Found', ServiceName, Kind, MsC,
			Sources, Sources', Errors1', Errors1''
	),
	get_module;
	
    Found = notfound,
    Kind = super,
    NextId ? SuperName |
	get_source(true, SuperName, NextId', Parsed, Errors1, Errors1'),
	analyse_source(Parsed, Found', SuperName, NextId', MsC,
			Sources, Sources', Errors1', Errors1''
	),
	get_module;

    Found = notfound,
    NextId = [] : Kind = _, ServiceName = _,
      Errors1 = Errors2,
      Status = false(no_module) |
	serve_main(Ms, SuperId, Sources, MsC);

    Found = notfound,
    NextId =\= [_|_], NextId =\= [] |	% hierarchy_server calls get_module
					% with [Name | context(SuperServer)]
	NextId # service_id(NextId'),
	get_module;

    Found =\= found(_),
    Found =\= notfound : Kind = _, NextId = _, ServiceName = _,
      Errors1 = Errors2,
      Status = Found |
	serve_main(Ms, SuperId, Sources, MsC).

next_service_id(ServiceName, LastId, NextId) :-

    LastId ? _ :
      NextId = [ServiceName | LastId'] ;

    LastId =\= [_|_], LastId =\= [] |	% hierarchy_server calls get_module
					% with [Name | context(SuperServer)]
	LastId # service_id(LastId'),
	next_service_id;

    LastId = [] : ServiceName = _,
      NextId = [].


get_source(Reply, Name, SuperId, Parsed, Errors1, Errors1') :-

    Reply = true :	% Is director - get self text.
      Errors1 ! errors(get_source(Name#self), Errors) |
	get_source#context([Name | SuperId], self, [], Parsed, Errors([]));

    Reply = false(_) :	% Not director - get Named text.
      Errors1 ! errors(get_source(Name), Errors) |
	get_source # context(SuperId, Name, [], Parsed, Errors([])).


analyse_source(Parsed, Reply, Name, Scope, MsC, Sources, Sources',
		Errors, Errors'
) :-

    Parsed = module(Options, Attributes, Clauses) : Scope = _,
      Errors = [name(Name) | Nested],
      Reply = found(Status?),
      Sources ! lookup(Name, Status?, _new),
      write_channel(merge(Ns), MsC) |
	make_channel(NsC, Ns),
	terminal(Dict),
	serve_module(Ids?, Name, Dict, NsC),
	transform # language_names(Options, lpi, Attributes, Languages),
	languages_through_lpi(Languages, List),
	transform(List, Options, Attributes, Clauses, Clauses',
			Nested, Nested'
	),
	remote_clauses(Attributes, Clauses', Ids, Status,
			Nested'([name | Errors'])
	);

    Parsed = false(error(not_found)),
    Scope =\= [_|_] : Name = _, MsC = _,
      Reply = notfound,			% no module or root scope
      Errors = Errors',
      Sources' = Sources;

    Parsed = false(error(not_found)),
    Scope ? Name' : Name = _, Scope' = _, MsC = _,
      Errors = Errors',			% try next outer scope
      Sources ! find(Name', Reply);

    Parsed = false(Other),
    Other =\= error(not_found) : Name = _, Scope = _, MsC = _,
      Reply = Parsed,
      Errors ! diagnostic(Other - "accessing module" - Name),
      Sources' = Sources .

languages_through_lpi(Languages, List) :-

    Languages ? L, L =\= compound, L =\= dfcp :
      List ! L |
	self;

    otherwise : Languages = _,
      List = [lpi] .
	

transform(List, Options, Attributes, Clauses1, Clauses2, Errors1, Errors2) :-

    List = [lpi] : Options = _, Attributes = _,
      Clauses2 = Clauses1,
      Errors1 = Errors2 ;

    otherwise |
	transform # languages([language(List) | Options], lpi, Attributes, _,
				Clauses1, Clauses2, Errors1, Errors2).


/***
* Analyse the new source.
***/

remote_clauses(Attributes, Clauses, Ids, Status, Diags) :-

    true :
      Status = module(Estate?, Exports?),
      make_channel(IdC, Ids) |
	list_ids(Signatures?, ProcIds),
	remote_exports(Attributes, ProcIds, Exports),
	module # clauses(Clauses, _Terms, Estate(_Rscs), IdC, IdZ, Diags),
	write_channel(ids(Signatures, []), IdZ).

remote_exports(Attributes, Ids, Exports) :-

    Attributes ? export(List), list(List) : Attributes' = _, Ids = _,
      Exports = List ;

    Attributes ? export(all) : Attributes' = _ |
      Exports = Ids ;

    Attributes ? _,
    otherwise |
	remote_exports;

    Attributes = [] |
      Exports = Ids .

list_ids(Ids, List) :-

    Ids ? F/L, L ? N-- :
      Ids'' = [F/L' | Ids'],
      List ! F/N' |
	list_ids;

    Ids ? _/[] |
	list_ids;

    Ids = [] :
      List = [].

/***
* Serve diagnostic requests and messages.
***/

diagnostics(Diagnostics, Errors) :-
	terminal(Lookup),
	diags(Diagnostics, Errors, [], [], [], [], Lookup).

diags(Diags, Errors, Name, Id, NameStack, IdStack, Lookup) :-

    Diags ? name(Name') :
      NameStack' = [Name | NameStack] |
	diags;

    Diags ? name,
    NameStack ? Name' : Name = _ |
	diags;

    Diags ? id(Atom),
    arg(1, Atom, F),
    N := arity(Atom) - 1 :
      IdStack' = [Id | IdStack],
      Id' = F/N |
	diags;

    Diags ? id,
    IdStack ? Id' : Id = _ |
	diags;
 
    Diags ? errors(Kind, Es),
    Es ? Error, Error =\= (_:_) :
      Diags'' = [errors(Kind, Es') | Diags'],
      Lookup ! lookup(Error, Kind, Found) |
	duplicate_diagnostic(Found, Kind - Error, Errors, Errors'),
	diags;

    Diags ? errors(Kind, Es),
    Es ? (Prefix : Comment) :
      Note = (Kind > Prefix : Comment),
      Diags'' = [errors(Kind, Es') | Diags'],
      Lookup ! lookup(Note, note, Found) |
	duplicate_diagnostic(Found, Note, Errors, Errors'),
	diags;

    Diags ? errors(_Kind, []) |
	diags;

    Diags ? diagnostic(Diagnostic) :
      Lookup ! lookup(Formatted?, [], Found) |
	format_diagnostic(Name, Id, Diagnostic, Formatted),
	duplicate_diagnostic(Found, Formatted, Errors, Errors'),
	diags;

    Diags ? diagnostic(Atom, Diagnostic),
    arg(1, Atom, F),
    N := arity(Atom) - 1 :
      Lookup ! lookup(Formatted?, [], Found) |
	format_diagnostic(Name, F/N, Diagnostic, Formatted),
	duplicate_diagnostic(Found, Formatted, Errors, Errors'),
	diags;
	
    Diags ? Other,
    otherwise :		% debugging aid
      Errors ! Name(Id, Other) |
	diags;

    Diags = [] : Name = _, Id = _, NameStack = _, IdStack = _,
      Lookup = [],
      Errors = [] .

format_diagnostic(Name, Id, Diagnostic, Formatted) :-

    Name = [], Id = [] :
      Formatted = Diagnostic ;

    Name = [], Id =\= [] :
      Formatted = Id > Diagnostic ;

    otherwise :
    Formatted = Name # Id > Diagnostic .


duplicate_diagnostic(Found, Diagnostic, Errors, Errors') :-

    Found = notfound :
      Errors ! Diagnostic ;

    otherwise : Found = _, Diagnostic = _,
      Errors = Errors' .
