/*$Header: /home/qiana/Repository/Logix/system/block/tree/self.cp,v 1.3 2002/06/05 18:33:00 bill Exp $ */
/*
 *  Block compile  Tree  at  RootId , producing  Blocked  and  Report .
 */

-export([load/5]).
-language(compound).
%-mode(trust).


Graph ::=  graph # Graph.
EntriesTuple ::= graph # EntriesTuple.
Controls ::= {GCalls, BlockedSource, Report}.
ServiceKind ::= procedures ; monitor ; excluded.
GCall ::= {Any, Graph, Any}.
GCalls ::= [GCall].
Call ::= call(Any, Any).

procedure load(ServiceId, ServiceId, hierarchy # RootTree,
		BlockedSource, Report
).

load(ScopeId, RootId, Tree, Blocked, Report) :-

    Tree =\= false(_), Tree =\= {_, [], []} |
	graph # build(ScopeId, RootId, Tree, Graph),
	GCalls ! {'_', Graph, _},
	serve_calls(GCalls, {GCalls', Blocked, Report}, Graph);

    Tree = {_, [], []} : ScopeId = _, RootId = _,
      Blocked = [],
      Report = [empty_block] ;

    otherwise : ScopeId = _, RootId = _,
      Blocked = [],
      Report = [] |
	computation # event(Tree) .


procedure serve_calls(GCalls, Controls, Graph).

serve_calls(GCalls, Controls, Root) :-

    GCalls ? {RemoteGoal, Graph, Goal},
    tuple(RemoteGoal),
    arg(1, RemoteGoal, Functor), string(Functor),
    RemoteGoal =\= service_id(_),
    RemoteGoal =\= "_unique_id"(_),
    RemoteGoal =\= clause(_, _),
    RemoteGoal =\= clause(_, _, _),
    RemoteGoal =\= clause(_, _, _, _),
    RemoteGoal =\= `_,
    RemoteGoal =\= ?_,
    RemoteGoal =\= _#_ |
	graph # goal(RemoteGoal, Graph, Goal, Reply),
	graph_reply(Reply, GCalls', GCalls'', Controls, Controls'),
	serve_calls;

    GCalls ? {Service # RemoteGoal, Graph, Goal},
    (Service # RemoteGoal) =\= (self # service_id(_)) |
	graph # rpc(RemoteGoal, Graph, Goal, Reply, Service),
	graph_reply(Reply, GCalls', GCalls'', Controls, Controls'),
	serve_calls;

    GCalls ? {Call, _Graph, Goal},
    Call =?= (self # service_id(_)) :
      Goal = Call |
	serve_calls;

    GCalls ? {RemoteGoal, Graph, Goal},
    string(RemoteGoal) |
	graph # goal(RemoteGoal, Graph, Goal, Reply),
	graph_reply(Reply, GCalls', GCalls'', Controls, Controls'),
	serve_calls;

    GCalls ? {RemoteGoal, Graph, Goal},		% This includes variables,
    otherwise |					% variable functors and lists
	graph # format_rpc(RemoteGoal, Graph, Goal),
	serve_calls;

    Controls = {GCalls, Blocked, Report} :	% End of List <-- Head = Tail
      GCalls = [],				% (not strictly necessary)
      Blocked = [],
      Report = [] |
	graph # cumulate_entries(Root).


procedure graph_reply(graph # Reply, Calls, Calls, Controls, Controls).

graph_reply(Reply, GCalls1, GCalls2, Controls1, Controls2) :-

    Reply = done :
      GCalls1 = GCalls2,
      Controls1 = Controls2 ;

    Reply = call(GCall) :
      GCalls2 = [GCall | GCalls1],			% push simplified rpc
      Controls1 = Controls2 ;

    Reply = load(SourceId, EntriesTuple, State, Calls),
    Controls1 = {QueueCalls, Blocked, Report} :
      GCalls1 = GCalls2,
      Calls ! calls(Lead, Path, QueueCalls, QueueCalls'),
      Controls2 = {QueueCalls', Blocked'', Report'} |
	parser # parse(SourceId, Lead, Clauses, ProcIds, Attributes, Result),
	auxils # member(monitor(_), Attributes, Monitor),
	service_included(Lead, Monitor, Path, Result, Kind, Report, Report'),
	save_attributes(Lead, Kind, Attributes, EntriesTuple,
			Clauses, Clauses', Blocked, Blocked'
	),
	rename # clauses(Clauses', Lead, ProcIds, Blocked', Blocked'', Calls'),
	service_state(Kind, ProcIds, State).


procedure service_included(String, Answer, Result, Result, ServiceKind).

service_included(Lead, Monitor, Path, Result, Kind, Report, Next) :-

    Result = included,
    Monitor = false : Lead = _,
      Report = [comment((includes : Path)) | Next],
      Kind = procedures ;

    Result = included,
    Monitor = true,
    Lead = '' :
      Report = [comment((includes : Path)) | Next],
      Kind = monitor ;

    Result = included,
    Monitor = true,
    Lead =\= '' :
      Report = [comment((excluded - monitor : Path)) | Next],
      Kind = excluded ;

    Result = (_-_) : Monitor = _, Lead = _,
      Report = [(Path > Result) | Next],
      Kind = excluded ;

    Result = _Diagnostic(Reports),
    Reports ? Error, Error =\= comment(_) :
      Report ! diagnostic((Path : Error)),
      Result' = excluded(Reports') |
	self;

    Result = Diagnostic(Reports),
    Reports ? comment(Comment) :
      Report ! comment((Path : Comment)),
      Result' = Diagnostic(Reports') |
	self;

    Result = Diagnostic([]),
    Diagnostic =\= excluded :
      Result' = included |
	self;

    Result = excluded([]) : Monitor = _,  Lead = _, Path = _,
      Report = Next,
      Kind = excluded .


procedure service_state(ServiceKind, ProcIds, graph # State).

service_state(Kind, ProcIds, State) :-

    Kind = procedures :
      ProcIds = State ;

    otherwise : ProcIds = _,
      Kind = State .


procedure save_attributes(String, ServiceKind, Attributes, EntriesTuple,
			  Clauses, Clauses, BlockedSource, BlockedSource
).

% If the server is the root's self (Lead = ''), the significant
% Attributes  are added to the beginning of  Block1 .
% The source (Clauses) of an excluded service is elided.

save_attributes(Lead, Kind, Attributes, EntriesTuple, Clauses1, Clauses2,
		Blocked1, Blocked2
) :-

    Kind =\= excluded,
    Lead = '' :
      EntriesTuple = CumulatedEntries(NodeEntries?),
      Blocked1 = [-export(Exports), -entries(CumulatedEntries?), -mode(Mode?)
		 | Blocked1'],
      Clauses1 = Clauses2 |
	server_entries,
	extract_root_attributes(Attributes, Exports, Mode,
				Blocked1', Blocked2
	);

    Kind = procedures,
    Lead =\= '' : Attributes = _,
      EntriesTuple = _Unused(NodeEntries),
      Clauses1 = Clauses2,
      Blocked1 = Blocked2 |
	server_entries;

    otherwise : Kind = _, Lead = _, Attributes = _, Clauses1 = _,
      EntriesTuple = _Unused(NodeEntries),
      Clauses2 = [],
      NodeEntries = [],
      Blocked1 = Blocked2 .

server_entries(Attributes, NodeEntries) :-

    Attributes ? entries(E) :
      Attributes' = _,
      NodeEntries = E;

    Attributes ? Other,
    Other =\= entries(_) |
	self;

    Attributes = [] :
      NodeEntries = [] .

procedure extract_root_attributes(Attributes, Any, Any,
				BlockedSource, BlockedSource
).

% Return in  Exports  the export-attribute of  Attributes  if
% any, and "all" otherwise.
% Returns in  Mode  the mode-attribute of  Attributes , if any,
% and "interrupt" otherwise.
% Adds other significant attributes to  Blocked1 .

extract_root_attributes(Attributes, Exports, Mode, Blocked1, Blocked2) :-

	Attributes ? export(Exports^) : Exports' = _ |
		extract_root_attributes;

	Attributes ? mode(Mode^) : Mode' = _ |
		extract_root_attributes;

        Attributes ? Monitor,
        Monitor = monitor(_) :
	    Blocked1 ! -Monitor |
		extract_root_attributes;

	Attributes ? block_prefix(_) |
		extract_root_attributes;

	Attributes ? Other,
	Other =\= export(_), Other =\= mode(_), Other =\= monitor(_),
	Other =\= block_prefix(_) |
		extract_root_attributes.

extract_root_attributes([], all^, interrupt^, Blocked, Blocked^).
