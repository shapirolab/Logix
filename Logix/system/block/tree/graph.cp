/* $Header: /home/qiana/Repository/Logix/system/block/tree/graph.cp,v 1.4 2004/06/22 12:00:52 bill Exp $ */
/*
 *  Transform the hierarchical tree into a graph.
 */

-export([build/4, format_rpc/3, goal/4, rpc/5, cumulate_entries/1]).
-language(compound).
%-mode(trust).

Tree ::= hierarchy # Tree.
Trees ::= [Tree].
Graph ::= SubGraph ; [].
DirGraph ::= {ServiceId, Lead, Graph, EntriesTuple, State, SubGraphs}.
SubGraph ::= {ServiceId, Lead, Graph, EntriesTuple, State, (SubGraphs;server)}.
SubGraphs ::= [SubGraph].
State ::= ProcIds ; monitor ; system ; excluded .
ProcIds ::= [String/Integer].
EntriesTuple ::= Entries(Entries).
Entries ::= [(String ; String/Integer)].
SubAnswer ::= found(SubGraph) ; not_found(Name).


procedure build(ServiceId, ServiceId, hierarchy # RootTree, SubGraph).

build(ScopeId, RootId, Tree, Root) :-
    Tree = {[], Servers, SubTrees} :
      Super = [] |
	scope_lead(ScopeId, RootId, "$", DirLead, Graph, _Root),
	director.

scope_lead(ScopeId, RootId, DirLead, SelfLead, Graph, Root) :-

    ScopeId = RootId : DirLead = _,
      SelfLead = "",
      Root = Graph ;

    ScopeId =\= RootId : Graph = _, Root = _,
      SelfLead = DirLead .


procedure director(ServiceId, ServiceId, Lead, Graph, Nodes, Trees,
			DirGraph, SubGraph).

director(ScopeId, RootId, DirLead, Super, Servers, SubTrees, Graph, Root) :-

    Servers ? self :
      Graph = {ScopeId, SelfLead?, Super, Entries?, _Status, SubGraphs?},
      Super' = Graph |
	scope_lead,
	director_entries,
	servers,
	subgraphs;

    otherwise :
      Graph = {ScopeId, DirLead, Super, _([]), excluded, SubGraphs?},
      Super' = Graph |
	servers,
	subgraphs.

procedure director_entries(Lead, Entries).

director_entries(SelfLead, Entries) :-

    SelfLead = "" :
      Entries = _CumulatedEntries(_NodeEntries);

    SelfLead =\= "" :
      Entries = {[], []}.


procedure servers(ServiceId, Lead, Graph, Nodes, SubGraphs).

servers(ScopeId, DirLead, Super, Servers, Modules) :-

    Servers ? Server :
      Modules ! {[Server | ScopeId], SLead, Super, _, _, server} |
	self,
	auxils # server_lead(DirLead, Server, SLead);

    Servers = [] : ScopeId = _, DirLead = _, Super = _,
      Modules = [] .


procedure subgraphs(ServiceId, DirLead, Graph, DirGraph, Trees,
		    SubGraphs, SubGraphs, DirGraph
).

subgraphs(ScopeId, RootId, DirLead, Graph, SubTrees, SubGraphs,
	  Modules, Root
) :-

    SubTrees ? {[Name | _], Servers, SubSubs} :
      SubGraphs ! SubGraph |
	auxils # server_lead(DirLead, Name, DLead),
	director([Name | ScopeId], RootId, DLead, Graph,
		 Servers, SubSubs, SubGraph, Root
	),
	self;

    SubTrees = [] : ScopeId = _, RootId = _, DirLead = _, Graph = _, Root = _,
/**** Directors must precede modules in search order. ****/
      SubGraphs = Modules .

/****************************** G O A L / 4 **********************************/

GoalReply ::= done ; load(ServiceId, EntriesTuple, State, FullCalls).
FullCalls ::= [ calls(Lead, String, GCalls, GCalls) | Calls].
RpcReply ::= done ; call(GCall).
Reply ::= GoalReply ; RpcReply.

procedure goal(Any, Graph, Any, GoalReply).

goal(Call, Graph, Goal, Reply) :-

    Graph = {_, Lead, _, _, ProcIds, _},
    ProcIds =\= excluded, ProcIds =\= monitor :
      Reply = done |					% Eureka!
	rename # goal(Call, Lead, ProcIds, Goal);	% rename the call!

    Graph = {_, _, Graph', _, NotCallable, Subs},
    string(NotCallable),				% "excluded" or
    Subs =\= server,					% "monitor"
    Graph' =\= [] |					% imbedded self
	self;						% delegate goal

    otherwise :
      Reply = done |
	format_rpc;

    Graph = {ScopeId, Lead, Super, EntriesTuple, State, Subs},
    writable(State) : Reply' = _,
      State = Locked? |
	Reply = load(NodeId, EntriesTuple, Locked,
		     [calls(Lead, SPath, Cs1, Cs2) | Calls]),
	source_service_id(Subs, ScopeId, NodeId),
	subgraph_path(ScopeId, Subs, Super, SPath),
	calls(Calls, Graph, Cs1, Cs2),
	self.					% after load, retry goal

procedure source_service_id(SubGraphs, ServiceId, NodeId).

source_service_id(Subs, ScopeId, NodeId) :-

    Subs = server :
      ScopeId = NodeId ;

    otherwise : Subs = _,
      NodeId = [self | ScopeId].

procedure subgraph_path(ServiceId, SubGraphs, Graph, Path).

subgraph_path(ScopeId, Subs, Super, Path) :-

    Subs = server,
    ScopeId ? Name |
	server_path(ScopeId', Super, Path, Name);

    Subs =\= server,
    ScopeId ? Name |
	server_path(ScopeId', Super, Path, Name # self);

    otherwise : ScopeId = _, Subs = _, Super = _,	% special case - both nil
      Path = ''	.				% <==> entire hierarchy

procedure server_path(ServiceId, Graph, Path, Path).

server_path(ScopeId, Super, Path, Partial) :-

    Super = {_, _, Super', _, _, _},
    ScopeId ? Name :
      Partial' = Name # Partial |
	server_path;

    Super = [] : ScopeId = _,
      Path = Partial .

procedure calls(Calls, Graph, GCalls, GCalls).
procedure calls(Calls, Graph, GCalls, GCalls, GCalls, GCalls).

calls(Calls, Graph, Calls1, Calls2) + (Cs1 = Cs, Cs2 = Cs) :-

    Calls ? call(RPC, Goal) :
      Cs2 ! {RPC, Graph, Goal} |
	calls;

    Calls = [],
    Graph = {_, _, _, _, excluded, _} : Cs1 = _, Cs2 = _,
      Calls1 = Calls2 ;

    Calls = [],
    otherwise : Graph = _,
      Calls1 = Cs1,
      Calls2 = Cs2 .

/****************************** R P C / 5 ************************************/

procedure rpc(Any, Graph, Any, RpcReply, Any).

rpc(Call, Graph, Goal, Reply, Service) :-

    Service = self :
      Reply = call({Call, Graph, Goal}) ;

    Service = super,
    Graph = {_, _, Super, _, _, _} |
	super_goal(Call, Super, Goal, Reply) ;

    Service =\= super, Service =\= self,
    Graph = {_, _, Super, _, _, Subs} |
	search_subs(Service, Subs, Answer),
	found_sub(Call, Graph, Goal, Reply, Super, Answer).

procedure super_goal(Any, Graph, Any, RpcReply).

super_goal(Call, Graph, Goal, Reply) :-

    Graph = [] :
      Goal = super # Call,
      Reply = done ;

    otherwise :
      Reply = call({Call, Graph, Goal}) .

procedure search_subs(Any, SubGraphs, SubAnswer).

search_subs(Service, Subs, Answer) :-

    Subs ? Sub,
    arg(1, Sub, [Service | _]) : Subs' = _,
      Answer = found(Sub) ;

    Subs ? _,
    otherwise |
	search_subs;

    Subs =\= [_ | _] : 
      Answer = not_found(Service) .

procedure found_sub(Any, Graph, Any, RpcReply, Graph, SubAnswer).

found_sub(Call, Self, Goal, Reply, Super, Answer) :-

    Answer = found(Sub) : Super = _, Self = _,
      Reply = call({Call, Sub, Goal}) ;

    Answer = not_found(Service),
    Super =\= [] : Self = _ |
	rpc(Call, Super, Goal, Reply, Service) ;	% delegate rpc

    Answer = not_found(Service),
    Super = [],
    arg(1, Self, [Service | _]) :			% special case for root
      Reply = call({Call, Self, Goal}) ;

    Answer = not_found(Service),
    otherwise : Super = _, Self = _,
      Goal = Service # Call,
      Reply = done .

procedure format_rpc(Any, Graph, Any).

format_rpc(Call, Graph, Goal) :-

    Graph = {[Name | _], _, Graph', _, _, _},
    Graph' =\= [] :
      Call' = Name # Call |
	format_rpc;

    arg(3, Graph, []),
    Call = _#_ :
      Goal = Call ;

    Graph = {[Name | _], _, [], _, _, _},	% remove this clause if the
    Call =\= _#_ :				% block is to be anonymous!
      Goal = Name # Call ;

    otherwise : Graph = _ |			% The root is anonymous!
      Goal = self # Call .


cumulate_entries(Graph)  :-

    Graph = {_, "", _, EntriesTuple, _, SubGraphs} :
      EntriesTuple = Entries(_) |
	cumulate_sub_entries(SubGraphs, Entries, []);

    otherwise :
      Graph = _ .
	

cumulate_graph_entries(Graph, Entries, NextEntries) :-

    Graph = {_, Lead, _, EntriesTuple, _, SubGraphs} :
      EntriesTuple = _(NodeEntries) |
	cumulate_entries(Lead, NodeEntries, Entries, Entries'),
	cumulate_sub_entries;

    Graph = [] :
      Entries = NextEntries;

    otherwise :
      Entries = NextEntries |
	fail((graph = Graph)).

  cumulate_entries(Lead, NodeEntries, Entries, NextEntries) :-

    NodeEntries ? Entry,
    string_to_dlist(Lead, LL,LT) :
      Entries ! LeadEntry |
	complete_entry_name,
	self;

    NodeEntries = [] :
      Lead = _,
      Entries = NextEntries;

    NodeEntries =\= [_|_], NodeEntries =\= [] :
      NodeEntries' = [NodeEntries] |
	self;

    we(NodeEntries) :
      NodeEntries = [] |
	self.

  complete_entry_name(LL, LT, Entry, LeadEntry) :-

    string(Entry),
    string_to_dlist(Entry, EL, []) :
      LT = EL |
	list_to_string(LL, LeadEntry);

    Entry = Entry'/Arity :
      LeadEntry = LeadEntry'/Arity |
	self;

    otherwise :
      Entry' = "" |
	fail(invalid_entry_name(Entry)),
	self.

  cumulate_sub_entries(SubGraphs, Entries, NextEntries) :-

    SubGraphs ? Graph |
	cumulate_graph_entries(Graph, Entries, Entries'),
	self;

    SubGraphs =\= [_|_] :
      Entries = NextEntries.
