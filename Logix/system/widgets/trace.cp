-export([tree/2, profile/4, close/1]).
-mode(interrupt).
-language(compound).

tree(RPC, Tree) :-
    RPC = Path # Conjunction |
	widgets # vanilla # tree(Path, Conjunction, Tree, -1).

close(Tree) :-
	widgets # vanilla # tree_trace(Tree, _).

profile(Tree, Output, Summary, Suspended) :-
	tree_trace(Tree, Trace),
	trace_profile(Trace, Items, Summary, Suspended),
	binary_merge(Items, Out),
	output(Out, Output).

trace_profile(Trace, Items, Summary, Suspended)
 + (IdSs = [], Id = [], R = 0, C = -1, S = 0, F = 0, Tt = {0,0,0}) :-

   Trace ? begin(_TId, CH),
   C++ :
      IdSs' = [Id(R, S, C') | IdSs],
      write_channel(service_id(Id'), CH) |
	self;

   Trace ? end,
   IdSs ? Id'(R', S', C'),
   RO := R - R', SO := S -S', CO := C - C',
   Tt = {Rp, Sp, Cp},
   Rp += RO, Sp += SO, Cp += CO :
      Items ! {Id, {RO, SO, CO}},
      Tt' = {Rp', Sp', Cp'} |
	self;

   Trace ? reduced(Goal, _Clause, _Time),
   string(Goal),
   R++ :
      Items ! {{Id, Goal, 0}, {1, 0, 0}} |
	self;

   Trace ? reduced(Goal, _Clause, _Time),
   tuple(Goal), arg(1, Goal, Functor), N := arity(Goal),
   R++ :
      Items ! {{Id, Functor, N}, {1, 0, 0}} |
	self;

   Trace ? rpc(_),
   C++ |
	self;

   Trace ? suspended(Goal),
   string(Goal),
   S++ :
      Items ! {{Id, Goal, 0}, {0, 1, 0}},
      Suspended ! (Id # Goal) |
	self;

   Trace ? suspended(Goal),
   tuple(Goal), arg(1, Goal, Functor), N := arity(Goal),
   S++ :
      Items ! {{Id, Functor, N}, {0, 1, 0}},
      Suspended ! (Id # Goal) |
	self;

   Trace ? failed(Goal, _Time),
   string(Goal),
   F++ :
      Items ! {{Id, Goal, 0}, {0, 0, 1}} |
	self;

   Trace ? failed(Goal, _Time),
   tuple(Goal), arg(1, Goal, Functor), N := arity(Goal),
   F++ :
      Items ! {{Id, Functor, N}, {0, 0, 1}} |
	self;

   Trace ? Other,
   otherwise |
	computation # diagnostic(Other),
	self;

   Trace = [],
   Tt = {Rp, Sp, Cp} : IdSs = _, Id = _, R = _, S = _, C = _,
      Items = [],
      Suspended = [],
      Summary = [reductions = Rp, calls = Cp, suspended = Sp, failures = F] .

/*
	tree_trace(Tree, TreeTrace)

   TreeTrace is a flattened list, corresponding to a prefix walk of Tree:

     rpc(Path # Goal)               is a remote procedure call;
     begin(TreeId, Channel)...end   brackets the goals derived from a remote
                                    procedure call;
     reduced(Goal, Id, Time)        is a goal reduced by clause Id at Time;
     failed(Goal, Time)             is a goal which failed at Time;
     unknown(Goal)                  is an unexported goal;
     suspended(Goal)                is a suspended goal.

*/
      
Path ::= {`'#', Path, Path} ; Channel ; String.

Conjunction, Body ::= Goal ; Goal, Conjunction.
Goal ::= Tuple ; String.

Tree ::= tree(TreeId, Context, BranchList) ; failed(TreeId, Any).
TreeId ::= Path # Conjunction ; RPC.
RPC ::= {`"#", Any, Any}.
BranchList, BranchHead, BranchTail ::= [Node].
Node ::= reduce(Goal, CId, Time, BranchList) ; RPC ; Tree.
CId ::= Number.
Time ::= Number ; failed(abort, Number).

procedure tree_trace(Tree, [Track]).

tree_trace(Tree, TreeTrace) :-
	nodes([Tree], TreeTrace, []).

procedure nodes(Trace, TreeTrace, TreeTrace).

TreeTrace ::= [Flattened].
Flattened ::= begin(TreeId, Channel) ; end; rpc(RPC) ; Executed .
Executed  ::= reduced(Goal, Id, Time) ; failed(Goal, Id) ; suspended(Goal) .

nodes(Nodes, Head, Tail) :-

    Nodes ? tree(TreeId, Channel, BranchList) :
      Head ! begin(TreeId, Channel) |
	nodes(BranchList, Head', [end | Head'']),
	self;

    Nodes ? RPC, RPC = _#_ :
      Head ! rpc(RPC) |
	self;

    Nodes ? reduce(Goal, Id, Time, BranchList) :
      Head ! Executed |
	nodes(Nodes', Head', Head''),
	executed(Goal, Id, Time, Executed, BranchList, Nodes''),
	self;

    Nodes = [] :
      Head = Tail .

procedure executed(Goal, Id, Time, Executed, BranchList, BranchList).

executed(Goal, Id, Time, Executed, BranchList, Nodes) :-

    Time = failed(_, FailTime) : Id = _, BranchList = _,
      Executed = failed(Goal, FailTime),
      Nodes = [] ;

    Time = unknown(_, _Time) : Id = _, BranchList = _,
      Executed = unknown(Goal),
      Nodes = [] ;

    number(Time) :
      Executed = reduced(Goal, Id, Time),
      Nodes = BranchList ;

    unknown(Time) : Id = _, BranchList = _,
      Executed = suspended(Goal),
      Nodes = [] .

output(Out, Output) :-

    Out ? { Id, {Reductions, Suspensions, RPCs} },
    list(Id) :
      Output ! (Id : Reductions, Suspensions, RPCs) |
	output;

    Out ? { {Id, Functor, Arity--}, {Reductions, Suspensions, Failures} },
    Id = [Name | _] :
      Output ! (Name#Functor/Arity' : Reductions, Suspensions, Failures) |
	output;

    Out = [] :
      Output = [] .


ordered_merge(In1, In2, Out) :-

    In1 ? {Id, {R1, S1, CF1}},
    In2 ? {Id, {R2, S2, CF2}} :
      In1'' = [{Id, Cumulant} | In1'] |
	cumulate_data(R1, S1, CF1, R2, S2, CF2, Cumulant),
	self;

    In1 ? {Id1, Data1},
    In2 = [{Id2, _ } | _], Id1 @< Id2 :
      Out ! { Id1, Data1 } |
	self;

    In2 ? {Id2, Data2},
    In1 = [{Id1, _} | _], Id2 @< Id1 :
      Out ! {Id2, Data2} |
	self;

    In1 = [] :
      In2 = Out ;

    In2 = [] :
      In1 = Out .


cumulate_data(R1, S1, CF1, R2, S2, CF2, {Reductions, Suspensions, CFs}^) :-
    Reductions := R1 + R2,
    Suspensions := S1 + S2,
    CFs := CF1 + CF2 |
	true.


binary_merge(In, Out) :-
	level1_server(In, Odd, Even, PairList),
	binary_merger3(Odd, Even, PairList, Out).


level1_server(In, Odds, Evens, PairList) :-

    In ? Odd :
      Odds = [Odd] |
	level1_server(In', Evens, PairList);

    otherwise : In = _,
      Odds = [],
      Evens = [],
      PairList = [] .


level1_server(In, Evens, PairList) :-

    In ? Even :
      Evens = [Even],
      PairList ! {One, Two} |
	level1_server(In', One, Two, PairList');

    otherwise : In = _,
      Evens = [],
      PairList = [] .


binary_merger0(In1, In2, PairList, Out) :-
	ordered_merge(In1, In2, Out1),
	binary_merger2(PairList, Out2, UpList),
	binary_merger3(Out1, Out2, UpList, Out).


binary_merger1(PairList, UpList) :-

    PairList ? {In1, In2} :
      UpList ! {Out1, Out2} |
	ordered_merge(In1, In2, Out1),
	binary_merger2(PairList', Out2, UpList');

    PairList = [] :
      UpList = [] .


binary_merger2(PairList, Out2, UpList) :-

    PairList ? {In1,In2} |
	binary_merger1(PairList', UpList),
	ordered_merge(In1, In2, Out2);

    PairList = [] :
      UpList = [],
      Out2 = [] .


binary_merger3(Out1, Out2, UpList, Out) :-

    list(Out2) |
	binary_merger0(Out1, Out2, UpList, Out);

    Out2 = [] : UpList = _,
      Out1 = Out .
