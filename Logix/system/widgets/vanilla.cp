/* $Header: /home/qiana/Repository/Logix/system/widgets/vanilla.cp,v 1.1.1.1 1999/07/09 07:03:23 bill Exp $ */
/*
   Meta-interpret a conjunction, producing either an execution tree or a
   trace of executed goals.

	vanilla # tree(Path, Conjunction, Tree)
	vanilla # tree(Path, Conjunction, Tree, Depth)
	vanilla # trace(Path, Conjunction, Trace)
	vanilla # trace(Path, Conjunction, Trace, Depth)

   Tree includes an open Channel to each module which is called (one for
   each separate call to the module). It may include uninstantiated bodies.

   Trace includes a static reference for each RPC.  The tail of Trace is
   uninstantiated if some goal has not yet been reduced.

   Depth limits the depth of remote procedure calls; e.g. Depth = 0 implies
   only the local execution tree/trace of the Conjunction is recorded, while
   Depth = -1 is effectively infinite.


   Flatten an execution tree, closing all Context references:

	vanilla # tree_trace(Tree, TreeTrace)

   TreeTrace is a flattened list, corresponding to a prefix walk of Tree:

     rpc(Path # Goal)               is a remote procedure call;
     begin(TreeID) ... end(TreeID)  brackets the goals derived from a remote
                                    procedure call;
     reduced(Goal, Id, Time)        is a goal reduced by clause Id at Time;
     failed(Goal, Time)             is a goal which failed at Time;
     unknown(Goal)                  is an unexported goal;
     suspended(Goal)                is a suspended goal.

*/

-export([tree/3, tree/4, trace/3, trace/4, tree_trace/2]).
-mode(interrupt).
-language(compound).

Path ::= {`'#', Path, Path} ; Channel ; String.

Conjunction, Body ::= Goal ; Goal, Conjunction.
Goal ::= Tuple ; String.

Tree ::= tree(TreeId, Context, BranchList) ; failed(TreeId, Any).
TreeId ::= Path # Conjunction ; RPC.
RPC ::= {`"#", Any, Any}.
BranchList, BranchHead, BranchTail ::= [Node].
Node ::= reduce(Goal, Id, Time, BranchList) ; RPC ; Tree.
Id ::= Number.
Time ::= Number ; failed(abort, Number).

Depth ::= Integer.


procedure tree(Path, Conjunction, Tree).
procedure tree(Path, Conjunction, Tree, Depth).

tree(Path, Conjunction, Tree) + (Depth = 0) :-
	computation_utils # path_context(Path, Channel, Ok),
	context_tree(Ok, Path # Conjunction, Channel, Conjunction, Tree,
			Depth
	).

procedure context_tree(SystemReply, TreeId, Channel, Body, Tree, Depth).

context_tree(SystemReply, TreeId, Channel, Body, Tree, Depth) :-

    SystemReply = true :
      Tree = tree(TreeId, Channel, BranchList) |
	reduce_tree(Channel, Body, BranchList, [], Depth, 0);

    SystemReply = false(Reason) : Channel = _, Body = _, Depth = _,
      Tree = failed(TreeId, Reason) .

procedure reduce_tree(Channel, Body, BranchHead, BranchTail, Depth, Time).

reduce_tree(Channel, Body, BranchHead, BranchTail, Depth, Time) :-

    number(Time),
    Body = (Goal, Body') |
	reduce_tree(Channel, Goal, BranchHead, BranchHead', Depth, Time),
	reduce_tree;

    number(Time),
    Body = true : Channel = _, Depth = _,
      BranchHead = BranchTail ;

    number(Time),
    Body = Name#_,
    Name =\= computation, Name =\= processor,
    Depth =\= 0,
    Depth' := Depth - 1 :
      BranchHead = [Tree | BranchTail] |
	computation_utils #
		call_context_goal(Channel # Body, Channel', Goal, Ok),
	context_tree(Ok, Body, Channel', Goal, Tree, Depth');

    number(Time),
    Body = Body' @ _ |
	reduce_tree;

    number(Time),
    Body ? Goal |
	reduce_tree(Channel, Goal, BranchHead, BranchHead', Depth, Time),
	reduce_tree;

    number(Time),
    Body = [] : Channel = _, Depth = _,
      BranchHead = BranchTail ;

    number(Time),
    Body =\= (_, _), Body =\= [_ | _], Body =\= (_ # _), Body =\= (_ @ _),
    Body =\= true, Body =\= [] :
      BranchHead = [reduce(Body, Id, Time', BranchHead') | BranchTail],
      BranchTail' = [],
      write_channel(clause(Body, Body', Id, Time'), Channel) |
	reduce_tree;

    tuple(Time) : Channel = _, Body = _, Depth = _,
      BranchHead = BranchTail ;

    number(Time),
    Body = _#_,
    otherwise : Depth = _,
      BranchHead = [Body | BranchTail],
      write_channel(Body, Channel) .


Trace, TraceHead, TraceTail ::= [Goal].
Head, Tail ::= Channel(Trace).
TraceId ::= TreeId.

procedure trace(Path, Conjunction, Trace).
procedure trace(Path, Conjunction, Trace, Depth).

trace(Path, Conjunction, Trace) + (Depth = 0) :-
	computation_utils # path_context(Path, Channel, Ok),
	context_trace(Ok, Path # Conjunction, Channel, Conjunction, Trace, [],
			Depth
	).

procedure context_trace(SystemReply, TraceId, Channel, Body,
			TraceHead, TraceTail, Depth
).

context_trace(SystemReply, TraceId, Channel, Body,
		TraceHead, TraceTail, Depth
) :-

    SystemReply = true : TraceId = _ |
	reduce_trace(Channel, Body, Channel(TraceHead), Channel'(TraceTail),
			Depth
	),
	close_channel(Channel');

    SystemReply = false(Reason) : Channel = _, Body = _, Depth = _,
      TraceHead = [failed(TraceId, Reason) | TraceTail] .

procedure reduce_trace(Channel, Body, Head, Tail, Depth).

reduce_trace(Channel, Body, Head, Tail, Depth) :-

    Body = (Goal, Body') |
	reduce_trace(Channel, Goal, Head, Head', Depth),
	reduce_trace;

    Body = true : Channel = _, Depth = _,
      Head = Tail ;

    Body = Name#_,
    Depth =\= 0,
    Name =\= computation, Name =\= processor,
    Depth' := Depth - 1 :
      Head = LC([Body | Left]),
      Tail = RC(Right) |
	computation_utils #
		call_context_goal(Channel # Body, Channel', Goal, Ok),
	close_circuit(Ok, LC, RC),
	context_trace(Ok, Body, Channel', Goal, Left, Right, Depth');

    Body = _#_,
    otherwise : Depth = _,
      Head = LC([Body | Right]),
      Tail = LC(Right),
      write_channel(Body, Channel) ;

    Body = Body' @ _ |
	reduce_trace;

    Body ? Goal |
	reduce_trace(Channel, Goal, Head, Head', Depth),
	reduce_trace;

    Body = [] : Channel = _, Depth = _,
      Head = Tail ;

    otherwise :
      Head = LC([Body | Left]), Head' = LC(Left),
	write_channel(clause(Body, Body'), Channel) |
	reduce_trace.

close_circuit(Ok, Left, Right) :-
    known(Ok) :
      Right = Left .


procedure tree_trace(Tree, [Track]).

tree_trace(Tree, TreeTrace) :-
	nodes([Tree], TreeTrace, []).

procedure nodes(Trace, TreeTrace, TreeTrace).

TreeTrace ::= [Flattened].
Flattened ::= begin(TreeId) ; end(TreeId) ; rpc(RPC) ; Reduced .
Executed  ::= reduced(Goal, Id, Time) ; failed(Goal, Id) ; suspended(Goal) .

nodes(Nodes, Head, Tail) :-

    Nodes ? tree(TreeId, Context, BranchList) :
      close_channel(Context),
      Head ! begin(TreeId) |
	nodes(BranchList, Head', [end(TreeId) | Head'']),
	nodes;

    Nodes ? RPC, RPC = _#_ :
      Head ! rpc(RPC) |
	nodes;

    Nodes ? reduce(Goal, Id, Time, BranchList) :
      Head ! Executed |
	nodes(Nodes', Head', Head''),
	executed(Goal, Id, Time, Executed, BranchList, Nodes''),
	nodes;

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
