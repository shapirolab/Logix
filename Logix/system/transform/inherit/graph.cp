/*

FCP Pre-Compiler - Find procedure lists.

William Silverman - 11/85

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:19 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/system/transform/inherit/graph.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([imports/2,lists/4]).
-mode(trust).
-language(compound).

Declarations ::= [procedure(GoalId, GoalIds)].
GoalId ::= String/Integer.
GoalIds ::= [GoalId].

procedure imports(Declarations, GoalIds).

imports(Declarations, ImportList) :-
	build_inverted_graph(Declarations?, sof, _Graph, ClosedGraph),
	list_imports(ClosedGraph, ImportList, []).

procedure lists(Declarations, GoalIds, GoalIds, GoalIds).

lists(Declarations, Procedures, DeadCode, Undeclared) :-
	build_calling_graph(Declarations, sof, Graph, ClosedGraph),
	list_procedures(Graph, Procedures, []),
	list_undeclared(ClosedGraph, Undeclared, []),
	color_alive(ClosedGraph, ColoredGraph),
	list_dead_code(ColoredGraph?, DeadCode, []).

/*
	These procedures generate the inverted (callers) graph from the
	declaration list.

	build_inverted_graph(CallList, Busy, Graph, ClosedGraph)

	input	CallList = [procedure(F/A, [FB1/AB1, FB2/AB2, ...]), ...]
	output	Graph = {_Hash, Node, LeftBranch, RightBranch}
			LeftBranch, RightBranch = Graph | Variable
			Node = {F/A, CallGraph, Import}
		ClosedGraph: Variable branches and calls = [].
*/

build_inverted_graph(Declarations, Left, Graph, Right) :-

    Declarations ? procedure(F/A, GoalIds),
    known(A),
    Hash := string_hash(F) :
      Node = {F/A, _Callers, _Import} |
	member(Hash, Node, Graph, GoalIds, GoalIds'),
	add_callers(GoalIds', Node, Graph, Left, Left', Right, Right'),
	build_inverted_graph;

    Declarations = [],
    known(Left) |
	close_graph(Graph, Graph, Right).


add_callers(GoalIds, Node, Graph, Left1, Right1, Left2, Right2) :-

    GoalIds ? '#'/2 |
	propagate_importing({_, Node, [], []}, Left2, Left2'), 
	self;

    GoalIds ? '@'/2 |
	propagate_importing({_, Node, [], []}, Left2, Left2'), 
	self;

    otherwise,
    GoalIds ? F/A,
    Hash := string_hash(F) :
      CalledNode = {F/A, Nodes, _} |
	member(Hash, CalledNode, Graph, done, Done), 
	wait_member(Done, Hash, Node, Nodes, Left1, Left1'), 
	self;

    GoalIds = [] : Node = _, Graph = _,
      Left1 = Right1,
      Left2 = Right2 .

propagate_importing(Node, Left, Right) :-

    Node = {_, {_, _, Import}, LeftNode, Node'},
    known(Import) |
	propagate_importing(LeftNode, Left, Left'),
	self;

    Node = {_, {_, HisCallers, Import}, LeftNode, Node'},
    writable(Import) :
      Import = importing |
	propagate_importing(HisCallers, Left, Left'), 
	propagate_importing(LeftNode, Left', Left''),
	self;

    Node = [] :
      Left = Right .

/*
	These procedures generate the (direct) calling graph from the
	declaration list.

	build_calling_graph(Declarations, Busy, Graph, ClosedGraph)

	input	Declarations = [procedure(F/A, [FB1/AB1, FB2/AB2, ...]) ...]
	output	Graph = {_Hash, Node, LeftBranch, RightBranch}
			LeftBranch, RightBranch = Graph | Variable | []
			Node = {F/A, Calls, Definition, Vitality}
			Calls = Variable | Graph
			Definition = declared if F/A is a declared procedure.
		ClosedGraph: Variable branches and calls = [].
*/

build_calling_graph(Declarations, Sof, Graph, ClosedGraph) :-

    Declarations ? procedure(F/A, GoalIds),
    known(A),
    Hash := string_hash(F) |
	member(Hash, {F/A, CallGraph, declared, _}, Graph, GoalIds, GoalIds1),
	add_calls(GoalIds1, CallGraph, Graph, Sof, Sof'),
	self;

    Declarations =[],
    known(Sof) |
	close_graph(Graph, Graph, ClosedGraph).


add_calls(Rest, CallGraph, Graph, Left, Right) :-

    Rest ? F/A,
    known(A),
    Hash := string_hash(F) :
      Call = {F/A, _, _, _} |
	member(Hash, Call, Graph, Left, Left'), 
	member(Hash, Call, CallGraph, Left', Left''), 
	self;

    Rest = [] : CallGraph = _, Graph = _,
      Left = Right .

/*
	Auxiliary Graph/Tree-building routines.
*/

wait_member(done, Hash, Node, Nodes, Left, Right) :-
	member(Hash, Node, Nodes, Left, Right).

member(Hash, Node, Nodes, Left, Right) :-

    true :
      Nodes = {Hash, Node, _, _},
      Left = Right ;

    Nodes = {Hash1, _, Nodes', _},
    Hash @< Hash1 |
	self;

    otherwise,
    Nodes = {_, _, _, Nodes'} |
	self.


close_graph(Graph, Left, Right) :-

    Graph = {_, Node, LeftGraph, Graph'}, 
    arg(2, Node, Tree) |
	close_tree(Tree, Left, Left'),
	close_graph(LeftGraph, Left', Left''),
	self;

    writable(Graph) :
      Graph = [],
      Left = Right .

  close_tree(Tree, Left, Right) :-
    Tree = {_, _, LeftTree, Tree'} |
	close_tree(LeftTree, Left, Left'),
	self;

    writable(Tree) :
      Tree = [],
      Left = Right .

/*
	This group of procedures propagates the "Vitality" indicator through
	the calling graph, coloring accessible procedures "alive".
*/

color_alive(Graph, ColoredGraph) :-
    Graph = {_, Alive, _, _} |
	color_alive1({_, Alive, [], []}, Graph, ColoredGraph);
    Graph = [] :
      ColoredGraph = [] .

  color_alive1(Graph, Left, Right) :-

    Graph = {_, {_, _, _, Vitality}, LeftGraph, Graph'},
    known(Vitality) |
	color_alive1(LeftGraph, Left, Left'),
	self;

    Graph = {_, {_, Calls, _, Vitality}, LeftGraph, Graph'},
    writable(Vitality) :
      Vitality = alive |
	color_alive1(Calls, Left, Left'), 
	color_alive1(LeftGraph, Left', Left''), 
	self;

    Graph = [] :
      Left = Right .


/*
	This group of procedures generates the output lists.
*/

list_imports(Graph, ImportList, Tail) :-

    Graph = {_, {_, _, not_importing^}, LeftGraph, Graph'} |
	list_imports(LeftGraph, ImportList, ImportList'),
	self;

    otherwise,
    Graph = {_, {Ident, _, _}, LeftGraph, Graph'} :
      ImportList ! Ident |
	list_imports(LeftGraph, ImportList', ImportList''),
	self;

    Graph = [] :
      ImportList = Tail .


list_procedures(Graph, Procedures, Tail) :-

    Graph = {_, {Ident, _, _, _}, LeftGraph, Graph'} :
      Procedures ! Ident |
	list_procedures(LeftGraph, Procedures', Procedures''),
	self;

    Graph = [] :
      Procedures = Tail .


list_undeclared(Graph, Undeclared, Tail) :-

    Graph = {_, {Ident, _, undeclared^, _}, LeftGraph, Graph'} :
      Undeclared ! Ident |
	list_undeclared(LeftGraph, Undeclared', Undeclared''),
	self;

    otherwise,
    Graph = {_, _, LeftGraph, Graph'} |
	list_undeclared(LeftGraph, Undeclared, Undeclared'),
	self;

    Graph = [] :
      Undeclared = Tail .


list_dead_code(Graph, DeadCode, Tail) :-

    Graph = {_, {Ident, _, Definition, dead^}, LeftGraph, Graph'} |
	list_dead_code(LeftGraph, DeadCode', DeadCode''),
	self,
	list_dead_code_definition(Definition, Ident, DeadCode, DeadCode');

    otherwise,
    Graph = {_, _, LeftGraph, Graph'} |
	list_dead_code(LeftGraph, DeadCode, DeadCode'),
	self;

    Graph = [] :
      DeadCode = Tail .

  list_dead_code_definition(Definition, Ident, DeadCode1, DeadCode2) :-

    Definition = undeclared : Ident = _,
      DeadCode1 = DeadCode2;

    Definition =\= undeclared :
      DeadCode1 = [Ident | DeadCode2] .
