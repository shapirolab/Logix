/* $Header: /home/qiana/Repository/Logix/system/transform/dguards.cp,v 1.1 1999/07/09 07:03:15 bill Exp $ */
-export([transform/5]).
-mode(trust).
-language(compound).

procedure transform([Any], [Any], [Any], [Any], [Any]).

transform(Options1, Terms, Options2, Clauses, Out) :-
    	true :
      	Options2 = Options1 |
	server(Clauses, Clauses'),
	transformer(Terms, Clauses', Out).
	

server(Clauses, ClausesT) :-

    	true :
      	Clauses = [	(server([relation(A,B,St) | Reqs]) :-
				ReServe,
				guards(A, B, St)
			),
/*
			(server([preconditions(A,Precs) | Reqs]) :-
				ReServe,
				preconditions(A, Precs)
			),
*/
      			(server([dominate(A,B,St) | Reqs]) :-
				ReServe,
				dominate(A, B, St)
			),
			(server([connect(A,Ins,Outs) | Reqs]) :-
				ReServe,
				connect(A,Ins, Outs)
			),
			(server([]))
	       		| ClausesT ],
 	A = `'A', B = `'B', St = `'St', 
      	Precs = `'Precs', Ins = `'Ins', Outs = `'Outs',
      	Reqs = `'Reqs', 
      	ReServe = server(Reqs) .

transformer(Terms, Clauses, Errors)
:-
	build_graphs(Terms, Relations, Guards, Iffs, Precs,
	                                           Clauses, Errors, Clauses'),
%        screen#display(Relations,[length(2000),depth(2000),type(ground)]),
	build_table(Relations, Guards, Iffs, Precs, Clauses').

build_graphs(Terms, Relations, Guards, Iffs, Precs, Clauses, Errors, ClausesT)+
			(AccRel=[], Dominate=[])	
:-
	Terms ? T, T = connect(G,In,Out),
	arity(G,Ar),
	make_tuple(Ar,G1),
	arg(1,G,Pred),
	arg(1,G1,Pred) :	
	Guards ! {G,Out},
	Clauses ! (connect(G1,In^,Out^)) |
	replace_args(Ar, G1),
	build_graphs ;

	Terms ? T, T = {'>',A,B}, A =\= B |
        get_keys(A, Keys),
	add_to_entries(Keys, Dominate, B, Dominate'),
	build_graphs ;

	Terms ? {'=>',A,B}, A =\= B |
	add_to_entry(A, AccRel, B, AccRel'),
	get_neg_keys(B, Keys),
	add_to_entries(Keys, AccRel', ~A, AccRel''),
	build_graphs ;

	Terms ? {'<=>',A,B}, A =\= B :
	Iffs ! {A,B} |
	add_to_entry(A, AccRel, B, AccRel'),
	add_to_entry(B, AccRel', A, AccRel''),
	get_neg_keys(A, KeysA),
	get_neg_keys(B, KeysB),
	add_to_entries(KeysA, AccRel'', ~B, AccRel'''),
	add_to_entries(KeysB, AccRel''', ~A, AccRel''''),
	build_graphs ;

	Terms ? {'=',A,B}, A =\= B :
	Iffs ! {A,B}, Iffs' ! {B1,A1} |
        strip_negs(~(A),A1),
	strip_negs(~(B),B1),
	build_graphs ;

	Terms = [] : 
	Guards = [], Iffs = [], 
	Clauses = [(connect(G,AN,AN) :- 
			otherwise | 
			fail(G, unknown)
		  )|ClausesT],
	G = `"G", AN = `"_" |
	close_graph(Dominate, Precs, Errors, Errors'),
	close_graph(AccRel, Relations, Errors', []).

replace_args(Ar,G)
:-
	Ar > 1, arg(Ar,G,(`"_")^) | Ar' := Ar - 1, replace_args ;
	Ar = 1 : G = _ | true.

get_keys(In, Keys)
:-
	In = (X,In') : Keys ! X | get_keys ;
	In =\= (_,_) : Keys = [In].

get_neg_keys(In, Keys)
:-
	In = (X,In') : Keys ! ~(X) | get_neg_keys ;
	In =\= (_,_) : Keys = [~(In)].

add_to_entries(Keys, AccRelI, Add, AccRelO)
:-
	Keys ? Key |
        add_to_entry(Key, AccRelI, Add, AccRelI'),
	add_to_entries ;

	Keys = [] : AccRelO = AccRelI, Add = _.

add_to_entry(Key, Rel, Add, RelO)
:-
	strip_negs(Key, Key1),
	add_to_entry1(Key1, Rel, Add, RelO).

strip_negs(Key, KeyO)
:-
	Key = ~(~Key') | strip_negs ;
	Key =\= ~(~(_)) : KeyO = Key.

add_to_entry1(Key, Rel, Add, RelO)
:-
	Rel ? {Key,{V,Ent}} : 
        RelO = [{Key,{V,EntO}}|Rel'] |
        adde(Add, Ent, EntO) ;

	Rel ? E, E = {A1,_}, Key =\= A1 : RelO ! E  | add_to_entry1 ;

	Rel = [] : RelO = [{Key,{0,Ent}}] |
        adde(Add, [], Ent).

adde(Add, In, Out)
:-
	Add = (X,Add') : Out ! X1 | strip_negs(X,X1), adde ;
	Add =\= (_,_) : Out = [Add1|In] | strip_negs(Add,Add1).

get_entry(Key, Rel, Implies)
:-
	Rel ? {Key,{_,Ent}} : Implies = Ent, Rel' = _ ; 
	Rel ? {Key,Ent}, Ent = [_|_] : Implies = Ent, Rel' = _ ; 
	Rel ? {A1,_}, Key =\= A1 | get_entry ;
	Rel = [] : Implies = [], Key = _.

close_graph(Graph, ClosedGraph, Errors, ErrorsT)
:-
	check_acyclic(Graph, {ok,St}),
	closure(St, Graph, ClosedGraph, Errors, ErrorsT).

closure(St, Graph, Closure, Errors, ErrorsT)
:-
	St = cycle : Closure = Graph, 
	Errors = ['WARNING - Cycle in guard relation definition'|ErrorsT] ;

	St = ok : Errors = ErrorsT |
	get_closure(Graph, ClosedGraph, Iff),
	close_iff(Iff, ClosedGraph, Closure'),
	delete_tautology(Closure', Closure).

delete_tautology(GIn, GOut)
:-
	GIn ? {G, Imply} : GOut ! {G, ImplyO} | 
	minus(G, Imply, ImplyO),
	delete_tautology ;

	GIn = [] : GOut = [].

get_closure(Graph, ClosedGraph, Iff)+(CloseGs=[])
:-
	Graph ? {G,{0,Implies}} |
	get_closure_of(Implies, G, [], ImpliesO, 
				[{G,ImpliesG}|CloseGs], CloseGs',
				Graph', Graph'', Iff, Iff'),
	union(Implies, ImpliesO, ImpliesG),
	get_closure ;

	Graph = [] : ClosedGraph = CloseGs, Iff = [].

get_closure_of(Gs, G, Parent, ImpliesO, CloseGs, CloseGsO, GraphI, GraphO, 
								Iff, IffT)
:-
	Gs ? I, I =\= Parent |
	get_entry(I, CloseGs, ClsI),
	get_entry(I, GraphI, Edges),
	get_closure_of_guard(I, G, ClsI, Edges, 
			Implies, CloseGs, CloseGs', GraphI, GraphI', 
						Iff, Iff'),
	union(Implies, ImpliesO', ImpliesO),
	get_closure_of ;

	Gs ? Parent : Iff ! {G,Parent} | get_closure_of ;

	Gs = [] : 
	ImpliesO = [], GraphO = GraphI, CloseGsO = CloseGs, IffT = Iff,
	G = _, Parent = _.

get_closure_of_guard(G, Parent, ClsI, Edges, Implies, 
			CloseGs, CloseGsO, GraphI, GraphO, IffI, IffO)
:-
	ClsI =\= [] : Implies = ClsI,
	CloseGsO = CloseGs, GraphO = GraphI, IffO = IffI, 
	G = _, Parent = _, Edges = _ ;

	ClsI = [], Edges =\= [] |
	get_closure_of(Edges, G, Parent, ImpliesO, 
					[{G,Implies}|CloseGs], CloseGsO, 
					Graph', GraphO, IffI, IffO),
	union(Edges, ImpliesO, Implies),
	delete_entry(G, GraphI, Graph') ;

	ClsI = [], Edges = [] : Implies = [],
	CloseGsO = CloseGs, GraphO = GraphI, IffO = IffI,
	G = _, Parent = _.

delete_entry(Key, GI, GO)
:-
	GI ? {Key,_} : GO = GI';
	GI ? E, E = {K,_}, K =\= Key : GO ! E | delete_entry.

close_iff(Iff, CloseG, Closure)
:-
	Iff ? {A,B} |
	get_entry(A, CloseG, Ea),
	get_entry(B, CloseG, Eb),
	union(Eb, Ea, Un),
	update_closure(A, Un, CloseG, CloseG'),
	close_iff ;

	Iff = [] : Closure =CloseG.

update_closure(Key, Entry, CloseI, CloseO)
:-
	CloseI ? {Key,_} : CloseO = [{Key,Entry}|CloseI'] ;
	CloseI ? Ent, Ent = {K,_}, Key =\= K : CloseO ! Ent | update_closure.

union(S1, S2, U)+(Um=S1)
:-
	S2 ? R | add(R, Um, Um'), union ;
	S2 = [] : U = Um, S1 = _.

add(R, U, Uo)
:-
	U ? R : Uo = U, U' = _ ;
	U ? R1, R =\= R1 : Uo ! R1 | add ;
	U =[] : Uo = [R].

append(Xs, Ys, Zs)
:-
	Xs ? X : Zs ! X | append ;
	Xs = [] : Zs = Ys.

check_acyclic(Graph, SC)
:-
	Graph ? {Node,{0,Edges}}, SC = {L,R} : SC' = {M,R} |
	check_sub_graphs(Node, Edges, [Node], 
				[{Node,{1,Edges}}|Graph'], Graph'', {L,M}),
	check_acyclic ;

	Graph ? {_,{1,_}} | check_acyclic ;

	Graph = [], SC = {L,R} : R = L.

check_sub_graphs(Node, Edges, Path, Graph, GraphO, SC)
:-
	Edges ? E, SC = {L,R} : SC' = {M,R} |
	member(E, Path, St),
	check_sub_graph(St, Node, E, Path, Graph, Graph', {L,M}),
	check_sub_graphs ;

	Edges = [], SC = {L,R} : R = L, GraphO = Graph, Node = _, Path = _.

member(X, List, St)
:-
	List = [X|_] : St = cycle ;
	List ? Y, X =\= Y | member ;
	List = [] : St = ok, X = _.

check_sub_graph(St, Parent, Edge, Path, Graph, GraphO, SC)
:-
	St = cycle, SC = {_,R} : 
	R = cycle, GraphO = [], Graph = _, Parent = _, Edge = _, Path = _ ;

	St = ok |
	update_entry(Edge, Graph, Edges, Graph'),
	minus(Parent, Edges, Sons),
	check_sub_graphs(Edge, Sons, [Edge|Path], Graph', GraphO, SC). 

update_entry(Edge, Graph, Edges, GraphO)
:-
	Graph ? {Edge,{_,Ent}} : 
	Edges = Ent, GraphO = [{Edge,{1,Ent}}|Graph'] ;
	
	Graph ? Ent, Ent = {E,{_,_}}, E =\= Edge : 
	GraphO ! Ent | update_entry ;

	Graph = [] : GraphO = [], Edges = [], Edge = _.

minus(Parent, Edges, Sons)
:-
	Edges ? Parent : Sons = Edges' ;
	Edges ? E, E =\= Parent : Sons ! E | minus ;
	Edges = [] : Sons = [], Parent = _.

build_table(Relations, Guards, Iffs, Precs, Clauses)
:-
	dominate_identical(Guards, Clauses, Clauses'),
	dominate_procedure(Precs, Clauses', Clauses''),
	tautologies(Guards, Clauses'', Clauses'''),
	delete_iffs(Iffs, Relations, Relations'),
	guard_relations(Relations', Clauses''', Clauses''''),
	guard_iffs(Iffs, Clauses'''', []).

delete_iffs(Iffs, Relations, RelationsO)
:-
	Iffs ? {A,B} |
	delete_from_entry1(A, Relations, B, Relations'),
	delete_from_entry1(B, Relations', A, Relations''),
	get_neg_keys(A, KeysA),
	get_neg_keys(B, KeysB),
	delete_from_entries(KeysA, Relations'', ~B, Relations'''),
	delete_from_entries(KeysB, Relations''', ~A, Relations''''),
	delete_iffs ;

	Iffs = [] : RelationsO = Relations.
 	
delete_from_entries(Keys, AccRelI, Del, AccRelO)
:-
	Keys ? Key |
        delete_from_entry(Key, AccRelI, Del, AccRelI'),
	delete_from_entries ;

	Keys = [] : AccRelO = AccRelI, Del = _.

delete_from_entry(Key, Rel, Del, RelO)
:-
	strip_negs(Key, Key1),
	strip_negs(Del, Del1),
	delete_from_entry1(Key1, Rel, Del1, RelO).

delete_from_entry1(Key, Rel, Del, RelO)
:-
	Rel ? {Key,Ent} : RelO = [{Key,Ent'}|Rel'] | minus(Del, Ent, Ent') ;
	Rel ? E, E = {A1,_}, Key =\= A1 : RelO ! E  | delete_from_entry1 ;
        Rel = [] : RelO = [], Ley = _, Del = _.

guard_iffs(Iffs, Clauses, ClausesT)
:-
	Iffs ? {G1,G2}, G1 =\= ~(_), G2 =\= ~(_) :
	Clauses ! guards(G1,G2,3^),
	Clauses' ! guards(G2,G1,3^) |
	guard_iffs ;

	Iffs ? {~G1,G2}, G2 =\= ~(_) :
	Clauses ! guards(G1,G2,1^),
	Clauses' ! guards(G2,G1,1^) |
	guard_iffs ;

	Iffs ? {G1,~G2}, G1 =\= ~(_) :
	Clauses ! guards(G1,G2,1^),
	Clauses' ! guards(G2,G1,1^) |
	guard_iffs ;

	Iffs ? {~G1,~G2} :
	Clauses ! guards(G1,G2,1^),
	Clauses' ! guards(G2,G1,1^) |
	guard_iffs ;

	Iffs = [] : 
	Clauses = [(guards(AN,AN,ow^) :- otherwise | true)|ClausesT],
	AN = `"_".

guard_relations(Relations, Clauses, ClausesT)
:-
	Relations ? {G,Implies}, G =\= ~(_), G = (X,Y) |
	guard_relations_of(X, Y, Implies, Clauses, Clauses'),
	guard_relations ;

	Relations ? {G,Implies}, G =\= ~(_), G =\= (_,_) |
	guard_relations_of(G, [], Implies, Clauses, Clauses'),
	guard_relations ;

	Relations ? {~G,Implies} |
%	neg_guard_relations_of(G, Implies, Clauses, Clauses'),
	guard_relations ;

	Relations = [] : Clauses = ClausesT.

/*
neg_guard_relations_of(G, Implies, Clauses, ClausesT)
:-
	Implies ? G1, G1 =\= ~(_) | neg_guard_relations_of ;
	
	Implies ? G1, G1 = ~(G2) : 
	Clauses ! guards(G, G2, 2^) | neg_guard_relations_of ;
	
	Implies = [] : Clauses = ClausesT, G = _.
*/
	
guard_relations_of(Head, Ask, Implies, Clauses, ClausesT)
:-
	Ask = [], 
	Implies ? G1, G1 =\= ~(_) : 
	Clauses ! guards(G1, Head, 2^) | guard_relations_of ;
	
	Ask = [],
	Implies ? G1, G1 = ~(G2) : 
	Clauses ! guards(Head, G2, 0^) | guard_relations_of ;
	
	Ask =\= [], 
	Implies ? G1, G1 =\= ~(_) : 
	Clauses ! (guards(G1, Head, 2^) :- Ask | true) | guard_relations_of ;
	
	Ask =\= [],
	Implies ? G1, G1 = ~(G2) : 
	Clauses ! (guards(Head, G2, 0^) :- Ask | true) | guard_relations_of ;
	
	Implies = [] : Clauses = ClausesT, Ask = _, Head = _.

tautologies(Guards, Clauses, ClausesT)
:-
	Guards ? {G,Conn},
	arity(G,Ar),
	make_tuple(Ar,G1),
	arg(1,G,Pred),
	arg(1,G1,Pred) :
        Clauses ! guards(G1, G1, 3^) | 
	get_args(Ar, G1, Conn),
        tautologies ;

	Guards = [] : Clauses = ClausesT.

dominate_identical(Guards, Clauses, ClausesT)
:-
	Guards ? {G,_},
	arity(G,Ar),
	make_tuple(Ar,G1),
	arg(1,G,Pred),
	arg(1,G1,Pred) :
	Clauses ! (dominate(G1,G1,G1^)) |
	put_args(Ar, G1),
	dominate_identical ;

	Guards = [] : ClausesT = Clauses.

put_args(Ar, G)
:-
	Ar > 1, arg(Ar,G,(`(X?))^), convert_to_string(Ar,X1) | 
                utils#append_strings(["A",X1],X),
		Ar':= Ar - 1,
	        put_args ;

	Ar = 1 : G = _.

get_args(Ar, G1, Conn)+(J=0)
:-
	Ar > 1 | 
            replace_arg(Ar, J, G1, Conn, J'),
	    Ar' := Ar - 1,
	    get_args ;
	Ar = 1 : G1 = _, Conn = _, J= _.

replace_arg(I, J, G, Conn, JO)
:-
	Conn ? I, arg(I,G,(`"_")^) : JO = J, Conn' = _ ;
	Conn ? I1, I1 =\= I | replace_arg ;
	Conn = [], arg(I,G,(`(X?))^), convert_to_string(J,X1) | 
                utils#append_strings(["A",X1],X),
		JO := J + 1.

dominate_procedure(Graph, Clauses, ClausesT)
:-
	Graph ? {A,Precs} |
	dominate_of(A, Precs, Clauses, Clauses'),
	dominate_procedure ;

	Graph = [] :
	Clauses = [(dominate(`"_",`"_",0^) :- otherwise | true)|ClausesT].

dominate_of(A, Precs, Clauses, ClausesT)
:-
	Precs ? B : 
	Clauses ! (dominate(A,B,A^)),
	Clauses' ! (dominate(B,A,A^)) |
	dominate_of ;

	Precs = [] : Clauses = ClausesT, A = _.


