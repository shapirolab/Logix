/* $Header: /home/qiana/Repository/Logix/system/widgets/dohier.cp,v 1.1.1.1 1999/07/09 07:03:22 bill Exp $ */
-language(compound).
-export([modules/3, directors/3]).

procedure modules(Any, [Any], Any).

modules(Root, Var, Calls) :-
	common(Root, Var, Calls, modules).

procedure directors(Any, [Any], Any).

directors(Root, Var, Calls) :-
	common(Root, Var, Calls, directors).

common(Root, Var, Calls, Kind) :-
    freeze(Var(Calls), FVC, _) |
	hierarchy # source_tree(Root, RTID, T),
	c(FVC, RTID, T, Kind, _).


c(FVC1, RTID, T, Kind, FVC2) :-
    T = {RId, Ms, Ts} |
	ds(FVC1', RTID, Ts, Kind, FVC2, Ds),
	choose(Kind, Ms, Ds, Dos),
	do(FVC1, RTID, RId, Dos, FVC1').

ds(FVC1, RTID, Ts, Kind, FVC2, Ds) :-

    Ts ? T, T = {[D | _], _, _} :
      Ds ! D |
	ds,
	c(FVC1, RTID, T, Kind, FVC1');

    Ts = [] : RTID = _, Kind = _,
      FVC2 = FVC1,
      Ds = [] .

choose(Kind, Ms, Ds, Dos) :-

    Kind = modules : Ds = _,
      Dos = Ms ;

    Kind = directors : Ms = _,
      Dos = Ds .

do(FVC1, RTID, RId, Dos, FVC2) :-

    known(FVC1),
    Dos ? Name :
      melt(FVC1, {RTID#V, Cs}, _) |
	relative_path(RId, Name, V),
	computation # display(term, V, [type(ground), close(FVC1, FVC1')]),
	computation_utils # call_list(Cs', Reply),
	do,
	when(FVC1', Cs, Cs'),
	when(Reply, FVC1', FVC1'');

    Dos = [] : RTID = _, RId = _,
      FVC2 = FVC1 .

relative_path(Id, Partial, Relative) :-

    Id ? Node, list(Id') :
      Partial' = Node # Partial |
	relative_path;

    Id = [Node] :
      Relative = Node # Partial ;

    Id = [] :
      Relative = Partial .


when(Reply, FVC1, FVC2) :-
    known(Reply) :
      FVC2 = FVC1 .
