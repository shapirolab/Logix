/* $Header: /home/qiana/Repository/Logix/system/widgets/rtimer.cp,v 1.1.1.1 1999/07/09 07:03:22 bill Exp $ */
/*
**
** Real Timer - time Sub-Computation Remote Procedure Call until terminated
**
**   Out is RealTime, or as defined for  timer.cp  with  real_time = RealTime
*/

-export([goal/2]).
-mode(trust).
-language(compound).

procedure goal(Any, Any).

goal(Call, Out) :-
	goal_rpc(Call, RPC, Goal, Goal'),
	computation_utils # call_id_goal(self # RPC, Id, Goal, Reply),
	time_start(Call, Out, Id, Goal', Reply).


goal_rpc(Call, RPC, Goal1, Goal2) :-

    Call = Call' @ Link :
      Goal2 = Goal2' @ Link |
	goal_rpc;

    otherwise :
      Call = RPC,
      Goal1 = Goal2 .


time_start(Call, Out, Id, Goal, Reply) :-

    Reply = true : Call = _ |
	computation # call([Id # [attributes(A), Goal']], Events),
	processor # machine(idle_wait(Done)),
	time0(Done, A, Goal, Goal', Measures0),
	time1(Events, Measures1),
	difference(Measures1, Measures0, Out);

    Reply = false(Reason) : Out = _, Id = _, Goal = _ |
	computation # failed(Call, Reason).


time0(Done, A, Goal1, Goal2, Measures0) :-

    known(A),
    Done = done,

    info(1, Start_cpu),
    info(2, Start_free_heap),
    info(3, Start_used_heap),
    info(4, Start_creations),
    info(5, Start_reductions),
    info(6, Start_suspensions),
    info(7, Start_terminations),
    info(8, Start_activations),
    info(9, Start_collections),
    info(12, Start_real_time) :

      Goal1 = Goal2,
      Measures0 = {Start_cpu, Start_free_heap, Start_used_heap,
		   Start_creations, Start_reductions, Start_suspensions,
		   Start_terminations, Start_activations, Start_collections,
		   Start_real_time} .


time1(Events, Measures1) :-

    Events = [],

    info(1, End_cpu),
    info(2, End_free_heap),
    info(3, End_used_heap),
    info(4, End_creations),
    info(5, End_reductions),
    info(6, End_suspensions),
    info(7, End_terminations),
    info(8, End_activations),
    info(9, End_collections),
    info(12, End_real_time) :

      Measures1 = {End_cpu, End_free_heap, End_used_heap,
		   End_creations, End_reductions, End_suspensions,
		   End_terminations, End_activations, End_collections,
		   End_real_time} ;

    Events ? String,
    string(String) |
	time1;

    otherwise,
    Events ? Diagnostic |
	computation # Diagnostic,
	time1.


difference(Measures1, Measures0, Out) :-

    Measures1 = {_, _, _, _, _, _, _, _, _, RT1},
    Measures0 =	{_, _, _, _, _, _, _, _, _, RT0},
    RealTime := RT1 - RT0 :
     RealTime = Out ;

    Out ? (cpu = X^),
    arg(1, Measures1, X1),
    arg(1, Measures0, X0),
    X := X1 - X0 |
	difference;

    Out ? (free_heap = X^),
    arg(2, Measures1, X1),
    arg(2, Measures0, X0),
    X := X1 - X0 |
	difference;

    Out ? (used_heap = X^),
    arg(3, Measures1, X1),
    arg(3, Measures0, X0),
    X := X1 - X0 |
	difference;

    Out ? (creations = X^),
    arg(4, Measures1, X1),
    arg(4, Measures0, X0),
    X := X1 - X0 |
	difference;

    Out ? (reductions = X^),
    arg(5, Measures1, X1),
    arg(5, Measures0, X0),
    X := X1 - X0 |
	difference;

    Out ? (suspensions = X^),
    arg(6, Measures1, X1),
    arg(6, Measures0, X0),
    X := X1 - X0 |
	difference;

    Out ? (terminations = X^),
    arg(7, Measures1, X1),
    arg(7, Measures0, X0),
    X := X1 - X0 |
	difference;

    Out ? (activations = X^),
    arg(8, Measures1, X1),
    arg(8, Measures0, X0),
    X := X1 - X0 |
	difference;

    Out ? (collections = X^),
    arg(9, Measures1, X1),
    arg(9, Measures0, X0),
    X := X1 - X0 |
	difference;

    Out ? (real_time = X^),
    arg(10, Measures1, X1),
    arg(10, Measures0, X0),
    X := X1 - X0 |
	difference;

    otherwise,
    Out ? _ |
	difference;

    Out =\= [_ | _], Out =\= [] :
      Out' = [Out] |
	difference;

    Out = [] : Measures1 = _, Measures0 = _ .
