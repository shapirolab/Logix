/*

Command Timer

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:02:54 $
Currently locked by 	$Locker:  $
			$Revision: 1.1.1.1 $
			$Source: /home/qiana/Repository/Logix/system/timer.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([rpc/2, time/2]).
-mode(trust).
-language(compound).


procedure rpc(Any, Any).

rpc(Call, Out) :-
	computation_utils # call_id_goal(self#Call, Id, Goal, Reply),
	rpc_start(Call, Out, Id, Goal, Reply).


procedure time(Any, Any).

time(Call, Out) :-
	computation_utils # call_id_goal(self # Call, Id, Goal, Reply),
	time_start(Call, Out, Id, Goal, Reply).


time_start(Call, Out, Id, Goal, Reply) :-

    Reply = true : Call = _ |
	processor # [machine(idle_wait(Idle)) | Waits],
	Id # [[] | Goals],
	time_real_goal(Base, Out, Goals', Goal, Waits'),
	time_goal(Idle, [], Goals, Goals', Waits, Waits', Base);

    Reply = false(Reason) : Out = _, Id = _, Goal = _ |
	computation # failed(Call, Reason).


time_real_goal(Base, Out, Goals, Goal, Waits) :-
    known(Base) :
      Waits ! machine(idle_wait(Idle)) |
	close_when_known(Measures, Goals', Waits''),
	difference(Measures, Base, Out),
	time_goal(Idle, Goal, Goals, Goals', Waits', Waits'', Measures).


time_goal(Idle, Goal, Goals1, Goals2, Waits1, Waits2, Measures) :-

    known(Idle),

    info(1, Start_cpu),
    info(2, Start_free_heap),
    info(3, Start_used_heap),
    info(4, Start_creations),
    info(5, Start_reductions),
    info(6, Start_suspensions),
    info(7, Start_terminations),
    info(8, Start_activations),
    info(9, Start_collections),
    info(11, Start_average_heap),
    info(12, Start_real_time) :

      Goals1 = [Goal | Goals2], 
      Waits1 = [machine(idle_wait(Idle1)) | Waits2] |

	timer(	{Start_cpu, Start_free_heap, Start_used_heap,
		 Start_creations, Start_reductions, Start_suspensions,
		 Start_terminations, Start_activations, Start_collections,
		 Start_real_time, Start_average_heap
		},
		Idle1, Measures).


timer(Measures1, Idle, Measures2):-

    known(Idle),
    Measures1 = {Start_cpu, Start_free_heap, Start_used_heap,
		 Start_creations, Start_reductions, Start_suspensions,
		 Start_terminations, Start_activations, Start_collections,
		 Start_real_time, Start_average_heap
		},

    info(1, End_cpu),
    info(2, End_free_heap),
    info(3, End_used_heap),
    info(4, End_creations),
    info(5, End_reductions),
    info(6, End_suspensions),
    info(7, End_terminations),
    info(8, End_activations),
    info(9, End_collections),
    info(11, End_average_heap),
    info(12, End_real_time),

    Cpu := End_cpu - Start_cpu,
    Freeheap := End_free_heap - Start_free_heap,
    Usedheap := End_used_heap - Start_used_heap,
    Creations := End_creations - Start_creations,
    Reductions := End_reductions - Start_reductions,
    Suspensions := End_suspensions - Start_suspensions,
    Terminations := End_terminations - Start_terminations,
    Activations := End_activations - Start_activations,
    Collections := End_collections - Start_collections,
    Real_time := End_real_time - Start_real_time :
      Measures2 = {Cpu, Freeheap, Usedheap, Creations, Reductions,
		   Suspensions, Terminations, Activations, Collections,
		   Real_time,
		   {Start_collections, Start_average_heap,
		    End_collections, End_average_heap
		   }
		  } .


difference(Measures1, Measures0, Out) :-

    Measures1 = {CP1, _, U1, C1, R1, _, _, _, _, _, {SC1, SW1, EC1, EW1}},
    Measures0 = {CP0, _, U0, C0, R0, _, _, _, _, _, {SC0, SW0, EC0, EW0}},
    CP := CP1 - CP0,
    C := C1 - C0,
    R := R1 - R0,
    HW := real(U1) - real(U0)
       + (real(EC1)*real(EW1) - real(SC1)*real(SW1))
       - (real(EC0)*real(EW0) - real(SC0)*real(SW0)) :
      Out = (cpu = CP, reductions = R, creations = C, heap_words = HW') |
	convert_to_number(HW, HW');

    Out ? (cpu = X),
    arg(1, Measures1, X1),
    arg(1, Measures0, X0),
    X^ := X1 - X0 |
	difference;

    Out ? (free_heap = X),
    arg(2, Measures1, X1),
    arg(2, Measures0, X0),
    X^ := X1 - X0 |
	difference;

    Out ? (used_heap = X),
    arg(3, Measures1, X1),
    arg(3, Measures0, X0),
    X^ := X1 - X0 |
	difference;

    Out ? (creations = X),
    arg(4, Measures1, X1),
    arg(4, Measures0, X0),
    X^ := X1 - X0 |
	difference;

    Out ? (reductions = X),
    arg(5, Measures1, X1),
    arg(5, Measures0, X0),
    X^ := X1 - X0 |
	difference;

    Out ? (suspensions = X),
    arg(6, Measures1, X1),
    arg(6, Measures0, X0),
    X^ := X1 - X0 |
	difference;

    Out ? (terminations = X),
    arg(7, Measures1, X1),
    arg(7, Measures0, X0),
    X^ := X1 - X0 |
	difference;

    Out ? (activations = X),
    arg(8, Measures1, X1),
    arg(8, Measures0, X0),
    X^ := X1 - X0 |
	difference;

    Out ? (collections = X),
    arg(9, Measures1, X1),
    arg(9, Measures0, X0),
    X^ := X1 - X0 |
	difference;

    Out ? (real_time = X),
    arg(10, Measures1, X1),
    arg(10, Measures0, X0),
    X^ := X1 - X0 |
	difference;

    Out ? (heap_words = HW'),
    arg(3, Measures1, X1),
    arg(3, Measures0, X0),
    arg(11, Measures1, {SC1, SW1, EC1, EW1}),
    arg(11, Measures0, {SC0, SW0, EC0, EW0}),
    HW := real(X1) - real(X0)
       + (real(EC1)*real(EW1) - real(SC1)*real(SW1))
       - (real(EC0)*real(EW0) - real(SC0)*real(SW0))
	|
	convert_to_number(HW, HW'),
	difference;

    otherwise,
    Out ? _ |
	difference;

    Out = [] : Measures1 = _, Measures0 = _ ;

    Out =\= [_ | _], Out =\= [] :
      Out' = [Out] |
	difference.

convert_to_number(Real, Result) :-

    convert_to_integer(Real, Integer) :
      Result = Integer ;

    otherwise :
      Result = Real .


close_when_known(Measures, []^, []^) :-
    known(Measures) |
	true.


% rpc is reductions per cycle...


rpc_start(Call, Out, Id, Goal, Reply) :-

    Reply = true : Call = _ |
	Id # [[] | Goals],
	processor # machine(idle_queue(Done, 1)),
	rpc0(Goal, Out, Goals, Done);

    Reply = false(Reason) : Out = _, Id = _, Goal = _ |
	computation # failed(Call, Reason).

rpc0(Goal, Out, Goals, Done) :-
    Done = done,
    info(5, InitialReductions) :
      Goals ! Goal |
	rpc1(InitialReductions, 0, Out, Goals').


rpc1(InitialReductions, Counter, Out, Goals) :-

    info(10, 0),
    info(5, FinalReductions) :
      Goals = [] |
	TotalReductions := FinalReductions - InitialReductions,
	Cycles := Counter - 1,
	Reductions := TotalReductions - Counter - 3,
	quotient(Reductions, Cycles, RPC),
	send(	RPC,
		('reductions'=Reductions, 'cycles'=Cycles,
			'reductions_per_cycle'=RPC
		),
		Out
	);

    otherwise,
    Counter' := Counter + 1 :
      deschedule |
	rpc1.

quotient(Numerator, Denominator, Quotient) :-

    Denominator = 0 :
      Numerator = Quotient ;

    otherwise |
	Quotient := Numerator / Denominator.


send(RPC, Out1, Out2) :-
    known(RPC) :
      Out1 = Out2 .
