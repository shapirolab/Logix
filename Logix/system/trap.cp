/*

Trap Remote Procedure Calls

William Silverman - 12/86

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:02:53 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/system/trap.cp,v $

Copyright (C) 1986, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language(compound).
-export([suspend/2,unify/2,filter/3,interrupt/3]).
-mode(failsafe).

/*
	Trap any Goal for a specified Service which is in Goals.

	Goals may be a single goal (equivalent to a one-element list),
        a list of goals or a variable, which matches anything.

	When Goals is variable, the process iterates until Service is
	untrapped by  Service#untrap .

	When Goals is not variable, each Goal which is trapped is removed
	from Goals, until Goals is empty, and the process terminates.
*/

/*	suspend / 2

	Suspend any computation which calls  Service, with a Goal which
        is in Goals.

	The term "trapped(Service#Goal)" is added to the computation's
	Events stream - the computation monitor reacts by suspending
	the computation.
*/

CCC ::= {InStream, Any, Any, Channel}.
InStream ::= [Any].
Channel ::= Vector.
Goal ::= Any.
GoalSet ::= Any.
WrappedGoal ::= export(CallerId, Scope, Goal, CCC).
Service ::= Any.
Action ::= SendAction ; CloseAction.
SendAction ::= unify ; suspend(Service) ; interrupt(ServiceId, OutStream) ;
		filter(ServiceId, TrappedStream, halt).
OutStream ::= [Any | InStream].
TrappedStream ::= [trapped({`"#", ServiceId, Any}, Any)].
CloseAction ::= unify ; suspend(Service) ; interrupt(ServiceId, []) ;
		filter(ServiceId, [], halt).
ControlPackage ::= {WrappedGoal, InStream, SendAction, InStream, OutStream}.
ServiceId, CallerId, Scope ::= [String].
SystemReply ::= true ; false(Any).


procedure suspend(Service, GoalSet).

suspend(Service, Goals) :-
	filter_service(Service, _, Out, In),
	retrap(In, suspend(Service), Goals, Out).


/*	unify/2

	Attempt to unify each goal for Service with a Goal which is in
        Goals, and proceed.

*/

procedure unify(Service, GoalSet).

unify(Service, Goals) :-
	filter_service(Service, _, Out, In),
	retrap(In, unify, Goals, Out).

/*	filter / 3

	Extract each call to  Service, with a Goal which is in Goals.

	The term "trapped(ServiceId#Goal, Reply)" is sent to  Trapped :
	If:  Reply = true ,  Goal  is forgotten;
	     Reply = false , Goal  is forwarded.
*/

procedure filter(Service, GoalSet, TrappedStream).

filter(Service, Goals, Trapped) :-
    true : Trapped = Trapped'? |
	filter_service(Service, ServiceId, Out, In),
	retrap(In, filter(ServiceId, Trapped', _Halt), Goals, Out).


/*	interrupt / 3

	Interrupt any computation which calls Service with a Goal which
        is in Goals.

	The term "ServiceId#Goal" is added to the stream of Interrupts.

	The stream of Interrupts is closed when the process terminates.
*/

procedure interrupt(Service, GoalSet, OutStream).

interrupt(Service, Goals, Interrupts) :-
	filter_service(Service, ServiceId, Out, In),
	retrap(In, interrupt(ServiceId, Interrupts), Goals, Out).


procedure filter_service(Service, ServiceId, InStream, InStream).

filter_service(Service, ServiceId, Out, In) :-
	computation_utils #
		call_list([Service # service_id(ServiceId)
			  | known(ServiceId,
			  	  ['_domain'(find(ServiceId, Channel))]
			    )
			  ],
			  Ok
		),
	filter_input(Ok, Channel, Service, Out, In).

procedure filter_input(SystemReply, Channel, Service, InStream, InStream).

filter_input(true, Channel, _, Out, In) :-
    true :
      write_channel(filter(Out, In), Channel) |
	true.
filter_input(_, _, Service, _, []^) :-
    otherwise |
	computation # event(cant_trap(Service)).


procedure trap(InStream, Action, GoalSet, InStream).

trap(In, Action, Goals, Out) :-	

    In ? Request |
	trap1(Request, In', Action, Goals, Out);

    otherwise : Goals = _,	% usually In = []
      In = Out |
	close_action(Action).


procedure trap1(Goal, InStream, Action, GoalSet, OutStream).

trap1(CallGoal, In, Action, Goals, Out) :-

    CallGoal = export(_, _, untrap, CCC) : Goals = _,
      In = Out |
	closeCC(CCC),
	close_action(Action);

    CallGoal = export(_CallerId, _Scope, Goal, _CCC),
    Goal =\= untrap |
	trap2(Goal, Goals, Goals', {CallGoal, In, Action, Goals', Out});

    otherwise |			% ignore non-export (e.g. reduce)
	send({CallGoal, In, Action, Goals, Out}).


procedure trap2(Goal, GoalSet, GoalSet, ControlPackage).

trap2(Goal, Goals, Goals', Controls) :-

    true :
      Goals ! Goal |
	trap_signal(Goal, Controls);

    otherwise |
	trap3(Goal, Goals, Goals', Controls);

    Controls = {export(_, _, _, CCC), _, _, _, _},
    CCC = {Signals, _, _, _},	% Trap would have suspended - maybe deadlock.
    known(Signals) : Goal = _,	% No cheap decision in this case; let's
      Goals = Goals' |		% hope that the computation is aborting.
	send(Controls).


procedure trap3(Goal, GoalSet, GoalSet, ControlPackage).

trap3(Goal, Goals, Rest, Controls) :-

    Goals ? Other :		% it wouldn't unify above.
      Rest ! Other |
	trap2(Goal, Goals', Rest', Controls);

    true :
      Goal = Goals, 
      Rest = [] |
	trap_signal(Goal, Controls);

    otherwise : Goal = _,
      Goals = Rest |
	send(Controls).


procedure trap_signal(Goal, ControlPackage).

trap_signal(Goal, Controls) :-

    Controls = {Request, In, unify, Goals, Out} : Goal = _,
      Out ! Request |
	retrap(In, unify, Goals, Out');

    arg(3, Controls, suspend(Service)) |
	suspend(Controls, Service, Goal);

    Controls =	{Request, In,
		 interrupt(ServiceId, Interrupts),
		 Goals, Out
		} :
      Interrupts ! ServiceId#Goal |
	close_request(Request),
	retrap(In, interrupt(ServiceId, Interrupts'), Goals, Out);

    
    Controls =	{Request, In,
		 filter(ServiceId, Trapped, Halt),
		 Goals, Out
		} :
      Trapped = [trapped(ServiceId#Goal, Reply) | Trapped'?] |
	trapped(Reply, Halt, Request, Out, Out'),
	retrap(In, filter(ServiceId, Trapped', Halt), Goals, Out').


procedure send(ControlPackage).

send(Controls) :-
    Controls = {Request, In, Action, Goals, Out} :
      Out ! Request |
	retrap(In, Action, Goals, Out').


procedure suspend(ControlPackage, Service, Goal).

suspend(Controls, Service, Goal) :-

    Controls = {export(CallerId, Scope, _, CCC), In, Action, Goals, Out},
    CCC = {Signals, _, _, Channel} :
      Out ! export(CallerId, Scope, Goal', CCC),
      write_channel(request(CallerId, event(trapped(Service#Goal)),
				  true, True
		    ),
		    Channel
      ) |
	retrap(In, Action, Goals, Out'),
	synchronize_suspend(True, Signals, Goal, Goal').


procedure synchronize_suspend(true, Any, Goal, Goal).

synchronize_suspend(True, Signals, Goal, Instantiate) :-
    True = true,
    known(Signals) :
      Goal = Instantiate |
	true.


procedure trapped(Any, halt, WrappedGoal, InStream, InStream).

trapped(Reply, Halt, Request, Out1, Out2) :-

    Reply = true : Halt = _,
      Out1 = Out2 |
	close_request(Request);

    Reply =\= true, Reply =\= changed(_) : Halt = _,
      Out1 = [Request | Out2] |
	true;

    Reply = changed(NewGoal),
    Request = export(CallerId, Scope, _, CCC) : Halt = _,
      Out1 = [export(CallerId, Scope, NewGoal, CCC) | Out2] |
	true;

    Halt = halt :
      Reply = false,
      Out1 = [Request | Out2] |
	true.


procedure retrap(InStream, Action, GoalSet, InStream).

retrap(In, Action, Goals, Out) :-

    Goals = [] :
      In = Out |
	close_action(Action);

    list(In) |
	trap(In, Action, Goals, Out);

    In = [] : Goals = _,
      In = Out |
	close_action(Action).


procedure close_action(CloseAction).

close_action(interrupt(_, []^)).
close_action(filter(_, []^, halt^)).
close_action(_) :-
    otherwise |
	true.


procedure close_request(WrappedGoal).

close_request(Request) :-
    arg(5, Request, CCC) |
	closeCC(CCC).

% library

procedure closeCC(CCC).
