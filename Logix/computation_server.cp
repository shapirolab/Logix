/*

Computation Server of Logix System

Michael Hirsch and Bill Silverman

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:02:51 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/computation_server.cp,v $

Copyright (C) 1989, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([computation/4]).
-language(dfcp).
-mode(trust).

Stream ::= [Any].
List ::= [Any].
CCC ::= {Stream, Any, Any, Stream}.
SystemReply ::= true ; false(Any).

Channel ::= Vector.

SubOut ::= [failed(Any, Any),
	    request(ServiceId, Goal, Left, Right),
	    call(Stream, Stream1),
	    comment(Any),
	    diagnostic(Any),
	    event(Any)].

/*****************************************************************************/
	
/*
 *  Fcp meta call: initiate computation, providing (input) control
 *  and (output) monitoring streams,  Requests  and  Events .
 *
 *	Requests carries messages:
 *
 *		'abort', 'suspend', 'resume',
 *		events(Events), state(State)
 *
 *	RPCs are delegated to the current context.
 *      Others are delegated to the super-computation.
 *
 *	Events carries messages:
 *
 *		'failed(Goal, Reason)', 'event(Event)',
 *		'comment(Comment)', 'diagnostic(Diagnostic)',
 *		'terminated', 'suspended', 'resumed', 'aborted'.
 *
 * monitor the execution, control signals and requests
 *
 * computation(	Requests,	% Input
 *		{SuperControl,	% Input
 *		 SuperLeft,	% Input
 *		 SuperRight,	% Output
 *		 SuperChannel
 *		}
 *		Domain,		% Channel
 *		Events,		% Output
 * )
 */

SCC ::= CCC.

procedure computation(Requests, SCC, Domain, Events).
procedure computation(Requests, SCC, Domain, Events, Identifier, Scope).

computation(Requests, CCC, Domain, Events) :-
	info(5, Identifier),
	Scope = [],
	computation1.

computation1(Requests, CCC, Domain, Events, Identifier, Scope) :-
    CCC = {SuperControl, SuperLeft, SuperRight, SuperChannel},
    known(Requests) |
	SubOut = SubChannel?,
	SuperCCC = {SuperLeft, SuperRight, SuperChannel, Domain},
	SubCCM = {SubSignals, SubSignals?, SubChannel!},
	CompState = {running, Identifier, Scope, running},
	call_monitor.

/*
 * call_monitor/7
 *
 * main logic process - must decide what to do with each interrupt relative
 * to computation state rather than changing state explicitly, since there
 * are two possible sources of suspend, resume and abort signals.
 *
 * call_monitor(Requests,	% Input
 *		SuperControl,	% Input
 *		SubOut,		% Input
 *		SuperCCC,
 *		SubCCM,
 *		Events,
 *		CompState
 * )
 *
 * SuperCCC  = {SuperLeft?, SuperRight!, SuperChannel!, Domain!},
 *
 * SubCCM    = {SubSignals^, SubSignals, SubLeft^, SubLeft, SubRight, SubChannel!} ;
 *             {SubSignals^, SubSignals, SubChannel!}	% initial/terminal state
 * CompState = {UserStatus, Identifier, Scope, SuperStatus} ;
 *	       aborted(Identifier, Scope)
 *
 */

procedure call_monitor(Requests, SuperControl, SubOut, SuperCCC, SubCCM,
				Events, CompState
).

call_monitor(Requests, SuperControl, SubOut, SuperCCC, SubCCM, Events,
		CompState
) :-			

    Requests ? Request,					/* from user */
    known(Request) |
	call_monitor_request_interrupt;

    SuperControl ? Signal,				/* from super */
    known(Signal) |
	call_monitor_super_signal;

    SubOut ? Link,
    Link = link(Name, _, _),				/* from sub-process */
    listener(SubCCM) |
	sub_goalCH,
	redelegate(linked(Name, CH?, Link), SuperCCC, SuperCCC'),
	self;

    SubOut ? request(From, Event, Latch, Latch^) |	/* from sub-process */
	call_monitor_sub_channel;

    SubOut ? filter(SubOut'', SubOut') |
	self;

    SubOut ? Delegated,
    Delegated = delegated(_, _) |		/* from sub-computation */
	redelegate(Delegated, SuperCCC, SuperCCC'),
	self;

    SubOut ? Linked,
    Linked = linked(_, _, _) |			/* from sub-computation */
	redelegate(Linked, SuperCCC, SuperCCC'),
	self;
			
    SubOut ? Event,					/* from ?? */
    Event =\= filter(_, _),
    Event =\= link(_, _, _),
    Event =\= request(_, _, _, _),
    Event =\= delegated(_, _),
    Event =\= linked(_, _, _) |
	From = [computation],
	call_monitor_sub_channel;

    SuperCCC = {SuperLeft, SuperRight, SuperChannel, Domain},
    SuperLeft ? Request,				/* super left sc */
    known(Request) |
	call_monitor_super_left;

    Requests = [],
    SubCCM = {WriteSignals, ReadSignals, WriteLeft,
		ReadLeft, SubRight, SubChannel} |
	WriteLeft ! done,
	call_monitor_done(SubRight, SubRight'),
	SubCCM' = {WriteSignals, ReadSignals, [done | WriteLeft'],
			ReadLeft, SubRight'?, SubChannel},
	
	Requests' = done,				/* end of requests */
	self;

    SubCCM = {WriteSignals, ReadSignals, WriteLeft,
		ReadLeft, SubRight, SubChannel},
    ReadSignals ? _ |
	SubCCM' = {WriteSignals, ReadSignals', WriteLeft,
			ReadLeft, SubRight, SubChannel},
	self;

    SubCCM = {WriteSignals, ReadSignals, WriteLeft,
		ReadLeft, SubRight, SubChannel},
    ReadLeft ? _ |
	SubCCM' = {WriteSignals, ReadSignals, WriteLeft,
			ReadLeft', SubRight, SubChannel},
	self;

    SubCCM = {WriteSignals, ReadSignals, _WriteLeft,
		Closed, Closed, SubChannel} |
	call_monitor_terminated;			/* terminated */    

    SubCCM = {SubSignals, _SubSignals, SubChannel},
    Requests =\= [_ | _] |				/* done */
	SuperControl = _, SubOut = _, CompState = _,
	Events = [],
	unify_without_failure(SubSignals, []),
	close_channel(SubChannel),
	closeSuperCCC(SuperCCC).

procedure call_monitor_terminated(Requests, SuperControl, SubOut, SuperCCC,
		WriteSignals, ReadSignals, SubChannel, Events, CompState
).

call_monitor_terminated(Requests, SuperControl, SubOut, SuperCCC,
		WriteSignals, ReadSignals, SubChannel, Events, CompState
) :-

    CompState = aborted(_Identifier, _Scope) |
	Requests = _, SuperControl = _, SubOut = _,
	WriteSignals = _, ReadSignals = _,
	Events = [],
	close_channel(SubChannel),
	closeSuperCCC(SuperCCC);		% already aborted

    otherwise |		% continue control until  Requests  done or "abort".
	Events ! terminated,
	SubCCM = {WriteSignals, ReadSignals, SubChannel},
	call_monitor.

procedure closeSuperCCC(SuperCCC).

closeSuperCCC(CCC) :-
   CCC = {Left, Right, _, _} |
	Right = Left.

/*
 * resume logic - a subcomputation may be resumed iff both user and
 * supercomputation will be running
 *
 * call_monitor_resume/7
 */

procedure call_monitor_resume(CompState, Requests, SuperControl, SubOut,
			      SuperCCC, SubCCM, Events
).

call_monitor_resume(CompState, Requests, SuperControl, SubOut, SuperCCC,
			SubCCM, Events
) :-
    CompState = {running, _Identifier, _Scope, running},
    SubCCM = {WriteSignals, ReadSignals, WriteLeft,
		ReadLeft, SubRight, SubChannel} |
	WriteSignals ! resume,
	SubCCM' = {WriteSignals', ReadSignals, WriteLeft,
			ReadLeft, SubRight, SubChannel},
	call_monitor;

    otherwise |
	call_monitor.

/*
 * suspend logic - a subcomputation must be suspended iff both user and
 *		   supercomputation were running.
 *
 * call_monitor_suspend/8
 */

procedure call_monitor_suspend(SState, Requests, SuperControl, SubOut,
				SuperCCC, SubCCM, Events, CompState
).

call_monitor_suspend(SState, Requests, SuperControl, SubOut, SuperCCC,
			SubCCM, Events, CompState
) :-
    SState = {running, running},
    SubCCM = {WriteSignals, ReadSignals, WriteLeft,
		ReadLeft, SubRight, SubChannel} |
	WriteSignals ! suspend,
	SubCCM' = {WriteSignals', ReadSignals, WriteLeft,
			ReadLeft, SubRight, SubChannel},
	call_monitor;

    otherwise |
	SState = _,
	call_monitor.

/*
 * abort logic - a subcomputation can always be aborted, but control
 *               continues until it has terminated.
 *
 * call_monitor_abort/6
 */

procedure call_monitor_abort(SubOut, SuperCCC, SubCCM, Events, CompState).

call_monitor_abort(SubOut, SuperCCC, SubCCM, Events, CompState) :-
    SuperCCC = {Left, Right, _, Domain} |
	Right = Left,
	call_monitor_aborted.

procedure call_monitor_aborted(SubOut, SubCCM, Domain, Events, CompState).

call_monitor_aborted(SubOut, SubCCM, Domain, Events, CompState) :-

    SubCCM = {[abort]^, _, _, _, _, _},
    CompState = aborted(_Identifier, _Scope) |
	Events ! aborted,
	Requests = None?, SuperControl = None,
	SuperCCC = {Done?, Done, [], Domain},
	call_monitor;

    SubCCM = {[abort]^, _, _, _, _, _},
    CompState = _(Identifier, Scope, _) |
	Events ! aborted,
	Requests = None?, SuperControl = None,
	SuperCCC = {Done?, Done, [], Domain},
	CompState' = aborted(Identifier, Scope),
	call_monitor;

    SubCCM = {SubSignals, _SubSignals, SubChannel} |
	SubOut = _, Domain = _, CompState = _,
	Events = [],				% already terminated
	unify_without_failure(SubSignals, []),
	close_channel(SubChannel).

/*
 * other goals - recognize object-oriented goals from requests/sub-channel
 *
 * call_monitor_goal/8
 *
 */

procedure call_monitor_goal(Goal, Requests, SuperControl, SubOut, SuperCCC,
				SubCCM, Events, CompState
).

call_monitor_goal(Goal, Requests, SuperControl, SubOut, SuperCCC,
			SubCCM, Events, CompState
) :-

    Goal = Service # Request,
    listener(CompState),
    SuperCCC = {_, _, _, Domain} |
	call_monitor_add(Service, Request, Domain, CompState, SubCCM, SubCCM'),
	call_monitor;

    Goal = events(CopiedEvents),
    listener(CompState),
    listener(SubCCM) |
	_ = Identifier?,
	CopiedEvents ! Status?,
	call_monitor_state,
	copy_listener(Events'?, Events, CopiedEvents'),
	call_monitor;

    Goal = change_scope(New, Ok) |
	unify_without_failure(Reply?, Ok),
	call_monitor_scope(New?,	% kluged - change shell!
			   Reply, Requests, Requests', SubOut, SubOut',
				CompState, CompState'
	),
	call_monitor;

    Goal = '_controls'(CCC?),
    listener(CompState) |
	sub_goalCC(CompState, SubCCM, CCC, SubCCM'),
	call_monitor;

    Goal = '_domain'(Command),
    SuperCCC = {_, _, _, Domain},
    listener(Domain) |
	domain_command,
	call_monitor;

    Goal = '_hierarchy'(Command),
    SuperCCC = {_, _, _, Domain},
    listener(Domain) |
	Command' = hierarchy(Command),
	domain_command,
	call_monitor;

     otherwise,
     listener(SuperCCC),
     listener(CompState) |
	delegate(Goal, SuperCCC, CompState, SubCCM, SubCCM'),
 	call_monitor.


procedure call_monitor_state(SubCCM, CompState, Status, Identifier).

call_monitor_state(SubCCM, CompState, Status, Identifier) :-

    SubCCM = {_,_,_},
    CompState = {_UserStatus, ComputationId, _Scope, _SuperStatus} |
	Status = terminated,
	Identifier = ComputationId ;

    otherwise : SubCCM = _ |
	call_monitor_state1.

procedure call_monitor_state1(CompState, Status, Identifier).

call_monitor_state1(CompState, Status, Identifier) :-

    CompState = {_UserStatus, ComputationId, _Scope, _SuperStatus} |
	Identifier = ComputationId,
	Status = suspended ;


    CompState = aborted(ComputationId, _Scope) |
	Identifier = ComputationId,
	Status = aborted ;

    CompState = {running, ComputationId, _Scope, running} |
	Identifier = ComputationId,
	Status = resumed ;

    CompState = {_UserStatus, ComputationId, _Scope, _SuperStatus},
    otherwise |
	Identifier = ComputationId,
	Status = suspended .

/*
 * change scope - block Requests and SubOut until scope is ground
 *
 * call_monitor_scope/6
 */

procedure call_monitor_scope(New, Reply, Requests, Requests,
			SubOut, SubOut, CompState, CompState
).
procedure call_monitor_scope(New, Reply, Requests, Requests, 
		SubOut, SubOut, CompState, CompState, List
).

call_monitor_scope(New, Reply, Requests1, Requests2, 
		SubOut1, SubOut2, CompState1, CompState2
) + (List) :-

    initially, listener(New) | List = New;

    List ? String,
    string(String) |
	self;

    List = [],
    CompState1 = {UserState, Identifier, _Old, SuperState} |
	Reply = true,
	Requests2 = Requests1,
	SubOut2 = SubOut1,
	CompState2 = {UserState, Identifier, New, SuperState} ;

    otherwise |
	New = _, List = _,
	Reply = false(invalid),
	Requests2 = Requests1,
	SubOut2 = SubOut1,
	CompState2 = CompState1 .

/*
 * add goal - a goal can always be added to a subcomputation
 *
 * call_monitor_add/6
 */

procedure call_monitor_add(Service, Goal, Channel, CompState, SubCC, SubCC).

call_monitor_add(Service, Goal, Domain, CompState, SubCCM, ExtendedCCC):-
    CompState = {_UserState, _Identifier, Scope, _SuperState},
    listener(Scope) |
	Command = export([computation | Scope], Scope, Service # Goal, CCC?),
	sub_goalCC(CompState, SubCCM, CCC, ExtendedCCC),
	domain_command.

/*
 * interpret user request stream
 *
 * call_monitor_request_interrupt(Request?, Requests?, SuperControl,
 *		SubOut, SuperCCC, SubCCM, Events, CompState)
 */

procedure call_monitor_request_interrupt(Request, Requests, SuperControl,
		SubOut, SuperCCC, SubCCM, Events, CompState
).

call_monitor_request_interrupt(Request, Requests, SuperControl,
			       SubOut, SuperCCC, SubCCM, Events, CompState
) :-
    Request = abort |
	Requests = _, SuperControl = _,
	call_monitor_abort;

    Request = suspend,
    CompState = {UserStatus, Identifier, Scope, SuperStatus},
    listener(SuperStatus) |
	Events ! suspended,
	SState = {UserStatus, SuperStatus},
	CompState' = {waiting, Identifier, Scope, SuperStatus},
	call_monitor_suspend;

    Request = resume,
    CompState = {_UserStatus, Identifier, Scope, SuperStatus} |
	Events ! resumed,
	CompState' = {running, Identifier, Scope, SuperStatus},
	call_monitor_resume;

    Request = state(Resolvent),
    SubCCM = {WriteSignals, ReadSignals, WriteLeft,
		ReadLeft, SubRight, SubChannel} |
	Resolvent = Resolvent'?,
	call_monitor,
	WriteLeft ! state(Resolvent'),
	call_monitor_resolvent(SubRight, SubRight'),
	SubCCM' = {WriteSignals, ReadSignals, WriteLeft',
			ReadLeft, SubRight'?, SubChannel};

    Request = extract(N, Goal?),
    SubCCM = {WriteSignals, ReadSignals, WriteLeft,
		ReadLeft, SubRight, SubChannel} |
	WriteLeft ! extract(N),
	call_monitor,
	call_monitor_reply(SubRight, SubRight', Goal),
	SubCCM' = {WriteSignals, ReadSignals, WriteLeft',
			ReadLeft, SubRight'?, SubChannel};

    Request = identifier(Identifier) |
	Request' = identifier(Identifier, _),
	self;

    Request = identifier(Identifier', Identifier),
    CompState = {UserStatus, ComputationId, Scope, SuperStatus} |
	Identifier = ComputationId,
	CompState' = {UserStatus, Identifier', Scope, SuperStatus},
	call_monitor;

    Request = delegate_channel(SuperChannel', SuperChannel^),
    SuperCCC = {SuperLeft, SuperRight, SuperChannel, Domain} |
	SuperCCC' = {SuperLeft, SuperRight, SuperChannel', Domain},
	call_monitor;

    otherwise |
	Goal = Request,
	call_monitor_goal.

call_monitor_resolvent(Left, Right) :-
    Left ? state(Resolvent) |
	Resolvent = [],
	Right = Left' .

call_monitor_done(Left, Right) :-
    Left ? done |
	Right = Left' .

%procedure call_monitor_reply(Reply, EGoal).

call_monitor_reply(Left, Right, Goal) :-

    Left ? found(Predicate) |
	Right = Left',
	Goal = Predicate;

    Left ? extract(_) |
	Goal = not_found,
	Right = Left'.

/*
 * interpret supercomputation signal stream
 *
 * call_monitor_super_signal(Requests, Signal?, SuperControl?, SubOut,
 *		SuperCCC, SubCCM, Events, CompState)
 */

procedure call_monitor_super_signal(Requests, Signal, SuperControl, SubOut,
					SuperCCC, SubCCM, Events, CompState
).

call_monitor_super_signal(Requests, Signal, SuperControl, SubOut,
			  SuperCCC, SubCCM, Events, CompState
) :-
    Signal = abort |
	Requests = _, SuperControl = _,
	call_monitor_abort;

    Signal = suspend,
    CompState = {UserStatus, Identifier, Scope, SuperStatus},
    listener(UserStatus) |
	SState = {UserStatus, SuperStatus},
	CompState' = {UserStatus, Identifier, Scope, waiting},
	call_monitor_suspend;

    Signal = resume,
    CompState = {UserStatus, Identifier, Scope, _SuperStatus} |
	CompState' = {UserStatus, Identifier, Scope, running},
	call_monitor_resume;

    otherwise,
    listener(CompState) |
	super_request(rejected(Signal, unknown_signal), CompState,
			SuperCCC, SuperCCC'
	),
	call_monitor.

/*
 * interpret supercomputation left short circuit
 *
 * call_monitor_super_left(Requests, SuperControl, SubOut,
 *			   Request?, SuperLeft?, SuperRight, SuperChannel,
 *			   Domain, SubCCM, Events, CompState
 * )
 */

procedure call_monitor_super_left(Requests, SuperControl, SubOut,
				  Request, SuperLeft, SuperRight, SuperChannel,
				  Domain, SubCCM, Events, CompState
).

call_monitor_super_left(Requests, SuperControl, SubOut,
			Request, SuperLeft, SuperRight, SuperChannel,
			Domain, SubCCM, Events, CompState
) :-

    Request = state((Resolvent)),
    listener(CompState),
    listener(SubCCM) |
	copy_listener(Events'?, Events, CopiedEvents),
	Resolvent ! call(Identifier?, [Status? | CopiedEvents?]),
	SuperRight ! state(Resolvent'),
	SuperCCC = {SuperLeft, SuperRight', SuperChannel, Domain},
	call_monitor_state,
	call_monitor;

    Request = extract(1),
    listener(CompState),
    listener(SubCCM) |
	SuperRight ! found(call(Identifier?)),
	SuperCCC = {SuperLeft, SuperRight', SuperChannel, Domain},
	_ = Status?,
	call_monitor_state,
	call_monitor;

    Request = extract(N),
    N-- =\= 1 |
	SuperRight ! extract(N'),
	SuperCCC = {SuperLeft, SuperRight', SuperChannel, Domain},
	call_monitor;

    otherwise |
	SuperRight ! Request,
	SuperCCC = {SuperLeft, SuperRight', SuperChannel, Domain},
	call_monitor.

/*
 * interpret events/exceptions/internal requests from the subcomputation
 *
 * call_monitor_sub_channel(Requests, SuperControl, From?, Event?, SubOut?,
 *			SuperCCC, SubCCM, Events, CompState)
 */

procedure call_monitor_sub_channel(Requests, SuperControl, From, Event, SubOut,
				   SuperCCC, SubCCM, Events, CompState
).

call_monitor_sub_channel(Requests, SuperControl, From, Event, SubOut,
			 SuperCCC, SubCCM, Events, CompState
) :-

    Event = failed(Goal, Reason),
    listener(CompState),
    listener(SubCCM) |
	Events ! Event1?,
	_ = Status?,
	fullid_goal(From, Goal, Goal'),
	copy_listener(failed(Goal'?, Reason), Event1, Event2),
	call_monitor_state,
	super_request(failed(call(Identifier?), Event2?),
			CompState, SuperCCC, SuperCCC'
	),
	call_monitor;

    Event = comment(_) |
	From = _,
	Events ! Event,
	call_monitor;

    Event = diagnostic(_) |
	From = _,
	Events ! Event,
	call_monitor;

    Event = event(_) |
	From = _,
	Events ! Event,
	call_monitor;
/*
    arg(1, Event, error) : From = _,	% ***** temporary kluge *****
      Events = [event(Event) | Events'?] |
	call_monitor;
*/
    Event = call(CallRequests, CallEvents),
    SuperCCC = {_, _, _, Domain},
    CompState = {_UserStatus, Identifier, Scope, _SuperStatus},
    listener(CompState),
    listener(SubCCM) |
	info(5, Now),
	CallEvents = CallEvents'?,
	From = _,
	Identifier' = (Now?, Identifier),
	sub_goalCC(CompState, SubCCM, CCC, SubCCM'),
	computation1(CallRequests, CCC?, Domain, CallEvents', Identifier'?, Scope),
	call_monitor;

    Event = clause(Goal, True),
    listener(CompState) |
	call_monitor_recall(Goal, From, CompState, SubCCM, SubCCM'),
	call_monitor,
	True = true;

    Event = clause(Goal, True, Controls),
    listener(CompState) |
	call_monitor_recall(Goal, From, CompState, SubCCM, SubCCM'),
	call_monitor,
	True = true,
	clause_controls(Controls);

    Event = clause(Goal, true^, 0^, Time?),
    listener(CompState) |
	info(5, Time),
	call_monitor_recall(Goal, From, CompState, SubCCM, SubCCM'),
	call_monitor;

    otherwise |
	From = _,
	Goal = Event,
	call_monitor_goal.

procedure fullid_goal(Id, Goal, Goal1).

fullid_goal(Id, Goal, Goal1) :-
    Goal = _#_ |
	Id = _,
	Goal1 = Goal ;

    Id = [computation | _],
    Goal = call(_) |
	Goal1 = Goal ;

    otherwise |
	Goal1 = Id # Goal .

copy_listener(L, L1, L2) :-
    listener(L) |
	L1 = L,
	L2 = L .

procedure clause_controls(ClauseControls).

clause_controls(Controls) :-

    Controls = Suspense(Id, Result) |
	info(5, Time),
	Id = 0,
	unify_without_failure(Suspense, reduce),
	unify_without_failure(Result, Time?);

    otherwise |
	Controls = _ .


procedure call_monitor_recall(Goal, From, CompState, SubCCM, ExtendedCCC).

call_monitor_recall(Goal, From, CompState, SubCCM, ExtendedCCC) :-
	Goals = Goal,
	sub_goalCC(CompState, SubCCM, CCC, ExtendedCCC),
	unify_without_failure(CCC?, {CO, CL, CR?, CC}),
	serve_computation_goals.

procedure serve_computation_goals(Goals, CL, CR, CO, From, CC).

serve_computation_goals(Goals, CL, CR, CO, From, CC) :-

    Goals = true |
	CO = _, From = _, CC = _,
	CL = CR;

    Goals = [] |
	CO = _, From = _, CC = _,
	CL = CR;

    CO ? abort |
	CO' = _, Goals = _, From = _, CC = _,
	CR = CL;

    Goals =\= [_,_], Goals =\= [], Goals =\= true,
    channel(CC) |
	CO = _,
	write_channel(request(From, Goals, CL, CR), CC) ;

    Goals ? Goal,
    listener(CO),
    listener(CC),
    listener(From) |
	serve_computation_goals(Goal, CL, CL', CO, From, CC),
	self;

    CO ? Other, Other =\= abort |
	self;

    CL ? Mss |
	CR ! Mss,
	self.

/*****************************************************************************/

procedure domain_command(Command, Channel).

domain_command(Command, Domain) :-

    channel(Domain) |
      write_channel(Command, Domain) ;

    otherwise |
	Command = _, Domain = _ .      % Domain Channel closed ?


procedure super_request(Message, CompState, SuperCCC, SuperCCC1).

super_request(Message, CompState, SuperCCC1, SuperCCC2) :-

    SuperCCC1 = {SL, SR, SC, Domain},
    CompState = {_UserState, _Identifier, Scope, _SuperState},
    channel(SC) |
	write_channel(request([computation | Scope], Message, SL, SM), SC),
	SuperCCC2 = {SM?, SR, SC, Domain} ;

    otherwise |
	Message = _, CompState = _,		% aborted  or
	SuperCCC2 = SuperCCC1 .			% Super Channel closed?


procedure delegate(Request, SuperCCC, CompState, SubCCM, SubCCM).

delegate(Request, SuperCCC, CompState, SubCCM, ExtendedCCC) :-

    SuperCCC = {_, _, SC, _},
    channel(SC) |
	write_channel(delegated(Request, CCC?), SC),
	sub_goalCC(CompState, SubCCM, CCC, ExtendedCCC);

    otherwise |					% Super Channel closed?
	Request = _, SuperCCC = _, CompState = _,
	ExtendedCCC = SubCCM .


procedure redelegate(Request, SubCCM, SuperCCC, SuperCCC).

redelegate(Command, SuperCCC1, SuperCCC2) :-

    SuperCCC1 = {_, _, SC, _},
    channel(SC) |
	write_channel(Command, SC),
	SuperCCC2 = SuperCCC1 ;
 
    otherwise |                  		% Super Channel closed ?
	Command = _,
	SuperCCC2 = SuperCCC1 .


sub_goalCC(CompState, SubCCM, CCC, ExtendedCCC) :-

    CompState =\= {running, _, _, running},
    CompState =\= aborted(_, _),
    SubCCM = {WS, RS, WL, RL, M, C},
    listener(RS),
    listener(C) |
	CCC = {[suspend | RS], M, R, C},
	ExtendedCCC = {WS, RS, WL, RL, R?, C} ;

    SubCCM = {WS, RS, WL, RL, M, C},
    otherwise,
    listener(RS),
    listener(C) |
	CompState = _,
	CCC = {RS, M, R, C},
	ExtendedCCC = {WS, RS, WL, RL, R?, C} ;

    CompState =\= {running, _, _, running},
    CompState =\= aborted(_, _),
    SubCCM = {WS, RS, C},
    listener(RS),
    listener(C) |
	CCC = {[suspend | RS], RL1?, R, C},
	copy_listener(WL?, RL, RL1),
	ExtendedCCC = {WS, RS, WL, RL?, R?, C} ;

    otherwise,
    SubCCM = {WS, RS, C},
    listener(RS),
    listener(C) |
	CompState = _,
	CCC = {RS, RL1?, R, C},
	copy_listener(WL?, RL, RL1),
	ExtendedCCC = {WS, RS, WL, RL?, R?, C} .


procedure sub_goalCH(SubCCC, Channel).

sub_goalCH(SubCCM, CH) :-

    SubCCM = {_S, _RS, _L, _RL, _R, C} |
	CH = C ;

    SubCCM = {_S, _RS, C} |
	CH = C .
