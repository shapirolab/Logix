/*

Computation Utilities
Bill Silverman  06/88

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:02:54 $
Currently locked by 	$Locker:  $
			$Revision: 1.1.1.1 $
			$Source: /home/qiana/Repository/Logix/system/computation_utils.cp,v $

Copyright (C) 1988, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language(compound).
-export([call_output/3, call_output/4, call_list/2, goal/3,
	 path_context/3, call_context_goal/4,
	 path_id/3, call_id_goal/4
	]).
-mode(failsafe).

ServiceId ::= [String].
SystemReply ::= true ; false(Any).

/*
** Start sub-computation, and display its output
*/

Input ::= Any.
Options ::= Any.
Output ::= [Any].

procedure call_output(Input, Output, Options).
procedure call_output(Input, Output, Options, SystemReply).

call_output(Input, Output, Options) + (SystemReply = _) :-
	computation # Display?,
	computation # [call(Signals, Events) | Relays?],
	continue_call(Input, Signals, Events, Reply),
	service_output(Events, done, Output, Options, Display, Relays,
			{Reply, SystemReply}
	).

Done ::= done.


procedure service_output(Events, Done, Output, Options,
			 Output, Events, Replies
).

service_output(Events, Done, Output, Options, Display, Relays, Replies) :-

    Events ? aborted : Events' = _, Done = _, Output = _, Options = _,
      Display = [],
      Relays = [],
      Replies = {_Reply, failed(aborted)} ;

    Events ? Event,
    constant(Event), Event =\= aborted |
	service_output;

    Events ? failed(call(_), _) |
	service_output;

    Events ? Relay,
    compound(Relay), Relay =\= failed(call(_),_) :
      Relays ! Relay |
	service_output;

    Output ? Comment, Comment = comment(_) :
      Relays ! Comment |
	service_output;

    Output ? Diagnostic, Diagnostic = diagnostic(_) :
      Relays ! Diagnostic |
	service_output;

    Output ? output(Term),
    Done = done :
      Display ! display(term, Term, [close(done, Done') | Options]) |
	service_output;

    Output ? Term,
    Term =\= comment(_), Term =\= diagnostic(_), Term =\= output(_),
    Done = done :
      Display ! display(term, Term, [close(done, Done') | Options]) |
	service_output;

    Output =\= [_ | _], Output =\= [] :
      Output' = [Output] |
	service_output;

    Output = [], Done = done, Events = [] : Options = _,
      Display = [],
      Relays = [],
      Replies = {Reply, Reply} .

/*
** Call the goal in context, if it is well-defined.
*/

procedure goal(Context, Goal, SystemReply).

Context, Goal ::= Any.

goal(Context, Goal, SystemReply) :-
	call_id_goal(Context # Goal, SId, Goal', Ok),
	goal_id,
	check_id_goal.

goal_id(Ok, Goal, GId, GoalOk) :-

    Ok = true,
    tuple(Goal),
    A := arity(Goal) - 1,
    arg(1, Goal, F) :
      GId = F/A,
      GoalOk = true;

    Ok = true,
    otherwise :
      GId = Goal/0,
      GoalOk = true;

    Ok =\= true :
      GId = Goal,
      GoalOk = Ok.

check_id_goal(SId, Goal, SystemReply, GId, GoalOk) :-

    GoalOk = true |
	call_list([SId#attributes(As)], _Ok),
	exports(As, Es),
	member(GId, Es, Ok),
	call_known_goal;
	
    GoalOk =\= true : SId = _, Goal = _, GId = _,
      SystemReply = GoalOk .

exports(As, Es) :-

    As ? export(Es^) : As' = _ ;

    As ? Other, Other =\= export(_) |
	self;

    As = [] :
      Es = [] .

member(A, As, Reply) :-

    As ? A : As' = _,
      Reply = true;

    As ? B,
    A =\= B |
	self;

    As =\= [_|_] : A = _,
      Reply = false(unknown) .

call_known_goal(SId, Goal, SystemReply, Ok) :-

    Ok = true :
      SystemReply = true |
	SId # Goal;

    Ok =\= true : SId = _, Goal = _,
      SystemReply = Ok .


/*
** Call the goals in the list, aborting on any failure.
*/

Goals ::= Any.

procedure call_list(Goals, SystemReply).

call_list(Goals, SystemReply) :-
	computation # call(Signals, Events),
	continue_call(Goals, Signals, Events, SystemReply).

Continue ::= SystemReply.
Signals ::= [Any].
Events ::= [Any].

procedure continue_call(Goals, Signals, Events, SystemReply).

continue_call(Goals, Signals, Events, SystemReply) :-

    Events ? Event |
	terminated(Event, Continue),
	event(Goals, Signals, Events', SystemReply, Continue);

    Goals = [] :
      Signals = [] |
	completed(Events, SystemReply);

    Goals ? Goal :
      Signals ! Goal |
	continue_call;

    Goals = known(Trigger, Goals'),
    known(Trigger) |
	continue_call.

Event ::= Any.

procedure event(Goals, Signals, Events, SystemReply, Continue).

event(Goals, Signals, Events, SystemReply, Continue) :-

    Continue = true |
	continue_call(Goals, Signals, Events, SystemReply);

    otherwise : Goals = _, Events = _,
      Signals = [abort],
      SystemReply = Continue .


procedure completed(Events, SystemReply).

completed(Events, SystemReply) :-

    Events ? Event |
	terminated(Event, Continue),
	terminating(Continue, Events', SystemReply);

    Events = [] :
      SystemReply = true .

procedure terminating(Continue, Events, SystemReply).

terminating(Continue, Events, SystemReply) :-

    Continue = true |
	completed(Events, SystemReply);

    otherwise : Events = _,
      Continue = SystemReply .

procedure terminated(Event, Continue).

terminated(Event, Continue) :-

    constant(Event), Event =\= aborted :
      Continue = true ;

    Event = comment(_) :
      Continue = true ;

    Event = diagnostic(_) :
      Continue = true ;

    Event = failed(call(_), _) :
      Continue = true ;

    Event = failed(_#_, Reason) :
      Continue = false(Reason) ;

    Event = event(Reason(_#_)) :
      Continue = false(Reason) ;

    Event = event(blocked_context(_)) :
      Continue = false(blocked_context) ;

    Event = event(Event'),
    Event' =\= blocked_context(_), Event' =\= _(_#_) :
      Continue = false(Event') ;

    otherwise :
      Continue = false(Event) .


ContextReply ::= Vector ; [].
Target ::= ContextReply.
Path ::= Any.

/*
** path_context/3
*/

procedure path_context(Path, ContextReply, SystemReply).

path_context(Path, ContextReply, SystemReply) :-
    true :
      Target? = ContextReply,
      Reply? = SystemReply |
	call_list([Path # service_id(Id)], Reply),
	result_context(Reply, Id, Target).


procedure result_context(SystemReply, ServiceId, ContextReply).

result_context(SystemReply, ServiceId, ContextReply) :-

    SystemReply = true :
      make_channel(ContextReply, Stream) |
	ServiceId # Stream? ;

    SystemReply =\= true :
      ContextReply = [],
      ServiceId = [] .

/*
** path_id/3
*/

procedure path_id(Path, ServiceId, SystemReply).

path_id(Path, ServiceId, SystemReply) :-
    true :
      Target? = ServiceId,
      Reply? = SystemReply |
	call_list([Path # service_id(Target)], Reply),
	result_id(Reply, Target).

procedure result_id(Reply, ServiceId).

result_id(Reply, ServiceId) :-

    Reply = true : ServiceId = _ ;

    otherwise : Reply = _,
      ServiceId = [] .

/*
** call_context_goal/4
*/

GoalReply ::= Any.

procedure call_context_goal(Path, ContextReply, GoalReply, SystemReply).

call_context_goal(Path, ContextReply, GoalReply, SystemReply) :-

    Path = Prefix # Goal,
    Goal =\= _#_ :
      Goal = GoalReply |
	path_context(Prefix, ContextReply, SystemReply);

    Path = Prefix # Goal,
    Goal = Segment # Suffix :
      Path' = (Prefix # Segment) # Suffix |
	call_context_goal;

    otherwise :
      Path = GoalReply,
      ContextReply = [],
      SystemReply = false(no_path) .


procedure call_id_goal(Path, ServiceId, GoalReply, SystemReply).

call_id_goal(Path, ServiceId, GoalReply, SystemReply) :-

    Path = Prefix # Goal,
    Goal =\= _#_ :
      Goal = GoalReply |
	path_id(Prefix, ServiceId, SystemReply);

    Path = Prefix # Goal,
    Goal = Segment # Suffix :
      Path' = (Prefix # Segment) # Suffix |
	call_id_goal;

    otherwise :
      Path = GoalReply,
      ServiceId = [],
      SystemReply = false(no_path) .
