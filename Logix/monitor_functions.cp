/*

Utility Procedures for Logix Monitors.

Bill Silverman

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:02:52 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/monitor_functions.cp,v $

Copyright (C) 1987, Weizmann Institute of Science - Rehovot, ISRAEL

*/
-export([monitor_events/1,monitor_events/2,choke_stream/3,choke_stream/4]).
-mode(trust).
-language(compound).

/*
 * monitor_events/1 - Monitor Events for failsafe/trusted process:
 *		      relay aborted to Common; ignore others;
 *		      quit when process is done.
 *
 * monitor_events(Common)
 *
 * monitor_events/2 - Monitor Events for failsafe/trusted process:
 *		      relay aborted; ignore others; quit when process is done.
 *
 * monitor_events(Done, Abort)
 *
 */

Done ::= Any.
Abort ::= abort.

procedure monitor_events(Done).
procedure monitor_events(Done, Abort).

monitor_events(Done) + (Abort = Done) :-
	computation # events(Events),
	monitor_events_stream(Done, Abort, Events).

Events ::= [Event].
Event ::= aborted ; resumed ; suspended.

procedure monitor_events_stream(Done, Abort, Events).

monitor_events_stream(_, abort^, [aborted | _]).
monitor_events_stream(Done, Abort, Events) :-
    known(Done) : Abort = _, Events = _ ;

    Events ? Event,
    Event =\= aborted |
	monitor_events_stream;

    Events = [] :
       Done = _, Abort = _.


/*
 * choke_stream/3 - monitor Events for failsafe/trusted process:
 *		    relay In stream elements to Out stream;
 *		    close Out stream if aborted; relay aborted to Common;
 *		    quit when process is Done.
 *
 * choke_stream(Common, In, Out)
 *
 * choke_stream/4 - monitor Events for failsafe/trusted process.
 *		    relay In stream elements to Out stream;
 *		    close Out stream if aborted; relay aborted to Abort;
 *		    quit when process is Done.
 *
 * choke_stream(Done, In, Out, Abort)
 *
 */

In, Out ::= [Any].

procedure choke_stream(Done, In, Out).
procedure choke_stream(Done, In, Out, Abort).

choke_stream(Done, In, Out) + (Abort = Done) :-
	computation#events(Events),
	events_choke_stream(Done, In, Out, Abort, Events).

procedure events_choke_stream(Done, In, Out, Abort, Events).

events_choke_stream(Done, In, Out, Abort, Events) :-
    Events ? Event |
	interrupt_stream(Done, In, Out, Abort, Events', Event);

    In ? Term,
    known(Term) :
      Out ! Term |
	events_choke_stream;

    In =\= [_|_] :
      In = Out |
	monitor_events_stream(Done, Abort, Events).

events_choke_stream(Done, Out, Out^, _, _) :-
    known(Done) |
	true.
events_choke_stream([], Out, Out^, _, _).

procedure interrupt_stream(Done, In, Out, Abort, Events, Event).

interrupt_stream(_, _, []^, abort^, _, aborted).
interrupt_stream(Done, In, Out, Abort, Events, Event) :-
    Event = suspended |
	resume_stream(Done, In, Out, Abort, Events);

    Event =\= aborted, Event =\= suspended |
	events_choke_stream(Done, In, Out, Abort, Events).
interrupt_stream(Done, Out, Out^, _, _, _) :-
    known(Done) |
	true.

procedure resume_stream(Done, In, Out, Abort, Events).

resume_stream(_, _, []^, abort^, [aborted|_]).
resume_stream(Done, In, Out, Abort, Events) :-
    Events ? resumed |
	events_choke_stream(Done, In, Out, Abort, Events');

    Events ? Event,
    Event =\= resumed, Event =\= aborted |
	resume_stream.
resume_stream(Done, Out, Out^, _, _) :-
    known(Done) |
	true.
