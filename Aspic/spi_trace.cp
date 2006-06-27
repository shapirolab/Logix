/*

SpiFcp Trace channel activity from monitor debug output
William Silverman

Last update by          $Author: bill $
                        $Date: 2006/06/27 08:49:52 $
Currently locked by     $Locker:  $
                        $Revision: 1.7 $
                        $Source: /home/qiana/Repository/Aspic/spi_trace.cp,v $

Copyright (C) 2000, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language([evaluate,compound,colon]).
-mode(trust).
-export([run/2, run/3, run/4, run/5]).

-include(spi_constants).

REALTIME => 12.

START => '+'.
END => '-'.
SEND => '->'.
RECEIVE => '<-'.
SEPARATOR => ':'.

run(Goal, Limit) :-

    Goal =?= _#_,
    Limit >= 0 |
	computation # spi_monitor # scheduler(Scheduler),
	write_channel(cutoff(Limit, _State), Scheduler),
	computation#Goal;

    otherwise |
	fail(run(Goal, Limit)).

run(Goal, File, Limit) :-
	runit + (Scale = 1, Format = none).

run(Goal, File, Limit, Arg) :-
    number(Arg) |
	runit + (Scale = Arg, Format = none);
    otherwise |
	format_arg + (Scale = 1, Format = Arg).

run(Goal, File, Limit, Scale, Format) :-
    number(Scale) |
	format_arg.
run(Goal, File, Limit, Format, Scale) :-
    number(Scale) |
	format_arg.

format_arg(Goal, File, Limit, Scale, Format) :-
    Format = none |
	runit;
    Format = process |
	runit;
    Format = creator |
	runit;
    Format = full |
	runit;
    otherwise :
      Limit = _,
      File = _,
      Goal = _,
      Scale = _ |
	fail("Unrecognized format" - Format).

runit(Goal, File, Limit, Scale, Format) :-

    Goal =?= _#_,
    string(File), File =\= "",
    Limit >= 0,
    convert_to_real(Scale, Scale'),
    0.0 < Scale' |
	computation # spi_monitor # scheduler(Scheduler),
	write_channel(debug(Stream), Scheduler, Scheduler'),
	write_channel(cutoff(Limit, _State), Scheduler'),
	computation#[Goal, events(Events)],
	computation # spi_utils # show_value(Values?, [Style, 3], Processes),
	synchronize_output,
	screen#display_stream(Out?, [put(File), width(10000)]),
	filter_data;

    otherwise |
	fail("Bad argument" - run(Goal, File, Limit, Scale, Format)).

synchronize_output(Processes, Out) :-

    Processes ? Process :
      Out ! Process |
	self;

    Processes =\= [_ | _] :
      Out = Processes.

filter_data(Stream, Events, Scale, Format, Style, Values) :-

    Format =?= none :
      Style = short |
	filter_none;

    Format =?= process :
      Style = short |
 	filter_process;

    Format =?= creator :
      Style = creator |
 	filter_creator;

    Format =?= full :
      Style = creator |
 	filter_full;

    otherwise :
      Events = _,
      Scale = _,
      Stream = _,
      Style = none,
      Values = [] |
	fail(invalid_format(Format)).

filter_none(Stream, Events, Scale, Values) :-

    Stream ? start(Process) :
      Values ! START(Process) |
	self;

    Stream ? done(Now, Sender(_SendName, _SendChannelId),
	               Receiver(_ReceiveName, _ReceiveChannelId)),
    Scaled := Now*Scale :
      Values ! Scaled,
      Values' ! END(Sender), 
      Values'' ! END(Receiver) |
	self;

    Stream ? _Element,
    otherwise |
	self;

    Stream =\= [_ | _] :
      Events = _,
      Scale = _,
      Values = Stream;

    Events ? Event,
    Event =\= aborted |
	self;

    unknown(Stream),
    Events ? aborted :
      Events' = _,
      Scale = _,
      Values = [].

filter_process(Stream, Events, Scale, Values) :-

    Stream ? start(Process) :
      Values ! START(Process) |
	self;

    Stream ? done(Now, Sender(SendName, _SendCreatedId),
	               Receiver(ReceiveName, _ReceiveCreatedId)),
    Scaled := Now*Scale :
      Values ! Scaled,
      Values' ! SEND(END(Sender), SendName), 
      Values'' ! RECEIVE(END(Receiver), ReceiveName) |
	self;

    Stream ? _Element,
    otherwise |
	self;

    Stream =\= [_ | _] :
      Events = _,
      Scale = _,
      Values = Stream;

    Events ? Event,
    Event =\= aborted |
	self;

    unknown(Stream),
    Events ? aborted :
      Events' = _,
      Scale = _,
      Values = [].


filter_creator(Stream, Events, Scale, Values) :-

    Stream ? start(Process) :
      Values ! START(Process) |
	self;

    Stream ? done(Now, Sender(_SendName, SendCreatedId),
	               Receiver(_ReceiveName, ReceiveCreatedId)),
    Scaled := Now*Scale :
      Values ! Scaled,
      Values' ! SEND(END(Sender), SendCreatedId), 
      Values'' ! RECEIVE(END(Receiver), ReceiveCreatedId) |
	self;

    Stream ? _Element,
    otherwise |
	self;

    Stream =\= [_ | _] :
      Events = _,
      Scale = _,
      Values = Stream;

    Events ? Event,
    Event =\= aborted |
	self;

    unknown(Stream),
    Events ? aborted :
      Events' = _,
      Scale = _,
      Values = [].


filter_full(Stream, Events, Scale, Values) :-


    Stream ? start(Process) :
      Values ! START(Process) |
	self;

    Stream ? done(Now, Sender(SendName, SendCreatedId),
	               Receiver(ReceiveName, ReceiveCreatedId)),
    Scaled := Now*Scale :
      Values ! Scaled,
      Values' ! SEND(END(Sender),
		     SEPARATOR(SendName, SendCreatedId)), 
      Values'' ! RECEIVE(END(Receiver), 
			 SEPARATOR(ReceiveName, ReceiveCreatedId)) |
	self;

    Stream ? _Element,
    otherwise |
	self;

    Stream =\= [_ | _] :
      Events = _,
      Scale = _,
      Values = Stream;

    Events ? Event,
    Event =\= aborted |
	self;

    unknown(Stream),
    Events ? aborted :
      Events' = _,
      Scale = _,
      Values = [].
