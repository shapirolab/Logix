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

run(Goal, Cutoff) :-

    Goal =?= _#_,
    Cutoff >= 0 |
	spi_monitor#scheduler(Scheduler),
	write_channel(cutoff(Cutoff), Scheduler),
	computation#Goal;

    otherwise |
	fail(run(Goal, Cutoff)).

run(Goal, File, Cutoff) :-
	runit + (Scale = 1, Format = none).

run(Goal, File, Cutoff, Arg) :-
    number(Arg) |
	runit + (Scale = Arg, Format = none);
    otherwise |
	format_arg + (Scale = 1, Format = Arg).

run(Goal, File, Cutoff, Scale, Format) :-
    number(Scale) |
	format_arg.
run(Goal, File, Cutoff, Format, Scale) :-
    number(Scale) |
	format_arg.

format_arg(Goal, File, Cutoff, Scale, Format) :-
    Format = none |
	runit;
    Format = process |
	runit;
    Format = creator |
	runit;
    Format = full |
	runit;
    otherwise :
      Cutoff = _,
      File = _,
      Goal = _,
      Scale = _ |
	fail("Unrecognized format" - Format).

runit(Goal, File, Cutoff, Scale, Format) :-

    Goal =?= _#_,
    string(File), File =\= "",
    Cutoff >= 0,
    convert_to_real(Scale, Scale'),
    0 < Scale' |
	spi_monitor#scheduler(Scheduler),
	write_channel(debug(Stream), Scheduler, Scheduler'),
	write_channel(cutoff(Cutoff), Scheduler'),
	computation#[Goal, events(Events)],
	spi_utils#show_value(Values?, [Style, 3], Processes),
	synchronize_output,
	screen#display_stream(Out?, [put(File), width(10000)]),
	filter_data;

    otherwise |
	fail("Bad argument" - run(Goal, File, Cutoff, Scale, Format)).

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

/* Old output */
    Stream ? end(Name(_ChannelName)) :
      Values ! END(Name) |
	self;

/* New output */
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

/* Old output */
    Stream ? end(Process(ChannelName)) :
      Values ! (END(Process) : ChannelName) |
	self;

/* New output */

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

/* Old output */
    Stream ? end(Process(ChannelName)) :
      Values ! (END(Process) : ChannelName) |
	self;

/* New output */

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

/* Old output */
    Stream ? end(Process(ChannelName)) :
      Values ! (END(Process) : ChannelName) |
	self;

/* New output */

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
