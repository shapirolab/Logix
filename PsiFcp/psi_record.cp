-language([evaluate,compound,colon]).
-mode(trust).
-export([run/2, run/3, run/4, run/5]).

-include(psi_constants).

REALTIME => 12.

run(Goal, Cutoff) :-

    Goal =?= _#_,
    Cutoff >= 0 |
	psi_monitor#scheduler(Scheduler),
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
	psi_monitor#scheduler(Scheduler),
	write_channel(record(Stream), Scheduler, Scheduler'),
	write_channel(cutoff(Cutoff), Scheduler'),
	computation#[Goal, events(Events)],
	file#put_file(File, Out?, write, Ok),
	filter_data,
	run_ok;

    otherwise |
	fail("Bad argument" - run(Goal, File, Cutoff, Scale, Format)).

  run_ok(File, Ok) :-

    Ok = true :
      File = _;

    otherwise |
	fail(("
		write"(File) - Ok)).


filter_data(Stream, Events, Out, Scale, Format) :-

    Format =?= none |
	filter_none;

    Format =?= process |
 	filter_process;

    Format =?= creator |
 	filter_creator;

    Format =?= full |
 	filter_full;

    otherwise :
      Events = _,
      Scale = _,
      Stream = _,
      Out = [] |
	fail(invalid_format(Format)).

filter_none(Stream, Events, Out, Scale) :-

    Stream ? Number, number(Number),
    Number' := Scale*Number :
      Out ! Number',
      Out' ! "
" |
	self;

    Stream ? start(Name), string(Name),
    string_to_dlist(Name, CP, [CHAR_EOL]),
    list_to_string([CHAR_PLUS | CP], String) :
      Out ! String |
	self;

/* Old output */
    Stream ? end(Name(_ChannelName)),
    string_to_dlist(Name, CP, [CHAR_EOL]),
    list_to_string([CHAR_MINUS | CP], String) :
      Out ! String |
	self;

/* New output */
    Stream ? end(Name(_ChannelName, _Action, _CreatedId)),
    string_to_dlist(Name, CP, [CHAR_EOL]),
    list_to_string([CHAR_MINUS | CP], String) :
      Out ! String |
	self;

    otherwise |
	filter_end;

    Events ? Event,
    Event =\= aborted |
	self;

    unknown(Stream),
    Events ? aborted :
      Events' = _,
      Scale = _,
      Out = [].

filter_process(Stream, Events, Out, Scale) :-

    Stream ? Number, number(Number),
    Number' := Scale*Number :
      Out ! Number',
      Out' ! "
" |
	self;

    Stream ? start(Name), string(Name),
    string_to_dlist(Name, CP, [CHAR_EOL]),
    list_to_string([CHAR_PLUS | CP], String) :
      Out ! String |
	self;

/* Old output */
    Stream ? end(Name(ChannelName)),
    string_to_dlist(ChannelName, CN, [CHAR_EOL]),
    string_to_dlist(Name, CP, [CHAR_SPACE, CHAR_MINUS, CHAR_SPACE | CN]),
    list_to_string([CHAR_MINUS | CP], String) :
      Out ! String |
	self;

/* New output */
    Stream ? end(Name(ChannelName, Action, _CreatedId)),
    string_to_dlist(ChannelName, CN, [CHAR_EOL]),
    string_to_dlist(Action, CA, CN),
    string_to_dlist(Name, CP, CA),
    list_to_string([CHAR_MINUS | CP], String) :
      Out ! String |
	self;

    otherwise |
	filter_end;

    Events ? Event,
    Event =\= aborted |
	self;

    unknown(Stream),
    Events ? aborted :
      Events' = _,
      Scale = _,
      Out = [].


filter_creator(Stream, Events, Out, Scale) :-

    Stream ? Number, number(Number),
    Number' := Scale*Number :
      Out ! Number',
      Out' ! "
" |
	self;

    Stream ? start(Name), string(Name),
    string_to_dlist(Name, CP, [CHAR_EOL]),
    list_to_string([CHAR_PLUS | CP], String) :
      Out ! String |
	self;

/* Old output */
    Stream ? end(Name(ChannelName)),
    string_to_dlist(ChannelName, CN, [CHAR_EOL]),
    string_to_dlist(Name, CP, [CHAR_SPACE, CHAR_MINUS, CHAR_SPACE | CN]),
    list_to_string([CHAR_MINUS | CP], String) :
      Out ! String |
	self;

/* New output */
    Stream ? end(Name(_ChannelName, Action, CreatedId)),
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(Action, CA, CI'),
    string_to_dlist(Name, CP, CA') :
      CI' = CI,
      CA'= CA,
      Out ! String? |
	list_to_string([CHAR_MINUS | CP], String),
	self;

    Stream ? end(Name(_ChannelName, Action, CreatedId(Ordinal))),
    convert_to_string(Ordinal, OrdinalString),
    string_to_dlist(OrdinalString, CO, [CHAR_RIGHT_BRACKET, CHAR_EOL]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_BRACKET | CO']),
    string_to_dlist(Action, CA, CI'),
    string_to_dlist(Name, CP, CA') :
      CO' = CO,
      CI' = CI,
      CA'= CA,
      Out ! String? |
	list_to_string([CHAR_MINUS | CP], String),
	self;

    otherwise |
	filter_end;

    Events ? Event,
    Event =\= aborted |
	self;

    unknown(Stream),
    Events ? aborted :
      Events' = _,
      Scale = _,
      Out = [].


filter_full(Stream, Events, Out, Scale) :-

    Stream ? Number, number(Number),
    Number' := Scale*Number :
      Out ! Number',
      Out' ! "
" |
	self;

    Stream ? start(Name), string(Name),
    string_to_dlist(Name, CP, [CHAR_EOL]),
    list_to_string([CHAR_PLUS | CP], String) :
      Out ! String |
	self;

/* Old output */

    Stream ? end(Name(ChannelName)),
    string_to_dlist(ChannelName, CN, [CHAR_EOL]),
    string_to_dlist(Name, CP, [CHAR_SPACE, CHAR_MINUS, CHAR_SPACE | CN]),
    list_to_string([CHAR_MINUS | CP], String) :
      Out ! String |
	self;

/* New output */

    Stream ? end(Name(ChannelName, Action, CreatedId)),
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON | CI']),
    string_to_dlist(Action, CA, CN'),
    string_to_dlist(Name, CP, CA') :
      CI' = CI,
      CN' = CN,
      CA'= CA,
      Out ! String? |
	list_to_string([CHAR_MINUS | CP], String),
	self;

    Stream ? end(Name(ChannelName, Action, CreatedId(Ordinal))),
    convert_to_string(Ordinal, OrdinalString),
    string_to_dlist(OrdinalString, CO, [CHAR_RIGHT_BRACKET, CHAR_EOL]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_BRACKET | CO']),
    string_to_dlist(ChannelName, CN, [CHAR_COLON | CI']),
    string_to_dlist(Action, CA, CN'),
    string_to_dlist(Name, CP, CA') :
      CO' = CO,
      CI' = CI,
      CN' = CN,
      CA'= CA,
      Out ! String? |
	list_to_string([CHAR_MINUS | CP], String),
	self;

    otherwise |
	filter_end;

    Events ? Event,
    Event =\= aborted |
	self;

    unknown(Stream),
    Events ? aborted :
      Events' = _,
      Scale = _,
      Out = [].


filter_end(Stream, Events, Out, Scale) :-

    Stream ? Element,
    otherwise :
      Events = _,
      Scale = _,
      Stream' = _,
      Out = [CHAR_QUERY, CHAR_EOL] |
	fail((data:Element));

    Stream =?= [] :
      Events = _,
      Scale = _,
      Out = [] ;

    otherwise :
      Events = _,
      Scale = _,
      Out = [CHAR_QUERY, CHAR_EOL] |
	fail((format:Stream)).
