-language([evaluate,compound,colon]).
-mode(trust).
-export([run/2, run/3, run/4, run/5]).

-include(spi_constants).

REALTIME => 12.


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
	write_channel(record(Stream), Scheduler, Scheduler'),
	write_channel(cutoff(Cutoff), Scheduler'),
	computation#[Goal, events(Events)],
	file#put_file(File, Out?, write, Ok),
	filter_data,
	run_ok;

    otherwise |
	fail("Bad argument" - run(Goal, File, Cutoff, Scale, Format)).

  run_ok(Events, File, Ok) :-

    Ok = true :
      Events = _,
      File = _;

    otherwise :
      Events = _ |
	fail(("
		write"(File) - Ok));

    Events ? Event,
    Event =\= aborted |
	self;

    Events ? aborted :
      Events' = _,
      File = _,
      Ok = _.


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
    string_to_dlist(Action, CA, [CHAR_SPACE | CN]),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]),
    list_to_string([CHAR_MINUS | CP], String) :
      Out ! String |
	self;

    Stream ? end(Name(self(p2c ChannelName), Action, _CreatedId)),
    string_to_dlist(ChannelName, CN, [CHAR_EOL]),
    string_to_dlist(" p2c ", CP2C, CN),
    string_to_dlist(Action, CA, CP2C),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

    Stream ? end(Name(parent(p2c ChannelName), Action, _CreatedId)),
    string_to_dlist(ChannelName, CN, [CHAR_EOL]),
    string_to_dlist(" c2p ", CC2P, CN),
    string_to_dlist(Action, CA, CC2P),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

    Stream ? end(Name(_parent(s2s ChannelName), Action, _CreatedId)),
    string_to_dlist(ChannelName, CN, [CHAR_EOL]),
    string_to_dlist(" s2s ", CS2S, CN),
    string_to_dlist(Action, CA, CS2S),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

    Stream ? end(Name(self(exit ChannelName), Action, _CreatedId)),
    string_to_dlist(ChannelName, CN, [CHAR_EOL]),
    string_to_dlist(" expel ", CEXPEL, CN),
    string_to_dlist(Action, CA, CEXPEL),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

    Stream ? end(Name(parent(exit ChannelName), Action, _CreatedId)),
    string_to_dlist(ChannelName, CN, [CHAR_EOL]),
    string_to_dlist(" exit ", CEXIT, CN),
    string_to_dlist(Action, CA, CEXIT),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

    Stream ? end(Name(_parent(enter ChannelName), Action, _CreatedId)),
    string_to_dlist(ChannelName, CN, [CHAR_EOL]),
    string_to_dlist(" enter ", CENTER, CN),
    string_to_dlist(Action, CA, CENTER),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

    Stream ? end(Name(_parent(merge(ChannelName)), Action, _CreatedId)),
    string_to_dlist(ChannelName, CN, [CHAR_EOL]),
    string_to_dlist(" merge ", CMERGE, CN),
    string_to_dlist(Action, CA, CMERGE),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

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
    number(Ordinal),
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

    Stream ? end(Name(_ChannelName, Action, Capability(CreatedId))),
    string(CreatedId),
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(Capability, CP, [CHAR_SPACE | CI']),
    string_to_dlist(Action, CA, CP'),
    string_to_dlist(Name, CN, CA') :
      CI' = CI,
      CP'= CP,
      CA'= CA,
      Out ! String? |
	list_to_string([CHAR_MINUS | CN], String),
	self;

    Stream ? end(Name(_ChannelName, Action, Capability(CreatedId(Ordinal)))),
    convert_to_string(Ordinal, OrdinalString),
    string_to_dlist(OrdinalString, CO, [CHAR_RIGHT_BRACKET, CHAR_EOL]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_BRACKET | CO']),
    string_to_dlist(Capability, CP, [CHAR_SPACE | CI']),
    string_to_dlist(Action, CA, CP'),
    string_to_dlist(Name, CN, CA') :
      CO' = CO,
      CI' = CI,
      CP'= CP,
      CA'= CA,
      Out ! String? |
	list_to_string([CHAR_MINUS | CN], String),
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
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(Action, CA, [CHAR_SPACE | CN]),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]),
    list_to_string([CHAR_MINUS | CP], String) :
      Out ! String |
	self;

    Stream ? end(Name(self(p2c ChannelName), Action, p2c CreatedId)),
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" p2c ", CP2C, CN),
    string_to_dlist(Action, CA, CP2C),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

    Stream ? end(Name(parent(p2c ChannelName), Action, p2c CreatedId)),
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" c2p ", CC2P, CN),
    string_to_dlist(Action, CA, CC2P),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

    Stream ? end(Name(_parent(s2s ChannelName), Action, s2s CreatedId)),
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" s2s ", CS2S, CN),
    string_to_dlist(Action, CA, CS2S),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

    Stream ? end(Name(self(exit ChannelName), Action, exit CreatedId)),
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" expel ", CEXPEL, CN),
    string_to_dlist(Action, CA, CEXPEL),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

    Stream ? end(Name(parent(exit ChannelName), Action, exit CreatedId)),
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" exit ", CEXIT, CN),
    string_to_dlist(Action, CA, CEXIT),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

    Stream ? end(Name(_parent(enter ChannelName), Action, enter CreatedId)),
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" enter ", CENTER, CN),
    string_to_dlist(Action, CA, CENTER),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

    Stream ? end(Name(_parent(merge(ChannelName)), Action, merge(CreatedId))),
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" merge ", CMERGE, CN),
    string_to_dlist(Action, CA, CMERGE),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

    Stream ? end(Name(ChannelName, Action, CreatedId(Ordinal))),
    convert_to_string(Ordinal, OrdinalString),
    string_to_dlist(OrdinalString, CO, [CHAR_RIGHT_BRACKET, CHAR_EOL]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_BRACKET | CO]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(Action, CA, [CHAR_SPACE | CN]),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String? |
	list_to_string([CHAR_MINUS | CP], String),
	self;

    Stream ? end(Name(self(p2c ChannelName), Action, p2c CreatedId(Ordinal))),
    convert_to_string(Ordinal, OrdinalString),
    string_to_dlist(OrdinalString, CO, [CHAR_RIGHT_BRACKET, CHAR_EOL]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_BRACKET | CO]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" p2c ", CP2C, CN),
    string_to_dlist(Action, CA, CP2C),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	list_to_string([CHAR_MINUS | CP], String),
	self;

    Stream ? end(Name(parent(p2c ChannelName), Action,
				p2c CreatedId(Ordinal))),
    convert_to_string(Ordinal, OrdinalString),
    string_to_dlist(OrdinalString, CO, [CHAR_RIGHT_BRACKET, CHAR_EOL]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_BRACKET | CO]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" c2p ", CC2P, CN),
    string_to_dlist(Action, CA, CC2P),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	list_to_string([CHAR_MINUS | CP], String),
	self;

    Stream ? end(Name(_parent(s2s ChannelName), Action,
				s2s CreatedId(Ordinal))),
    convert_to_string(Ordinal, OrdinalString),
    string_to_dlist(OrdinalString, CO, [CHAR_RIGHT_BRACKET, CHAR_EOL]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_BRACKET | CO]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" s2s ", CS2S, CN),
    string_to_dlist(Action, CA, CS2S),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	list_to_string([CHAR_MINUS | CP], String),
	self;

    Stream ? end(Name(self(exit ChannelName), Action,
				exit CreatedId(Ordinal))),
    convert_to_string(Ordinal, OrdinalString),
    string_to_dlist(OrdinalString, CO, [CHAR_RIGHT_BRACKET, CHAR_EOL]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_BRACKET | CO]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" expel ", CEXPEL, CN),
    string_to_dlist(Action, CA, CEXPEL),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	list_to_string([CHAR_MINUS | CP], String),
	self;

    Stream ? end(Name(parent(exit ChannelName), Action,
				exit CreatedId(Ordinal))),
    convert_to_string(Ordinal, OrdinalString),
    string_to_dlist(OrdinalString, CO, [CHAR_RIGHT_BRACKET, CHAR_EOL]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_BRACKET | CO]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" exit ", CEXIT, CN),
    string_to_dlist(Action, CA, CEXIT),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	list_to_string([CHAR_MINUS | CP], String),
	self;

    Stream ? end(Name(_parent(enter ChannelName), Action,
				enter CreatedId(Ordinal))),
    convert_to_string(Ordinal, OrdinalString),
    string_to_dlist(OrdinalString, CO, [CHAR_RIGHT_BRACKET, CHAR_EOL]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_BRACKET | CO]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" enter ", CENTER, CN),
    string_to_dlist(Action, CA, CENTER),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	list_to_string([CHAR_MINUS | CP], String),
	self;

    Stream ? end(Name(_parent(merge(ChannelName)), Action,
				merge(CreatedId(Ordinal)))),
    convert_to_string(Ordinal, OrdinalString),
    string_to_dlist(OrdinalString, CO, [CHAR_RIGHT_BRACKET, CHAR_EOL]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_BRACKET | CO]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" merge ", CMERGE, CN),
    string_to_dlist(Action, CA, CMERGE),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
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
    list_to_string([CHAR_QUERY, CHAR_EOL], String) :
      Events = _,
      Scale = _,
      Stream' = _,
      Out = [String] |
	fail((data:Element));

    Stream =?= [] :
      Events = _,
      Scale = _,
      Out = [] ;

    otherwise,
    list_to_string([CHAR_QUERY, CHAR_EOL], String) :
      Events = _,
      Scale = _,
      Out = [String] |
	fail((format:Stream)).
