-language([evaluate,compound,colon]).
-mode(trust).
-export([run/2, run/3, run/4, run/5
	, filter_ambient/5]).

-include(spi_constants).

% Numeric arguments should be evaluated by convert_to_real.

run(Goal, Cutoff) :-

    Goal =?= _#_,
    convert_to_real(Cutoff, Cutoff'),
    Cutoff' >= 0 |
	spi_monitor#scheduler(Scheduler),
	write_channel(cutoff(Cutoff'), Scheduler),
	computation#Goal;

    otherwise |
	fail(run(Goal, Cutoff)).

run(Goal, File, Cutoff) :-
	runit + (Scale = 1, Format = none).

run(Goal, File, Cutoff, Arg) :-
    convert_to_real(Arg, Scale) |
	runit + (Format = none);
    otherwise |
	format_arg + (Scale = 1, Format = Arg).

run(Goal, File, Cutoff, Scale, Format) :-
    convert_to_real(Scale, Scale') |
	format_arg.
run(Goal, File, Cutoff, Format, Scale) :-
    convert_to_real(Scale, Scale') |
	format_arg.

format_arg(Goal, File, Cutoff, Scale, Format) :-
    Format =?= none |
	runit;
    Format =?= process |
	runit;
    Format =?= creator |
	runit;
    Format =?= full |
	runit;
    Format =?= ambient |
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
    convert_to_real(Cutoff, Cutoff'),
    0 =< Cutoff',
    convert_to_real(Scale, Scale'),
    0 < Scale' |
	spi_monitor#scheduler(Scheduler),
	write_channel(record(Stream), Scheduler, Scheduler'),
	write_channel(cutoff(Cutoff'), Scheduler'),
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

    Format =?= ambient |
 	filter_ambient + (Last = 0);

    otherwise :
      Events = _,
      Scale = _,
      Stream = _,
      Out = [] |
	fail(invalid_format(Format)).


filter_ambient(Stream, Events, Out, Scale, Last) :-

    Stream ? Number, number(Number),
    Number > 0,
    Last' := Scale*Number :
      Last = _ |
	self;

    Stream ? start(_Name) |
	self;

    Stream ? end(_Name(_ChannelName, _Action, _FileId)) |
	self;

/* For ambient merge */
    Stream ? reset(_Prefix) |
	self;

    Stream ? ambient(terminated(system, system)) |
	self;

    Stream ? ambient(F(A1(N1), A2(N2))),
    string(F), string(A1), number(N1), string(A2), number(N2),
    Last > 0,
    convert_to_string(Last, SL),
    string_to_dlist(SL, DLast, [CHAR_EOL | List?]) :
      Last' = -1,
      Out ! TwoLines? |
	ambient_line_to_list,
	list_to_string(DLast, TwoLines),
	self;

    Stream ? ambient(F(A1(N1), A2(N2))),
    string(F), string(A1), number(N1), string(A2), number(N2),
    Last =< 0 :
      Out ! OneLine? |
	ambient_line_to_list,
	list_to_string(List, OneLine),
	self;

    otherwise :
      Last = _ |
	filter_end;

    Events ? Event,
    Event =\= aborted |
	self;

    unknown(Stream),
    Events ? aborted :
      Events' = _,
      Last = _,
      Scale = _,
      Out = [].

  ambient_line_to_list(F, A1, N1, A2, N2, List) :-

    convert_to_string(N2, SN2),
    string_to_dlist(SN2, DN2, [CHAR_RIGHT_PAREN, CHAR_EOL]),
    string_to_dlist(A2, DA2, [CHAR_LEFT_PAREN | DN2]),
    convert_to_string(N1, SN1),
    string_to_dlist(SN1, DN1, [CHAR_RIGHT_PAREN, CHAR_SPACE | DA2]),
    string_to_dlist(A1, DA1, [CHAR_LEFT_PAREN | DN1]),
    string_to_dlist(F, DF, [CHAR_SPACE | DA1]) :
      List = DF.


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

    Stream ? end(Name(_ChannelName, _Action, _FileId)),
    string_to_dlist(Name, CP, [CHAR_EOL]),
    list_to_string([CHAR_MINUS | CP], String) :
      Out ! String |
	self;

/* For ambient merge */
    Stream ? reset(Prefix),
    convert_to_string(Prefix, SPrefix),
    string_to_dlist(SPrefix, LPrefix, [CHAR_EOL]),
    list_to_string([CHAR_BANG | LPrefix], ResetPrefix) :
      Out ! ResetPrefix? |
	self;

    Stream ? reset(AmbientName(UniqueId)),
    convert_to_string(UniqueId, UniqueId'),
    string_to_dlist(UniqueId', CU, [CHAR_RIGHT_PAREN, CHAR_EOL]),
    string_to_dlist(AmbientName, CN, [CHAR_LEFT_PAREN | CU]) :
      Out ! ResetPrefix? |
	list_to_string([CHAR_BANG | CN], ResetPrefix),
	self;

    Stream ? ambient(_) |
	/* Just ignore it for now */
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

    Stream ? end(Name(ChannelName, Action, _FileId)),
    string_to_dlist(ChannelName, CN, [CHAR_EOL]),
    string_to_dlist(Action, CA, [CHAR_SPACE | CN]),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]),
    list_to_string([CHAR_MINUS | CP], String) :
      Out ! String |
	self;

    Stream ? end(Name(self(p2c ChannelName), Action, _FileId)) |
	process_inter_comm(Name, ChannelName, Action, " p2c ", Out, Out'),
	self;

    Stream ? end(Name(parent(p2c ChannelName), Action, _FileId)) |
	process_inter_comm(Name, ChannelName, Action, " c2p ", Out, Out'),
	self;

    Stream ? end(Name(_parent(s2s ChannelName), Action, _FileId)) |
	process_inter_comm(Name, ChannelName, Action, " s2s ", Out, Out'),
	self;

    Stream ? end(Name(_self(exit ChannelName), Action, _FileId)),
    Action =?= RECEIVED_ARROW |
	process_inter_comm(Name, ChannelName, Action, " expel ", Out, Out'),
	self;

    Stream ? end(Name(_parent(exit ChannelName), Action, _FileId)),
    Action =?= SENT_ARROW |
	process_inter_comm(Name, ChannelName, Action, " exit ", Out, Out'),
	self;

    Stream ? end(Name(_parent(enter ChannelName), Action, _FileId)),
    Action =?= RECEIVED_ARROW |
	process_inter_comm(Name, ChannelName, Action, " accept ", Out, Out'),
	self;

    Stream ? end(Name(_parent(enter ChannelName), Action, _FileId)),
    Action =?= SENT_ARROW |
	process_inter_comm(Name, ChannelName, Action, " enter ", Out, Out'),
	self;

    Stream ? end(Name(_parent(merge(ChannelName)), Action, _FileId)),
    Action =?= RECEIVED_ARROW |
	process_inter_comm(Name, ChannelName, Action, " merge+ ", Out, Out'),
	self;

    Stream ? end(Name(_parent(merge(ChannelName)), Action, _FileId)),
    Action =?= SENT_ARROW |
	process_inter_comm(Name, ChannelName, Action, " merge- ", Out, Out'),
	self;

/* For ambient merge */
    Stream ? reset(Prefix),
    convert_to_string(Prefix, SPrefix),
    string_to_dlist(SPrefix, LPrefix, [CHAR_EOL]),
    list_to_string([CHAR_BANG | LPrefix], ResetPrefix) :
      Out ! ResetPrefix? |
	self;

    Stream ? reset(AmbientName(UniqueId)),
    convert_to_string(UniqueId, UniqueId'),
    string_to_dlist(UniqueId', CU, [CHAR_RIGHT_PAREN, CHAR_EOL]),
    string_to_dlist(AmbientName, CN, [CHAR_LEFT_PAREN | CU]) :
      Out ! ResetPrefix? |
	list_to_string([CHAR_BANG | CN], ResetPrefix),
	self;

    Stream ? ambient(_) |
	/* Just ignore it for now */
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


  process_inter_comm(Name, ChannelName, Action, Capability, Out, Out') :-

    string_to_dlist(ChannelName, CN, [CHAR_EOL]),
    string_to_dlist(Capability, CC, CN),
    string_to_dlist(Action, CA, CC),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]),
    list_to_string([CHAR_MINUS | CP], String) :
      Out ! String? ;

    otherwise :
      Element = end(Name(ChannelName), Action, Capability),
      Out = Out' |
	fail(Element).


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

    Stream ? end(Name(_ChannelName, Action, CreatedId)),
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(Action, CA, [CHAR_SPACE | CI]),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String? |
	list_to_string([CHAR_MINUS | CP], String),
	self;

    Stream ? end(Name(_ChannelName, Action, CreatedId(UniqueId))),
    number(UniqueId),
    convert_to_string(UniqueId, UniqueIdString),
    string_to_dlist(UniqueIdString, CU, [CHAR_RIGHT_PAREN, CHAR_EOL]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_PAREN | CU]),
    string_to_dlist(Action, CA, [CHAR_SPACE | CI]),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String? |
	list_to_string([CHAR_MINUS | CP], String),
	self;

    Stream ? end(Name(self(_InterAmbientId), Action, p2c FileId)) |
	creator_inter_comm(Name, Action, FileId, " p2c ", Out, Out'?),
	self;

    Stream ? end(Name(parent(_InterAmbientId), Action, p2c FileId)) |
	creator_inter_comm(Name, Action, FileId, " c2p ", Out, Out'?),
	self;

    Stream ? end(Name(_LocusId, Action, s2s FileId)) |
	creator_inter_comm(Name, Action, FileId, " s2s ", Out, Out'?),
	self;

    Stream ? end(Name(_LocusId, Action, exit FileId)),
    Action =?= RECEIVED_ARROW |
	creator_inter_comm(Name, Action, FileId, " expel ", Out, Out'?),
	self;

    Stream ? end(Name(_LocusId, Action, exit FileId)),
    Action =?= SENT_ARROW |
	creator_inter_comm(Name, Action, FileId, " exit ", Out, Out'?),
	self;

    Stream ? end(Name(_LocusId, Action, enter FileId)),
    Action =?= RECEIVED_ARROW |
	creator_inter_comm(Name, Action, FileId, " accept ", Out, Out'?),
	self;

    Stream ? end(Name(_LocusId, Action, enter FileId)),
    Action =?= SENT_ARROW |
	creator_inter_comm(Name, Action, FileId, " enter ", Out, Out'?),
	self;

    Stream ? end(Name(_LocusId, Action, merge(FileId))),
    Action =?= RECEIVED_ARROW |
	creator_inter_comm(Name, Action, FileId, " merge+ ", Out, Out'?),
	self;

    Stream ? end(Name(_LocusId, Action, merge(FileId))),
    Action =?= SENT_ARROW |
	creator_inter_comm(Name, Action, FileId, " merge- ", Out, Out'?),
	self;

/* The following code does not work in-line

    Stream ? end(Name(_LocusId, Action, merge(CreatedId))),
    string(CreatedId),
    Action =?= SENT_ARROW,
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(" merge- ", CP, CI),
    string_to_dlist(Action, CA, CP),
    string_to_dlist(Name, CN, [CHAR_SPACE | CA]) :
      Out ! String? |
	list_to_string([CHAR_MINUS | CN], String),
	self;

    Stream ? end(Name(_LocusId, Action, merge(CreatedId(UniqueId)))),
    Action =?= SENT_ARROW,
    convert_to_string(UniqueId, UniqueIdString),
    string_to_dlist(UniqueIdString, CU, [CHAR_RIGHT_PAREN, CHAR_EOL]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_PAREN | CU]),
    string_to_dlist(" merge- ", CP, CI),
    string_to_dlist(Action, CA, [CHAR_SPACE | CP]),
    string_to_dlist(Name, CN, [CHAR_SPACE | CA]) :
      Out ! String? |
	list_to_string([CHAR_MINUS | CN], String),
	self;
*/

/* For ambient merge */
    Stream ? reset(Prefix),
    convert_to_string(Prefix, SPrefix),
    string_to_dlist(SPrefix, LPrefix, [CHAR_EOL]),
    list_to_string([CHAR_BANG | LPrefix], ResetPrefix) :
      Out ! ResetPrefix? |
	self;

    Stream ? reset(AmbientName(UniqueId)),
    convert_to_string(UniqueId, UniqueId'),
    string_to_dlist(UniqueId', CU, [CHAR_RIGHT_PAREN, CHAR_EOL]),
    string_to_dlist(AmbientName, CN, [CHAR_LEFT_PAREN | CU]) :
      Out ! ResetPrefix? |
	list_to_string([CHAR_BANG | CN], ResetPrefix),
	self;

    Stream ? ambient(_) |
	/* Just ignore it for now */
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

  creator_inter_comm(Name, Action, FileId, Capability, Out, Out') :-

    string(FileId),
    string_to_dlist(FileId, CI, [CHAR_EOL]),
    string_to_dlist(Capability, CC, CI),
    string_to_dlist(Action, CA, CC),
    string_to_dlist(Name, CN, [CHAR_SPACE | CA]) :
      Out ! String? |
	list_to_string([CHAR_MINUS | CN], String);

    FileId =?= CreatedId(UniqueId),
    convert_to_string(UniqueId, UniqueIdString),
    string_to_dlist(UniqueIdString, CU, [CHAR_RIGHT_PAREN, CHAR_EOL]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_PAREN | CU]),
    string_to_dlist(Capability, CC, CI),
    string_to_dlist(Action, CA, CC),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String? |
	list_to_string([CHAR_MINUS | CP], String).


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

    Stream ? end(Name(ChannelName, Action, CreatedId)),
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(Action, CA, [CHAR_SPACE | CN]),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]),
    list_to_string([CHAR_MINUS | CP], String) :
      Out ! String |
	self;
/*
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

    Stream ? end(Name(_self(exit ChannelName), Action, exit CreatedId)),
    Action =?= RECEIVED_ARROW,
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" expel ", CEXPEL, CN),
    string_to_dlist(Action, CA, CEXPEL),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

    Stream ? end(Name(_parent(exit ChannelName), Action, exit CreatedId)),
    Action =?= SENT_ARROW,
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" exit ", CEXIT, CN),
    string_to_dlist(Action, CA, CEXIT),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

    Stream ? end(Name(_parent(enter ChannelName), Action, enter CreatedId)),
    Action =?= RECEIVED_ARROW,
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" accept ", CENTER, CN),
    string_to_dlist(Action, CA, CENTER),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

    Stream ? end(Name(_parent(enter ChannelName), Action, enter CreatedId)),
    Action =?= SENT_ARROW,
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" enter ", CENTER, CN),
    string_to_dlist(Action, CA, CENTER),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

    Stream ? end(Name(_parent(merge(ChannelName)), Action, merge(CreatedId))),
    Action =?= RECEIVED_ARROW,
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" merge+ ", CMERGE, CN),
    string_to_dlist(Action, CA, CMERGE),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);

    Stream ? end(Name(_parent(merge(ChannelName)), Action, merge(CreatedId))),
    Action =?= SENT_ARROW,
    string_to_dlist(CreatedId, CI, [CHAR_EOL]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(" merge- ", CMERGE, CN),
    string_to_dlist(Action, CA, CMERGE),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String |
	self,
	list_to_string([CHAR_MINUS | CP], String);
*/

    Stream ? end(Name(ChannelName, Action, CreatedId(UniqueId))),
    convert_to_string(UniqueId, UniqueIdString),
    string_to_dlist(UniqueIdString, CU, [CHAR_RIGHT_PAREN, CHAR_EOL]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_PAREN | CU]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(Action, CA, [CHAR_SPACE | CN]),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String? |
	list_to_string([CHAR_MINUS | CP], String),
	self;

    Stream ? end(Name(_parent(s2s ChannelName), Action, s2s FileId)) |
	full_inter_comm(Name, ChannelName, Action, FileId, " s2s ",
			Out, Out'),
	self;

    Stream ? end(Name(self(p2c ChannelName), Action, p2c FileId)) |
	full_inter_comm(Name, ChannelName, Action, FileId, " p2c ",
			Out, Out'),
	self;

    Stream ? end(Name(parent(p2c ChannelName), Action, p2c FileId)) |
	full_inter_comm(Name, ChannelName, Action, FileId, " c2p ",
			Out, Out'),
	self;

    Stream ? end(Name(_parent(exit ChannelName), Action, exit FileId)),
    Action =?= RECEIVED_ARROW |
	full_inter_comm(Name, ChannelName, Action, FileId, " expel ",
			Out, Out'),
	self;

    Stream ? end(Name(_parent(exit ChannelName), Action, exit FileId)),
    Action =?= SENT_ARROW |
	full_inter_comm(Name, ChannelName, Action, FileId, " exit ",
			Out, Out'),
	self;

    Stream ? end(Name(_parent(enter ChannelName), Action, enter FileId)),
    Action =?= RECEIVED_ARROW |
	full_inter_comm(Name, ChannelName, Action, FileId, " accept ",
			Out, Out'),
	self;

    Stream ? end(Name(_parent(enter ChannelName), Action, enter FileId)),
    Action =?= SENT_ARROW |
	full_inter_comm(Name, ChannelName, Action, FileId, " enter ",
			Out, Out'),
	self;

    Stream ? end(Name(_parent(merge(ChannelName)), Action, merge(FileId))),
    Action =?= RECEIVED_ARROW |
	full_inter_comm(Name, ChannelName, Action, FileId, " merge+ ",
			Out, Out'),
	self;

    Stream ? end(Name(_parent(merge(ChannelName)), Action, merge(FileId))),
    Action =?= SENT_ARROW |
	full_inter_comm(Name, ChannelName, Action, FileId, " merge- ",
			Out, Out'),
	self;

/* For ambient merge */
    Stream ? reset(Prefix),
    convert_to_string(Prefix, SPrefix),
    string_to_dlist(SPrefix, LPrefix, [CHAR_EOL]),
    list_to_string([CHAR_BANG | LPrefix], ResetPrefix) :
      Out ! ResetPrefix? |
	self;

    Stream ? reset(AmbientName(UniqueId)),
    convert_to_string(UniqueId, UniqueId'),
    string_to_dlist(UniqueId', CU, [CHAR_RIGHT_PAREN, CHAR_EOL]),
    string_to_dlist(AmbientName, CN, [CHAR_LEFT_PAREN | CU]) :
      Out ! ResetPrefix? |
	list_to_string([CHAR_BANG | CN], ResetPrefix),
	self;

    Stream ? ambient(_) |
	/* Just ignore it for now */
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

  full_inter_comm(Name, ChannelName, Action, FileId, Capability, Out, Out') :-

    string(FileId),
    string_to_dlist(FileId, CI, [CHAR_EOL]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(Capability, CC, CN),
    string_to_dlist(Action, CA, CC),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String? |
	list_to_string([CHAR_MINUS | CP], String);

    FileId =?= CreatedId(UniqueId),
    convert_to_string(UniqueId, UniqueIdString),
    string_to_dlist(UniqueIdString, CU, [CHAR_RIGHT_PAREN, CHAR_EOL]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_PAREN | CU]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(Capability, CC, CN),
    string_to_dlist(Action, CA, CC),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]) :
      Out ! String? |
	list_to_string([CHAR_MINUS | CP], String);

    otherwise :
      Element = end(Name(ChannelName), Action, Capability, FileId),
      Out = Out' |
	fail(Element).


filter_end(Stream, Events, Out, Scale) :-

    Stream ? idle(Number),
    Number' := Scale*Number :
      Events = _,
      Stream' = _,
      Out = [Number', "
"];

    Stream ? Element,
    otherwise,
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
