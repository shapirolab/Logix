/*

SpiFcp Record channel activity from monitor record output
William Silverman

Last update by          $Author: bill $
                        $Date: 2007/02/22 10:42:05 $
Currently locked by     $Locker:  $
                        $Revision: 1.15 $
                        $Source: /home/qiana/Repository/Aspic/spi_record.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language([evaluate,compound,colon]).
-mode(trust).
-export([run/2, run/3, run/4, run/5]).

-include(spi_constants).

FILE_OPEN => 9.
FILE_CLOSE => 10. 
FILE_BUFFER => 12.

% Numeric arguments should be evaluated by convert_to_real.

run(Goal, Limit) :-

    Goal =?= _#_,
    convert_to_real(Limit, Limit'),
    Limit' >= 0 |
	computation # spi_monitor # scheduler(Scheduler),
	write_channel(cutoff(Limit', _State), Scheduler),
	computation # Goal;

    otherwise |
	fail(run(Goal, Limit)).

run(Goal, File, Limit) :-

	check_arguments([goal(Goal), file(File), limit(Limit)],
			Reply),
	run_check + (Scale = 1.0, Format = none, Entry = run/3). 

run(Goal, File, Limit, Arg) :-

    convert_to_real(Arg, _) |
	check_arguments([goal(Goal), file(File), limit(Limit),
			 scale(Arg)],
			Reply),
	run_check + (Scale = Arg, Format = none, Entry = run/3);

    otherwise |
	check_arguments([goal(Goal), file(File), limit(Limit),
			 format(Arg)],
			Reply),
	run_check + (Scale = 1.0, Format = Arg, Entry = run/4). 

run(Goal, File, Limit, Scale, Format) :-

	check_arguments([goal(Goal), file(File), limit(Limit),
			 scale(Scale), format(Format)],
			Reply),
	run_check + (Entry = run/5).

check_arguments(Args, Reply) :-

    Args ? goal(_ # _) |
	self;

    Args ? file(File),
    convert_to_string(File, String), String =\= "" |
	self;

    Args ? limit(Limit),
    convert_to_real(Limit, Real), Real > 0.0 |
	self;

    Args ? scale(Scale),
    convert_to_real(Scale, Real), Real > 0.0 |
	self;

    Args ? format(Format),
    Format =?= none |
	self;

    Args ? format(Format),
    Format =?= process |
	self;

    Args ? format(Format),
    Format =?= creator |
	self;

    Args ? format(Format),
    Format =?= full |
	self;

    Args ? format(Format),
    Format =?= ambient |
	self;

    Args ? Type(Value),
    otherwise :
      Reply ! Type(Value) |
	self;

    Args =?= [] :
      Reply = [].

run_check(Goal, File, Limit, Scale, Format, Entry, Reply) :-

    Reply =?= [],
    convert_to_string(File, File'),
    convert_to_real(Limit, Limit'),
    convert_to_real(Scale, Scale') :
      Entry = _ |
	file # pwd(UID),
	make_absolute(File', UID?, record, FileName),
	processor # link(lookup(file, Offset)),
	runit;

    otherwise :
      File = _,
      Format = _,
      Goal = _,
      Limit = _,
      Scale = _ |
	/* should wait here */
	utils # list_to_tuple([errors | Reply], Tuple),
	fail(Entry, Tuple).

  make_absolute(Name, UID, OpCode, NewName) :-

    string_to_dlist(Name, [First | _], []),
    First =:= ascii('/') : UID = _, OpCode = _,
    Name = NewName ;

    otherwise,
    string_to_dlist(Name, NL, []),
    string_to_dlist(UID, UIDNL, NL) : OpCode = _ |
	list_to_string(UIDNL, NewName);

    otherwise,
    string(UID) : Name' = '?' |
	fail("bad file name" - OpCode(Name)),
	self;

    UID = false(Reason) :
      NewName = '?' |
	fail(Reason - OpCode(Name)).

  runit(Goal, FileName, Limit, Scale, Format, Offset) :-
    known(FileName),
    Offset < 0 :
      execute(Offset, {FILE_OPEN, FileName, write, Fd}) |
	check_open,
	begin_monitor.

  check_open(Fd, Ok) :-

    integer(Fd),
    Fd > 0 :
      Ok = true;

    integer(Fd),
    Fd =< 0 :
      Status = Fd /* for now */,
      Ok = false(Status).

  begin_monitor(Goal, Fd, Limit, Scale, Format, Offset, Ok) :-

    Ok = true |
	computation # spi_monitor # scheduler(Scheduler),
	write_channel(cutoff(Limit, _State), Scheduler, Scheduler'),
	begin_computation.
	
  begin_computation(Goal, Fd, Scheduler, Scale, Format, Offset) :-
    vector(Scheduler) :
      write_channel(record(Stream), Scheduler) |
	computation # [events(Events), Goal],
	filter_data + (Last = 0).


filter_data(Stream, Fd, Events, Scale, Format, Offset, Last) :-

    Format =?= none |
	filter_none;

    Format =?= process |
 	filter_process;

    Format =?= creator |
 	filter_creator;

    Format =?= full |
 	filter_full;

    Format =?= ambient |
 	filter_ambient + (Then = 0).


filter_ambient(Stream, Events, Fd, Scale, Offset, Last, Then) :-

    Stream ? Number, number(Number),
    Number > 0,
    Then' := Scale*Number :
      Then = _ |
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

    Last =?= 0,
    Stream ? ambient(F(A1(N1), A2(N2))),
    string(F), string(A1), number(N1), string(A2), number(N2),
    Then > 0,
    convert_to_string(Then, SL),
    string_to_dlist(SL, DThen, [CHAR_EOL | List?]) :
      Then' = -1 |
	ambient_line_to_list,
	ambient_line_out(DThen, Offset, Fd, Last'),
	self;

    Last =?= 0,
    Stream ? ambient(F(A1(N1), A2(N2))),
    string(F), string(A1), number(N1), string(A2), number(N2),
    Then =< 0 |
	ambient_line_to_list,
	ambient_line_out(List, Offset, Fd, Last'),
	self;

    Last =\= 0 :
      Events = _,
      Scale = _,
      Stream = _,
      Then = _,
      Stream' = file_error - Last |
	filter_end;

    otherwise :
      Last = _,
      Then = _ |
	filter_end;

    Events ? Event,
    Event =\= aborted |
	self;

    unknown(Stream),
    Events ? aborted :
      Events' = _,
      Last = _,
      Then = _,
      Scale = _ |
	close_file.

  ambient_line_to_list(F, A1, N1, A2, N2, List) :-

    convert_to_string(N2, SN2),
    string_to_dlist(SN2, DN2, [CHAR_RIGHT_PAREN]),
    string_to_dlist(A2, DA2, [CHAR_LEFT_PAREN | DN2]),
    convert_to_string(N1, SN1),
    string_to_dlist(SN1, DN1, [CHAR_RIGHT_PAREN, CHAR_SPACE | DA2]),
    string_to_dlist(A1, DA1, [CHAR_LEFT_PAREN | DN1]),
    string_to_dlist(F, DF, [CHAR_SPACE | DA1]) :
      List = DF.

  ambient_line_out(Lines, Offset, Fd, Last) :-
    list_to_string(Lines, OutString) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, OutString, Last)).


filter_none(Stream, Events, Fd, Scale, Offset, Last) :-

    Last =?= 0,
    Stream ? Number, number(Number),
    Number' := Scale*Number,
    convert_to_string(Number', NumberString) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, NumberString, Last')) |
	self;

    Last =?= 0,
    Stream ? start(Name),
    string_to_dlist(Name, CP, []),
    list_to_string([CHAR_PLUS | CP], String) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, String, Last')) |
	self;

    Last =?= 0,
    Stream ? end(Name(_ChannelName, _Action, _FileId)),
    string_to_dlist(Name, CP, []),
    list_to_string([CHAR_MINUS | CP], String) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, String, Last')) |
	self;

/* For ambient merge */
    Last =?= 0,
    Stream ? reset(Prefix),
    convert_to_string(Prefix, SPrefix),
    string_to_dlist(SPrefix, LPrefix, []),
    list_to_string([CHAR_BANG | LPrefix], ResetPrefix) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, ResetPrefix, Last')) |
	self;

    Last =?= 0,
    Stream ? reset(AmbientName(UniqueId)),
    convert_to_string(UniqueId, UniqueId'),
    string_to_dlist(UniqueId', CU, [CHAR_RIGHT_PAREN]),
    string_to_dlist(AmbientName, CN, [CHAR_LEFT_PAREN | CU]),
    list_to_string(CN, Prefix) :
      Stream'' = [reset(Prefix) | Stream'] |
	self;

    Last =?= 0,
    Stream ? ambient(_) |
	/* Just ignore it for now */
	self;

    Last =\= 0 :
      Events = _,
      Scale = _,
      Stream = _,
      Stream' = file_error - Last |
	filter_end;

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
      Scale = _ |
	close_file.


filter_process(Stream, Events, Fd, Scale, Offset, Last) :-

    Last =?= 0,
    Stream ? Number, number(Number),
    Number' := Scale*Number,
    convert_to_string(Number', NumberString) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, NumberString, Last')) |
	self;

    Last =?= 0,
    Stream ? start(Name), string(Name),
    string_to_dlist(Name, CP, []),
    list_to_string([CHAR_PLUS | CP], String) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, String, Last')) |
	self;

    Last =?= 0,
    Stream ? end(Name(ChannelName, Action, _FileId)),
    string_to_dlist(ChannelName, CN, []),
    string_to_dlist(Action, CA, [CHAR_SPACE | CN]),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]),
    list_to_string([CHAR_MINUS | CP], String) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, String, Last')) |
	self;

    Last =?= 0,
    Stream ? end(Name(self(p2c ChannelName), Action, _FileId)) |
	process_inter_comm(Name, ChannelName, Action, " p2c ",
			   Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(parent(p2c ChannelName), Action, _FileId)) |
	process_inter_comm(Name, ChannelName, Action, " c2p ",
			   Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_parent(s2s ChannelName), Action, _FileId)) |
	process_inter_comm(Name, ChannelName, Action, " s2s ",
			   Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_self(exit ChannelName), Action, _FileId)),
    Action =?= RECEIVED_ARROW |
	process_inter_comm(Name, ChannelName, Action, " expel ",
			   Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_parent(exit ChannelName), Action, _FileId)),
    Action =?= SENT_ARROW |
	process_inter_comm(Name, ChannelName, Action, " exit ",
			   Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_parent(enter ChannelName), Action, _FileId)),
    Action =?= RECEIVED_ARROW |
	process_inter_comm(Name, ChannelName, Action, " accept ",
			   Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_parent(enter ChannelName), Action, _FileId)),
    Action =?= SENT_ARROW |
	process_inter_comm(Name, ChannelName, Action, " enter ",
			   Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_parent(merge(ChannelName)), Action, _FileId)),
    Action =?= RECEIVED_ARROW |
	process_inter_comm(Name, ChannelName, Action, " merge+ ",
			   Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_parent(merge(ChannelName)), Action, _FileId)),
    Action =?= SENT_ARROW |
	process_inter_comm(Name, ChannelName, Action, " merge- ",
			   Fd, Offset, Last'),
	self;

/* For ambient merge */
    Last =?= 0,
    Stream ? reset(Prefix),
    convert_to_string(Prefix, SPrefix),
    string_to_dlist(SPrefix, LPrefix, []),
    list_to_string([CHAR_BANG | LPrefix], ResetPrefix) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, ResetPrefix, Last')) |
	self;

    Last =?= 0,
    Stream ? reset(AmbientName(UniqueId)),
    convert_to_string(UniqueId, UniqueId'),
    string_to_dlist(UniqueId', CU, [CHAR_RIGHT_PAREN]),
    string_to_dlist(AmbientName, CN, [CHAR_LEFT_PAREN | CU]),
    list_to_string(CN, Prefix) :
      Stream'' = [reset(Prefix) | Stream'] |
	self;

    Last =?= 0,
    Stream ? ambient(_) |
	/* Just ignore it for now */
	self;

    Last =\= 0 :
      Events = _,
      Scale = _,
      Stream = _,
      Stream' = file_error - Last |
	filter_end;

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
      Scale = _ |
	close_file.

  process_inter_comm(Name, ChannelName, Action, Capability, Fd, Offset, Last) :-

    string_to_dlist(ChannelName, CN, []),
    string_to_dlist(Capability, CC, CN),
    string_to_dlist(Action, CA, CC),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]),
    list_to_string([CHAR_MINUS | CP], String) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, String, Last));

    otherwise :
      Fd = _,
      Offset = _,
      Element = end(Name(ChannelName), Action, Capability),
      Last = 0 |
	fail(Element).


filter_creator(Stream, Events, Fd, Scale, Offset, Last) :-

    Last =?= 0,
    Stream ? Number, number(Number),
    Number' := Scale*Number,
    convert_to_string(Number', NumberString) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, NumberString, Last')) |
	self;

    Last =?= 0,
    Stream ? start(Name), string(Name),
    string_to_dlist(Name, CP, []),
    list_to_string([CHAR_PLUS | CP], String) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, String, Last')) |
	self;

    Last =?= 0,
    Stream ? end(Name(_ChannelName, Action, CreatedId)),
    string_to_dlist(CreatedId, CI, []),
    string_to_dlist(Action, CA, [CHAR_SPACE | CI]),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]),
    list_to_string([CHAR_MINUS | CP], String) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, String, Last')) |
	self;

    Last =?= 0,
    Stream ? end(Name(_ChannelName, Action, CreatedId(UniqueId))),
    number(UniqueId),
    convert_to_string(UniqueId, UniqueIdString),
    string_to_dlist(UniqueIdString, CU, [CHAR_RIGHT_PAREN]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_PAREN | CU]),
    string_to_dlist(Action, CA, [CHAR_SPACE | CI]),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]),
    list_to_string([CHAR_MINUS | CP], String) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, String, Last')) |
	self;

    Last =?= 0,
    Stream ? end(Name(self(_InterAmbientId), Action, p2c FileId)) |
	creator_inter_comm(Name, Action, FileId, " p2c ", Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(parent(_InterAmbientId), Action, p2c FileId)) |
	creator_inter_comm(Name, Action, FileId, " c2p ", Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_LocusId, Action, s2s FileId)) |
	creator_inter_comm(Name, Action, FileId, " s2s ", Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_LocusId, Action, exit FileId)),
    Action =?= RECEIVED_ARROW |
	creator_inter_comm(Name, Action, FileId, " expel ", Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_LocusId, Action, exit FileId)),
    Action =?= SENT_ARROW |
	creator_inter_comm(Name, Action, FileId, " exit ", Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_LocusId, Action, enter FileId)),
    Action =?= RECEIVED_ARROW |
	creator_inter_comm(Name, Action, FileId, " accept ", Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_LocusId, Action, enter FileId)),
    Action =?= SENT_ARROW |
	creator_inter_comm(Name, Action, FileId, " enter ", Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_LocusId, Action, merge(FileId))),
    Action =?= RECEIVED_ARROW |
	creator_inter_comm(Name, Action, FileId, " merge+ ", Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_LocusId, Action, merge(FileId))),
    Action =?= SENT_ARROW |
	creator_inter_comm(Name, Action, FileId, " merge- ", Fd, Offset, Last'),
	self;

/* For ambient merge */
    Last =?= 0,
    Stream ? reset(Prefix),
    convert_to_string(Prefix, SPrefix),
    string_to_dlist(SPrefix, LPrefix, []),
    list_to_string([CHAR_BANG | LPrefix], ResetPrefix) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, ResetPrefix, Last')) |
	self;

    Last =?= 0,
    Stream ? reset(AmbientName(UniqueId)),
    convert_to_string(UniqueId, UniqueId'),
    string_to_dlist(UniqueId', CU, [CHAR_RIGHT_PAREN]),
    string_to_dlist(AmbientName, CN, [CHAR_LEFT_PAREN | CU]),
    list_to_string(CN, Prefix) :
      Stream'' = [reset(Prefix) | Stream'] |
	self;

    Last =?= 0,
    Stream ? ambient(_) |
	/* Just ignore it for now */
	self;

    Last =\= 0 :
      Events = _,
      Scale = _,
      Stream = _,
      Stream' = file_error - Last |
	filter_end;

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
      Scale = _ |
	close_file.

  creator_inter_comm(Name, Action, FileId, Capability, Fd, Offset, Last) :-

    string(FileId),
    string_to_dlist(FileId, CI, []),
    string_to_dlist(Capability, CC, CI),
    string_to_dlist(Action, CA, CC),
    string_to_dlist(Name, List, [CHAR_SPACE | CA]) |
	output_minus_string;

    FileId =?= CreatedId(UniqueId),
    convert_to_string(UniqueId, UniqueIdString),
    string_to_dlist(UniqueIdString, CU, [CHAR_RIGHT_PAREN]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_PAREN | CU]),
    string_to_dlist(Capability, CC, CI),
    string_to_dlist(Action, CA, CC),
    string_to_dlist(Name, List, [CHAR_SPACE | CA]) |
	output_minus_string.


filter_full(Stream, Events, Fd, Scale, Offset, Last) :-

    Last =?= 0,
    Stream ? Number, number(Number),
    Number' := Scale*Number,
    convert_to_string(Number', NumberString) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, NumberString, Last')) |
	self;

    Last =?= 0,
    Stream ? start(Name), string(Name),
    string_to_dlist(Name, CP, []),
    list_to_string([CHAR_PLUS | CP], String) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, String, Last')) |
	self;

    Last =?= 0,
    Stream ? end(Name(ChannelName, Action, CreatedId)),
    string_to_dlist(CreatedId, CI, []),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(Action, CA, [CHAR_SPACE | CN]),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]),
    list_to_string([CHAR_MINUS | CP], String) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, String, Last')) |
	self;

    Last =?= 0,
    Stream ? end(Name(ChannelName, Action, CreatedId(UniqueId))),
    convert_to_string(UniqueId, UniqueIdString),
    string_to_dlist(UniqueIdString, CU, [CHAR_RIGHT_PAREN]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_PAREN | CU]),
    list_to_string(CI, CreatedId') :
      Stream'' = [end(Name(ChannelName, Action, CreatedId')) | Stream'] |
	self;

    Last =?= 0,
    Stream ? end(Name(_parent(s2s ChannelName), Action, s2s FileId)) |
	full_inter_comm(Name, ChannelName, Action, FileId, " s2s ",
			Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(self(p2c ChannelName), Action, p2c FileId)) |
	full_inter_comm(Name, ChannelName, Action, FileId, " p2c ",
			Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(parent(p2c ChannelName), Action, p2c FileId)) |
	full_inter_comm(Name, ChannelName, Action, FileId, " c2p ",
			Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_parent(exit ChannelName), Action, exit FileId)),
    Action =?= RECEIVED_ARROW |
	full_inter_comm(Name, ChannelName, Action, FileId, " expel ",
			Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_parent(exit ChannelName), Action, exit FileId)),
    Action =?= SENT_ARROW |
	full_inter_comm(Name, ChannelName, Action, FileId, " exit ",
			Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_parent(enter ChannelName), Action, enter FileId)),
    Action =?= RECEIVED_ARROW |
	full_inter_comm(Name, ChannelName, Action, FileId, " accept ",
			Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_parent(enter ChannelName), Action, enter FileId)),
    Action =?= SENT_ARROW |
	full_inter_comm(Name, ChannelName, Action, FileId, " enter ",
			Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_parent(merge(ChannelName)), Action, merge(FileId))),
    Action =?= RECEIVED_ARROW |
	full_inter_comm(Name, ChannelName, Action, FileId, " merge+ ",
			Fd, Offset, Last'),
	self;

    Last =?= 0,
    Stream ? end(Name(_parent(merge(ChannelName)), Action, merge(FileId))),
    Action =?= SENT_ARROW |
	full_inter_comm(Name, ChannelName, Action, FileId, " merge- ",
			Fd, Offset, Last'),
	self;

/* For ambient merge */
    Last =?= 0,
    Stream ? reset(Prefix),
    convert_to_string(Prefix, SPrefix),
    string_to_dlist(SPrefix, LPrefix, []),
    list_to_string([CHAR_BANG | LPrefix], ResetPrefix) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, ResetPrefix, Last')) |
	self;

    Last =?= 0,
    Stream ? reset(AmbientName(UniqueId)),
    convert_to_string(UniqueId, UniqueId'),
    string_to_dlist(UniqueId', CU, [CHAR_RIGHT_PAREN]),
    string_to_dlist(AmbientName, CN, [CHAR_LEFT_PAREN | CU]),
    list_to_string(CN, Prefix) :
      Stream'' = [reset(Prefix) | Stream'] |
	self;

    Stream ? ambient(_) |
	/* Just ignore it for now */
	self;

    Last =\= 0 :
      Events = _,
      Scale = _,
      Stream = _,
      Stream' = file_error - Last |
	filter_end;

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
      Scale = _ |
	close_file.

  full_inter_comm(Name, ChannelName, Action, FileId, Capability,
		  Fd, Offset, Last) :-

    string(FileId),
    string_to_dlist(FileId, CI, []),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(Capability, CC, CN),
    string_to_dlist(Action, CA, CC),
    string_to_dlist(Name, List, [CHAR_SPACE | CA]) |
	output_minus_string;

    FileId =?= CreatedId(UniqueId),
    convert_to_string(UniqueId, UniqueIdString),
    string_to_dlist(UniqueIdString, CU, [CHAR_RIGHT_PAREN]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_PAREN | CU]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(Capability, CC, CN),
    string_to_dlist(Action, CA, CC),
    string_to_dlist(Name, List, [CHAR_SPACE | CA]) |
	output_minus_string.


output_minus_string(List, Fd, Offset, Last) :-
    list_to_string([CHAR_MINUS | List], String) :
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, String, Last)).


filter_end(Stream, Events, Fd, Scale, Offset) :-

    Stream ? idle(Number),
    Number' := Scale*Number,
    convert_to_string(Number', String) :
      Events = _,
      Stream' = _,
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, String, _Last)) |
	close_file;

    Stream ? Element,
    otherwise :
      Events = _,
      Scale = _,
      Stream' = _,
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, "?", _Last)) |
	close_file,
	fail((data:Element));

    Stream =?= [] :
      Events = _,
      Scale = _ |
	close_file;

    otherwise :
      Events = _,
      Scale = _,
      execute(Offset, FILE_BUFFER(Fd, CHAR_EOL, "?", _Last)) |
	close_file,
	fail((format:Stream)).


close_file(Fd, Offset) :-
    known(Offset), known(Fd) :
      execute(Offset, {FILE_CLOSE, Fd});
% | screen#display(close_file(Fd,Offset) - ok);
    otherwise : Offset = _, Fd = _ .
% | screen#display(close_file(Fd,Offset) - ng).
