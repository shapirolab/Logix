/*

William Silverman 04/08/85
modified: Avshalom Houri 12/87

Device manager process

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:38 $
Currently locked by 	$Locker:  $
			$Revision: 1.1.1.1 $
			$Source: /home/qiana/Repository/Logix/processor_server/device_server.cp,v $

Copyright (C) 1992, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-mode(trust).
-export([start/2]).
-language(compound).


procedure start(Input, Channel).

Input ::= {Command, Ok, Common}.

Initial ::= [String(Chars) | In] ; [].
Chars ::= Any.


start(Input, Defer) :-
   true : Signals = _ |
	filter(Input, In, Defer),
	creator.

procedure filter(Input, In, Channel).

filter(Input, In, Defer) :-

    Input ? {Command, Ok, Common},
    Command =\= signals(_),
    arg(2, Command, Device),
    unknown(Device) :
      write_channel(defer(Device, {device(Command, Ok), Common}), Defer) |
	self;

    Input ? {quit, Ok, Common} : Defer = _,
      In = [],
      Ok = true |
	self,
	terminate(In'?),
	unify_without_failure(Common, done);

    Input ? Other,
    otherwise :
      In ! Other |
	self;

    Input = [] : Defer = _,
      In = [] .

terminate(In) :-

    In ? {_, Ok, Common} :
      Ok = false(terminated) |
	self,
	unify_without_failure(Common,done);

    In = [] | true.

procedure create(Device, Stream).
procedure open(Device).
procedure close(Device).
procedure quit(Device).
procedure signals(Stream).
procedure restart.

In ::= [Command] ; [].
State ::= open ; closed.
Events ::= [read].
Offset ::= Integer.

Command ::= open ; close ; quit.

procedure creator(In, Stream).

creator(In, Signals) :-

    In ? {create(Device, Stream?), Ok, Common},
    string(Device) :
      Common = processor(link(lookup(Device, Offset), Reply), Common') |
	create_stream;

    In ? {restart, true^, done^} :
      Signals = [restart | Signals'?] |
	self;

    In ? {signals(Stream), true^, done^} :
      Stream = Signals? |
	self;

    In ? {_Other, Ok, Common},
    otherwise,
    unknown(Common) :
      Ok = false(unknown) |
	self,
	unify_without_failure(Common, done);

    In ? {_Any, Ok, Common},
    known(Common) :
      Ok = false(aborted) |
	self;

    In = [] :
      Signals = [] .

procedure create_stream(In, Stream, Reply, Device, Offset, Stream, Ok, Common).

create_stream(In, Signals, Reply, Device, Offset, Ok, Stream, Common) :-

    Reply = true :
      Ok = true,
      State = closed,
      Events = _ |
	server,
	creator(Out?, Signals);

    Reply =\= true : Device = _, Offset = _,
      Stream = [],
      Ok = Reply |
	creator,
	unify_without_failure(Common, done).


procedure server(In, State, Device, Offset, Events, Stream, Common).

server(In, State, Device, Offset, Events, Stream, Common, Out) :-

    In ? {open(Device), true^, Done},
    State = closed : Events = _ |
	self,
	opener(Offset, State', Events'),
	unify_without_failure(Done, done);

    In ? {open(Device), true^, Done},
    State = open |
	self,
	unify_without_failure(Done, done);

    In ? {close(Device), true^, Done} : Events = _,
      Events' = _, State' = closed |
	closer(State, Offset, Offset'),
	self,
	unify_without_failure(Done, done);

    In ? {stop(Device), true^, Done} : Device = _, Events = _,
      Stream = [],
      Out = In'? |
	closer(State, Offset, _),
	unify_without_failure(Common, done),
	unify_without_failure(Done, done);

    In ? Restart, Restart = {restart, _, _},
    State = open : Events = _,
      Out ! Restart |
	self,
	opener(Offset, State', Events');

    In ? Restart, Restart = {restart, _, _},
    State = closed :
      Out ! Restart |
	self;

    In ? {create(Device, Stream?), true^, Done} |
	self,
	unify_without_failure(Done, done);

    In ? Create,
    Create = {create(Other, _), _, _}, Other =\= Device :
      Out ! Create |
	self;

    In ? Signals, Signals = {signals(_), _, _} :
      Out ! Signals |
	self;

    otherwise,
    In ? Other :
      Out ! Other |
	self;

    Events ? read,
    unknown(In) :
      execute(Offset, read(Stream, Stream')) |
	self;

    In = [] : Device = _, Events = _,
      Stream = [],
      Out = [] |
	closer(State, Offset, _),
	unify_without_failure(Common, done);

    known(Common) : State = _, Device = _, Events = _,
      Stream = [],
      Out = In |
	closer(State, Offset, _).


procedure opener(Offset, State, Events).

opener(Offset, State, Events) :-
    true :
      execute(Offset, open(Events)),
      State = open ;

    otherwise : Offset = _, Events = _,
      State = closed |
	true.


Offset1, Offset2 ::= Offset.

procedure closer(State, Offset1, Offset2).

closer(State, Offset1, Offset2) :-
    State = open :
      execute(Offset1, {close}),
      Offset1 = Offset2 ;

    otherwise :
      State = _,
      Offset1 = Offset2.
