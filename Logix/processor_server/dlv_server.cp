/*

Dlv server
Bill Silverman - 6/93

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:39 $
Currently locked by 	$Locker:  $
			$Revision: 1.1.1.1 $
			$Source: /home/qiana/Repository/Logix/processor_server/dlv_server.cp,v $

Copyright (C) 1993, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([start/1]).
-mode(trust).
-language(dfcp).

start(In) :-

    list(In) |
	filter(In, In', []),
	dlv # initial(In'?);

    In = [] |
      true.

filter(In, Out, Servers) :-

    In ? Request,
    Request = Function(Received, Common),
    Function = Interface(_Predicate, Reply),
    string(Interface),
    writable(Reply) |
      Common = done,
      Received = true,
      Out ! Function,
	self;

    In ? Start,
    Start = Initial(Received, Common),
    Initial = Interface(Protocol),
    string(Interface),
    tuple(Protocol) |
      Common = done,
	filter_protocol;

    In ? Start,
    Start = Initial(Received, Common),
    Initial = Interface(Identifier, Protocol),
    string(Interface),
    tuple(Protocol) |
      Common = done,
	filter_identified_protocol;

    In ? {increment(Identifier, Ok?), Received, Common} |
      Common = done,
      Received = true,
	member(Identifier, Servers, Answer),
	increment_server;

    In ? {decrement(Identifier, Ok?), Received, Common} |
      Common = done,
      Received = true,
	member(Identifier, Servers, Answer),
	decrement_server;

    In ? {connect(Identifier), Received, Common},
    listener(Identifier) |
      Common = done,
      Received = true,
	_ = Ok?,
	member(Identifier, Servers, Answer),
	connect_server;

    In ? {connect(Identifier, Ok?), Received, Common} |
      Common = done,
      Received = true,
	member(Identifier, Servers, Answer),
	connect_server;

    otherwise,
    In ? Other(Received, Common) |
      Common = done,
      Received = true,
      Out ! Other,
	self;

    In ? _Any(Received, Aborted),
    known(Aborted) |
      Received = false(aborted),
	self;

    In = [] |
      Out = [],
	close_servers.

  close_servers(Servers) :-

    Servers ? _Server(_, disconnected(_, Abort)) |
      Abort = abort,
	self;

    Servers ? _Other,
    otherwise |
	self;

    Servers = [] | true.

filter_identified_protocol(In, Out, Servers, Interface, Protocol, Received,
				Identifier
) :-

    Protocol = room(Server, Ok?),
    listener(Identifier) |
	Port = -1,
	member(Identifier, Servers, Answer),
	existing_server;

    Protocol = room(Server, Port, Ok?),
    listener(Server) |
	member(Identifier, Servers, Answer),
	existing_server;

    otherwise |
      Out ! Interface(Identifier, Protocol, Received),
	filter.

filter_protocol(In, Out, Servers, Interface, Protocol, Received) :-

    Protocol = room(Server, Ok?),
    constant(Server) |
	Port = -1,
	member(Server, Servers, Answer),
	existing_server;

    Protocol = room(Server, Port, Ok?),
    constant(Server) |
	member(Server, Servers, Answer),
	existing_server;

    otherwise |
      Out ! Interface(Protocol, Received),
	filter.


existing_server(In, Out, Answer, Interface, Ok, Port, Received, Server) :-

    Answer = found(Servers, Identifier, Other, Count, Status, Insert, Tail),
    Other =\= Server |
      Insert = Tail,
      Interface = _, Port = _,
      Received = true,
      Ok = false(server_id_conflict),
	Servers' = [Identifier(Other, Count, Status) | Servers],
	filter;

    Answer = found(Servers, Identifier, Any, Count, Reconnecting, Insert, Tail),
    Reconnecting = reconnecting(Reply, _), unknown(Reply) |
      Interface = _, Port = _, Server = _,
      Received = true,
      Ok = false(reconnecting),
      Insert = [Identifier(Any, Count, Reconnecting) | Tail],
	filter;

    Answer = found(Servers, Identifier, Server, Count++, _Status, Insert, Tail),
    otherwise |
      Insert = Tail,
      Interface = _, Port = _,
      Received = true,
      Ok = true,
	Servers' = [Identifier(Server, Count', connected) | Servers],
	filter;

    Answer = not_found(Servers, Identifier, Insert),
    Server = Identifier |
      Insert = [],
      Out ! Interface(room(Server, Port, Ready), Received),
	new_server;

    Answer = not_found(Servers, Identifier, Insert),
    Server =\= Identifier,
    listener(Server),
    listener(Identifier) |
      Insert = [],
      Out ! Interface(identified_room(Server, Identifier, Port, Ready),
		      Received
	    ),
	new_server.

  new_server(In, Out, Servers, Ready, Ok, Server, Identifier) :-

    Ready = true |
      Ok = true,
	Servers' = [Identifier(Server, 1, connected) | Servers],
	filter;

    Ready =\= true |
      Server = _,
      Identifier = _,
      Ok = Ready,
	filter.


increment_server(In, Out, Answer, Ok) :-

    Answer = found(Servers, Identifier, Server, Count, Reconnecting, Insert, Tail),
    Reconnecting = reconnecting(Reply, _), unknown(Reply) |
      Ok = false(reconnecting),
      Insert = [Identifier, Server(Count, Reconnecting) | Tail],
	filter;

    Answer = found(Servers, Identifier, Server, Count++, _Status, Insert, Tail),
    otherwise |
      Insert = Tail,
      Ok = true,
	Servers' = [Identifier(Server, Count', connected) | Servers],
	filter;

    Answer = not_found(Servers, _Identifier, Insert) |
      Insert = [],
      Ok = false(unknown),
	filter.


decrement_server(In, Out, Answer, Ok) :-

    Answer = found(Servers, Identifier, Server, Count, reconnecting(Reply, Abort),
			Insert, Tail
	     ),
    unknown(Reply),
    Count = 1 |
      Abort = abort,
      Ok = true,
	filter,
	connection_aborted;

    Answer = found(Servers, Identifier, Server, Count--, Status, Insert, Tail),
    otherwise |
      Insert = [Identifier(Server, Count', Status) | Tail],
      Ok = true,
	filter;

    Answer = not_found(Servers, _Identifier, Insert) |
      Insert = [],
      Ok = false(unknown),
	filter.

  connection_aborted(Reply, Identifier, Server, Insert, Tail) :-

    Reply = true |		% already reconnected
      Insert = [Identifier(Server, 0, connected) | Tail] ;

    Reply =\= true |		% successfully aborted reconnect - remove from list
      Identifier = _,
      Server = _,
      Insert = Tail .

/* This procedure may be incorrectly synchronized in some rare cases, such
   that a late caller "breaks" a working connection - this results in the
   clients of the server becoming non-responsive (I think).  It should be
   done better/differently in the C++ version.
*/

connect_server(In, Out, Answer, Ok) :-

    Answer = found(Servers, Identifier, Server, Count, Reconnecting, Insert, Tail),
    Reconnecting = reconnecting(Reply, _), unknown(Reply) |
      Ok = true,		% Already reconnecting
      Insert = [Identifier(Server, Count, Reconnecting) | Tail],
	filter;

    Answer = found(Servers, Identifier, Server, Count, _Status, Insert, Tail),
    otherwise,
    listener(Server) |
      Insert = [Identifier(Server, Count, reconnecting(Reply?, Abort)) | Tail],
      Ok = true,
      Out ! connect(Server, Abort?, Reply),
	filter;

    Answer = not_found(Servers, _Identifier, Insert) |
      Insert = [],
      Ok = false(unknown),
	filter.


member(Identifier, Servers, Answer) + (NewServers = CopyServers?, CopyServers) :-

    Servers ? Identifier(Server, Count, Status) |
	Answer = found(NewServers, Identifier, Server, Count, Status,
			CopyServers, Servers');

    Servers ? Other, Other =\= Identifier(_,_,_) |
      CopyServers ! Other,
	self;

    Servers = [] |
	Answer = not_found(NewServers, Identifier, CopyServers) .
