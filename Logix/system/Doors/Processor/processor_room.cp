/*  $Header: /home/qiana/Repository/Logix/system/Doors/Processor/processor_room.cp,v 1.1.1.1 1999/07/09 07:03:26 bill Exp $ */


-language([inherit,dfcp]).
-mode(interrupt).
-export([create/3]).

% A processor room.
/*

Call:  Doors#Processor#create(...)
Can handle:

	log(Message,Ok);

	input(String, Ok);

	goal(Any, Ok).

	computation(Any, Any, Ok).

*/

create(IdCard, Door, Ok) :-
	+room_with_events # create.

room(Rcv, Doors, Name, Type) + (AppTbl, Start=false) :-
	+room_with_events # room_with_events;
	+application#application_table(Rcv, AppTbl);
	+initialize_application(Rcv, AppTbl);
	+application # generic_application_snapshot;
	+processor_log;
	+restart;
	+computation_requests.
	

initialize_application(Rcv, AppTbl) :-
    Rcv ? initialize_application(Ok?) |
	AppTbl' = application, AppTbl = _,
	Rcv'' = [initialize_events(Ok1),
		 initialize_application_table(Ok2)
	| Rcv'?],
	and([Ok1?, Ok2?], Ok),
	self;

    Rcv ? (_From : initialize_connection(ForServer?, Ok?)) | % client message
    	Rcv'' = [message([connections], [], 
		initialize_connection(ForServer, Ok), Ok1) 
	| Rcv'?],
	ok(Ok1?, message_to_client_connections_room),
	self;

   Rcv ? (_From : initialize_connection(ServerDoorId, ClientDoorId, 
					ForServer?, Ok?)) | % client message
    	Rcv'' = [message([connections], [], 
		initialize_connection(ServerDoorId, ClientDoorId,
							ForServer, Ok), Ok1) 
	| Rcv'?],
	ok(Ok1?, message_to_client_connections_room),
	self;

   Rcv ? (_From : initialize_connection([ServerDoorId, ClientDoorId], 
					ForServer?, Ok?)) | % client message
    	Rcv'' = [message([connections], [], 
		initialize_connection([ServerDoorId, ClientDoorId],
							ForServer, Ok), Ok1) 
	| Rcv'?],
	ok(Ok1?, message_to_client_connections_room),
	self;

    Rcv ? (_From : connect_to(Door, ClientId, Ok?)), % server message
    string(ClientId) |
	Rcv'' = [message([connections], [], 
		connect_to(Door?, ClientId, Ok), Ok1) 
	| Rcv'?],
	ok(Ok1?, message_to_server_connections_room),
	self;

    Rcv ? (_From : connect_to(_Door, ClientId, _)), % server message
    invalid(ClientId) |
	self;

    Rcv ? (_From : connect_to(Door, _ClientId, _)), % server message
    invalid(Door) |
	self.



processor_log(Rcv,Doors, Start) :-

    Rcv ? (_FromId: log(_Message, Ok?)),
    Start = false |
	Ok = true,
	self;

    Rcv ? (FromId: log(Message, Ok?)),
    Start = true,
    ground(FromId) |
	% Contribution to log
	processor # interface(date(Date), Ok1), ok(Ok1?),
	Rcv'' = [event(application, (FromId : log(Date?, Message?))) | Rcv'?],
	Ok = true,
	self.

restart(Rcv, Start) :-

    Rcv ? (_From : restart(Update)) |
	Start = _,
	Start' = true,
	Rcv'' = [([self] : update(id_card(self, Update), Ok)) | Rcv'?],
	ok(Ok?, "processor_room restart"),
	self.

computation_requests(Rcv) :-

    Rcv ? (_From : input(String, Ok)) |
	Ok = true,
	computation # input(string, String),
	self;

    Rcv ? (_From : goal(Goal, Ok)) |
	Ok = true,
	computation # shell(Goal),
	self;

    Rcv ? (_From : computation(Requests, Events, Ok)) |
	Ok = true,
	computation # call(Requests, Events),
	self.
