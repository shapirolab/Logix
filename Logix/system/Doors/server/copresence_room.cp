/*  $Header: /home/qiana/Repository/Logix/system/Doors/server/copresence_room.cp,v 1.1.1.1 1999/07/09 07:03:27 bill Exp $ */
-language(dfcp).
-mode(interrupt).
-export([create, room]).
-include([api_includes,server_includes]).
-include(string_includes).

/************************************************************************
*** copresence_room *** manages people and their conversation in a room
*
*
*************************************************************************/

PEOPLE_ROOM => [people].
PLACES_ROOM => [super].
SCI_ROOM => [sci].

create(IdCard,Door,Ok)  :-
	+basic_room#create.

room(Rcv, Doors, Name, Type) +
 (URL, PlaceIdleTime, EventsCacheMax, 
  MaxPresencesPerPlace, ActualPresencesPerPlace = 0,
  PersonEventsOperations, PlaceState, PlaceTimeStamp, NoStateEventsOperations):-

  initially |
	PlaceTimeStamp = "",
	PlaceState = [];

+room_with_events#room_with_events;
+initialize_application;
+copresence_events#copresence_events;
+presence_requests;
+queries_and_updates;	
+connections_requests;
+create_object;
+events;
+state_changes.

initialize_application(Rcv, URL, PlaceIdleTime,
 EventsCacheMax, PersonEventsOperations, ActualPresencesPerPlace,
 MaxPresencesPerPlace,
 NoStateEventsOperations) :-
  Rcv ? initialize_application(Ok?) |
	Rcv'' = [([self] : show(id_card(self), IdCard, Ok)) | Rcv'?],
	URL = _, PlaceIdleTime = _, EventsCacheMax = _,
	PersonEventsOperations = _, NoStateEventsOperations = _,
	MaxPresencesPerPlace = _, 
	id_card#query(IdCard?, [(name: URL'), (idle_time : PlaceIdleTime'),
                (events_cache_max : EventsCacheMax'),
		(person_events_operations : PersonEventsOperations'),
		(no_state_events_operations : NoStateEventsOperations'),
		(max_presences_per_place : MaxPresencesPerPlace')]),
	self;

  Rcv ? (_: paste_presence(Door, DoorId, Ok?)), constant(DoorId),
  integer(MaxPresencesPerPlace),
  ActualPresencesPerPlace <  MaxPresencesPerPlace |
	Rcv'' = [paste_presence1(Door, DoorId, Ok) | Rcv'?],
	ActualPresencesPerPlace++,
	self;

  Rcv ? (_: paste_presence(Door, DoorId, Ok?)), constant(DoorId),
  MaxPresencesPerPlace = [] |
	Rcv'' = [paste_presence1(Door, DoorId, Ok) | Rcv'?],
	self;

  Rcv ? (_: paste_presence(_Door, _DoorId, Ok?)), 
  integer(MaxPresencesPerPlace),
  ActualPresencesPerPlace >=  MaxPresencesPerPlace |
	Ok = false(DOORS_PLACE_CONNECT_PLACE_IS_FULL),
	self;

  Rcv ? paste_presence1(Door, DoorId, Ok?), constant(DoorId) |
	Rcv'' = [([self] : paste(Door, DoorId, Ok1)),
		 ([self] : update(id_card(DoorId,
			 [(PRESENCE_TYPE_ATTR : PRESENCE_TYPE_STR),
				(SUBSCRIBED_EVENTS_ATTR : [])]), Ok2))
	| Rcv'?],
	and([Ok1?, Ok2?], Ok),
	self.

/* Presences are managed in the Doors.
 * We keep PresenceId as a parameter to all messages so that messages about
 * a presence can come through any door and in case we later
 * choose to change the representation to one of an attr_value_table.
*/ 
presence_requests(Rcv, Doors, URL, PlaceIdleTime, 
ActualPresencesPerPlace, MaxPresencesPerPlace,
PersonEventsOperations, NoStateEventsOperations, PlaceState, PlaceTimeStamp)
 + (GoAway):-

  initially |
	GoAway = false;

  Rcv ? ( _ : initialize_copresence(Ok?)) |
	Rcv'' = [listen_to(PEOPLE_ROOM, [user_details], [], Ok1), 
		 listen_to(PLACES_ROOM, [server_broadcast], [], Ok2)
	| Rcv'?],
	and([Ok1?, Ok2?], Ok),
	self;

/* CONNECT PRESENCE */
  Rcv ? (_ : connect_presence(PresenceId, PlaceState, PresenceStateTuple,
						 OldDoor)), 
  ground(PresenceStateTuple), ground(OldDoor), 
  constant(PresenceId) |
	GoAway = _, GoAway' = false,
	Rcv'' = [connect_presence1(PresenceId, PlaceState, PresenceStateTuple, 
					OldDoor)
	| Rcv'?],	
	self;

  Rcv ? connect_presence1(PresenceId, PlaceState, PresenceStateTuple, 
							OldDoor),
  ground(OldDoor), ground(PresenceStateTuple), constant(PresenceId), 
  ground(PersonEventsOperations), ground(PlaceState) |
	Rcv'' = [([self] : update(id_card(PresenceId, 
			[(PRESENCE_TYPE_ATTR : PRESENCE_TYPE_STR)]), Ok1)),
		 ([self] : show(id_card(PresenceId), IdCard, Ok2)),
		([self] : got_doorsEvent(PresenceId, PersonEventsOperations,
		 DOORS_PLACE, PRESENCE_EVENT_CATEGORY, PRESENCE_ENTERED_EVENT, 
		{PlaceState, PresenceStateTuple'?, OldDoor})) | Rcv'?],
	ok([Ok1?, Ok2?], presence_type),
	id_card#query(IdCard?, [(PRESENCE_JOINED_DATE_ATTR : JoinedDate)]),
	replace_joined_date(PresenceStateTuple, JoinedDate?, 
						PresenceStateTuple'),
	self;

/* DISCONNECT or TIME OUT PRESENCE */

  Rcv ? (_ : timeout_presence(PresenceId, Ok)),
  constant(PresenceId), string(URL) |
	Rcv'' = [decrement_actual_presences,
		cleanup(PresenceId, []),
		([self] : discard(PresenceId, Ok1)),
		message(SCI_ROOM, [],
			doorsPropagateError(DOORS_PLACE_CONNECT_ERROR_CATEGORY,
				DOORS_PLACE_CONNECT_TIMED_OUT,
				ERROR_SEVERITY_ERROR,
				URL, PresenceId, [],
				[PresenceId]), Ok2),
		disconnect_presence1(Ok1?, PresenceId, [], Ok)
	| Rcv'?],
	ok(Ok2?, timeout_cannot_complete_error_to_sci),
	self;

  Rcv ? (_ : disconnect_presence(PresenceId, NewDoor, Ok)), 
  constant(PresenceId), ground(NewDoor) |
%	computation#display("Copresence room got disconnect_presence"),
	Rcv'' = [decrement_actual_presences,
		cleanup(PresenceId, NewDoor),
		([self] : discard(PresenceId, Ok1)),
		disconnect_presence1(Ok1?, PresenceId, NewDoor, Ok)
	| Rcv'?],
	self;

  Rcv ? decrement_actual_presences, MaxPresencesPerPlace = [] |
	self;

  Rcv ? decrement_actual_presences, integer(MaxPresencesPerPlace),
  integer(ActualPresencesPerPlace) |
	ActualPresencesPerPlace--,
	self;

  Rcv ? cleanup(PresenceId, NewDoor), constant(PresenceId) |
	Rcv'' = [([self] : show(id_card(PresenceId), IdCard, Ok1)),
		 do_choose_events(PresenceId, EventsToDelete?, delete),
		 cleanup_objects(PresenceId, NewDoor, Objects?),
		 cleanup_connection(PresenceId, Connection?)
	| Rcv'?],
	ok(Ok1?, cleanup),
	id_card#query(IdCard?, [(SUBSCRIBED_EVENTS_ATTR:EventsToDelete),
				(PRESENCE_OBJECTS_ATTR : Objects),
				(PRESENCE_CONNECTION_ATTR : Connection)]),
	self;

  Rcv ? cleanup_objects(PresenceId, [], Objects),
  constant(PresenceId), Objects ? Object |
	id_card#query(Object, [(PRESENCE_OBJECT_PASSENGERS_ATTR : Passengers),
				(PRESENCE_OBJECT_OBJECT_ID_ATTR:ObjectId)]),
	processor#interface(gmtime(Date)),
	listen2(R?, R1, R2),
	listen2(Date?, Date1, Date2),
	listen2(ObjectId?, O1, O2),
	Rcv'' = [compute_recipients(PLACE_EVENT_CATEGORY, DOORS_PLACE, _, R),
			do_cache_event(PresenceId, Date1?,
			PLACE_EVENT_CATEGORY,
			PLACE_DELETED_OBJECT_EVENT, DOORS_PLACE,
			O1?, R1?),
		broadcast_doorsEvent(true, PresenceId, Date2?,
			PLACE_EVENT_CATEGORY,
			PLACE_DELETED_OBJECT_EVENT, DOORS_PLACE,
			O2?, R2?),
		break_connections(Passengers?),
	cleanup_objects(PresenceId, [], Objects') | Rcv'?],
	self;

  Rcv ? cleanup_objects(PresenceId, NewDoor, Objects),
  constant(PresenceId), Objects ? Object, NewDoor =\= [] |
	id_card#query(Object, [(PRESENCE_OBJECT_PASSENGERS_ATTR : Passengers)]),
	Rcv'' = [break_connections(Passengers?),
	cleanup_objects(PresenceId, NewDoor, Objects') | Rcv'?],
	self;

  Rcv ? cleanup_objects(_, _, []) |
	self;

  Rcv ? cleanup_objects(_, _, undefined) |
	self;

  Rcv ? cleanup_connection(PresenceId, Connection), constant(PresenceId),
  Connection = {DriverId, ObjectId, DOORS_CONNECTION_TO_OBJECT},
  constant(DriverId), constant(ObjectId) |
	processor#interface(gmtime(Date)),
	listen2(R?, R1, R2),
	listen2(Date?, Date1, Date2),
	Rcv'' = [delete_passenger(DriverId, ObjectId, PresenceId),
		compute_recipients(PLACE_EVENT_CATEGORY, DOORS_PLACE, _, R),
			do_cache_event(PresenceId, Date1?,
			PLACE_EVENT_CATEGORY,
			PLACE_DISCONNECTED_OBJECT_EVENT, DOORS_PLACE,
			{DriverId, ObjectId},
			R1?),
		broadcast_doorsEvent(true, PresenceId, Date2?,
			PLACE_EVENT_CATEGORY,
			PLACE_DISCONNECTED_OBJECT_EVENT, DOORS_PLACE,
			{DriverId, ObjectId},
			R2?),
		 replace_connection(PresenceId, []) | Rcv'?],
	self;

  Rcv ? cleanup_connection(PresenceId1, Connection), 
  Connection = {PresenceId2, _ObjectId, DOORS_CONNECTION_TO_PRESENCE} |
	Rcv'' = [replace_connection(PresenceId1, []),
		 replace_connection(PresenceId2, [])
	| Rcv'?],
	self;

  Rcv ? cleanup_connection(_PresenceId, []) |
	self;

  Rcv ? delete_passenger(DriverId, ObjectId, PassengerId),
  constant(DriverId), constant(PassengerId) |
	Rcv'' = [([self] : show(id_card(DriverId), IdCard, Ok1)),
                 ([self] : update(id_card(DriverId, IdCard'?),Ok2)),
		 ([self] : show(id_card(PassengerId), PassIdCard, Ok3))
	| Rcv'?],
	listen2(IdCard?, IdCard1, IdCard2),
        id_card#query(IdCard1?, [(PRESENCE_OBJECTS_ATTR : Objects)]),
        id_card#update(IdCard2?, [(PRESENCE_OBJECTS_ATTR : Objects'?)],
                                                                IdCard'),
	id_card#query(PassIdCard?, [(PRESENCE_POSITION_ATTR : Pos)]),
	delete_passenger(Objects?, ObjectId, Objects', PassengerId, Pos?),
	ok([Ok1?, Ok2?, Ok3?], delete_passenger),
	self;

  Rcv ? disconnect_presence1(Error, PresenceId, _NewDoor, Ok), 
  Error =\= true, string(URL), ground(Error) |
	Ok = Error,
	ok(Error, diconnect_presence(PresenceId)),
	self;

  Rcv ? disconnect_presence1(true, PresenceId, NewURL, Ok),
  ground(NewURL), constant(PresenceId), ground(PersonEventsOperations) |
	Rcv'' = [([self] : got_doorsEvent(PresenceId, PersonEventsOperations,
		 DOORS_PLACE, PRESENCE_EVENT_CATEGORY, PRESENCE_LEFT_EVENT,
		 NewURL)),
		destroy_if_no_people | Rcv'?],
	Ok = true,
	self;

  Rcv ? destroy_if_no_people |
	Rcv'' = [([self] : show(doors, Entries, Ok1)),
		destroy_if_no_people(Ok1?, Entries?)
	| Rcv'?],
	self;

  Rcv ? destroy_if_no_people(true, Entries),
  Entries = [_Entry1, _Entry2, _Entry3, _Entry4 | Rest], Rest =\= [] |  
  	self;

  Rcv ? destroy_if_no_people(Error, _Entries), Error =\= true |
	ok(Error, show_doors),
  	self;

  Rcv ? destroy_if_no_people(true, Entries), ground(PlaceIdleTime),
  Entries = [_Entry1, _Entry2, _Entry3, _Entry4] |% for self, super, sci, people
	% Set timer so that room can disappear automatically
%	processor#interface(gmtime(Date)),
%	computation#display(term, (Date? : 
%	"Room ready to be destroyed - setting timer"), type(ground)),
	processor # time(signals(real, PlaceIdleTime, 1, GoAway')),
	GoAway = _,
	self;

  GoAway ? tick |
	Rcv' = [message([super], [], ok_to_disappear(Ok1), Ok2),
		 do_destroy_if_no_people(Ok3?) | Rcv?],
	and([Ok1?, Ok2?], Ok3), 
	self; 

  Rcv ? do_destroy_if_no_people(true) |
%	processor#interface(gmtime(Date)),
%	computation#display(term, (Date? : 
%		"Room going away"), type(ground)),
	Rcv'' = [([self] :  close(_Ok)) | Rcv'?],
	self;

  Rcv ? do_destroy_if_no_people(false(denied)) |
	self; % can wait until next interval 
	
  Rcv ? do_destroy_if_no_people(Error), Error =\= false(denied),
  Error =\= true |
	ok(Error, destroy_if_no_people),
	self. % can wait until next interval

queries_and_updates(Rcv, PlaceState, URL) :-
+place_queries;
+object_queries;
+update_user_details.

place_queries(Rcv, PlaceState, URL):-
  Rcv ? (_ : respond_query_snapshot(PresenceId, ClientData)), 
  ground(PlaceState), string(URL) |
	Rcv'' = [([self]:show(doors([(PRESENCE_TYPE_ATTR:PRESENCE_TYPE_STR)]), 
								Snapshot, Ok1)),
		 message(SCI_ROOM, [],
		 doorsPropagateResponse(URL, PresenceId, ClientData, 
			PLACE_RESPONSE_CATEGORY, PLACE_SNAPSHOT_RESPONSE,
				{PlaceState, Snapshot'?}), Ok2)
	| Rcv'?],
	server_api#presence_state_list_to_tuple(Snapshot?, Snapshot'),
	ok([Ok1?, Ok2?], respond_query_presences_snapshot),
	self;

  Rcv ? (_ : respond_query_place_presences(PresenceId, ClientData)),
  string(URL) |
	Rcv'' = [([self]:show(doors([(PRESENCE_TYPE_ATTR:PRESENCE_TYPE_STR)]), 
								Snapshot, Ok1)),
		 message(SCI_ROOM, [],
		 doorsPropagateResponse(URL, PresenceId, ClientData, 
			PLACE_RESPONSE_CATEGORY, PLACE_PRESENCES_RESPONSE,
				Presences?), Ok2)
	| Rcv'?],
	get_presences_from_snapshot(Snapshot?, Presences),
	ok([Ok1?, Ok2?], respond_query_place_presences),
	self;

  Rcv ? (_ : get_place_presences(Presences?)) |
	Rcv'' = [([self]:show(doors([(PRESENCE_TYPE_ATTR:PRESENCE_TYPE_STR)]), 
								Snapshot, Ok1))
	| Rcv'?],
	get_presences_from_snapshot(Snapshot?, Presences),
	ok(Ok1?, include_place_presences),
	self;

  Rcv ? (_ : add_to_count_place_presences(OldCount, NewCount)) |
	Rcv'' = [([self]:show(doors([(PRESENCE_TYPE_ATTR:PRESENCE_TYPE_STR)]), 
								Snapshot, Ok1))
	| Rcv'?],
	add_to_count_presences_from_snapshot(Snapshot?, OldCount, NewCount),
	ok(Ok1?, add_to_count_place_presences),
	self.

object_queries(Rcv, URL) :-

  Rcv ? (_ : respond_query_object_connections(PresenceId, ClientData,
                        ConnecteePresenceId, ConnecteeObjectId)),
  ground(ConnecteePresenceId), ground(PresenceId), constant(ConnecteeObjectId) |
	Rcv'' = [get_passengers(ConnecteePresenceId, ConnecteeObjectId,
				Passengers),
		respond_query_object_connections1(PresenceId, ClientData,
			ConnecteePresenceId, ConnecteeObjectId, Passengers?)
	| Rcv'?],
	self;

  Rcv ? get_passengers(ConnecteePresenceId, ConnecteeObjectId, Passengers),
  ground(ConnecteePresenceId), ground(ConnecteeObjectId) |
	Rcv'' = [([self] : show(id_card(ConnecteePresenceId), IdCard, Ok1)),
		get_passengers1(Ok1?, IdCard?, ConnecteeObjectId,
				Passengers)
	| Rcv'?],
	self;

  Rcv ? get_passengers1(Ok, _IdCard, _ConnecteeObjectId, Passengers),
  Ok =\= true |
	Passengers = [],
	self;

  Rcv ? get_passengers1(true, IdCard, ConnecteeObjectId, Passengers?),
  constant(ConnecteeObjectId) |
	id_card#query(IdCard, [(PRESENCE_OBJECTS_ATTR : Objects)]),
	query_object_ok(Objects?,  ConnecteeObjectId,
		 [(PRESENCE_OBJECT_PASSENGERS_ATTR : P)], _Ok),
	Rcv'' = [get_passengers2(P?, Passengers) | Rcv'?],
	self;

  Rcv ? get_passengers2(undefined, Passengers) |
	Passengers = [],
	self;

  Rcv ? get_passengers2(P, Passengers), P =\= undefined |
	Passengers = P,
	self;

  Rcv ? respond_query_object_connections1(PresenceId,
	ClientData, ConnecteePresenceId, ConnecteeObjectId, Passengers),
  string(URL) |
	Rcv'' = [message(SCI_ROOM, [],
                 doorsPropagateResponse(URL, PresenceId, ClientData, 
                    PLACE_RESPONSE_CATEGORY, PLACE_OBJECT_CONNECTIONS_RESPONSE,
                    {ConnecteePresenceId, ConnecteeObjectId, Passengers}), Ok)
        | Rcv'?],
	ok(Ok?, respond_query_object_connections2),
	self.

update_user_details(Rcv) :-
 
/* UPDATE USER DETAILS */
  Rcv ? (_ : event(user_details, _Date, [FullName, ObjectName, TimeStamp])),
  string(FullName),  constant(ObjectName), constant(TimeStamp) |
	Rcv'' = [([self] : show(doors([(PERSON_FULLNAME_ATTR : FullName)]),
			 				Presences, Ok1)),
		update_user_details(Presences?, ObjectName, TimeStamp)
	| Rcv'?],
	ok(Ok1?, show_updated_people), 
	self;

  Rcv ? (_ : event(user_details, _Date, refresh(_))) |
	self;

  Rcv ? update_user_details([Presence | Rest], "", TimeStamp), 
  Presence  = (PresenceId - _), constant(PresenceId),
  constant(TimeStamp) |
	IdCard = [(PRESENCE_TIMESTAMP_ATTR : TimeStamp)],
	Rcv'' = [([self] : update(id_card(PresenceId, IdCard?), Ok1)),
		update_user_details(Rest,  "", TimeStamp)
	| Rcv'?],
	ok(Ok1?, update_user_details),
	self;
	
  Rcv ? update_user_details([Presence | Rest], ObjectName, TimeStamp), 
  ObjectName =\= "",
  Presence  = (PresenceId - PresenceCard), constant(PresenceId), 
  constant(ObjectName), constant(TimeStamp), ground(PresenceCard) |
	UpdateCard = [(OBJECT_TIMESTAMP_ATTR: TimeStamp)],
	Rcv'' = [update_object_by_name(PresenceId, ObjectName, UpdateCard?),
		update_user_details(Rest, ObjectName, TimeStamp)
	| Rcv'?],
	self;
	
  Rcv ? update_user_details([], _,  _) |
	self.

events(Rcv, URL) :-

/* SERVER_WIDE BROADCAST MESSAGES */
  Rcv ? (_ : event(server_broadcast, _Date, [PresenceId, Operations, To, 
	EventCategory, EventType, Contents])) |
	Rcv'' = [([self]: got_doorsEvent(PresenceId, Operations, To,
		EventCategory, EventType, Contents))
	| Rcv'?],
	self;

  Rcv ? (_ : event(server_broadcast, _Date, refresh(_))) |
	self;

/* PLACE EVENTS BROADCASTING */
  Rcv ? broadcast_doorsEvent(true, PresenceId, GmtDate, 
			EventCategory, EventType,To, Contents, Recipients),
  string(URL),
  constant(EventCategory), constant(EventType), ground(To),
  ground(Contents), ground(Recipients) |
	Rcv'' = [message(SCI_ROOM, [],
		doorsPropagateEvent(URL, PresenceId, GmtDate, EventCategory,
		EventType, To, Contents, Recipients), Ok)
	| Rcv'?],
/*	computation#display(term, [broadcasting, EventCategory, "", 
		EventType, " ", To, " ", Contents, " ", Recipients],
		type(ground)),
*/
	ok(Ok?, broadcast_doors),
	self;

  Rcv ? broadcast_doorsEvent(DoBroadCast, _PresenceId, _GmtDate, 
		_EventCategory, _EventType, _To, _Contents, _Recipients),
  DoBroadCast =\= true |
	self.

state_changes(Rcv, PersonEventsOperations) :-

/* PRESENCE STATE UPDATES */
  Rcv ? update_state(DoState, _PresenceId, _Date, _EventCategory,
						 _EventType, _Contents),
  DoState =\= true |
/*	computation#display(term,
	  [" Not Updating ", PresenceId, 
		EventCategory, EventType, Contents], type(ground)),
*/
	self;

  
  Rcv ? update_state(true, PresenceId, Date, 
				EventCategory, EventType, Contents),
  constant(EventCategory), constant(EventType), 
  ground(Contents), constant(PresenceId),
  EventCategory =\= TRANSIENT_EVENT_CATEGORY |
/*	computation#display(term,
	  ["Updating1 ", PresenceId, 
		EventCategory, EventType, Contents], type(ground)),
*/
	get_update_presence_state(Date,EventCategory,EventType, Contents, 
		UpdateCard, TypeOfUpdate),
	Rcv'' = [update_state1(TypeOfUpdate?, UpdateCard?, PresenceId, 
				EventCategory, EventType, Contents)
	| Rcv'?],
	self;

  Rcv ? update_state(true, PresenceId, Date, 
				TRANSIENT_EVENT_CATEGORY, EventType, Contents),
  constant(EventType),
  EventType =\= TRANSIENT_MOVED_EVENT, 
  ground(Contents), constant(PresenceId) |
/*	computation#display(term,
	  ["Updating2 ", PresenceId, 
		TRANSIENT_EVENT_CATEGORY, EventType, Contents], type(ground)),
*/
	get_update_presence_state(Date, TRANSIENT_EVENT_CATEGORY, 
		EventType, Contents, 
		UpdateCard, TypeOfUpdate),
	Rcv'' = [update_state1(TypeOfUpdate?, UpdateCard?, PresenceId, 
				TRANSIENT_EVENT_CATEGORY, EventType, Contents)
	| Rcv'?],
	self;


  Rcv ? update_state1(update, UpdateCard, PresenceId, 
		EventCategory, EventType, Contents), 
  constant(PresenceId) |
	Rcv'' = [([self]:update(id_card(PresenceId,UpdateCard), Ok1)),
		update_state_for_object_if_ok(Ok1?, PresenceId, EventCategory, 
						EventType,Contents) 
	| Rcv'?],
	self;

  Rcv ? update_state1(tail_and_update, UpdateCard, PresenceId, 
  EventCategory, EventType, Contents), constant(PresenceId) |
	Rcv'' = [([self]:tail_and_update(id_card(PresenceId,UpdateCard), Ok1)),
		update_state_for_object_if_ok(Ok1?, PresenceId, EventCategory, 
							EventType,Contents) 
	| Rcv'?],
	self;

  Rcv ? update_state(true, PresenceId, Date, 
		TRANSIENT_EVENT_CATEGORY, TRANSIENT_MOVED_EVENT, Contents),
  Contents = {_, Position}, % check connection
  ground(Contents), constant(PresenceId) | 
/*
	computation#display(term,
	  ["Updating4 ", PresenceId, 
		TRANSIENT_EVENT_CATEGORY, TRANSIENT_MOVED_EVENT,
		Contents], type(ground)),
*/
	Rcv'' = [([self] : show(id_card(PresenceId), IdCard, Ok)),
		update_connection(PresenceId, Position, Conn?, Date),
		move_passengers(PresenceId, Objects?)
	| Rcv'?],
	id_card#query(IdCard?, [(PRESENCE_CONNECTION_ATTR : Conn),
				(PRESENCE_OBJECTS_ATTR : Objects)]),
	ok(Ok?, update_state_transient_moved),
	self;

  Rcv ? move_passengers(_, []) |
	self;

  Rcv ? move_passengers(_, undefined) |
	self;

% get passengers from the Doors (and not from driver) to ensure correct order
  Rcv ? move_passengers(PresenceId, Objects), Objects = [Object],
  Object = [(PRESENCE_OBJECT_OBJECT_ID_ATTR : ObjectId) | _],
  constant(PresenceId), constant(ObjectId) |
	Rcv'' = [([self] : show(doors(
		[(PRESENCE_CONNECTION_ATTR :
		 {PresenceId, ObjectId, DOORS_CONNECTION_TO_OBJECT})]),P, Ok1)),
		move_passengers1(P?)
	| Rcv'?],
	ok(Ok1?, move_passengers),
	self;

  Rcv ? move_passengers1([]) |
	self;

  Rcv ? move_passengers1(Passengers), Passengers ? Passenger - _, 
  constant(Passenger) |
	Rcv'' = [([self]:tail_and_update(id_card(Passenger, []), Ok1)),
		move_passengers1(Passengers'?)
	| Rcv'?],
%	computation#display(term, Passenger, type(ground)),
	ok(Ok1?, move_passengers1),
	self;
	
  Rcv ? update_connection(PresenceId, Position, Conn, Date),
  ground(Date), 
  Conn = {OldConnectionPresenceId, _OldConnectionObjectId, 
				DOORS_CONNECTION_TO_PRESENCE},
  ground(PersonEventsOperations), ground(Date),
  constant(PresenceId), constant(OldConnectionPresenceId), ground(Position) |
	Rcv'' = [([self]:tail_and_update(id_card(PresenceId,
		  [(PRESENCE_POSITION_ATTR : Position),
		  (PRESENCE_CONNECTION_ATTR : [])]), Ok1)),
		([self] :  update(id_card(OldConnectionPresenceId,
		   [(PRESENCE_CONNECTION_ATTR : [])]), Ok2)),
		compute_recipients(PLACE_EVENT_CATEGORY, DOORS_PLACE, _, R),
		do_cache_event(PresenceId, Date, 
			PLACE_EVENT_CATEGORY,
			PLACE_DISCONNECTED_PRESENCE_EVENT, DOORS_PLACE,
			OldConnectionPresenceId,
                        R1?),
		broadcast_doorsEvent(true, PresenceId, Date,
			PLACE_EVENT_CATEGORY,
			PLACE_DISCONNECTED_PRESENCE_EVENT, DOORS_PLACE,
			OldConnectionPresenceId,
                        R2?)
	| Rcv'?],
	listen2(R?, R1, R2),
%	computation#display(term, [position, Position], type(ground)),
	ok([Ok1?, Ok2?], moved_away_from_presence),
	self;


  Rcv ? update_connection(PresenceId, Position, Conn, Date),
  Conn = {OldConnectionPresenceId, OldConnectionObjectId, 
					DOORS_CONNECTION_TO_OBJECT},
  constant(PresenceId), constant(OldConnectionPresenceId), 
  ground(PersonEventsOperations), constant(Date),
  constant(OldConnectionObjectId) |
	Rcv'' = [delete_passenger(OldConnectionPresenceId, 
			OldConnectionObjectId, PresenceId),
		([self]:tail_and_update(id_card(PresenceId,
		  [(PRESENCE_POSITION_ATTR : Position),
		  (PRESENCE_CONNECTION_ATTR : [])]), Ok1)),
		compute_recipients(PLACE_EVENT_CATEGORY, DOORS_PLACE, _, R),
		do_cache_event(PresenceId, Date,
			PLACE_EVENT_CATEGORY,
			PLACE_DISCONNECTED_OBJECT_EVENT, DOORS_PLACE,
			{OldConnectionPresenceId, OldConnectionObjectId},
			R1?),
		broadcast_doorsEvent(true, PresenceId, Date,
			PLACE_EVENT_CATEGORY,
			PLACE_DISCONNECTED_OBJECT_EVENT, DOORS_PLACE,
			{OldConnectionPresenceId, OldConnectionObjectId},
			R2?)
	| Rcv'?],
	listen2(R?, R1, R2),
	ok(Ok1?, moved_off_the_bus),
	self;

  Rcv ? update_connection(PresenceId, Position, [], _Date) |
	Rcv'' = [([self]:tail_and_update(id_card(PresenceId,
		[(PRESENCE_POSITION_ATTR : Position)]), Ok1)) 
	| Rcv'?],
	ok(Ok1?, update_position_no_connection),
	self;

  Rcv ? update_state_for_object_if_ok(true, PresenceId, EventCategory, 
						EventType,Contents) |
	Rcv'' = [update_state_for_object(PresenceId, EventCategory, 
							EventType,Contents) 
	| Rcv'?],
	self;

  Rcv ? update_state_for_object_if_ok(Error, _PresenceId, _EventCategory, 
				_EventType, _Contents), Error =\= true |
	self; % presence gone already

  Rcv ? update_state_for_object(_, EventCategory, _EventType, _Contents),
  EventCategory =\= PLACE_EVENT_CATEGORY, 
  EventCategory =\= TRANSIENT_EVENT_CATEGORY |
	self;

  Rcv ? update_state_for_object(_, TRANSIENT_EVENT_CATEGORY, EventType, _),
  EventType =\= TRANSIENT_CONVERSATION_TYPE_EVENT,
  EventType =\= TRANSIENT_MOVED_EVENT |
	self;
				
  Rcv ? update_state_for_object(PresenceId, TRANSIENT_EVENT_CATEGORY, 
		TRANSIENT_CONVERSATION_TYPE_EVENT, Contents),
  ground(PresenceId),
  Contents = {ObjectId, NewConversationType} |
	Rcv'' = [([self] : show(id_card(PresenceId), IdCard, Ok1)),
                 ([self] : update(id_card(PresenceId, IdCard'?),Ok2))
        | Rcv'?],
        listen2(IdCard?, IdCard1, IdCard2),
        id_card#query(IdCard1?, [(PRESENCE_OBJECTS_ATTR : Objects)]),
        id_card#update(IdCard2?, [(PRESENCE_OBJECTS_ATTR : Objects'?)],
                                                                IdCard'),
        update_object(Objects?, ObjectId, Objects', 
		[(PRESENCE_OBJECT_CONV_TYPE_ATTR : NewConversationType)]),
        ok([Ok1?, Ok2?], update_object_conv_type),
        self;

  Rcv ? update_state_for_object(PresenceId, TRANSIENT_EVENT_CATEGORY, 
		TRANSIENT_MOVED_EVENT, Contents),
  ground(PresenceId),
  Contents = {ObjectId, Position} |
	Rcv'' = [([self] : show(id_card(PresenceId), IdCard, Ok1)),
                 ([self] : update(id_card(PresenceId, IdCard'?),Ok2))
        | Rcv'?],
        listen2(IdCard?, IdCard1, IdCard2),
        id_card#query(IdCard1?, [(PRESENCE_OBJECTS_ATTR : Objects)]),
        id_card#update(IdCard2?, [(PRESENCE_OBJECTS_ATTR : Objects'?)],
                                                                IdCard'),
        update_object(Objects?, ObjectId, Objects', 
		[(PRESENCE_POSITION_ATTR : Position)]),
        ok([Ok1?, Ok2?], update_object_position),
        self;

  Rcv ? update_state_for_object(PresenceId, PLACE_EVENT_CATEGORY,
		 PLACE_DELETED_OBJECT_EVENT, Contents) |
  	Rcv'' = [delete_object(PresenceId, Contents) | Rcv'?],
	self;

% for now, assume one bus 
  Rcv ? delete_object(PresenceId, ObjectId),
  constant(PresenceId) |
	Rcv'' = [([self] : show(id_card(PresenceId), IdCard, Ok1)),
		 ([self] : update(id_card(PresenceId, IdCard'?),Ok2)),
		break_connections(Passengers?)
	| Rcv'?],
	listen2(IdCard?, IdCard1, IdCard2),
	id_card#query(IdCard1?, [(PRESENCE_OBJECTS_ATTR : Objects)]),
	id_card#update(IdCard2?, [(PRESENCE_OBJECTS_ATTR : Objects'?)], 
								IdCard'),
	delete_object(Objects?, ObjectId, Objects', Passengers),
	ok([Ok1?, Ok2?], delete_object),
	self;

  Rcv ? update_state_for_object(_FullName, PLACE_EVENT_CATEGORY,
		 Other, _Contents),
  Other =\= PLACE_DELETED_OBJECT_EVENT |
	self;

  Rcv ? update_object_by_name(PresenceId, ObjectName, UpdateCard),
  constant(PresenceId) |
	Rcv'' = [([self] : show(id_card(PresenceId), IdCard, Ok1)),
		 ([self] : update(id_card(PresenceId, IdCard'?),Ok2))
	| Rcv'?],
	listen2(IdCard?, IdCard1, IdCard2),
	id_card#query(IdCard1?, [(PRESENCE_OBJECTS_ATTR : Objects)]),
	id_card#update(IdCard2?, [(PRESENCE_OBJECTS_ATTR : Objects'?)],
								IdCard'),
	update_object_by_name(Objects?, ObjectName, UpdateCard, Objects'),
	ok([Ok1?, Ok2?], update_object_by_name),
	self;

% For now, only bus objects, so always do tail_and_update.
% May change once we have other objects 
  Rcv ? add_object(PresenceId, ObjectCard),
  constant(PresenceId), ground(ObjectCard) |
	Rcv'' = [([self] : show(id_card(PresenceId), IdCard, Ok1)),
		 ([self] : tail_and_update(id_card(PresenceId, IdCard'?),Ok2))
	| Rcv'?],
	listen2(IdCard?, IdCard1, IdCard2),
	id_card#query(IdCard1?, [(PRESENCE_OBJECTS_ATTR : Objects)]),
	id_card#update(IdCard2?, [(PRESENCE_OBJECTS_ATTR : Objects'?)], 
								IdCard'),
	add_object(Objects?, ObjectCard, Objects'),
	ok([Ok1?, Ok2?], add_object),
	self;

  Rcv ? break_connections(undefined) | % shouldn't happen
	self;

  Rcv ? break_connections([]) |
	self;

  Rcv ? break_connections(Passengers), Passengers ? Passenger |
	Rcv'' = [([self] : update(id_card(Passenger, 
			[(PRESENCE_CONNECTION_ATTR : []), 		
			 (PRESENCE_POSITION_ATTR : [])]), _Ok1)),
		 break_connections(Passengers') 
	| Rcv'?],
	self;

  Rcv ? replace_connection([], _Contents) |
	self;

  Rcv ? replace_connection(PresenceId, Contents), 
  constant(PresenceId), PresenceId =\= [] |
	Rcv'' = [([self] : update(id_card(PresenceId, 
			[(PRESENCE_CONNECTION_ATTR : Contents)]), _Ok1))
	| Rcv'?], 
	self.

connections_requests(Rcv, PersonEventsOperations, NoStateEventsOperations) :-
+to_presence;
+to_object.

to_presence(Rcv, PersonEventsOperations) :-
  Rcv ? (_ : connect_to_presence(PresenceId,  
                    ConnecteeId, ConnecteePosition, NewPosition)),
  constant(PresenceId), constant(ConnecteeId),
  ground(ConnecteePosition), ground(NewPosition) |
%	computation#display(term, [PresenceId, 
%                    ConnecteeId, ConnecteePosition, NewPosition],
%			type(ground)),
	Rcv'' = [([self] : show(id_card(ConnecteeId), IdCard1, Ok1)),
		 ([self] : show(id_card(PresenceId), IdCard2, Ok2)),
		connect_to_presence1(Ok3?, ConnecteePosition1?,
		ConnecteeConnection?, ConnecteeObjects?,
		PresenceId, ConnecteeId, 
		ConnecteePosition, NewPosition, CurrConn?, Objects?)
	| Rcv'?],
	id_card#query(IdCard1?, [(PRESENCE_POSITION_ATTR : ConnecteePosition1),
			(PRESENCE_CONNECTION_ATTR : ConnecteeConnection),
			(PRESENCE_OBJECTS_ATTR : ConnecteeObjects)]),
	id_card#query(IdCard2?, [
			(PRESENCE_CONNECTION_ATTR : CurrConn),
			(PRESENCE_OBJECTS_ATTR : Objects)]),
	and(Ok1?, Ok2?, Ok3),
	self;

  Rcv ? connect_to_presence1(Error, _ConnecteePosition1, _ConnecteeConnection,
		_ConnecteeObjects,
		PresenceId, ConnecteeId, 
		_ConnecteePosition, _NewPosition, _CurrConn, _),
  Error =\= true, constant(PresenceId) |
	Rcv'' = [failed_to_connect_to_presence(PresenceId, 
		[PresenceId], 
		DOORS_CONNECT_FAILED_NO_CONNECTEE, ConnecteeId) 
	| Rcv'?],
	self;

  Rcv ? connect_to_presence1(true, _ConnecteePosition1, ConnecteeConnection,
		_ConnecteeObjects,
		PresenceId,  ConnecteeId, 
		_ConnecteePosition, _NewPosition, _CurrConn, []),
  constant(PresenceId),  constant(ConnecteeId),
  ConnecteeConnection =\= {PresenceId, _, DOORS_CONNECTION_TO_PRESENCE},
  ConnecteeConnection =\= [] |
	presence_connection_list(PresenceId, ConnecteeId, List),
	Rcv'' = [failed_to_connect_to_presence(PresenceId, List?,
				DOORS_CONNECT_FAILED_BUSY, ConnecteeId) 
	| Rcv'?],
	self;

  Rcv ? connect_to_presence1(true, _ConnecteePosition1, ConnecteeConnection,
		_ConnecteeObjects,
		PresenceId,  _ConnecteeId, 
		_ConnecteePosition, _NewPosition, _CurrConn, []),
  constant(PresenceId), 
  ConnecteeConnection = {PresenceId, _, DOORS_CONNECTION_TO_PRESENCE} |
	% impatient user - already connected so ignore
	self;

  Rcv ? connect_to_presence1(true, _ConnecteePosition1, _ConnecteeConnection,
		ConnecteeObjects,
		PresenceId,  ConnecteeId, 
		_ConnecteePosition, _NewPosition, _CurrConn, []),
  constant(PresenceId),  constant(ConnecteeId),
  ConnecteeObjects =\= [] |
	presence_connection_list(PresenceId, ConnecteeId, List),
	Rcv'' = [failed_to_connect_to_presence(PresenceId, List?,
				DOORS_CONNECT_FAILED_BUSY, ConnecteeId) 
	| Rcv'?],
	self;

 Rcv ? connect_to_presence1(true, _ConnecteePosition1, _ConnecteeConnection,
		_ConnecteeObjects,
		PresenceId,  ConnecteeId, 
		_ConnecteePosition, _NewPosition, _CurrConn, Objects),
  constant(PresenceId),  constant(ConnecteeId),
  Objects =\= [] |
	presence_connection_list(PresenceId, ConnecteeId, List),
	Rcv'' = [failed_to_connect_to_presence(PresenceId, List?,
				DOORS_CONNECT_FAILED_BUSY, ConnecteeId) 
	| Rcv'?],
	self;

  Rcv ? connect_to_presence1(true, _ConnecteePosition1, ConnecteeConnection,
		_ConnecteeObjects,
		PresenceId,  ConnecteeId, 
		_ConnecteePosition, _NewPosition, _CurrConn, _Objects),
  constant(PresenceId),  constant(ConnecteeId),
  ConnecteeConnection = {_, _, DOORS_CONNECTION_TO_OBJECT} | %  a passenger
	presence_connection_list(PresenceId, ConnecteeId, List),
	Rcv'' = [failed_to_connect_to_presence(PresenceId, List?,
				DOORS_CONNECT_FAILED_BUSY, ConnecteeId) 
	| Rcv'?],
	self;

  Rcv ? connect_to_presence1(true, ConnecteePosition1, _ConnecteeConnection,
		_ConnecteeObjects,
		PresenceId, ConnecteeId, 
		ConnecteePosition, _NewPosition, _CurrConn, []),
  constant(PresenceId),  constant(ConnecteeId),
  ConnecteePosition1 =\= ConnecteePosition |
	presence_connection_list(PresenceId, ConnecteeId, List),
	Rcv'' = [failed_to_connect_to_presence(PresenceId, List?,
				DOORS_CONNECT_FAILED_MISMATCH, ConnecteeId) 
	| Rcv'?],
	self;

  Rcv ? connect_to_presence1(true, ConnecteePosition1, [], [],
		PresenceId, ConnecteeId, 
		ConnecteePosition, NewPosition, CurrConn, []),
  ConnecteePosition1 = ConnecteePosition,
  ground(PersonEventsOperations),
  constant(PresenceId), 
  ground(NewPosition),
  CurrConn = {_, _, DOORS_CONNECTION_TO_PRESENCE} |
	processor#interface(gmtime(Date)),
	Rcv'' = [update_connection(PresenceId, NewPosition, CurrConn, Date?),
		connect_to_new_presence(PresenceId, ConnecteeId,
						NewPosition)
	| Rcv'?],
	self;

  Rcv ? connect_to_presence1(true, ConnecteePosition1, [], [],
		PresenceId, ConnecteeId, 
		ConnecteePosition, NewPosition, CurrConn, []),
  ConnecteePosition1 = ConnecteePosition,
  ground(PersonEventsOperations),
  constant(PresenceId), ground(NewPosition),
  CurrConn = {_, _, DOORS_CONNECTION_TO_OBJECT} |
	processor#interface(gmtime(Date)),
	Rcv'' = [update_connection(PresenceId, NewPosition, CurrConn, Date?),
		connect_to_new_presence(PresenceId, ConnecteeId,
						NewPosition)
	| Rcv'?],
	self;

  Rcv ? connect_to_presence1(true, ConnecteePosition1, [], [],
		PresenceId, ConnecteeId, 
		ConnecteePosition, NewPosition, [], []),
  ConnecteePosition1 = ConnecteePosition |
	Rcv'' = [connect_to_new_presence(PresenceId, ConnecteeId,
						NewPosition)
	| Rcv'?],
	self;

  Rcv ? connect_to_new_presence(PresenceId, ConnecteeId,
						NewPosition),
  constant(PresenceId), constant(ConnecteeId),
  ground(NewPosition) |
	Rcv'' = [([self] : tail_and_update(id_card(PresenceId,
		    [(PRESENCE_CONNECTION_ATTR : 
			{ConnecteeId, [], DOORS_CONNECTION_TO_PRESENCE}),
		     (PRESENCE_POSITION_ATTR : NewPosition)]), Ok1)),
		 ([self] :  update(id_card(ConnecteeId,
		    [(PRESENCE_CONNECTION_ATTR : 
		      {PresenceId, [], DOORS_CONNECTION_TO_PRESENCE})]), Ok2)),
		connect_to_presence2(Ok3?, PresenceId, 
						 ConnecteeId, NewPosition)
	| Rcv'?],
	and([Ok1?, Ok2?], Ok3),
	self;

  Rcv ? connect_to_presence2(Error, PresenceId,  ConnecteeId, _NewPosition),
  constant(PresenceId),  constant(ConnecteeId),
  Error =\= true |
	presence_connection_list(PresenceId, ConnecteeId, List),
	Rcv'' = [failed_to_connect_to_presence(PresenceId, List?, 
			DOORS_CONNECT_FAILED_SERVER_ERROR, ConnecteeId) 
	| Rcv'?],
	ok(Error, connect_to_presence_failed),
	self;

  Rcv ? failed_to_connect_to_presence(PresenceId, 
				Recipients, ErrorCode, ConnecteeId),
  ground(Recipients), constant(ErrorCode), constant(PresenceId), 
  constant(ConnecteeId) |
	processor#interface(gmtime(GmtDate)),
	listen2(GmtDate?, D1, D2),
	Rcv'' = [do_cache_event(PresenceId, D1?,
			PLACE_EVENT_CATEGORY,
			PLACE_FAILED_TO_CONNECT_TO_PRESENCE_EVENT, 
			DOORS_LIST,
			{ErrorCode, ConnecteeId}, Recipients),
		broadcast_doorsEvent(true, PresenceId, D2?,
			PLACE_EVENT_CATEGORY,
			PLACE_FAILED_TO_CONNECT_TO_PRESENCE_EVENT, 
			DOORS_LIST,
			{ErrorCode, ConnecteeId}, Recipients)
	| Rcv'?],
	self;
  
  Rcv ? connect_to_presence2(true, PresenceId, ConnecteeId, NewPosition),
  constant(PresenceId), constant(ConnecteeId), ground(PersonEventsOperations) |
	Rcv'' = [([self] : got_doorsEvent(PresenceId, PersonEventsOperations,
		DOORS_PLACE, PLACE_EVENT_CATEGORY, 
		PLACE_CONNECTED_PRESENCE_EVENT,
		{ConnecteeId, NewPosition}))
	| Rcv'?],
	self.
		
to_object(Rcv, PersonEventsOperations, NoStateEventsOperations) :-
  Rcv ? (_ : connect_to_object(PresenceId,  
                    ConnecteePresenceId, ConnecteeObjectId,
                    ConnecteePosition, NewPosition)),
  constant(PresenceId), constant(ConnecteePresenceId), 
  constant(ConnecteeObjectId), ground(ConnecteePosition), ground(NewPosition) |
%	computation#display(term, [PresenceId, 
%                    ConnecteePresenceId, ConnecteeObjectId,
%                    ConnecteePosition, NewPosition], type(ground)),
        id_card#query(IdCard0?, [(PRESENCE_OBJECTS_ATTR : ConnecteeObjects),
				 (PRESENCE_POSITION_ATTR : 	
						ConnecteePosition1)]),
	id_card#query(IdCard1?, [(PRESENCE_CONNECTION_ATTR : CurrConn),
				(PRESENCE_OBJECTS_ATTR : CurrObjects),
				(PRESENCE_POSITION_ATTR : OldPosition)]),
	listen2(ConnecteeObjects?, O1, O2),
	Rcv'' = [([self] : show(id_card(ConnecteePresenceId), IdCard0, Ok0)),
		 ([self] : show(id_card(PresenceId), IdCard1, Ok1)),
                connect_to_object1(Ok3?, ConnecteePosition1?,
                MaxCapacity?, NumberUsed?, Passengers?, 
		UsedSlots?, O2?, CurrConn?,
		CurrObjects?,
                PresenceId, ConnecteePresenceId, ConnecteeObjectId,
                ConnecteePosition, NewPosition, OldPosition?)
        | Rcv'?],
        query_object_ok(O1?, ConnecteeObjectId, 
		[(PRESENCE_OBJECT_MAX_CAPACITY_ATTR : MaxCapacity),
		 (PRESENCE_OBJECT_NUMBER_USED_ATTR : NumberUsed),
		 (PRESENCE_OBJECT_PASSENGERS_ATTR : Passengers),
		 (PRESENCE_OBJECT_USED_SLOTS_ATTR : UsedSlots)], Ok2),
	and([Ok0?, Ok1?, Ok2?], Ok3),
	self;

  Rcv ? connect_to_object1(Error, _ConnecteePosition1,
                _MaxCapacity, _NumberUsed, _Passengers,
		_UsedSlots, _Objects, _CurrConn,
		_CurrObjects,
                PresenceId, ConnecteePresenceId, ConnecteeObjectId,
                _ConnecteePosition, _NewPosition, _OldPosition),
  Error =\= true, constant(PresenceId) |
        Rcv'' = [failed_to_connect_to_object(PresenceId, 
                [PresenceId], 
                DOORS_CONNECT_FAILED_NO_CONNECTEE, 
		ConnecteePresenceId, ConnecteeObjectId) 
        | Rcv'?],
        self;

  Rcv ? connect_to_object1(true, _ConnecteePosition1,
                MaxCapacity, NumberUsed, _Passengers, _UsedSlots, _Objects, [],
		[],
                PresenceId, ConnecteePresenceId, ConnecteeObjectId,
                _ConnecteePosition, _NewPosition, _OldPosition),
  NumberUsed >= MaxCapacity, constant(PresenceId) |
	Rcv'' = [failed_to_connect_to_object(PresenceId, 
                [PresenceId], 
                DOORS_CONNECT_FAILED_NO_ROOM, 
		ConnecteePresenceId, ConnecteeObjectId) 
        | Rcv'?],
        self;

  Rcv ? connect_to_object1(true, _ConnecteePosition1,
                MaxCapacity, NumberUsed, _Passengers, _UsedSlots, _Objects, 
		CurrConn, [],
                PresenceId, ConnecteePresenceId, ConnecteeObjectId,
                _ConnecteePosition, _NewPosition, _OldPosition),
  CurrConn = {_, _, DOORS_CONNECTION_TO_PRESENCE},
  NumberUsed >= MaxCapacity, constant(PresenceId) |
	Rcv'' = [failed_to_connect_to_object(PresenceId, 
                [PresenceId], 
                DOORS_CONNECT_FAILED_NO_ROOM, 
		ConnecteePresenceId, ConnecteeObjectId) 
        | Rcv'?],
        self;

  Rcv ? connect_to_object1(true, _ConnecteePosition1,
                MaxCapacity, NumberUsed, _Passengers, _UsedSlots, _Objects, 
		CurrConn, [],
                PresenceId, ConnecteePresenceId, ConnecteeObjectId,
                _ConnecteePosition, _NewPosition, _OldPosition),
  CurrConn = {_, _, DOORS_CONNECTION_TO_OBJECT},
  CurrConn =\= {ConnecteePresenceId, ConnecteeObjectId, _},
  NumberUsed >= MaxCapacity, constant(PresenceId) |
	Rcv'' = [failed_to_connect_to_object(PresenceId, 
                [PresenceId], 
                DOORS_CONNECT_FAILED_NO_ROOM, 
		ConnecteePresenceId, ConnecteeObjectId) 
        | Rcv'?],
        self;

  Rcv ? connect_to_object1(true, _ConnecteePosition1,
                _MaxCapacity, _NumberUsed, _Passengers, 
		_UsedSlots, _Objects, _CurrConn,
		CurrObjects,
                PresenceId, ConnecteePresenceId, ConnecteeObjectId,
                _ConnecteePosition, _NewPosition, _OldPosition),
  CurrObjects =\= [], constant(PresenceId) |
	Rcv'' = [failed_to_connect_to_object(PresenceId, 
                [PresenceId], 
                DOORS_CONNECT_FAILED_BUSY, 
		ConnecteePresenceId, ConnecteeObjectId) 
        | Rcv'?],
        self;

% already on the bus - just need to change position
  Rcv ? connect_to_object1(true, _ConnecteePosition1,
                _MaxCapacity, _NumberUsed, _Passengers, 
		UsedSlots, Objects, CurrConn,
		_CurrObjects,
                PresenceId, ConnecteePresenceId, ConnecteeObjectId,
               _ConnecteePosition, NewPosition, OldPosition),
  constant(PresenceId), ground(NewPosition),
  ground(NoStateEventsOperations),
  CurrConn={ConnecteePresenceId,ConnecteeObjectId,DOORS_CONNECTION_TO_OBJECT} |
	compute_slot(UsedSlots, OldPosition,
			NewPosition, UsedSlots1, NewPosition1),
	update_object(Objects, ConnecteeObjectId, Objects', 
                [(PRESENCE_OBJECT_USED_SLOTS_ATTR : UsedSlots1?)]),
	listen2(NewPosition1?, NewPos1, NewPos2),
        Rcv'' = [([self] : got_doorsEvent(PresenceId, NoStateEventsOperations,
                DOORS_PLACE, TRANSIENT_EVENT_CATEGORY, TRANSIENT_MOVED_EVENT,
		{[], NewPos1?})),
		([self]:tail_and_update(id_card(PresenceId,
                [(PRESENCE_POSITION_ATTR : NewPos2?)]), Ok1)),
		([self] : update(id_card(ConnecteePresenceId,
		[(PRESENCE_OBJECTS_ATTR : Objects'?)]), Ok2))		
        | Rcv'?],
        ok([Ok1?, Ok2?], update_position_on_bus),
        self;

% on different bus - disconnect and join this new bus
  Rcv ? connect_to_object1(true, ConnecteePosition1,
                MaxCapacity, NumberUsed, Passengers, UsedSlots,
		Objects, CurrConn,
		_CurrObjects,
                PresenceId, ConnecteePresenceId, ConnecteeObjectId,
                ConnecteePosition, NewPosition, OldPosition),
  CurrConn = {_, _, DOORS_CONNECTION_TO_OBJECT},
  CurrConn =\= {ConnecteePresenceId, ConnecteeObjectId, _},
  constant(PresenceId), ground(NewPosition),
  ground(PersonEventsOperations),
  ConnecteePosition1 = ConnecteePosition, NumberUsed < MaxCapacity |
 	processor#interface(gmtime(Date)),
	Rcv'' = [update_connection(PresenceId, NewPosition, 
                                                        CurrConn, Date?),
		connect_to_new_object(PresenceId,
				 ConnecteePresenceId, ConnecteeObjectId,
				 NumberUsed, Passengers, UsedSlots,
				 Objects, NewPosition, OldPosition)
	| Rcv'?],
	self;

% connected to a presence - disconnect and join this bus
  Rcv ? connect_to_object1(true, _ConnecteePosition1,
                MaxCapacity, NumberUsed, Passengers, UsedSlots,
		Objects, CurrConn,
		_CurrObjects,
                PresenceId, ConnecteePresenceId, ConnecteeObjectId,
                _ConnecteePosition, NewPosition, OldPosition),
  CurrConn = {_, _, DOORS_CONNECTION_TO_PRESENCE}, 
  constant(PresenceId),
  ground(PersonEventsOperations), ground(NewPosition),
  NumberUsed < MaxCapacity |
	processor#interface(gmtime(Date)),
	Rcv'' = [update_connection(PresenceId, NewPosition, CurrConn, Date?),
		connect_to_new_object(PresenceId,
				 ConnecteePresenceId, ConnecteeObjectId,
				 NumberUsed, Passengers, UsedSlots,
				 Objects, NewPosition, OldPosition)
	| Rcv'?],
	self;

% not a driver, not connected to anyone, there's room - so just hop on the bus
  Rcv ? connect_to_object1(true, _ConnecteePosition1,
                MaxCapacity, NumberUsed, Passengers, UsedSlots, Objects, [],
		[], PresenceId, ConnecteePresenceId, ConnecteeObjectId,
                _ConnecteePosition, NewPosition, OldPosition),
  NumberUsed < MaxCapacity |
	Rcv'' = [connect_to_new_object(PresenceId,
				 ConnecteePresenceId, ConnecteeObjectId,
				 NumberUsed, Passengers, UsedSlots,
				 Objects, NewPosition, OldPosition)
	| Rcv'?],
	self;

  Rcv ? connect_to_new_object(PresenceId,
				ConnecteePresenceId, ConnecteeObjectId,
				NumberUsed, Passengers, UsedSlots,
				Objects, NewPosition, OldPosition),
  constant(PresenceId), constant(ConnecteePresenceId),
  ground(NewPosition), constant(ConnecteeObjectId) |
        Rcv'' = [([self] : tail_and_update(id_card(PresenceId,
                    [(PRESENCE_CONNECTION_ATTR : 
                        {ConnecteePresenceId, ConnecteeObjectId,
			 DOORS_CONNECTION_TO_OBJECT}),
                     (PRESENCE_POSITION_ATTR : NewPos1?)]), Ok1)),
                 ([self] :  update(id_card(ConnecteePresenceId,
			[(PRESENCE_OBJECTS_ATTR : Objects'?)]), Ok2)),
		connect_to_object2(Ok3?, PresenceId, 
                     ConnecteePresenceId, ConnecteeObjectId, NewPos2?)
        | Rcv'?],
	compute_slot(UsedSlots, OldPosition,
                        NewPosition, UsedSlots1, NewPosition1),
	listen2(NewPosition1?, NewPos1, NewPos2),
	sets#add(PresenceId, Passengers, Passengers'),
	NumberUsed++,
	update_object(Objects, ConnecteeObjectId, Objects',
		[(PRESENCE_OBJECT_PASSENGERS_ATTR : Passengers'?),
		 (PRESENCE_OBJECT_NUMBER_USED_ATTR : NumberUsed'?),
		 (PRESENCE_OBJECT_USED_SLOTS_ATTR : UsedSlots1?)]),
        and([Ok1?, Ok2?], Ok3),
        self;

  Rcv ? connect_to_object2(Error, PresenceId, 
                     ConnecteePresenceId, ConnecteeObjectId, _NewPosition),
  constant(PresenceId), 
  Error =\= true |
	Rcv'' = [failed_to_connect_to_object(PresenceId, 
                [PresenceId], 
                DOORS_CONNECT_FAILED_SERVER_ERROR, 
		ConnecteePresenceId, ConnecteeObjectId) 
        | Rcv'?],
        ok(Error, connect_to_object_failed),
        self;

  Rcv ? failed_to_connect_to_object(PresenceId, Recipients, ErrorCode, 
			ConnecteePresenceId, ConnecteeObjectId),
  ground(Recipients), constant(ErrorCode), constant(ConnecteePresenceId),
  constant(PresenceId), constant(ConnecteeObjectId) |
%	computation#display(term, Recipients, type(ground)),
        processor#interface(gmtime(GmtDate)),
	listen2(GmtDate?, D1, D2),
	Rcv'' = [do_cache_event(PresenceId, D1?,
			PLACE_EVENT_CATEGORY,
			PLACE_FAILED_TO_CONNECT_TO_OBJECT_EVENT, 
               		DOORS_LIST,
               	 	{ErrorCode, ConnecteePresenceId, ConnecteeObjectId}, 
			Recipients),
		broadcast_doorsEvent(true, PresenceId, D2?,
                	PLACE_EVENT_CATEGORY, 
			PLACE_FAILED_TO_CONNECT_TO_OBJECT_EVENT, 
               		DOORS_LIST,
                	{ErrorCode, ConnecteePresenceId, ConnecteeObjectId}, 
			Recipients)
        | Rcv'?],
        self;

  Rcv ? connect_to_object2(true, PresenceId, 
                     ConnecteePresenceId, ConnecteeObjectId, NewPosition),
  constant(PresenceId), 
  ground(PersonEventsOperations) |
        Rcv'' = [([self] : got_doorsEvent(PresenceId, PersonEventsOperations,
                DOORS_PLACE, PLACE_EVENT_CATEGORY, PLACE_CONNECTED_OBJECT_EVENT,
		{ConnecteePresenceId, ConnecteeObjectId, NewPosition}))
        | Rcv'?],
        self.

compute_slot(UsedSlots, OldPosition,
                        NewPosition, UsedSlots1, NewPosition1) :-

  ground(NewPosition), 
  NewPosition = {PositionType, DOORS_ANCHOR_PRESENCE, Door, Anchor},
  Anchor = {PresenceId, ObjectId, PropOffset, AbsOffset},
  PropOffset = {X, Y, Z} |
	NewPropOffset = {X, Y, GrantedSlot?},
	NewAnchor = {PresenceId, ObjectId, NewPropOffset?, AbsOffset},
	NewPosition1 = {PositionType, DOORS_ANCHOR_PRESENCE, Door, NewAnchor?},
	convert_to_integer(Z, RequestedSlot),
	extract_old_slot(OldPosition, OldSlot),
	compute_slot1(UsedSlots, OldSlot?, RequestedSlot?, GrantedSlot,
			UsedSlots1);

  otherwise |
	OldPosition = _,
	UsedSlots1 = UsedSlots,
	NewPosition1 = NewPosition.

extract_old_slot(OldPosition, OldSlot) :-
  OldPosition = {_PositionType, DOORS_ANCHOR_PRESENCE, _Door, Anchor},
  Anchor = {_PresenceId, _ObjectId, PropOffset, _AbsOffset},
  PropOffset = {_X, _Y, Z} |
	OldSlot = Z;

  otherwise |
	OldPosition = _,
	OldSlot = 0.

compute_slot1(UsedSlots, OldSlot, RequestedSlot, GrantedSlot, UsedSlots1) :-
  ground(UsedSlots), integer(RequestedSlot), OldSlot =< 0 |
	sets#element(RequestedSlot, UsedSlots, Taken),
	grant_slot(Taken?, RequestedSlot, UsedSlots, GrantedSlot, UsedSlots1);

  ground(UsedSlots), integer(RequestedSlot), OldSlot > 0 |
	sets#element(RequestedSlot, UsedSlots, Taken),
	replace_slot(Taken?, OldSlot, RequestedSlot, UsedSlots,
					 GrantedSlot, UsedSlots1).	
	
grant_slot(Taken, RequestedSlot, UsedSlots, GrantedSlot, UsedSlots1) :-
  Taken = true, ground(UsedSlots) |
	RequestedSlot = _,
	new_slot(UsedSlots, NewSlot),
	listen2(NewSlot?, NewSlot1, NewSlot2),
	GrantedSlot = NewSlot1?,
	sets#add(NewSlot2?, UsedSlots, UsedSlots1);

  Taken = false, integer(RequestedSlot) |
	GrantedSlot = RequestedSlot,
	sets#add(RequestedSlot, UsedSlots, UsedSlots1).

replace_slot(Taken, OldSlot, RequestedSlot, UsedSlots, GrantedSlot, UsedSlots1) :-
  Taken = true, OldSlot = RequestedSlot | % keep old slot
	UsedSlots1 = UsedSlots,
	GrantedSlot = RequestedSlot;

  Taken = true, OldSlot =\= RequestedSlot, ground(UsedSlots) |
	new_slot(UsedSlots, NewSlot),
	listen2(NewSlot?, NewSlot1, NewSlot2),
	GrantedSlot = NewSlot1?,
	sets#delete(OldSlot, UsedSlots, UsedSlots'),
	sets#add(NewSlot2?, UsedSlots'?, UsedSlots1);

  Taken = false, ground(UsedSlots), integer(RequestedSlot) |
	GrantedSlot = RequestedSlot,
	sets#delete(OldSlot, UsedSlots, UsedSlots'),
	sets#add(RequestedSlot, UsedSlots'?, UsedSlots1).
  
new_slot(UsedSlots, NewSlot) + (ProposedSlot = 1) :-
  ground(UsedSlots), integer(ProposedSlot) |
	sets#element(ProposedSlot, UsedSlots, Taken),
	new_slot1(Taken?, UsedSlots, ProposedSlot, NewSlot).

new_slot1(Taken, UsedSlots, ProposedSlot, NewSlot) :-
  integer(ProposedSlot), Taken = false |
	UsedSlots = _,
	NewSlot = ProposedSlot;

  integer(ProposedSlot), Taken = true |
	ProposedSlot++,
	new_slot(UsedSlots, NewSlot, ProposedSlot'?).

	
create_object(Rcv, NoStateEventsOperations) :-
  Rcv ? (_ : create_object(PresenceId, ObjectId, ObjectState)),
  constant(PresenceId), constant(ObjectId), ground(ObjectState),
  ground(NoStateEventsOperations) |
%	computation#display("got create object"),
        id_card#query(IdCard?, [(PRESENCE_CONNECTION_ATTR : CurrConn),
                                (PRESENCE_OBJECTS_ATTR : Objects)]),
        Rcv'' = [([self] : show(id_card(PresenceId), IdCard, Ok1)),
                create_object1(CurrConn?, Objects?, PresenceId, ObjectId,
								ObjectState)
        | Rcv'?],
        ok(Ok1?, create_object),
        self;

% can't create a tour if already have a connection (passenger or to a presence)

  Rcv ? create_object1(CurrConn, _Objects, PresenceId,
			ObjectId, _ObjectState),
  CurrConn = {_, _, DOORS_CONNECTION_TO_OBJECT} |
%	computation#display("in create_object1 2"),
        Rcv'' = [failed_to_create_object(PresenceId, 
                	DOORS_CREATE_OBJECT_FAILED_ALREADY_PASSENGER, ObjectId) 
        | Rcv'?],
        self;

  Rcv ? create_object1(_CurrConn, Objects, PresenceId,
			ObjectId, _ObjectState),
  Objects =\= [] |
%	computation#display("in create_object1 3"),
        Rcv'' = [failed_to_create_object(PresenceId, 
                	DOORS_CREATE_OBJECT_FAILED_ALREADY_DRIVER, ObjectId) 
        | Rcv'?],
        self;

  Rcv ? failed_to_create_object(PresenceId, ErrorCode, ObjectId),
  constant(PresenceId), constant(ErrorCode), constant(ObjectId) |
        processor#interface(gmtime(GmtDate)),
	listen2(GmtDate?, D1, D2),
%	computation#display("in create_object1 4"),
        Rcv'' = [do_cache_event(PresenceId, D1?,
			PLACE_EVENT_CATEGORY,
			PLACE_FAILED_TO_CREATE_OBJECT_EVENT, 
                	DOORS_LIST,
               		{ErrorCode, ObjectId}, [PresenceId]),
		broadcast_doorsEvent(true, PresenceId, D2?,
                	PLACE_EVENT_CATEGORY, 
			PLACE_FAILED_TO_CREATE_OBJECT_EVENT, 
                	DOORS_LIST,
                	{ErrorCode, ObjectId}, [PresenceId])
        | Rcv'?],
        self;

% already in 1-1 : disconnect from current presence and create the object		
  Rcv ? create_object1(CurrConn, [], PresenceId, ObjectId, ObjectState),
  CurrConn = {OldConnectionPresenceId, _, DOORS_CONNECTION_TO_PRESENCE},
  constant(PresenceId), constant(OldConnectionPresenceId) |
%	computation#display("in create_object1 1"),
	processor#interface(gmtime(GmtDate)),
        Rcv'' = [([self]:update(id_card(PresenceId,
                  [(PRESENCE_CONNECTION_ATTR : [])]), Ok1)),
		([self] :  update(id_card(OldConnectionPresenceId,
                   [(PRESENCE_CONNECTION_ATTR : [])]), Ok2)),
                compute_recipients(PLACE_EVENT_CATEGORY, DOORS_PLACE, _, R),
		do_cache_event(PresenceId, D1?,
			PLACE_EVENT_CATEGORY,
			PLACE_DISCONNECTED_PRESENCE_EVENT, DOORS_PLACE,
                        OldConnectionPresenceId,
                        R1?),
                broadcast_doorsEvent(true, PresenceId, D2?,
                        PLACE_EVENT_CATEGORY,
                        PLACE_DISCONNECTED_PRESENCE_EVENT, DOORS_PLACE,
                        OldConnectionPresenceId,
                        R2?),
		create_object1([], [], PresenceId, ObjectId, ObjectState)
        | Rcv'?],
	listen2(R?, R1, R2),
	listen2(GmtDate?, D1, D2),
	ok([Ok1?, Ok2?], moved_away_from_presence_to_create_tour),
        self;

%not a driver or a passenger so ok
  Rcv ? create_object1([], [], PresenceId, ObjectId, ObjectState),
  ground(ObjectState), constant(ObjectId), constant(PresenceId),
  ground(NoStateEventsOperations) |
%	computation#display("in create_object1 - trying to create the bus"),
        server_api#object_state_to_idcard(ObjectId, ObjectState, ObjectCard),
        strings#extract(HASH, PresenceId, FullName, _), 
        listen2(ObjectCard?, O1, O2),
        id_card#query(O1?, [(PRESENCE_OBJECT_STATE_NAME_ATTR : ObjectName),
                        (PRESENCE_OBJECT_STATE_TIMESTAMP_ATTR : TimeStamp)]),
        Rcv'' = [message(PEOPLE_ROOM, [], 
                    object(PresenceId, FullName?, ObjectName?,TimeStamp?), Ok1),
                    add_object(PresenceId, O2?),
		    ([self] : got_doorsEvent(PresenceId,NoStateEventsOperations,
                        DOORS_PLACE, PLACE_EVENT_CATEGORY,
                        PLACE_CREATED_OBJECT_EVENT, {ObjectId, ObjectState}))
        | Rcv'?],
        ok(Ok1?, create_object1_ok),
        self.

/*****************************************************************************
 * SUPPORT ROUTINES
******************************************************************************/
get_update_presence_state(Date, EventCategory, EventType, Contents, UpdateCard, T) :-

  EventCategory = PRESENCE_EVENT_CATEGORY, EventType = PRESENCE_ENTERED_EVENT,
  Contents = {_PlaceState, PresenceState, _OldDoor} |
	Date = _,
	T = update, % already put into the bottom of the list
	server_api#presence_state_tuple_to_idcard(PresenceState, UpdateCard);

  EventCategory = PLACE_EVENT_CATEGORY, EventType = PLACE_EDIT_STARTED_EVENT |
	Contents = _,
	T = update,
	UpdateCard = [(PRESENCE_EDIT_ATTR : Date)];

  EventCategory = PLACE_EVENT_CATEGORY, EventType = PLACE_EDIT_FINISHED_EVENT |
	Contents = _, Date = _,
	T = update,
	UpdateCard = [(PRESENCE_EDIT_ATTR : NO_DATE)];

  EventCategory = PLACE_EVENT_CATEGORY, EventType = PLACE_EDIT_FAILED_EVENT |
	Contents = _, Date = _,
	T = update,
	UpdateCard = [(PRESENCE_EDIT_ATTR : NO_DATE)];

  EventCategory = PLACE_EVENT_CATEGORY,
  EventType = PLACE_CREATED_OBJECT_EVENT |
	Date = _, Contents = _,
	T = tail_and_update,
	UpdateCard = []; % object update handled elsewhere

  EventCategory = ALERT_EVENT_CATEGORY |
	Contents = _, EventType = _,
	T = update,
	UpdateCard = [(PRESENCE_ALERTED_ATTR: Date)];

  EventCategory = TRANSIENT_EVENT_CATEGORY, EventType = TRANSIENT_CLICKED_EVENT,
  Contents = {Position, Door} |
	Date = _,
	T = update,
  	UpdateCard = [(PRESENCE_CLICK_POSITION_ATTR : Position),
			(PRESENCE_CLICK_DOOR_ATTR : Door)];

  EventCategory = TRANSIENT_EVENT_CATEGORY,
  EventType = TRANSIENT_FACE_STATE_EVENT |
	Date = _,
	T = update,
	UpdateCard = [(PRESENCE_USER_STATE_ATTR : Contents)];

  EventCategory = TRANSIENT_EVENT_CATEGORY,
  EventType = TRANSIENT_AUDIO_FOCUS_EVENT |
	Date = _,
	T = update,
	UpdateCard = [(PRESENCE_AUDIO_FOCUS_ATTR : Contents)];

  otherwise | 
	EventCategory = _, EventType = _, Contents = _, Date = _,
	T = update,
%	computation#display(term, [EventCategory, EventType, 
%			Contents, Date], type(ground)),
	UpdateCard = [].

update_object_by_name(ObjectsData, ObjectName, UpdateCard, ObjectsData1) :-
  ObjectsData ? IdCard,
  ground(IdCard), ground(UpdateCard), string(ObjectName) |
        id_card#query(IdCard, [(PRESENCE_OBJECT_STATE_NAME_ATTR: Name)]),
        ObjectsData1 ! UpdatedIdCard?,
        update_object_by_name1(ObjectName, Name?, IdCard, 
                        UpdateCard, UpdatedIdCard),
        self;

  ObjectsData = [] |
        ObjectName = _, UpdateCard = _,
        ObjectsData1 = [];

  ObjectsData = undefined |
	ObjectName = _, UpdateCard = _,
        ObjectsData1 = [].

update_object_by_name1(ObjectName, Name, IdCard, UpdateCard, UpdatedIdCard) :-
  ObjectName = Name |
        id_card#update(IdCard, UpdateCard, UpdatedIdCard);
        
  ObjectName =\= Name |
        UpdatedIdCard = IdCard,
        UpdateCard = _.

% assumes Contents is in correct attribute value/pair format
add_object(Objects, Contents, Objects1) :-
  list(Objects) |
	Objects1 = [Contents | Objects];

  Objects = [] |
	Objects1 = [Contents];

  Objects = undefined |
	Objects1 = [Contents];

  otherwise, ground(Objects) |
	computation#display(term, ("Server software error - add_object"
		: [Objects, Contents]), type(ground)),
	Objects1 = Objects.

delete_object(Objects, Id_To_Delete, Objects1, Passengers) + (P = []) :-
 
  Objects ? Object, Object = [(PRESENCE_OBJECT_OBJECT_ID_ATTR : ObjectId) | _],
  Id_To_Delete = ObjectId |
	P = _,
	id_card#query(Object, [(PRESENCE_OBJECT_PASSENGERS_ATTR : P')]),
	Objects1 = Objects1'?,
	self;
  
  Objects ? Object, Object = [(PRESENCE_OBJECT_OBJECT_ID_ATTR : ObjectId) | _],
  Id_To_Delete =\= ObjectId |
	Objects1 = [Object | Objects1'?],
	self;

  Objects = [] |
	Id_To_Delete = _,
	Passengers = P,
	Objects1 = [];

  Objects = undefined |
	Id_To_Delete = _,
	Passengers = P,
	Objects1 = [];

  otherwise |
	computation#display(term, 
	("Unexpected parameter to delete object ignored " : Objects), 
								type(ground)),
	Id_To_Delete = _,
	Passengers = P,	
	Objects1 = [].	

update_object(Objects, Id_To_Update, Objects1, UpdateCard) :-
 
  Objects ? Object, Object = [(PRESENCE_OBJECT_OBJECT_ID_ATTR : ObjectId) | _],
  Id_To_Update = ObjectId, ground(Object), ground(UpdateCard) |
	id_card#update(Object, UpdateCard, Object'),
	Objects1 = [Object'? | Objects1'?],
	self;
  
  Objects ? Object, Object = [(PRESENCE_OBJECT_OBJECT_ID_ATTR : ObjectId) | _],
  Id_To_Update =\= ObjectId |
	Objects1 = [Object | Objects1'?],
	self;

  Objects = [] |
	Id_To_Update = _, UpdateCard = _,
	Objects1 = [];

  Objects = undefined |
	Id_To_Update = _, UpdateCard = _,
	Objects1 = [];

  otherwise |
	computation#display(term, 
	("Unexpected parameter to update object ignored " : Objects), 
								type(ground)),
	Id_To_Update = _, UpdateCard = _,
	Objects1 = [].	

query_object_ok(Objects, Id_To_Query, QueryCard, Ok) :-
  Objects = [Object] |
	% if there is only one object, Id_To_Query is irrelevant
	Id_To_Query = _,
	id_card#query_ok(Object, QueryCard, Ok);

  Objects ? Object, Object = [(PRESENCE_OBJECT_OBJECT_ID_ATTR : ObjectId) | _],
  Id_To_Query = ObjectId, ground(Object) |
	Objects' = _,
	id_card#query_ok(Object, QueryCard, Ok);
  
  Objects ? Object, Object = [(PRESENCE_OBJECT_OBJECT_ID_ATTR : ObjectId) | _],
  Id_To_Query =\= ObjectId, Id_To_Query =\= [] |
	self;

  Objects = [] |
	Id_To_Query = _,
	id_card#query_ok(Objects, QueryCard, Ok); % To instantiate variables

  Objects = undefined |
	Id_To_Query = _,
	id_card#query_ok([], QueryCard, Ok); % To instantiate variables

  otherwise, ground(Objects) |
	computation#display(term, 
	("Unexpected parameter to query object ignored " : Objects), 
								type(ground)),
	Id_To_Query = _, 
	id_card#query_ok([], QueryCard, Ok).	% to instantiate the variables	

delete_passenger(Objects, Id_To_Update, Objects1, 
			PassengerToDelete, PassengerPosition) :-
 
  Objects ? Object, Object = [(PRESENCE_OBJECT_OBJECT_ID_ATTR : ObjectId) | _],
  Id_To_Update = ObjectId, ground(Object), constant(PassengerToDelete),
  ground(PassengerPosition) |
	id_card#query(Object, [(PRESENCE_OBJECT_PASSENGERS_ATTR : Passengers),
				(PRESENCE_OBJECT_USED_SLOTS_ATTR : UsedSlots),
				(PRESENCE_OBJECT_NUMBER_USED_ATTR : N)]),
	sets#delete(PassengerToDelete, Passengers?, Passengers'),
	delete_slot(PassengerPosition, UsedSlots?, UsedSlots'),
	N' := N? - 1,
	id_card#update(Object, 
			[(PRESENCE_OBJECT_PASSENGERS_ATTR : Passengers'?),
			 (PRESENCE_OBJECT_USED_SLOTS_ATTR : UsedSlots'?),
			 (PRESENCE_OBJECT_NUMBER_USED_ATTR : N'?)],
			Object'),
	Objects1 = [Object'? | Objects1'?],
	self;
  
  Objects ? Object, Object = [(PRESENCE_OBJECT_OBJECT_ID_ATTR : ObjectId) | _],
  Id_To_Update =\= ObjectId |
	Objects1 = [Object | Objects1'?],
	self;

  Objects = [] |
	Id_To_Update = _, PassengerToDelete = _, PassengerPosition = _,
	Objects1 = [];

  Objects = undefined |
	Id_To_Update = _, PassengerToDelete = _, PassengerPosition = _,
	Objects1 = [];

  otherwise |
	computation#display(term, 
	("Unexpected parameter to delete passenger ignored " : Objects), 
								type(ground)),
	Id_To_Update = _, PassengerToDelete = _, PassengerPosition = _,
	Objects1 = [].
	
delete_slot(Position, UsedSlots, UsedSlots1) :-
  Position = {_PositionType, DOORS_ANCHOR_PRESENCE, _Door, Anchor},
  Anchor = {_PresenceId, _ObjectId, PropOffset, _AbsOffset},
  PropOffset = {_X, _Y, Z}, Z =\= 0 |
	sets#delete(Z, UsedSlots, UsedSlots1);

  otherwise |
%	computation#display(term, Position, type(ground)),
	Position = _,
	UsedSlots1 = UsedSlots.
 
get_presences_from_snapshot(Snapshot, Presences) + (PHead, PTail) :-

  initially |
	PHead = PTail? ;

  Snapshot ? PresenceId - _ |
	PTail ! PresenceId,
	self;

  Snapshot = [] |
	PTail = [],
	Presences = PHead? .


add_to_count_presences_from_snapshot(Snapshot, OldCount, NewCount) :-

   Snapshot ? _PresenceId - _ |
	OldCount++,
	self;

  Snapshot = [] |
	NewCount = OldCount.

replace_joined_date(Tuple, JoinedDate, Tuple1) :-
  JoinedDate = undefined |
	processor#gmtime(Date),
	replace_joined_date1(Date?, Tuple, Tuple1);

  JoinedDate = [] |
	processor#gmtime(Date),
	replace_joined_date1(Date?, Tuple, Tuple1);

  string(JoinedDate), JoinedDate =\= undefined |
	replace_joined_date1(JoinedDate, Tuple, Tuple1).

replace_joined_date1(JoinedDate, PresenceTuple, PresenceTuple1) :-
  PresenceTuple = {NickName, Version, Position, ClickPosition, ClickDoor,
                State, EditDate, AlertDate, _JoinedDate, DetailsDate,
                HasIcon, Objects, Connection, AudioPort, AudioFocus} |
	PresenceTuple1 = {NickName, Version, Position, ClickPosition, 
		ClickDoor, State, EditDate, AlertDate, JoinedDate, DetailsDate,
                HasIcon, Objects, Connection, AudioPort, AudioFocus}.

presence_connection_list(PresenceId1, PresenceId2, List) :-
  PresenceId1 = PresenceId2 |
	List = [PresenceId1];

  PresenceId1 =\= PresenceId2 |
	List = [PresenceId1, PresenceId2].


