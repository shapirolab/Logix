/*  $Header:  */

-language([dfcp]).
-mode(interrupt).
-include([string_includes, api_includes, server_includes]).

/***************************************************************************
 *** places_room *** Manages and Dynamically creates Doors places
****************************************************************************
*/
PEOPLE_ROOM => [super, people].
SCI_ROOM => [sci].

create(IdCard,Door,Ok) :-
	+basic_room#create.

room(Rcv, Doors, Name, Type) +
	 (I, PlaceIdleTime, EventsCacheMax, 
	 URLMapping, MaxPresencesPerPlace, RequestedConnectionsPerServer, 
	 MaxClonesToServer, EventsOperations, AllowPlaceRemovals,
	 PresenceIdleTime) :- 
  +room_with_events#room_with_events;
  +initialize_application;
  +api_messages;

  initially |
	AllowPlaceRemovals = true,
	EventsOperations = [].
 

initialize_application(Rcv,PlaceIdleTime,  EventsCacheMax, 
	URLMapping, MaxPresencesPerPlace, RequestedConnectionsPerServer, 
	MaxClonesToServer, PresenceIdleTime, I) :-
  Rcv ? initialize_application(Ok?) |
	PlaceIdleTime = _, EventsCacheMax = _, 
	URLMapping = _, MaxPresencesPerPlace = _,
	RequestedConnectionsPerServer = _, MaxClonesToServer = _,
	PresenceIdleTime = _,
        Rcv'' = [([self] : show(id_card(self), IdCard, Ok1)),
		 ([self] : replace(id_card(self, IdCard'?), Ok3)) | Rcv'?],
	listen2(IdCard?, IdCard1, IdCard2),
        id_card#query_ok(IdCard1?,
	  [(idle_time : PlaceIdleTime'),
          (events_cache_max : EventsCacheMax'),
	  (url_mapping_script : URLMapping'),
	  (max_presences_per_place : MaxPresencesPerPlace'),
	  (requested_connections_per_server : RequestedConnectionsPerServer'),
	  (max_clones_to_server : MaxClonesToServer'),
	  (presence_idle_time : PresenceIdleTime')], Ok2),
	id_card#remove_attributes(IdCard2?, 
		[idle_time, events_cache_max, url_mapping_script,
		 max_presences_per_place, requested_connections_per_server,
		 max_clones_to_server, presence_idle_time], IdCard'),
        and([Ok1?, Ok2?, Ok3?], Ok),
        self;

  Rcv ? (_ : initialize_api(TokenFile, GrantedConnections?, Ok)),
  integer(MaxClonesToServer),  integer(RequestedConnectionsPerServer),
  string(TokenFile) |
	I = _,
	Rcv'' = [message(SCI_ROOM, [], 
			server_api_initialize(I', 
				{TokenFile, 
				MaxClonesToServer, 
				RequestedConnectionsPerServer, 
				GrantedConnections}, Ok), Ok1) 
	| Rcv'?],
	ok(Ok1?, initialize_api),
	self;

  Rcv ? (_From : make_snapshot(server_broadcast, Snapshot?, Ok?)) |
        Snapshot=[],   
        Ok=true,
        self.


api_messages(Rcv, Doors, I, PlaceIdleTime, EventsCacheMax,
	URLMapping, MaxPresencesPerPlace, 
	EventsOperations, AllowPlaceRemovals, PresenceIdleTime) :-
+api_housekeeping;
+api_connect;
+api_disconnect;
+api_events;
+api_people;
+api_presences;
+api_connections;
+api_presence_timeout;

  I ? Other, otherwise |
	computation#display(term, ("Unexpected API message ignored" : Other),
		type(ground)),
	self.

api_presence_timeout(Rcv, I, PresenceIdleTime) +(P):-

  initially |
        stream#hash_table(P?);
 
  finally |
        P = [];

  Rcv ? (_ : dump_timers) |
        P ! entries(Entries),
        dump_timers(Entries?),
        self;

  Rcv ? start_timer(_), PresenceIdleTime = [] |
	self;

  Rcv ? start_timer(PresenceId), 
  constant(PresenceId), ground(PresenceIdleTime), PresenceIdleTime =\= [] |
	timeout(PresenceId,  PresenceIdleTime, TimerInput?, Output),
	P ! lookup(PresenceId, TimerInput, _OldValue, Status),
	P' ! send(PresenceId, start, Ok),
	Rcv'' = [send([self], merge(Output?)),
		 start_timer1(Status?, Ok?) | Rcv'?],
	self;

  Rcv ? start_timer1(new, true) |
	self;

  Rcv ? start_timer1(Status, _), Status =\= new |
	computation#display(
			"Doors Server INTERNAL warning: duplicate presence"),
	self;

  Rcv ? start_timer1(new, Error), Error =\= true |
	ok(Error, start_timer),
	self;

  Rcv ? stop_timer(_), PresenceIdleTime = [] |
	self;

  Rcv ? stop_timer(PresenceId), PresenceIdleTime =\= [] |
	P ! delete(PresenceId, T?, Ok),
	Rcv'' = [stop_timer1(Ok?, T) | Rcv'?],
	self;

  Rcv ? stop_timer1(true, T) |
	T = [],
	self;

  Rcv ? stop_timer1(Error, _), Error =\= true |
	ok(Error, stop_timer1),
	self;

  Rcv ? timeout_presence(PresenceId), constant(PresenceId) |
	Rcv'' = [stop_timer(PresenceId), message([PresenceId], [], 
		timeout_presence(PresenceId, Ok1), Ok2)
	| Rcv'?],
	ok([Ok1?, Ok2?], timeout),
	self;

  I ? action(_), PresenceIdleTime = [] |
	self;	% server not configured to time out presences

  I ? action(PresenceId), PresenceIdleTime =\= [], constant(PresenceId) |
%	computation#display("action sending a reset"),
	P ! send(PresenceId, reset, _Ok), % ok if no record there yet
%	ok(Ok?, action_reset),
	self.


api_housekeeping(Rcv, Doors, I, PlaceIdleTime, EventsCacheMax,
	EventsOperations, AllowPlaceRemovals) :-
/* HOUSEKEEPING */
  Rcv ? (_ :internal_add_event_operations(Additions)), EventsOperations =\= [] |
	update_events_operations(add, Additions, 
				EventsOperations, EventsOperations'),
	self;

  Rcv ? (_:internal_add_event_operations(Additions)), EventsOperations = [] |
	EventsOperations' = Additions,
	self;

  Rcv ? (_:internal_delete_event_operations(Deletions)) |
	update_events_operations(delete, Deletions, 
				EventsOperations, EventsOperations'),
	self.

api_connect(Rcv, Doors, I, PlaceIdleTime, EventsCacheMax,
	URLMapping, MaxPresencesPerPlace,
	EventsOperations, AllowPlaceRemovals, PresenceIdleTime) :-

/* CONNECTION PROCESSING */

/* Connects a PresenceId to a room. If room doesn't exist, creates it */
  I ? DOORS_CONNECT_FUNCTOR(PresenceId, URL, PlaceState, 
				PresenceStateTuple, OldDoor, BusinessCard),
  string(URL), constant(PresenceId), ground(PlaceState), 
  ground(OldDoor), ground(PresenceStateTuple),
  ground(URLMapping) |
	server_api#presence_state_tuple_to_idcard(PresenceStateTuple, 
							PresenceState),
	listen2(PresenceState?, P1, P2),
%	computation#display("Got doorsConnect"),
	strings#extract(HASH, PresenceId, FullName, _),
	Rcv' = [message(PEOPLE_ROOM, [], 
			person(PresenceId, FullName?, HasFace?,
			TimeStamp?, ObjectStamps?, BusinessCard, Ok0), Ok1)
	| Rcv?],
	id_card#query(P1?, [(PRESENCE_HAS_ICON_ATTR : HasFace)]),
	listen2(URL'?, URL1, URL2),
	I'' =	[map_url(URLMapping, URL, URL'),
		 do_doorsConnect(Ok0?, Ok2?, PresenceId, 
			PlaceState, PresenceStateTuple,
			OldDoor, URL1?, Ok3),
		 continue_api_input(Ok3?)
	| I'?],
	extract_timestamps(P2?, TimeStamp, ObjectStamps),
	AllowPlaceRemovals' = false, AllowPlaceRemovals = _,
	ok(Ok1?, "can't reach people room"),
	entries#check_entry(URL2?,Doors,Doors',Ok2),
	self;

  I ? map_url({[], _}, URL, URL') |
	URL' = URL,
	self;

  I ? map_url(URLMappingScript(_), URL, URL'), [] @< URLMappingScript |
	URL' = URL,
	self;

  I ? map_url(URLMappingScript(_), URL, URL'), URLMappingScript @< "" |
	URL' = URL,
	self;

  I ? map_url(URLMappingScript(MappingFile), URL, URL'),
  string(URLMappingScript), string(MappingFile),
  string(URL) |
	utils#append_strings([URLMappingScript, " ", URL, " > ",
			MappingFile], URLMappingCmd),
	processor#interface(unix_call(URLMappingCmd?), Ok),
	I'' = [map_url1(Ok?, MappingFile, URL, URL') | I'?],
	self;

  I ? map_url1(true, MappingFile, URL, URL') |
	file#get_file(MappingFile, DoorsMappedURL, chars, Ok),
%	computation#display(DoorsMappedURL??),
	I'' = [map_url2(Ok?, DoorsMappedURL?, URL, URL') | I'?],
	self;

  I ? map_url1(Error, URL, URL'), Error =\= true |
	ok(Error, map_url1),
	URL' = URL,
	self;

  I ? map_url2(true, DoorsMappedURL, URL, URL'), string(DoorsMappedURL) |
	URL = _,
	URL' = DoorsMappedURL,
	self;

  I ? map_url2(Error, _, URL, URL'), Error =\= true |
	ok(Error, map_url2),
	URL' = URL,
	self;

  I ? continue_api_input(Ok), known(Ok) |
	AllowPlaceRemovals' = true, AllowPlaceRemovals = _,
	self;

  Rcv ? (_ : ok_to_disappear(Ok?)), AllowPlaceRemovals = true |
	Ok = true,
	self;

  Rcv ? (_ : ok_to_disappear(Ok?)), AllowPlaceRemovals =\= true |
	Ok = false(denied),
	self; %  will get another chance to go away after next interval

  I ? do_doorsConnect(Ok0, Error, PresenceId, PlaceState, PresenceStateTuple, 
					OldDoor, URL, Ok), 
  Error =\= true, constant(PresenceId), string(URL) |
%	computation#display("Must Create New Place"),
        I'' =	[create_new_place(URL, Ok1),
		 retry_doorsConnect(Ok0, Ok1?, PresenceId, PlaceState, 
			PresenceStateTuple, OldDoor, URL, Ok)
        | I'?],
        self;

  I ? create_new_place(URL, Ok), string(URL), 
  ground(PlaceIdleTime), integer(EventsCacheMax),
  constant(MaxPresencesPerPlace), 
  ground(EventsOperations) |
	PlaceIdCard = [(name:URL), (type : PLACE_TYPE_STR), 
		(idle_time : PlaceIdleTime),
		(events_cache_max : EventsCacheMax),
                (max_presences_per_place : MaxPresencesPerPlace),
		(person_events_operations : P1?),
		(no_state_events_operations : NoStateEventsOperations?)],
	get_events_operations(PRESENCE_EVENT_CATEGORY, EventsOperations, 
						PersonEventsOperations),
	listen2(PersonEventsOperations?, P1, P2),
	sets#delete(state, P2?, NoStateEventsOperations),			
	copresence_room#create(PlaceIdCard?, Door, Ok1),
	doors#corridor(Door1, Door2),
	doors#corridor(Door3, Door4),
	Rcv' = [([self] : paste(Door?, URL, Ok2)),
		message([URL], [],
			paste(Door1?, sci, Ok3), Ok4),
		message(SCI_ROOM, [],
			paste(Door2?, URL, Ok5), Ok6),
		message([URL], [],
			paste(Door3?, people, Ok7), Ok8),
		message(PEOPLE_ROOM, [],
			paste(Door4?, URL, Ok9), Ok10),
		message([URL], [], initialize_copresence(Ok11), Ok12),
		([self] : update(id_card(URL,
		[(PLACE_MASTER_ATTR : PLACE_MASTER_TRUE)]),Ok13)) | Rcv?],
	sequential_and([Ok1?, Ok2?, Ok4?, Ok3?, Ok6?, Ok5?, Ok8?, 
				Ok7?, Ok10?, Ok9?, Ok12?, Ok11?, Ok13?], Ok),
%	ok(Ok1??, ok1), ok(Ok2??, ok2), ok(Ok3??, ok3), ok(Ok4??, ok4),
%	ok(Ok5??, ok5), ok(Ok6??, ok6), ok(Ok7??, ok7), ok(Ok8??, ok8),
%	ok(Ok9??, ok9), ok(Ok10??, ok10), ok(Ok11??, ok11), ok(Ok12??, ok12),
	self;

  I ? retry_doorsConnect(Ok0, true, PresenceId, PlaceState,
		 PresenceStateTuple, OldDoor,URL, Ok) |
	I'' = [do_doorsConnect(Ok0, true, PresenceId, PlaceState,
		 PresenceStateTuple,  OldDoor, URL, Ok) | I'?],
	self;

  I ? retry_doorsConnect(_, Error, PresenceId, _PlaceState, _PresenceStateTuple,
			_OldDoor, URL, Ok?), 
  Error =\= true, constant(PresenceId) |
	Rcv' = [message(SCI_ROOM, [],
			doorsPropagateError(DOORS_PLACE_CONNECT_ERROR_CATEGORY,
				DOORS_PLACE_CONNECT_SERVER_INTERNAL, 
				ERROR_SEVERITY_ERROR,
				URL, PresenceId, [],
				[PresenceId]), Ok1)
	| Rcv?],
	ok(Error, retry_doorsConnect),
	ok(Ok1?, cannot_create_error_to_sci),
	Ok = true, % Can continue to get input from API stream
	self;
  
  I ? do_doorsConnect(Ok0, true, PresenceId, PlaceState,
			PresenceStateTuple,  OldDoor, URL, Ok),
  constant(PresenceId), string(URL), known(Ok0) |
	doors#corridor(Door1, Door2),
	Rcv' = [([self] : paste(Door1?, PresenceId, Ok2)) | Rcv?],
	I'' = [do_doorsConnect1(Ok2?, Door2?, 
		PresenceId, PlaceState, PresenceStateTuple, 
		OldDoor, URL, Ok) | I'?],
	self;

  I ? do_doorsConnect1(Error, _Door2, PresenceId, _,  _, _, URL, Ok?), 
  Error =\= true, constant(PresenceId), string(URL) |
%	ok(Error, presence_door),
	Rcv' = [message(SCI_ROOM, [],
			doorsPropagateError(DOORS_PLACE_CONNECT_ERROR_CATEGORY,
				DOORS_PLACE_CONNECT_SERVER_INTERNAL,
				ERROR_SEVERITY_ERROR,
				URL, PresenceId, [],
				[PresenceId]), Ok1)
	| Rcv?],
	Ok = true, % Can continue to get input from API stream
	ok(Ok1?, cannot_connect_error_to_sci),
	self;

  I ? do_doorsConnect1(true, Door2, PresenceId, PlaceState, 
		PresenceStateTuple, OldDoor, URL, Ok?), 
  constant(PresenceId), string(URL) |
	Rcv' = [message([URL], [], 
		paste_presence(Door2, PresenceId, Ok1), Ok2) | Rcv?],
	I'' = [do_doorsConnect2(Ok3?, PresenceId,
				PlaceState, PresenceStateTuple, 
				OldDoor, URL, Ok) 
	| I'?],
	sequential_and([Ok2?, Ok1?], Ok3),
	self;


  I ? do_doorsConnect2(Error, PresenceId, _PlaceState, _PresenceStateTuple,
				 _OldDoor, URL, Ok?), 
  Error =\= true, Error =\= false(DOORS_PLACE_CONNECT_PLACE_IS_FULL), 
  constant(PresenceId), string(URL) |
	ok(Error, presence_paste),
	Rcv' = [message(SCI_ROOM, [],
			doorsPropagateError(DOORS_PLACE_CONNECT_ERROR_CATEGORY,
				DOORS_PLACE_CONNECT_SERVER_INTERNAL,
				ERROR_SEVERITY_ERROR,
				URL, PresenceId, [],
				[PresenceId]), Ok1)
	| Rcv?],
	Ok = true, % Can continue to get input from API stream
	ok(Ok1?, connect_cannot_complete_error_to_sci),
	self;

  I ? do_doorsConnect2(Error, PresenceId, _PlaceState, _PresenceStateTuple,
				 _OldDoor, URL, Ok?), 
  Error = false(DOORS_PLACE_CONNECT_PLACE_IS_FULL), 
  constant(PresenceId), string(URL) |
	Rcv' = [([self] : discard(PresenceId, Ok1)),
		message(SCI_ROOM, [],
			doorsPropagateError(DOORS_PLACE_CONNECT_ERROR_CATEGORY,
				DOORS_PLACE_CONNECT_PLACE_IS_FULL,
				ERROR_SEVERITY_ERROR,
				URL, PresenceId, [],
				[PresenceId]), Ok2)
	| Rcv?],
	Ok = true, % Can continue to get input from API stream
	ok([Ok1?, Ok2?], connect_cannot_complete_error_to_sci_place_is_full),
	self;

  I ? do_doorsConnect2(true, PresenceId, PlaceState, PresenceStateTuple, 
					OldDoor, URL, Ok?),
  constant(PresenceId), string(URL) |
	Rcv' = [message([URL], [], 
		connect_presence(PresenceId, PlaceState, 
		PresenceStateTuple, OldDoor), Ok1),
		start_timer(PresenceId)
	| Rcv?],
	Ok = true,
	ok(Ok1?, url_connect_presence),
	self.

api_disconnect(Rcv, Doors, I, PlaceIdleTime, EventsCacheMax,
	EventsOperations, AllowPlaceRemovals) :-

/* DISCONNECTION PROCESSING */	

  I ? DOORS_DISCONNECT_FUNCTOR(PresenceId, NewDoor),
  constant(PresenceId) |
%	computation#display("Got doorsDisConnect"),
	Rcv' = [message([PresenceId], [], 
		disconnect_presence(PresenceId,  NewDoor, DisconOk), DoorOk),
		stop_timer(PresenceId)
	| Rcv?],
	I'' = [do_doorsDisconnect(DoorOk?, DisconOk?, PresenceId) | I'?],
	self;

  I ? do_doorsDisconnect(true, true, _) |
	self;

  I ? do_doorsDisconnect(Error, _, _PresenceId), 
  Error =\= true |
	% do nothing, this can now happen because server can eject presence
	self;

  I ? do_doorsDisconnect(true, Error, PresenceId),
  Error =\= true, constant(PresenceId) |
	% do nothing, this can now happen because server can eject presence
	self.

api_events(Rcv, Doors, I, PlaceIdleTime, EventsCacheMax,
	EventsOperations, AllowPlaceRemovals) :-

/* EVENT PROCESSING */

  I ? DOORS_VIEW_EVENTS_FUNCTOR(PresenceId, ClientData, 
					GetSnapshot, EventCategories),   
  constant(PresenceId) |
%	computation#display("Got View Events"),
	Rcv' = [message([PresenceId], [], 
		view_events(PresenceId, ClientData, 
				GetSnapshot, EventCategories), Ok)
	| Rcv?],
	I'' = [verify_doorsViewEvents(Ok?, PresenceId) | I'?],
	self;

  I ? verify_doorsViewEvents(true, _) |
	self;

  I ? verify_doorsViewEvents(Error, _PresenceId), 
  Error =\= true |
	% do nothing, this can now happen because server can eject presence
	self;

  I ? DOORS_EVENT_FUNCTOR(PresenceId, To, EventCategory, EventType, Contents),
  constant(PresenceId), constant(EventCategory), ground(EventsOperations),
  To =\= DOORS_SERVER |
%	computation#display("Got doorsEvent"),
	get_events_operations(EventCategory, EventsOperations, Operations),
	Rcv' = [message([PresenceId], [], 
		got_doorsEvent(PresenceId, Operations?, To,
			EventCategory, EventType, Contents), Ok) | Rcv?],
	I'' = [verify_got_doors_event(Ok?, PresenceId) | I'?],
	self;

  I ? verify_got_doors_event(Error, _PresenceId),
  Error =\= true |
	% do nothing, this can now happen because server can eject presence
	self;

  I ? verify_got_doors_event(true, _PresenceId) |
	self;

  I ? DOORS_EVENT_FUNCTOR(PresenceId, DOORS_SERVER, 
				EventCategory, EventType, Contents),
  constant(PresenceId), constant(EventCategory), constant(EventType), 
  ground(Contents), ground(EventsOperations) |
	processor#interface(gmtime(Date)),
	get_events_operations(EventCategory, EventsOperations, Operations),
	% broadcasts message to all copresence rooms
%	computation#display("Got doorsEvent for broadcast"),
	log_if_required(Operations?, PresenceId, Date?,
		EventCategory, EventType, Contents, Operations'),	
	Rcv' = [event(server_broadcast, [PresenceId, Operations'?,DOORS_SERVER, 
					EventCategory, EventType, Contents]) 
	| Rcv?],
	self.

api_people(Rcv, Doors, I, PlaceIdleTime, EventsCacheMax,
	EventsOperations, AllowPlaceRemovals) :-

/* PERSON DATA PROCESSING:  */

  I ? DOORS_QUERY_USER_DETAILS_FUNCTOR(PresenceId, ClientData, RequestType,
			UserObjectNames) |
%	computation#display("Got QueryUserDetails"),
	Rcv' = [message(PEOPLE_ROOM, [],
		 respond_query_user_details(PresenceId, ClientData,
			RequestType, UserObjectNames), Ok1)
	| Rcv?],
	ok(Ok1?, query_user_details),
	self;

% assumes always update everything; assumes date is in IdCard
  I ? DOORS_UPDATE_USER_DETAILS_FUNCTOR(PresenceId, Updates),
  Updates ? IdCard, ground(Updates) |
%	computation#display(term,
%		("Got UpdateUserDetails" : Updates), type(ground)),
	Rcv' = [message(PEOPLE_ROOM, [], 
			update_user_details(IdCard), Ok1) | Rcv?],
	I'' = [doorsUpdateUserDetails(PresenceId, Updates') | I'?],
	ok(Ok1?, update_user_details), % shouldn't happen
	self;

  I ? DOORS_UPDATE_USER_DETAILS_FUNCTOR(_PresenceId, []) |
	self.

api_presences(Rcv, Doors, I, PlaceIdleTime, EventsCacheMax,
	EventsOperations, AllowPlaceRemovals) :-

/* PRESENCE QUERY PROCESSING */
  I ? DOORS_QUERY_CACHE_FUNCTOR(PresenceId, ClientData, EventCategories, 
						Number, From, Until),
  constant(PresenceId), ground(ClientData), constant(From), constant(Until),
  constant(Number) |
%	computation#display("Got QueryCache"),
	Rcv' = [message([PresenceId], [], 
		respond_query_cache(PresenceId, ClientData, EventCategories, 
				From, Until, Number), Ok1),
		report_query_cache_error(Ok1?, PresenceId, ClientData)
	| Rcv?],
	self;

  Rcv ? report_query_cache_error(true, _PresenceId, _ClientData) |
	self;

  Rcv ? report_query_cache_error(Error, _PresenceId, _ClientData), 
  Error =\= true |
	% this can now happen so just ignore
	self;

  I ? DOORS_QUERY_PLACE_PRESENCES_FUNCTOR(PresenceId, ClientData),
  constant(PresenceId), ground(ClientData) | 
%	computation#display("Got QueryPlacePresences"),
	Rcv' = [message([PresenceId], [], 
		respond_query_place_presences(PresenceId, ClientData), Ok1),
		report_query_place_presences_error(Ok1?, PresenceId, ClientData)
	| Rcv?],
	self;

  Rcv ? report_query_place_presences_error(true, _PresenceId, _ClientData) |
	self;

  Rcv ? report_query_place_presences_error(Error, _PresenceId, _ClientData), 
  Error =\= true |
	% do nothing, this can now happen because server can eject presence
	self;

  I ? DOORS_QUERY_SERVER_PRESENCES_COUNT_FUNCTOR(PresenceId, ClientData) |
	Rcv' = [([self]:show(doors([(PLACE_MASTER_ATTR:PLACE_MASTER_TRUE)]), 
								Places, Ok1))
	| Rcv?],
	I'' = [doorsQueryServerPresencesNumber1(Places?, 0,
						 PresenceId, ClientData)
	| I'?],
	ok(Ok1?, doorsQueryServerPresencesNumber),
	self;

  I ? doorsQueryServerPresencesNumber1(Places, OldCount,
						PresenceId, ClientData), 
  Places ? Place, integer(OldCount) |
	Rcv' = [message([Place], [],
		add_to_count_place_presences(OldCount, OldCount1), Ok1)
	| Rcv?],
	I'' = [doorsQueryServerPresencesNumber0(Ok1?, 
		 OldCount, OldCount1?, OldCount'),
		 doorsQueryServerPresencesNumber1(Places', OldCount'?,
						PresenceId, ClientData)
	| I'?],
	self;
  
  I ? doorsQueryServerPresencesNumber0(true, OldCount, OldCount1, OldCount2) |
	OldCount = _,
	OldCount2 = OldCount1,
	self;
  
  I ? doorsQueryServerPresencesNumber0(Error, OldCount, OldCount1, OldCount2),
  Error =\= true |
	OldCount1 = _,
	OldCount2 = OldCount,
	self;
	
  I ? doorsQueryServerPresencesNumber1([], Count, 
						PresenceId, ClientData) |
	Rcv' = [message(SCI_ROOM, [],
		doorsPropagateResponse([], PresenceId, ClientData, 
		SERVER_RESPONSE_CATEGORY, SERVER_PRESENCES_COUNT_RESPONSE,
				Count), Ok1)
	| Rcv?],
	ok(Ok1?, doorsQueryServerPresencesNumber1_end),
	self;
	
  I ? DOORS_QUERY_SERVER_PRESENCES_LIST_FUNCTOR(PresenceId, ClientData),
  constant(PresenceId), ground(ClientData) |
	Rcv' = [([self]:show(doors([(PLACE_MASTER_ATTR:PLACE_MASTER_TRUE)]), 
								Places, Ok1)),
		message(SCI_ROOM, [],
			doorsPropagateResponse([], PresenceId, ClientData, 
			SERVER_RESPONSE_CATEGORY,
			SERVER_PRESENCES_LIST_RESPONSE, Presences?), 
		Ok2)
	| Rcv?],
	I'' = [doorsQueryServerPresencesList1(Places?, Presences)
	| I'?],
	ok([Ok1?, Ok2?], doorsQueryServerPresencesList),
%	computation#display(term, Presences??, type(ground)),
	self;

  I ? doorsQueryServerPresencesList1(Places, Presences),
  Places ? Place - _, constant(Place) |
	Rcv' = [message([Place], [],
		 get_place_presences(P), Ok1)
	| Rcv?],
	I'' = [doorsQueryServerPresencesList0(Ok1?, P?, Place, 
					Presences, Presences'?), 
		doorsQueryServerPresencesList1(Places', Presences')
	| I'?],
	self;

  I ? doorsQueryServerPresencesList0(true, P, Place, Presences, Presences1),
  P =\= [], ground(P) |
%	computation#display(term, P, type(ground)),
	Presences = [{ {[], [], Place}, P} | Presences1],
	self;

  I ? doorsQueryServerPresencesList0(true, [], _Place, Presences, Presences1) |
%	computation#display("no presences"),
	Presences = Presences1,
	self;

  I ? doorsQueryServerPresencesList0(Error, _P, _Place, Presences, Presences1),
  Error =\= true | % room went away already 
%	computation#display("no presences - room went away"),
	Presences = Presences1,
	self;

  I ? doorsQueryServerPresencesList1([], Presences) |
	Presences = [],
	self.

api_connections(Rcv, I) :-

/* REQUESTS AND QUERIES RELATED TO CONNECTIONS (one-to-one and tourist) */
  I ? DOORS_CONNECT_TO_PRESENCE_FUNCTOR(PresenceId, 
		ConnecteeId, ConnecteePosition, NewPosition),
  constant(PresenceId) |
	Rcv' = [message([PresenceId], [], 
                connect_to_presence(PresenceId, 
                    ConnecteeId, ConnecteePosition, NewPosition), Ok1),
                report_connect_to_presence_error(Ok1?, PresenceId)
        | Rcv?],
        self;

  Rcv ? report_connect_to_presence_error(true, _PresenceId) |
        self;

  Rcv ? report_connect_to_presence_error(Error, _PresenceId), 
  Error =\= true |
  	% ignore since this can happen if server ejects
        self;

  I ? DOORS_CONNECT_TO_OBJECT_FUNCTOR(PresenceId,
		ConnecteePresenceId, ConnecteeObjectId,
		ConnecteePosition, NewPosition),
  constant(PresenceId) |
	Rcv' = [message([PresenceId], [], 
                connect_to_object(PresenceId, 
                    ConnecteePresenceId, ConnecteeObjectId,
		    ConnecteePosition, NewPosition), Ok1),
                report_connect_to_object_error(Ok1?, PresenceId)
        | Rcv?],
        self;

  Rcv ? report_connect_to_object_error(true, _PresenceId) |
        self;

  Rcv ? report_connect_to_object_error(Error, _PresenceId), 
  Error =\= true |
   	% ignore since this can happen
        self;	

  I ? DOORS_QUERY_OBJECT_CONNECTIONS_FUNCTOR(PresenceId, ClientData,
	ConnecteePresenceId, ConnecteeObjectId),
    constant(PresenceId), ground(ClientData) | 
%       computation#display("Got QueryObjectConnections"),
        Rcv' = [message([PresenceId], [], 
                respond_query_object_connections(PresenceId, ClientData,
			ConnecteePresenceId, ConnecteeObjectId), Ok1),
		report_query_object_connections_error(Ok1?,
						 PresenceId,ClientData)
	| Rcv?],
        self;

  Rcv ? report_query_object_connections_error(true, _PresenceId, _ClientData) |
        self;

  Rcv ? report_query_object_connections_error(Error, _PresenceId, _ClientData), 
  Error =\= true |
        % ignore since this can now happen if server ejected presence
        self;

  I ? DOORS_CREATE_OBJECT_FUNCTOR(PresenceId, ObjectId, ObjectState),
  constant(PresenceId) |
	Rcv' = [message([PresenceId], [], 
			create_object(PresenceId, ObjectId, ObjectState), Ok1),
		report_connect_to_object_error(Ok1?, PresenceId)
        | Rcv?],
        self.


update_events_operations(Action, Updates, OldTable, NewTable) :-
  Updates ? EventCategory - Operations, string(Action) |
	update_events_operations1(Action, 
		EventCategory, Operations, OldTable, OldTable'),
	self;

  Updates = EventCategory - Operations, string(Action) |
	update_events_operations1(Action, 
		EventCategory, Operations, OldTable, NewTable);

  Updates = [] |
	Action = _,
	NewTable = OldTable.


update_events_operations1(Action, 
		EventCategory, Operations, OldTable, OldTable1) :-
  OldTable ? T - O, T =\= EventCategory |
	OldTable1 ! T - O,
	self;

  OldTable ? EventCategory - O, string(Action), ground(Operations) |
	set_new_operations(Action, O, Operations, NewOperations),
	OldTable1 ! EventCategory - NewOperations?,
	self;

  OldTable = [] |
	Action = _, Operations = _, EventCategory = _,
	OldTable1 = [].

set_new_operations(Action, O, Operations, NewOperations) :-
  Action = add |
	sets#union(O, Operations, NewOperations);

  Action = delete |
	sets#difference(Operations, O, NewOperations).

get_events_operations(EventCategory, EventsOperations, Operations) :-
  EventsOperations ? EventCategory - O |
	Operations = O,
	EventsOperations' = _;

  EventsOperations ? E - _, E =\= EventCategory |
	self;

  EventsOperations = [] |
	EventCategory = _,
	Operations = [].

extract_timestamps(PresenceState,TimeStamp, ObjectStamps):-
  ground(PresenceState) |
	id_card#query(PresenceState, 
	    [(PRESENCE_TIMESTAMP_ATTR : TimeStamp),
			 (PRESENCE_OBJECTS_ATTR : Objects)]),
%	computation#display(term, Objects??, type(ground)),
	extract_object_timestamps(Objects?, ObjectStamps).

extract_object_timestamps(Objects, ObjectStamps) :-
  Objects = [] |
	ObjectStamps = [];

  Objects = undefined |
	ObjectStamps = [];

  Objects ? Object |
	id_card#query(Object, [(PRESENCE_OBJECT_STATE_NAME_ATTR : ObjectName), 
		(PRESENCE_OBJECT_STATE_TIMESTAMP_ATTR : ObjectTimeStamp)]),
	ObjectStamps ! [ObjectName?, ObjectTimeStamp?],
	self.
	


log_if_required(Operations, PresenceId, Date,
		EventCategory, EventType, Contents, Operations1) :-
  ground(Operations) |
	sets#element(log, Operations, Result),
	log_if_required1(Operations, Result?, PresenceId, Date,
		EventCategory, EventType, Contents, Operations1).

log_if_required1(Operations, Result, PresenceId, Date,
		EventCategory, EventType, Contents, Operations1) :-
  Result = true |
	logging_monitor#log_event(server, PresenceId, Date, 
			EventCategory, EventType, DOORS_SERVER, Contents, []),
	sets#delete(log, Operations, Operations1);

  Result =\= true |
	Operations1 = Operations,
	PresenceId = _,
	Date = _, 
	EventCategory = _, 
	EventType = _, 
	Contents = _.

timeout(PresenceId, PresenceIdleTime, I, O) + (Control, Tick = false) :-
	
  I = [] |
	PresenceId = _, PresenceIdleTime = _, O = _, Tick = _,
	Control = [cancel];	% presence left so exit

  I ? start, ground(PresenceIdleTime) |
	Control = _, Tick = _,
	processor # time(ticks(real, PresenceIdleTime, Control'?, Tick')),
	self;

  I ? reset |
%	computation#display("setting reset in timeout"),
	Control ! reset,
	self;

  Tick ? tick, constant(PresenceId) |
	Control = _, I = _, PresenceIdleTime = _, Tick' =_,
	O = [timeout_presence(PresenceId)].


dump_timers(Entries) + (Done = true) :-
  Entries ? entry(Name, Stream) |
        computation#display(term, ("Person " : Name), 
                [close(Done, Done'), type(ground)]),
        computation#display(term, Stream, type(known)),
        self;

  Entries = [] |
        Done = _.


