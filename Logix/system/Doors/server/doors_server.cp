/*  $Header: /home/qiana/Repository/Logix/system/Doors/server/doors_server.cp,v 1.1 1999/07/09 07:03:30 bill Exp $ */

-language([evaluate, dfcp]).
-mode(interrupt).
-scope("Doors").
-export([start, begin, abort, exit]).
-include([api_includes, server_includes]).

VERSION => "0.8.0".

/************************************************************************** 
 *** start ***  Entry point of doors server.
 * Creates the places, people, and sci rooms and sets up corridors
 * among them. ***************************************************************************
*/
start(Ready, Ok) :-
  known(Ready) |
	processor#interface(getenv("DoorsRoot", DoorsRoot)),
	processor#interface(getenv("DoorsInput", DoorsInput)),
	terminal_input(DoorsInput?),
	start1(DoorsRoot?, Ok).

terminal_input(DoorsInput) :-
  DoorsInput = on |
	processor#device(open(tty));

  otherwise |
	DoorsInput = _.

start1(DoorsRoot, Ok) :-
  string(DoorsRoot) |
	begin(DoorsRoot, Ok);

  DoorsRoot = [] |
	begin(DOORS_ROOT_DEFAULT, Ok);

  otherwise |
	computation#display(term, ("INTERNAL server warning - unexpected $DoorsRoot value of " : DoorsRoot), type(ground)),
	begin(DOORS_ROOT_DEFAULT, Ok).

begin(DoorsRoot, Ok) :-
  string(DoorsRoot) |
	make_logging_directory(DoorsRoot, Ok0),
	utils#append_strings([DoorsRoot, "/", TOKEN_FILE], TokenFile),
	display_log_error(Ok0?),
	read_config_file(DoorsRoot, 
		Places_Config_Card, People_Config_Card, Hello1),
	id_card#query_ok(IdCard?, 
		[(net_name :P), (net_domain : D), (net_id : NetId)], Ok1),
	listen2(Places_Config_Card?, P1, P2),
	id_card#remove_attributes(P1?, [what_to_log], P1'),
	append([(name: places_room), (type:room)], P1'?, Places_IdCard),
	initial_events_operations(P2?, InitialEventsOperations),
	places_room#create(Places_IdCard?, PlacesDoor, Ok2),
	append(People_Config_Card?, [(name: people_room), (type:room), 
                        (net_name:P?), (domain_name: D?), (net_id : NetId?),
			(version : VERSION)], People_IdCard),
        people_room#create(People_IdCard?, PeopleDoor, Ok3),
	Sci_IdCard = [(name : sci_room), (type :room)],
	sci_room#create(Sci_IdCard?, SciDoor, OkSci0),
	doors#corridor(Door1, Door2),
	doors#corridor(Door3, Door4),
	processor#room([show(id_card(self), IdCard, Ok4),
			paste(PlacesDoor?, places, Ok5),
			paste(PeopleDoor?, people, Ok6),
			paste(SciDoor?, sci, Ok7),
			message([sci], [],
				paste(Door1?, places, OkSci1), OkSci2),
			message([places], [],
				paste(Door2?, sci, OkSci3), OkSci4),
			message([sci], [],
				paste(Door3?, people, OkSci5), OkSci6),
			message([people], [],
				paste(Door4?, sci, OkSci7), OkSci8),
			message([places], [], internal_add_event_operations(
				InitialEventsOperations?), Ok8),
			message([places], [], 
				initialize_api(TokenFile?, 
					GrantedConnections, OkAPI), Ok9)
			],Okend),
	sequential_and([Okend?, Ok4?, Ok1?, Ok2?, Ok3?, Ok5?, 
			Ok6?, Ok7?, OkSci0?, 
			OkSci2?, OkSci1?, OkSci4?, OkSci3?, OkSci6?, OkSci5?,
			OkSci8?, OkSci7?, Ok8?, Ok9?, OkAPI?], OkAll),
	Ok = OkAll?,
	display_hello(OkAll??, GrantedConnections?, Hello1?).

display_hello(Ok, GrantedConnections, Hello1) :-
  Ok =\= true |
	Hello1 = _, GrantedConnections = _;

  Ok = true |
	date#get_date(Date),
	utils#append_strings(["
Welcome to Doors Server Version ", 
				VERSION, " - ", Date?], Hello),
	utils#append_strings([CONFIG_CONNECTIONS_PER_SERVER_MAX, ": ", 
		GrantedConnections, "."], Capacity),
	display_hello1([Hello?, Capacity? | Hello1]).

display_hello1(H) + (Done = true) :-
  H ? [] |
	self;

  H ? Msg, Msg =\= [] |
	computation#display(term, Msg, [close(Done, Done'), type(ground)]),
	self;

  H = [] |
	Done = _.

	

display_log_error(Ok) :-
  Ok = true |
	true;

  Ok =\= true |
	computation#display("Logging error: server operating without logging").

make_logging_directory(DoorsRoot, Ok) :-
  string(DoorsRoot) |
	utils#append_strings([DoorsRoot, "/", LOG_DIR], LogDir),
	listen2(LogDir?, Ld1, Ld2),
	file#isdirectory(Ld1?, Reply),
	make_directory(Ld2?, Reply?, Ok).
 
make_directory(LogDir, Reply, Ok) :-
    string(LogDir), Reply = true |
    	Ok = true;
 
    string(LogDir), Reply =\= true |
%       computation#display("making dir"),
        utils#append_strings(["mkdir ", LogDir], Mkdir),
        processor#interface(unix_call(Mkdir?), Ok).
  
read_config_file(DoorsRoot, Places_Config_Card, People_Config_Card, Hello1) :-
  string(DoorsRoot) |
        utils#append_strings([DoorsRoot, "/", CONFIG_FILE], DoorsConfigFile),
	utils#append_strings([DoorsRoot, "/",  LOG_DIR, "/", LOG_FILE], 
								LogFile),
	utils#append_strings([DoorsRoot, "/",  DOORS_MAPPED_URL_FILE],
							MappedUrlFile),
        get_config_parameters(DoorsConfigFile?, WhatToLog, 
                PlaceIdleTime, EventsCacheMax, 
                PeopleCacheMax, URLMappingScript, MaxPresencesPerPlace, 
		MaxConnectionsPerServer, MaxClonesToServer, 
		PresenceIdleTime, Hello1),
	listen2(WhatToLog?, W1, W2),
        logging_monitor#[log_file(LogFile?), log(W1?), start],
         
        Places_Config_Card =    
		[(idle_time : PlaceIdleTime?),
                 (events_cache_max : EventsCacheMax?),
		 (what_to_log : W2?),
		 (url_mapping_script : URLMappingScript?(MappedUrlFile?)),
		 (max_presences_per_place : MaxPresencesPerPlace?),
		 (requested_connections_per_server : MaxConnectionsPerServer?),
		 (max_clones_to_server : MaxClonesToServer?),
		 (presence_idle_time : PresenceIdleTime?)],

        People_Config_Card = [(people_cache_max : PeopleCacheMax?)]. 

get_config_parameters(DoorsConfigFile,  WhatToLog, 
                PlaceIdleTime, EventsCacheMax, PeopleCacheMax, 
		URLMappingScript, MaxPresencesPerPlace, 
		MaxConnectionsPerServer, MaxClonesToServer, 
		PresenceIdleTime, Hello1) :-
	
  string(DoorsConfigFile) |
        file#get_file(DoorsConfigFile, Text, chars, Ok),
        extract_parameters(Ok?, DoorsConfigFile, Text?,  WhatToLog, 
                PlaceIdleTime, EventsCacheMax, PeopleCacheMax, 
		URLMappingScript, MaxPresencesPerPlace, 
		MaxConnectionsPerServer, MaxClonesToServer, 
		PresenceIdleTime, Hello1).

extract_parameters(Ok, DoorsConfigFile, Text,  WhatToLog, 
                PlaceIdleTime, EventsCacheMax, PeopleCacheMax,
		URLMappingScript, MaxPresencesPerPlace, 
		MaxConnectionsPerServer, MaxClonesToServer,
		PresenceIdleTime, Hello1) :-

  Ok =\= true |
        Text = _,
        Hello1 = [M0?, M1?, M2?, M3?, M4?],
        utils#append_strings([DoorsConfigFile, 
			" not found - using system defaults."], M0),
	utils#append_strings([CONFIG_PLACE_IDLE_TIME, ": ", 
		PLACE_IDLE_TIME_DEFAULT_STRING, "."], M1),
	utils#append_strings([CONFIG_EVENTS_CACHE_MAX, ": ", 
		EVENTS_CACHE_MAX_DEFAULT, "."], M2),
	utils#append_strings([CONFIG_LOG, ": ", 
		WHAT_TO_LOG_DEFAULT_STRING, "."], M3),
	utils#append_strings([CONFIG_PEOPLE_CACHE_MAX, ": ", 
		PEOPLE_CACHE_MAX_DEFAULT, "."], M4),
        WhatToLog = WHAT_TO_LOG_DEFAULT, 
        PlaceIdleTime = PLACE_IDLE_TIME_DEFAULT,
        EventsCacheMax = EVENTS_CACHE_MAX_DEFAULT,
        PeopleCacheMax = PEOPLE_CACHE_MAX_DEFAULT,
	URLMappingScript = URL_MAPPING_SCRIPT_DEFAULT,
	MaxPresencesPerPlace = PRESENCES_PER_PLACE_MAX_DEFAULT,
	MaxConnectionsPerServer = CONNECTIONS_PER_SERVER_MAX_DEFAULT,
	MaxClonesToServer = CLONES_TO_SERVER_MAX_DEFAULT,
	PresenceIdleTime = PRESENCE_IDLE_TIME_DEFAULT;

  Ok = true | 
	DoorsConfigFile = _,
	parse#tokenize(Text, Params),
	extract_parameters1(Params?, WhatToLog, 
                PlaceIdleTime, EventsCacheMax, PeopleCacheMax, 
		URLMappingScript, MaxPresencesPerPlace, 
		MaxConnectionsPerServer, MaxClonesToServer, 
		PresenceIdleTime, Hello1).

extract_parameters1(Params, WhatToLog, PlaceIdleTime, EventsCacheMax, 
	PeopleCacheMax, URLMappingScript, MaxPresencesPerPlace, 
	MaxConnectionsPerServer, MaxClonesToServer,
	PresenceIdleTime, Hello1) :-

  Params ? Param, Param = `CONFIG_URL_MAPPING_SCRIPT,
  Params' ? Script |
	verify_string_param_no_msg(Script, URLMappingScript, 
		CONFIG_URL_MAPPING_SCRIPT, URL_MAPPING_SCRIPT_DEFAULT, Msg),
	Hello1 ! Msg?,
	URLMappingScript' = instantiated,
	self;

  Params ? Param, Param = `CONFIG_PRESENCES_PER_PLACE_MAX,
  Params' ? Max |
	verify_one_integer_param_no_msg(Max, MaxPresencesPerPlace,
		CONFIG_PRESENCES_PER_PLACE_MAX,
					PRESENCES_PER_PLACE_MAX_DEFAULT, Msg),
	Hello1 ! Msg?,
	MaxPresencesPerPlace' = instantiated,
	self;

  Params ? Param, Param = `CONFIG_CONNECTIONS_PER_SERVER_MAX,
  Params' ? Max |
	verify_one_integer_param_no_msg(Max, MaxConnectionsPerServer,
		CONFIG_CONNECTIONS_PER_SERVER_MAX,
				CONNECTIONS_PER_SERVER_MAX_DEFAULT, _Msg),
	MaxConnectionsPerServer' = instantiated,
	self;

  Params ? Param, Param = `CONFIG_CLONES_TO_SERVER_MAX,
  Params' ? Max |
	verify_one_integer_param_no_msg(Max, MaxClonesToServer,
		CONFIG_CLONES_TO_SERVER_MAX,
				CLONES_TO_SERVER_MAX_DEFAULT, Msg),
	Hello1 ! Msg?,
	MaxClonesToServer' = instantiated,
	self;

  Params ? Param, Param = `CONFIG_PLACE_IDLE_TIME,
  Params' ? Length, Params'' ? Units |
	verify_place_idle_time(Length, Units, PlaceIdleTime, Msg),
	Hello1 ! Msg?,
	PlaceIdleTime' = instantiated,
	self;

  Params ? Param, Param = `CONFIG_PRESENCE_IDLE_TIME,
  Params' ? Length, Params'' ? Units |
	verify_presence_idle_time(Length, Units, PresenceIdleTime, Msg),
	Hello1 ! Msg?,
	PresenceIdleTime' = instantiated,
	self;

  Params ? Param, Param = `CONFIG_EVENTS_CACHE_MAX,
  Params' ? Size |
	verify_one_integer_param(Size, EventsCacheMax, CONFIG_EVENTS_CACHE_MAX,
					EVENTS_CACHE_MAX_DEFAULT, Msg),
	Hello1 ! Msg?,
	EventsCacheMax' = instantiated,
	self;

  Params ? Param, Param = `CONFIG_LOG |
	verify_and_get_log_params(Params', Params'', WhatToLog, Msg),
	Hello1 ! Msg?,
	WhatToLog' = instantiated,
	self;
 
  Params ? Param, Param =`CONFIG_PEOPLE_CACHE_MAX,
  Params' ? Size |
	verify_one_integer_param(Size, PeopleCacheMax, CONFIG_PEOPLE_CACHE_MAX,
					PEOPLE_CACHE_MAX_DEFAULT, Msg),
	Hello1 ! Msg?,
	PeopleCacheMax' = instantiated,
	self;

  Params ? Param, otherwise |
	utils#append_strings(["Unrecognized configuration file parameter ",
		Param, " ignored."], Msg),
	Hello1 ! Msg?,
	self;

  Params = [] |
	Hello1 = [Msg1?, Msg2?, Msg3?, Msg4?],
	default_if_omitted(PlaceIdleTime, CONFIG_PLACE_IDLE_TIME,
			PLACE_IDLE_TIME_DEFAULT,
			PLACE_IDLE_TIME_DEFAULT_STRING, Msg1),
	default_if_omitted(EventsCacheMax, CONFIG_EVENTS_CACHE_MAX,
			EVENTS_CACHE_MAX_DEFAULT, 
			EVENTS_CACHE_MAX_DEFAULT, Msg2),
	default_if_omitted(PeopleCacheMax, CONFIG_PEOPLE_CACHE_MAX,
			PEOPLE_CACHE_MAX_DEFAULT, 
			PEOPLE_CACHE_MAX_DEFAULT, Msg3),
	default_if_omitted(WhatToLog, CONFIG_LOG, WHAT_TO_LOG_DEFAULT,
			WHAT_TO_LOG_DEFAULT_STRING, Msg4),
	default_if_omitted_no_msg(URLMappingScript, URL_MAPPING_SCRIPT_DEFAULT),
	default_if_omitted_no_msg(MaxPresencesPerPlace, 
					PRESENCES_PER_PLACE_MAX_DEFAULT),
	default_if_omitted_no_msg(MaxConnectionsPerServer, 
					CONNECTIONS_PER_SERVER_MAX_DEFAULT),
	default_if_omitted_no_msg(MaxClonesToServer, 
					CLONES_TO_SERVER_MAX_DEFAULT),
	default_if_omitted_no_msg(PresenceIdleTime, PRESENCE_IDLE_TIME_DEFAULT).


default_if_omitted_no_msg(ParameterVar, Default) :-
  ParameterVar = instantiated |
	Default = _;

  unknown(ParameterVar), ground(Default) |
	ParameterVar = Default.

default_if_omitted(ParameterVar, ParameterString, Default, DefaultMsg, Msg) :-
  ParameterVar = instantiated |
	Msg = [],
	ParameterString = _,
	Default = _, DefaultMsg = _;

  unknown(ParameterVar), ground(Default) |
	ParameterVar = Default, 
	utils#append_strings([ParameterString, 
                ": not specified - using default value of ", DefaultMsg, "."], 
                Msg).
        
verify_place_idle_time(Length, Units, PlaceIdleTime, Msg) :-
  integer(Length), string(Units) |
	sets#element(Units, CONFIG_TIME_SET, Result),
	verify_place_idle_time1(Result?, Length, Units, PlaceIdleTime, Msg);

  otherwise |
	place_idle_time_error(Length, Units, PlaceIdleTime, Msg).

verify_place_idle_time1(Result, Length, Units, PlaceIdleTime, Msg) :-
  Result = true, integer(Length), string(Units) |
	utils#append_strings([CONFIG_PLACE_IDLE_TIME, ": ", Length, " ", Units, "."],
		Msg),
	PlaceIdleTime = Units(Length);

  Result =\= true |
	place_idle_time_error(Length, Units, PlaceIdleTime, Msg).

place_idle_time_error(Length, Units, PlaceIdleTime, Msg) :-
	PlaceIdleTime = PLACE_IDLE_TIME_DEFAULT,
	utils#append_strings([CONFIG_PLACE_IDLE_TIME, ": invalid parameters (",
		Length, " ", Units, ") - using default value of ",
		PLACE_IDLE_TIME_DEFAULT_STRING, "."], Msg).

verify_presence_idle_time(Length, Units, PresenceIdleTime, Msg) :-
  integer(Length), string(Units) |
	sets#element(Units, CONFIG_TIME_SET, Result),
	verify_presence_idle_time1(Result?, Length, Units, 
					PresenceIdleTime, Msg);

  otherwise |
	presence_idle_time_error(Length, Units, PresenceIdleTime, Msg).

verify_presence_idle_time1(Result, Length, Units, PresenceIdleTime, Msg) :-
  Result = true, integer(Length), string(Units) |
	utils#append_strings([CONFIG_PRESENCE_IDLE_TIME, ": ", Length, " ", Units, "."],
		Msg),
	PresenceIdleTime = Units(Length);

  Result =\= true |
	presence_idle_time_error(Length, Units, PresenceIdleTime, Msg).

presence_idle_time_error(Length, Units, PresenceIdleTime, Msg) :-
	PresenceIdleTime = PRESENCE_IDLE_TIME_DEFAULT,
	utils#append_strings([CONFIG_PRESENCE_IDLE_TIME, ": invalid parameters (",
		Length, " ", Units, ") - ignored."], Msg).
	
verify_one_integer_param(Val, ParameterVar, ParameterString, Default, Msg) :-
  integer(Val) |
	Default = _,
	ParameterVar = Val,
	utils#append_strings([ParameterString, ": ", Val, "."], Msg);

  otherwise, integer(Default) |
	ParameterVar = Default,
	utils#append_strings([ParameterString, ": invalid parameter (",	Val,
		") - using default value of ", Default, "."], Msg).

verify_one_integer_param_no_msg(Val, ParameterVar, ParameterString, Default, Msg) :-
  integer(Val) |
	Default = _,
	ParameterVar = Val,
	utils#append_strings([ParameterString, ": ", Val, "."], Msg);

  otherwise, integer(Default) |
	ParameterVar = Default,
	ParameterString = _, Val = _,
	Msg = [].

verify_string_param_no_msg(Val, ParameterVar, ParameterString, Default, Msg) :-
  string(Val) |
	Default = _,
	ParameterVar = Val,
	utils#append_strings([ParameterString, ": ", Val, "."], Msg);

  otherwise, constant(Default) |
	ParameterVar = Default,
	ParameterString = _, Val = _,
	Msg = [].

verify_and_get_log_params(Params, Params1, WhatToLog, Msg) :-
	verify_and_get_log_params1(Params, Params1, WhatToLogStrings, Msg),
	codes#category_strings_to_codes(WhatToLogStrings?, WhatToLog).


verify_and_get_log_params1(Params, Params1, WhatToLog, Msg)
 + (W, LogList, M, MsgList):-

  initially |
	W = LogList?,
	M = ["LOG: " | MsgList?];

  Params ? EventCategory, string(EventCategory), EventCategory =\= ",", 
  EventCategory =\= CONFIG_LOG_NONE, EventCategory =\= CONFIG_LOG_ALL |
	include_if_event_category(EventCategory, LogList, LogList'?, 
			MsgList, MsgList'?),
	self;

  Params ? "," |
	self;

  Params ? CONFIG_LOG_NONE |
	LogList = [],
	MsgList = nothing,
	utils#append_strings(M?, Msg),
	WhatToLog = W?,
	Params1 = Params' ;

  Params ? CONFIG_LOG_ALL |
	LogList = WHAT_TO_LOG_ALL,
	MsgList = WHAT_TO_LOG_ALL_MSG,
	utils#append_strings(M?, Msg),
	WhatToLog = W?,
	Params1 = Params' ;

  Params = [] |
	Params1 = [],
	LogList = [],
	MsgList = [],
	utils#append_strings(M?, Msg), 
	WhatToLog = W? ;

  Params ? _, otherwise |
	LogList  = [],
	Params1 = Params, Params' = _,
	MsgList = [],
	utils#append_strings(M?, Msg),
	WhatToLog = W?.

include_if_event_category(EventCategory, LogList, LogList1, MsgList, MsgList1) :-
  EventCategory = PRESENCE_EVENT_CATEGORY_STRING |
	LogList = [EventCategory | LogList1],
	MsgList = [EventCategory, " " | MsgList1];
 
  EventCategory = PLACE_EVENT_CATEGORY_STRING |
	LogList = [EventCategory | LogList1],
	MsgList = [EventCategory, " " | MsgList1];

  EventCategory = TRANSIENT_EVENT_CATEGORY_STRING |
	LogList = [EventCategory | LogList1],
	MsgList = [EventCategory, " " | MsgList1];

  EventCategory = ALERT_EVENT_CATEGORY_STRING |
	LogList = [EventCategory | LogList1],
	MsgList = [EventCategory, " " | MsgList1];

  EventCategory = OTHER_EVENT_CATEGORY_STRING |
	LogList = [EventCategory | LogList1],
	MsgList = [EventCategory, " " | MsgList1];

  otherwise |
	computation#display(term, ("Illegal LOG category ignored" :
		EventCategory), type(ground)),
	LogList = LogList1,
	MsgList = MsgList1.
  
initial_events_operations(Place_IdCard, InitialEventsOperations) :-
	id_card#query_ok(Place_IdCard, [(what_to_log : WhatToLog)], Ok1),
	EventsOperations = INITIAL_EVENTS_OPERATIONS,			  
	ok(Ok1?, initial_events_operations),
	include_log(WhatToLog?, EventsOperations?, InitialEventsOperations).

include_log(WhatToLog, EventsOperations, InitialEventsOperations) :-
  WhatToLog ? EventCategory |
	places_room#update_events_operations(add, EventCategory - [log],
		EventsOperations, EventsOperations'),
	self;

  WhatToLog = [] |
	InitialEventsOperations = EventsOperations.

abort :-
%	computation#display("Want to abort").
	computation#shell(signal_fcp(exit(1))).

exit :-
%	computation#display("Want to exit").
	computation#shell(signal_fcp(exit(0))).
