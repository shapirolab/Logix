/*  $Header: */

-language(dfcp).
-mode(interrupt).
-include([api_includes,server_includes]).
-include(string_includes).

/************************************************************************
*** server_api.cp 	Module to translate api structures to/from
*			fcp attribute value pairs. This operation is only
*			done if necessary, not for all api structures.
*************************************************************************/

presence_state_tuple_to_idcard(Tuple, IdCard) :-

  Tuple = {NickName, Version, Position, ClickPosition, ClickDoor,
		State, EditDate, AlertDate, _JoinedDate, DetailsDate,
		HasIcon, Objects, Connection, AudioPort, AudioFocus} |
%	computation#display(Tuple),
	IdCard = [(PRESENCE_NICKNAME_ATTR : NickName), 
			(PRESENCE_VERSION_ATTR : Version),
			(PRESENCE_POSITION_ATTR : Position),
			(PRESENCE_CLICK_POSITION_ATTR :  ClickPosition),
			(PRESENCE_CLICK_DOOR_ATTR :  ClickDoor),
			(PRESENCE_USER_STATE_ATTR : State),
			(PRESENCE_EDIT_ATTR : EditDate),
			(PRESENCE_ALERTED_ATTR : AlertDate),
			(PRESENCE_TIMESTAMP_ATTR : DetailsDate),
			(PRESENCE_HAS_ICON_ATTR : HasIcon),
			(PRESENCE_OBJECTS_ATTR : ObjectsIdCards?),
			(PRESENCE_CONNECTION_ATTR : Connection),
			(PRESENCE_AUDIO_PORT_ATTR : AudioPort),
			(PRESENCE_AUDIO_FOCUS_ATTR : AudioFocus)],
	objects_tuple_to_idcards(Objects, ObjectsIdCards).

objects_tuple_to_idcards(Objects, ObjectsIdCards) :-
  Objects = [] |
	ObjectsIdCards = [];


  Objects ? Object, Object = {ObjectId, ObjectState} |
	object_state_to_idcard(ObjectId, ObjectState, IdCard),
	ObjectsIdCards ! IdCard?,
	self.

object_state_to_idcard(ObjectId, ObjectState, IdCard) :-
  ObjectState = {Type, Name, Position, DetailsDate, HasIcon,
		ConvType, MaxCapacity, NumUsed, MboneChannel} |
	IdCard =  [(PRESENCE_OBJECT_OBJECT_ID_ATTR : ObjectId),
			(PRESENCE_OBJECT_STATE_NAME_ATTR : Name),
			(PRESENCE_OBJECT_STATE_TIMESTAMP_ATTR : DetailsDate),
			(PRESENCE_OBJECT_HAS_ICON_ATTR : HasIcon),
			(PRESENCE_OBJECT_STATE_POSITION_ATTR : Position),
			(PRESENCE_OBJECT_STATE_TYPE_ATTR : Type),
			(PRESENCE_OBJECT_CONV_TYPE_ATTR : ConvType),
			(PRESENCE_OBJECT_MAX_CAPACITY_ATTR : MaxCapacity'?),
			(PRESENCE_OBJECT_NUMBER_USED_ATTR : NumUsed'?),
			(PRESENCE_OBJECT_MBONE_CHANNEL_ATTR : MboneChannel),
			(PRESENCE_OBJECT_PASSENGERS_ATTR : []),
			(PRESENCE_OBJECT_USED_SLOTS_ATTR : [])],
	convert_to_integer_report_error(MaxCapacity, MaxCapacity'),
	convert_to_integer_report_error(NumUsed, NumUsed').

presence_state_list_to_tuple(IdCardList, List) +
(OutputList, O = OutputList?) :- 
  IdCardList = [] |
	OutputList = [],
	List = O? ;

  IdCardList ? PresenceId - IdCard |
	presence_state_idcard_to_tuple(IdCard, T),
	OutputList ! {PresenceId, T?},
	self.

presence_state_idcard_to_tuple(IdCard, Tuple) :-
	id_card#query_ok(IdCard, [(PRESENCE_JOINED_DATE_ATTR : JoinedDate),
			(PRESENCE_NICKNAME_ATTR : NickName), 
			(PRESENCE_VERSION_ATTR : Version),
			(PRESENCE_POSITION_ATTR : Position),
			(PRESENCE_CLICK_POSITION_ATTR :  ClickPosition),
			(PRESENCE_CLICK_DOOR_ATTR :  ClickDoor),
			(PRESENCE_USER_STATE_ATTR : State),
			(PRESENCE_EDIT_ATTR : EditDate),
			(PRESENCE_ALERTED_ATTR : AlertDate),
			(PRESENCE_TIMESTAMP_ATTR : DetailsDate),
			(PRESENCE_HAS_ICON_ATTR : HasIcon),
			(PRESENCE_OBJECTS_ATTR : ObjectsIdCards),
			(PRESENCE_CONNECTION_ATTR : Connection),
			(PRESENCE_AUDIO_PORT_ATTR : AudioPort),
			(PRESENCE_AUDIO_FOCUS_ATTR : AudioFocus)], Ok1),
	ok(Ok1?, presence_state_idcard_to_tuple),
	Tuple = {NickName?, Version?, Position?, ClickPosition?, ClickDoor?,
                State?, EditDate?, AlertDate?, JoinedDate?, DetailsDate?,
                HasIcon?, Objects?, Connection?, AudioPort?, AudioFocus?},
	objects_idcards_to_tuple(ObjectsIdCards?, Objects).


objects_idcards_to_tuple(ObjectsIdCards, Objects) + (L, LL = L?) :-
 
  ObjectsIdCards = [] |
	L = [],
	Objects = LL? ;

  ObjectsIdCards = undefined |
	L = [],
	list_to_struct(LL?, Objects);

  ObjectsIdCards ? IdCard |
	object_idcard_to_tuple(IdCard, T),
	L ! T?,
	self.

object_idcard_to_tuple(IdCard, T) :-
	id_card#query_ok(IdCard, [(PRESENCE_OBJECT_STATE_NAME_ATTR : Name),
                        (PRESENCE_OBJECT_OBJECT_ID_ATTR : ObjectId),
                        (PRESENCE_OBJECT_STATE_TIMESTAMP_ATTR : DetailsDate),
                        (PRESENCE_OBJECT_HAS_ICON_ATTR : HasIcon),
                        (PRESENCE_OBJECT_STATE_POSITION_ATTR : Position),
                        (PRESENCE_OBJECT_STATE_TYPE_ATTR : Type),
			(PRESENCE_OBJECT_CONV_TYPE_ATTR : ConvType),
			(PRESENCE_OBJECT_MAX_CAPACITY_ATTR : MaxCapacity),
			(PRESENCE_OBJECT_NUMBER_USED_ATTR : NumUsed),
			(PRESENCE_OBJECT_MBONE_CHANNEL_ATTR:MboneChannel)],Ok1),
	ok(Ok1?, object_idcard_to_tuple),
	T = {ObjectId?, {Type?, Name?, Position?, DetailsDate?, HasIcon?,
	     ConvType?, MaxCapacity?, NumUsed?, MboneChannel?}}.


user_details_tuple_to_idcards(UserResponseType, Updates, U) :-
  UserResponseType = USER_DATA_RESPONSE |
	user_data_tuple_to_idcards(Updates, U);

  UserResponseType = USER_FACE_RESPONSE |
	user_face_tuple_to_idcards(Updates, U);

  UserResponseType = USER_ALL_RESPONSE |
	user_all_tuple_to_idcards(Updates, U).

user_data_tuple_to_idcards(Updates, U) :-
  Updates = [] |
	U = [];

  Updates ? Update |
	user_data_update_to_idcard(Update, IdCard),
	U ! IdCard?,
	self.

user_data_update_to_idcard(Update, IdCard) :-
  Update = {DetailsDate, NetName, AgentData} |
	IdCard = [	(PERSON_FULLNAME_ATTR : NetName), 
			(PERSON_OBJECT_NAME_ATTR : ""),
			(PERSON_TIMESTAMP_ATTR : DetailsDate),
			(PERSON_DATA_ATTR : AgentData)].

user_face_tuple_to_idcards(Updates, U) :-
  Updates = [] |
	U = [];

  Updates ? Update |
	user_face_update_to_idcard(Update, IdCard),
	U ! IdCard?,
	self.

user_face_update_to_idcard(Update, IdCard) :-
  Update = {DetailsDate, NetName, ObjectName, Face} |
	IdCard = [	(PERSON_FULLNAME_ATTR : NetName), 
			(PERSON_OBJECT_NAME_ATTR : ObjectName),
			(PERSON_TIMESTAMP_ATTR : DetailsDate),
			(PERSON_FACE_ATTR : Face)].

user_all_tuple_to_idcards(Updates, U) :-
  Updates = [] |
	U = [];

  Updates ? Update |
	user_all_update_to_idcard(Update, IdCard),
	U ! IdCard?,
	self.

user_all_update_to_idcard(Update, IdCard) :-
  Update = {DetailsDate, NetName, AgentData, Face} |
	IdCard = [	(PERSON_FULLNAME_ATTR : NetName), 
			(PERSON_OBJECT_NAME_ATTR : ""),
			(PERSON_TIMESTAMP_ATTR : DetailsDate),
			(PERSON_DATA_ATTR : AgentData),
			(PERSON_FACE_ATTR : Face)].


events_cache_list_to_tuple(URL, EventsCache, Tuple) +(L, LL = L?) :-
  EventsCache = [] |
	URL = _,
	L = [],
	Tuple = LL? ;

  EventsCache ? Event, 
  Event = [PresenceId, Date, EventCategory, EventType, To,Contents,Recipients],
  constant(EventCategory), constant(To), constant(URL),
  constant(PresenceId) |
	make_presences_list(PresenceId, To, Recipients, PresencesList),
	make_event_content(EventCategory, EventType, Contents, EventContent),
	EventTuple = {EventCategory, EventContent?},
	HeaderTuple = {PresenceId, To, PresencesList?, {[], [], URL}, Date},
	L ! {EventTuple?, HeaderTuple?},
	self.

% Most of the structures are uniform 
make_event_content(EventCategory, EventType, Contents, EventContent) :-
  EventCategory = ALERT_EVENT_CATEGORY, EventType = ALERT_ALERT_EVENT |
	EventContent = {EventType},
	Contents = _;

  otherwise |
	EventCategory = _,
	EventContent = EventType(Contents).

make_presences_list(PresenceId, To, Recipients, PresencesList) :-
  To = DOORS_LIST |
	sets#difference(Recipients, [PresenceId], PresencesList);

  To =\= DOORS_LIST |
	PresenceId = _,
	PresencesList = [],
	Recipients = _.

translate_event(HeaderTuple, EventTuple, Term) :-
  HeaderTuple = {PresenceId, Destination, List, _Door, _Date}, 
  EventTuple = {EventCategory, EventContent},
  EventContent = EventType(Contents),
  EventCategory =\= PRESENCE_EVENT_CATEGORY |
	destination_to_to(Destination, List, To),
	Term = DOORS_EVENT_FUNCTOR(PresenceId, To?, EventCategory, EventType,
					Contents);

  HeaderTuple = {PresenceId, Destination, List, _Door, _Date}, 
  EventTuple = {EventCategory, EventContent},
  EventContent = {EventType} |
	destination_to_to(Destination, List, To),
	Term = DOORS_EVENT_FUNCTOR(PresenceId, To?,EventCategory, EventType,[]);

  HeaderTuple = {PresenceId, _Destination, _List, _Door, _Date}, 
  EventTuple = {PRESENCE_EVENT_CATEGORY, {PRESENCE_LEFT_EVENT, NewDoor}} |
	Term = DOORS_DISCONNECT_FUNCTOR(PresenceId, NewDoor);

  otherwise |
	computation#display(term, ("Cannot translate event " :
			{HeaderTuple, EventTuple}), type(ground)),
	Term = [].


translate_request(HeaderTuple, RequestTuple, Term, Action) :-
  HeaderTuple = {PresenceId, _Destination, _List, _Door, _Date},
  RequestTuple = {ClientData, USER_REQUEST_CATEGORY, 
				{UserRequestType, UserObjectNames}},
  constant(UserRequestType) |
	Action = no_action,	% results from other person's entry
	Term = DOORS_QUERY_USER_DETAILS_FUNCTOR(PresenceId, ClientData,
			UserRequestType, UserObjectNames);

  HeaderTuple = {PresenceId, _Destination, _List, _Door, _Date},
  RequestTuple = {ClientData, SERVER_REQUEST_CATEGORY, 
					{SERVER_PRESENCES_COUNT_REQUEST}} |
	Action = action,
	Term = DOORS_QUERY_SERVER_PRESENCES_COUNT_FUNCTOR(
							PresenceId, ClientData);

  HeaderTuple = {PresenceId, _Destination, _List, _Door, _Date},
  RequestTuple = {ClientData, SERVER_REQUEST_CATEGORY, 
					{SERVER_PRESENCES_LIST_REQUEST}} |
	Action = action,
	Term = DOORS_QUERY_SERVER_PRESENCES_LIST_FUNCTOR(
							PresenceId, ClientData);

  HeaderTuple = {PresenceId, _Destination, _List, _Door, _Date}, 
  RequestTuple = {_ClientData, PLACE_REQUEST_CATEGORY, 
				{PLACE_CONNECT_REQUEST, PlaceConnectRequest}},
  PlaceConnectRequest = {PlaceState, PresenceState, FromDoor, ToDoor, 
								BusinessCard},
  ToDoor = {_, _, URL}, ground(PresenceState) |
	Action = action,
	Term = DOORS_CONNECT_FUNCTOR(PresenceId, URL, PlaceState, 
		PresenceState,  FromDoor, BusinessCard);

  HeaderTuple = {PresenceId, _Destination, _List, _Door, _Date}, 
  RequestTuple = {ClientData, PLACE_REQUEST_CATEGORY, 
				{PLACE_VIEW_EVENTS_REQUEST, 
					{GetSnapshot, Categories}}} |
	Action = action,
	Term = DOORS_VIEW_EVENTS_FUNCTOR(PresenceId, ClientData,
						GetSnapshot, Categories);

  HeaderTuple = {PresenceId, _Destination, _List, _Door, _Date}, 
  RequestTuple = {ClientData, PLACE_REQUEST_CATEGORY, 
  				{PLACE_CACHE_REQUEST, 
					{Categories, Number, From, Until}}} |
	Action = action,
	convert_to_integer_set_to_max(Number, Number'),
	Term = DOORS_QUERY_CACHE_FUNCTOR(PresenceId, ClientData,
					Categories, Number'?, From, Until);

  HeaderTuple = {PresenceId, _Destination, _List, _Door, _Date}, 
  RequestTuple = {ClientData, PLACE_REQUEST_CATEGORY,
					PLACE_PRESENCES_REQUEST(_)} |
	Action = action,
	Term = DOORS_QUERY_PLACE_PRESENCES_FUNCTOR(PresenceId, ClientData);

  HeaderTuple = {PresenceId, _Destination, _List, _Door, _Date}, 
  RequestTuple = {_ClientData, PLACE_REQUEST_CATEGORY,
				{PLACE_CONNECT_TO_PRESENCE_REQUEST,
					{ConnecteePresenceId, ConnecteePosition,
					 NewPosition}}} |
	Action = action,
	Term = DOORS_CONNECT_TO_PRESENCE_FUNCTOR(PresenceId, 
		ConnecteePresenceId, ConnecteePosition,	NewPosition);

  HeaderTuple = {PresenceId, _Destination, _List, _Door, _Date}, 
  RequestTuple = {_ClientData, PLACE_REQUEST_CATEGORY,
				{PLACE_CONNECT_TO_OBJECT_REQUEST,
					{ConnecteePresenceId, 
					 ConnecteeObjectId, ConnecteePosition,
					 NewPosition}}} |
	Action = action,
	Term = DOORS_CONNECT_TO_OBJECT_FUNCTOR(PresenceId, 
		ConnecteePresenceId, ConnecteeObjectId,
					ConnecteePosition, NewPosition);

  HeaderTuple = {PresenceId, _Destination, _List, _Door, _Date}, 
  RequestTuple = {ClientData, PLACE_REQUEST_CATEGORY,
				{PLACE_OBJECT_CONNECTIONS_REQUEST,
					{ConnecteePresenceId, 
					 ConnecteeObjectId}}} |
	Action = action,
	Term = DOORS_QUERY_OBJECT_CONNECTIONS_FUNCTOR(PresenceId, ClientData,
		ConnecteePresenceId, ConnecteeObjectId);

  HeaderTuple = {PresenceId, _Destination, _List, _Door, _Date}, 
  RequestTuple = {_ClientData, PLACE_REQUEST_CATEGORY,
				{PLACE_CREATE_OBJECT_REQUEST,
					{ObjectId, ObjectState}}} |
	Action = action,
%	computation#display("server api got create object"),
	Term = DOORS_CREATE_OBJECT_FUNCTOR(PresenceId, ObjectId, ObjectState);

  otherwise |
	computation#display(term, ("Cannot translate request " :
			{HeaderTuple, RequestTuple}), type(ground)),
	Action = no_action,
	Term = [].

translate_response(HeaderTuple, ResponseTuple, Term) :-
  HeaderTuple = {PresenceId, _Destination, _List, _Door, _Date},
  ResponseTuple = {_, USER_RESPONSE_CATEGORY, {UserResponseType, Updates}} |
	user_details_tuple_to_idcards(UserResponseType, Updates, U),
	Term = DOORS_UPDATE_USER_DETAILS_FUNCTOR(PresenceId, U?);

  otherwise |
	computation#display(term, ("Cannot translate response " :
			{HeaderTuple, ResponseTuple}), type(ground)),
	Term = [].
	
destination_to_to(Destination, List, To) :-
  Destination = DOORS_LIST |
	To = List;

  Destination = DOORS_SERVER |
	List = _,
	To = DOORS_SERVER;

  Destination = DOORS_PLACE |
	List = _,
	To = DOORS_PLACE;

  Destination = DOORS_CONVERSATION |
	List = _,
	To = DOORS_CONVERSATION;

  otherwise |
	computation#display(term, ("Unknown destination : ", Destination),
							type(ground)),
	To = DOORS_PLACE, List = _.
	

list_to_struct(Term, Struct) :-
  list(Term) |
	list_to_tuple(Term, Struct);

  otherwise |
	Term = _,
	Struct = [].
	
convert_to_integer_set_to_max(Number, Number1) :-
	strings#convert_to_integer(Number, N),
	set_to_max_if_nil(N?, Number1).


FCP_MAX_INT => 33554431. % 2**25 - 1

set_to_max_if_nil(N, Number1) :-
  integer(N) |
	Number1 = N;

  otherwise |
	N = _,
	Number1 = FCP_MAX_INT.

convert_to_integer_report_error(Number, Number1) :-
	strings#convert_to_integer(Number, N),
	convert_to_integer_report_error1(N?, Number1).

convert_to_integer_report_error1(N, Number1) :-
  integer(N) |
	Number1 = N;

  otherwise |
	ok(false("Trying to convert to integer - set to 0"), N),
	Number1 = 0.
