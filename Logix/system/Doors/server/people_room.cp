/*  $Header:  */

-language([inherit, dfcp]).
-mode(interrupt).
-export([create, create1]).
-include([api_includes, server_includes, string_includes]).

/***************************************************************************
 *** people_room *** Manages cache of people information in server
****************************************************************************
*/
SCI_ROOM => [sci].

/* Fields in a person record :
 * PERSON_FULLNAME_ATTR - key (not in the record)
 * PERSON_TIMESTAMP_ATTR 
 * PERSON_ACCESS_DATE_ATTR
 * PERSON_NEXT_ATTR
 * PERSON_PREVIOUS_ATTR
 * PERSON_HAS_ICON_ATTR
 * PERSON_FACE_ATTR 
 * PERSON_DATA_ATTR
 * PERSON_WAITING_LIST_ATTR - list of PresenceId, ResponseType pairs
 * PERSON_OBJECTS_ATTR - list of Idcards with fields
	OBJECT_NAME_ATTR - key (in the record)
	OBJECT_TIMESTAMP_ATTR 
	OBJECT_FACE_ATTR
	OBJECT_WAITING_LIST_ATTR - list of PresenceId, ResponseType pairs
*/
 
create(IdCard,Door,Ok) :-
	+basic_room#create.

room(Rcv, Doors, Name, Type) + (PeopleCacheMax) :-
	+room_with_events#room_with_events;
	+initialize_application;
	+people_messages;
	+people_cache.

initialize_application(Rcv, PeopleCacheMax) :-
  Rcv ? initialize_application(Ok?) |
	PeopleCacheMax = _,
        Rcv'' = [([self] : show(id_card(self), IdCard, Ok1)) | Rcv'?],
        id_card#query_ok(IdCard?,
		 [(people_cache_max : PeopleCacheMax')], Ok2),
        and([Ok1?, Ok2?], Ok),
        self;

   Rcv ? (_From : make_snapshot(user_details, Snapshot?, Ok?)) |
        Snapshot=[],   
        Ok=true,
        self.

/* Messages received from the places_room about people or objects */
people_messages(Rcv) :-
  +person;
  +object;
  +query;
  +update.

person(Rcv) :-

/* PERSON ENTERED ROOM - UPDATE THE CACHE */
  Rcv ? (_ : person(PresenceId, FullName, HasFace, 
			TimeStamp, ObjectStamps, BusinessCard, Ok0)),
  string(PresenceId), string(FullName) |
	Rcv'' = [get_cache_element(FullName, Element, Ok1),
		 person1(Ok1?, PresenceId, FullName, HasFace, Element?, 
				TimeStamp, ObjectStamps, BusinessCard, Ok0)
	| Rcv'?],
	self;

  Rcv ? person1(Ok, PresenceId, FullName, HasFace, _E, 
			_TimeStamp, ObjectStamps, [], Ok0),
  Ok =\= true, string(FullName), constant(PresenceId), constant(HasFace)  |
	Rcv'' = [new_cache_element(FullName, HasFace),
		request_person(PresenceId, FullName, RequestType?, Ok0),
		request_object_faces(PresenceId, UserObjectNames?)
	| Rcv'?],
	make_user_object_names(FullName, HasFace, 
			ObjectStamps, RequestType, UserObjectNames),
	self;

  Rcv ? person1(Ok, PresenceId, FullName, HasFace, _E, 
			_TimeStamp, ObjectStamps, BusinessCard, Ok0),
  Ok =\= true, string(FullName), constant(PresenceId), constant(HasFace),
  BusinessCard =\= []  |
	Rcv'' = [initial_cache_idcard(FullName, IdCard),
		new_cache_element_with_idcard(FullName, IdCard'?),
		request_person(PresenceId, FullName, RequestType?, Ok0),
		request_object_faces(PresenceId, UserObjectNames?)
	| Rcv'?],
	make_user_object_names(FullName, HasFace, 
			ObjectStamps, RequestType, UserObjectNames),
	id_card#update(IdCard?, [(PERSON_DATA_ATTR : BusinessCard),
			 (PERSON_HAS_ICON_ATTR : HasFace)], IdCard'),
	self;

  Rcv  ? request_person(PresenceId, FullName, RequestType, Ok0),
  RequestType =\= [] |
        Rcv'' = [message(SCI_ROOM, [], 
                    doorsPropagateRequest(PresenceId, USER_REQUEST_CATEGORY,
                        RequestType, 
                        [{FullName, []}]), Ok1)
	| Rcv'?],
	Ok0 = true,
	ok(Ok1?, request_person),
	self;

  Rcv  ? request_person(_PresenceId, _FullName, [], Ok0) |
	Ok0 = true,
	self;

  Rcv  ? request_object_faces(PresenceId, UserObjectNames), 
  UserObjectNames =\= [], ground(UserObjectNames), string(PresenceId) |
	% only makes sense to ask gifs from the owner
	strings#extract(HASH, PresenceId, FullName, _),
	Rcv'' = [request_object_faces1(FullName?, PresenceId, UserObjectNames)
	| Rcv'?],
	self;


  Rcv  ? request_object_faces1(FullName, PresenceId, UserObjectNames), 
  UserObjectNames = [{FullName, _} | _], ground(UserObjectNames) |
%	computation#display(term, ("Requesting object faces" : UserObjectNames),
%		type(ground)),
        Rcv'' = [message(SCI_ROOM, [], 
                    doorsPropagateRequest(PresenceId, USER_REQUEST_CATEGORY,
                        USER_FACE_REQUEST, 
                        UserObjectNames), Ok1)
	| Rcv'?],
	ok(Ok1?, request_object_faces),
	self;

  Rcv  ? request_object_faces1(FullName, PresenceId, UserObjectNames), 
  UserObjectNames = {FullName, _}, ground(UserObjectNames) |
%	computation#display(term, ("Requesting object faces" : UserObjectNames),
%		type(ground)),
        Rcv'' = [message(SCI_ROOM, [], 
                    doorsPropagateRequest(PresenceId, USER_REQUEST_CATEGORY,
                        USER_FACE_REQUEST, 
                        UserObjectNames), Ok1)
	| Rcv'?],
	ok(Ok1?, request_object_faces),
	self;

  Rcv  ? request_object_faces1(FullName, _PresenceId, UserObjectNames), 
  UserObjectNames =\= {FullName, _}, UserObjectNames =\= [{FullName, _}| _] |
%	computation#display(term, ["Requesting object faces", UserObjectNames,
%		" From ", PresenceId], type(ground)),
	self;

  Rcv  ? request_object_faces(_PresenceId, []) |
	self;


  Rcv ? person1(true, PresenceId, FullName, HasFace, 
		Element, TimeStamp, ObjectStamps, BusinessCard, Ok0),
  string(FullName), constant(HasFace), constant(PresenceId)  | % Person Exists
	id_card#query(Element, [(PERSON_TIMESTAMP_ATTR : PersonTimeStamp),
				(PERSON_OBJECTS_ATTR : ObjectsData)]),
	listen2(ObjectsData?, ObjectsData1, ObjectsData2),
	Rcv'' = [touch_and_update_cache(FullName, 
			[(PERSON_HAS_ICON_ATTR: HasFace),
			 (PERSON_DATA_ATTR : BusinessCard)]),
		 request_person_if_needed(FullName, PresenceId, 
						RequestType?, Ok0),
		 request_objects_if_needed(PresenceId, UserObjectNames?,
			FullName, ObjectsData1?)
	| Rcv'?],
	compute_request_names(FullName, HasFace, TimeStamp, 
			PersonTimeStamp?, RequestType,
			ObjectStamps, ObjectsData2?,
			UserObjectNames),
	self;

  Rcv ? person1(true, PresenceId, FullName, HasFace, 
		Element, TimeStamp, ObjectStamps, [], Ok0),
  string(FullName), constant(HasFace), constant(PresenceId)  | % Person Exists
	id_card#query(Element, [(PERSON_TIMESTAMP_ATTR : PersonTimeStamp),
				(PERSON_OBJECTS_ATTR : ObjectsData)]),
	listen2(ObjectsData?, ObjectsData1, ObjectsData2),
	Rcv'' = [touch_and_update_cache(FullName, 
					[(PERSON_HAS_ICON_ATTR: HasFace)]),
		 request_person_if_needed(FullName, PresenceId, 
						RequestType?, Ok0),
		 request_objects_if_needed(PresenceId, UserObjectNames?,
			FullName, ObjectsData1?)
	| Rcv'?],
	compute_request_names(FullName, HasFace, TimeStamp, 
			PersonTimeStamp?, RequestType,
			ObjectStamps, ObjectsData2?,
			UserObjectNames),
	self;

  Rcv ? request_person_if_needed(_FullName, _PresenceId, [], Ok0) |
	Ok0 = true,
	self;

  Rcv ? request_person_if_needed(FullName, PresenceId, RequestType, Ok0), 
  RequestType =\= [], string(FullName), constant(RequestType) |
	set_person_request(RequestType, UpdateCard),
	Rcv'' = [update_cache(FullName, UpdateCard?),
		request_person(PresenceId, FullName, RequestType, Ok0)
	| Rcv'?],
	self;

  Rcv ? request_objects_if_needed(PresenceId, UserObjectNames, 
						FullName, ObjectsData), 
  UserObjectNames =\= [], string(FullName), ground(UserObjectNames) |
%	computation#display(term, UserObjectNames, type(ground)),
	set_object_request(UserObjectNames, ObjectsData, UpdateCard),
	Rcv'' = [update_cache(FullName, UpdateCard?), 
		request_object_faces(PresenceId, UserObjectNames)
	| Rcv'?],
	self;

  Rcv ? request_objects_if_needed(_PresenceId, [], _, _) |
	self.

% set requested field to undefined so that other clients can be put on waiting
% list
set_person_request(RequestType, UpdateCard) :-
  RequestType = USER_ALL_REQUEST |
	UpdateCard = [(PERSON_FACE_ATTR : undefined), 
				(PERSON_DATA_ATTR : undefined)];

  RequestType = USER_FACE_REQUEST |
	UpdateCard = [(PERSON_FACE_ATTR : undefined)];

  RequestType = USER_DATA_REQUEST |
	UpdateCard = [(PERSON_DATA_ATTR : undefined)];

  otherwise |
	RequestType = _,
	UpdateCard =  [].

set_object_request(UserObjectNames, ObjectsData, UpdateCard) :-
  UserObjectNames ? {_, ObjectName} |
	update_object(ObjectsData, ObjectName, 
		[(OBJECT_FACE_ATTR : undefined)], ObjectsData'),
	self;

  UserObjectNames = [] |
	UpdateCard = [(PERSON_OBJECTS_ATTR :ObjectsData)].

object(Rcv):-

/* OBJECT ADDED */
  Rcv ? (_ : object(PresenceId, FullName, ObjectName, TimeStamp)),
  string(PresenceId), string(FullName), constant(ObjectName), constant(TimeStamp) |
	Rcv'' = [get_cache_element(FullName, Element, Ok1),
		 object1(Ok1?, PresenceId, FullName, Element?, 
					ObjectName, TimeStamp)
	| Rcv'?],
	self;

  Rcv ? object1(Ok, PresenceId, FullName, _E, ObjectName, TimeStamp),
  Ok =\= true, string(FullName), constant(PresenceId)  |
	Rcv'' = [message(SCI_ROOM, [], 
		    doorsPropagateError(DOORS_INTERFACE_ERROR_CATEGORY,
			{CREATE_OBJECT_NO_PERSON_ERROR, []}, 
			ERROR_SEVERITY_ERROR, PresenceId, [],
			[FullName, ObjectName, TimeStamp], [PresenceId]), Ok1)
	| Rcv'?],
	ok(Ok1?, object1),
	self;

  Rcv ? object1(true, PresenceId, FullName, Element, ObjectName, TimeStamp),
  string(FullName), constant(ObjectName), constant(TimeStamp)   | % Person Exists
        id_card#query(Element, [(PERSON_OBJECTS_ATTR : ObjectsData)]),
	listen2(ObjectsData?, ObjectsData1, ObjectsData2),
        Rcv'' = [request_objects_if_needed(PresenceId, UserObjectNames?,
			FullName, ObjectsData1?)
        | Rcv'?],
%	computation#display(term, [ObjectName, TimeStamp, ObjectsData??,
%		UserObjectNames??], type(ground)),
        compute_object_names(FullName, [[ObjectName, TimeStamp]],
                       				ObjectsData2?, UserObjectNames),
        self.

query(Rcv):-

/* RESPOND TO QUERIES ABOUT PEOPLE  */  	
  Rcv ? (_ : respond_query_user_details(PresenceId, ClientData, RequestType,
						UserObjectNames)),
  constant(PresenceId), ground(RequestType), ground(ClientData),
  constant(ClientData), ground(UserObjectNames) |
%	computation#display(term, ("Responding with details about " :
%		UserObjectNames), type(ground)),
	get_response_type(RequestType, ResponseType),
	listen2(ResponseType?, ResponseType1, ResponseType2),
	Rcv'' = [respond_query_user_details1(PresenceId, 
			ResponseType1?, ClientData, UserObjectNames, Response),
		send_user_details_response(PresenceId, ClientData,
			ResponseType2?, Response?)
	| Rcv'?],
	self;

  Rcv ? send_user_details_response(PresenceId, ClientData,
			ResponseType, Response),
  Response =\= [] |
	Rcv'' = [message(SCI_ROOM, [], 
			doorsPropagateResponse([], PresenceId, ClientData, 
			USER_RESPONSE_CATEGORY,		
			ResponseType, Response), Ok)
	| Rcv'?],
	ok(Ok?, query_user_details),
	self;

  Rcv ? send_user_details_response(_PresenceId, _ClientData,
  			_ResponseType, []) |
%	computation#display(term, ["No response to send to ", PresenceId,
%		" with client data ", ClientData, "for category ",
%		USER_RESPONSE_CATEGORY, "response type ", ResponseType],
%		type(ground)),
	self;

  Rcv ? respond_query_user_details1(_, _, _, [], Response?) |
	Response = [],
	self;


  Rcv ? respond_query_user_details1(PresenceId,
			ResponseType, ClientData, UserObjectNames, Response?),
  constant(ResponseType), 
  ground(UserObjectNames), constant(PresenceId),
  UserObjectNames = [{FullName, ObjectName} | Rest], ground(ClientData) |
	Rcv'' = [get_cache_element(FullName, IdCard, Ok),
		 respond_query_user_details2(Ok?, PresenceId, IdCard?,
		 ResponseType, ClientData, FullName, ObjectName, 	
								ResponseHead),
		 respond_query_user_details1(PresenceId,  
			ResponseType, ClientData, Rest, ResponseTail),
		 compute_response(ResponseHead?, ResponseTail?, Response)
	| Rcv'?],
	self;

  Rcv ? compute_response([], ResponseTail, Response?) |
	Response = ResponseTail,
	self;

  Rcv ? compute_response(ResponseHead, ResponseTail, Response?),
  ResponseHead =\= [] |
	Response = [ResponseHead | ResponseTail],
	self;
	
  Rcv ? respond_query_user_details2(Error, PresenceId, _IdCard, 
	ResponseType,
	ClientData, FullName, ObjectName, Response?),
  Error =\= true, string(FullName), ObjectName = [], constant(PresenceId) |
	Rcv'' = [initial_cache_idcard(FullName, CacheIdCard),
		new_cache_element_with_idcard(FullName, CacheIdCard'?)
		 | Rcv'?],
	new_waiting_list_idcard(PresenceId, ObjectName, ResponseType,
				ClientData, CacheIdCard?, CacheIdCard'),
	Response = [],
	self;

  Rcv ? respond_query_user_details2(Error, PresenceId, _IdCard, 
	ResponseType,
	ClientData, FullName, ObjectName, Response?),
  Error =\= true, string(FullName), ObjectName = "",  constant(PresenceId) |
	Rcv'' = [initial_cache_idcard(FullName, CacheIdCard),
		new_cache_element_with_idcard(FullName, CacheIdCard'?)
		 | Rcv'?],
	new_waiting_list_idcard(PresenceId, ObjectName, ResponseType,
				ClientData, CacheIdCard?, CacheIdCard'),
	Response = [],
	self;

  Rcv ? respond_query_user_details2(Error, PresenceId, _IdCard, 
	ResponseType, ClientData, FullName, ObjectName, Response?),
  Error =\= true, string(FullName), string(ObjectName), ObjectName =\= "",
  constant(PresenceId) |
	Rcv'' = [initial_cache_idcard(FullName, CacheIdCard),
		new_cache_element_with_idcard(FullName, CacheIdCard'?),
		request_object_faces(PresenceId, [{FullName, ObjectName}])
		 | Rcv'?],
	new_waiting_list_idcard(PresenceId, ObjectName, ResponseType,
					ClientData, CacheIdCard?, CacheIdCard'),
	Response = [],
	self;


  Rcv ? respond_query_user_details2(true, PresenceId, IdCard, 
		ResponseType,ClientData, FullName, [], Response?),
  constant(ResponseType),  string(FullName), ground(IdCard) |
	get_person_info(FullName, IdCard, ResponseType, Response, NeedsUpdate),
	Rcv'' = [update_person_waiting_list(NeedsUpdate?, FullName, IdCard, 
			PresenceId, ResponseType, ClientData)
	| Rcv'?],
	self;

  Rcv ? respond_query_user_details2(true, PresenceId, IdCard, 
		ResponseType,ClientData, FullName, "", Response?),
  constant(ResponseType),  string(FullName), ground(IdCard) |
	get_person_info(FullName, IdCard, ResponseType, Response, NeedsUpdate),
	Rcv'' = [update_person_waiting_list(NeedsUpdate?, FullName, IdCard, 
			PresenceId, ResponseType, ClientData)
	| Rcv'?],
	self;

  Rcv ? update_person_waiting_list(NeedsUpdate, _FullName, _IdCard,
				 _PresenceId, _ResponseType, _ClientData),
  NeedsUpdate =\= true, NeedsUpdate =\= error |
	self;

  Rcv ? update_person_waiting_list(error, _FullName, _IdCard,
				 PresenceId, ErrorType, ClientData),
  string(PresenceId) |
	Rcv'' = [message(SCI_ROOM, [],
			doorsPropagateError(DOORS_INTERFACE_ERROR_CATEGORY,
			{ErrorType, ClientData}, ERROR_SEVERITY_ERROR, "",
			PresenceId, [], [PresenceId]), Ok1)
	| Rcv'?],
	ok(Ok1?, query_user_details_error),
	self;

  Rcv ? update_person_waiting_list(true, FullName, IdCard, PresenceId,
						ResponseType, ClientData),
  ground(IdCard), constant(FullName) |
	id_card#query(IdCard, [(PERSON_WAITING_LIST_ATTR : WaitingList)]),
	update_waiting_list(WaitingList?, PresenceId, ResponseType, ClientData,
								WaitingList'),
	id_card#update(IdCard, [(PERSON_WAITING_LIST_ATTR : WaitingList'?),
		(PERSON_PREVIOUS_ATTR : Previous?),
		(PERSON_NEXT_ATTR : Next?)], IdCard'),
	touch_element(IdCard'?, PERSON_ACCESS_DATE_ATTR, IdCard''),
	Rcv'' = [adjust_lru(add, FullName, Previous, Next),
		replace_cache(FullName, IdCard''?) | Rcv'?],
	self;
	
  Rcv ? respond_query_user_details2(true, PresenceId, IdCard, _ResponseType,
				ClientData, FullName, ObjectName, Response?),
  string(FullName), constant(ObjectName), ground(IdCard), ObjectName =\= "",
  ObjectName =\= [] |
	get_object_info(IdCard, FullName, ObjectName,
				Response, ObjectsData, NeedsUpdate), 
	Rcv'' = [update_object_in_cache(NeedsUpdate?, PresenceId, 
			FullName, ObjectName, ClientData, IdCard, ObjectsData?) 
	| Rcv'?],
	self;

  Rcv ? update_object_in_cache(true, PresenceId, FullName, ObjectName, 
			ClientData, IdCard, ObjectsData) |
	add_to_object_waiting_list(PresenceId, USER_FACE_RESPONSE, ClientData,
		ObjectsData, ObjectName, ObjectsData'),
	id_card#update(IdCard, 
			[(PERSON_OBJECTS_ATTR : ObjectsData'?)], IdCard'),
	Rcv'' = [replace_cache(FullName, IdCard'?) | Rcv'?],
	self;

  Rcv ? update_object_in_cache(NotNeeded, _, _, _, _, _, _),
  NotNeeded =\= true |
	self.

update(Rcv):-

/* UPDATE DATA ABOUT A PERSON AND/OR OBJECTS. ASSUMES HAS_FLACE_ATTR included 
 * SEND TO WAITING PEOPLE - CLEAR WAITING LIST 
*/

  Rcv ? (_ : update_user_details(UpdateCard)), ground(UpdateCard) |
	% Assumption: PERSON_TIMESTAMP_ATTR = OBJECT_TIMESTAMP_ATTR
	id_card#query(UpdateCard, [(PERSON_FULLNAME_ATTR : FullName), 		
		(PERSON_OBJECT_NAME_ATTR:ObjectName),
		(PERSON_TIMESTAMP_ATTR: TimeStamp)]),
	Rcv'' = [do_update_user_details(FullName?, ObjectName?, 
					TimeStamp?, UpdateCard) | Rcv'?],
	self;

  Rcv ? do_update_user_details(FullName, ObjectName, TimeStamp, UpdateCard),
  string(FullName), constant(ObjectName), ground(TimeStamp), ground(UpdateCard) |
	Rcv'' = [get_cache_element(FullName, IdCard, Ok1),
		 update_user_details1(Ok1?, FullName, ObjectName, UpdateCard,
			IdCard?),
		 event(user_details, [FullName, ObjectName, TimeStamp])
	| Rcv'?],
	self;

  Rcv ? do_update_user_details(_FullName, [], [], _UpdateCard) |
	self;

  Rcv ? update_user_details1(Error, FullName, ObjectName, UpdateCard, _),
  Error =\= true, ground(UpdateCard), string(FullName) |
	Rcv'' = [initial_cache_idcard(FullName, InitialCard),
		 new_cache_element_with_idcard(FullName, IdCard?)
	| Rcv'?],
	update_user_idcard(InitialCard?, UpdateCard, FullName,
							ObjectName, IdCard),
	self;

  
 Rcv ? update_user_details1(true, FullName, ObjectName, 
							UpdateCard, IdCard),
  string(FullName) |
	update_user_idcard(IdCard, UpdateCard, FullName,
						 ObjectName, UpdatedCard),
 	Rcv'' = [replace_cache(FullName, UpdatedCard?) | Rcv'?],
	self.


/* The people cache is implemented as an attribute value table,
 * keyed by a person's FullName inside a system hash table, and with
 * an LRU chain handled by "previous" and "next" link attributes
*/
people_cache(Rcv, PeopleCacheMax) +
 (PeopleCache, Count, Oldest, Newest, S) :-

  initially |
	Count = 0,
	Oldest = "", Newest = "",
	stream#hash_table(S?);

/* CACHE PROCESSING */ 

  Rcv ? initial_cache_idcard(FullName, IdCard?), constant(FullName) |
	processor#interface(gmtime(Date)),
	Rcv'' = [adjust_lru(add, FullName, Previous, Next) | Rcv'?],
	IdCard = [(PERSON_FULLNAME_ATTR : FullName),
		(PERSON_TIMESTAMP_ATTR : []),
		(PERSON_ACCESS_DATE_ATTR : Date?),
		(PERSON_OBJECTS_ATTR : []),
		(PERSON_PREVIOUS_ATTR : Previous?), (PERSON_NEXT_ATTR : Next?),
		(PERSON_WAITING_LIST_ATTR : [])],
	self;

  Rcv ? new_cache_element(FullName, HasFace), string(FullName) |
	Rcv'' = [initial_cache_idcard(FullName, IdCard),
		new_cache_element_with_idcard(FullName, IdCard'?) | Rcv'?],
	id_card#update(IdCard?, [(PERSON_HAS_ICON_ATTR:HasFace)], IdCard'),
	self;

   Rcv ? new_cache_element_with_idcard(FullName, IdCard), string(FullName) |
	S ! lookup(FullName, IdCard, _, Reply),
	Rcv'' = [new_cache_element1(Reply?, FullName) | Rcv'?],
	self;

  Rcv ? new_cache_element1(new, _) |
	self;

  Rcv ? new_cache_element1(Old, FullName), Old =\= new |
	computation#display(term, ("hash table lookup found unexpected element":
		FullName), type(ground)),
	self;

  Rcv ? touch_and_update_cache(FullName, UpdateCard) |
	touch_element(UpdateCard, PERSON_ACCESS_DATE_ATTR, UpdateCard'),
	Rcv'' = [update_cache(FullName, UpdateCard'?) | Rcv'?],
	self;

  Rcv ? update_cache(FullName, UpdateCard), 
  ground(UpdateCard), string(FullName) |
	Rcv'' = [get_cache_element(FullName, IdCard, Ok),
		 update_cache1(Ok?, FullName, IdCard?, UpdateCard) 
	| Rcv'?],
	self;
  
  Rcv ? update_cache1(Error, FullName, _IdCard, _UpdateCard), Error =\= true |
	ok(Error, update_cache1(FullName)),
	self;

  Rcv ? update_cache1(true, FullName, IdCard, UpdateCard) |
	id_card#update(IdCard, UpdateCard, IdCard'),
	Rcv'' = [replace_cache(FullName, IdCard'?) | Rcv'?],
	self;

  Rcv ? replace_cache(FullName, IdCard) |
	S ! replace(FullName, IdCard, _, Ok),
	ok(Ok?, replace_cache),
	self;

  Rcv ? get_cache_element(FullName, IdCard, Ok?) |
	S ! member(FullName, IdCard, Ok),
	self;

  Rcv ? (_ : get_cache_element(FullName, IdCard, Ok?)) | % drive from outside
	S ! member(FullName, IdCard, Ok),
	self;

  Rcv ? (_ : dump_cache) |
	S ! entries(Entries),
	dump_cache(Entries?),
	self;

  Rcv ? delete_cache(FullName) |
	S ! delete(FullName, _, Ok),
	ok(Ok?, delete_cache),
	self;

/* LRU PROCESSING */
  Rcv ? adjust_lru(add, Key, Previous?, Next?), 
  ground(Key), constant(Count) |
/*	computation#display(term, ["adjust_lru add key ", Key,
		" [previous = ", Previous??, " next ", Next??,
		" count ", Count], type(ground)),
*/
	Rcv'' = [adjust_previous_if_needed(Key),
		add_newest(Key, Previous, Next),
		adjust_oldest_if_needed(Key),
		delete_lru_if_overflow
	| Rcv'?],
	self;

  Rcv ? adjust_oldest_if_needed(Key), Oldest = "" |
	Oldest' = Key,
	self;

  Rcv ? adjust_oldest_if_needed(_Key), Oldest =\= "" |
	self;

  Rcv ? adjust_previous_if_needed(Key), string(Newest), Newest =\= "" |
	Rcv'' = [get_cache_element(Newest, IdCard, Ok1),   %previous newest
		 update_cache(Newest, IdCard'?) | Rcv'?],
	id_card#update(IdCard?, [(PERSON_NEXT_ATTR : Key)], IdCard'),
	ok(Ok1?, add_newest),
	self;

  Rcv ? adjust_previous_if_needed(_Key), Newest = "" |
	self;

  Rcv ? add_newest(Key, Previous?, Next?), constant(Key) |	
	Previous = Newest,
	Newest' = Key,
	Next = "",
	Count++,
	self;

  Rcv ? delete_lru_if_overflow, Count =< PeopleCacheMax |
	self;

  Rcv ? delete_lru_if_overflow,
  string(Oldest), PeopleCacheMax < Count |
	Rcv'' = [get_cache_element(Oldest, IdCard, Ok),
		 delete_lru_if_overflow1(Ok?, Oldest, IdCard?)
	| Rcv'?],
/*	computation#display(term, ["want to delete oldest : ", IdCard??],
							 type(ground)),
*/
	self;

  Rcv ? delete_lru_if_overflow1(Error, Oldest, _ ), Error =\= true |
	computation#display(term, ("Server software error - Cannot find Oldest person in the cache " :
		Oldest), type(ground)),
	self;

  Rcv ? delete_lru_if_overflow1(true, _, IdCard), ground(IdCard) |
	id_card#query(IdCard, [(PERSON_PREVIOUS_ATTR : Previous), 
		(PERSON_NEXT_ATTR : Next), (PERSON_WAITING_LIST_ATTR : W),
		(PERSON_ACCESS_DATE_ATTR : AccessDate),
		(PERSON_TIMESTAMP_ATTR : T)]),
	set_delete_flag(W?, T?, AccessDate?, Delete),
	Rcv'' = [delete_lru_if_overflow2(Previous?, Next?, Delete?) | Rcv'?],
/*	computation#display(term, ["previous = ", Previous??, " next = ", 
		Next??], type(ground)),
*/
	self;

  Rcv ? delete_lru_if_overflow2(Previous, _Next, _W), Previous =\= "",
  string(Oldest) |
	computation#display(term, ("Server software error in person cache lru  - previous field of oldest member not set properly " : 
		Previous(Oldest)), type(ground)),
	self;
	
  Rcv ? delete_lru_if_overflow2(_, "", _) |
	computation#display("attempt to delete the last cached person ignored!"),
	self;

  Rcv ? delete_lru_if_overflow2(_, _, false) |
	/* don't delete - let cache grow */
	self;

  Rcv ? delete_lru_if_overflow2(Previous, Next, true), Previous = "",
  string(Next), Next =\= "" |
	Rcv'' = [get_cache_element(Next, IdCard, Ok),
		 delete_lru_if_overflow3(Ok?, IdCard?, Next),
		 delete_lru_if_overflow |  % clean up more if we can
	Rcv'?],
/*	computation#display(term, ["in overflow2, next = ", Next, " idcard = ",
		IdCard??], type(ground)),
*/
	self;

  Rcv ? delete_lru_if_overflow3(Error, _, Person), Error =\= true |
	computation#display(term, ("Server software error - Can't find person in cache " : Person),
		type(ground)),
	self;

  Rcv ? delete_lru_if_overflow3(true, IdCard, Current), 
  string(Current), ground(IdCard), string(Oldest) |
	id_card#update(IdCard, [(PERSON_PREVIOUS_ATTR : "")], IdCard'),
	Oldest' = Current, 
	Count--,
	Rcv'' = [update_cache(Current, IdCard'?),
		 delete_cache(Oldest)
	| Rcv'?],
/*	computation#display(term, ["current = ", Current, " IdCard = ",
		IdCard, " IdCard' = ", IdCard'??], type(ground)),
*/
	self;
  
/* FINALIZATION */
  finally |
	S = [].


set_delete_flag(WaitingList, TimeStamp, AccessDate, Delete) :-
  list(WaitingList) |
	Delete = false, % don't delete if there are people on the waiting list
	TimeStamp = _,
	AccessDate = _;

  TimeStamp = [] |
	WaitingList= _, AccessDate = _,
	Delete = false; % don't delete someone just inserted 

  otherwise |
	set_delete_flag1(AccessDate, Delete),
	TimeStamp = _,
	WaitingList= _.

set_delete_flag1(AccessDate, Delete) :-
	processor#interface(gmtime(Date)),
	strings#date_diff_bigger(Date?, AccessDate, 
				MINIMUM_TIME_IN_CACHE, Delete).
	
make_user_object_names(FullName, HasFace, ObjectStamps, 
			RequestType, UserObjectNames) :-
  HasFace =\= HAS_ICON_TRUE | % this user has no face
	RequestType = [],
	make_object_names(FullName, ObjectStamps, UserObjectNames);

  HasFace = HAS_ICON_TRUE, string(FullName) |
	RequestType = USER_FACE_REQUEST,
	make_object_names(FullName, ObjectStamps, UserObjectNames).


% assumes that no object names with empty details date are requested
make_object_names(FullName, ObjectStamps, UserObjectNames) :-
  string(FullName), ObjectStamps ? [ObjectName | _] |
	UserObjectNames ! {FullName, ObjectName},
	self;

  ObjectStamps = [] |
	FullName = _,
	UserObjectNames = [].

compute_request_names(FullName, HasFace, TimeStamp, 
  PersonTimeStamp, RequestType, ObjectStamps, ObjectData, UserObjectNames) :-
  string(FullName) |
	compute_person_request(HasFace,TimeStamp,PersonTimeStamp, RequestType),	
	compute_object_names(FullName, ObjectStamps,
					 ObjectData, UserObjectNames).

compute_person_request(HasFace, TimeStamp, PersonTimeStamp, RequestType) :-
  string(TimeStamp), string(PersonTimeStamp) |
	compute_person_request1(HasFace, TimeStamp, PersonTimeStamp, 
								RequestType);

  string(TimeStamp), PersonTimeStamp = [] |
	compute_person_request1(HasFace, TimeStamp, "", RequestType);

  TimeStamp = [], string(PersonTimeStamp) |
	compute_person_request1(HasFace, "", PersonTimeStamp, RequestType);

  TimeStamp = [], PersonTimeStamp = [] |
	HasFace = _,
	RequestType = [].

compute_person_request1(HasFace, TimeStamp, PersonTimeStamp, RequestType) :-
  TimeStamp =  PersonTimeStamp |
	HasFace = _,
	RequestType = [];

  TimeStamp @< PersonTimeStamp |
	HasFace = _,
	RequestType = [];

  PersonTimeStamp @< TimeStamp, HasFace = HAS_ICON_TRUE |
	RequestType = USER_FACE_REQUEST;
	
  PersonTimeStamp @< TimeStamp, HasFace =\= HAS_ICON_TRUE |
	RequestType = [].

compute_object_names(FullName, ObjectStamps, ObjectData, UserObjectNames) :-
  ObjectStamps = [] |
	UserObjectNames = [],
	FullName = _,
	ObjectData = _;

  ObjectStamps ? [ObjectName, TimeStamp], 
  constant(ObjectName), constant(TimeStamp), ground(ObjectData),      
  string(FullName), ObjectName =\= [] |
	find_object(ObjectData, ObjectName, ObjectIdCard),
	id_card#query(ObjectIdCard?,
		 [(OBJECT_TIMESTAMP_ATTR : ObjectTimeStamp)]),
	include_object_if_needed(FullName, ObjectName, TimeStamp, 
		ObjectTimeStamp?, UserObjectNames, UserObjectNames'?),
	self;

   ObjectStamps ? [[], _TimeStamp] |
	UserObjectNames = UserObjectNames'?,
	self.

  
find_object(ObjectData, ObjectName, ObjectIdCard) :-
  ObjectData ? IdCard, ground(IdCard) |
	id_card#query(IdCard, [(OBJECT_NAME_ATTR : Name)]),
	find_object1(ObjectData', ObjectName, Name?, IdCard, ObjectIdCard);

  ObjectData = [] |
	ObjectName = _,
	ObjectIdCard = [];

  ObjectData = undefined |
	ObjectName = _,
	ObjectIdCard = [].

find_object1(ObjectData, ObjectName, Name, IdCard, ObjectIdCard) :-
  ObjectName = Name |
	ObjectIdCard = IdCard,
	ObjectData = _;

  ObjectName =\= Name |
	IdCard = _,
	find_object(ObjectData, ObjectName, ObjectIdCard).

include_object_if_needed(FullName, ObjectName, TimeStamp, ObjectTimeStamp,
		UserObjectNames, UserObjectNames1) :-

  ObjectTimeStamp = undefined, TimeStamp =\= [] |
	TimeStamp = _,
	UserObjectNames = [{FullName, ObjectName} | UserObjectNames1];

  ObjectTimeStamp = "" , TimeStamp =\= [] |
	TimeStamp = _,
	UserObjectNames = [{FullName, ObjectName} | UserObjectNames1];

  ObjectTimeStamp = [], TimeStamp =\= []|
	TimeStamp = _,
	UserObjectNames = [{FullName, ObjectName} | UserObjectNames1];

  ObjectTimeStamp =\= undefined, ObjectTimeStamp =\= "",ObjectTimeStamp =\= [],
  TimeStamp =\= [],
  ObjectTimeStamp @< TimeStamp |
	UserObjectNames = [{FullName, ObjectName} | UserObjectNames1];

  ObjectTimeStamp =\= undefined, ObjectTimeStamp =\= "",ObjectTimeStamp =\= [],
  TimeStamp @<  ObjectTimeStamp |
	UserObjectNames = UserObjectNames1,
	FullName = _, ObjectName = _,
	TimeStamp = _;

  ObjectTimeStamp =\= undefined, ObjectTimeStamp =\= "",ObjectTimeStamp =\= [],
  TimeStamp =  ObjectTimeStamp |
	UserObjectNames = UserObjectNames1,
	FullName = _, ObjectName = _,
	TimeStamp = _;

  TimeStamp = [] |
	UserObjectNames = UserObjectNames1,
	FullName = _, ObjectName = _,
	ObjectTimeStamp = _.

get_response_type(RequestType, ResponseType) :-
  RequestType = USER_ALL_REQUEST |
	ResponseType = USER_ALL_RESPONSE;

  RequestType = USER_DATA_REQUEST |
	ResponseType = USER_DATA_RESPONSE;

  RequestType = USER_FACE_REQUEST |
	ResponseType = USER_FACE_RESPONSE.

new_waiting_list_idcard(PresenceId, ObjectName, ResponseType, ClientData,
					CacheIdCard, CacheIdCard1) :-
  ObjectName = "" |
	UpdateCard = [(PERSON_WAITING_LIST_ATTR :
		[[PresenceId, ResponseType, ClientData]])],
	id_card#update(CacheIdCard, UpdateCard?, CacheIdCard1);

  ObjectName = [] |
	UpdateCard = [(PERSON_WAITING_LIST_ATTR :
		[[PresenceId, ResponseType, ClientData]])],
	id_card#update(CacheIdCard, UpdateCard?, CacheIdCard1);

  ObjectName =\= "", ObjectName =\= [] |
	UpdateCard = [(PERSON_OBJECTS_ATTR :
			 [(OBJECT_NAME_ATTR : ObjectName),
			  (OBJECT_WAITING_LIST_ATTR :
				 [[PresenceId, ResponseType, ClientData]])])],
	id_card#update(CacheIdCard, UpdateCard?, CacheIdCard1).

get_person_info(FullName, IdCard, ResponseType, Info, NeedsUpdate) :-
  ResponseType = USER_ALL_RESPONSE |
	id_card#query(IdCard, [(PERSON_TIMESTAMP_ATTR:DetailsDate),
				(PERSON_HAS_ICON_ATTR: HasFace),
				(PERSON_FACE_ATTR : Face), 
				(PERSON_DATA_ATTR : Data)]),
	get_person_info_all(FullName, DetailsDate?, HasFace?, Face?, Data?, 
							NeedsUpdate, Info);

  ResponseType = USER_FACE_RESPONSE |
	id_card#query(IdCard, [(PERSON_TIMESTAMP_ATTR:DetailsDate),
				(PERSON_HAS_ICON_ATTR: HasFace),
				(PERSON_FACE_ATTR : Face)]),
	get_person_info_face(FullName, DetailsDate?, HasFace?, Face?, 
							NeedsUpdate, Info);

  ResponseType = USER_DATA_RESPONSE |
	id_card#query(IdCard, [(PERSON_TIMESTAMP_ATTR:DetailsDate),
				(PERSON_DATA_ATTR : Data)]),
	get_person_info_data(FullName, DetailsDate?, Data?, NeedsUpdate, Info).

get_person_info_all(FullName, DetailsDate, HasFace, Face, Data, 
							NeedsUpdate, Info):-
  Face =\= undefined, Data =\= undefined, HasFace = HAS_ICON_TRUE | 
	NeedsUpdate = false,
	Info = {DetailsDate, FullName, Data, Face};

  HasFace =\= HAS_ICON_TRUE, Data =\= undefined |
	Face = _,
	NeedsUpdate = false,
	Info = {DetailsDate, FullName, Data, []};

  Data = undefined |
	Face = _, HasFace = _, FullName = _, DetailsDate = _, 
	NeedsUpdate = true,
	Info = [];

  HasFace = HAS_ICON_TRUE, Face = undefined |
	Data = _, FullName = _, DetailsDate = _,
	NeedsUpdate = true,
	Info = [].
  
get_person_info_face(FullName, DetailsDate, HasFace, Face, NeedsUpdate, Info) :-
  HasFace =\= HAS_ICON_TRUE |
	NeedsUpdate = false,
	Info = {DetailsDate, FullName, [], []},
	Face = _;

  HasFace = HAS_ICON_TRUE, Face = undefined |
	NeedsUpdate = true, DetailsDate = _, FullName = _,
	Info = [];

   HasFace = HAS_ICON_TRUE, Face =\= undefined |
	NeedsUpdate = false,
	Info = {DetailsDate, FullName, [], Face}.

get_person_info_data(FullName, DetailsDate, Data, NeedsUpdate, Info) :-
  Data = undefined |
	NeedsUpdate = true, DetailsDate = _, FullName = _,
	Info = [];

  Data =\= undefined |
	NeedsUpdate = false, DetailsDate = _, 
	Info = {FullName, Data}.

% NOTE : if objects get data, this will have to be updated
%	Also, assume that if object is cached it indeed has a face - so no flag
get_object_info(IdCard, FullName, ObjectName, 
	Response, ObjectsData, ObjectNeedsUpdate) :-
 
  ObjectName =\= "", string(ObjectName) |
	id_card#query(IdCard, [(PERSON_OBJECTS_ATTR : O)]),
	listen2(O?, ObjectsData, O1),
	get_object_info1(O1?, FullName, ObjectName,
				 Response, ObjectNeedsUpdate).

get_object_info1(ObjectsData, FullName, ObjectName, 
					Response, ObjectNeedsUpdate) :-
  string(ObjectName) |
	find_object(ObjectsData, ObjectName, ObjectIdCard),
	id_card#query(ObjectIdCard?, [(OBJECT_TIMESTAMP_ATTR : DetailsDate), 
					(OBJECT_FACE_ATTR : Face)]),
	get_object_info2(DetailsDate?, Face?, Response, FullName, 
					ObjectName, ObjectNeedsUpdate).

get_object_info2(DetailsDate, Face, Response, FullName, ObjectName,
							 ObjectNeedsUpdate):-
  Face = undefined |
	FullName = _,
	ObjectName = _, DetailsDate = _,
	Response = [],
	ObjectNeedsUpdate = true;

  Face =\= undefined  |
	Response = {DetailsDate, FullName, ObjectName, Face},
	ObjectNeedsUpdate = false.

  
add_to_object_waiting_list(PresenceId, ResponseType, ClientData,
				ObjectsData, ObjectName, ObjectsData1) :-

  ground(ObjectsData), string(ObjectName) |
	find_object(ObjectsData, ObjectName, ObjectIdCard),
	id_card#query(ObjectIdCard?, [(OBJECT_WAITING_LIST_ATTR:WaitingList)]),
	update_waiting_list(WaitingList?, PresenceId, ResponseType,
						ClientData, WaitingList'), 
	update_object(ObjectsData, ObjectName, 
		[(OBJECT_WAITING_LIST_ATTR:WaitingList'?)], ObjectsData1).

update_object(ObjectsData, ObjectName, UpdateCard, UpdatedObjectsData)
  +(Found = false) :-
  ObjectsData ? IdCard, ground(IdCard), ground(UpdateCard), constant(ObjectName) |
        id_card#query(IdCard, [(OBJECT_NAME_ATTR : Name)]),
	UpdatedObjectsData ! UpdatedIdCard?,
        update_object1(ObjectName, Name?, IdCard, 
			UpdateCard, UpdatedIdCard, Found, Found'),
	self;

  ObjectsData = [], Found = true |
        ObjectName = _, UpdateCard = _,
        UpdatedObjectsData = [];

  ObjectsData = [], Found = false |
	id_card#update([(OBJECT_NAME_ATTR : ObjectName)], 
				UpdateCard, NewObjectCard),
	UpdatedObjectsData = [NewObjectCard?].

update_object1(ObjectName, Name, IdCard, UpdateCard, UpdatedIdCard, 
							Found, Found1) :-
  ObjectName = Name |
	Found1= true, Found = _,
        id_card#update(IdCard, UpdateCard, UpdatedIdCard);
        
  ObjectName =\= Name |
	Found1 = Found,
        UpdatedIdCard = IdCard,
        UpdateCard = _.

update_waiting_list(WaitingList, PresenceId, 
				ResponseType, ClientData, WaitingList1) :-
  WaitingList = [] |
	WaitingList1 = [[PresenceId, ResponseType, ClientData]];

   WaitingList = undefined |
	WaitingList1 = [[PresenceId, ResponseType, ClientData]];

% unfair but cheaper than append
  WaitingList = [A | B] |
	WaitingList1 = [[PresenceId, ResponseType, ClientData], A | B]. 					    

touch_element(IdCard, Attribute, IdCard1) :-
	processor#interface(gmtime(Date)),
	id_card#update(IdCard, [(Attribute: Date?)], IdCard1).

update_user_idcard(InitialCard, UpdateCard, FullName, ObjectName, IdCard) :-
  ground(InitialCard), ground(UpdateCard), ObjectName = "" |
	id_card#query(InitialCard,
		 [(PERSON_HAS_ICON_ATTR : HasFace),
			(PERSON_WAITING_LIST_ATTR : WaitingList)]),
	touch_element(UpdateCard, PERSON_ACCESS_DATE_ATTR, UpdateCard1),
	id_card#update(UpdateCard1?, 
		[(PERSON_WAITING_LIST_ATTR : NewWaitingList?)], UpdateCard2),
	id_card#update(InitialCard, UpdateCard2?, IdCard),
	propogate_to_waiting_list(WaitingList?, FullName, HasFace?,
			"", UpdateCard, NewWaitingList);

  ground(InitialCard), ground(UpdateCard), ObjectName = [] |
	id_card#query(InitialCard,
		 [(PERSON_HAS_ICON_ATTR : HasFace),
			(PERSON_WAITING_LIST_ATTR : WaitingList)]),
	touch_element(UpdateCard, PERSON_ACCESS_DATE_ATTR, UpdateCard1),
	id_card#update(UpdateCard1?, 
		[(PERSON_WAITING_LIST_ATTR : NewWaitingList?)], UpdateCard2),
	id_card#update(InitialCard, UpdateCard2?, IdCard),
	propogate_to_waiting_list(WaitingList?, FullName, HasFace?,
			"", UpdateCard, NewWaitingList);


  ground(InitialCard), ground(UpdateCard),ObjectName =\= "",string(ObjectName) |
	touch_element(InitialCard, PERSON_ACCESS_DATE_ATTR, InitialCard'),
	id_card#query(InitialCard, [(PERSON_OBJECTS_ATTR :ObjectsData)]),
	listen2(ObjectsData?, O1, O2),
	find_object(O1?, ObjectName, ObjectIdCard),
	id_card#query_update(ObjectIdCard?, 
		[(PERSON_WAITING_LIST_ATTR : [])], ObjectIdCard', OldCard),
	id_card#query(OldCard?, [(PERSON_WAITING_LIST_ATTR : WaitingList)]),
	id_card#update(ObjectIdCard'?, UpdateCard, ObjectIdCard''),
	update_object(O2?, ObjectName, ObjectIdCard''?, UpdatedObjectsData),
	id_card#update(InitialCard'?,
			[(PERSON_OBJECTS_ATTR :UpdatedObjectsData?)], IdCard),
	propogate_to_waiting_list(WaitingList?, FullName, HAS_ICON_TRUE,
					ObjectName, UpdateCard, _).
  

propogate_to_waiting_list(WaitingList, FullName, HasFace,
	ObjectName, UpdateCard, NewWaitingList) :-
  ObjectName =\= "", ObjectName =\= [] |
	NewWaitingList = [], HasFace = _,
	propogate_object_update(FullName, ObjectName, UpdateCard, WaitingList);

  ObjectName = [] |
	propogate_person_update(FullName, HasFace, UpdateCard, 
						WaitingList, NewWaitingList);
  ObjectName = "" |
	propogate_person_update(FullName, HasFace, UpdateCard, 
						WaitingList, NewWaitingList).

propogate_person_update(FullName, HasFace,
			UpdateCard, WaitingList, NewWaitingList) :-
  WaitingList = undefined |
	NewWaitingList = [],
	FullName = _, HasFace = _, UpdateCard = _;

  WaitingList = [] |
	NewWaitingList = [],
	FullName = _, HasFace = _, UpdateCard = _;

  WaitingList ? W, ground(UpdateCard), string(FullName), constant(HasFace) |
	id_card#query(UpdateCard, [
				(PERSON_TIMESTAMP_ATTR : DetailsDate),
				(PERSON_FACE_ATTR : Face), 
				(PERSON_DATA_ATTR : Data)]),
	propogate_person_update1(W, FullName, HasFace,
		 Face?, Data?, DetailsDate?, NewWaitingList, NewWaitingList'?),
	self.

propogate_person_update1(WaitingPresence, FullName, HasFace, Face, Data, 
				 DetailsDate, NewWaitingList, NewWaitingList1):-
  WaitingPresence = [PresenceId, USER_DATA_RESPONSE, ClientData],
  Data =\= undefined |
	NewWaitingList = NewWaitingList1,
	Face = _, HasFace = _,
	propogate(PresenceId, USER_DATA_RESPONSE, ClientData,
		{DetailsDate, FullName, Data});

  WaitingPresence = [_PresenceId, USER_DATA_RESPONSE, _ClientData],
  Data = undefined |
	Face = _, FullName = _, HasFace = _, DetailsDate = _, 
	NewWaitingList = [WaitingPresence | NewWaitingList1];

  WaitingPresence = [PresenceId, USER_FACE_RESPONSE, ClientData],
  Face =\= undefined, string(Face), HasFace = HAS_ICON_TRUE |
	NewWaitingList = NewWaitingList1,
	Data = _,
	propogate(PresenceId, USER_FACE_RESPONSE, ClientData, 
		{DetailsDate, FullName, [], Face});

  WaitingPresence = [_PresenceId, USER_FACE_RESPONSE, _ClientData],
  Face = undefined |
	Data = _, FullName = _, HasFace = _, DetailsDate = _, 
	NewWaitingList = [WaitingPresence | NewWaitingList1];

  WaitingPresence = [_PresenceId, USER_FACE_RESPONSE, _ClientData],
  HasFace = HAS_ICON_FALSE |
	Data = _, FullName = _, Face = _, DetailsDate = _, 
	NewWaitingList = [WaitingPresence | NewWaitingList1];

  WaitingPresence = [PresenceId, USER_FACE_RESPONSE, ClientData],
  Face =\= undefined, string(Face), 
  HasFace =\= HAS_ICON_TRUE, HasFace =\= HAS_ICON_FALSE |
	computation#display(term, ("Server software error - Face exists but HasIcon flag is ": HasFace),
		type(ground)),
	NewWaitingList = NewWaitingList1,
	Data = _,
	propogate(PresenceId, USER_FACE_RESPONSE, ClientData, 
		{DetailsDate, FullName, [], Face});

  WaitingPresence = [_PresenceId, USER_ALL_RESPONSE, _ClientData],
  Face = undefined |
	FullName = _, Data = _, HasFace = _, DetailsDate = _, 
	NewWaitingList = [WaitingPresence | NewWaitingList1];

  WaitingPresence = [_PresenceId, USER_ALL_RESPONSE, _ClientData],
  Data = undefined |
	FullName = _, Face = _, HasFace = _, DetailsDate = _, 
	NewWaitingList = [WaitingPresence | NewWaitingList1];

  WaitingPresence = [PresenceId, USER_ALL_RESPONSE, ClientData],
  Data =\= undefined, Face =\= undefined, HasFace = HAS_ICON_TRUE |
	NewWaitingList = NewWaitingList1,
	propogate(PresenceId,  USER_ALL_RESPONSE, ClientData, 
		{DetailsDate, FullName, Data, Face});

  WaitingPresence = [PresenceId, USER_ALL_RESPONSE, ClientData],
  Data =\= undefined, Face =\= undefined, 
  HasFace =\=  HAS_ICON_TRUE, HasFace =\= HAS_ICON_FALSE |
	computation#display(term, ("Server software error - Face exists but HasIcon flag is ": HasFace),
		type(ground)),
	NewWaitingList = NewWaitingList1,
	propogate(PresenceId,  USER_ALL_RESPONSE, ClientData, 
		{DetailsDate, FullName, Data, Face});

  WaitingPresence = [PresenceId, USER_ALL_RESPONSE, ClientData],
  Data =\= undefined, Face =\= undefined, HasFace = HAS_ICON_FALSE |
	Face = _,
	NewWaitingList = NewWaitingList1,
	propogate(PresenceId, USER_ALL_RESPONSE, ClientData,
		 {DetailsDate, FullName, Data, []}).


propogate_object_update(FullName, ObjectName, UpdateCard, WaitingList):-
  WaitingList = [] |
        FullName = _, ObjectName = _, UpdateCard = _;

 WaitingList = undefined |
        FullName = _, ObjectName = _, UpdateCard = _;

  WaitingList ? W, string(FullName), constant(ObjectName), ground(UpdateCard) |
        id_card#query(UpdateCard, [(PERSON_TIMESTAMP_ATTR : DetailsDate), 
					(PERSON_FACE_ATTR : Face)]),
        propogate_object_update1(W, DetailsDate?, FullName, ObjectName, Face?),
        self.

propogate_object_update1(WaitingPresence, DetailsDate,
					FullName, ObjectName, Face) :-
  WaitingPresence = [PresenceId, USER_FACE_RESPONSE, ClientData], Face = "" |
	computation#display("server error: update for object with empty face"),
	FullName = _, ObjectName = _, ClientData = _, PresenceId = _,
	DetailsDate = _;

  WaitingPresence = [PresenceId, USER_FACE_RESPONSE, ClientData], 
  Face = undefined |
	computation#display(
		"server error: update for object with undefined face"),
	FullName = _, ObjectName = _, ClientData = _, PresenceId = _,
 	DetailsDate = _;

  WaitingPresence = [PresenceId, USER_FACE_RESPONSE, ClientData],
  Face =\= "", Face =\= undefined |
	propogate(PresenceId, USER_FACE_RESPONSE, ClientData,
		 {DetailsDate, FullName, ObjectName, Face}).

propogate(PresenceId, ResponseType, ClientData, Info):-
	processor#room(message([sci], [],
		doorsPropagateResponse([], PresenceId, ClientData, 
                        USER_RESPONSE_CATEGORY,         
                        ResponseType, [Info]), Ok1), Ok2),
	ok([Ok1?, Ok2?], propogate).

dump_cache(Entries) + (Done = true) :-
  Entries ? entry(Name, IdCard) |
	computation#display(term, ("Person " : Name), 
		[close(Done, Done'), type(ground)]),
	computation#display(term, IdCard, 
		[close(Done'?, Done''), type(ground)]),
	self;

  Entries = [] |
	Done = _.

