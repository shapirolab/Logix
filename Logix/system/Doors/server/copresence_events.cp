
/* $Header: /home/qiana/Repository/Logix/system/Doors/server/copresence_events.cp,v 1.1 1999/07/09 07:03:29 bill Exp $ */
-language([inherit, dfcp]).
-mode(interrupt).
-export(all).
-include([api_includes, server_includes, string_includes]).

/* copresence_events is responsible for intercepting doors events,
 * logging them, preparing them for further processing,
 * and for managing the event cache and listeners.
*/

copresence_events(Rcv, URL, EventsCacheMax) :-
+messages;
+cache;
+update_events_table.

messages(Rcv, URL) :-

  Rcv ? (_ : view_events(PresenceId, ClientData, GetSnapshot, EventCategories)),
  GetSnapshot =\= DOORS_TRUE |
	ClientData = _,
	Rcv'' = [do_view_events(PresenceId, EventCategories) | Rcv'?],
	self;

  Rcv ? (_ : view_events(PresenceId, ClientData, GetSnapshot, EventCategories)),
  GetSnapshot = DOORS_TRUE, constant(PresenceId) |
	Rcv'' = [([self] : respond_query_snapshot(PresenceId, ClientData)),
		do_view_events(PresenceId, EventCategories)
	| Rcv'?],
	self;

  Rcv ? do_view_events(PresenceId, EventCategories),
  constant(PresenceId), ground(EventCategories) |
	Rcv'' = [([self] : show(id_card(PresenceId), IdCard, Ok1)), 
		([self] : update(id_card(PresenceId, IdCard'?),Ok2)),	
		do_choose_events(PresenceId, EventsToAdd?, add),
		do_choose_events(PresenceId, EventsToDelete?, delete) | Rcv'?],
	listen2(IdCard?, IdCard1, IdCard2),
	id_card#query(IdCard1?, [(SUBSCRIBED_EVENTS_ATTR:OldSubScribedEvents)]),
	listen2(OldSubScribedEvents?, Old1, Old2),
	sets#difference(EventCategories, Old1?, EventsToAdd),
	sets#difference(Old2?, EventCategories, EventsToDelete),
	id_card#update(IdCard2?, 
		[(SUBSCRIBED_EVENTS_ATTR:EventCategories)], IdCard'),
	ok([Ok1?, Ok2?], subscribe_events),
	self;

  Rcv ? do_choose_events(_PresenceId, [], _) |
	self;

  Rcv ? do_choose_events(PresenceId, EventCategory, Action), 
  string(EventCategory),
  string(PresenceId), string(Action) |
	Rcv'' = [do_choose_event_type(PresenceId, EventCategory, Action)
	| Rcv'?],
	self;

  Rcv ? do_choose_events(PresenceId, [EventCategory | Rest], Action),
  string(PresenceId), string(Action) |
	Rcv'' = [do_choose_event_type(PresenceId, EventCategory, Action),
		 do_choose_events(PresenceId, Rest, Action)
	| Rcv'?],
	self;

  Rcv ? do_choose_event_type(PresenceId, EventCategory, Action),
  string(PresenceId), constant(EventCategory), string(Action) |
	Rcv'' = [update_events_table(EventCategory, PresenceId, Action)
	| Rcv'?],
%	computation#display(term, ["Calling update_events_table with ",
%		EventCategory, PresenceId, Action], type(ground)),
	self;

% Assumes Operations is a list of one or more of log, cache, state, broadcast
% This table driven approach is elegant but inefficient - should be optimized 
 
 Rcv ? (_ : got_doorsEvent(PresenceId, Operations, To, 
					EventCategory, EventType, Contents)),
  constant(PresenceId), ground(Operations), ground(Contents),
  ground(To), constant(EventCategory), constant(EventType), string(URL),
  To =\= DOORS_CONVERSATION |
/*	computation#display(term, [got, EventCategory, " ", 
					EventType, " ", Contents],
		 type(ground)),
*/
	processor#interface(gmtime(Date)),
	listen4(Date?, D1, D2, D3, D4),
	listen3(NewTo?, NewTo1, NewTo2, NewTo3),
	listen3(Recipients?, R1, R2, R3),	
	Rcv'' = [compute_recipients(EventCategory, To, NewTo, Recipients),
		do_log(DoLog?, URL, PresenceId, D1?, EventCategory,
					EventType, NewTo1?, Contents, R1?),
		update_state(DoState?, PresenceId, D4?, 
					EventCategory, EventType, Contents),
		broadcast_doorsEvent(DoBroadCast?, PresenceId, D2?,
			EventCategory, EventType, NewTo2?, Contents, R2?),
		cache_event(DoCache?, PresenceId, D3?, EventCategory, 
			EventType, NewTo3?, Contents, R3?) 
	| Rcv'?],
	sets#element(log, Operations, DoLog),
	sets#element(cache, Operations, DoCache),
	sets#element(broadcast, Operations, DoBroadCast),
	sets#element(state, Operations, DoState),
	self;

 Rcv ? (_ : got_doorsEvent(PresenceId, Operations, To, 
					EventCategory, EventType, Contents)),
  constant(PresenceId), ground(Operations), ground(Contents),
  ground(To), constant(EventCategory), constant(EventType), string(URL),
  To = DOORS_CONVERSATION |
/*	computation#display(term, [got_conv, EventCategory, " ", EventType,
		" ", Contents],
		 type(ground)),
*/
	processor#interface(gmtime(Date)),
	listen4(Date?, D1, D2, D3, D4),
	listen3(NewTo?, NewTo1, NewTo2, NewTo3),
	listen3(Recipients?, R1, R2, R3),	
	Rcv'' = [get_conversation_list(PresenceId, List),
		compute_recipients(EventCategory, To(List?),NewTo, Recipients),
		do_log(DoLog?, URL, PresenceId, D1?, EventCategory,
					EventType, NewTo1?, Contents, R1?),
		broadcast_doorsEvent(DoBroadCast?, PresenceId, D2?,
			EventCategory, EventType, NewTo2?, Contents, R2?),
		update_state(DoState?, PresenceId, D4?, 
					EventCategory, EventType, Contents),
		cache_event(DoCache?, PresenceId, D3?, EventCategory, 
			EventType, NewTo3?, Contents, R3?) 
	| Rcv'?],
	sets#element(log, Operations, DoLog),
	sets#element(cache, Operations, DoCache),
	sets#element(broadcast, Operations, DoBroadCast),
	sets#element(state, Operations, DoState),
	self;

  Rcv ? get_conversation_list(PresenceId, List), constant(PresenceId) |
	Rcv'' = [([self]: show(id_card(PresenceId), IdCard, _Ok)),
		 get_conversation_list1(PresenceId, Connection?, Objects?, List)
	| Rcv'?],
	id_card#query(IdCard?, [(PRESENCE_CONNECTION_ATTR:Connection),
				(PRESENCE_OBJECTS_ATTR : Objects)]),
	self;

  Rcv ? get_conversation_list1(_PresenceId, [], [], List) |
	List = [],
	self;

  Rcv ? get_conversation_list1(_PresenceId, undefined, undefined, List) |
	List = [],
	self;

  Rcv ? get_conversation_list1(_PresenceId, [], undefined, List) |
	List = [],
	self;

  Rcv ? get_conversation_list1(_PresenceId, undefined, [], List) |
	List = [],
	self;

  Rcv ? get_conversation_list1(PresenceId, Connection, _, List), 
  constant(PresenceId),
  Connection = {ConnecteePresenceId, _ConnecteeObjectId, 
					DOORS_CONNECTION_TO_PRESENCE},
  PresenceId =\= ConnecteePresenceId |
	List = [PresenceId, ConnecteePresenceId],
	self;

  Rcv ? get_conversation_list1(PresenceId, Connection, _, List), 
  constant(PresenceId),
  Connection = {PresenceId, _ConnecteeObjectId, 
					DOORS_CONNECTION_TO_PRESENCE} |
	List = [PresenceId],
	self;

  Rcv ? get_conversation_list1(PresenceId, Connection, _, List), 
  constant(PresenceId),
  Connection = {ConnecteePresenceId, ConnecteeObjectId, 
					DOORS_CONNECTION_TO_OBJECT},
  ground(ConnecteePresenceId) |
	Rcv'' = [get_passengers(ConnecteePresenceId, ConnecteeObjectId,
				Passengers)
	| Rcv'?],
	List = [ConnecteePresenceId | Passengers?],
	self;
	

  Rcv ? get_conversation_list1(PresenceId, [], Objects, List), 
  constant(PresenceId), list(Objects)  | % bus driver 
	Rcv'' = [get_passengers(PresenceId, [], Passengers)
	| Rcv'?],
	List = [PresenceId | Passengers?],
	self;

  Rcv ? get_conversation_list1(PresenceId, undefined, Objects, List), 
  constant(PresenceId), list(Objects)  | % bus driver 
	Rcv'' = [get_passengers(PresenceId, object_id_irrelevant, Passengers)
	| Rcv'?],
	List = [PresenceId | Passengers?],
	self;

  Rcv ? do_log(NotTrue, _URL, _PresenceId, _Date,
				_EventCategory, _EventType, _To, _Contents, _R),
  NotTrue =\= true |
	self;

  Rcv ? do_log(true, URL, PresenceId, Date, EventCategory, 
					EventType, To, Contents, Recipients) |
	logging_monitor#log_event(URL, PresenceId, Date, 
			EventCategory, EventType, To, Contents, Recipients),
	self;

  Rcv ? compute_recipients(EventCategory, [], NewTo?, R?) |
	EventCategory = _,
	NewTo = DOORS_LIST,
	R =  [],
	self;

  Rcv ? compute_recipients(EventCategory, To, NewTo?, Recipients?),
  ground(To), list(To), constant(EventCategory) |
	Rcv'' = [get_list(EventCategory, List)
	| Rcv'?],
	sets#intersection(To, List?, Recipients),
	NewTo = DOORS_LIST,
	self;

  Rcv ? compute_recipients(EventCategory, DOORS_PLACE, NewTo?, Recipients?),
  string(URL)  |
	Rcv'' = [get_list(EventCategory, Recipients) | Rcv'?],
	NewTo = DOORS_PLACE,
	self;

  Rcv ? compute_recipients(EventCategory, DOORS_SERVER, NewTo?, Recipients?),
  string(URL)  |
	Rcv'' = [get_list(EventCategory, Recipients) | Rcv'?],
	NewTo = DOORS_SERVER,
	self;

  Rcv ? compute_recipients(EventCategory, DOORS_CONVERSATION(To), 
						NewTo?, Recipients?),
  string(URL)  |
	Rcv'' = [get_list(EventCategory, List) | Rcv'?],
	sets#intersection(To, List?, Recipients),
	NewTo = DOORS_CONVERSATION,
	self.
  
cache(Rcv, EventsCacheMax, URL) 
+ (Count, CacheHead, CacheTail) :-


  initially |
	CacheHead = CacheTail?,
	Count = 0;

  Rcv ? cache_event(DoCache, _PresenceId, _Date, _EventCategory, _EventType, 
		_To, _Contents, _Recipients), DoCache =\= true |
	self; % don't cache certain (say transient) events

  Rcv ? cache_event(true, PresenceId, Date, EventCategory, 
					EventType,To,Contents, R) |
	Rcv'' = [do_cache_event(PresenceId, Date, 
			EventCategory, EventType, To, Contents, R)
	| Rcv'?],
	self;

  Rcv ? do_cache_event(_PresenceId, _Date, 
			_EventCategory, _EventType, _To, _Contents, _R),
  EventsCacheMax =< 0 |
	self;	

  Rcv ? do_cache_event(PresenceId, Date, 
				EventCategory, EventType, To, Contents, R), 
  Count <  EventsCacheMax,
  string(Date), constant(To) |
	Count++,
	make_presences_list(To, R, P),
	CacheTail ! [PresenceId, Date, 
				EventCategory, EventType, To, Contents, P?],
	self;

  Rcv ? do_cache_event(PresenceId, Date, EventCategory, 
						EventType, To, Contents, R),
  string(Date), constant(To), EventsCacheMax > 0,
  Count >= EventsCacheMax, CacheHead ? _Oldest | % throw away the oldest
	make_presences_list(To, R, P),
	CacheTail ! [PresenceId, Date, EventCategory, 
						EventType, To, Contents, P?],
	self;

  Rcv ? ( _ : respond_query_cache(PresenceId, ClientData, 
		EventCategories, From, Until, Number)),
  string(PresenceId), constant(URL), integer(Count) |
	CacheHead1 = CacheHead?,
	listen2(CacheHead1?, Ch1, Ch2),
	CacheHead' = Ch1?,
	listen2(CacheTail'?, R, CacheTail),
	respond_query_cache(true, URL, Count, Ch2?, R?, PresenceId, 
			ClientData, From, Until, Number, EventCategories),
	self.


respond_query_cache(Ok, URL, Count, CacheHead, CacheTailR, PresenceId, 
 			ClientData, From, Until, Number, SubScribedEvents) :-
  
  Ok =\= true |
	ok(Ok, respond_query_cache),
	computation#display(
		"Server software error: Cannot respond to cache query"),
	CacheHead = _,
	CacheTailR = _,
	PresenceId = _, Count = _,
	ClientData = _, From = _, Until = _, Number = _, URL = _,
	SubScribedEvents = _;
	
  Ok = true |
	respond_query_cache1(URL, Count, CacheHead, CacheTailR, PresenceId, 
		ClientData, From, Until, Number, SubScribedEvents).


respond_query_cache1(URL, Count, CacheHead, CacheTailR, PresenceId, 
			ClientData, From, Until, Number, SubScribedEvents) 
  +(Cache, EventsCache, C, ListCount):-

  initially |
	C = 0, ListCount = 0,
	EventsCache = Cache? ;

  CacheHead ? Event, string(From),
  Event = [_PresenceId, Date, _EventCategory, _EventType, _To, _Contents,_R],   
  ground(SubScribedEvents), Date @< From, 
  integer(ListCount), integer(Count) |
	ListCount++,
%	computation#display(term, 
%		[ListCount, Count, "iterating Date @< From ", Date, From],
%							type(ground)),
	self;

  CacheHead ? Event, ListCount =< Count, string(From),
  Event = [_PresenceId, Date, EventCategory, _EventType, _To, _Contents, _R],   
  ground(SubScribedEvents), Date = From, C < Number,
  integer(ListCount), integer(Count), constant(PresenceId) |
	ListCount++,
	sets#element(EventCategory, SubScribedEvents, Result),
	include_if_subscribed(Result?, PresenceId,Cache, Cache'?, Event, C, C'),
%	computation#display(term, 
%		[ListCount, Count, "checking Date = From ", Date, From],
%							type(ground)),
	self;

  CacheHead ? Event, ListCount =< Count, string(From),
  Event = [_PresenceId, Date, EventCategory, _EventType, _To, _Contents, _R],   
  ground(SubScribedEvents), From @< Date, Date @< Until, C < Number,
  integer(ListCount), integer(Count), constant(EventCategory),
  constant(PresenceId) |
	ListCount++,
	sets#element(EventCategory, SubScribedEvents, Result),
	include_if_subscribed(Result?, PresenceId,Cache, Cache'?, Event, C, C'),
%	computation#display(term, 
%		[ListCount, Count, "checking date in range ", Date, From,
%		"EventCategory ", EventCategory, " SubScribedEvents ",
%		SubScribedEvents, " Result ", Result??],
%							type(ground)),
	self;


  CacheHead ? Event,
  Event = [_PresenceId, Date, _EventCategory, _EventType, _To, _Contents, _R],
  Until @< Date | % termination
	Cache = [],
	CacheTailR = _, From = _,  Count = _, ListCount = _,
	C = _,
	SubScribedEvents = _, CacheHead' = _, Number = _,
%	computation#display(term,[1, "Sending ", C, " events."], type(ground)),
	send_cache_response(URL, PresenceId, ClientData, EventsCache);

  CacheHead ? Event,
  Event = [_PresenceId, Date, _EventCategory, _EventType, _To, _Contents, _R],
  Date = Until | % termination
	Cache = [],
	CacheTailR = _, From = _, Count = _, ListCount = _,
	CacheHead' = _, Number = _,
	SubScribedEvents = _,
	C = _, 
%	computation#display(term,[2, "Sending ", C, " events."], type(ground)),
	send_cache_response(URL, PresenceId, ClientData, EventsCache);


  C >= Number | % termination
	Cache = [],
	CacheTailR = _, From = _, Until = _,
	SubScribedEvents = _, CacheHead = _, Count = _, ListCount = _,
%	computation#display(term,[3, "Sending ", C, " events."], type(ground)),
	send_cache_response(URL, PresenceId, ClientData, EventsCache);

	
  ListCount >= Count,
  integer(ListCount), integer(Count) | % termination
	Cache = [], C = _,
	CacheTailR = _, From = _, Until = _, Number = _, %C = _,
	SubScribedEvents = _, CacheHead = _, 
%	computation#display(term,[4, 
%		"ListCount ", ListCount, "Count ", Count,
%			"Sending ", C, " events."], type(ground)),
	send_cache_response(URL, PresenceId, ClientData, EventsCache).
	

include_if_subscribed(Result, PresenceId, Cache, Cache1, Event, C, C1) :-


  Result = true |
	include_if_appropriate(PresenceId, Cache, Cache1, Event, C, C1);

  Result =\= true |
	Event = _, PresenceId = _,
%	computation#display(term, ["not including ", Event], type(ground)),
	C1 = C,
	Cache = Cache1.

include_if_appropriate(PresenceId, Cache, Cache1, Event, C, C1) :-
  Event = [PresenceId, _Date, _EventCategory, _EventType, _To, _Contents,_R] |
%	computation#display(term, ["including ", Event], type(ground)),
	C1 := C + 1,
	Cache = [Event | Cache1];

  Event = [_PresenceId, _Date, _EventCategory, _EventType, To, _Contents,_R],
  To = DOORS_PLACE |
	PresenceId = _,
%	computation#display(term, ["including ", Event], type(ground)),
	C1 := C + 1,
	Cache = [Event | Cache1];

  Event = [_PresenceId, _Date, _EventCategory, _EventType, To, _Contents,_R],
  To = DOORS_SERVER |
	PresenceId = _,
%	computation#display(term, ["including ", Event], type(ground)),
	C1 := C + 1,
	Cache = [Event | Cache1];

  otherwise, constant(PresenceId),
  Event = [_PresenceId, _Date, _EventCategory, _EventType, _To, _Contents, R] |
	person_in_event(PresenceId, R, Result),
	include_if_appropriate1(Result?, Cache, Cache1, Event, C, C1).

include_if_appropriate1(Result, Cache, Cache1, Event, C, C1) :-
   Result = true, ground(Event) |
	%	computation#display(term, ["including ", Event], type(ground)),
	C1 := C + 1,
	Cache = [Event | Cache1];

  Result =\= true |
	Event = _, 
%	computation#display(term, ["not including ", Event], type(ground)),
	C1 = C,
	Cache = Cache1.

person_in_event(PresenceId, R, Result) :-
	strings#extract(HASH, PresenceId, FullName, _),
	person_in_event1(FullName?, R, Result).

person_in_event1(FullName, R, Result) + (Result1 = false) :-
  R = [] |
	FullName = _,
	Result = Result1;

  R ? Recipient, Result1 = false, constant(FullName) |
	strings#extract(HASH, Recipient, FullName1, _),
	compare_people(FullName, FullName1?, Result1'),
	self;

  Result1 = true |
	R = _, FullName = _,
	Result = true.

compare_people(FullName, FullName1, Result1) :-
  FullName = FullName1 |
	Result1 = true;

  FullName =\= FullName1 |
	Result1 = false.

update_events_table(Rcv) + (S) :-

  initially |
	stream#hash_table(S?);

  finally |
	S = [];

  Rcv ? update_events_table(EventCategory, PresenceId, Action) |
	S ! lookup(EventCategory, NewList?, OldList, Status),
	update_list(Status?, Action, PresenceId, OldList?, NewList),
%	computation#display(term, NewList??, type(ground)),
	self; 

  Rcv ? get_list(EventCategory, List?), constant(EventCategory) |
	S ! member(EventCategory, List1, Ok),
%	computation#display(term, (EventCategory : List1??), type(ground)),
	Rcv'' = [get_list1(Ok?, List1?, List) | Rcv'?],
	self;

  Rcv ? get_list1(true, List1, List) |
	List = List1,
	self;

  Rcv ? get_list1(Error, _List1, List), Error =\= true |
	List = [],
	self.

% Don't just call sets#Action because block compilation wouldn't understand
update_list(Status, Action, PresenceId,  OldList,  NewList) :-

  Action = add, Status = new |
	OldList = _,
	NewList = [PresenceId];
  
  Action = add, Status = old |
	sets#add(PresenceId, OldList, NewList);

  Action = delete, Status = old |
	sets#delete(PresenceId, OldList, NewList);

  Action = delete, Status = new |
	PresenceId = _, OldList = _,
	NewList = [].	


make_presences_list(To, Recipients, PresencesList) :-
  To = DOORS_LIST |
        PresencesList = Recipients;

  To = DOORS_CONVERSATION |
	PresencesList = Recipients;

  To =\= DOORS_LIST, To =\= DOORS_CONVERSATION |
        PresencesList = [],
        Recipients = _.


send_cache_response(URL, PresenceId, ClientData, EventsCache) :-
ground(EventsCache), string(URL) |
%	computation#display(term, 
%			[cache, EventsCache, EventsCache'??], type(ground)),
	server_api#events_cache_list_to_tuple(URL, EventsCache, EventsCache'),	
	processor#room(message([sci], [],
		doorsPropagateResponse(URL, PresenceId, ClientData,
			PLACE_RESPONSE_CATEGORY,
			PLACE_CACHE_RESPONSE, EventsCache'?), Ok1), Ok2),
	ok([Ok1?, Ok2?], send_cache1).
