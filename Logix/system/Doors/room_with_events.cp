/* $Header: /home/qiana/Repository/Logix/system/Doors/room_with_events.cp,v 1.1 1999/07/09 07:03:25 bill Exp $ */
-language([inherit, dfcp]).
-mode(interrupt).
-export(all).
-override([room, room_with_events, room_send_events_to_eventslog,
	 event_filter]).

/*
events produced:

event(doors, GmDate, refresh(DoorsInfo))      DoorsInfo ::= [(Key - IdCard)]
event(doors, GmDate, (From : discard(From,IdCard)))
event(doors, GmDate, (From : paste(EntryId))) 
event(doors, GmDate, (From : invalid(Reason,IdCard))),
event(id_cards, GmDate, (From : update(id_card(Id, IdCard)))),

*/

create(IdCard,Door,Ok) :-
  +basic_room#create.
 
/************************************************************************
*
* Events Room has one more argument namely EventsLog,
*
* 1.  To open a "Pure" events room - Call "create",
* 2.  To inherit room_with_events functionality - Inherit "room_with_events"  

*
* The difference: 
* (1) initializes application while (2) doesnt
* (2) makes sure doors and id_cards events are on the events log while (3) 
*   doesnt,
*

The flow should be as follows:
 
The room creates messages and sends them on EventsLog, with the exception
of doors and id_cards events which are created in basic_room
and thus have no EventsLog to write on.
 
The data on EventsLog is sent  to  all listening processes 
in a copying mechanism which distributes it to the listening rooms.
(The data can then be further filtered by overriding "event_filter").
 
Every room, whether with a direct door to the listened room (A), or a 
room with no direct connection, or even A itself (such as done in the
tables and in meeting_room), can listen to events in A.
The event flow to a listening room is stopped in one of theses cases:
(1) The listener closes the control stream to the events.
(2) The listener puts  stop([XXX]) on the control stream, where XXX is the
event type wished to stop
(3) The listening room has died.
 
You can listen in one of two ways:
 
(1) send the room to be listened to the message:
 listen(Types, BackChannel, EventControl, UserOk)
 
where Types is a list of types to be listened for BackChannel, will contain
events and EventControl can be used to change the query:
by writing:  
start(AdditionalEventTypeList)
stop(lEventTypeList)

closing the Control stream stops the event stream.
dying stops event stream.

(2) put on your OWN receive stream the following:
  listen_to(RoomToListenTo, EventTypes, EventControl, Ok)
 
(The reason is that we merge directly into the caller's receive stream,
and we do not want everyone to be able to merge from a remote room, only
the listener can do so).

*************************************************************************/

room(Rcv, Doors, Name, Type) + (EventsLog) :-
    +room_initialize_application;
    +room_with_events.

room_initialize_application(Rcv) :-
  Rcv ? initialize_application(Ok?) |
	Rcv'' = [initialize_events(Ok) | Rcv'?],
	self.

/* PLEASE DO NOT ADD CLAUSES HERE */

room_with_events(Rcv, Doors, Name, Type) + (EventsLog) :-
    +room_send_events_to_eventslog(Rcv, EventsLog);
    +room_with_events_clauses(Rcv, Doors, Name, Type, EventsLog).   

/* PLEASE ADD ALL NEW INHERITANCE CALLS HERE AND NOT ABOVE */

room_with_events_clauses(Rcv, Doors, Name, Type, EventsLog) :-
    +basic_room#basic_room;
    +room_listen;
    +initialize_events;
    +room_ignore_snapshots.

initialize_events(Rcv) :-
    Rcv ? initialize_events(Ok?) |
	Ok = true,
        self.

/* PLEASE DO NOT ADD CLAUSES HERE */
  
room_send_events_to_eventslog(Rcv, EventsLog) :-

  Rcv ? event(EventType, Event) |
	processor#interface(gmdate(Date)),
	EventsLog ! event(EventType, Date?, Event),
	self.

room_listen(Rcv,Doors,EventsLog)  :-

    Rcv ? get_snapshots(From, Me, NewTypes, Log, Log'),
    ground(From), ground(NewTypes), ground(Me) |
%	computation#display(term, ("received get snapshots from " : From)),
	send_snapshots(From, Me, NewTypes, Log, Log', 
				Rcv'', Rcv'?, Doors, Doors'),
        self;

    Rcv ? (From : listen(Types, BackChannel, EventControl, UserOk?)), 
    From =\= [self], ground(From), ground(Types), list(Types) |
         Rcv'' = [
                message(From, [], death_signal(Death), Ok),
          	listen(From, Types, BackChannel, Death?, EventControl)  
       | Rcv'?],
        UserOk = Ok?,
        self;

    Rcv ? (From : listen(_Types, _BackChannel, _EventControl, UserOk?)), 
    invalid(From) |
	UserOk = false(invalid),
	self;

    Rcv ? (_From : listen(Types, _BackChannel, _EventControl, UserOk?)), 
    invalid(Types) |
	UserOk = false(invalid),
	self;

    Rcv ? (_From : listen(_Types, BackChannel, _EventControl, UserOk?)), 
    invalid(BackChannel) |
	UserOk = false(invalid),
	self;

    Rcv ? (_From : listen(_Types, _BackChannel, EventControl, UserOk?)), 
    invalid(EventControl) |
	UserOk = false(invalid),
	self;

    Rcv ? (_From : listen(_Types, _BackChannel, _EventControl, UserOk)), 
    invalid(UserOk) |
	self;

    Rcv ? ([self] : listen(Types, BackChannel, EventControl, UserOk)), 
    ground(Types), list(Types) |
         Rcv'' = [
                message([self], [], death_signal(Death), UserOk),
           listen([self], Types, BackChannel, Death?, EventControl) 
        | Rcv'?],
         self;
    
    Rcv ? listen(From, EventTypes, UserLog?, Choke, EventControl), 
    From=\=[self],
    list(EventTypes), ground(EventTypes),ground(From), From = [Who|_] | 
 	send_snapshots(From, Entry_Id2?, EventTypes, UserLog, UserLog'?,
		Rcv'', Rcv'?, Doors'?,Doors''),
	Rcv''' = [message([self], [], merge(SelfConnect?), Ok1) | Rcv''?],
        ok(Ok1?, selfconnect),
	entries#query(Who, Doors, Doors',
		 [(unique_id:ListenerName), (entry_id : Entry_Id)], _Ok2),
	listen2(Entry_Id?, Entry_Id1, Entry_Id2),
	my_dfcp_copy(EventsLog'?, EventsLog, EventsLog1, TheEnd?),
	filter_events(From, [Entry_Id1?], 
		ListenerName?, EventTypes, EventsLog1?,UserLog',
		SelfConnect, Choke, EventControl, TheEnd),
	self;

    Rcv ? listen(From, EventTypes, UserLog?, Choke, EventControl), 
    From=[self],
    list(EventTypes), ground(EventTypes),ground(From) | 
 	send_snapshots(From, From, EventTypes, UserLog, UserLog'?,
		Rcv'', Rcv'?, Doors,Doors'),
	Rcv''' = [message([self], [], merge(SelfConnect?), Ok1) | Rcv''?],
        ok(Ok1?, selfconnect),
	my_dfcp_copy(EventsLog'?, EventsLog, EventsLog1, TheEnd?),
	filter_events(From, From, self,  EventTypes, EventsLog1?,UserLog',
		SelfConnect, Choke, EventControl, TheEnd),
	self.

my_dfcp_copy(A, B, C, TheEnd) :-

   unknown(TheEnd),
   A ? M, ground(M) |
        B ! M , C ! M,
        self;

   A = [] | B = [], C = [], TheEnd = _;

   TheEnd = true |
        B = A, C = [].
     

filter_if_ok(Ok0, To, SelfConnect, SelfConnect1, Type, Event, ListenerName, Ok) :-
  Ok0 = true |
	event_filter;

  Ok0 =\= true |
	Type = _, Event = _, ListenerName =  _, To = _,
	SelfConnect = SelfConnect1,
	Ok = Ok0.


event_filter(To, Type, SelfConnect, SelfConnect1, Event, ListenerName, Ok) :- 
	Type = _, Event = _, ListenerName =  _, To = _,
	SelfConnect = SelfConnect1,
	Ok = true.

% The purpose of this routine is to keep knowledge of "system" type event
% filtering isolated from the applications. Applications can inherit filtering
% from various places depending on event types and add their own if they wish
event_filter_system(To, SelfConnect, SelfConnect1, Type, Event, ListenerName, Ok) :-

  Type = doors    | To = _, Event = _, ListenerName = _, 
		    SelfConnect = SelfConnect1, Ok = true;

  Type = doors(_) | To = _,  Event = _, ListenerName = _, 
		    SelfConnect = SelfConnect1, Ok = true;

  Type = id_cards | To = _, Event = _, ListenerName = _,
		    SelfConnect = SelfConnect1, Ok = true.

send_snapshots(Listener, Me,  EventTypes, Log, Log1, Rcv, Rcv1, Doors, Doors1) :-
 
    EventTypes ? id_cards | self;

    EventTypes ? doors, ground(Me) |
	processor#interface(gmdate(Date)),
 	basic_room#show_doors(Doors, Doors',DoorsInfo),
	Log ! (Me : event(doors, Date?, refresh(DoorsInfo?))),
	self;
   
    EventTypes ? doors(Pairs), ground(Pairs), ground(Me) |
	processor#interface(gmdate(Date)),
 	basic_room#show_filtered_doors(Pairs, Doors, Doors',DoorsInfo),
 	Log ! (Me : event(doors(Pairs), Date?, refresh(DoorsInfo?))),
 	self;

    EventTypes ? EventType, 
    ground(EventType), ground(Listener), ground(Me),
    EventType=\=id_cards, EventType=\=doors, EventType =\= doors(_) |
	processor#interface(gmdate(Date)),
	Rcv ! (Listener: make_snapshot(EventType, Snapshot, _Ok)),
        Log ! (Listener : event(EventType, Date?, refresh(Snapshot?))),
	self;

    EventTypes = [] |
	Listener = _, Me = _, Log = Log1, Rcv = Rcv1, Doors1 = Doors.

room_ignore_snapshots(Rcv) :-

    Rcv ? refresh(_EventType, null^, false^) |
        self.

/* 
To              - the destination path
Me		- the entry_id of this room in To's door to this room
EventTypes      - events to be filtered.
ThisRoomEvents  - room events
ToListener      - send to listening room on here,
SelfConnect     - connection back to this room (for getting snapshots)
Choke           - signals the death of the listener.
EventControl    - further commands from the listener,
TheEnd          - instantiated when filter_events terminates so that the 
		  events coming to this process can be stopped.
*/
filter_events(To, Me, ListenerName, EventTypes, ThisRoomEvents,  ToListener, 
                SelfConnect, Choke, EventControl, TheEnd) :-

    ThisRoomEvents ? M, invalid(M), unknown(Choke) |
	self;

    ThisRoomEvents ? M, ground(M), unknown(Choke), ground(EventTypes),
    M = event(EventType, _GmDate, Event), ground(ListenerName), ground(To),
    ground(Me)  |
%	computation#display(term,
%		["Filtering ", M, " for ", ListenerName, "Type ", EventType, 
%		" types ", EventTypes], type(ground)),
	member(EventType, EventTypes, OkMember),
	filter_if_ok(OkMember?, To, SelfConnect, SelfConnect'?, EventType, 
		Event, ListenerName, OkFilter),
	include_if_ok(OkFilter?, (Me : M), ToListener, ToListener'?),
	self;

    ThisRoomEvents ? M, ground(M), unknown(Choke),
    M =\= event(_, _, _) |
	computation#display(
		("event in events-log with incorrect event format" - M)
	),
	self;

    EventControl ? start(Types), ground(EventTypes),
        ground(To), ground(Me) |
        SelfConnect ! 
          get_snapshots(To, Me, NewMembers1?, ToListener, ToListener'?),  
      	diff_types(EventTypes, Types, NewMembers),
	listen2(NewMembers?, NewMembers1, NewMembers2),
        append(EventTypes, NewMembers2?, EventTypes'),
        self;
 
    EventControl ? stop(Types), Types=\=all |
        diff_types(Types, EventTypes, EventTypes'),
        self;
 
    EventControl ? stop(all) |
        EventTypes = _, EventTypes' = [],
        self;
  
    ground(Choke) |
        ToListener=[], ThisRoomEvents=_, EventTypes = _, ListenerName = _, 
        To = _, Me = _, EventControl = _, SelfConnect = [], TheEnd = true;
 
    invalid(Choke) |
        ToListener=[], ThisRoomEvents=_, EventTypes = _, ListenerName = _,
        To = _, Me = _, EventControl = _, SelfConnect = [], TheEnd = true;
 
    ThisRoomEvents=[] |
        ToListener=[],  Choke=_, EventTypes = _, ListenerName = _, 
        To  = _, Me = _, EventControl = _, SelfConnect = [], TheEnd = true.

diff_types(A, B, InBNotInA) :-

    B ? M, ground(M), ground(A) |
        member(M, A, Ok),
        not(Ok?, Ok2),
        include_if_ok(Ok2?, M, InBNotInA, InBNotInA'?),
        self;

    B = [] | InBNotInA = [], A = _.

member(Elem, List, Ok) :-

    List ? Elem  | Ok = true, List' = _;
    List = [] | Ok = false, Elem = _;
    List ? NotElem, NotElem =\= Elem | self.

 
not(Ok1, Ok2) :-
    Ok1 = true |
	Ok2 = false;
    Ok1 = false |
	Ok2 = true.

include_if_ok(Ok, M, L1, L2) :-

    Ok = true |
	L1 = [M | L2] ;
    Ok =\= true |
	L1 = L2, M = _.

