/* $Header: /home/qiana/Repository/Logix/system/Doors/basic_room.cp,v 1.1.1.1 1999/07/09 07:03:24 bill Exp $ */
-language(dfcp).
-mode(interrupt).
-export([create, basic_room, show_doors, show_filtered_doors,
	 rcv_control, send, update_id_card, self_rcv,
	 update_cards, discard_entry, unify_if_ok, which_update_id]).
-override([create1, room, room_close]).

create(IdCard,Door,Ok) :-
	create1(IdCard,Door,super,Ok).

create1(IdCard,InitialDoor,InitialDoorId,Ok) :-
    ground(IdCard) |
	id_card#query(IdCard,[(name:Name),(type:Type)]),
	update_id_card(IdCard,IdCard'), 
	doors#corridor(InitialDoor,Door1),
	self_rcv(SndSelf?,SndSelf1),
	doors#merger(SndSelf1?,RcvSelf),
	entries#create(Doors),
	Rcv' = [initialize(IdCard'?,SndSelf,Ok), 
			  ([self] : paste(Door1?,InitialDoorId?,Ok1)),
			  initialize_application(Ok2)
		 	 | RcvSelf? ],
	ok([Ok1?, Ok2?], create1_ok),
	room.

update_id_card(IdCard1, IdCard2) :-
	processor#interface(gmtime(Date)),
	id_card#update(IdCard1, [(created: Date?)], IdCard2).


self_rcv(Rcv,Snd) :-
	Rcv ? message([],From,Request,Ok?), Request =\= merge(_) |
		Snd ! ([self|From] : Request),
		Ok = true,
		self;
	Rcv ? message([],[],merge(Snd1),Ok?) |
		Ok = true,
		Snd ! merge(Snd1?),
		self;
	Rcv ? message(To,From,Message,Ok), To =\= [] |
		Snd ! message(To,[self|From],Message,Ok),
		self;

	Rcv = [] | Snd = [].
	

/************************************************************************
*
* To open a "Pure" basic room - Call "create"
* To inherit basic room functionality - Inherit "basic_room"
*
*************************************************************************/


room(Rcv, Doors, Name, Type) :-
    +initialized_room;
    +room_ignore_events.



initialized_room(Rcv, Doors, Name, Type)  :-
    +room_initialize_application;
    +basic_room.
 
room_initialize_application(Rcv) :-
    Rcv ? initialize_application(Ok?) |
	Ok = true,
	self.

basic_room(Rcv, Doors, Name, Type)  :-
    +room_initialize;
    +room_close;
    +room_route;
    +room_send;
    +room_show;
    +room_paste;
    +room_discard;
    +room_discarded;
    +room_invalid;
    +room_update_id_card;
    +room_protect;
    +room_listen_to.

	
room_initialize(Rcv,Doors, Name, Type) :-
    Rcv ? initialize(IdCard,Snd,Ok) |
	entries#allocate_entry(self,Doors,Doors',Entry?,_EntryId,Ok), 
	entries#fill_entry(Entry,IdCard,Snd,_DiscardNotify),
	self.


room_close(Rcv, Doors) :-
    Rcv ? (_From: close(Ok?)) |
	Rcv'=_, 
	entries#close(Doors),
	finalize, 
	Ok=true.

room_show(Rcv,Doors) :-
    Rcv ? (_From: show(doors,DoorsInfo,Ok?)) |
	show_doors(Doors,Doors',DoorsInfo),
	Ok=true,
	self;

    Rcv ? (_From: show(doors(Pairs), DoorsInfo, Ok?)), ground(Pairs) |
%	computation#display(["want to show doors of ", Pairs]),
	show_filtered_doors(Pairs, Doors,Doors',DoorsInfo),
	Ok = true,
	self;
 
    Rcv ? (From: show(path,From^,true^)) |
	self;

    Rcv ? (_From: show(id_card(Id),IdCard,Ok)) |
	entries#id_to_id_card(Id,IdCard,Doors,Doors',Ok),
	self.


show_doors(Doors,Doors1,DoorsInfo) :-
	entries#retrieve_IdCards(Doors,Doors1,DoorsInfo).

show_filtered_doors(Pairs, Doors,Doors1, FilteredInfo) :-
	entries#retrieve_IdCards(Doors,Doors1,Info),
%	computation#display(["unfiltered doors are ", Info??]),
	filter_doors_info(Pairs, Info?, FilteredInfo).

filter_doors_info(Pairs, Info, FilteredInfo) :-
    Info ? DoorInfo, DoorInfo = (Id - IdCard), ground(IdCard), ground(Pairs) |
	id_card#match(IdCard, Pairs, Ok),
	include_if_ok(Ok?, Id, IdCard, FilteredInfo, FilteredInfo'?),
	self;

    Info = [] |
	Pairs = _,
%	computation#display("finished filtering"),
	FilteredInfo = [].

include_if_ok(Ok, Id, IdCard,FilteredInfo1, FilteredInfo2):-
    Ok = true, constant(Id), ground(IdCard) |
%	computation#display(["matched Id of ", Id, " IdCard of ", IdCard]),
	FilteredInfo1 =  [(Id - IdCard) | FilteredInfo2];

    Ok =\= true |
	Id = _, IdCard = _,
	FilteredInfo1 = FilteredInfo2.

update_cards(From,IdCard1,EntryId,IdCard2,DoorCard,Doors1,Doors2) :-
	From = _,
	processor#interface(gmtime(Date)),
	id_card#update([],[(joined_date: Date?)],DoorCard),
	Doors2 = Doors1,
	id_card#update(IdCard1,[(entry_id:EntryId)], IdCard2).



rcv_control(Rcv,EntryId,Snd, DiscardNotify)  :-

	DiscardNotify = true | doors#door_terminator(Rcv), Snd=[], EntryId=_;

	Rcv=[] | Snd=[([EntryId]:discarded(_))],  DiscardNotify = _;


	Rcv ? message(To,From,Message,Ok),
	To =\= [], unknown(DiscardNotify), ground(EntryId) |
		Snd ! message(To,[EntryId|From],Message,Ok),
		self;

	Rcv ? message([],From,Message,Ok?),
	unknown(DiscardNotify), ground(EntryId) |
		Snd ! ([EntryId|From] : Message),
		Ok = true,
		self;


	Rcv ? Message, Message =\= message(_,_,_,_), 
	unknown(DiscardNotify), ground(EntryId) |
		Snd ! ([EntryId] : Message),
		self;

	invalid(Rcv), ground(EntryId) |
		invalid_reason(Rcv,Reason),
		Snd ! (EntryId: invalid(Reason?)),
		Snd' = [],
		DiscardNotify = _,
		EntryId = _.
		

room_route(Rcv,Doors) :-
	Rcv ? message(To,From,Message,Ok), To =\= [] |
		send(To,From,Message,Doors,Doors',Ok),
		self.


room_send(Rcv,Doors) :-
	Rcv ? send(To,Message), To =\= [] |
		send(To,[],Message,Doors,Doors',Ok),
		ok(Ok?,room_send),
		self.


send(To,From,Message,Doors1,Doors2,Ok) :-
	entries#send(To,From,Message,Doors1,Doors2,Ok).


room_discard(Rcv,Doors) :-
    Rcv ? (From: discard(Id,Ok1?)), ground(Id) | 
	discard_entry(Id,Doors,Doors',IdCard,  Rcv'?, Rcv'', Ok),
	listen2(Ok?, Ok1, Ok2),
	TheEvent = event(doors, (From : discard(Id,IdCard?))),
 	unify_if_ok(Ok2?, TheEvent?, Rcv''',Rcv''?),	
 	self.


room_discarded(Rcv,Doors) :-
    Rcv ? ([From]: discarded(OkRet?)), ground(From) |
	OkRet = true,
	discard_entry(From,Doors,Doors',IdCard,  Rcv'?, Rcv'', Ok),
	listen2(Ok?, Ok1, Ok2),
	TheEvent = event(doors, ([From] : discard(From,IdCard?))),
 	unify_if_ok(Ok1?, TheEvent?, Rcv''',Rcv''?),	
 	ok(Ok2?, room_discarded),
	self.


room_invalid(Rcv,Doors) :-
    Rcv ? (From: invalid(Reason)), ground(From) |
	discard_entry(From,Doors,Doors',IdCard, Rcv'?, Rcv'', Ok),	
 	TheEvent =  event(doors, (From : invalid(Reason,IdCard?))),
 	unify_if_ok(Ok?, TheEvent?, Rcv''',Rcv''?),	
 	self.



room_ignore_events(Rcv) :-

    Rcv ? event(_Type, _Event) | self.

discard_entry(Id, Doors, Doors1, IdCard, Rcv1, Rcv2, Ok) :-
	entries#discard_entry(Id,Doors,Doors1,IdCard,Ok, FloatingRoom),
	is_inaccessible_room(FloatingRoom?, Rcv1, Rcv2).
 

/* 
 	Check whether the room is inaccessible from the outside
	i.e. the only door is self. if so -> terminate room.
*/

is_inaccessible_room(FloatingRoom, Rcv1, Rcv2) :-
    FloatingRoom = false | 
	Rcv2 = Rcv1;

    FloatingRoom = true  | 
	Rcv2 = [(self: close(Ok)) | Rcv1], 
	ok(Ok?, close_room).


room_update_id_card(Rcv,Doors) :-
  Rcv ? ([From|_]: update(id_card(UpdateId,IdCard),Ok1?)),
  ground(From) | 
	which_update_id(From,UpdateId,Id),
	listen2(Id?, Id1, Id2),
	listen2(IdCard?, IdCard1, IdCard2),
	entries#update_id(Id1?,IdCard1?,Doors,Doors',Ok),
	listen2(Ok?, Ok1, Ok2),
	TheEvent = event(id_cards, ([From] : update(id_card(Id2?, IdCard2?)))),
 	unify_if_ok(Ok2?, TheEvent?, Rcv'', Rcv'?),
 	self;

  Rcv ? ([From|_]: tail_and_update(id_card(UpdateId,IdCard),Ok1?)),
  ground(From) | 
	which_update_id(From,UpdateId,Id),
	listen2(Id?, Id1, Id2),
	listen2(IdCard?, IdCard1, IdCard2),
	entries#tail_and_update(Id1?,IdCard1?,Doors,Doors',Ok),
	listen2(Ok?, Ok1, Ok2),
	TheEvent = event(id_cards, ([From] : update(id_card(Id2?, IdCard2?)))),
 	unify_if_ok(Ok2?, TheEvent?, Rcv'', Rcv'?),
 	self;

  Rcv ? ([From|_]: replace(id_card(UpdateId,IdCard),Ok1?)),
  ground(From) | 
	which_update_id(From,UpdateId,Id),
	listen2(Id?, Id1, Id2),
	listen2(IdCard?, IdCard1, IdCard2),
	entries#replace_id(Id1?,IdCard1?,Doors,Doors',Ok),
	listen2(Ok?, Ok1, Ok2),
	TheEvent = event(id_cards, ([From] : replace(id_card(Id2?, IdCard2?)))),
 	unify_if_ok(Ok2?, TheEvent?, Rcv'', Rcv'?),
 	self.

  

which_update_id(From, To, Id) :-
    To = self |
	Id = From;
    To =\= self |
	From = _,
	Id = To.


unify_if_ok(Ok, Message, Rcv1, Rcv2) :-
    Ok =\= true | Rcv1 = Rcv2, Message = _;
    Ok = true |	Rcv1 = [Message | Rcv2].


 
room_protect(Rcv) :-
	Rcv ? protect(Ok,Message),  Ok = true |
		Rcv''= [Message|Rcv'],
		self;
	Rcv ? protect(Ok,Message),  Ok =\= true |
		doors#generic_reply(Message,Ok),
		self.



room_listen_to(Rcv, Doors) :-

    Rcv ? (From : death_signal(DiscardNotify?)) |
      entries#retrieve_DiscardNotify(From, Doors, Doors', DiscardNotify),
      self;

    Rcv ? listen_to(RoomToListenTo, EventTypes, EventControl, Ok) |
      Rcv'' = [ send([self],merge(M?)),
                message(RoomToListenTo, [],	
                       listen(EventTypes, M, EventControl, Ok1), Ok2)
      | Rcv'?],
      and([Ok1?, Ok2?], Ok),
      self.

room_paste(Rcv, Doors) :-
    Rcv ? (From: paste(Door,EntryId,Ok1?)), Door = door(_,_) |
        entries#allocate_entry(EntryId,Doors,Doors',Entry?,EntryId',Ok),
	listen2(Ok?, Ok1, Ok2),
        Rcv'' = [protect(Ok2?,paste(From,Door,Entry,EntryId'?,_Ok))|Rcv'],
        self;

    Rcv ? (_From: paste(Door, _EntryId,Ok?)), invalid(Door) |
        Ok = false(invalid),
        self;
 
    Rcv ? paste(From,Door,Entry,EntryId,_Ok), ground(EntryId), ground(From),
    Door = door(DoorSnd?,DoorRcv) |                     
        update_cards(From,IdCard?,EntryId,IdCard',DoorCard,Doors,Doors'),
        DoorSnd ! update(id_card(self,IdCard'?),_Ok1), 
        entries#fill_entry(Entry,DoorCard?,DoorSnd',DiscardNotify),
        rcv_control(DoorRcv,EntryId,DoorRcv',DiscardNotify?),
        Rcv'' = [ send([self],merge(DoorRcv'?)),
                ([]: show(id_card(self),IdCard,Ok2)),
                event(doors, (From : paste(EntryId))) 
        | Rcv'], 
        ok(Ok2?, update_cards),
        self.
