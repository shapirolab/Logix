/*  $Header: /home/qiana/Repository/Logix/system/Doors/entries.cp,v 1.1 1999/07/09 07:03:25 bill Exp $ */

-language([inherit,dfcp]).
-mode(interrupt).

Doors ::= {NextId,Entries}.

Entries ::= [Key-entry(IdCard,Snd,DiscardNotify)].


create(Doors) :-
	Doors = {0,Hash?},
	hash#create(Hash).

close(Doors) :-	
	Doors = {_,Entries} |
	close_entries(Entries).

close_entries(Values) :-	
	Values= [] | true;

	Values ? _Id-Entry |
		close_entry(Entry),
		self.


close_entry(Entry) :-
	Entry = entry(_IdCard,Snd?,DiscardNotify?) | 
		DiscardNotify = true,
		Snd = [].


allocate_entry(EntryId,Doors,Doors1,Entry,EntryId1,Ok) :-
	writable(EntryId), 
	Doors = {NextId,Entries} |
		NextId++,
		listen4(NextId'?, N1, N2, N3, N4),
		EntryId = N1?,
		EntryId1 = N2?,
		hash#insert(N3?,Entry?,Entries,Entries',Ok),
		Doors1 = {N4?,Entries'?};

	string(EntryId), Doors = {NextId,Entries} |
		EntryId1 = EntryId,
		hash#insert(EntryId,Entry?,Entries,Entries',Ok),
		Doors1 = {NextId,Entries'?};

	integer(EntryId), Doors = {NextId,Entries} |
		EntryId1 = EntryId,
		hash#insert(EntryId,Entry?,Entries,Entries',Ok1),
		listen2(Ok1?, Ok2, Ok3),
		bump_NextId(Ok2?,EntryId,NextId,NextId'),
		Ok = Ok3?,
		Doors1 = {NextId'?,Entries'?}.

bump_NextId(Ok,EntryId,NextId,NextId1) :-
	Ok =\= true | NextId1 = NextId, EntryId = _;
	Ok = true, NextId =< EntryId | NextId1 := EntryId + 1;
	Ok = true, NextId > EntryId | NextId1 = NextId.


fill_entry(Entry,IdCard,Snd,DiscardNotify) :-
		Entry = entry(IdCard,Snd,DiscardNotify).


check_entry(EntryId,Doors,Doors1,Ok) :-
	Doors = {NextId,Entries} |
		hash#check(EntryId,Entries,Entries',Ok),
		Doors1 = {NextId,Entries'?}.

discard_entry(Id,Doors,Doors1,IdCard,Ok, FloatingRoom) :-
    Doors = {NextId,Entries} |
	hash#delete(Id,Value,Entries,Entries',Ok1),
	check_for_floating_room(Entries'?, Entries'', FloatingRoom),
	Doors1 = {NextId,Entries''?},
	discard_entry1(Ok1?,Ok,Value?,IdCard).


discard_entry1(Ok1,Ok,Value,IdCard1) :-
	Ok1 =\= true | 
		Ok=Ok1, Value=_, IdCard1=_;
	Ok1 = true, Value=entry(IdCard,Snd?,DiscardNotify?) |
		Ok=true,
		DiscardNotify = true,
		IdCard1 = IdCard,
		Snd = [].


check_for_floating_room(Entries, Entries1, Result) :-
    Entries = [Entry] |		% which means that only "self" is in the
	Entries1 = [Entry],     % room - thus- it is inaccessible, and should
	Result = true;		% be closed.

    Entries =  [A | B], B =\= [] |
	Entries1 = [A | B],
	Result = false.
 
retrieve_DoorIds(Doors,Doors1,DoorsIds) :-
	Doors = {NextId,Entries} |
		retrieve_DoorIds1(Entries?,Entries',DoorsIds),
		Doors1 = {NextId,Entries'?}.


% This routine depends on current representation of hash table.
% If representation is changed then conversion routines should be
% defined.

retrieve_DoorIds1(Entries,Entries1,DoorsIds) :-
	Entries = [] | Entries1 = [], DoorsIds = [];

	Entries ? (Key-entry(IdCard,Snd,DiscardNotify)),
	ground(Key) |
		Entries1 ! (Key-entry(IdCard,Snd,DiscardNotify)),
		DoorsIds ! Key,
		self.


retrieve_IdCards(Doors,Doors1,DoorsInfo) :-
	Doors = {NextId,Entries} |
		retrieve_IdCards1(Entries?,Entries',DoorsInfo),
		Doors1 = {NextId,Entries'?}.


% This routine depends on current representation of hash table.
% If representation is changed then conversion routines should be
% defined.

retrieve_IdCards1(Entries,Entries1,DoorsInfo) :-
	Entries = [] | Entries1 = [], DoorsInfo = [];

	Entries ? (Key-entry(IdCard,Snd,DiscardNotify)),
	ground(Key), listener(IdCard) |
		Entries1 ! (Key-entry(IdCard,Snd,DiscardNotify)),
		DoorsInfo ! (Key-IdCard),
		self.


retrieve_IdCard(Doors,Doors1,DoorId,IdCard,Ok) :-
	Doors = {NextId,Entries} |
		retrieve_IdCard1(Entries?,Entries',DoorId,IdCard,Ok),
		Doors1 = {NextId,Entries'?}.


% This routine depends on current representation of hash table.
% If representation is changed then conversion routines should be
% defined.

retrieve_IdCard1(Entries,Entries1,DoorId,IdCard,Ok) :-
	Entries = [] |
		 Entries1 = [], IdCard = [], Ok = false(not_found(DoorId));

	Entries ? (Key-entry(IdCard1,Snd,DiscardNotify)),
	Key = DoorId, listener(IdCard1) |
		IdCard = IdCard1,
		Ok = true,
		Entries1 = [(Key-entry(IdCard1,Snd,DiscardNotify))|Entries'];

	Entries ? (Key-entry(IdCard1,Snd,DiscardNotify)), Key =\= DoorId |
		Entries1 ! (Key-entry(IdCard1,Snd,DiscardNotify)),
		self.

retrieve_DiscardNotify(From,Doors,Doors1,DiscardNotify) :-
	From = [Id|_] |
		replace(Id,Entry,Entry'?,Doors,Doors1,Ok),  
		retrieve_DiscardNotify1(Ok?, Entry?,Entry',DiscardNotify).

% This routine depends on current representation of hash table.
% If representation is changed then conversion routines should be
% defined.

retrieve_DiscardNotify1(Ok, Entry,Entry1,DiscardNotify1) :-
  Entry = entry(IdCard,Snd,D1?), Ok = true | 
		Entry1 = entry(IdCard,Snd,DiscardNotify),
		listen2(DiscardNotify?, D1, D2),
		DiscardNotify1 = D2? ;

  Ok =\= true |
	Entry1 = Entry,
	DiscardNotify1 = true.   

tail_and_update(Key, UpdateIdCard, Doors, Doors1, Ok) :-
  Doors = {NextId,Entries} |
		hash#tail_and_update(Key, OldEntry, NewEntry?,
						Entries, Entries', Ok),
		tail_and_update1(UpdateIdCard, OldEntry?, NewEntry),
		Doors1 = {NextId,Entries'?}.

tail_and_update1(UpdateIdCard, OldEntry, NewEntry) :-
  OldEntry = entry(OldIdCard, Snd, DiscardNotify) |
	id_card#update(OldIdCard,UpdateIdCard,NewIdCard),
	NewEntry = entry(NewIdCard?, Snd, DiscardNotify).

replace(Key,Value,NewValue,Doors,Doors1,Ok) :-
	Doors = {NextId,Entries} |
		hash#replace(Key,Value,NewValue,Entries,Entries',Ok),
		Doors1 = {NextId,Entries'?}.

retrieve_users(Doors,Doors1,DoorsInfo) :-
    Doors = {NextId,Entries} |
	retrieve_users1(Entries?,Entries',DoorsInfo),
	Doors1 = {NextId,Entries'?}.


retrieve_users1(Entries,Entries1,DoorsInfo) + (Info = []):-
    Entries = [] | Entries1 = [], DoorsInfo = Info;

    Entries ? (Key-entry(IdCard,Snd,CutNotify)), listener(IdCard),ground(Key) |
	Entries1 ! (Key-entry(IdCard,Snd,CutNotify)),
	id_card#attr_to_value_ok(IdCard, user, Value,Ok),
	insert_user(Ok?, Info, Info',  Value?, Key),
	self.

insert_user(Ok, D, D1, User, Key) :-
    Ok = true | insert(D, D1, User, Key);
    Ok =\= true | D1 = D, User = _, Key = _.

insert(List, List1, User, Key) :-

    List ? (User - Keys) | 
        List1 = [(User - [Key | Keys]) | List'?];

    List ? (Small - Keys), Small @< User |
        List1 !  (Small - Keys),
        self;

    List ? (Big - Keys), User @< Big |
        List1 = [(User - [Key]),  (Big - Keys) | List'?];

    List = [] | List1 = [(User - [Key])]. 

/* Calls concerning both entry and IdCard  */
query_and_update(Id,UpdateIdCard, Olds, Doors, Doors1, Ok) :-
    ground(Id) |
        replace(Id,OldDoorEntry,NewDoorEntry?,Doors,Doors1,Ok1),
        query_and_update1(Ok1?,Ok,OldDoorEntry?,NewDoorEntry,UpdateIdCard, Olds).

query_and_update1(Ok,Ok1,OldDoorEntry,NewDoorEntry,UpdateIdCard, Olds) :-
    Ok=true, OldDoorEntry = entry(OldIdCard,Snd,DiscardNotify) |
        id_card#query_update(OldIdCard,UpdateIdCard,NewIdCard, Olds),
        NewDoorEntry = entry(NewIdCard?,Snd,DiscardNotify),     
        Ok1=true;

    Ok=\=true |
        UpdateIdCard = _, Olds = [],
        NewDoorEntry = OldDoorEntry,
        Ok1=Ok.

update_ids(DoorsInfo, Doors, Doors1, Ok) :-
  DoorsInfo = [] |
	Doors1 = Doors,
	Ok = true;

  DoorsInfo ? Id - IdCard, constant(Id), listener(IdCard) |
	update_id(Id, IdCard, Doors, Doors', Ok1),
	report_problem(Ok1?, Id, IdCard),
	self.

report_problem(Ok, Id, IdCard) :-
  Ok = true |
	IdCard = _, Id = _;

  Ok =\= true |
	IdCard = _, Id = _,
	true.
%	computation#display(term, "door not viable"(Id - IdCard), type(ground)).

update_id(Id,UpdateIdCard,Doors,Doors1, Ok) :-
	ground(Id) |
		replace(Id,OldDoorEntry,NewDoorEntry?,Doors,Doors1,Ok1),
		update_id1(Ok1?,Ok,OldDoorEntry?,NewDoorEntry,UpdateIdCard).


update_id1(Ok,Ok1,OldDoorEntry,NewDoorEntry,UpdateIdCard) :-
	Ok=true, OldDoorEntry = entry(OldIdCard,Snd,DiscardNotify) |
		id_card#update(OldIdCard,UpdateIdCard,NewIdCard),
 		NewDoorEntry = entry(NewIdCard?,Snd,DiscardNotify),	
		Ok1=true;
	Ok=\=true |
		UpdateIdCard = _,
		NewDoorEntry = OldDoorEntry,
		Ok1=Ok.

replace_id(Id,UpdateIdCard,Doors,Doors1, Ok) :-
	ground(Id) |
		replace(Id,OldDoorEntry,NewDoorEntry?,Doors,Doors1,Ok1),
		replace_id1(Ok1?,Ok,OldDoorEntry?,NewDoorEntry,UpdateIdCard).


replace_id1(Ok,Ok1,OldDoorEntry,NewDoorEntry,UpdateIdCard) :-
	Ok=true, OldDoorEntry = entry(_OldIdCard,Snd,DiscardNotify) |
 		NewDoorEntry = entry(UpdateIdCard,Snd,DiscardNotify),	
		Ok1=true;
	Ok=\=true |
		UpdateIdCard = _,
		NewDoorEntry = OldDoorEntry,
		Ok1=Ok.

/* 
   given an idcard, find all keys which have the same values (as in the card)
   for all the attributes mentioned in the given card.
*/

get_keys_with_given_attributes(Card, Doors, Doors1, KeyList) :-
    Doors = {NextId,Entries} |
        get_keys_with_given_attributes1(Card, Entries?, Entries', KeyList),
        Doors1 = {NextId,Entries'?}.

get_keys_with_given_attributes1(Card, Entries, Entries1, KeyList) :-
 
    Entries  ? Key-entry(IdCard,Snd,DiscardNotify), 
    ground(Card), listener(IdCard), listener(Key) |
        id_card#query_ok(IdCard, Card, Ok),
        insert_if_ok(Ok?, KeyList, KeyList'?, Key),
        Entries1 ! Key - entry(IdCard,Snd,DiscardNotify),
        self;

    Entries=[] | Entries1=[], KeyList = [], Card = _.

get_id_cards_with_given_attributes(Card, Doors, Doors1, IdCards) :-
    Doors = {NextId,Entries} |
        get_id_cards_with_given_attributes1(Card, Entries?, Entries', IdCards),
        Doors1 = {NextId,Entries'?}.

get_id_cards_with_given_attributes1(Card, Entries, Entries1, IdCards) :-
 
    Entries  ? Key-entry(IdCard,Snd,DiscardNotify), 
    ground(Card), ground(Key), listener(IdCard) |
        id_card#query_ok(IdCard, Card, Ok),
        insert_if_ok(Ok?, IdCards, IdCards'?, Key - IdCard),
        Entries1 ! Key - entry(IdCard,Snd,DiscardNotify),
        self;

    Entries=[] | Entries1=[], IdCards = [], Card = _.

insert_if_ok(Ok, List1, List2, Value) :-
    Ok = true | List1 = [Value | List2];
    Ok =\= true | List1 = List2, Value = _.

query(Key, Doors,Doors1,IdFrame, Ok) :-
	Doors = {NextId,Entries} |
		query1(Key, Entries?,Entries',IdFrame, Ok),
		Doors1 = {NextId,Entries'?}.


query1(Key,Entries,Entries1, IdFrame, Ok) :-
	+hash#find(Key,Entries,Entries1,Ok);

	Entries=[] | Entries1=[], Ok=false(not_found(Key)), IdFrame = _;
	Entries  ? Key-entry(IdCard,Snd,DiscardNotify), listener(IdCard) |
 		id_card#query_ok(IdCard, IdFrame, Ok),
		Entries1=[Key-entry(IdCard,Snd,DiscardNotify)|Entries'].


id_to_id_card(Key,IdCard,Doors,Doors1,Ok) :-
	Doors = {NextId,Entries} |
	id_to_id_card1(Key,IdCard,Entries,Entries1,Ok),
	Doors1 = {NextId,Entries1?}.

id_to_id_card1(Key,IdCard,Entries,Entries1,Ok) :-
	+hash#find(Key,Entries,Entries1,Ok);

	Entries=[] | Entries1=[], Ok=false(not_found(Key)), IdCard = [];
	Entries  ? Key-entry(IdCard1,Snd,DiscardNotify), listener(IdCard1) |
		IdCard=IdCard1,
		Entries1=[Key-entry(IdCard1,Snd,DiscardNotify)|Entries'],
		Ok=true.

id_attr_to_value(Key,Attr,Value,Doors,Doors1,Ok) :-
	Doors = {NextId,Entries} |
	id_attr_to_value1(Key,Attr,Value,Entries,Entries1,Ok),
	Doors1 = {NextId,Entries1?}.

id_attr_to_value1(Key,Attr,Value,Entries,Entries1,Ok) :-
	+hash#find(Key,Entries,Entries1,Ok);

	Entries=[] |
		Entries1=[], Ok=false(not_found(Key,Attr)), Value = unknown;
	Entries  ? Key-entry(IdCard,Snd,DiscardNotify), listener(IdCard)|
		id_card#attr_to_value_ok(IdCard,Attr,Value,Ok),
		Entries1=[Key-entry(IdCard,Snd,DiscardNotify)|Entries'].


attr_value_to_id(AttrValue,Doors,Doors1,Id,Ok) :-
	Doors = {NextId,Entries} |
	attr_value_to_id1(AttrValue,Entries,Entries1,Id,Ok),
	Doors1 = {NextId,Entries1?}.

attr_value_to_id1(AttrValue,Entries,Entries1,Id,Ok) :-
	Entries=[] | Entries1=[], Ok=false(not_found(AttrValue)), Id = unknown;

	Entries  ? Key-entry(IdCard,Snd,DiscardNotify), 
	ground(Key),  ground(AttrValue), listener(IdCard) |
		Entries1 ! Key-entry(IdCard,Snd,DiscardNotify),
 		attr_value_to_id2(AttrValue,Key, IdCard,
				 Entries', Entries1', Id, Ok).


attr_value_to_id2(AttrValue,Key,IdCard,Entries,Entries1,Id,Ok) :-
  IdCard=[] | 
	Key = _, 	
	attr_value_to_id1(AttrValue,Entries,Entries1,Id,Ok);

  IdCard ? AttrValue |
	 Id=Key, Ok=true, Entries1=Entries, IdCard' = _;

  IdCard ? AttrValue1, AttrValue=\=AttrValue1 | 
	self.


send(To,From,Message,Entries,Entries1,Ok1) :-
		To = [Id|To'] |
		replace(Id,Value,Value1?,Entries,Entries1,Ok2),
		send_to_id1(Ok2?,Ok1,To',From,Message,Value?,Value1).

send_to_id1(Ok,Ok1,To,From,Message,Value,Value1) :-
  Ok = false(Error) |
	Ok1=false([Error,message(To,From,Message)]), 
	Value=_, Value1=unknown;

  Ok = true, Value=entry(IdCard,Snd?,DiscardNotify) |
	Snd ! message(To,From,Message,Ok1),
	Value1=entry(IdCard,Snd',DiscardNotify).



