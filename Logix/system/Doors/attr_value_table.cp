/* $Header: */
-language(dfcp).
-mode(interrupt).
-export(all).

/* Package for  keyed attribute/value tables.
 * These tables use the facilities already provided in hash.cp and id_card.cp
*/
Table ::= attr_value_table(Name, Entries).
Name ::= String.
Entries ::= [Key - IdCard].
IdCard ::= [(attr : value)].

table(Rcv, Table) :-
% BEGIN adding for voice
  Rcv ? retrieve_keys(Table, Keys?, Ok?) |
	retrieve_keys(Table, Keys, Table', Ok),
	self;

  Rcv ? get_subtable(Table, SubName, Attrs, Subtable1, Ok) |
	get_subtable(Table, SubName, Attrs, Subtable1, Table', Ok),
	self;

  Rcv ? query(Table, Key, IdFrame, Ok?) |
	query(Table, Key, IdFrame, Table', Ok),
	self;

  Rcv ? query_and_update(Table, Key, NewFrame, OldFrame, Ok?) |
	query_and_update(Table, Key, NewFrame, OldFrame, Table', Ok),
	self;
% END adding for voice 

  Rcv ? create_attr_value_table(Name, Data, Ok?),
  string(Name), Name = Table |
	create_table(Name, Data, Table', Ok),
	self;
  
  Rcv ? create_indexed_attr_value_table(Name, Count, Ok?),
  string(Name), Name = Table |
	create_indexed_table(Name, Count, Table', Ok),
	self; 

  Rcv ? get_table_data(Table, Data?, Ok?) |
        get_table_data(Table, Data, Ok),
        self;

  Rcv ? show(Table, Key, IdCard, Ok?) |
	show(Table, Key, IdCard, Table', Ok),
	self;

  Rcv ? insert(Table, Key, IdCard, Ok?) |
	insert(Table, Key, IdCard, Table', Ok),
	self;

  Rcv ? delete(Table, Key, Ok?) |
	delete(Table, Key, Table', Ok),
	self;

  Rcv ? replace(Table, Key, IdCard, Ok?) |
	replace(Table, Key, IdCard, Table', Ok),
	self;

  Rcv ? update(Table, Key, IdCard, Ok?) |
        update(Table, Key, IdCard, Table', Ok),
        self;
 
  Rcv ? set(Table, Key, IdCard, Ok?) |
        set(Table, Key, IdCard, Table', Ok),
        self.

/* Rudimentary Table Routines */
create_table(Name, Data, Table, Ok) :-
  string(Name), list(Data) |
	Table = attr_value_table(Name, Data),
	Ok = true;
  
  string(Name), Data = [] |
	Table = attr_value_table(Name, Data),
	Ok = true;

  otherwise |
	Name = _, Data = _,
	Table = undefined,
	Ok = false("bad table format").
   
get_table_data(Table, Data, Ok) :-

  Table = attr_value_table(_Name, TableData) |
	Data = TableData,
	Ok  = true;

  otherwise |
	Data = unknown, Table = _,
	Ok = false("bad table format").

show(Table1, Key, IdCard, Table2, Ok) :-
  Table1 = attr_value_table(Name, Entries) |
	hash#lookup(Key, IdCard, Entries, Entries', Ok),
	Table2 = attr_value_table(Name, Entries'?).

insert(Table1, Key, IdCard, Table2, Ok) :-
  Table1 = attr_value_table(Name, Entries) |
	hash#insert(Key, IdCard, Entries, Entries', Ok),
	Table2 = attr_value_table(Name, Entries'?).

delete(Table1, Key, Table2, Ok) :-
  Table1 = attr_value_table(Name, Entries) |
	hash#delete(Key, _, Entries, Entries', Ok),
	Table2 = attr_value_table(Name, Entries'?).


replace(Table1, Key, NewEntry, Table2, Ok) :-
  Table1 = attr_value_table(Name, Entries), constant(Key) |
	hash#replace_or_insert(Key, _OldEntry, 
		NewEntry, Entries, Entries', Ok),
	Table2 = attr_value_table(Name, Entries'?).

update(Table1, Key, UpdateIdCard, Table2, Ok) :-
  Table1 = attr_value_table(Name, Entries), constant(Key) |
        hash#replace_or_insert(Key, OldEntry, NewEntry?,
		 Entries, Entries', Ok1),
        update1(Ok1?, Ok, OldEntry?, NewEntry, UpdateIdCard),
        Table2 = attr_value_table(Name, Entries'?).

update1(Ok,Ok1,OldEntry,NewEntry,UpdateIdCard) :-
        Ok=true |
                id_card#update(OldEntry,UpdateIdCard,NewEntry),     
                Ok1=true;
        Ok=\=true |
                UpdateIdCard = _,
                NewEntry = OldEntry,
                Ok1=Ok.

set(Table1, Key, UpdateIdCard, Table2, Ok) :-
  Table1 = attr_value_table(Name, Entries), constant(Key) |
        hash#replace_or_insert(Key, OldEntry, NewEntry?,
				 Entries, Entries', Ok1),
        set1(Ok1?, Ok, OldEntry?, NewEntry, UpdateIdCard),
        Table2 = attr_value_table(Name, Entries'?).
 
set1(Ok,Ok1,OldEntry,NewEntry,UpdateIdCard) :-
        Ok=true |
                id_card#set(OldEntry,UpdateIdCard,NewEntry),     
                Ok1=true;
        Ok=\=true |
                UpdateIdCard = _,
                NewEntry = OldEntry,
                Ok1=Ok.

/* Indexed access methods */

create_indexed_table(Name, Count, Table, Ok) :-
  string(Name), integer(Count) |
	clear_table(Data, Count),
	Table = attr_value_table(Name, Data?),
	Ok = true;

  otherwise |
	Name = _, Count = _,
	Table = undefined,
	Ok = false("bad table format").

clear_table(Data,Count) :-
  Count > 0 |
        Data ! "" - undefined,
        Count--,
        self;
  Count = 0 |
        Data = [].
   

indexed_show_key_given_slot(Table1, Slot, Key, IdCard, Ok) :-
  Table1 = attr_value_table(_Name, Entries) |
	indexed_show_key1(Entries, Slot, Key, IdCard, Ok).

indexed_show_slot_given_key(Table1, Slot, Key, IdCard, Ok) :-
  Table1 = attr_value_table(_Name, Entries) |
	indexed_show_slot1(Entries, Slot, Key, IdCard, Ok).

indexed_insert(Table1, Slot, Key, IdCard, Table2, Ok) :-
  Table1 = attr_value_table(Name, Entries) |
	indexed_insert1(Entries, Slot, Key, IdCard, Entries', Ok),
	Table2 = attr_value_table(Name, Entries'?).

indexed_delete(Table1, Slot, Key, Table2, Ok) :-
  Table1 = attr_value_table(Name, Entries), integer(Slot) |
	indexed_delete1(Entries, Slot, Key, Entries', Ok),
	Table2 = attr_value_table(Name, Entries'?);

  Table1 = attr_value_table(Name, Entries), Slot = unknown |
	indexed_delete_key(Entries, Key, Entries', Ok),
	Table2 = attr_value_table(Name, Entries'?).

indexed_show_key1(Entries, Slot, Key, IdCard, Ok) + (Counter = 1) :-
  Entries ? _, Counter < Slot |
        Counter++,
        self;

  Entries ? FoundKey - FoundIdCard, Counter = Slot |
	Key = FoundKey,
	IdCard = FoundIdCard,
	Entries' = _, 
        Ok = true;

  otherwise |
	Key = undefined, IdCard = undefined,
	Entries = _, Counter = _, Slot = _, 
        Ok = false("illegal location").

indexed_show_slot1(Entries, Slot, Key, IdCard, Ok) + (Counter = 1) :-
  Entries ? Key - KeyCard |
	IdCard = KeyCard,
	Slot = Counter,
	Entries' = _, 
        Ok = true;

  Entries ? Key1 - _, Key1 =\= Key |
	Counter++,
	self;

  otherwise |
	Slot = undefined,
	IdCard = undefined,
	Counter =_, Entries = _, Key = _,
	Ok = false("no such person").
 
indexed_insert1(Entries, Slot, Key, IdCard, Entries1, Ok)
					+ (Counter = 1) :- % slot must be empty
  Entries ? Entry, Counter =\= Slot |
	Entries1 ! Entry,
	Counter++,
	self;

  Entries ? Entry, Counter = Slot, Entry = ""-undefined,
  ground(Key), ground(IdCard) |
	Entries1 ! Key - IdCard,
	Counter++,
	self;

  Entries ? Entry, Counter = Slot, Entry =\= ""-undefined |
	Entries1 = undefined, Entries' = _, Key = _, IdCard = _,
	Ok = false("nonempty slot");

  Entries = [] |
	Entries1 = [],
	Counter = _, Slot = _, Key = _, IdCard = _,
	Ok = true.

indexed_delete1(Entries, Slot, Key, Entries1, Ok)
					+ (Counter = 1) :- % slot must be empty
  Entries ? Entry, Counter =\= Slot |
	Entries1 ! Entry,
	Counter++,
	self;

  Entries ? Entry, Counter = Slot, Entry = Key - _ |
	Entries1 ! "" - undefined,
	Counter++,
	self;

  Entries = [] |
	Entries1 = [],
	Counter = _, Key = _, Slot = _,
	Ok = true.

indexed_delete_key(Entries, Key, Entries1, Ok) :-
  Entries ? Entry, Entry =\=  Key - _  |
	Entries1 ! Entry,
	self;

  Entries ? Entry,  Entry = Key - _ |
	Entries1 ! "" - undefined,
	self;

  Entries = [] |
	Entries1 = [],
	Key = _, 
	Ok = true.


/*
 *	Added query_and_update, to retreive attribute-value elements for a 
 *	given key, and update them with a new set.
 */

query_and_update(Table, Key, OldFrame, NewFrame, Table1, Ok) :-
  Table = attr_value_table(_Name, _Data), Key =\= "",
  listener(Table), ground(Key) |
%	computation # display(term, query_andupdate(Table, Key), type(ground)),
	query(Table, Key, OldFrame?, Table', Ok1),
	update_frame(Ok1?, Table'?, Key, NewFrame?, Table1, Ok)

; otherwise,
  Key = _, NewFrame = _ |
	OldFrame = _,
	Table1 = Table?,
	Ok = false("bad table format").

update_frame(Continue, Table, Key, NewFrame, Table1, Ok) :-
  Continue =\= true,
  Key = _, NewFrame = _ |
	Table1 = Table?,
	Ok = Continue?

; Continue = true |
%	computation # display(term, uf(Table??, Key??, NewFrame??), type(ground)),
%	computation # display(term, uft(Table??), type(ground)),
%	computation # display(term, ufk(Key??), type(ground)),
%	computation # display(term, ufnf(NewFrame??), type(ground)),
        update(Table?, Key?, NewFrame?, Table1, Ok).

/*
 *	Added query, to retrieve attribute-value elements for a given
 *	key.
 */

query(Table, Key, Frame, Table1, Ok) :-
  Table = attr_value_table(_Name, _Data), Key =\= "" , listener(Table) |
%	computation # display(term, q(Table??, Key??), type(ground)),
	show(Table, Key?, IdCard, Table1, Ok1),
	extract_frame(Ok1?, IdCard?, Frame?, Ok)

/* ; Table = attr_value_table(_Name, _Data), Key="" |
  Table1=Table?,
  Ok = true */

; otherwise,
  Key = _, Frame = _ |
	Table1 = Table?,
	Ok = false("bad table format").

extract_frame(Continue, IdCard, Frame, Ok) :-
  Continue =\= true,
  IdCard = _, Frame = _ |
	Ok = Continue?

; Continue = true |
%	computation # display(term, ef(IdCard??),type(ground)),
	id_card # query_ok(IdCard?, Frame, Ok).

/*
 *	Added retrieve_keys that will return all keys in an attribute value
 *	table.
 */

retrieve_keys(Table, Keys, Table1, Ok) :-
  Table = attr_value_table(Name, Data) |
	hash # retrieve_keys(Data?, Keys, Data1),
	Table1 = attr_value_table(Name?, Data1?),
	Ok = true

; otherwise |
	Keys = _,
	Table1 = Table?,
	Ok = false("bad table format").

/*
 *	Added get_subtable that will return a subtable of the given table
 *	specified by the desired attributes.
 */

get_subtable(Table, SubName, Attrs, Subtable1, Table1, Ok) :-
  Table = attr_value_table(Name, Data) |
	hash # retrieve_keys(Data?, Keys, Data1),
	create_table(SubName?, [], Subtable, Ok1),
%	computation#display(term,{SubName??,Keys??},type(ground)),
%	computation#display(term,data(Data1??),type(ground)),
	process_keys(Ok1?, Keys?, Attrs, attr_value_table(Name?, Data1?), Subtable?,
							 Table1, Subtable1, Ok)

; otherwise,
  SubName = _,
  Attrs = _ |        
	Subtable1 = _,
	Table1 = Table?,
%	computation#diag([Table??]),
        Ok = false("bad table format").
 
process_keys(Continue, Keys, Attrs, Table, Subtable, Table1, Subtable1, Ok) :-
  Continue =\= true,
  Keys = _,
  Attrs = _ |
	Subtable1 = Subtable?,
	Table1 = Table?,
	Ok = Continue?

; Continue = true,
  Keys = [],
  Attrs = _ |
	Subtable1 = Subtable?,
	Table1 = Table?,
	Ok = true

; Continue = true,
  Keys ? Key , Key =\= "", ground(Key), listener(Attrs) |
	show(Table?, Key, IdCard, Table', Ok1),
%	computation#display(term,{key, Key},type(ground)),
	process_id_card(Ok1?, Key, IdCard?, Attrs, Subtable?, 
							Subtable', Continue'),
	Attrs' = Attrs,
	self

; Continue = true,
  Keys ? Key , Key = "" |
  self

; otherwise,
  Continue = _,
  Keys = _,
  Attrs = _ |
	Subtable1 = Subtable?,
        Table1 = Table?,
        Ok = false("Bad key list").

process_id_card(Continue, Key, IdCard, Attrs, Subtable, Subtable1, Ok) :-
  Continue =\= true,
  Key = _,
  IdCard = _,
  Attrs = _ |
	Subtable1 = Subtable?,
	Ok = Continue?

; Continue = true |
%	computation#display(term,card(IdCard??),type(ground)),
	create_sub_card(Attrs?, SubCard),
	listen2(SubCard?, SubCard1, SubCard2),
	id_card # query_ok(IdCard?, SubCard1?, Ok1),
	insert_sub_card(Ok1?, Key?, SubCard2?, Subtable?, Subtable1, Ok).

create_sub_card(Attrs, SubCard) :-
  string(Attrs) |
	SubCard = [(Attrs : _Value)]

; Attrs = [] |
	SubCard = []

; Attrs ? Attr |
	SubCard ! (Attr? : _Value),
	self

; otherwise,
  Attrs = _ |
	SubCard = [].

insert_sub_card(Matched, Key, SubCard, Subtable, Subtable1, Ok) :-
  Matched =\= true,
  Key = _,
  SubCard = _ |
	Subtable1 = Subtable?,
	Ok = true

; Matched = true |
        update(Subtable?, Key?, SubCard?, Subtable1, Ok).
