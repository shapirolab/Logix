/* $Header: */
-language([inherit,dfcp]).
-mode(interrupt).
-export([create_table, get_table_data, get_table_description,
	insert_table_data, delete_table_data, delete_indexed_table_data,
	push_table_data, pop_table_data]
).

/* Beginnings of "relational database" table package.
 * This will be improved and generalized - for now contains sufficient
 * functionality for the needs of the Active Mail application
*/
Table ::= table(Name, Description, Data).
Name ::= String.
Description ::= Tuple(String). % each element in the tuple describes the column
Data ::= [Tuple].

table(Rcv, Table) :-
  Rcv ? create_table(Name, Description, Data, Ok?),
  string(Name), Name = Table | % assumes Table is initialized to its name
	Rcv'' = [listen_to([self], [Name], [], Ok1) | Rcv'?],
	create_table(Name, Description, Data, Table', Ok),
	ok(Ok1?, ok_listen),
	self;

  Rcv ? get_table_data(Table, Data?, Ok?) |
	get_table_data(Table, Data, Ok),
	self;

  Rcv ? get_table_description(Table, Description?, Ok?) |
	get_table_description(Table, Description,  Ok),
	self;

  Rcv ? insert_table_data(Table, Data, Ok?) |
	insert_table_data(Table,  Data, Table', Ok),
	self;

  Rcv ? ([self] : event(Name, _Date, refresh(_Data))),
  Table = table(Name, _Description, _OldData) |
	self;

  Rcv ? ([self] : event(Name, _Date, Data)), Table = table(Name, _, _),
  Data = (From : Content) |
	Rcv'' = [insert_table_data(Table, {From, Content}, Ok) | Rcv'?],
	ok(Ok?, insert_from_event),
	self;

  Rcv ? ([self] : event(Name, _Date, Data)), Table = table(Name, _, _), 
  Data=\=refresh(_), Data =\= (_ : _) |
	Rcv'' = [insert_table_data(Table, Data, Ok) | Rcv'?],
	ok(Ok?, insert_from_tuple_event),
	self.

/* Rudimentary Table Routines */
create_table(Name, Description, Data, Table, Ok) :-
  string(Name), tuple(Description), list(Data) |
	Table = table(Name, Description, Data),
	Ok = true;
  
  string(Name), tuple(Description), Data = [] |
	Table = table(Name, Description, Data),
	Ok = true;

  otherwise |
	Name = _, Data = _, Description = _,
	Ok = false("bad table format"),
	Table = [].
   
get_table_data(Table, Data, Ok) :-

  Table = table(_Name,  _Description, TableData) |
	Data = TableData,
	Ok  = true;

  otherwise |
	Data = unknown, Table = _,
	Ok = false("bad table format").

get_table_description(Table, Description, Ok) :-
  Table = table(_Name,  TableDescription, _Data) |
	Description = TableDescription,
	Ok  = true;

  otherwise |
	Description = unknown, Table = _,
	Ok = false("bad table format").

insert_table_data(Table1, Data, Table2, Ok) :-
 Table1 = table(Name, Description, OldData), tuple(Data), ground(Description),
 string(Name), listener(Data) |
    	arity(Description, A1), arity(Data, A2), 
	do_insert(A1?, A2?, Name, Description, OldData, Data, Table2, Ok);

 Table1 = table(Name, Description, OldData), ground(Description), string(Name),
 Data = [Data1 | _] | 		% for now only verify first element
	arity(Description, A1), arity(Data1, A2),		
	do_insert(A1?, A2?, Name, Description, OldData, Data, Table2, Ok);

  otherwise | Data = _,
	Table2 = Table1,
	Ok = false("cannot insert").

do_insert(Description_Arity, Data_Arity, Name, Description, OldData, Data,
								Table, Ok) :-
  Description_Arity = Data_Arity, tuple(Data) |
	append(OldData, [Data], NewData),
	Table = table(Name, Description, NewData?),
	Ok = true;

 Description_Arity = Data_Arity, list(Data) |
	append(OldData, Data, NewData),
	Table = table(Name, Description, NewData?),
	Ok = true;

  otherwise |
	Data_Arity = _, Description_Arity = _, % for lint
	Ok = false(("cannot insert - bad data format" : Data)),
	Table = table(Name, Description, OldData).

pop_table_data(Table1, TopRecord, Table2, Ok) :-
  Table1 = table(Name, Description, OldData), ground(Description),
  ground(OldData), string(Name), OldData ? Top |
	TopRecord = Top,
	Table2 = table(Name, Description, OldData'),
	Ok = true;

  Table1 = table(_Name, _Description, []) |
	TopRecord = [],
	Table2 = Table1,
	Ok = true;

  otherwise, ground(Table1) |
	TopRecord = undefined,
	Ok = false(("cannot pop " - Table1)),
	Table2 = Table1.
	
push_table_data(Table1, Data, Table2, Ok) :-
 Table1 = table(Name, Description, OldData), tuple(Data), ground(Description),
 string(Name), listener(Data) |
    	arity(Description, A1), arity(Data, A2), 
	do_push(A1?, A2?, Name, Description, OldData, Data, Table2, Ok);

 Table1 = table(Name, Description, OldData), ground(Description), string(Name),
 Data = [Data1 | _] | 		% for now only verify first element
	arity(Description, A1), arity(Data1, A2),		
	do_push(A1?, A2?, Name, Description, OldData, Data, Table2, Ok);

  otherwise | Data = _,
	Table2 = Table1,
	Ok = false("cannot push").

do_push(Description_Arity, Data_Arity, Name, Description, OldData, Data,
								Table, Ok) :-
  Description_Arity = Data_Arity, tuple(Data) |
	append([Data], OldData, NewData),
	Table = table(Name, Description, NewData?),
	Ok = true;

 Description_Arity = Data_Arity, list(Data) |
	append(Data, OldData, NewData),
	Table = table(Name, Description, NewData?),
	Ok = true;

  otherwise |
	Data_Arity = _, Description_Arity = _, % for lint
	Ok = false(("cannot push - bad data format" : Data)),
	Table = table(Name, Description, OldData).

% VERY RUDIMENTARY: can delete based on one field only
delete_table_data(Table1, Key, Table2, Ok) :-
	delete_indexed_table_data(Table1, 1, Key, Table2, Ok).

delete_indexed_table_data(Table1, Index, Key, Table2, Ok):-
 Table1 = table(Name, Description, OldData), ground(Key) | 
	delete(Index, Key, OldData, NewData, Ok),
	Table2 = table(Name, Description, NewData?).


delete(Index, Key, OldData, NewData, Ok) :-
  OldData ? Record, ground(Record), ground(Key), ground(Index) |
	arg(Index, Record, Value),
	delete_if_match(Key, Value?, Record, NewData, NewData'?),
	self;

  OldData = [] | Index = _, Key = _,
	NewData = [],
	Ok = true;

  otherwise |
	Ok = false(("cannot delete " - Index(Key))),
	NewData = OldData.

delete_if_match(Key, Value, Record, NewData1, NewData2) :-
  Key = Value |
	NewData1 = NewData2,
	Record = _;

  Key =\= Value |
	NewData1 = [Record | NewData2].

/* MORE : query services must be added */
