/* $Header: /home/qiana/Repository/Logix/system/Doors/application.cp,v 1.1 1999/07/09 07:03:24 bill Exp $ */

-language([inherit,dfcp]).
-export(generic_data_to_snapshot).
-mode(interrupt).

application_table(Rcv, AppTbl) :-
+tables#table(Rcv, AppTbl);

    Rcv ? initialize_application_table(Ok?), AppTbl = application |
	Rcv'' = [create_table(application, {from, content}, [], Ok)
	| Rcv'?],
	self.

nil_application_snapshot(Rcv) :-
  Rcv ? (_From : make_snapshot(application, Snapshot?, Ok?)) |
	Snapshot = [],
	Ok = true,
	self.

generic_application_snapshot(Rcv, AppTbl) :- 
  Rcv ? (_From : make_snapshot(application, Snapshot?, Ok?)), ground(AppTbl) |
	Rcv'' = [get_table_data(AppTbl, Data, Ok) | Rcv'?],
	generic_data_to_snapshot(application, Data?, Snapshot),
	self.

generic_data_to_snapshot(Type, Data, Snapshot) :-
  Data ? {From, Content}, ground(Type) |
	processor#interface(gmdate(Date)),
	Snapshot ! ([application_itself] : event(Type, Date?,(From : Content))),
	self;

  Data = [] |
	Type = _,
	Snapshot = [].

generic_application_event_filter(Type,SelfConnect, SelfConnect1, Event, ListenerName, Ok) :-

  Type = application |
	SelfConnect = SelfConnect1,
	Event = _, ListenerName = _, Ok = true.
