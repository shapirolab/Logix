/*

Hierarchy Server of Logix System

Bill Silverman

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:02:50 $
Currently locked by 	$Locker:  $
			$Revision: 1.1.1.1 $
			$Source: /home/qiana/Repository/Logix/hierarchy_server.cp,v $

Copyright (C) 1987, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([start/1]).
-language(compound).
-mode(trust).

Stream ::= [Any].
List ::= [Any].
Id, Scope ::= [Name].
  Name ::= String.
SystemReply ::= true ; false(Any).

Channel ::= Vector.

% hierarchy server goals

IHS, NewS, OldS ::= [HSR].
HSR ::= get_service(Id, Answer) ;
	locate(Id, Response) ;
	add_service(Id, AddService, SystemReply) ;
	services(Id, [Any]) ;
	close(Id, SystemReply) ;
	filter(NewS, OldS).

  Answer ::= director(Closed) ;
	     director(Being, ModuleKind, Closed) ;
	     module(Being, ModuleKind, Closed) ;
	     relay(Channel, Closed) ;
	     import(Id, Closed).

    Being ::= Binary ; Channel ; Id.
      Binary ::= String.	% Module

    AddService ::= director(UID) ;
		   director(UID, BinMode) ;
		   module(Binary, UID).

  Response ::= false(Any) ;
	       module(UID) ;
	       UID.

% find(Id, Reply).

  Reply ::= found(NodeData, NodeData, Proceed) ;
	    not_found(NodeData, Open) ;
	    invalid_key(Any).

    NodeData ::= Kind(Being, NodeDescriptor, Close).
      Kind ::= director ; module ; import, exported.
      NodeDescriptor ::= BinMode(UID).
        BinMode ::= binary ; auto ; query.
        UID ::= String.
      Close, Closed ::= close.
    Proceed ::= proceed ; close ; replace(Closed).
    Open ::= open(Closed) ; abort.

% get_node(UID, Name, Scope, BinMode, Result)

Result ::= import;
	   director(UID);
	   module(Binary, UID, Date).
  Date ::= String.


DSC, PIOC ::= Channel.
SCC ::= {Stream, Any, Any, Channel}.


procedure start(IHS).

start(IHS) :-
	processor # file(working_directory(UID), true),
	server(PIN, FPS'),
	FPS ! find([], not_found(director([], binary(UID), _Close), open(_))),
	domain_server # process_dictionary(FPS, Closes),
	close_services(Closes),
	make_channel(PIOC, PIN),
	hierarchy(IHS, PIOC).

/*
** A Node of the hierarchy is a process node.  Its process manages:
**
**   Identifier,	Inverse list of names (root identifier = [])
**
**   NodeData = Kind(Being, BinMode(UID), Close)
**
**	Kind,		director, module, import
**	Being,		Channel | Module | Identifier (director/import)
**	BinMode,	director's auto-compile control ("binary" => none)
**	UID,		Full UNIX Path - unused for import ('')
**	Close,		= close =>  close all subs/indirect and service
**
**   Closed,		= close <= all subs closed by super
*/

Cs ::= [close(Id, NodeData)].

procedure close_services(Cs).

close_services(Cs) :-

    Cs ? close(_Id, _(_, _, close^)) |
	close_services;

    Cs = [] | true.


procedure hierarchy(IHS, PIOC).

hierarchy(IHS, PIOC) :-

    IHS ? get_service(Id, Answer) :
      write_channel(find(Id, Reply), PIOC) |
	found_node(Reply, Id, PIOC, Answer),
	hierarchy;

    IHS ? locate(Id, Response) :
      write_channel(find(Id, Reply), PIOC) |
	unique_id(Reply, Response),
	hierarchy;

    IHS ? add_service(Id, AddService, SystemReply),
    Id = [_ | Scope] :
      write_channel(find(Scope, Reply), PIOC) |
	add_service_data(AddService, BinMode, Data),
	add_service(Id, BinMode, Data, SystemReply, PIOC, PIOC', Reply),
	hierarchy;

    IHS ? services(Id, Services) :
      write_channel(services(Id, Services), PIOC) |
	hierarchy;

    IHS ? close(Id, SystemReply) :
      write_channel(find(Id, Reply), PIOC) |
	close_node(Reply, SystemReply),
	hierarchy;

    IHS ? filter(IHS'', IHS'^) |
	hierarchy;

    IHS = [] :
      close_channel(PIOC) .


procedure close_node(Reply, SystemReply).

close_node(Reply, SystemReply) :-

    Reply = found(NodeData, NodeData^, close^) :
      SystemReply = true ;

    Reply = not_found(_, abort^) :
      SystemReply = false(no_service) ;

    otherwise :
      SystemReply = false(Reply) .


procedure unique_id(Reply, Response).

unique_id(Reply, Response) :-

    Reply = found(NodeData, NodeData^, proceed^) |
	unique_id_response(NodeData, Response);

    Reply = not_found(_, abort^) :
      Response = false(not_found) ;

    otherwise :
      Response = false(Reply) .

procedure unique_id_response(NodeData, Response).

unique_id_response(NodeData, Response) :-

    NodeData = director(_, _(UID), _) :
      Response = UID ;

    NodeData = module(_, _(UID), _) :
      Response = module(UID) ;

    NodeData = exported(_, _(UID), _) :
      Response = module(UID) ;

    otherwise : NodeData = _,
      Response = false(not_node) .


AddData ::= exported(Binary, NodeDescriptor, Close) ;
	    director(Being, NodeDescriptor, Close) ;
	    false.

procedure add_service_data(AddService, BinMode, AddData).

add_service_data(AddService, BinMode, Data) :-

    AddService = director(UID),
    string(UID) :
      Data = director(_, BinMode(UID), _) ;

    AddService = module(Binary, UID),
    string(UID) :
      Data = exported(Binary, BinMode(UID), _) ;

    AddService = director(UID, Self),
    string(UID), string(Self) : BinMode = _,
      Data = director(_, Self(UID), _) ;

    otherwise : AddService = _, BinMode = _,
      Data = false .


procedure add_service(Id, BinMode, AddData, SystemReply, PIOC, PIOC, Reply).

add_service(Id, BinMode, Data, SystemReply, PIOC1, PIOC2, Reply) :-

    Reply = not_found(_, abort^) : Id = _, BinMode = _, Data = _,
      SystemReply = false(not_found),
      PIOC1 = PIOC2 ;
 
    Data =\= false,
    Reply = found(NodeData, NodeData^, proceed^),
    NodeData = director(_, {BinMode^, _}, Close) :
      SystemReply = true,
      write_channel(find(Id, Reply'), PIOC1, PIOC2) |
	added_service(Data, Reply', Close);

    otherwise,
    Reply = found(ND, ND^, proceed^) : Id = _, BinMode = _, Data = _,
      SystemReply = false(invalid),
      PIOC1 = PIOC2 ;

    otherwise : Id = _, BinMode = _, Data = _,
      SystemReply = false(Reply),
      PIOC1 = PIOC2 .

procedure added_service(AddData, Reply, Close).

added_service(Data, Reply, Close) :-

    Reply = not_found(Data^, open(Close)^) |
	true;

    Data = director(_, BinMode(UID), _),	% Change BinMode only
    Reply = found(director(Being, _(UID), Closed),
		  director(Being, BinMode(UID), Closed)^,
		  proceed^
	    ) : Close = _ ;

    otherwise,
    Reply = found(OldData, Data^, proceed^) : Close = _,
      OldData = _(_, _, close) ;

    otherwise : Data = _, Reply = _, Close = _ .


procedure found_node(Reply, Id, PIOC, Answer).

found_node(Reply, Id, PIOC, Answer) :-

    Reply = found(NodeData, NodeData', proceed^) : Id = _ |
	found_node_answer(NodeData, NodeData', Id, PIOC, Answer);

    Reply = not_found(NodeData, Open),
    Id ? _ :
      write_channel(find([], RootReply), PIOC) |
	reverse(Id', Di),
	director_list(Di, PIOC, RootReply, Ancestors),
	open_node(Ancestors, Id, PIOC, Answer, NodeData, Open);

    otherwise : Reply = _, Id = _, PIOC = _,
      Answer = import([], _) .

NodeData1 ::= NodeData.

procedure found_node_answer(NodeData, NodeData1, Id, PIOC, Answer).

found_node_answer(NodeData, NodeData1, Id, PIOC, Answer) :-

    NodeData = module(Being, _, Close) : Id = _, PIOC = _,
      NodeData = NodeData1 |
	found_module_node(Being, Close, Answer);

    NodeData = director(Being, NodeDescriptor, Close) :
      NodeData = NodeData1 |
	found_director_node(Being, NodeDescriptor, Close, Id, PIOC, Answer);

    NodeData = exported(Binary, NodeDescriptor, Close) : Id = _, PIOC = _,
      NodeData1 = module(Being, NodeDescriptor, Close),
      Answer = module(Binary, ModuleKind, Close) |
	module_kind(ModuleKind, Binary, Being);

    NodeData = import(TargetId, _, Close) : Id = _, PIOC = _,
      NodeData = NodeData1,
      Answer = import(TargetId, Close) .

procedure found_module_node(Being, Close, Answer).

found_module_node(Being, Close, Answer) :-

    module(Being) :
      Answer = module(Being, _, Close) ;

    channel(Being) :
      Answer = relay(Being, Close) .

procedure found_director_node(Being, NodeDescriptor, Close, Id, PIOC, Answer).

found_director_node(Being, NodeDescriptor, Close, Id, PIOC, Answer) :-

    Being = Id : NodeDescriptor = _, PIOC = _,
      Answer = director(Close) ;

    otherwise : Being= _, NodeDescriptor = _, Id = _, PIOC = _,
      Answer = director(Being, _, Close) ;

    NodeDescriptor = BinMode(UID) :
      Being = Becoming?,
      write_channel(get_node(UID, self, Id, BinMode, Result), PIOC) |
	self_service(Result, Becoming, Close, Id, Answer).

procedure self_service(Result, Being, Close, Id, Answer).

self_service(Result, Being, Close, Id, Answer) :-

    Result = module(Binary, _, _) : Id = _,
      Answer = director(Binary, ModuleKind, Close) |
	module_kind(ModuleKind, Binary, Being);

    otherwise : Result = _,
      Being = Id,
      Answer = director(Close) .

/************************** Add New Service **********************************/

Di ::= Id.
Ancestors, NDs ::= Directors ; none(Closed).
Directors ::= [director(Being, NodeDescriptor, Close)].

procedure director_list(Di, PIOC, Reply, Ancestors).
procedure director_list(Di, PIOC, Reply, Ancestors, NDs, Id).

director_list(Di, PIOC, Reply, Ancestors) + (NDs = [], Id = []) :-

    Reply = found(NodeData, NodeData^, proceed^) |
	director_answer(Di, PIOC, Ancestors, NDs, Id, NodeData);

    Reply = not_found(NodeData, Open),
    Id = [DirName | Scope],
    NDs = [_(_, BinMode(UID), Closed) | _] :
      write_channel(get_node(UID, DirName, Scope, BinMode, Result), PIOC) |
	director_result(Di, PIOC, Ancestors, NDs, Id, BinMode, Closed,
			NodeData, Open, Result
	).
/*
director_list(_, _, not_found(_, abort^), [], []).	%  root vanished ??
*/
director_list(_, _, _, none(_)^, _, _) :-
    otherwise |
	true.


procedure director_result(Di, PIOC, Ancestors, NDs, Id, BinMode, Closed,
				NodeData, Open, Result
).

director_result(Di, PIOC, Ancestors, NDs, Id, BinMode, Closed,
			NodeData, Open, Result
) :-
    Result = director(UID) :
      NodeData = director(_Being, BinMode(UID), _Close),
      Open = open(Closed) |
	director_answer(Di, PIOC, Ancestors, NDs, Id, NodeData).
director_result(_, _, none(Closed)^, _, _, _, Closed, _, abort^, _) :-
    otherwise |
	true.

procedure director_answer(Di, PIOC, Ancestors, NDs, Id, NodeData).

director_answer(Di, PIOC, Ancestors, NDs, Id, NodeData) :-

    Di ? D,
    NodeData = director(_, _, _) :
      Id' = [D | Id],
      write_channel(find(Id', Reply), PIOC) |
	director_list(Di', PIOC, Reply, Ancestors, [NodeData | NDs], Id');

    Di = [],
    NodeData = director(_, _, _) : PIOC = _, Id = _,
      Ancestors = [NodeData | NDs] ;

    otherwise,
    NodeData = _(_, _, Closed) : Di = _, PIOC = _, NDs = _, Id = _,
      Ancestors = none(Closed) .


procedure open_node(Ancestors, Id, PIOC, Answer, NodeData, Open).

open_node(Ancestors, Id, PIOC, Answer, NodeData, Open) :-

    Ancestors ? director(_, BinMode(UID), Closed),
    Id = [Name | Scope] :
      Open = open(Closed'),
      write_channel(get_node(UID, Name, Scope, BinMode, Result), PIOC) |
	opened_node(Ancestors', Id, PIOC, Answer, NodeData, BinMode,
			Closed, Closed', Result
	);

    Ancestors = none(Closed) : Id = _, PIOC = _, NodeData = _,
      Answer = import([], Closed),
      Open = abort .

Closed1, Closed2 ::= Closed.

procedure opened_node(Ancestors, Id, PIOC, Answer, NodeData, BinMode,
			Closed1, Closed2, Result
).

opened_node(Ancestors, Id, PIOC, Answer, NodeData, BinMode,
		Closed1, Closed2, Result
) :-
    Result = module(Binary, UID, _) : Ancestors = _, Id = _, PIOC = _,
      NodeData = module(Being, BinMode(UID), Close),
      Answer = module(Binary, ModuleKind, Close),
      Closed1 = Closed2 |
	module_kind(ModuleKind, Binary, Being);

    Result = director(UID) : Ancestors = _,
      NodeDescriptor = BinMode(UID),
      NodeData = director(Being, NodeDescriptor, Close),
      Closed1 = Closed2 |
	found_director_node(Being, NodeDescriptor, Close, Id, PIOC, Answer);

    Result = import,
    Id ? Name :
      NodeData = import(Being, BinMode(''), Close),
      Answer = import(Being, Closed2) |
	combine_closed(Closed, Closed1, Close, Closed2),
	import_node(Ancestors, Name, Id', PIOC, Closed, Being).

ModuleKind ::= procedures ; monitor(Channel).

procedure module_kind(ModuleKind, Binary, Being).

module_kind(monitor(MMC), _, MMC^).
module_kind(procedures, Binary, Binary^).

/*********************** I M P O R T   N O D E *******************************/

Equivalent ::= Id.

procedure import_node(Ancestors, Name, Id, PIOC, Close, Equivalent).

import_node(Ancestors, Name, Id, PIOC, Close, Equivalent) :-
    Id ? _ :
      SId = [Name | Id'],
      write_channel(find(SId, Reply), PIOC) |
	import_node_reply(Ancestors, SId, PIOC, Close, Equivalent, Reply).
import_node([], _, [], _, _, []^).


procedure import_node_reply(Ancestors, Id, PIOC, Close, Equivalent, Reply).

import_node_reply(Ancestors, Id, PIOC, Close, Equivalent, Reply) :-

    Reply = found(NodeData, NodeData^, proceed^),
    NodeData = import(Equivalent^, _, Close^) : Ancestors = _, Id = _,
						PIOC = _ ;
    Reply = found(NodeData, NodeData^, proceed^),
    NodeData = Kind(_, _, Close^),
    Kind =\= import : Ancestors = _, PIOC = _,
      Id = Equivalent ;

    Reply = not_found(NodeData, open(Closed2)^),
    Ancestors ? director(_, BinMode(UID), CloseSubs),
    Id ? Name :
      write_channel(get_node(UID, Name, Id', BinMode, Result), PIOC) |
	imported_node(Ancestors', Id, PIOC, CloseSubs, Closed2, BinMode,
			NodeData, Equivalent, Close, Result
	).

procedure imported_node(Ancestors, Id, PIOC, Closed1, Closed2, BinMode,
			NodeData, Equivalent, Close, Result
).

imported_node(Ancestors, Id, PIOC, Closed1, Closed2, BinMode,
		NodeData, Equivalent, Close, Result
) :-
    Result = director(UID) : Ancestors = _, PIOC = _,
      Closed1 = Closed2,
      NodeData = director(_Being, BinMode(UID), Close),
      Id = Equivalent ;

    Result = module(Binary, UID, _) : Ancestors = _, PIOC = _,
      Closed1 = Closed2,
      NodeData = exported(Binary, BinMode(UID), Close),
      Id = Equivalent ;

    Result = import,
    Id ? Name :
      NodeData = import(Equivalent, BinMode(''), Close) |
	combine_closed(Closed, Closed1, Close, Closed2),
	import_node(Ancestors, Name, Id', PIOC, Closed, Equivalent).


procedure combine_closed(Closed, Closed1, Close, Closed2).

combine_closed(close, _, _, close^).
combine_closed(_, close, _, close^).
combine_closed(_, _, close, close^).

/***************************** S E R V E R ***********************************/

WD ::= String.

procedure server([Any], [Any]).

server(PIN, FPS) :-

    PIN ? get_node(WD, Name, Scope, BinMode, Result) |
	concatenate(WD, Name, UName),
	processor # [file(directory(UName), Ok) | More],
	is_director(Ok, WD, Name, UName, Result, More, Scope, BinMode),
	server;

    otherwise,
    PIN ? Service :
      FPS ! Service |
	server;

    PIN = [] :
      FPS = [] .

/************************ G E T   S E R V I C E ******************************/

procedure is_director(Ok, WD, Name, Name, Result, [Any], Integer, Scope,
			BinMode
).

is_director(Ok, WD, Name, UName, Result, More, Scope, BinMode) :-

    Ok =\= true,
    BinMode = binary : Scope = _ |
	get_binary(WD, Name, UName, Result, More);

    Ok =\= true,
    BinMode =\= binary : UName = _,
      make_channel(C, O),
      More = [] |
	[system] #			% We know where computation_utils is !
	    computation_utils #
		call_output([[system] #	% We know where get_module is !
			       get_module #
				 update(C, Name, BinMode, Out, Result)
			    ],
			    Out, [prefix(Prefix), type(unparse)]
		),
	concatenate('compiling ', Name, Prefix),
	get_server(O, WD, Scope, Result);

    Ok = true : WD = _, Name = _, Scope = _, BinMode = _,
      Result = director(UPath),
      More = [] |
	concatenate(UName, "/", UPath).

procedure get_binary(WD, Name, Name, Result, [Any]).

get_binary(WD, Name, UName, Result, More) :-
    true :
      More = [file(info(Bin, DateBin), Ok1),
	      file(info(BinBin, DateBinBin), Ok2),
	      file(info(Dot_o, DateDot_o), Ok3),
	      file(info(BinDot_o, DateBinDot_o), Ok4)
	     | More''] |
	concatenate(UName, ".bin", Bin),
	concatenate(UName, ".o", Dot_o),
	concatenate4(WD, Name, BinBin, ".bin"),
	concatenate4(WD, Name, BinDot_o, ".o"),
	compare_dates(Ok1, DateBin, {Bin, DateBin},
		      Ok2, DateBinBin, {BinBin, DateBinBin},
		      Bins
	),
	include_dot_o(Ok3, DateDot_o, {Dot_o, DateDot_o}, Bins, Bins1),
	include_dot_o(Ok4, DateBinDot_o, {BinDot_o, DateBinDot_o},
			Bins1, AllBins
	),
	get_module(false, import, AllBins, Result, More'').

DateBin ::= Integer ; String.
PathDate ::= {String, Date}.

procedure compare_dates(Ok, DateBin, PathDate, Ok, DateBin, PathDate,
			[PathDate]
).

compare_dates(Ok1, DateBin, B, Ok2, DateBinBin, BB, Bins) :-

    Ok1 = true, Ok2 = true,
    DateBinBin =\= 0,
    DateBinBin @< DateBin :
      Bins = [B, BB] ;

    Ok1 = true, Ok2 = true,
    DateBinBin =\= 0,
    otherwise, DateBin =\= 0 :	% DateBin @=< DateBinBin
      Bins = [BB, B] ;

    Ok1 = true, Ok2 =\= true : BB = _, DateBin = _, DateBinBin = _,
      Bins = [B] ;

    Ok1 =\= true, Ok2 = true : B = _, DateBin = _, DateBinBin = _,
      Bins = [BB] ;

    Ok1 =\= true, Ok2 =\= true : B = _, BB = _, DateBin = _, DateBinBin = _,
      Bins = [] .

DateDot_o ::= Integer ; String.

procedure include_dot_o(Ok, DateDot_o, PathDate, [PathDate], [PathDate]).

include_dot_o(Ok, DateDot_o, D, Bins, AllBins) :-

    Ok =\= true : D= _, DateDot_o = _,
      AllBins = Bins;

    Ok = true,
    Bins = [B1], B1 = {_, DateBin1},
    DateDot_o @< DateBin1 :
      AllBins = [B1, D];

    Ok = true,
    Bins = [B1],
    otherwise: DateDot_o = _,		% DateDot_o @>= DateBin1
      AllBins = [D, B1];

    Ok = true,
    Bins = [B1, B2], B1 = {_, DateBin1},
    DateBin1 @< DateDot_o :
      AllBins = [D, B1, B2];

    Ok = true,
    Bins = [B1, B2], B2 = {_, DateBin2},
    DateDot_o @< DateBin2 :
      AllBins = [B1, B2, D];

    Ok = true,
    Bins = [B1, B2],
    otherwise :	DateDot_o = _,		% date of the .o file is in between
      AllBins = [B1, D, B2];

    Ok = true,
    Bins = [B1, B2, B3], B1 = {_, DateBin1},
    DateBin1 @< DateDot_o :
      AllBins = [D, B1, B2, B3];

    Ok = true,
    Bins = [B1, B2, B3], B3 = {_, DateBin3},
    DateDot_o @< DateBin3 :
      AllBins = [B1, B2, B3, D];

    Ok = true,
    Bins = [B1, B2, B3], B2 = {_, DateBin2},
    otherwise, 		% we know the date is between B1 and B3
    DateDot_o @< DateBin2 :
      AllBins = [B1, B2, D, B3];

    Ok = true,
    Bins = [B1, B2, B3],
    otherwise : DateDot_o = _,
      AllBins = [B1, D, B2, B3].

procedure get_module(Integer, Result, [String], Result, [Any]).

get_module(IOStatus, Last, Bins, Result, More) :-

    IOStatus =\= true,
    Bins ? {Name, Date} : Last = _,
      More ! file(get_module(Name, Module), IOStatus'),
      Last' = module(Module, Name, Date) |
	self;

    IOStatus =\= true,
    Bins = [] : Last = _,
      Result = import,
      More = [] ;

    IOStatus = true,
    Last = module(Module, _Name, _Date),
    module(Module) : Bins = _,
      Result = Last,
      More = [] ;

    IOStatus = true,
    otherwise :
      IOStatus' = false |
	self.


procedure get_server([Any], WD, Scope, Result).

get_server(O, WD, Scope, Result) :-

    O ? '_unique_id'(WD^) |
	get_server;

    O ? service_id(Scope^) |
	get_server;

    known(Result) : O = _, WD = _, Scope = _ .


/************************* A U X I L I A R Y *********************************/

RI, IR, RR ::= List.

procedure reverse(RI, IR).
procedure reverse(RI, IR, RR).

reverse(RI, IR) + (RR = []) :-

    RI ? E :
      RR' = [E | RR] |
	reverse;

    RI = [] :
      RR = IR .


procedure concatenate(String, String, String).

concatenate(String1, String2, String) :-
    string_to_dlist(String1, Cs, Cs1),
    string_to_dlist(String2, Cs1, []) |
	list_to_string(Cs, String).


procedure concatenate4(WD, Name, String, String).

concatenate4(WD, Name, String, Suffix) :-
    string_to_dlist(WD, CWD, CB),
    string_to_dlist("Bin/", CB, CN),
    string_to_dlist(Name, CN, Cb),
    string_to_dlist(Suffix, Cb, []) |
	list_to_string(CWD, String).
