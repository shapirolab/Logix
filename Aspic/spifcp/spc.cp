/*
Precompiler for Stochastic Pi Calculus - Output Phase.

Bill Silverman, February 1999.

Last update by		$Author: bill $
		       	$Date: 2002/07/07 17:28:45 $
Currently locked by 	$Locker:  $
			$Revision: 1.2 $
			$Source: /home/qiana/Repository/Aspic/spifcp/spc.cp,v $

Copyright (C) 2000, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export(stochasticize/2).
-language([evaluate, compound, colon]).

-include(spi_constants).

stochasticize(In, Terms) :-
	stream#hash_table(HashTable),
	procedure_channels,
	output.

  procedure_channels(In, HashTable, ProcessTable) :-

    In ? conflict(_Atom, _RHSS, _Procedure) |
	self;

    In ? Type(Atom, _RHSS, _Procedure), Type =\= conflict,
    arg(1, Atom, Name) :
      HashTable ! lookup(Name, Channels, _Old, _Found) |
	utils#tuple_to_dlist(Atom, [_ | Variables], []),
	extract_spi_channels(Variables, Channels),
	self;

    In =?= [] :
      ProcessTable = HashTable.

/*
** output/3
**
** Input:
**
**   In is a stream of  Mode(Atom, RHSS, Procedure).
**
**      Mode is one of {export,
**			none, no_guard,
**			compare, logix,
**			communication}.
**
**      Atom is an Fcp atom of a compound procedure:
**
**        ProcedureName(<arguments>)
**  or
**        ProcedureName
**
**      RHSS is the right-hand-side of the compound procedure, except
**      that communication requests are reperesented by:
**
**		request(Type, ChannelName, Multiplier, Tag)
**
**      Procedure is [] or the compound procedure's communication part.
**
**        ProcedureName.comm(<arguments'>) :- <compound rhs>
**
** Output:
**
**   Terms is a stream of compound procedures.
*/

output(In, ProcessTable, Terms) :-

    /* Discard conflicted code. */
    In ? conflict(_Atom, _RHSS, _Procedure) |
	self;

    In ? export(Atom, RHSS, _Procedure) :
      Terms ! (Atom'? :- NewRHSS?) |
	utilities#tuple_to_atom(Atom, Atom'),
	/* Eliminate unused global channels for this export. */
	update_globals(RHSS, RHSS', ProcessTable, ProcessTable'?),
	kluge_globals,
	self;

    /* We could add a check here for unused channels, i.e. ones
       that are not needed by the derived procedure. */
    In ? outer(Atom, RHSS, _Procedure) :
      Terms ! (Atom :- NewRHSS?) |
	kluge_news,
	self;

    In ? Mode(Atom, RHSS, Procedure),
    Mode =\= outer, Mode =\= export, Mode =\= conflict,
    arg(1, Atom, Name),
    Arity := arity(Atom) - 1,
    make_tuple(Arity, ProcessId)  :
      ProcessTable ! member(Name, Channels, _Ok),
      Terms ! (Atom :- NewRHSS?) |
	prototype_channel_table,
	utilities#untuple_predicate_list(';', RHSS, RHSS'),
	make_process_id,
	stochastic;

    In = [] :
      Terms = [] |
	ProcessTable = [].

  make_process_id(Arity, Atom, ProcessId) :-

    Arity-- > 0,
    arg(Arity, Atom, Arg),
    arg(Arity, ProcessId, A) :
      A = Arg |
	self;

    Arity =< 0 :
      Atom = _,
      ProcessId = _.


stochastic(In, ProcessTable, Terms, ProcessId, RHSS, Prototype, Procedure,
		NewRHSS) :-

    Procedure =\= (_ :- _) :
      ProcessId = _ |
	analyze_rhss(RHSS, Prototype, [], ChannelTables,
			ProcessTable, ProcessTable'?),
	update_rhss(false, RHSS, ChannelTables,
			ProcessTable', ProcessTable''?, Rhss),
	utilities#make_predicate_list(';', Rhss, NewRHSS),
	output;

    Procedure =?= (Atom :- Communicate1),
    RHSS =?= [(Ask : Requests | Body)] :
      Tell =
	write_channel(start(ProcessId, Operations?, `"Message.",
					`spifcp(chosen)), `"Scheduler."),
      NewRHSS = (Ask'? : Tell | Body),
      Terms ! (Atom :- Communicate2?) |
	utilities#untuple_predicate_list(',', Ask, Asks'),
	analyze_multipliers(Requests''?, Asks, Asks'?),
	utilities#make_predicate_list(',', Asks?, Ask'),
	utilities#untuple_predicate_list(',', Requests, Requests'),
	utilities#untuple_predicate_list(';', Communicate1, Communicate1'),
	analyze_rhss(Communicate1'?, Prototype, Requests'?, ChannelTables,
			ProcessTable, ProcessTable'?),
        /* Check for homodimerized communication (send and receive on the
           same channel). Convert one of the requests to a "dimer" request
           and suppress the other, retaining its .comm clause.             */
	dimerize_requests(Requests'?, Requests''),
	utils#binary_sort_merge(Requests''?, Communications),
	rewrite_clauses,
	communication_to_operations,
	update_rhss(true, Communicate2''?, ChannelTables,
			ProcessTable', ProcessTable''?, Communicate2'),
	utilities#make_predicate_list(';', Communicate2'?, Communicate2),
	output.

  communication_to_operations(Communications, Operations) :-

    Communications ? request(send, ChannelName, Multiplier, Tag) :
      Operations ! {SPI_SEND, ChannelName, `ChannelName, Multiplier, Tag} |
	self;

    Communications ? request(receive, ChannelName, Multiplier, Tag) :
      Operations ! {SPI_RECEIVE, ChannelName, `ChannelName, Multiplier, Tag} |
	self;

    Communications ? request(dimer, ChannelName, Multiplier, Tags) :
      Operations ! {SPI_DIMER, ChannelName, `ChannelName, Multiplier, Tags} |
	self;

    Communications = [] :
      Operations = [].

  analyze_multipliers(Requests, Multipliers,  Asks) :-

    Requests ? _write_message(Ms, _Channel),
    Ms =?= _Type(_Id, _Message, _Tag, Multiplier, _Chosen),
    Multiplier =?= `_ :
      Multipliers ! integer(Multiplier),
      Multipliers' ! (Multiplier > 0) |
	self;

    Requests ? _Other,
    otherwise |
	self;

    Requests =?= [] :
     Multipliers = Asks.

  prototype_channel_table(Channels, Prototype) :-

    Channels ? Name :
      Prototype ! Name(0, Name) |
	self;

    Channels = [] :
      Prototype = [].


update_globals(RHSS, NewRHSS, ProcessTable, NextProcessTable) :-

    RHSS =?=  (spi_monitor # global_channels(Globals, Scheduler), Body) :
      NewRHSS = (spi_monitor # global_channels(NewGlobals?, Scheduler), Body),
      ProcessTable = [member(Body, Channels, Ok) | NextProcessTable] |
	update_globals1;

    otherwise :
      NewRHSS = RHSS,
      NextProcessTable = ProcessTable.

  update_globals1(Globals, Channels, NewGlobals, Ok) :-

    Ok = true,
    Globals ? Global, arg(1, Global, Name) |
	update_global_list(Name, Channels, Global, NewGlobals, NewGlobals'?),
	self;

    otherwise :
      Ok = _,
      Channels = _,
      NewGlobals = Globals.

  update_global_list(Name, Channels, Global, NewGlobals, NextNewGlobals) :-

    Channels ? Name :
      Channels' = _,
      NewGlobals = [Global | NextNewGlobals];

    Channels ? Other, Other =\= Name |
	self;

    Channels =?= [] :
      Name = _,
      Global = _,
      NewGlobals = NextNewGlobals.


rewrite_clauses(Communicate1, Requests, Communications, Communicate2) :-

    true :
      Communications = _,
      Requests = _,
      Communicate2 = Communicate1.


analyze_rhss(RHSS, Prototype, Requests, ChannelTables,
		ProcessTable, NextProcessTable) :-

    RHSS =?= [] :
      Prototype = _,
      Requests = _,
      ChannelTables = [],
      ProcessTable = NextProcessTable;

    RHSS ? RHS,
    Requests =?= [] :
      ChannelTables ! NewChannelTable |
	stream#hash_table(ChannelTable),
	initialize_channel_table(Prototype, ChannelTable, ChannelTable'?),
	analyze_rhss1(RHS, ChannelTable', NewChannelTable?,
			ProcessTable, ProcessTable'?),
	self;

    RHSS ? RHS,
    Requests ? Request :
      ChannelTables ! NewChannelTable |
	extract_message_channels,
	update_prototype(Code, Channels, Prototype, ClPrototype),
	stream#hash_table(ChannelTable),
	initialize_channel_table(ClPrototype?, ChannelTable, ChannelTable'?),
	analyze_rhss1(RHS, ChannelTable', NewChannelTable?,
			ProcessTable, ProcessTable'?),
	self.

  extract_message_channels(Request, RHS, Code, Channels) :-

    arg(2, Request, send) :
      RHS = (_ : _ = Variables | _),
      Code = send |
	extract_spi_channels;

    arg(2, Request, receive),
    RHS = (_, _ = Variables | _) :
      Code = receive |
	extract_spi_channels.

 update_prototype(Code, Channels, Prototype, NewPrototype) :-

    Channels ? Channel,
    Code =?= send |
	update_prototype_send(Channel, Prototype, Prototype'),
	self;

    Channels ? Channel,
    Code =?= receive |
	update_prototype_receive(Channel, Prototype, Prototype'),
	self;

    Channels = [] :
      Code = _,
      Prototype = NewPrototype.

  update_prototype_send(Channel, Prototype, NewPrototype) :-

    Prototype ? Channel(Refs, PrimeName),
    Refs++ :
      NewPrototype = [Channel(Refs', PrimeName) | Prototype'];

    Prototype ? Other,
    otherwise :
      NewPrototype ! Other |
	self;

    Prototype = [] :
      NewPrototype = [] |
	screen#display("Can't find in Prototype" - Channel).

  update_prototype_receive(Channel, Prototype, NewPrototype) :-

    /* Primed Channel */
    Prototype ? Channel(Refs, PrimeName),
    string_to_dlist(Channel, CL, Prime) :
      Prime = [39],
      NewPrototype = [Channel(Refs, PrimeName?), PrimeName?(0, PrimeName?)
		     | Prototype'] |
	list_to_string(CL, PrimeName);

    Prototype ? Other,
    otherwise :
      NewPrototype ! Other |
	self;

    /* Add Local channel */
    Prototype = [] :
      NewPrototype = [Channel(0, Channel)].

  initialize_channel_table(Prototype, ChannelTable, Initialized) :-

    Prototype ? ChannelName(Refs, PrimeName) :
      ChannelTable ! lookup(ChannelName, {Refs, PrimeName}, _, _) |
	self;

    Prototype =?= [] :
      ChannelTable = Initialized.

analyze_rhss1(RHS, ChannelTable, NewChannelTable,
		ProcessTable, NextProcessTable) :-

    RHS =?= (_Guard | Body) |
	utilities#untuple_predicate_list(',', Body, Body'),
	body_channel_usage;

    RHS =\= (_ | _) |
	utilities#untuple_predicate_list(',', RHS, Body),
	body_channel_usage.

  body_channel_usage(ChannelTable, NewChannelTable, Body,
			ProcessTable, NextProcessTable) :-

    Body ? (Name + Args), string(Name) :
      ProcessTable ! member(Name, Channels, Ok) |
	utilities#untuple_predicate_list(',', Args, Args'),
	body_channel_usage1;

    Body ? Name, string(Name) :
      ProcessTable ! member(Name, Channels, Ok) |
	body_channel_usage1 + (Args = []);

    Body ? (_ # Goal) |
	remote_call;

    Body ? _Other,
    otherwise |
	self;

    Body =?= [] :
      ChannelTable = NewChannelTable,
      ProcessTable = NextProcessTable.

  remote_call(ChannelTable, NewChannelTable, Body,
		ProcessTable, NextProcessTable, Goal) :-

    Goal =?= (_ # Goal') |
	self;

    Goal =\= _ # _, arg(1, Goal, Name), string(Name),
    nth_char(1, Name, C), ascii('A') =< C, C =< ascii('Z') |
	extract_spi_channels(Goal, Channels),
	body_channel_usage2;

    otherwise :
      Goal = _ |
	body_channel_usage.	


body_channel_usage1(ChannelTable, NewChannelTable, Body,
		ProcessTable, NextProcessTable, Channels, Ok, Args) :-

    Ok =?= true |
	replace_arguments(Args, Channels, Channels'),
	body_channel_usage2;

    /* Call to .comm procedure or perhaps some library procedure. */
    Ok =\= true,
    Args = [] :
      Channels = _ |
	body_channel_usage.

  replace_arguments(Args, Channels, NewChannels) :-

    Args ? (`ChannelName = `Channel),
    string(ChannelName), nth_char(1, ChannelName, C),
    ascii(a) =< C, C =< ascii(z) |
	replace_argument(ChannelName, Channel, Channels, Channels'),
	self;

    Args ? _Other,
    otherwise |
	self;

    Args = [] :
      NewChannels = Channels.

  replace_argument(ChannelName, Channel, Channels, NewChannels) :-

    Channels ? ChannelName :
      NewChannels = [Channel | Channels'];

    Channels ? OtherName, ChannelName =\= OtherName :
      NewChannels ! OtherName |
	self.


body_channel_usage2(ChannelTable, NewChannelTable, Body,
		ProcessTable, NextProcessTable, Channels) :-

    /* This will fail for library processes. */
    Channels ? ChannelName :
      Old = {Refs, ChannelName},
      ChannelTable ! lookup(ChannelName, New, Old, Found),
      New = {NewRefs?, ChannelName} |
	body_channel_usage3,
	self;

    Channels =?= [] |
	body_channel_usage.

  body_channel_usage3(Found, Refs, NewRefs) :-

    Found =?= old,
    Refs++ :
      NewRefs = Refs';

    Found =?= new :
      Refs = _,
      NewRefs = 1.


update_rhss(Comm, RHSS, ChannelTables, ProcessTable, NextProcessTable, Rhss) :-

    RHSS ? RHS,
    ChannelTables ? Table :
      Table ! entries(Entries),
      Rhss ! (NewAsk'? : NewTell'? | Body?) |
	partition_rhs(RHS, Ask, Tell, Body),
	reduce_channel_table(Entries, AddAsk, AddTell, Table'),
	utilities#untuple_predicate_list(',', Ask, NewAsk, AddAsk?),
	utilities#untuple_predicate_list(',', Tell, NewTell, AddTell?),
	utilities#make_predicate_list(',', NewAsk?, NewAsk'),
	utilities#make_predicate_list(',', NewTell?, NewTell'),
	self;

    RHSS =?= [] :
      Comm = _,
      ChannelTables = [],
      Rhss = [],
      ProcessTable = NextProcessTable.

  partition_rhs((Ask : Tell | Body), Ask^, Tell^, Body^).
  partition_rhs((Ask  | Body), Ask^, true^, Body^) :-
    Ask =\= (_ : _) | true.
  partition_rhs(Body, true^, true^, Body^) :-
    Body =\= (_ | _) | true.


reduce_channel_table(Entries, AddAsk, AddTell, Table) +
			(Close = Closes?, Closes, NC = 0) :-

    Entries ? entry(Name, {1, _}) :
      Table ! delete(Name, _, _) |
	self;

    Entries ? entry(Name, {0, _}),
    NC++ :
      Table ! delete(Name, _, _),
      Closes ! `Name |
	self;

    Entries ? entry(Name, {N, PName}),
     N-- > 1 :
      Table ! replace(Name, {1, PName}, _Old, _Ok),
      AddAsk ! read_vector(SPI_CHANNEL_REFS, `PName, `spirefs(PName)),
      AddAsk' ! (`spirefs'(PName) := `spirefs(PName) + N'),
      AddTell ! store_vector(SPI_CHANNEL_REFS, `spirefs'(PName), `PName) |
	self;

    Entries =?= [] :
      AddAsk = [],
      Closes = [],
      Table = [] |
	close_channels.

  close_channels(NC, Close, AddTell) :-

    NC =?= 0 :
      Close = _,
      AddTell = [];

    NC =\= 0 :
      AddTell = [write_channel(close(Channels?), `"Scheduler.")] |
	catch_primed_channels(Close, Close'),
	list_to_tuple(Close'?, Channels).

  catch_primed_channels(Channels, NewChannels) + (List = Channels) :-

    Channels ? `PrimedName,
    string(PrimedName),
    string_to_dlist(PrimedName, CNL,[]) :
      NewChannels ! `CloseName? |
	catch_primed_channel + (CNLnp = L, CNLL = L),
	self;

    Channels ? `InterChannel,
    tuple(InterChannel) :
      NewChannels ! `InterChannel |
	self;

    Channels = [] :
      List = _,
      NewChannels = [].

  catch_primed_channel(CNL, PrimedName, List, CloseName, CNLnp, CNLL) :-

    CNL ? CHAR_PRIME :
      CNL' = [],
      CNLL = [] |
	list_to_string(CNLnp, CNnp),
	replace_primed_channel;

    CNL ? C, C =\= CHAR_PRIME :
      CNLL ! C |
	self;

    CNL = [] :
      CNLL = _,
      CNLnp = _,
      List = _,
      CloseName = PrimedName.

  replace_primed_channel(CNnp, PrimedName, List, CloseName) :-

    List ? `CNnp :
      List' = _,
      CloseName = PrimedName;

    List ? Other, Other =\= `CNnp |
	self;

    List =?= [] :
      PrimedName = _,
      CloseName = CNnp.


dimerize_requests(Tell, NewTell) :-

    Tell ? Request :
      NewTell ! NewRequest? |
	dimerize_requests(Retell?, NewTell'),
	dimerize_requests1;

    Tell = [] :
      NewTell = [].

  dimerize_requests1(Tell, Request, NewRequest, Retell) :-

    Tell ? request(send, ChannelName, Multiplier, SendTag),
    Request =?= request(receive, ChannelName, Multiplier, ReceiveTag) :
      NewRequest =
	request(dimer, ChannelName, Multiplier, {SendTag, ReceiveTag}),
      Retell = [NewRequest | Tell'?] ;

    Tell ? request(receive, ChannelName, Multiplier, ReceiveTag),
    Request =?= request(send, ChannelName, Multiplier, SendTag) :
      NewRequest =
	request(dimer, ChannelName, Multiplier, {SendTag, ReceiveTag}),
      Retell = [NewRequest | Tell'?] ;

    /* No change if already dimerized. */
    Tell ? Other,
    otherwise :
      Retell ! Other |
	self;

    Tell =?= [] :
      NewRequest = Request,
      Retell = [].      


extract_spi_channels(Variables, Channels) :-

    Variables ? `Name, string(Name),
    nth_char(1, Name, C), ascii(a) =< C, C =< ascii(z) :
      Channels ! Name |
	self;

    Variables ? _Other,
    otherwise |
	self;

    tuple(Variables) |
	utils#tuple_to_dlist(Variables, Variables', []),
	self;

    Variables = [] :
      Channels = [].


kluge_news(RHSS, NewRHSS) :-
    true :
      NewRHSS = (NewAsks? : NewTells? | Body) |
	partition_rhs(RHSS, Asks, Tells, Body),
	utilities#untuple_predicate_list(",", Asks, Asks', NewConvert?),
	utilities#untuple_predicate_list(",", Tells, Tells'),
	kluge_new_channels + (Convert = []),
	utilities#make_predicate_list(",", Asks'?, NewAsks),
	utilities#make_predicate_list(",", NewTells'?, NewTells).

  kluge_new_channels(Tells, Convert, NewTells, NewConvert) :-

    Tells ? write_channel(new_channel(Name, Channel, ComputeWeight, BaseRate),
				Scheduler),
    tuple(ComputeWeight), ComputeWeight =\= `_,
    arity(ComputeWeight, A),
    make_tuple(A, ComputeWeight') :
      NewTells ! write_channel(
			new_channel(Name, Channel, ComputeWeight'?, BaseRate),
			Scheduler) |
	kluge_real_parameters(ComputeWeight, Convert, 0,
				ComputeWeight', Convert'),
	self;

    Tells ? Tell,
    otherwise :
      NewTells ! Tell |
	self;

    Tells =?= [] :
      NewTells = [],
      NewConvert = Convert.

	
kluge_globals(RHSS, NewRHSS) :-

    RHSS =?= (spi_monitor#global_channels(Globals, Scheduler), Goals) :
      NewRHSS =
	(Ask? | spi_monitor#global_channels(NewGlobals?, Scheduler), Goals) |
	kluge_global_channels + (Convert = []),
	utilities#make_predicate_list(",", NewConvert?, Ask);

    otherwise :
      NewRHSS = RHSS.


  kluge_global_channels(Globals, Convert, NewGlobals, NewConvert) :-

    Globals ? Global, Global =?= Name(Channel, ComputeWeight, BaseRate),
    tuple(ComputeWeight), ComputeWeight =\= `_,
    arity(ComputeWeight, A),
    make_tuple(A, ComputeWeight') :
      NewGlobals ! Name(Channel, ComputeWeight'?, BaseRate) |
	kluge_real_parameters(ComputeWeight, Convert, 0,
				ComputeWeight', Convert'),
	self;

    Globals ? Global,
    otherwise :
      NewGlobals ! Global |
	self;

    Globals =?= [] :
      NewConvert = Convert,
      NewGlobals = [].

kluge_real_parameters(Tuple, Convert, Index, NewTuple, NewConvert) :-

    Index++,
    arg(Index', Tuple, Parameter),
    arg(Index', NewTuple, Parameter'),
    Index' < arity(Tuple),
    real(Parameter),
    convert_to_string(Parameter, String),
    Convert =?= [] :
      Convert' = [convert_to_real(String, `spifcp(1))],
      Parameter' = `spifcp(1) |
	self;

    Index++,
    arg(Index', Tuple, Parameter),
    arg(Index', NewTuple, Parameter'),
    Index' < arity(Tuple),
    real(Parameter),
    convert_to_string(Parameter, String),
    Convert =\= [] :
      Parameter' = `spifcp(N?) |
	kluge_real_parameter(String, Convert, Convert, N, Convert'),
	self;

    Index++,
    arg(Index', Tuple, Parameter),
    arg(Index', NewTuple, Parameter'),
    Index' < arity(Tuple),
    otherwise :
      Parameter = Parameter' |
	self;

    Index++,
    arg(Index', Tuple, Parameter),
    arg(Index', NewTuple, Parameter'),
    Index' =:= arity(Tuple) :
      Parameter' = Parameter,      
      NewConvert = Convert.

  kluge_real_parameter(String, Search, Convert, N, NewConvert) :-

    Search ? convert_to_real(String, `spifcp(I)) :
      Search' = _,
      N = I,
      NewConvert = Convert;

    Search ? _,
    otherwise |
	self;

    Search =?= [],
    Convert = [convert_to_real(_String, `spifcp(I)) | _],
    I++ :
      N = I',
      NewConvert = [convert_to_real(String, `spifcp(I')) | Convert].
