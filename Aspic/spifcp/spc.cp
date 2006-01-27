/*
Precompiler for Stochastic Pi Calculus - Output Phase.

Bill Silverman, February 1999.

Last update by		$Author: bill $
		       	$Date: 2006/01/27 10:33:47 $
Currently locked by 	$Locker:  $
			$Revision: 1.10 $
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
	/* Eliminate unused public channels for this export. */
	update_publics(RHSS, NewRHSS, ProcessTable, ProcessTable'?),
	self;

    In ? outer(Atom, RHSS, _Procedure),
    RHSS =?= (Asks : Tells | Body) :
      ProcessTable ! member(Body, Channels, Ok),
      Terms ! (Atom :- Asks : NewTells? | Body) |
	remove_redundant_news,
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
	update_rhss(RHSS, ChannelTables?, Rhss),
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
	update_rhss(Communicate2''?, ChannelTables?, Communicate2'),
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


update_publics(RHSS, NewRHSS, ProcessTable, NextProcessTable) :-

    RHSS =?=  (spi_monitor # public_channels(Publics, Scheduler), Body) :
      NewRHSS = (spi_monitor # public_channels(NewPublics?, Scheduler), Body),
      ProcessTable = [member(Body, Channels, Ok) | NextProcessTable] |
	utilities # find_logix_variables(Modifiers?, Variables, []),
	update_publics1;

    otherwise :
      NewRHSS = RHSS,
      NextProcessTable = ProcessTable.

  update_publics1(Publics, Channels, Variables, Modifiers, NewPublics, Ok) :-

    Ok = true,
    Publics ? Public, arg(1, Public, Name) |
	update_public_list(Name, Channels, Variables, Public,
			Modifiers, Modifiers'?, NewPublics, NewPublics'?),
	self;

    otherwise :
      Ok = _,
      Channels = _,
      Variables = _,
      Modifiers = [],
      NewPublics = Publics.

  update_public_list(Name, Channels, Variables, Public,
		Modifiers, NextModifiers, NewPublics, NextNewPublics) :-

    Channels ? Name,
    Public = Name(_Variable) :
      Channels' = _,
      Variables = _,
      Modifiers = NextModifiers,
      NewPublics = [Public | NextNewPublics];

    Channels ? Name,
    Public = Name(_Variable, BaseRate) :
      Channels' = _,
      Variables = _,
      Modifiers = [BaseRate | NextModifiers],
      NewPublics = [Public | NextNewPublics];

    Channels ? Name,
    Public = Name(_Variable, BaseRate, WeighterDeclaration) :
      Channels' = _,
      Variables = _,
      Modifiers = [BaseRate, WeighterDeclaration | NextModifiers],
      NewPublics = [Public | NextNewPublics];

    Channels ? Other, Other =\= Name |
	self;

    Channels =?= [],
    Public = Name(Variable) :
      Modifiers = NextModifiers |
	need_variable_in_modifier;

    otherwise :
      Channels = _,
      Name = _,
      Public = _,
      Variables = _,
      Modifiers = NextModifiers,
      NewPublics = NextNewPublics.

  need_variable_in_modifier(Variable, Variables, NewPublics, NextNewPublics) :-

    Variables ? Variable, Variable =?= `Name :
      Variables' = _,
      NewPublics = [Name(Variable) | NextNewPublics];

    Variables ? Other,
    Other =\= Variable |
	self;

    otherwise :
      Variable = _,
      Variables = _,      
      NewPublics = NextNewPublics.
      

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

    Channels ? ChannelName,
    Code =?= send |
	update_prototype_send(ChannelName, Prototype, Prototype'),
	self;

    Channels ? ChannelName,
    Code =?= receive |
	update_prototype_receive(ChannelName, Prototype, Prototype'),
	self;

    Channels = [] :
      Code = _,
      Prototype = NewPrototype.

  update_prototype_send(ChannelName, Prototype, NewPrototype) :-

    Prototype ? ChannelName(Refs, PrimeName),
    Refs++ :
      NewPrototype = [ChannelName(Refs', PrimeName) | Prototype'];

    Prototype ? Other,
    otherwise :
      NewPrototype ! Other |
	self;

    Prototype = [] :
      NewPrototype = [] |
	screen#display("Can't find in Prototype" - ChannelName).

  update_prototype_receive(ChannelName, Prototype, NewPrototype) :-

    /* Primed Channel Name */
    Prototype ? ChannelName(Refs, PrimeName),
    string_to_dlist(ChannelName, CL, Prime) :
      Prime = [CHAR_PRIME],
      NewPrototype = [ChannelName(0, PrimeName?), PrimeName?(Refs, PrimeName?)
		     | Prototype'] |
	list_to_string(CL, PrimeName);

    Prototype ? Other,
    otherwise :
      NewPrototype ! Other |
	self;

    /* Add Local channel */
    Prototype = [] :
      NewPrototype = [ChannelName(0, ChannelName)].

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
    nth_char(1, Name, C), CHAR_A =< C, C =< CHAR_Z |
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
    CHAR_a =< C, C =< CHAR_z |
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
    Channels ? ChannelName,
    L := string_length(ChannelName),
    nth_char(L, ChannelName, CHAR_PRIME) :
      Old = {Refs, ChannelName},
      ChannelTable ! lookup(ChannelName, New, Old, Found),
      New = {NewRefs?, ChannelName} |
	body_channel_usage3,
	self;

    Channels ? ChannelName,
    L := string_length(ChannelName),
    nth_char(L, ChannelName, C),
    C =\= CHAR_PRIME,
    string_to_dlist(ChannelName, CN, Prime) :
      Prime = [CHAR_PRIME],
      ChannelTable ! member(PrimeName?, _Value, Ok),
      Old = {Refs, Name?},
      ChannelTable' ! lookup(Name?, New, Old, Found),
      New = {NewRefs?, Name?} |
	list_to_string(CN, PrimeName),
	body_channel_usage_name,
	body_channel_usage3,
	self;

    Channels =?= [] |
	body_channel_usage.

  body_channel_usage_name(Ok, ChannelName, PrimeName, Name) :-

    Ok = false :
      PrimeName = _,
      Name = ChannelName;

    Ok = true :
      ChannelName = _,
      Name = PrimeName.

  body_channel_usage3(Found, Refs, NewRefs) :-

    Found =?= old,
    Refs++ :
      NewRefs = Refs';

    Found =?= new :
      Refs = _,
      NewRefs = 1.


update_rhss(RHSS, ChannelTables, Rhss) :-

    RHSS ? RHS,
    ChannelTables ? Table :
      Table ! entries(Entries),
      Rhss ! (NewAsk'? : NewTell'? | Body?) |
	partition_rhs(RHS, Ask, Tell, Body),
	remove_logix_variables(Entries, ChEntries),
	reduce_channel_table(ChEntries?, ForkAsk, ForkTell, Close, Table'),
	fork_and_close,
	utilities#untuple_predicate_list(',', Ask, NewAsk, AddAsk?),
	utilities#untuple_predicate_list(',', Tell, NewTell, AddTell?),
	utilities#make_predicate_list(',', NewAsk?, NewAsk'),
	utilities#make_predicate_list(',', NewTell?, NewTell'),
	self;

    RHSS =?= [] :
      ChannelTables = [],
      Rhss = [].

  partition_rhs((Ask : Tell | Body), Ask^, Tell^, Body^).
  partition_rhs((Ask  | Body), Ask^, true^, Body^) :-
    Ask =\= (_ : _) | true.
  partition_rhs(Body, true^, true^, Body^) :-
    Body =\= (_ | _) | true.

  remove_logix_variables(Entries, ChEntries) :-

    Entries = [] :
      ChEntries = [];

    Entries ? _entry(Name, _Status),
    nth_char(1, Name, C), CHAR_A =< C, C =< CHAR_Z |
	self;

    Entries ? Entry,
    otherwise :
      ChEntries ! Entry |
	self.

  reduce_channel_table(Entries, AddAsk, AddTell, Close, Table) +
			(CloseList, Closes = CloseList) :-

    Entries ? entry(Name, {1, _}) :
      Table ! delete(Name, _, _) |
	self;

    Entries ? entry(Name, {0, _}) :
      Table ! delete(Name, _, _),
      Closes ! `Name |
	self;

    Entries ? entry(Name, {N, ChName}),
      N-- > 1 :
      Table ! replace(Name, {1, ChName}, _Old, _Ok),
      AddAsk ! read_vector(SPI_CHANNEL_REFS, `ChName, `spirefs(ChName)),
      AddAsk' ! (`spirefs'(ChName) := `spirefs(ChName) + N'),
      AddTell ! store_vector(SPI_CHANNEL_REFS, `spirefs'(ChName), `ChName) |
	self;

    Entries =?= [] :
      AddAsk = [],
      AddTell = [],
      Closes = [],
      Table = [] |
	close_channels.

  close_channels(CloseList, Close) :-

    CloseList = [] :
      Close = [];

    CloseList =\= [] :
      Close = close(Channels?) |
	list_to_tuple(CloseList, Channels).

  fork_and_close(Body, ForkAsk, ForkTell, Close, AddAsk, AddTell) :-

    ForkTell =?= [],
    Close =?= [] :
      ForkAsk = _,
      AddAsk = [] |
	continue_with_scheduler;

    ForkTell =?= [],
    Close =\= [] :
      Body = _,
      ForkAsk = _,
      AddAsk = [],
      AddTell = [write_channel(Close?, `"Scheduler.")];

    ForkTell =?= [_Update],
    Close =?= [] :
      Body = _,
      AddAsk = ForkAsk,
      AddTell = ForkTell;

    ForkTell =?= [Update],
    Close =\= [] :
      Body = _,
      AddAsk = ForkAsk,
      AddTell = [Update, write_channel(Close?, `"Scheduler.")];

    ForkTell =?= [_Update1, _Update2 | _],
    Close =?= [] :
      Body = _,
      AddAsk = [],
      AddTell = [write_channel(update_references(List?), `"Scheduler.")] |
	make_update_channel_refs_list;

    otherwise :
      Body = _,
      ForkTell = _,
      AddAsk = [],
      AddTell = [write_channel(update_references(List?),
			       `"Scheduler.", `"Scheduler.'"),
		 write_channel(Close?, `"Scheduler.'")] |
	make_update_channel_refs_list.
 
  make_update_channel_refs_list(ForkAsk, List) :-

    ForkAsk ? _,
    ForkAsk' ? (`spirefs'(ChName) := `spirefs(ChName) + N) :
      List ! {`ChName, N} |
	self;

    ForkAsk =?= [] :
      List = [].

  /* A little kluge for lint */
  continue_with_scheduler(Body, AddTell) :-

    Body =?= (Goal, _),
    Goal =\= (_ # _), Goal =\= true :
      AddTell = [];

    Body =?= (_, Body'),
    otherwise |
	self;

    Body =\= (_, _),
    Body =\= (_ # _), Body =\= true :
      AddTell = [];

    Body =\= (_, _),
    otherwise :
      AddTell = [(`"Scheduler." = `"_")].


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

    /* No change if no match or already dimerized. */
    Tell ? Other,
    otherwise :
      Retell ! Other |
	self;

    Tell =?= [] :
      NewRequest = Request,
      Retell = [].      


extract_spi_channels(Variables, Channels) :-

    Variables ? `Name, string(Name),
    nth_char(1, Name, C),
    CHAR_a =< C, C =< CHAR_z :
      Channels ! Name |
	self;

    Variables ? `Name, string(Name),
    nth_char(1, Name, C),
    CHAR_A =< C, C =< CHAR_Z :
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

remove_redundant_news(Tells, Channels, Ok, NewTells) :-

    Ok =?= true |
	utilities#untuple_predicate_list(",", Tells, Tells'),
	ignore_redundant_news(Channels, Tells'?, NewTells'),
	utilities#make_predicate_list(",", NewTells'?, NewTells);

    Ok =\= true :
      Channels = _,
      NewTells = Tells.

  ignore_redundant_news(Channels, Tells, NewTells) :-

    Tells ? Write, Write =?= write_channel(new_channel(_, Channel, _), _) |
	ignore_redundant_new(Channels, Write, Channel, NewTells, NewTells'),
	self;

    Tells ? Write, Write =?= write_channel(new_channel(_, Channel, _, _), _) |
	ignore_redundant_new(Channels, Write, Channel, NewTells, NewTells'),
	self;

    Tells ? Other, Other =\= write(new_channel(_, _, _, _), _) :
      NewTells ! Other |
	self;

    Tells =?= [] :
      Channels = _,
      NewTells = [].

  ignore_redundant_new(Channels, Write, Channel, Tells, NewTells) :-

    Channels ? Name,
    Channel =?= `Name :
      Channels' = _,
      Tells = [Write | NewTells];

    Channels ? Name,
    Channel =\= `Name |
	self;

    Channels =?= [] :
      Channel = _,
      Write = _,
      Tells = NewTells.
