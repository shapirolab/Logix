/*
Precompiler for Pi Calculus procedures - Stochastic Pi Calculus Phase.

Bill Silverman, February 1999.

Last update by		$Author: bill $
		       	$Date: 2000/07/26 07:21:54 $
Currently locked by 	$Locker:  $
			$Revision: 1.4 $
			$Source: /home/qiana/Repository/PsiFcp/psifcp/spc.cp,v $

Copyright (C) 2000, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export(stochasticize/2).
-mode(interpret).
-language(compound).


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
	extract_psi_channels(Variables, Channels),
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
**      RHSS is the right-hand-side of the compound procedure.
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
      Terms ! (Atom'? :- RHSS'?) |
	utilities#tuple_to_atom(Atom, Atom'),
	/* Eliminate unused global channels for this export. */
	update_globals(RHSS, RHSS', ProcessTable, ProcessTable'?),
	self;

    /* We could add a check here for unused channels, i.e. ones
       that are not needed by the derived procedure. */
    In ? outer(Atom, RHSS, _Procedure) :
      Terms ! (Atom' :- RHSS) |
	utilities#tuple_to_atom(Atom, Atom'),
	self;

    In ? Mode(Atom, RHSS, Procedure),
    Mode =\= outer, Mode =\= export, Mode =\= conflict,
    arg(1, Atom, Name) :
      ProcessTable ! member(Name, Channels, _Ok),
      Terms ! (Atom'? :- NewRHSS?) |
	utilities#tuple_to_atom(Atom, Atom'),
	prototype_channel_table,
	utilities#untuple_predicate_list(';', RHSS, RHSS'),
	stochastic;

    In = [] :
      Terms = [] |
	ProcessTable = [].


stochastic(In, ProcessTable, Terms, Name, RHSS, Prototype, Procedure,
		NewRHSS) :-

    Procedure =\= (_ :- _) :
      Name = _ |
	analyze_rhss(RHSS, Prototype, [], ChannelTables,
			ProcessTable, ProcessTable'?),
	update_rhss(false, RHSS, ChannelTables,
			ProcessTable', ProcessTable''?, Rhss),
	utilities#make_predicate_list(';', Rhss, NewRHSS),
	output;

    Procedure =?= (Atom :- Communicate1),
    RHSS =?= [(Ask : Writes | Body)] :
      NewRHSS = (Ask'? : Tell? | Body),
      Terms ! (Atom :- Communicate2?) |
	utilities#untuple_predicate_list(',', Ask, Asks'),
	analyze_multipliers(Writes''?, Asks, Asks'?),
	utilities#make_predicate_list(',', Asks?, Ask'),
	utilities#untuple_predicate_list(',', Writes, Writes'),
	utilities#untuple_predicate_list(';', Communicate1, Communicate1'),
	analyze_rhss(Communicate1'?, Prototype, Writes'?, ChannelTables,
			ProcessTable, ProcessTable'?),
        /* Check for homodimerized communication (send and receive on the
           same channel). Convert one of the operations to "dimer" and
           suppress the other, retaining its .comm clause.               */
	dimerize_requests(Writes'?, Writes''),
	utils#binary_sort_merge(Writes'', Tell'),
	utilities#make_predicate_list(',',
		[write_channel(start(Name), `"Scheduler.") | Tell'?], Tell),
	extract_communication_list,
	utils#binary_sort_merge(CommunicationsList, Communications),
	rewrite_clauses,
	update_rhss(true, Communicate2''?, ChannelTables,
			ProcessTable', ProcessTable''?, Communicate2'),
	utilities#make_predicate_list(';', Communicate2'?, Communicate2),
	output.

  analyze_multipliers(Writes, Multipliers,  Asks) :-

    Writes ? _write_message(Ms, _Channel),
    Ms =?= _Type(_Id, _Message, _Tag, Multiplier, _Chosen),
    Multiplier =?= `_ :
      Multipliers ! integer(Multiplier),
      Multipliers' ! (Multiplier > 0) |
	self;

    Writes ? _Other,
    otherwise |
	self;

    Writes =?= [] :
     Multipliers = Asks.

  prototype_channel_table(Channels, Prototype) :-

    Channels ? Name :
      Prototype ! Name(0, Name) |
	self;

    Channels = [] :
      Prototype = [].

  extract_communication_list(Asks, Writes, CommunicationsList) :-

    Asks ? Identify,
    Writes ? _write_channel(Ms, Channel),
    Ms = Type(_Id, _Message, Tag, Multiplier, _Chosen) :
      CommunicationsList ! {Tag, Identify, Type, Multiplier, Channel} |
	self;

    Asks = [],
    Writes = [] :
       CommunicationsList = [].

update_dimerized(Writes, Tell) :-

    Writes ? Write, Write =?= write_channel(Request, _Channel),
    arg(1, Request, Type), Type =\= dimer :
      Tell ! Write |
	self;

    Writes ? Write,
    otherwise :
      Tell ! Write |
	remove_second_dimer(Write, Writes', Writes''),
	self;

    Writes = [] :
      Tell = [].

  remove_second_dimer(Write, Writes, NewWrites) :-

    Writes ? Write :
      NewWrites = Writes';

    Writes? Other, Other =\= Write :
      NewWrites ! Write |
	self.


update_globals(RHSS, NewRHSS, ProcessTable, NextProcessTable) :-

    RHSS =?=  (psi_monitor # global_channels(Globals, Scheduler), Body) :
      NewRHSS = (psi_monitor # global_channels(NewGlobals?, Scheduler),
						NewBody?),
      ProcessTable = [member(Name, Channels, Ok) | NextProcessTable] |
	extract_entry_name,
	update_globals1,
	update_body_list,
	utilities#make_predicate_list(',', NewBody'?, NewBody);

    otherwise :
      NewRHSS = RHSS,
      NextProcessTable = ProcessTable.

  extract_entry_name(Body, Name, BodyList) :-

    Body =?= (Assignment, Body'), Assignment =?= (_Variable = _Real) :
      BodyList ! Assignment |
	self;

    otherwise :
      Name = Body,
      BodyList = [Body].

  update_globals1(Globals, Channels, NewGlobals, Ok) :-

    Ok = true,
    Globals ? Name(`Name, Multiplier) |
	update_global_list(Name, Channels, Multiplier,
		NewGlobals, NewGlobals'?),
	self;

    otherwise :
      Ok = _,
      Channels = _,
      NewGlobals = Globals.

  update_global_list(Name, Channels, Multiplier, NewGlobals, NextNewGlobals) :-

    Channels ? Name :
      Channels' = _,
      NewGlobals = [Name(`Name, Multiplier) | NextNewGlobals];

    Channels ? Other, Other =\= Name |
	self;

    Channels =?= [] :
      Name = _,
      Multiplier = _,
      NewGlobals = NextNewGlobals.

update_body_list(BodyList, NewGlobals, NewBody) :-

    BodyList ? Assignment, Assignment =?= (Variable = _Real) |
	ignore_unused_assignment(Assignment, Variable, NewGlobals,
					NewBody, NewBody'?),
	self;

    otherwise :
      NewGlobals = _,
      NewBody = BodyList.

  ignore_unused_assignment(Assignment, Variable, Globals, Body, NextBody) :-

    Globals ? _Name(_Vector, Variable) :
      Globals' = _,
      Body = [Assignment | NextBody];

    Globals ? _Other,
    otherwise |
	self;

    Globals =?= [] :
      Assignment = _,
      Variable = _,
      Body = NextBody.


rewrite_clauses(Communicate1, Writes, Communications, Communicate2) :-

    Communicate1 ? (Ask | Body),
    Ask = (Select, Consume),
    Writes ? write_channel(_Type(_Id, _Ms, ClauseTag, _Mult, _Chosen), _Ch) :
      Communicate2 ! (Ask'? : Withdrawn? | Body),
      Asks = [Select, Consume | Identifies?] |
	generate_withdraws,
	utilities#make_predicate_list(',', Asks, Ask'),
	utilities#make_predicate_list(',', Withdraws?, Withdrawn),
	self;

    Communicate1 ? (Guard | Body),
    Guard = (Select : Unify),
    Writes ? write_channel(_Type(_Id, _Ms, ClauseTag, _Mult, _Chosen), _Ch) :
      Communicate2 ! (Select, Identifies? : Unify, Withdrawn? | Body) |
	generate_withdraws,
	utilities#make_predicate_list(',', Identifies'?, Identifies),
	utilities#make_predicate_list(',', Withdraws?, Withdrawn),
	self;

    Communicate1 = [] :
      Writes = _,
      Communications = _,
      Communicate2 = [].

  generate_withdraws(Communications, ClauseTag, Identifies, Withdraws) :-

    Communications ? {ClauseTag, _Identify, _Type, _Multiplier, _Channel} |
	self;

    Communications ? {Tag, Identify, Type, Multiplier, Channel},
    Tag =\= ClauseTag :
      Identifies ! Identify,
      Withdraws ! write_channel(withdraw(Type, Multiplier), Channel) |
	self;

    Communications = [] :
      ClauseTag = _,
      Identifies = [],
      Withdraws = [].


analyze_rhss(RHSS, Prototype, Writes, ChannelTables,
		ProcessTable, NextProcessTable) :-

    RHSS =?= [] :
      Prototype = _,
      Writes = _,
      ChannelTables = [],
      ProcessTable = NextProcessTable;

    RHSS ? RHS,
    Writes =?= [] :
      ChannelTables ! NewChannelTable |
	stream#hash_table(ChannelTable),
	initialize_channel_table(Prototype, ChannelTable, ChannelTable'?),
	analyze_rhss1(RHS, ChannelTable', NewChannelTable?,
			ProcessTable, ProcessTable'?),
	self;

    RHSS ? RHS,
    Writes ? Write :
      ChannelTables ! NewChannelTable |
	extract_message_channels,
	update_prototype(Code, Channels, Prototype, ClPrototype),
	stream#hash_table(ChannelTable),
	initialize_channel_table(ClPrototype?, ChannelTable, ChannelTable'?),
	analyze_rhss1(RHS, ChannelTable', NewChannelTable?,
			ProcessTable, ProcessTable'?),
	self.

  extract_message_channels(Write, RHS, Code, Channels) :-

    arg(2, Write, Request), arg(1, Request, send) :
      RHS = (_ : _ = Variables | _),
      Code = send |
	extract_psi_channels;

    arg(2, Write, Request), arg(1, Request, receive),
    RHS = (_, _ = Variables | _) :
      Code = receive |
	extract_psi_channels.

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
	extract_psi_channels(Goal, Channels),
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
      Rhss ! (NewAsk'? : NewTell''? | NewBody''?) |
	partition_rhs(RHS, Ask, Tell, Body),
	reduce_channel_table(Entries, AddAsk, AddTell, Table', Table''?),
	utilities#untuple_predicate_list(',', Ask, NewAsk, AddAsk?),
	utilities#untuple_predicate_list(',', Tell, NewTell, AddTell?),
	update_tell(Comm, NewTell, NewTell', Table'', Table'''?),
	utilities#untuple_predicate_list(',', Body, NewBody),
	update_body(NewBody, Table''', NewBody', ProcessTable, ProcessTable'?),
	utilities#make_predicate_list(',', NewAsk?, NewAsk'),
	utilities#make_predicate_list(',', NewTell'?, NewTell''),
	utilities#make_predicate_list(',', NewBody'?, NewBody''),
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


reduce_channel_table(Entries, AddAsk, AddTell, Table, NextTable) :-

    Entries ? entry(Name, {1, _}) :
      Table ! delete(Name, _, _) |
	self;

    Entries ? entry(Name, {0, _}) :
      Table ! delete(Name, _, _),
      AddAsk ! known(`Name),
      AddTell ! (`Name = {`"_", `"_",
			 {`psiln(Name), `psiln(Name)}}) |
	self;

    Entries ? entry(Name, {N, PName}),
    N > 1,
    convert_to_string(N, NS),
    string_to_dlist(psiln_, PsiL, PsiT),
    string_to_dlist(NS, NSL, []) :
      PsiT = NSL,
      Table ! replace(Name, {1, PName}, _Old, _Ok),
      AddAsk ! (`Name = {`psiid(Name), `psich(Name),
			{`psiln_0(Name), `Psiln_N?(Name)}}) |
	list_to_string(PsiL, Psiln_N),
	add_tells(Name, 0, N, AddTell, AddTell'),
	self;

    Entries =?= [] :
      AddAsk = [],
      AddTell = [],
      Table = NextTable.

  add_tells(Name, Index, N, AddTell, NextAddTell) :-

    Index < N,
    Index++,
    convert_to_string(Index, IS),
    string_to_dlist(psiln_, PsiL, PsiT),
    string_to_dlist(IS, ISL, []),
    convert_to_string(Index', IS'),
    string_to_dlist(psiln_, PsiL', PsiT'),
    string_to_dlist(IS', ISL', []),
    string_to_dlist(psich_, PsiC, PsiU) :
      PsiT = ISL,
      PsiT' = ISL',
      PsiU = ISL',
      AddTell ! (`PsichIndex?(Name) =
			{`psiid(Name), `psich(Name),
			 {`PsilnIndex?(Name), `PsilnIndex'?(Name)}}) |
	list_to_string(PsiL, PsilnIndex),
	list_to_string(PsiL', PsilnIndex'),
	list_to_string(PsiC, PsichIndex),
	self;

    Index >= N :
      Name = _,
      AddTell = NextAddTell.


update_tell(Comm, Tell, NewTell, Table, NextTable) :-

    Comm = true,
    Tell = [(`"Message." = Message) | Tail],
    tuple(Message),
    arity(Message, A),
    make_tuple(A, Message') :
      NewTell = [(`"Message." = Message') | Tail] |
	replace_sent_channels(1, Message, Message', Table, NextTable);

    otherwise :
      Comm = _,
      NewTell = Tell,
      Table = NextTable.

  replace_sent_channels(Index, Message, NewMessage, Table, NextTable) :-

    Index =< arity(Message),
    arg(Index, Message, `ChannelName),
    arg(Index, NewMessage, NewChannel),
    Index++ :
      Table ! member(ChannelName, Value, Ok) |
	replace_channel + (Table = Table', NextTable = Table''?),
	self;

    Index > arity(Message) :
      Message = _,
      NewMessage = _,
      Table = NextTable.


update_body(Body, Table, NewBody, ProcessTable, NextProcessTable) :-

    Body ? Goal, string(Goal) :
      NewBody ! Goal'?,
      ProcessTable ! member(Goal, Channels, Ok) |
	update_goal(Goal, Goal, Channels, Ok, [], Goal', Table, Table'?),
	self;

    Body ? Goal, Goal = Functor + Substitutions, string(Functor) :
      NewBody ! Goal'?,
      ProcessTable ! member(Functor, Channels, Ok) |
	utilities#untuple_predicate_list(',', Substitutions, Substitutions'),
	update_goal(Goal, Functor, Channels, Ok, Substitutions'?, Goal', 
			Table, Table'?),
	self;

    Body ? RemoteCall, RemoteCall =?= Path#RemoteGoal :
      NewBody ! Path#RemoteGoal'? |
	update_remote_goal(RemoteGoal, RemoteGoal', Table, Table'?),
	self;

    Body ? Other,
    otherwise :
      NewBody ! Other |
	self;

    Body =?= [] :
      NewBody = [],
      Table = [],
      ProcessTable = NextProcessTable.


update_remote_goal(RemoteGoal, NewRemoteGoal, Table, NextTable) :-

    RemoteGoal = Path#RemoteGoal' :
      NewRemoteGoal = Path#NewRemoteGoal'? |
	self;

    RemoteGoal =\= (_#_), tuple(RemoteGoal), arg(1, RemoteGoal, Functor),
    string(Functor), nth_char(1, Functor, C),
    ascii('A') =< C, C =< ascii('Z'),
    arity(RemoteGoal, Index),
    make_tuple(Index, NRG),
    arg(1, NRG, F) :
      F = Functor |
	update_remote_goal1;

    otherwise :
      NewRemoteGoal = RemoteGoal |
	Table = NextTable.

  update_remote_goal1(RemoteGoal, NewRemoteGoal, Table, NextTable,
			NRG, Index) :-

    Index > 1,
    arg(Index, RemoteGoal, Arg),
    arg(Index, NRG, NewArg),
    Index-- |
	update_argument(Arg, NewArg, Table, Table'?),
	self;

    Index =< 1 :
      RemoteGoal = _,
      NewRemoteGoal = NRG,
      Table = NextTable.

  update_argument(Arg, NewChannel, Table, NextTable) :-

    Arg = `ChannelName, string(ChannelName) :
      Table ! member(ChannelName, Value, Ok) |
	replace_channel;

    otherwise :
      NewChannel = Arg,
      Table = NextTable.


update_goal(Goal, Functor, Channels, Ok, Substitutions, NewGoal,
		Table, NextTable) :-

    Ok =?= true :
      Goal = _ |
	update_substitutions(Substitutions, Substitutions', NextSubstitutions?,
				Channels, Channels', Table, Table'?),
	update_implicit(Channels'?, NextSubstitutions, Table', NextTable),
	construct_newgoal(Functor, Substitutions'?, NewGoal);
	

    Ok =\= true :
      Goal = _,
      Channels = _,
      Table = NextTable |
	construct_newgoal(Functor, Substitutions, NewGoal).


update_substitutions(Subs, NewSubs, NextSubs, Channels, NewChannels,
			Table, NextTable) :-

    Subs ? (`Channel1 = `Channel2) :
      NewSubs ! (`Channel1 = Channel2'?),
      Table ! member(Channel2, CV, Ok) |
	utilities#remove_item(Channel1, Channels, Channels'),
	substitute_channel(Channel2, CV, Ok, Channel2', Table', Table''?),
	self;

    Subs =?= [] :
      NewSubs = NextSubs,
      NewChannels = Channels,
      Table = NextTable.

  substitute_channel(ChannelName, Value, Ok, NewChannel, Table, NextTable) :-

    Ok =?= true,
    Value = {_Value, ChannelName} |
	replace_channel;

    Ok =?= true,
    Value = {_Value, ChannelName'}, ChannelName =\= ChannelName' :
      Table ! member(ChannelName', Value', Ok') |
	self;

    Ok =?= false :
      Value = _,
      NewChannel = `ChannelName,
      Table = NextTable.


update_implicit(Channels, Substitutions, Table, NextTable) :-

    Channels ? ChannelName :
      Table ! member(ChannelName, Value, Ok) |
	update_implicit1;

    Channels =?= [] :
      Substitutions = [],
      Table = NextTable.

  update_implicit1(Channels, Substitutions, Table, NextTable,
			ChannelName, Value, Ok) :-

    Ok =?= true :
      Substitutions ! (`ChannelName = Substitute?) |
	replace_channel(ChannelName, Substitute, Ok, Value, Table, Table'?),
	update_implicit;

    Ok =\= true :
      ChannelName = _,
      Value = _ |
	update_implicit.    


dimerize_requests(Tell, NewTell) :-

    Tell ? write_channel(Request, Channel) :
      NewTell ! write_channel(NewRequest?, Channel) |
	dimerize_requests(Retell?, NewTell'),
	dimerize_requests1;

    Tell = [] :
      NewTell = [].

  dimerize_requests1(Tell, Request, Channel, NewRequest, Retell) :-

    Tell ? write_channel(Send, Channel),
    Send = send(Id, Message, SendTag, Multiplier, Chosen),
    Request =?= receive(Id, Message, ReceiveTag, Multiplier, Chosen) :
      NewRequest = dimer(Id, Message, {SendTag, ReceiveTag},
				Multiplier, Chosen),
      Retell = [write_channel(NewRequest, Channel) | Tell'?] ;

    Tell ? write_channel(Receive, Channel),
    Receive = receive(Id, Message, ReceiveTag, Multiplier, Chosen),
    Request =?= send(Id, Message, SendTag, Multiplier, Chosen) :
      NewRequest = dimer(Id, Message, {SendTag, ReceiveTag},
				Multiplier, Chosen),
      Retell = [write_channel(NewRequest, Channel) | Tell'?] ;

    /* No change if already dimerized. */
    Tell ? Other,
    otherwise :
      Retell ! Other |
	self;

    Tell =?= [] :
      Channel = _,
      NewRequest = Request,
      Retell = [].      


construct_newgoal(Functor, Substitutions, NewGoal) :-

    Substitutions = [] :
      NewGoal = Functor;

    Substitutions =\= [] :
      NewGoal = Functor + Substitutions'? |
	utilities#make_predicate_list(',', Substitutions, Substitutions').


replace_channel(ChannelName, NewChannel, Ok, Value, Table, NextTable) :-

    Ok =?= true,
    Value = {Index, PName},
    Index++,
    convert_to_string(Index, IS),
    string_to_dlist(IS, ISL, []),
    string_to_dlist(psich_, PsiC, PsiU) :
      PsiU = ISL,
      NewChannel = `PsichIndex?(ChannelName),
      Table = [lookup(ChannelName, {Index', PName}, Value, Status)
	      |	NextTable] |
	list_to_string(PsiC, PsichIndex),
	Status? = old;				/* Debugging aid. */

    Ok =\= true :
      Value = _,
      NewChannel = `ChannelName,
      Table = NextTable.

extract_psi_channels(Variables, Channels) :-

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

/*
display_update(Name, Rhss) :-
	screen#display((Name ::= Rhss), type(ground)).
*/
