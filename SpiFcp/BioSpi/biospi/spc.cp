/*
Precompiler for Biological Stochastic Pi Calculus procedures - Output Phase.

Bill Silverman, February 1999.

Last update by		$Author: bill $
		       	$Date: 2002/10/13 10:08:03 $
Currently locked by 	$Locker:  $
			$Revision: 1.6 $
			$Source: /home/qiana/Repository/SpiFcp/BioSpi/biospi/spc.cp,v $

Copyright (C) 2000, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export(stochasticize/3).
-mode(interpret).
-language([evaluate, compound, colon]).

-include(spi_constants).
-include(bio_constants).

stochasticize(BlockPrefix, In, Terms) :-
	stream#hash_table(HashTable),
	stream#hash_table(SignatureTable),
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
** output/5
**
** Input:
**
**   BlockPrefix is a prefix string for new_ambient Name.
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
**		request(Type, ChannelName, Multiplier, Tag, Locus)
**
**      Procedure is [] or the compound procedure's communication part.
**
**        <procedure name>'(<arguments'>) :- <compound rhs>
**
** Output:
**
**   Terms is a stream of compound procedures.
*/

output(BlockPrefix, In, ProcessTable, SignatureTable, Terms) :-

    /* Discard conflicted code. */
    In ? conflict(_Atom, _RHSS, _Procedure) |
	self;

    In ? export(Atom, RHSS, _Procedure) :
      Terms ! (Atom'? :- NewRHSS?) |
	utilities#tuple_to_atom(Atom, Atom'),
	/* Eliminate unused public channels for this export. */
	update_publics(RHSS, RHSS', ProcessTable, ProcessTable'?),
	kluge_publics,
	self;

    In ? outer(Atom, RHSS, _Procedure),
    arg(1, Atom, Name),
    RHSS =?= (Asks : Tells | Body) :
      ProcessTable ! member(Body, Channels, Ok),
      Terms ! (Atom :- NewAsks? : NewTells? | Body),
      SignatureTable ! lookup(Name, Atom, Atom, _Status) |
	kluge_news,
	self;

    In ? Mode(Atom, RHSS, Procedure),
    Mode =\= outer, Mode =\= export, Mode =\= conflict,
    arg(1, Atom, Name),
    Arity := arity(Atom) - 1,
    make_tuple(Arity, Signature)  :
      ProcessTable ! member(Name, Channels, _Ok),
      SignatureTable ! lookup(Name, Signature?, Signature?, _Status),
      Terms ! (Atom :- NewRHSS?) |
	prototype_channel_table,
	utilities#untuple_predicate_list(';', RHSS, RHSS'),
	make_process_id,
	stochastic;

    In = [] :
      BlockPrefix = _,
      Terms = [],
      ProcessTable = [],
      SignatureTable = [entries(Entries)] |
	complete_signature_table.

  complete_signature_table(Entries) :-

    Entries ? entry(Name, Name^) |
	self;

    Entries ? _Entry,
    otherwise |
	self;

    Entries = [] | true.

  make_process_id(Arity, Atom, Signature) :-

    Arity-- > 0,
    arg(Arity, Atom, Arg),
    arg(Arity, Signature, A) :
      A = Arg |
	self;

    Arity =< 0 :
      Atom = _,
      Signature = _.


stochastic(BlockPrefix, In, ProcessTable, SignatureTable, Terms,
	Signature, RHSS, Prototype, Procedure, NewRHSS) :-

    Procedure =\= (_ :- _) :
      Signature = _ |
	analyze_rhss(RHSS, Prototype, [], ChannelTables,
			ProcessTable, ProcessTable'?),
	update_rhss(BlockPrefix, RHSS, [], ChannelTables?,
		SignatureTable, SignatureTable'?, Rhss),
	utilities#make_predicate_list(';', Rhss, NewRHSS),
	output;

    Procedure =?= (Atom :- Communicate1),
    RHSS =?= [(Ask : Requests | Body)] :
      Start =
	[write_channel(start(Signature, Operations?, BIO_MESSAGE, BIO_CHOSEN),
		       BIO_SCHEDULER)],
      NewRHSS = (Ask'? : Tell'? | Body),
      Terms ! (Atom'? :- Communicate2?) |
	utilities#untuple_predicate_list(',', Ask, Asks'),
	analyze_multipliers(Requests''?, Asks, Asks'?),
	utilities#make_predicate_list(',', Asks?, Ask'),
	utilities#untuple_predicate_list(',', Requests, Requests'),
	utilities#untuple_predicate_list(';', Communicate1, Communicate1'),
	analyze_rhss(Communicate1'?, Prototype, Requests''?, ChannelTables,
			ProcessTable, ProcessTable'?),
        /* Check for homodimerized communication (send and receive on the
           same channel). Convert one of the requests to a "dimer" request
           and suppress the other, retaining its primed clause.             */
	dimerize_requests(Requests'?, Requests''),

	lookup_inter_channels(Requests''?, Requests''', Lookups),
	/* Remove duplicate Lookups */
	utils#binary_sort_merge(Lookups?, Lookups'),
	extract_inter_channels(Lookups', Start, Tell, InterChannels),
	utilities#make_predicate_list(',', Tell?, Tell'),
	/* May need to insert inter-ambient-channels before BIO_SCHEDULER. */
	utils#tuple_to_dlist(Atom, Arglist, InterChannels?),
	utils#list_to_tuple(Arglist, Atom'),

	/* Remove duplicate Requests */
	utils#binary_sort_merge(Requests'''?, Communications),
	rewrite_clauses,
	communication_to_operations,
	/* Add InterChannels to arguments ([] above) - append to Closes. */
	update_rhss(BlockPrefix, Communicate2'''?, InterChannels,
	    ChannelTables?, SignatureTable, SignatureTable'?, Communicate2''),
	generate_inter_comm(Signature, InterChannels?,
		Communicate2''?, Communicate2', Terms', Terms''),
	utilities#make_predicate_list(';', Communicate2'?, Communicate2),
	output.

  communication_to_operations(Communications, Operations) :-

    Communications ? request(send, ChannelName, Multiplier, Tag),
    string(ChannelName) :
      Operations ! {SPI_SEND, ChannelName, `ChannelName, Multiplier,
		    Tag, []} |
	self;

    Communications ? request(send, ChannelName, Multiplier, Tag),
    ChannelName = Level(Kind(_Name)) :
      Operations ! {SPI_SEND, ChannelName, `Level(Kind(Tag)), Multiplier,
		    Tag, BIO_SCHEDULER} |
	self;

    Communications ? request(receive, ChannelName, Multiplier, Tag),
    string(ChannelName) :
      Operations ! {SPI_RECEIVE, ChannelName, `ChannelName, Multiplier,
		    Tag, []} |
	self;

    Communications ? request(receive, ChannelName, Multiplier, Tag),
    ChannelName = Level(Kind(_Name)) :
      Operations ! {SPI_RECEIVE, ChannelName, `Level(Kind(Tag)), Multiplier,
		    Tag, BIO_SCHEDULER} |
	self;

    Communications ? request(dimer, ChannelName, Multiplier, Tags),
    string(ChannelName) :
      Operations ! {SPI_DIMER, ChannelName, `ChannelName, Multiplier,
		    Tags, []} |
	self;

    Communications ? request(dimer, ChannelName, Multiplier, Tags),
    ChannelName = Level(Kind(_Name)) :
      Operations ! {SPI_DIMER, ChannelName, `Level(Kind(Tags)), Multiplier,
		    Tags, BIO_SCHEDULER} |
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

/* lookup_inter_channels/3
**
** Scan Requests for inter-ambient requests where:
**
**   Locus = Level(Kind)
**
** Generate:
**
**    InterChannelName = Level(Kind(Tag)),
**    InterChannelId   = Level(Kind(ChannelName))
**
** replace ChannelName in Request by InterChannelId
** add a lookup for each such file to Lookups (see
** extract_interchannels.
**
*/

lookup_inter_channels(Requests, NewRequests, Lookups) :-

    Requests ? Request,
    Request =?= Functor(Type, ChannelName, Multiplier, Tag, Locus),
    Locus =?= LOCAL :
      NewRequests ! Functor(Type, ChannelName, Multiplier, Tag) |
	self;

    Requests ? Request,
    Request =?= Functor(Type, ChannelName, Multiplier, Tag, Locus),
    Locus =?= Level(Kind) :
      InterChannelId = Level(Kind(ChannelName)),
      InterChannelName = Level(Kind(Tag)),
      NewRequests ! Functor(Type, InterChannelId, Multiplier, Tag),
      Lookups ! lookup(Locus, `ChannelName, `InterChannelName) |
	self;

    Requests =?= [] :
      NewRequests = [],
      Lookups = [].

/* extract_inter_channels/4
**
** For each Lookup:
**
**   add to Tell to obtain the inter-ambient channel at runtime:
**
**     write_channel(lookup(Locus, Channel, `InterChannelId), BIO_SCHEDULER)
**
**   add to list of InterChannels.
**
*/

extract_inter_channels(Lookups, Tail, Tell, InterChannels) :-

    Lookups ? Lookup,
    Lookup =?= _lookup(_Locus, _Channel, InterChannel) :
      Tell ! write_channel(Lookup, BIO_SCHEDULER),
      InterChannels ! InterChannel |
	self;

    Lookups = [] :
      Tell = Tail,
      InterChannels = [].


/* generate_inter_comm/6
**
** For each Channel in InterChannels (Level((Kind(Tag))),
**    extract the parts of Communicate1:
**       receive ~ Select = Tag,  Message = Variables
**       send    ~ Select = Tag : Message = Variables
**
** where:
**
**   Level = parent or self
**   Kind  = merge or enter or exit
**
**   if Kind = merge or enter or exit:
**
**     inform ambient in Tell:
**	 write_channel(inter_ambient(Level(Kind), Channel, Reply),
**		       BIO_SCHEDULER),
**     in body:
**	 Name.inter.Tag
**
**     and add term:
**	 Name.inter.Tag(<arguments>, Reply) :- known(Reply) | Body .
**
**   if receive and Variables is a tuple, revise guard:
**
**     Select = Tag : write_channel(new_locals(Message, Variables))
*/

generate_inter_comm(Signature, InterChannels,
	Communicate1, Communicate2, Terms1, Terms2) :-

    InterChannels ? `_(Kind(Tag)),
    Kind =\= MERGE, Kind =\= ENTER, Kind =\= EXIT |
	receive_new_locals(Tag,	Communicate1, Communicate1'),
	self;

    InterChannels ? `_(Kind(Tag)),
    otherwise |
	capability(Signature, Kind, Tag, Communicate1, Communicate1',
				Terms1 , Terms1'?),
	self;

    InterChannels =?= [] :
      Signature = _,
      Communicate2 = Communicate1,
      Terms2 = Terms1.

  receive_new_locals(Tag, Communicate1, Communicate2) :-

    Communicate1 ? (Select = Tag, Message = Variables | Body),
    tuple(Variables),
    Arity := arity(Variables) :
      Communicate2 = [(Select = Tag, arity(Message, Arity) :
			write_channel(new_locals(Message, Variables),
				      BIO_SCHEDULER)
			  | Body)
		     | Communicate1'];

    Communicate1 ? (Select = Tag, Message = Variables : Tell | Body),
    tuple(Variables),
    Arity := arity(Variables) :
      Communicate2 = [(Select = Tag, arity(Message, Arity) :
			write_channel(new_locals(Message, Variables),
				      BIO_SCHEDULER), Tell
			  | Body)
		     | Communicate1'];

    Communicate1 ? Other,
    otherwise :
      Communicate2 ! Other |
	self;

    Communicate1 = [] :
      Tag = _,
      Communicate2 = [].

  capability(Signature, Kind, Tag, Communicate1, Communicate2,
				Terms1 , Terms2) :-

    integer(Tag) |
	send_capability(Signature, Tag, Communicate1, Communicate1',
			Terms1, Terms1'),
	receive_capability;

    Tag = {SendTag, ReceiveTag}|
	send_capability(Signature, SendTag,
			Communicate1, Communicate1', Terms1, Terms1'),
	receive_capability + (Tag = ReceiveTag).

  receive_capability(Signature, Kind, Tag, Communicate1, Communicate2,
				Terms1 , Terms2) :-

    Communicate1 ? (Ask | Body),
    Ask =?= (Select = Tag, Message = []) :
      Ask' = (Select = Tag, Message = {BIO_AMBIENT, BIO_READY}) |
	complete_capability + (Tell = true);

    Communicate1 ? (Ask : Tell | Body),
    Ask =?= (Select = Tag, Message = []) :
      Ask' = (Select = Tag, Message = {BIO_AMBIENT, BIO_READY}) |
	complete_capability;

    Communicate1 ? Other,
    otherwise :
      Communicate2 ! Other |
	self;

    Communicate1 = [] :
      Kind = _,
      Signature = _,
      Tag = _,
      Communicate2 = [],
      Terms1 = Terms2.

  send_capability(Signature, Tag, Communicate1, Communicate2,
				Terms1 , Terms2) :-

    Communicate1 ? (Ask : Tell | Body),
    Ask =?= (_Select = Tag),
    Tell =?= (Message = []) :
      Tell' = (Message = {BIO_SCHEDULER, BIO_READY}) |
	complete_capability + (Kind = delegate);

    Communicate1 ? (Ask : Tell | Body),
    Ask =?= (_Select = Tag),
    Tell =?= (Message = [], MoreTell) :
      Tell' = (Message = {BIO_SCHEDULER, BIO_READY}, MoreTell) |
	complete_capability + (Kind = delegate);

    Communicate1 ? Other,
    otherwise :
      Communicate2 ! Other |
	self;

    Communicate1 = [] :
      Signature = _,
      Tag = _,
      Communicate2 = [],
      Terms1 = Terms2.

  complete_capability(Signature, Kind, Tag, Communicate1, Communicate2,
				Ask, Tell, Body, Terms1, Terms2) :-

    Kind =?= delegate,
    arg(1, Signature, Functor),
    convert_to_string(Tag, String),
    string_to_dlist(String, Tail, []),
    string_to_dlist(Functor, FL, [CHAR_MINUS | Tail]),
    list_to_string(FL, Complete) :
      Communicate2 = [(Ask : Tell | Complete) | Communicate1],
      Terms1 = [(Completer? :- known(BIO_READY) | Body) | Terms2] |
	utilities#untuple_predicate_list(',', Tell, Tell'),
	closed_channels(Tell'?, Closed),
	utils#tuple_to_dlist(Signature, [_Functor | Arguments],
				[BIO_SCHEDULER, BIO_READY]),
	utilities#subtract_list(Arguments?, Closed?, Arguments'),
	utils#list_to_tuple([Complete | Arguments'?], Completer);

    Kind =\= delegate,
    arg(1, Signature, Functor),
    convert_to_string(Tag, String),
    string_to_dlist(String, Tail, []),
    string_to_dlist(Functor, FL, [CHAR_MINUS | Tail]),
    list_to_string(FL, Complete) :
      Communicate2 = [(Ask : Tell'''? | Complete) | Communicate1],
      Terms1 = [(Completer? :- known(BIO_READY) | Body) | Terms2] |
	utilities#untuple_predicate_list(',', Tell, Tell'),
	utilities#concatenate_lists([Tell'?,
		[write_channel(Kind(BIO_AMBIENT, BIO_READY), BIO_SCHEDULER)]],
				    Tell''),
	utilities#make_predicate_list(',', Tell''?, Tell'''),
	closed_channels(Tell'?, Closed),
	utils#tuple_to_dlist(Signature, [_Functor | Arguments],
				[BIO_SCHEDULER, BIO_READY]),
	utilities#subtract_list(Arguments?, Closed?, Arguments'),
	utils#list_to_tuple([Complete | Arguments'?], Completer).

  closed_channels(Tell, Closed) :-

    Tell ? write_channel(close(Channels), BIO_SCHEDULER) :
      Tell' = _ |
	tuple_to_dlist(Channels, Closed, []);

    Tell ? _Other,
    otherwise |
	self;

    Tell =?= [] :
      Closed = [].

update_publics(RHSS, NewRHSS, ProcessTable, NextProcessTable) :-

    RHSS =?=  (computation # public_channels(Publics, Scheduler), Body) :
      NewRHSS = (computation # event(public_channels(NewPublics?, Scheduler)),
		 Body),
      ProcessTable = [member(Body, Channels, Ok) | NextProcessTable] |
	update_publics1;

    otherwise :
      NewRHSS = RHSS,
      NextProcessTable = ProcessTable.

  update_publics1(Publics, Channels, NewPublics, Ok) :-

    Ok = true,
    Publics ? Public, arg(1, Public, Name) |
	update_public_list(Name, Channels, Public, NewPublics, NewPublics'?),
	self;

    otherwise :
      Ok = _,
      Channels = _,
      NewPublics = Publics.

  update_public_list(Name, Channels, Public, NewPublics, NextNewPublics) :-

    Channels ? Name :
      Channels' = _,
      NewPublics = [Public | NextNewPublics];

    Channels ? Other, Other =\= Name |
	self;

    Channels =?= [] :
      Name = _,
      Public = _,
      NewPublics = NextNewPublics.


rewrite_clauses(Communicate1, Communicate2) :-

    true :
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
    Requests ? Request,
    arg(6, Request, Locus) :
      ChannelTables ! NewChannelTable |
	extract_message_channels,
	update_prototype(Locus, Code, Channels, Prototype, ClPrototype),
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
	extract_spi_channels;

    arg(2, Request, dimer),
    RHS = (_ : _ = Variables | _) :
      Code = send |
	extract_spi_channels;

    arg(2, Request, dimer),
    RHS = (_, _ = Variables | _) :
      Code = receive |
	extract_spi_channels.

 update_prototype(Locus, Code, Channels, Prototype, NewPrototype) :-

    /* Don't update count of virtual channel sent to another ambient! */
    Code =?= send,
    Locus =\= LOCAL :
      Channels = _,
	NewPrototype = Prototype;

    Code =?= send,
    Locus =?= LOCAL,
    Channels ? ChannelName |
	update_prototype_send(ChannelName, Prototype, Prototype'),
	self;

    Code =?= receive,
    Channels ? ChannelName |
	update_prototype_receive(ChannelName, Prototype, Prototype'),
	self;

    Channels = [] :
      Code = _,
      Locus = _,
      NewPrototype = Prototype.

  update_prototype_send(ChannelName, Prototype, NewPrototype) :-

    Prototype ? ChannelName(Refs, PrimeName),
    Refs++ :
      NewPrototype = [ChannelName(Refs', PrimeName) | Prototype'];

    Prototype ? Other,
    otherwise :
      NewPrototype ! Other |
	self;

    /* Compiler error! */
    Prototype = [] :
      NewPrototype = [] |
	screen#display("Can't find in Prototype" - ChannelName).

  update_prototype_receive(ChannelName, Prototype, NewPrototype) :-

    /* Primed Channel */
    Prototype ? ChannelName(Refs, PrimeName),
    string_to_dlist(ChannelName, CL, Prime) :
      Prime = [39],
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

    /* Call to primed procedure or perhaps some library procedure. */
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


update_rhss(BlockPrefix, RHSS, InterChannels, ChannelTables,
		SignatureTable, NextSignatureTable, Rhss) :-

    RHSS ? RHS,
    ChannelTables ? Table :
      Table ! entries(Entries),
      Rhss ! (NewAsk'? : NewTell'? | NewBody?) |
	partition_rhs(RHS, Ask, Tell, Goals),
	reduce_channel_table(Entries, InterChannels, ForkAsk, ForkTell,
				Close, Table'),
	fork_and_close,	/* Forks -> Asks, Body -> NewBody */
	utilities#untuple_predicate_list(',', Ask, NewAsk, AddAsk?),
	utilities#untuple_predicate_list(',', Tell, NewTell, AddTell?),
	utilities#make_predicate_list(',', NewAsk?, NewAsk'),
	utilities#make_predicate_list(',', NewTell?, NewTell'),
	utilities#untuple_predicate_list(',', Goals?, Goals'),
	ambient_goals(BlockPrefix, Goals'?, NewGoals,
			SignatureTable, SignatureTable'?),
	utilities#make_predicate_list(',', NewGoals?, Body),
	self;

    RHSS =?= [] :
      InterChannels = _,
      BlockPrefix = _,
      ChannelTables = [],
      Rhss = [],
      SignatureTable = NextSignatureTable.

  partition_rhs((Ask : Tell | Body), Ask^, Tell^, Body^).
  partition_rhs((Ask  | Body), Ask^, true^, Body^) :-
    Ask =\= (_ : _) | true.
  partition_rhs(Body, true^, true^, Body^) :-
    Body =\= (_ | _) | true.

  ambient_goals(BlockPrefix, Goals, Body,
	SignatureTable, NextSignatureTable ):-

    Goals =?= [new_ambient(Name, Id, ServiceId), Id | Goals'] :
      SignatureTable ! lookup(Id, Signature, Signature, _Status),
      Body = [self#service_id(ServiceId),
	      write_channel(new_ambient(Name, ServiceId, Goal?),
			    BIO_SCHEDULER)
	     | Body'?] |
	utils#tuple_to_dlist(Signature, [Functor|ArgList], [BIO_SCHEDULER]),
	string_to_dlist(Functor, FL, []),
	string_to_dlist(BlockPrefix, LL, FL?),
	list_to_string(LL?, Functor'),
	utils#list_to_tuple([Functor'? | ArgList?], Goal),
	self;

    Goals ? Goal,
    otherwise :
      Body ! Goal |
	self;

    Goals = [] :
      BlockPrefix = _,
      Body = [],
      NextSignatureTable = SignatureTable.


  reduce_channel_table(Entries, InterChannels, AddAsk, AddTell, Close, Table) +
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
      Closes = InterChannels,
      Table = [] |
	close_channels.

  close_channels(CloseList, Close) :-

    CloseList =?= [] :
      Close = [];


    CloseList =\= [] :
      Close = close(Channels?) |
	list_to_tuple(CloseList, Channels).

  fork_and_close(ForkAsk, ForkTell, Close, Body, AddAsk, AddTell, NewBody) :-

    ForkTell =?= [],
    Close =?= [] :
      ForkAsk = _,
      AddAsk = [],
      AddTell = [],
      NewBody = Body;

    ForkTell =?= [],
    Close =\= [] :
      ForkAsk = _,
      AddAsk = [],
      AddTell = [write_channel(Close?, `"Scheduler.")],
      NewBody = Body;

    ForkTell =?= [_],
    Close =?= [] :
      AddAsk = ForkAsk,
      AddTell = ForkTell,
      NewBody = Body;

    otherwise :
      ForkTell = _,
      AddAsk = [],
      AddTell = [],
      NewBody = (spi_update_channel_refs(List?, `"Scheduler.", `"Scheduler.'"),
		 Body) |
	make_update_channel_refs_list.

  make_update_channel_refs_list(ForkAsk, Close, List) :-

    ForkAsk ? _,
    ForkAsk' ? (`spirefs'(ChName) := `spirefs(ChName) + N) :
      List ! {N, `ChName} |
	self;

    ForkAsk =?= [],
    Close =?= [] :
      List = [];

    ForkAsk =?= [],
    Close =\= [] :
      List = [Close].


dimerize_requests(Tell, NewTell) :-

    Tell ? Request :
      NewTell ! NewRequest? |
	dimerize_requests(Retell?, NewTell'),
	dimerize_requests1;

    Tell = [] :
      NewTell = [].

  dimerize_requests1(Tell, Request, NewRequest, Retell) :-

    Tell ? request(send, ChannelName, Multiplier, SendTag, Locus),
    Request =?= request(receive, ChannelName, Multiplier, ReceiveTag, Locus) :
      NewRequest =
	request(dimer, ChannelName, Multiplier, {SendTag, ReceiveTag}, Locus),
      Retell = [NewRequest | Tell'?] ;

    Tell ? request(receive, ChannelName, Multiplier, ReceiveTag, Locus),
    Request =?= request(send, ChannelName, Multiplier, SendTag, Locus) :
      NewRequest =
	request(dimer, ChannelName, Multiplier, {SendTag, ReceiveTag}, Locus),
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


kluge_news(Asks, Tells, Channels, Ok, NewAsks, NewTells) :-

    Ok =?= true |
	utilities#untuple_predicate_list(",", Asks, Asks', NewConvert?),
	utilities#untuple_predicate_list(",", Tells, Tells'),
	ignore_redundant_news(Channels, Tells'?, Tells''),
	kluge_new_channels + (Convert = []),
	utilities#make_predicate_list(",", Asks'?, NewAsks),
	utilities#make_predicate_list(",", NewTells'?, NewTells);

    Ok =\= true :
      Channels = _,
      NewAsks = Asks,
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

	
kluge_publics(RHSS, NewRHSS) :-

    RHSS =?= (computation # public_channels(Publics, Scheduler), Goals) :
      NewRHSS =
	(Ask? | computation # event(public_channels(NewPublics?, Scheduler)),
		Goals) |
	kluge_public_channels + (Convert = []),
	utilities#make_predicate_list(",", NewConvert?, Ask);

    otherwise :
      NewRHSS = RHSS.


  kluge_public_channels(Publics, Convert, NewPublics, NewConvert) :-

    Publics ? Public, Public =?= Name(Channel, ComputeWeight, BaseRate),
    tuple(ComputeWeight), ComputeWeight =\= `_,
    arity(ComputeWeight, A),
    make_tuple(A, ComputeWeight') :
      NewPublics ! Name(Channel, ComputeWeight'?, BaseRate) |
	kluge_real_parameters(ComputeWeight, Convert, 0,
				ComputeWeight', Convert'),
	self;

    Publics ? Public,
    otherwise :
      NewPublics ! Public |
	self;

    Publics =?= [] :
      NewConvert = Convert,
      NewPublics = [].

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
