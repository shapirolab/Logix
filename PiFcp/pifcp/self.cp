/*
Precompiler for Pi Calculus procedures.

Bill Silverman, December 1999.

Last update by		$Author: bill $
		       	$Date: 2000/04/16 08:04:01 $
Currently locked by 	$Locker:  $
			$Revision: 2.0 $
			$Source: /home/qiana/Repository/PiFcp/pifcp/self.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export(transform/5).
-mode(failsafe).
-language(compound).

/*
** Transform/5
**
** Transform pifcp module to compound Fcp.
**
** This module should be integrated with the transform application,
** which should recognize pifcp alone and translate it to:
**
**    [evaluate,pifcp,compound,colon]
**
** Input:
**
**   Attributes1 - Source attributes.
**   Source      - Pifcp code, minus attributes.
**
** Output:
**
**   Attributes2 - Attributes1 augmented by exported Fcp procedures.
**   Compound    - Compound Fcp code.
**   Errors      - Diagnostics in the form:  Name - comment(ARgument)
*/

transform(Attributes1, Source, Attributes2, Compound, Errors) :-

    true :
      Compound = Terms? |

	/* Get Exported list. */
	filter_attributes(Attributes1, Attributes1', Exported),
	Attributes2 = [export(Exports?) | Attributes1'?],
	program.

  filter_attributes(In, Out, Exported) :-

    In ? export(Es), string(Es), Es =\= all :
      Exported = [Es] |
	self;

    In ? export(all) :
      Exported = all |
	self;

    In ? export(Es), list(Es) :
      Exported = Es |
	self;

    In ? Other,
    Other =\= export(_) :
      Out ! Other |
	self;

    In =?= [] :
      Out = [] |
	unify_without_failure(Exported, all).


program(Source, Exported, Exports, Terms, Errors) :-

	filter_pifcp_attributes(Source, Exported, Controls, Delay, Source',
					Errors, Errors'?),
	servers#serve_empty_scope(Scope?, Controls?, Exports,
					NextTerms, Errors'),
	process_definitions+(Processes = [], NextScope = []),
	spc#output(Terms'?, Delay, Terms).


/* Extract Global channel declarations and Stochastic means. */
filter_pifcp_attributes(Source, Exported, Controls, Delay, NextSource,
			Errors, NextErrors) +
	(GlobalDescriptors = [], Means = _Delay(_Receive, _Send)) :-

    Source ? String, string(String) |
	pifcp_attribute(String, GlobalDescriptors, GlobalDescriptors',
				Means, Means', Errors, Errors'),
	self;

    Source ? Tuple, Tuple =\= (_ :- _) |
	pifcp_attribute(Tuple, GlobalDescriptors, GlobalDescriptors',
				Means, Means', Errors, Errors'),
	self;

    otherwise :
      NextSource = Source,
      Errors = NextErrors |
	complete_pifcp_attributes.

  pifcp_attribute(Attribute, OldDescriptors, NewDescriptors,
			Means1, Means, Errors, Errors') :-

    Attribute = global(Gs) :
      Means = Means1 |
	validate_globals(Gs, Means, OldDescriptors, NewDescriptors,
				Errors, Errors');

    Attribute = stochastic :
      Errors' = Errors,
      NewDescriptors = OldDescriptors,
      Means1 = _(Receive, Send),
      Means = stochastic(Receive, Send);
  
    Attribute = stochastic(Both) :
      NewDescriptors = OldDescriptors,
      Means3 = stochastic(_Receive, _Send),
      Means = Means3 |
	validate_means(Both, Both, Means1, Means2, Errors, Errors'),
	update_means;

    Attribute = stochastic(Receive, Send) :
      NewDescriptors = OldDescriptors,
      Means2 = stochastic(_Receive, _Send),
      Means = Means3? |
	validate_means(Receive, Send, Means1, Means2, Errors, Errors'),
	update_means;

    otherwise :
      NewDescriptors = OldDescriptors,
      Means = Means1,
      Errors ! invalid_pifcp_attribute(Attribute).

  complete_pifcp_attributes(Exported, Means, Delay, GlobalDescriptors,
					Controls) :-

    Means =?= stochastic(_Receive, _Send) :
      Controls = {Exported?, GlobalDescriptors, GlobalNames?, Means?},
      Delay = stochastic |
	extract_global_names,
	unify_without_failure(Means, _(0, 0));

    true :
      Means = none(0, 0),
      Controls = {Exported?, GlobalDescriptors, GlobalNames?, Means},
      Delay = none |
	extract_global_names.

  update_means(Means1, Means2, Means3) :-

    true :
      Means1 = Means2,
      Means3 = Means2;

    otherwise :
      Means1 = _,
      Means3 = Means2.

  extract_global_names(GlobalDescriptors, GlobalNames) :-

    GlobalDescriptors ? Name(_Receive, _Send) :
      GlobalNames ! Name |
	self;

    GlobalDescriptors =?= [] :
      GlobalNames = [].

  validate_globals(GlobalDescriptors, Means, Old, New, Errors, NextErrors) +
			(Head = Tail?, Tail) :-

    GlobalDescriptors ? String,
    string(String), String =\= "", String =\= "_",
    Means =?= _Type(Receive, Send) :
      Tail ! String(Receive, Send) |
	self;

    GlobalDescriptors ? String(Both),
    string(String), String =\= "", String =\= "_" :
      Global = String(_Receive, _Send),
      Tail ! Global |
	validate_means(Both, Both, Means, Global, Errors, Errors'),
	self;

    GlobalDescriptors ? String(Receive, Send),
    string(String), String =\= "", String =\= "_" :
      Global = String(_Receive, _Send),
      Tail ! Global |
	validate_means(Receive, Send, Means, Global, Errors, Errors'),
	self;

    GlobalDescriptors ? Other, otherwise :
      Errors ! invalid_global_channel_name(Other) |
	self;

    GlobalDescriptors =\= [], GlobalDescriptors =\= [_|_] :
      GlobalDescriptors' = [GlobalDescriptors] |
	self;

    GlobalDescriptors =?= [] :
      Means = _,
      Tail = [],
      Errors = NextErrors |
	piutils#sort_out_duplicates([Old], Old', Reply),
	piutils#sort_out_duplicates([Old'?, Head], New, _Reply),
	diagnose_duplicates + (Diagnostic = duplicate_global_channel).

  diagnose_duplicates(Reply, Diagnostic, Errors, NextErrors) :-

    Reply ? Duplicate :
      Errors ! Diagnostic(Duplicate) |
	self;

    Reply =?= [] :
      Diagnostic = _,
      Errors = NextErrors;

    otherwise :
      Reply = _,
      Errors = [Diagnostic | NextErrors].

validate_means(ReceiveMean, SendMean, Means, Global, Errors, NextErrors) :-

    number(ReceiveMean), 0 =< ReceiveMean,
    number(SendMean), 0 =< SendMean :
      Means = _,
      Global = _Name(ReceiveMean, SendMean),
      Errors = NextErrors;

    otherwise,
    Means = _Type(Receive, Send) :
      Global = _Name(Receive, Send),
      Errors = [invalid_means(ReceiveMean, SendMean) | NextErrors].

/************************* Program Transformations ***************************/

process_definitions(Source, Processes, Terms, NextTerms, Scope, NextScope) :-

    Source ? (PiLHS :- RHSS) :
      Scope ! process(PiLHS, NewChannelList, ProcessScope),
      ProcessScope ! lhss(OuterLHS, InnerLHS) |
	process_definitions(Processes, [], Nested, Nested'?,
				ProcessScope', ProcessScope''?),
	process(RHSS, OuterLHS, InnerLHS, NewChannelList, ProcessScope'',
			Process, Nested'),
	nested_procedures(Process, Nested?, Terms, Terms'?),
	self;

    Source ? P,
    P =\= (_ :- _) :
      Scope ! error(invalid_process_definition(P)) |
	self;

    Source = [] :
      Processes = _,
      Terms = NextTerms,
      Scope = NextScope.

/************************* Process Transformations ***************************/

process(RHSS, OuterLHS, InnerLHS, NewChannelList, Scope, Process, Nested) :-

    OuterLHS =?= [] :
      RHSS = _,
      InnerLHS = _,
      NewChannelList = _,
      Scope = [],
      Process = [],
      Nested = [];

    OuterLHS =\= [],
    NewChannelList =\= [] :
      Nested ! outer(Atom?, Initializer?, []),
      NewChannelList' = [] |
	piutils#tuple_to_atom(OuterLHS, Atom),
	arg(1, InnerLHS, Name),
	initialize_channels(Name, NewChannelList, Initializer),
	self;

    OuterLHS =\= [],
    NewChannelList =?= [],
    RHSS =\= (_|_), RHSS =\= (_;_)  :
      Scope ! code(no_guard, [], []),
      Process = no_guard(Atom?, RHSS'?, []) |
	piutils#tuple_to_atom(InnerLHS?, Atom),
	transform_body(RHSS, RHSS', Nested, [], Scope', []);

    otherwise :
      OuterLHS = _,
      NewChannelList = _,
      Process = _Type(Atom?, RHSS'?, _Action) |
	piutils#tuple_to_atom(InnerLHS?, Atom),
	guarded_clauses(RHSS, RHSS', Process, Nested, Scope).

  initialize_channels(Name, NewChannelList, Initializer) +
			(Body = Name, MakeList = Make?, Make) :-

    NewChannelList ? Descriptor |
	make_and_name_channel(Name, Body, Descriptor, Body', Make, Make'),
	self;

    NewChannelList = [] :
      Name = _,
      Make = [],
      Initializer = (true : Tell? | Body) |
	piutils#make_predicate_list(',', MakeList?, Tell).

  make_and_name_channel(Name, Body, Descriptor, NewBody, Make, NextMake) :-

    Descriptor = ChannelName(Receive, Send),
    string_to_dlist(ChannelName, Suffix, []),
    string_to_dlist(Name, PH, PS) :
      Make = [make_vector(2, `pivec(ChannelName), `pimss(ChannelName)),
	      `pimss(ChannelName) = {`pieos(ChannelName), `"_"},
	      store_vector(2, `pieos(ChannelName), `pivec(ChannelName)),
              `ChannelName =
			ChannelId?(`pivec(ChannelName), {Receive'?, Send'?}) |
	      NextMake],
      PS = Suffix |
	list_to_string(PH, ChannelId),
	piutils#real_mean_kluge(Receive, Body, Receive', Body'),
	piutils#real_mean_kluge(Send, Body'?, Send', NewBody).

nested_procedures(Process, Nested, Terms, NextTerms) :-

    Process =\= [] :
      Terms ! Process,
      Process' = [] |
	self;

    Process =?= [],
    Nested ? Proc :
      Terms ! Proc |
	self;

    Process =?= [],
    Nested =?= [] :
      Terms = NextTerms.


/************************* Guard Transformations *****************************/

guarded_clauses(RHS1, RHS2, Process, Nested, Scope) +
		(Mode = none, SendId = _, Index = 0,
	NextRHSS, RHSS = NextRHSS?, NextPrepares = [], FinalMode = _) :-

    RHS1 =?= (_ | _),
    Index++ :
      NextRHSS = [Clauses?],
      FinalMode = Mode'? |
	guarded_clause(RHS1, GuardMode(SendId?, Index'), Clauses,
			Nested, [], Scope, Scope'?),
	piutils#update_process_mode(Mode, GuardMode, Mode'),
	make_right_hand_side + (Index = 1),
	make_rhs2 + (Scope = Scope', NextScope = Scope''?),
	code_reply;

    RHS1 =?= (Guarded ; RHS1'), Guarded =?= (_|_),
    Index++ :
      NextRHSS ! Clauses? |
	guarded_clause(Guarded, GuardMode(SendId?, Index'), Clauses,
			Nested, Nested'?, Scope, Scope'?),
	piutils#update_process_mode(Mode, GuardMode, Mode'),
	self;

    otherwise :
      Process = _,
      Index = _,
      FinalMode = none,
      Scope ! error(invalid_guarded_clause(RHS1)),
      NextRHSS = [],
      Nested = [] |
	make_right_hand_side + (Index = 1),
	make_rhs2 + (PrepareProcedure = _, NextScope = []).

  code_reply(Process, FinalMode, PrepareProcedure, Scope) :-

    FinalMode =?= cdr :
      Process = cdr(_Atom, ProcessRHS, []),
      PrepareProcedure = _,
      Scope = [code(cdr, [], ProcessRHS)];

    PrepareProcedure =?= (_ :- PrepareRHS) :
      Process = FinalMode(_Atom, ProcessRHS, PrepareProcedure),
      Scope = [code(FinalMode, ProcessRHS, PrepareRHS)];

    otherwise :
      PrepareProcedure = _,
      Process = FinalMode(_Atom, _RHSS, []),
      Scope = [code(FinalMode, [], [])].

  make_rhs2(Mode, SendId, ClauseList, Prepares, RHS2,
		PrepareProcedure, Scope, NextScope) :-

    Mode =?= receive :
      SendId = "_",
      RHS2 = (PrepareGuards? | Communicator?),
      PrepareProcedure = (CommunicationAtom? :- FcpClauses?),
      Scope = [lhss(OuterLHS, InnerLHS) | NextScope] |
	arg(1, OuterLHS, PName),
	make_communication_name(PName, ".receive", Communicator),
	piutils#make_predicate_list(';', ClauseList, FcpClauses),
	prepares_to_guards(Prepares, PrepareGuards, Streams),
	make_communication_atom + (ChoiceVars = Streams);

    Mode =?= send :
      SendId = "_",
      RHS2 = (PrepareGuards? | Communicator?),
      PrepareProcedure = (CommunicationAtom? :- FcpClauses?),
      Scope = [lhss(OuterLHS, InnerLHS) | NextScope] |
	arg(1, OuterLHS, PName),
	make_communication_name(PName, ".send", Communicator),
	piutils#make_predicate_list(';', ClauseList, FcpClauses),
	prepares_to_guards(Prepares, PrepareGuards, _Streams),
	make_communication_atom + (ChoiceVars = [`pifcp(chosen)]);

    Mode =?= mixed :
      SendId = pifcp(sendid),
      RHS2 = (PrepareGuards? | pi_monitor#unique_sender(PName?, `SendId),
			Communicator),
      PrepareProcedure = (CommunicationAtom? :- FcpClauses?),
      Scope = [lhss(OuterLHS, InnerLHS) | NextScope] |
	arg(1, OuterLHS, PName),
	make_communication_name(PName?, ".mixed", Communicator),
	piutils#make_predicate_list(';', ClauseList, FcpClauses),
	prepares_to_guards(Prepares, PrepareGuards, Streams),
	make_communication_atom +
		(ChoiceVars = [`pifcp(chosen),`SendId | Streams?]);

    /* compared, logix, none */
    Mode =\= receive, Mode =\= send, Mode =\= mixed,
	Mode =\= compare, Mode =\= conflict :
      Prepares = _,
      SendId = "_",
      RHS2 = FcpClauses?,
      PrepareProcedure = [],
      Scope = NextScope |
	piutils#make_predicate_list(';', ClauseList, FcpClauses);

    Mode =?= compare :
      Prepares = _,
      SendId = "_",
      RHS2 = FcpClauses?,
      PrepareProcedure = [],
      Scope = [error("missing_otherwise") | NextScope] |
	piutils#make_predicate_list(';', ClauseList, FcpClauses);

    Mode =?= conflict :
      Prepares = _,
      SendId = "_",
      ClauseList = _,
      RHS2 = true,
      PrepareProcedure = [],
      Scope = [error("conflicting_guards") | NextScope].
      

  prepares_to_guards(Prepares, PrepareGuards, Streams) +
	(NextAsk, Asks = NextAsk?, NextTell, Tells = NextTell,
	 NextRead, Reads = NextRead?) :-

    Prepares ? {Ask, Tell} :
      NextAsk ! Ask,
      NextTell ! Tell |
	self;

    Prepares ? (Identify, Read) :
      Read = read_vector(_, _, Stream),
      Streams ! Stream,
      NextRead ! Identify,
      NextRead' ! Read |
	self;

    Prepares =?= [] :
      NextAsk = Reads,
      NextRead = [],
      NextTell = [],
      PrepareGuards = (Asks'? : Tells'?),
      Streams = [] |
	piutils#make_predicate_list(',', Asks, Asks'),
	piutils#make_predicate_list(',', Tells, Tells').

  make_communication_name(Prefix, Suffix, Communicator) :-
    string_to_dlist(Prefix, PL, PS),
    string_to_dlist(Suffix, SL, []) :
      PS = SL |
	list_to_string(PL, Communicator).

  make_communication_atom(InnerLHS, Communicator, ChoiceVars,
				CommunicationAtom) :-

	utils#tuple_to_dlist(InnerLHS, [_ | Channels], ChoiceVars),
	utils#list_to_tuple([Communicator | Channels], CommunicationAtom).

  make_right_hand_side(RHSS, FinalMode, Index, ClauseList,
				Prepares, NextPrepares) :-

    RHSS ? RHS,
    RHS = {Mode, RHSList},
    Index++ |
	make_clauselist(Mode, FinalMode, Index, RHSList,
		ClauseList, ClauseList'?, Prepares, Prepares'?),
	self;

    RHSS ? true,
    Index++ |
	self;

    RHSS =?= [] :
      FinalMode = _,
      Index = _,
      ClauseList = [],
      Prepares = NextPrepares.


  make_clauselist(Mode, FinalMode, Index, RHSList,
	ClauseList, NextClauseList, Prepares, NextPrepares) :-

    Mode =?= receive,
    RHSList = [Cdr, Consume] :
      FinalMode = _,
      Index = _,
      ClauseList = [Cdr, Consume | NextClauseList],
      Prepares = [PrepareRead? | NextPrepares] |
	identify_read;

    Mode =?= receive,
    RHSList = [Cdr, Exclude, Consume] :
      FinalMode = _,
      Index = _,
      ClauseList = [Cdr, Exclude, Consume | NextClauseList],
      Prepares = [PrepareRead? | NextPrepares] |
	identify_read;

    Mode =?= send, FinalMode =?= mixed,
    RHSList ? (Send | Body) :
      Prepares ! Send,
      ClauseList ! (`pifcp(chosen) = Index : `pifcp(sendid) = `"_" | Body) |
	self;

    Mode = send, FinalMode =\= mixed,
    RHSList ? (Send | Body) :
      Prepares ! Send,
      ClauseList ! (`pifcp(chosen) = Index | Body) |
	self;

    Mode =?= none,
    RHSList ? _ |
	self;

    Mode =\= send, Mode =\= none, Mode =\= receive,
    RHSList ? Other :
      ClauseList ! Other |
	self;

    RHSList =?= [] :
      Mode = _,
      FinalMode = _,
      Index = _,
      ClauseList = NextClauseList,
      Prepares = NextPrepares.

  identify_read(Consume, PrepareRead) :-

    Consume = (Stream ? _, Identify, _ : store_vector(2, _, Vector), _ | _) :
      PrepareRead = (Identify, read_vector(2, Vector, Stream)).


guarded_clause(RHS1, Control, Clauses, Nested, NextNested,
			Scope, NextScope) :-

    /* Recognize compound_guard */
    RHS1 =?= (Guard | Guarded), Guarded =?= (_ | _) :
      RHS1' = (Guard | [Guarded]) |
	self;

    RHS1 =?= (Guard | Guarded), Guarded =?= (_ ; _) :
      RHS1' = (Guard | [Guarded]) |
	self;

    RHS1 =?= (Cdr | self),
    tuple(Cdr), arity(Cdr) > 1, arg(1, Cdr, cdr) :
      Control = GuardMode(_, _),
      Clauses = cdr(ClauseList?),
      Nested = NextNested,
      Scope = [guard_cdr(Cdr, ClauseList, GuardMode) | NextScope] ;

    RHS1 =?= (Guard | Body1), Body1 =\= (_ | _), Body1 =\= (_ ; _) :
      LastClause = (BodyGuard? | Body2?) |
	transform_guard(Guard, Control, LastClause, Clauses, BodyGuard,
			Scope, Scope'?),
	transform_body.

transform_guard(Guard, Control, LastClause, Clauses, BodyGuard,
			Scope, NextScope) :-

    Guard =?= (Channel ? Message) :
      Control = receive(SendId, _SendIndex),
      Scope = [guard_receive(Channel, Message, SendId, Iterates, BodyGuard) |
		NextScope] |
	receive_in_guard_iterates;

    Guard =?= (Channel ! Message) :
      Control = send(SendId, SendIndex),
      Scope = [guard_send(Channel, Message, SendId, SendIndex, BodyGuard) |
		NextScope],
      Clauses = send([LastClause]);

    Guard =?= otherwise :
      Control = otherwise(_SendId, _SendIndex),
      Clauses = otherwise([LastClause]),
      BodyGuard = otherwise,
      Scope = NextScope;      

    Guard =\= (_ =?= _), Guard =\= (_ =\= _),
    Guard =\= (_ & _), Guard =\= otherwise |
	logix_guards;

    otherwise :
      Clauses = compare([LastClause]),
      Control = compare(_SendId, _SendIndex) |
	compare_channels + (Channels = [], NextChannels = _).

  receive_in_guard_iterates(SendId, Iterates, LastClause, Clauses) :-

    SendId =?= "_",
    Iterates =?= {Cdr, _Mixed} :
      Clauses = receive([Cdr, LastClause]);

    SendId =\= "_",
    Iterates =?= {Cdr, Mixed} :
      Clauses = receive([Cdr, Mixed, LastClause]).

logix_guards(Guard, Control, LastClause, Clauses, BodyGuard,
			Scope, NextScope) :-

    tuple(Guard),
    Guard =\= `_, Guard =\= ?_,
    arity(Guard, Arity) :
      Clauses = logix([LastClause]),
      Control = logix(_, _),
      Scope = NextScope,
      Index = 1 |
	copy_ask_guards;

    otherwise:
      BodyGuard = true,
      Clauses = none([LastClause]),
      Control = none(_, _),
      Scope = [error(invalid_guard(Guard)) | NextScope].

  copy_ask_guards(Guard, Index, Arity, BodyGuard) :-

    Index++ < Arity,
    arg(Index, Guard, Predicate) :
      BodyGuard = (Predicate, BodyGuard') |
	self;

    Index =:= Arity,
    arg(Index, Guard, Predicate) :
      BodyGuard = Predicate.


compare_channels(Guard, BodyGuard, Channels, NextChannels, Scope, NextScope) :-

    Guard =?= (Guard' & Compares) :
      BodyGuard = (BodyGuard'?, Comparers?) |
	compare_channels(Guard', BodyGuard', Channels, Channels',
				Scope, Scope'?),
	compare_channels(Compares, Comparers, Channels'?, NextChannels,
				Scope', NextScope);

    Guard =\= (_ & _), Guard =\= (_ =?= _), Guard =\= (_ =\= _) :
      BodyGuard = true,
      NextChannels = Channels,
      Scope = [error(invalid_compare_guard(Guard)) | NextScope];

    otherwise :
      Scope = [guard_compare(Guard, Channels, NextChannels, BodyGuard) |
		NextScope].


/************************* Body Transformations ******************************/

transform_body(Body1, Body2, Nested, NextNested, Scope, NextScope) :-
    true :
      NextGoals = [] |
	transform_body1,
	piutils#make_predicate_list(',', Goals?, Body2).

  transform_body1(Body1, Goals, NextGoals, Nested, NextNested,
			Scope, NextScope) :-

    Body1 = (Body2, Body1') |
	transform_body1(Body2, Goals, Goals'?, Nested, Nested'?,
			Scope, Scope'?),
	self;

    Body1 = (Channel ? Message) :
      Scope ! body_receive(Channel, Message, Channel', ChannelList),
      Goals = [pi_receive(Channel'?, ChannelList?) | NextGoals],
      Nested = NextNested,
      Scope' = NextScope;

    Body1 = (Channel ! Message) :
      Scope ! body_send(Message, Channel, Sender, ChannelList, Channel'),
      Goals = [pi_send(Sender?, ChannelList?, Channel'?) | NextGoals],
      Nested = NextNested,
      Scope' = NextScope;

    list(Body1) :
      Goals = [Body2 | NextGoals] |
	new_scope;

    Body1 =?= Name # Call1 :
      Goals = [(Name # Call2) | NextGoals],
      Nested = NextNested |
	parse_remote_call(Call1, Call2, Scope, NextScope);

    otherwise :
      Goals = [Body2 | NextGoals],
      Scope ! call(Body1, Body2),
      Nested = NextNested,
      Scope' = NextScope.

  new_scope(Body1, Body2, Nested, NextNested, Scope, NextScope) :-

    Body1 =?= [(_ :- _) | _] :
      Body2 = true,
      Nested = NextNested,
      Scope = [error(incomplete_new_scope(Body1)) | NextScope];

    Body1 =?= [Body],
    Body =\= (_ :- _) :
      Processes = [],
      Channels = [] |
	expand_new_scope;

    Body1 =?= [Body | Processes],
    Body =\= (_ :- _), Processes =?= [(_ :- _)| _] :
      Channels = [] |
	expand_new_scope;

    Body1 =?= [Channels, Body | Processes],
    Channels =\= (_ :- _), Body =\= (_ :- _) |
	expand_new_scope.


expand_new_scope(Channels, Body, Processes, Body2,
		Nested, NextNested, Scope, NextScope) :-
    true :
      Scope ! new_scope_id(Id),
      Body2 = Id? |
	make_new_lhs,
	process_definitions([(PiLHS :- Body)], Processes, Nested, NextNested,
				Scope', NextScope).

  make_new_lhs(Id, Channels, PiLHS) :-

    Channels =?= [] :
      PiLHS = `Id;

    Channels =\= [] :
      PiLHS = `Id + Channels.


parse_remote_call(Call1, Call2, Scope, NextScope) :-

    Call1 =?= Name # Call1', string(Name) :
      Call2 = Name # Call2' |
	self;

    Call1 =\= _ # _, Call1 =\= `_,
    tuple(Call1), arg(1, Call1, Name), string(Name) :
      Call2 = Call1,
      Scope = NextScope;

    Call1 = `Name, string(Name) :
      Call2 = Name,
      Scope = NextScope;

    Call1 =\= _ # _,
    tuple(Call1), arg(1, Call1, `_) :
      Scope = [remote_call(Call1, Call2) | NextScope].
