/*
Precompiler for Pi Calculus procedures.

Bill Silverman, December 1999.

Last update by		$Author: bill $
		       	$Date: 2000/02/13 09:03:08 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/PiFcp/pifcp/self.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export(transform/5).
-mode(interrupt).
-language(compound).

/*
** Transform/5
**
** Transform pifcp module to compound Fcp.
**
** This module should be integrated with the transform application.
**
** Input:
**
**   Attributes1 - Source attributes.
**   Source      - Pifcp code, minus attributes.
**
** Output:
**
**   Attributes2 - Attributes1 augmented by exported Fcp procedures.
**   Terms       - Compound Fcp code.
**   Errors      - Diagnostice in the form:  Name - comment(ARgument)
*/

transform(Attributes1, Source, Attributes2, Terms, Errors) :-

	/* Extract Global channel declarations, and get Exported list. */
	filter_attributes(Attributes1, [], GlobalList, Attributes1', Exported,
				Errors, Errors'?),
	Attributes2 = [export(Exports?) | Attributes1'?],
	program.

  filter_attributes(In, GlobalList, NewGlobalList, Out, Exported,
			Errors, NextErrors) :-

    In ? export(Es), string(Es), Es =\= all :
      Exported = [Es] |
	self;

    In ? export(Es), list(Es) :
      Exported = Es |
	self;

    In ? global(Gs) |
	validate_globals(Gs, GlobalList, GlobalList', Errors, Errors'?),
	self;

    In ? Other,
    Other =\= export(_), Other =\= global(_) :
      Out ! Other |
	self;

    In = [] :
      NewGlobalList = GlobalList,
      Out = [],
      Errors = NextErrors |
	unify_without_failure(Exported, all).


  validate_globals(GlobalList, Old, New, Errors, NextErrors) +
			(Head = Tail?, Tail) :-

    GlobalList ? String, String =\= "", String =\= "_" :
      Tail ! String |
	self;

    GlobalList ? Other, otherwise :
      Errors ! invalid_global_channel_name(Other) |
	self;

    GlobalList =\= [], GlobalList =\= [_|_] :
      GlobalList' = [GlobalList] |
	self;

    GlobalList =?= [] :
      Tail = Old,
      Errors = NextErrors |
	utils#binary_sort_merge(Head, New).


/************************* Program Transformations ***************************/

program(Source, GlobalList, Exported, Exports, Terms, Errors) :-

	servers#serve_empty_scope(Scope?, GlobalList, Exported, Exports,
					NextTerms, Errors),
	process_definitions+(Processes = [], NextScope = []).

process_definitions(Source, Processes, Terms, NextTerms, Scope, NextScope) :-

    Source ? (PiLHS :- RHSS) :
      Scope ! process(PiLHS, Status, ProcessScope) |
	process_definitions(Processes, [], Nested, Nested'?,
				ProcessScope, ProcessScope'?),
	process(Status?, RHSS, ProcessScope', Process, Nested'),
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

process(Status, RHSS, Scope, Process, Nested) :-

    Status = nil :
      RHSS = _,
      Scope = [],
      Process = [],
      Nested = [];

    Status = double :
      Scope ! lhss(OuterLHS, InnerLHS),    
      Nested ! (Atom? :- Initializer?),
      Status' = initialized |
	piutils#tuple_to_atom(OuterLHS?, Atom),
	Index := arity(OuterLHS) + 1,
	arg(1, OuterLHS, Name),
	initialize_channels(Index, InnerLHS, Name, Initializer),
	self;

    Status =\= nil, Status =\= double, /* single or initialized */
    RHSS =\= (_|_), RHSS =\= (_;_)  :
      Scope = [lhss(_OuterLHS, InnerLHS), code(no_guard, [], []) | Scope'?],
      Process = (Atom? :- RHSS'?) |
	piutils#tuple_to_atom(InnerLHS?, Atom),
	transform_body(RHSS, RHSS', Nested, [], Scope', []);

    Status =\= nil, Status =\= double, /* single or initialized */
    otherwise :
      Scope ! lhss(_OuterLHS, InnerLHS),
      Process = (Atom? :- RHSS'?) |
	piutils#tuple_to_atom(InnerLHS?, Atom),
	guarded_clauses(RHSS, RHSS', Process, Nested, Scope').

  initialize_channels(Index, Tuple, Name, Initializer) +
			(MakeAll = More?, More) :-

    Index < arity(Tuple),
    arg(Index, Tuple, `Channel),
    Index++ :
      More = (MakeChannel?, NameChannel?, More'?) |
	make_and_name_channel,
	self;

    Index =:= arity(Tuple),
    arg(Index, Tuple, `Channel),
    arg(1, Tuple, PName) :
      More = (MakeChannel?, NameChannel?),
      Initializer = (true : MakeAll | PName) |
	make_and_name_channel.

  make_and_name_channel(Name, Channel, MakeChannel, NameChannel) :-

    string_to_dlist(".", DotName, Dt),
    string_to_dlist(Channel, Suffix, []),
    string_to_dlist(Name, PH, PS) :
      MakeChannel = make_channel(`pinch(Channel), `pimss(Channel)),
      NameChannel = (`Channel = ChannelId?(?pinch(Channel), ?pimss(Channel))),
      Dt = Suffix,
      PS = DotName |
	list_to_string(PH, ChannelId).


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
	NextRHSS, RHSS = NextRHSS?, NextSends = [], FinalMode = _) :-

    RHS1 =?= (_ | _),
    Index++ :
      NextRHSS = [Clauses?],
      FinalMode = Mode'? |
	guarded_clause(RHS1, GuardMode(SendId?, Index'), Clauses,
			Nested, Nested'?, Scope, Scope'?),
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
	make_rhs2 + (SendProcedure = _, NextScope = []).

  code_reply(Process, FinalMode, SendProcedure, Nested, Scope) :-

    FinalMode =?= receive,
    Process =?= (_ :- ProcessRHS) :
      SendProcedure = _,
      Nested = [],
      Scope = [code(receive, [], ProcessRHS)];

    SendProcedure =?= (_ :- SendRHS),
    Process =?= (_ :- ProcessRHS) :
      Nested = [SendProcedure],
      Scope = [code(FinalMode, ProcessRHS, SendRHS)];

    otherwise :
      Process = _,
      SendProcedure = _,
      Nested = [],
      Scope = [code(FinalMode, [], [])].

  make_rhs2(Mode, SendId, ClauseList, Sends, RHS2,
	SendProcedure, Scope, NextScope) :-

    Mode =?= send :
      SendId = "_",
      RHS2 = (Writes | SendChoices),
      SendProcedure = (ChoiceAtom? :- FcpClauses?),
      Scope = [lhss(OuterLHS, InnerLHS) | NextScope] |
	arg(1, OuterLHS, PName),
	make_choice_name(PName, ".sends", SendChoices),
	piutils#make_predicate_list(';', ClauseList, FcpClauses),
	sends_to_writes(Sends, Writes),
	make_choice_atom+(Name = SendChoices, ChoiceVars = [`pifcp(chosen)]);

    Mode =?= mixed :
      SendId = pifcp(sendid),
      RHS2 = (Writes | pi_monitor#unique_sender(PName?, `SendId),
			MixedChoices),
      SendProcedure = (ChoiceAtom? :- FcpClauses?),
      Scope = [lhss(OuterLHS, InnerLHS) | NextScope] |
	arg(1, OuterLHS, PName),
	make_choice_name(PName?, ".mixed", MixedChoices),
	piutils#make_predicate_list(';', ClauseList, FcpClauses),
	sends_to_writes(Sends, Writes),
	make_choice_atom+(Name = MixedChoices,
			ChoiceVars = [`pifcp(chosen), `SendId]);

    /* receive, compared, logix, none */
    Mode =\= send, Mode =\= mixed, Mode =\= compare, Mode =\= conflict :
      Sends = [],
      SendId = "_",
      RHS2 = FcpClauses?,
      SendProcedure = [],
      Scope = NextScope |
	piutils#make_predicate_list(';', ClauseList, FcpClauses);

    Mode =?= compare :
      SendId = "_",
      Sends = [],
      RHS2 = FcpClauses?,
      SendProcedure = [],
      Scope = [error("missing_otherwise") | NextScope] |
	piutils#make_predicate_list(';', ClauseList, FcpClauses);

    Mode =?= conflict :
      SendId = "_",
      ClauseList = [],
      Sends = [],
      RHS2 = true,
      SendProcedure = [],
      Scope = [error("conflicting_guards") | NextScope].
      

  sends_to_writes(Sends, Writes) +
	(NextAsk, Asks = NextAsk?, NextTell, Tells = NextTell) :-

    Sends ? {Ask, Tell}, Sends' =\= [] :
      NextAsk = (Ask, NextAsk'?),
      NextTell = (Tell, NextTell'?) |
	self;

    Sends =?= [{Ask, Tell}] :
      NextAsk = Ask,
      NextTell = Tell,
      Writes = (Asks : Tells).

  make_choice_name(Prefix, Suffix, Name) :-
    string_to_dlist(Prefix, PL, PS),
    string_to_dlist(Suffix, SL, []) :
      PS = SL |
	list_to_string(PL, Name).

  make_choice_atom(InnerLHS, Name, ChoiceVars, ChoiceAtom) :-
	utils#tuple_to_dlist(InnerLHS, [_|Channels], ChoiceVars),
	utils#list_to_tuple([Name|Channels], ChoiceAtom).

  make_right_hand_side(RHSS, FinalMode, Index, ClauseList, Sends, NextSends) :-

    RHSS ? RHS,
    RHS = {Mode, RHSList},
    Index++ |
	make_clauselist(Mode, FinalMode, Index, RHSList,
			ClauseList, ClauseList'?, Sends, Sends'?),
	self;

    RHSS ? true,
    Index++ |
	self;

    RHSS =?= [] :
      FinalMode = _,
      Index = _,
      ClauseList = [],
      Sends = NextSends.


  make_clauselist(Mode, FinalMode, Index, RHSList, ClauseList, NextClauseList,
			Sends, NextSends) :-

    Mode =?= send, FinalMode =?= mixed,
    RHSList ? (Send | Body):
      Sends ! Send,
      ClauseList ! (`pifcp(chosen) = Index : `pifcp(sendid) = `"_" | Body) |
	self;

    Mode = send, FinalMode =\= mixed,
    RHSList ? (Send | Body):
      Sends ! Send,
      ClauseList ! (`pifcp(chosen) = Index | Body) |
	self;

    Mode =?= none,
    RHSList ? _ |
	self;

    Mode =\= send, Mode =\= none,
    RHSList ? Other :
      ClauseList ! Other |
	self;

    RHSList =?= [] :
      Mode = _,
      FinalMode = _,
      Index = _,
      ClauseList = NextClauseList,
      Sends = NextSends.


guarded_clause(RHS1, Control, Clauses, Nested, NextNested,
			Scope, NextScope) :-

    /* Recognize compound_guard */
    RHS1 =?= (Guard | Guarded), Guarded =?= (_ | _) :
      RHS1' = (Guard | [Guarded]) |
	self;

    RHS1 =?= (Guard | Guarded), Guarded =?= (_ ; _) :
      RHS1' = (Guard | [Guarded]) |
	self;

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

    Guard =\= (_ =?= _), Guard =\= (_ =\= _) :
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
