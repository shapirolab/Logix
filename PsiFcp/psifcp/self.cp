/*
Transformer for Stochastic Psi Calculus procedures.

Bill Silverman, June 2000.

Last update by		$Author: bill $
		       	$Date: 2000/06/27 11:01:08 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/PsiFcp/psifcp/self.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export(transform/5).
-mode(failsafe).
-language(compound).

/*
** Transform/5
**
** Transform psifcp module to compound Fcp.
**
** This module should be integrated with the transform application,
** which should recognize psifcp alone and translate it to:
**
**    [evaluate,psifcp,compound,colon]
**
** Input:
**
**   Attributes1 - Source attributes.
**   Source      - PsiFcp code, minus attributes.
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
	filter_attributes(Attributes1, Attributes1', Exported, Level),
	Attributes2 = [export(Exports?) | Attributes1'?],
	topsifcp#translate(Source, Source', Errors, Errors'?),
	program.

  filter_attributes(In, Out, Exported, Level) :-

    In ? export(Es), string(Es), Es =\= all :
      Exported = [Es] |
	self;

    In ? export(all) :
      Exported = all |
	self;

    In ? export(Es), list(Es) :
      Exported = Es |
	self;

    In ? optimize :
      Out ! optimize,
      Level = 1 |
	self;

    In ? optimize(I) :
      Out ! optimize,
      Level = I |
	self;

    In ? Other,
    otherwise :
      Out ! Other |
	self;

    In =?= [] :
      Out = [] |
	unify_without_failure(Level, 2),
	unify_without_failure(Exported, all).


program(Source, Exported, Level, Exports, Terms, Errors) :-
	filter_psifcp_attributes(Source, Exported, Controls,
				_ModuleType, Source', Errors, Errors'?),
	servers#serve_empty_scope(Scope?, Controls?, Exports,
				  NextTerms, Optimize, Errors'),
	process_definitions+(Processes = [], NextScope = []),
	optimize#initialize(Optimize?, Exports?, Accessible),
	optimize_terms(Level, Terms''?, Accessible, Terms'),
	spc#stochasticize(Terms'?, Terms).

  optimize_terms(Level, In, Table, Out) :-

    integer(Level), Level > 0 |
	optimize#procedures(Level, In, Table, [], Out);

    otherwise :
      Level = _,
      Table = [],
      Out = In.


/* Extract Global channel declarations and Stochastic base rates. */
filter_psifcp_attributes(Source, Exported, Controls, ModuleType, NextSource,
			Errors, NextErrors) +
	(GlobalDescriptors = [], TypeRate = _ModuleType(_Rate)) :-

    Source ? String, string(String) |
	psifcp_attribute(String, GlobalDescriptors, GlobalDescriptors',
				TypeRate, TypeRate', Errors, Errors'),
	self;

    Source ? Tuple, Tuple =\= (_ :- _) |
	psifcp_attribute(Tuple, GlobalDescriptors, GlobalDescriptors',
				TypeRate, TypeRate', Errors, Errors'),
	self;

    otherwise :
      NextSource = Source,
      Errors = NextErrors |
	complete_psifcp_attributes.

  psifcp_attribute(Attribute, OldDescriptors, NewDescriptors,
			TypeRate1, TypeRate, Errors, Errors') :-

    Attribute = global(Gs) :
      TypeRate = TypeRate1 |
	validate_globals(Gs, TypeRate, OldDescriptors, NewDescriptors,
				Errors, Errors');
  
    Attribute = baserate(Rate) :
      NewDescriptors = OldDescriptors,
      TypeRate3 = stochastic(_Rate),
      TypeRate = TypeRate3 |
	validate_base(Rate, TypeRate1, TypeRate2, Errors, Errors'),
	update_base;

    /* skip fcp attributes - testing */
    Attribute = -_ :
      Errors' = Errors,
      NewDescriptors = OldDescriptors,
      TypeRate = TypeRate1;

    otherwise :
      NewDescriptors = OldDescriptors,
      TypeRate = TypeRate1,
      Errors ! invalid_psifcp_attribute(Attribute).

  complete_psifcp_attributes(Exported, TypeRate, ModuleType, GlobalDescriptors,
					Controls) :-

    TypeRate =?= stochastic(_Rate) :
      Controls = {Exported?, GlobalDescriptors, GlobalNames?, TypeRate?},
      ModuleType = stochastic |
	extract_global_names,
	unify_without_failure(TypeRate, _(infinite));

    true :
      TypeRate = none(infinite),
      Controls = {Exported?, GlobalDescriptors, GlobalNames?, TypeRate},
      ModuleType = none |
	extract_global_names.

  update_base(TypeRate1, TypeRate2, TypeRate3) :-

    true :
      TypeRate1 = TypeRate2,
      TypeRate3 = TypeRate2;

    otherwise :
      TypeRate1 = _,
      TypeRate3 = TypeRate2.

  extract_global_names(GlobalDescriptors, GlobalNames) :-

    GlobalDescriptors ? Name(_Rate) :
      GlobalNames ! Name |
	self;

    GlobalDescriptors =?= [] :
      GlobalNames = [].

  validate_globals(GlobalDescriptors, TypeRate, Old, New, Errors, NextErrors) +
			(Head = Tail?, Tail) :-

    GlobalDescriptors ? String,
    string(String), String =\= "", String =\= "_",
    TypeRate =?= _Type(Rate) :
      Tail ! String(Rate) |
	self;

    GlobalDescriptors ? String(Rate),
    string(String), String =\= "", String =\= "_" :
      Global = String(_Rate),
      Tail ! Global |
	validate_base(Rate, TypeRate, Global, Errors, Errors'),
	self;

    GlobalDescriptors ? Other, otherwise :
      Errors ! invalid_global_channel_name(Other) |
	self;

    GlobalDescriptors =\= [], GlobalDescriptors =\= [_|_] :
      GlobalDescriptors' = [GlobalDescriptors] |
	self;

    GlobalDescriptors =?= [] :
      TypeRate = _,
      Tail = [],
      Errors = NextErrors |
	utilities#sort_out_duplicates([Old], Old', Reply),
	utilities#sort_out_duplicates([Old'?, Head], New, _Reply),
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

validate_base(Rate, TypeRate, Global, Errors, NextErrors) :-

    number(Rate), 0 =< Rate :
      TypeRate = _,
      Global = _Name(Rate),
      Errors = NextErrors;

    Rate =?= infinite :
      TypeRate = _,
      Global = _Name(Rate),
      Errors = NextErrors;

    otherwise,
    TypeRate = _Type(Rate0) :
      Global = _Name(Rate0),
      Errors = [invalid_base_rate(Rate) | NextErrors].

/************************* Program Transformations ***************************/

process_definitions(Source, Processes, Terms, NextTerms, Scope, NextScope) :-

    Source ? (PsiLHS :- RHSS) :
      Scope ! process(PsiLHS, LHSS, NewChannelList, ProcessScope) |
	process_definitions(Processes, [], Nested, Nested'?,
				ProcessScope, ProcessScope'?),
	process(LHSS, RHSS, NewChannelList, ProcessScope', Process, Nested'),
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

process(LHSS, RHSS, NewChannelList, Scope, Process, Nested) :-

    LHSS = [] :
      RHSS = _,
      NewChannelList = _,
      Scope = [],
      Process = [],
      Nested = [];

    LHSS = {OuterLHS, InnerLHS},
    NewChannelList =\= [] :
      Nested ! outer(OuterLHS, Initializer?, []),
      /* This process needs access to the scheduler. */
      Scope ! logix_variables(["Scheduler."]),
      NewChannelList' = [] |
	arg(1, InnerLHS, Name),
	initialize_channels(Name, NewChannelList, Initializer),
	self;

    LHSS = {_OuterLHS, InnerLHS},
    NewChannelList =?= [],
    RHSS =\= (_|_), RHSS =\= (_;_)  :
      Scope ! code(no_guard, [], []),
      Process = no_guard(InnerLHS, RHSS'?, []) |
	transform_body(RHSS, RHSS', Nested, [], Scope', []);

    LHSS = {_OuterLHS, InnerLHS},
    otherwise :
      NewChannelList = _,
      Process = _Type(InnerLHS, RHSS'?, _Action) |
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
	utilities#make_predicate_list(',', MakeList?, Tell).

  make_and_name_channel(Name, Body, Descriptor, NewBody, Make, NextMake) :-

    Descriptor = ChannelName(BaseRate),
    nth_char(1, ChannelName, C), ascii(a) =< C, C =< ascii(z),
    string_to_dlist(ChannelName, Suffix, []),
    string_to_dlist(Name, PH, PS) :
      Make = [write_channel(
		new_channel(ChannelId, `ChannelName, BaseRate'?),
				`"Scheduler.") |
	      NextMake],
      PS = Suffix |
	list_to_string(PH, ChannelId),
	utilities#real_base_kluge(BaseRate, Body, BaseRate', NewBody);

    Descriptor = ChannelName(_BaseRate),
    otherwise :
      Name = _,
      NewBody = Body,
      Make = [(`ChannelName = `"_") | NextMake].


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
			(Mode = none, Index = 0,
	NextRHSS, RHSS = NextRHSS?, NextPrepares = [], FinalMode = _) :-

    RHS1 =?= (_ | _),
    Index++ :
      NextRHSS = [Clauses?],
      FinalMode = Mode'? |
	guarded_clause(RHS1, GuardMode(Index'), Clauses,
			Nested, [], Scope, Scope'?),
	utilities#update_process_mode(Mode, GuardMode, Mode'),
	make_right_hand_side + (Index = 1),
	make_rhs2 + (Scope = Scope', NextScope = Scope''?),
	code_reply;

    RHS1 =?= (Guarded ; RHS1'), Guarded =?= (_|_),
    Index++ :
      NextRHSS ! Clauses? |
	guarded_clause(Guarded, GuardMode(Index'), Clauses,
			Nested, Nested'?, Scope, Scope'?),
	utilities#update_process_mode(Mode, GuardMode, Mode'),
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

    PrepareProcedure =?= (_ :- PrepareRHS) :
      Process = FinalMode(_LHS, ProcessRHS, PrepareProcedure),
      Scope = [code(FinalMode, ProcessRHS, PrepareRHS)];

    otherwise :
      PrepareProcedure = _,
      Process = FinalMode(_LHS, _RHSS, []),
      Scope = [code(FinalMode, [], [])].

  make_rhs2(Mode, ClauseList, Prepares, RHS2,
	PrepareProcedure, Scope, NextScope) :-

    Mode =?= communicate :
      RHS2 = (PrepareGuards? | Communicator?),
      PrepareProcedure = (CommunicationLHS? :- FcpClauses?),
      /* This process needs access to the scheduler. */
      Scope ! logix_variables(["Scheduler."]),
      Scope' = [lhss(OuterLHS, InnerLHS) | NextScope] |
	arg(1, OuterLHS, PName),
	make_communication_name(PName, ".comm", Communicator),
	utilities#make_predicate_list(';', ClauseList, FcpClauses),
	prepares_to_guards(Prepares, PrepareGuards, Results),
	make_communication_atom + (ChoiceVars = [`psifcp(chosen) | Results]);

    /* compared, logix, none */
    Mode =\= communicate, Mode =\= compare, Mode =\= conflict :
      Prepares = _,
      RHS2 = FcpClauses?,
      PrepareProcedure = [],
      Scope = NextScope |
	utilities#make_predicate_list(';', ClauseList, FcpClauses);

    Mode =?= compare :
      Prepares = _,
      RHS2 = FcpClauses?,
      PrepareProcedure = [],
      Scope = [lhss(_Outer, Inner) | NextScope] |
	arg(1, Inner, Name),
	utilities#concatenate_lists(
			[ClauseList,[(otherwise | fail(Name-compare))]],
					ClauseList'),
	utilities#make_predicate_list(';', ClauseList'?, FcpClauses);

    Mode =?= conflict :
      Prepares = _,
      ClauseList = _,
      RHS2 = true,
      PrepareProcedure = [],
      Scope = [error("conflicting_guards") | NextScope].
      

  prepares_to_guards(Prepares, PrepareGuards, Results) +
	(NextAsk, Asks = NextAsk?, NextTell, Tells = NextTell?) :-

    Prepares ? {Ask, Tell} :
      NextAsk ! Ask,
      NextTell ! Tell |
	self;

    Prepares ? {Ask, Tell, Result} :
      NextAsk ! Ask,
      NextTell ! Tell,
      Results ! Result |
	self;

    Prepares =?= [] :
      NextAsk = [],
      NextTell = [],
      PrepareGuards = (Asks'? : Tells'?),
      Results = [] |
	utilities#make_predicate_list(',', Asks, Asks'),
	utilities#make_predicate_list(',', Tells, Tells').

  make_communication_name(Prefix, Suffix, Communicator) :-
    string_to_dlist(Prefix, PL, PS),
    string_to_dlist(Suffix, SL, []) :
      PS = SL |
	list_to_string(PL, Communicator).

  make_communication_atom(InnerLHS, Communicator, ChoiceVars,
				CommunicationLHS) :-

	utils#tuple_to_dlist(InnerLHS, [_ | Channels], ChoiceVars),
	utils#list_to_tuple([Communicator | Channels], CommunicationLHS).

  make_right_hand_side(RHSS, Index, ClauseList,	Prepares, NextPrepares) :-

    RHSS ? RHS,
    RHS = {Mode, RHSList},
    Index++ |
	make_clauselist(Mode, Index, RHSList,
		ClauseList, ClauseList'?, Prepares, Prepares'?),
	self;

    RHSS ? true,
    Index++ |
	self;

    RHSS =?= [] :
      Index = _,
      ClauseList = [],
      Prepares = NextPrepares.


  make_clauselist(Mode, Index, RHSList,
	ClauseList, NextClauseList, Prepares, NextPrepares) :-

    Mode =?= receive,
    RHSList ? ({{Identify, Write}, Consume} | Body),
    Consume =?= (Result = _)  :
      ClauseList ! (`psifcp(chosen) = Index, Consume | Body),
      Prepares ! {Identify, Write, Result} |
	self;

    Mode =?= send,
    RHSList ? ({{Identify, Write}, Unify}  | Body),
    Unify =?= (Result = _) :
      Prepares ! {Identify, Write, Result},
      ClauseList ! (`psifcp(chosen) = Index : Unify | Body) |
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
      Index = _,
      ClauseList = NextClauseList,
      Prepares = NextPrepares.


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
      Control = receive(CommunicationIndex),
      Scope = [guard_receive(Channel, Message, CommunicationIndex, BodyGuard) |
		NextScope],
      Clauses = receive([LastClause]);

    Guard =?= (Channel ! Message) :
      Control = send(CommunicationIndex),
      Scope = [guard_send(Channel, Message, CommunicationIndex, BodyGuard) |
		NextScope],
      Clauses = send([LastClause]);

    Guard =?= otherwise :
      Control = otherwise(_SendIndex),
      Clauses = otherwise([LastClause]),
      BodyGuard = otherwise,
      Scope = NextScope;      

    Guard =\= (_ =?= _), Guard =\= (_ =\= _),
    Guard =\= (_ & _), Guard =\= otherwise :
      Scope = [Result? | NextScope] |
	logix_guards;

    otherwise :
      Clauses = compare([LastClause]),
      Control = compare(_SendIndex) |
	compare_channels + (Channels = [], NextChannels = _).


logix_guards(Guard, Control, LastClause, Clauses, BodyGuard, Result) :-

    tuple(Guard),
    Guard =\= `_, Guard =\= ?_,
    arity(Guard, Arity) :
      Clauses = logix([LastClause]),
      Control = logix(_),
      Index = 1,
      Result = logix_variables(LogixVars?) |
	copy_ask_guards;

    otherwise:
      BodyGuard = true,
      Clauses = none([LastClause]),
      Control = none(_),
      Result = error(invalid_guard(Guard)).

  copy_ask_guards(Guard, Index, Arity, BodyGuard, LogixVars) :-

    Index++ < Arity,
    arg(Index, Guard, Predicate) :
      BodyGuard = (Predicate, BodyGuard') |
	utilities#find_logix_variables(Predicate, LogixVars, LogixVars'?),
	self;

    Index =:= Arity,
    arg(Index, Guard, Predicate) :
      BodyGuard = Predicate |
	utilities#find_logix_variables(Predicate, LogixVars, []).


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
	utilities#make_predicate_list(',', Goals?, Body2).

  transform_body1(Body1, Goals, NextGoals, Nested, NextNested,
			Scope, NextScope) :-

    Body1 = (Body2, Body1') |
	transform_body1(Body2, Goals, Goals'?, Nested, Nested'?,
			Scope, Scope'?),
	self;
/*
    Body1 = (Channel ? Message) :
      Scope ! body_receive(Channel, Message, Goal),
      Goals = [Goal? | NextGoals],
      Nested = NextNested,
      Scope' = NextScope;

    Body1 = (Channel ! Message) :
      Scope ! body_send(Message, Channel, Goal),
      Goals = [Goal? | NextGoals],
      Nested = NextNested,
      Scope' = NextScope;
*/
    Body1 = (_Channel ? _Message) :
      Scope ! error(receive_in_body(Body1)),
      Goals = NextGoals,
      Nested = NextNested,
      Scope' = NextScope;

    Body1 = (_Channel ! _Message) :
      Scope ! error(send_in_body(Body1)),
      Goals = NextGoals,
      Nested = NextNested,
      Scope' = NextScope;

    list(Body1) :
      Goals = [Body2 | NextGoals] |
	new_scope;

    Body1 =?= Name # Call1 :
      Goals = [(Name # Call2) | NextGoals],
      Scope = [Result? | NextScope],
      Nested = NextNested |
	parse_remote_call(Call1, Call2, Result);

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
	process_definitions([(PsiLHS :- Body)], Processes, Nested, NextNested,
				Scope', NextScope).

  make_new_lhs(Id, Channels, PsiLHS) :-

    Channels =?= [] :
      PsiLHS = `Id;

    Channels =\= [] :
      PsiLHS = `Id + Channels.


parse_remote_call(Call1, Call2, Result) :-

    Call1 =?= Name # Call1', string(Name) :
      Call2 = Name # Call2' |
	self;

    Call1 =\= _ # _, Call1 =\= `_,
    tuple(Call1), arg(1, Call1, Name), string(Name) :
      Call2 = Call1,
      Result = logix_variables(Call2);

    Call1 = `Name, string(Name) :
      Call2 = Name,
      Result = logix_variables([]);

    Call1 =\= _ # _,
    tuple(Call1), arg(1, Call1, `_) :
      Result = remote_call(Call1, Call2);

    otherwise :
      Call2 = Call1,
      Result = logix_variables(Call2).
