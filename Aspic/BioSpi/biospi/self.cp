/*
Transformer for Ambient Stochastic Pi Calculus procedures.

Bill Silverman, June 2000.

Last update by		$Author: bill $
		       	$Date: 2003/02/19 07:41:31 $
Currently locked by 	$Locker:  $
			$Revision: 1.7 $
			$Source: /home/qiana/Repository/Aspic/BioSpi/biospi/self.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/
-language([evaluate, compound, colon]).
-export(transform/5).
-mode(trust).

-include(spi_constants).
-include(bio_constants).

/*
** Transform/5
**
** Transform spifcp module to compound Fcp.
**
** Input:
**
**   Attributes1 - Source attributes.
**   Source      - SpiFcp code, minus attributes.
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
	filter_attributes(Attributes1, Attributes1', Exported, BlockPrefix),
	Attributes2 = [export(Exports?), entries(Entries?) | Attributes1'?],
	tospifcp#translate(Source, Source', Errors, Errors'?),
	program.

  filter_attributes(In, Out, Exported, BlockPrefix) :-

    In ? export(Es), string(Es), Es =\= all :
      Exported = [Es] |
	self;

    In ? export(all) :
      Exported = all |
	self;

    In ? export(Es), list(Es) :
      Exported = Es |
	self;

    In ? block_prefix(String),
    string(String) :
      BlockPrefix = String |
	self;

    In ? Other,
    otherwise :
      Out ! Other |
	self;

    In =?= [] :
      Out = [] |
	unify_without_failure(Exported, []),
	unify_without_failure(BlockPrefix, "").


program(Source, Exported, Exports, Entries, BlockPrefix, Terms, Errors) :-
	filter_spifcp_attributes(Source, Exported, Controls, Source',
					Errors, Errors'?),
	servers#serve_empty_scope(Scope?, Controls?, Exports, Ambients,
				  NextTerms, Optimize, Errors'),
	process_definitions+(Processes = [], NextScope = []),
	optimize#initialize(Optimize?, Exports?, Accessible),
	optimize#procedures(Terms''?, Accessible, [], Terms'),
	spc#stochasticize(BlockPrefix?, Terms'?, Terms),
	ambient_entries(Terms, Ambients, Entries).

  ambient_entries(Terms, Ambients, Entries) :-

    Ambients =\= [],
    Terms ? Procedure, Procedure =?= (Atom :- _RHSS) |
	ambient_entry(Atom, Ambients, Ambients', Entries, Entries'),
	self;

    Ambients =?= [] :
      Terms = _,
      Entries = [];

    Terms =?= [] :
      Entries = Ambients;

    Terms ? _Atom,
    otherwise |	
	self.

  ambient_entry(Atom, Ambients, NewAmbients, Entries, NewEntries) :-

    Ambients ? Name,
    arg(1, Atom, Name),
    Arity := arity(Atom) - 1 :
      Entries = [Name/Arity | NewEntries],
      NewAmbients = Ambients';

    Ambients ? Name,
    otherwise :
      NewAmbients ! Name |
	self;

    Ambients =?= [] :
      Atom = _,
      NewAmbients = [],
      NewEntries = Entries.
      

/* Extract Exports and Public channel declarations, base rate and weighter. */
filter_spifcp_attributes(Source, Exported, Controls, NextSource,
				Errors, NextErrors) +
		(PublicDescriptors = [], Defaults = {_Weighter, _Rate},
		 SpiExports = AddExports?, AddExports) :-

    Source ? String, string(String) |
	spifcp_attribute(String, PublicDescriptors, PublicDescriptors',
		Defaults, Defaults', AddExports, AddExports', Errors, Errors'),
	self;

    Source ? Tuple, Tuple =\= (_ :- _) |
	spifcp_attribute(Tuple, PublicDescriptors, PublicDescriptors',
		Defaults, Defaults', AddExports, AddExports', Errors, Errors'),
	self;

    otherwise :
      AddExports = [],
      NextSource = Source,
      Errors = NextErrors |
	choose_exports(Exported, SpiExports, Exported'),
	complete_spifcp_attributes.

  spifcp_attribute(Attribute, OldDescriptors, NewDescriptors,
	Defaults, NewDefaults, Exports, NextExports, Errors, Errors') :-

    /* obsolescent - allow for global(list) */
    Attribute = global(Gs) :
      Exports = NextExports,
      NewDefaults = Defaults |
	validate_publics(Gs, Defaults, OldDescriptors, NewDescriptors,
				Errors, Errors');

    tuple(Attribute), arity(Attribute) > 2,
    arg(1, Attribute, global) :
      Exports = NextExports,
      NewDefaults = Defaults |
	utils#tuple_to_dlist(Attribute, [_ | Gs], []),
	validate_publics(Gs?, Defaults, OldDescriptors, NewDescriptors,
				Errors, Errors');

    tuple(Attribute), arity(Attribute) > 1,
    arg(1, Attribute, public) :
      Exports = NextExports,
      NewDefaults = Defaults |
	utils#tuple_to_dlist(Attribute, [_ | Gs], []),
	validate_publics(Gs?, Defaults, OldDescriptors, NewDescriptors,
				Errors, Errors');

    tuple(Attribute), arity(Attribute) >= 2,
    arg(1, Attribute, export) :
      NewDescriptors = OldDescriptors,
      NewDefaults = Defaults |
	utils#tuple_to_dlist(Attribute, [_ | Es], []),
	validate_exports(Es?, Exports, NextExports, Errors, Errors');
  
    Attribute = baserate(Rate) :
      NewDescriptors = OldDescriptors,
      Exports = NextExports |
	validate_default_base_rate(Rate, Defaults, NewDefaults,
					Errors, Errors');

    Attribute = weighter(Weighter) :
      NewDescriptors = OldDescriptors,
      Exports = NextExports |
	validate_default_weighter(Weighter, Defaults, NewDefaults,
					Errors, Errors');

    /* skip fcp attributes - testing */
    Attribute = -_ :
      Errors' = Errors,
      Exports = NextExports,
      NewDescriptors = OldDescriptors,
      NewDefaults = Defaults;

    otherwise :
      NewDescriptors = OldDescriptors,
      NewDefaults = Defaults,
      Exports = NextExports,
      Errors ! invalid_spifcp_attribute(Attribute).

  choose_exports(FcpExports, SpiExports, Exports) :-

    FcpExports =?= [], SpiExports =?= [] :
      Exports = all;

    FcpExports =?= all :
      SpiExports = _,
      Exports = all;

    otherwise |
	utilities#concatenate_lists([FcpExports, SpiExports], Exports).


complete_spifcp_attributes(Exported, Defaults, PublicDescriptors, Controls) :-

    Defaults =?= {DefaultWeighter, DefaultRate} :
      Controls = {Exported?, PublicDescriptors, PublicNames?, Defaults} |
	extract_public_names,
	unify_without_failure(DefaultWeighter, SPI_DEFAULT_WEIGHT_NAME),
	unify_without_failure(DefaultRate, infinite).

  extract_public_names(PublicDescriptors, PublicNames) :-

    PublicDescriptors ? Public,
    arg(1, Public, Name) :
      PublicNames ! Name |
	self;

    PublicDescriptors =?= [] :
      PublicNames = [].


validate_exports(New, Exports, NextExports, Errors, NextErrors) :-

    New ? `String,
    nth_char(1, String, C), ascii('A') =< C, C =< ascii('Z') :
      Exports ! String |
	self;

    New ? Other,
    otherwise :
      Errors ! invalid_export(Other) |
	self;

    New =?= [] :
      Exports = NextExports,
      Errors = NextErrors.


validate_default_base_rate(Rate, Defaults, NewDefaults, Errors, NextErrors) :-

    Defaults = {_DefaultWeighter, DefaultRate},
    we(DefaultRate),
    number(Rate), 0 =< Rate :
      DefaultRate = Rate,
      NewDefaults = Defaults,
      Errors = NextErrors;

    Defaults = {_DefaultWeighter, DefaultRate},
    we(DefaultRate),
    string(Rate),
    convert_to_integer(Rate, IntegerRate), 0 =< IntegerRate :
      DefaultRate = IntegerRate,
      NewDefaults = Defaults,
      Errors = NextErrors;

    Defaults = {_DefaultWeighter, DefaultRate},
    we(DefaultRate),
    string(Rate), otherwise,
    convert_to_real(Rate, RealRate), 0 =< RealRate :
      DefaultRate = RealRate,
      NewDefaults = Defaults,
      Errors = NextErrors;

    Defaults = {_DefaultWeighter, DefaultRate},
    we(DefaultRate),
    Rate =?= infinite :
      DefaultRate = Rate,
      NewDefaults = Defaults,
      Errors = NextErrors;

    otherwise :
      NewDefaults = Defaults,
      Errors = [invalid_default_base_rate(Rate) | NextErrors].

validate_default_weighter(Weighter, Defaults, NewDefaults,
				Errors, NextErrors) :-

    Defaults = {DefaultWeighter, _DefaultRate},
    we(DefaultWeighter),
    string(Weighter), nth_char(1, Weighter, C),
    ascii('a') =< C, C =< ascii('z') :
      DefaultWeighter = Weighter,
      NewDefaults = Defaults,
      Errors = NextErrors;

    Defaults = {DefaultWeighter, _DefaultRate},
    we(DefaultWeighter),
    tuple(Weighter), arg(1, Weighter, Name),
    string(Name), nth_char(1, Name, C),
    ascii('a') =< C, C =< ascii('z') :
      DefaultWeighter = NewWeighter?,
      NewDefaults = Defaults |
	utils#tuple_to_dlist(Weighter, [_Name | Args], []),
	validate_public_weighter_params,
	utils#list_to_tuple([Name, BIO_NULL | Params], NewWeighter);

    otherwise :
      NewDefaults = Defaults,
      Errors = [invalid_default_weighter(Weighter) | NextErrors].


validate_publics(PublicDescriptors, Defaults, Old, New,
			Errors, NextErrors) +
			(Head = Tail?, Tail) :-

    PublicDescriptors ? Public, string(Public),
    nth_char(1, Public, C), ascii(a) =< C, C =< ascii(z),
    Defaults = {SPI_DEFAULT_WEIGHT_NAME, Rate} :
      Tail ! Public(Rate) |
	self;

    PublicDescriptors ? Public, string(Public),
    nth_char(1, Public, C), ascii(a) =< C, C =< ascii(z),
    Defaults = {Weighter, Rate},
    Weighter =\= SPI_DEFAULT_WEIGHT_NAME :
      Tail ! Public(Rate, Weighter) |
	self;

    PublicDescriptors ? Public(Rate), string(Public),
    nth_char(1, Public, C), ascii(a) =< C, C =< ascii(z),
    Defaults = {SPI_DEFAULT_WEIGHT_NAME, _Rate} :
      Tail ! Public(Rate'?) |
	validate_public_channel_rate(Rate, Defaults, Rate', Errors, Errors'),
	self;

    PublicDescriptors ? Public(Rate), string(Public),
    nth_char(1, Public, C), ascii(a) =< C, C =< ascii(z),
    Defaults = {Weighter, _Rate},
    Weighter =\= SPI_DEFAULT_WEIGHT_NAME :
      Tail ! Public(Rate'?, Weighter) |
	validate_public_channel_rate(Rate, Defaults, Rate', Errors, Errors'),
	self;

    PublicDescriptors ? Public(Rate, Weighter), string(Public),
    nth_char(1, Public, C), ascii(a) =< C, C =< ascii(z) :
      Tail ! Public(Rate'?, Weighter'?) |
	validate_public_channel_rate(Rate, Defaults, Rate', Errors, Errors'),
	validate_public_channel_weighter(Weighter, Defaults, Weighter',
						Errors', Errors''),
	self;

    PublicDescriptors ? Other, otherwise :
      Errors ! invalid_public_channel_descriptor(Other) |
	self;

    PublicDescriptors =\= [], PublicDescriptors =\= [_|_] :
      PublicDescriptors' = [PublicDescriptors] |
	self;

    PublicDescriptors =?= [] :
      Defaults = _,
      Tail = [],
      Diagnostic = duplicate_public_channel |
	utilities#sort_out_duplicates([Head], Head', Reply),
	diagnose_duplicates(Reply, Diagnostic, Errors, Errors'?),
	utilities#sort_out_duplicates([Old, Head'?], New, Reply'),
	diagnose_duplicates + (Diagnostic = duplicate_public_channel).

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


validate_public_channel_rate(Rate, Defaults, Rate', Errors, NextErrors) :-

    number(Rate), 0 =< Rate :
      Defaults = _,
      Rate' = Rate,
      Errors = NextErrors;

    string(Rate),
    convert_to_integer(Rate, IntegerRate), 0 =< IntegerRate :
      Defaults = _,
      Rate' = IntegerRate,
      Errors = NextErrors;

    string(Rate), otherwise,
    convert_to_real(Rate, RealRate), 0 =< RealRate :
      Defaults = _,
      Rate' = RealRate,
      Errors = NextErrors;

    Rate =?= infinite :
      Defaults = _,
      Rate' = Rate,
      Errors = NextErrors;

    otherwise,
    Defaults  = _Weighter(DefaultRate) :
      Rate' = DefaultRate,
      Errors = [invalid_public_channel_rate(Rate) | NextErrors].


validate_public_channel_weighter(Weighter, Defaults, NewWeighter,
					Errors, NextErrors) :-

    string(Weighter), nth_char(1, Weighter, C),
    ascii('a') =< C, C =< ascii('z') :
      Defaults = _,
      NewWeighter = Weighter,
      Errors = NextErrors;

    tuple(Weighter), arg(1, Weighter, Name),
    nth_char(1, Name, C), ascii(a) =< C, C =< ascii(z) :
      Defaults = _ |
	utils#tuple_to_dlist(Weighter, [_Name | Args], []),
	validate_public_weighter_params,
	utils#list_to_tuple([Name, BIO_NULL | Params], NewWeighter);

    otherwise,
    Defaults = DefaultWeighter(_DefaultRate) :
      NewWeighter = DefaultWeighter,
      Errors = [invalid_public_channel_weighter(Weighter) | NextErrors].


validate_public_weighter_params(Args, Params, Errors, NextErrors) :-

    Args ? Arg, number(Arg) :
      Params ! Arg |
	self;

    Args ? Arg, string(Arg),
    convert_to_integer(Arg, Arg') :
      Params ! Arg' |
	self;

    Args ? Arg, string(Arg), otherwise, 
    convert_to_real(Arg, Arg') :
      Params ! Arg' |
	self;

    Args ? Arg, otherwise :
      Errors ! invalid_default_public_weighter_parameter(Arg) |
	self;

    Args = [] :
      Params = [],
      Errors = NextErrors.

/************************* Program Transformations ***************************/

process_definitions(Source, Processes, Terms, NextTerms, Scope, NextScope) :-

    Source ? (SpiLHS :- RHSS) :
      Scope ! process(SpiLHS, LHSS, NewChannelList, ProcessScope) |
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
      Scope ! logix_variables([SCHEDULER_DOT]),
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
		(AskList = Asks?, Asks, MakeList = Make?, Make) :-

    NewChannelList ? Descriptor |
	make_and_name_channel(Name, Descriptor, Asks, Asks'?, Make, Make'),
	self;

    NewChannelList = [] :
      Name = _,
      Asks = [],
      Make = [],
      Initializer = (Ask? : Tell? | Name) |
	utilities#make_predicate_list(',', AskList?, Ask),
	utilities#make_predicate_list(',', MakeList?, Tell).

  make_and_name_channel(Name, Descriptor, Asks, NextAsks, Make, NextMake) :-

    Descriptor = ChannelName(BaseRate),
    string_to_dlist(ChannelName, Suffix, []),
    string_to_dlist(Name, PH, PS) :
      Make = [write_channel(
		new_channel(ChannelId, `ChannelName, BaseRate),
				BIO_SCHEDULER) |
	      NextMake],
      PS = Suffix |
	list_to_string(PH, ChannelId),
	parameters_to_asks([BaseRate], [number], Asks, NextAsks);

    Descriptor = ChannelName(BaseRate, ComputeWeight),
    string(ComputeWeight),
    string_to_dlist(ChannelName, Suffix, []),
    string_to_dlist(Name, PH, PS) :
      Make = [write_channel(
		new_channel(ChannelId, `ChannelName, ComputeWeight, BaseRate),
				BIO_SCHEDULER) |
	      NextMake],
      PS = Suffix |
	list_to_string(PH, ChannelId),
	parameters_to_asks([BaseRate], [number], Asks, NextAsks);

    Descriptor = ChannelName(BaseRate, ComputeWeight),
    tuple(ComputeWeight), ComputeWeight =?= `_,
    string_to_dlist(ChannelName, Suffix, []),
    string_to_dlist(Name, PH, PS) :
      Make = [write_channel(
		new_channel(ChannelId, `ChannelName, ComputeWeight, BaseRate),
				BIO_SCHEDULER) |
	      NextMake],
      PS = Suffix |
	list_to_string(PH, ChannelId),
	parameters_to_asks([BaseRate, ComputeWeight], [number, string],
				Asks, NextAsks);

    Descriptor = ChannelName(BaseRate, ComputeWeight),
    tuple(ComputeWeight), ComputeWeight =\= `_,
    string_to_dlist(ChannelName, Suffix, []),
    string_to_dlist(Name, PH, PS) :
      Make = [write_channel(
		new_channel(ChannelId, `ChannelName, ComputeWeight, BaseRate),
				BIO_SCHEDULER) |
	      NextMake],
      Ops = [number | Ops],
      PS = Suffix |
	list_to_string(PH, ChannelId),
	utils#tuple_to_dlist(ComputeWeight, [Functor, _ | List], []),
	parameters_to_asks([BaseRate, Functor | List], [number, string | Ops],
				Asks, NextAsks);

    string(Descriptor) |
      Name = _,
      Asks = NextAsks,
      Make = [(`Descriptor = BIO_NULL) | NextMake].

  parameters_to_asks(List, Ops, Asks, NextAsks) :-

    List ? Parameter, Parameter = `_,
    Ops ? Op :
      Asks ! Op(Parameter) |
	self;
 
    List ? Parameter, constant(Parameter),
    Ops ? _ |
	self;

    List =?= [] :
      Ops = _,
      Asks = NextAsks.

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
			Nested, Nested'?, Scope, [end_clause | Scope'?]),
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
      Scope ! logix_variables([SCHEDULER_DOT]),
      Scope' = [lhss(_OuterLHS, InnerLHS) | NextScope] |
	utilities#make_predicate_list(';', ClauseList, FcpClauses),
	prepares_to_guards(Prepares, PrepareGuards),
	utilities#make_communicator(InnerLHS, Communicator, CommunicationLHS);

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
      

  prepares_to_guards(Prepares, PrepareGuards) +
	(NextAsk, Asks = NextAsk?, NextTell, Tells = NextTell?) :-

    Prepares ? {Ask, Tell} :
      NextAsk ! Ask,
      NextTell ! Tell |
	self;

    Prepares =?= [] :
      NextAsk = [],
      NextTell = [],
      PrepareGuards = (Asks'? : Tells'?) |
	utilities#make_predicate_list(',', Asks, Asks'),
	utilities#make_predicate_list(',', Tells, Tells').


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
    RHSList ? ({{Identify, Write}, Consume} | Body) :
      ClauseList ! (BIO_CHOSEN = Index, Consume | Body),
      Prepares ! {Identify, Write} |
	self;

    Mode =?= send,
    RHSList ? ({{Identify, Write}, Unify}  | Body) :
      Prepares ! {Identify, Write},
      ClauseList ! (BIO_CHOSEN = Index : Unify | Body) |
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

    Guard =?= (merge + _) :
      Control = receive(CommunicationIndex),
      Message = [],
      Scope = [guard_receive(Guard, Message, CommunicationIndex, BodyGuard) |
		NextScope],
      Clauses = receive([LastClause]);

    Guard =?= (merge - _) :
      Control = send(CommunicationIndex),
      Message = [],
      Scope = [guard_send(Guard, Message, CommunicationIndex, BodyGuard) |
		NextScope],
      Clauses = send([LastClause]);

    Guard =?= accept(_) :
      Control = receive(CommunicationIndex),
      Message = [],
      Scope = [guard_receive(Guard, Message, CommunicationIndex, BodyGuard) |
		NextScope],
      Clauses = receive([LastClause]);

    Guard =?= enter(_) :
      Control = send(CommunicationIndex),
      Message = [],
      Scope = [guard_send(Guard, Message, CommunicationIndex, BodyGuard) |
		NextScope],
      Clauses = send([LastClause]);

    Guard =?= expel(_) :
      Control = receive(CommunicationIndex),
      Message = [],
      Scope = [guard_receive(Guard, Message, CommunicationIndex, BodyGuard) |
		NextScope],
      Clauses = receive([LastClause]);

    Guard =?= exit(_) :
      Control = send(CommunicationIndex),
      Message = [],
      Scope = [guard_send(Guard, Message, CommunicationIndex, BodyGuard) |
		NextScope],
      Clauses = send([LastClause]);

    Guard =?= otherwise :
      Control = otherwise(_SendIndex),
      Clauses = otherwise([LastClause]),
      BodyGuard = otherwise,
      Scope = NextScope;      

    Guard =\= (_ =?= _), Guard =\= (_ =\= _),
    Guard =\= (_ & _), Guard =\= otherwise,
    Guard =\= (merge + _), Guard =\= (merge - _),
    Guard =\= accept(_), Guard =\= enter(_),
    Guard =\= expel(_), Guard =\= exit(_) :
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

    Body1 =?= Name(Content),
    string(Name), Name =\= EMPTY,
    list(Content) :
      Body1' = (Body?, Content) |
	new_ambient(Name, Body, Scope, Scope'?),
	self;

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

  new_ambient(Name, Body, Scope, NextScope) :-

    string_to_dlist(SERVICE, ServiceIdL, [CHAR_MINUS | IdL?]) :
      Scope = [next_scope_id(Id), ambient(Id?) | NextScope],
      Body = {new_ambient(Name, Id, `ServiceId?)} |
	string_to_dlist(Id?, IdL, []),
	list_to_string(ServiceIdL, ServiceId).

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
	process_definitions([(SpiLHS :- Body)], Processes, Nested, NextNested,
				Scope', NextScope).

  make_new_lhs(Id, Channels, SpiLHS) :-

    Channels =?= [] :
      SpiLHS = `Id;

    Channels =\= [] :
      SpiLHS = `Id + Channels.


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
