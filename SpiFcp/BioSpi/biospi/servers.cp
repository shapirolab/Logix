/*
Precompiler for Stochastic Pi Calculus procedures - servers.

Bill Silverman, December 1999.

Last update by		$Author: bill $
		       	$Date: 2002/09/16 16:28:54 $
Currently locked by 	$Locker:  $
			$Revision: 1.3 $
			$Source: /home/qiana/Repository/SpiFcp/BioSpi/biospi/servers.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language([evaluate, compound, colon]).
-export([serve_empty_scope/7]).

-include(spi_constants).
-include(bio_constants).

/*
** serve_empty_scope/7+7
**
** Serve requests from first-level processes.  See serve_process_scope/8+6
**
** Input:
**
**   In is the request input stream:
**
**      call_sum(Name, Sum, Call)
**
**      error(Diagnostic) - copy Diagnostic to Errors stream.
**
**      ambient(Name) - add to named Ambients.
**
**      lookup_functor(Functor, CallType, CallDefinition)
**
**          Lookup process Functor and return:
**               CallType in {none, outer, inner}
**               CallDefinition = {Name, Arity, ChannelNames,
**                                 {OuterLHS, InnerLHS},
**				   Mode(SendRHS, ProcessRHS)}
**
**      procedure(Notes, Arity, LHSS)
**
**      process(PiLHS, Status, ProcessScope)
**
**          Analyze PiLHS (Left-hand-side) of process declaration and return
**               Status is one of  {single, double, nil} ;
**               ProcessScope - a stream to a process context handler.
**
**   Controls = {Exported, PublicDescriptors, PublicNames, WeightRate}
**
**          Exported is "all" or a list of exported  Name  or  Name/Arity.
**
**          PublicDescriptors is a list of public channels, Name(BaseRate)
**                            or Name(BaseRate, Weighter).
**
**          WeightRate = Weighter(BaseRate) rate for new channels,
**
**          where Weighter is in {none, stochastic}.
**
** Output:
**
**   Exports is a list of exported procedure identifiers.
**
**   Ambients is a list of generated ambient procedure names.
**
**   Generated is a list of processes for first level procedure entry.
**
**   Optimize is a list of procedure object descriptors.
**
**   Errors is a list of error diagnostics.
**
** Local:
**
**   Progeny is a list of child ProcessDefinitions.
**
**   Refs, AddRef is a list of deferred lookup_functor/3 requests.
**
**   Sum, Summed is a list of called summed processes.
*/

serve_empty_scope(In, Controls, Exports, Ambients, Generated, Optimize, Errors)
		+ (Progeny = [], Refs = AddRef?, AddRef,
		   Sums, Summed = Sums?) :-

    In ? error(Error) :
      Errors ! Error |
	self;

    In ? ambient(Ambient),
    string(Ambient) :
      Ambients ! Ambient |
	self;

    In ? lookup_functor(Functor, CallType, CallDefinition) :
      AddRef ! lookup_functor(Functor, CallType, CallDefinition) |
	self;

    In ? process(PiLHS, LHSS, NewChannelList, ProcessScope),
    Controls = {Exported, PublicDescriptors, PublicNames, WeightRate} |
	make_process_scope(PiLHS, WeightRate, ProcessScope, [],
				NewChannelList, In'', In'?,
		NewDefinition?, ProcessDefinition, Errors, Errors'?),
	export_process(ProcessDefinition?, Exported, Export,
				Exports, Exports'?),
	make_prefix_call(Export, Prefix),
	create_entry(PublicDescriptors, PublicNames, Prefix?,
			ProcessDefinition?, NewDefinition,
			Generated, Generated'?, Optimize, Optimize'?),
	add_process_definition(NewDefinition?, LHSS, Progeny, Progeny'),
	self;

    In ? call_sum(Name, Sum, Call) :
      Sums ! Name(Sum, Call) |
	self;

    In ? procedure(Notes, Arity, LHSS) :
      Optimize ! procedure(Notes, Arity, LHSS, _Value) |
	self;

    In = [] :
      Ambients = [],
      Controls = _,
      AddRef = [],
      Exports = [],
      Sums = [] |
	find_process_refs(Refs, Progeny, [], []),
	/* sum_procedures. */
	call#sum_procedures(Summed, Generated, Optimize, [], Errors).

  make_prefix_call(Export, Prefix) :-

    Export =?= true |
      Prefix = public_channels(_PublicPairList, BIO_SCHEDULER);

    otherwise :
      Export = _,
      Prefix = [].


create_entry(PublicDescriptors, PublicNames, Prefix,
		ProcessDefinition, NewDefinition,
		Generated, NextGenerated, Optimize, NextOptimize) :-

    ProcessDefinition =?= {Name, Arity, ChannelNames, LHSS, CodeTuple},
    Name =\= NULL,
    LHSS = {OuterLHS, _InnerLHS},
    tuple(Prefix),
    Index := arity(OuterLHS),
    Index++,
    string_to_dlist(Name, NL, [CHAR_DOT, CHAR_ZERO]),
    list_to_string(NL, Name') :
      Generated ! export(OuterLHS, Initializer?, []),
      NextGenerated = Generated',
      Optimize ! procedure([call(Name'?)], Arity, OuterLHS, _Value),
      NextOptimize = Optimize',
      NewDefinition = {Name, Arity, ChannelNames'?, NewLHS?, CodeTuple},
      NewLHS = {OuterLHS'?, InnerLHS'?} |
	split_channels(1, Index, ChannelNames, ParamList, ChannelList),
	make_lhs_tuples,
	initialize_public_channels(Index', OuterLHS'?, PublicDescriptors,
					Initializer, Prefix);

    ProcessDefinition =?= {Name, Arity, ChannelNames, LHSS, CodeTuple},
    LHSS = {OuterLHS, _InnerLHS},
    Name =\= NULL,
    Prefix = [],
    PublicNames =\= [],
    Index := arity(OuterLHS) :
      PublicDescriptors = _,
      NextGenerated = Generated,
      NextOptimize = Optimize,
      NewDefinition = {Name, Arity, ChannelNames'?, NewLHS?, CodeTuple},
      NewLHS = {OuterLHS'?, InnerLHS'?} |
	split_channels(1, Index, ChannelNames, ParamList, ChannelList),
	make_lhs_tuples;
	
    otherwise :
      PublicDescriptors = _,
      PublicNames = _,
      Prefix = _,
      NewDefinition = ProcessDefinition,
      Generated = NextGenerated,
      NextOptimize = Optimize.

  split_channels(I, Index, ChannelNames, ParamList, ChannelNameList) :-

    ChannelNames ? ChannelName,
    I < Index,
    I++ :
      ParamList ! ChannelName |
	self;

    ChannelNames ? ChannelName,
    I >= Index,
    I++ :
      ChannelNameList ! ChannelName |
	self;

    ChannelNames =?= [] :
      I = _,
      Index = _,
      ParamList = [],
      ChannelNameList = [].


  initialize_public_channels(Index, Tuple, PublicDescriptors,
				Initializer, Prefix) + (List = Tail?, Tail) :-

    Index =< arity(Tuple),
    arg(Index, Tuple, `ChannelName),
    PublicDescriptors ? DuplicateName(_BaseRate),
    DuplicateName =\= ChannelName |
	self;

    Index =< arity(Tuple),
    arg(Index, Tuple, `ChannelName),
    PublicDescriptors ? DuplicateName(_BaseRate, _Weighter),
    DuplicateName =\= ChannelName |
	self;

    Index =< arity(Tuple),
    arg(Index, Tuple, `ChannelName),
    PublicDescriptors ? ChannelName(BaseRate),
    Index++ :
      Tail ! ChannelName(`ChannelName, BaseRate) |
	self;

    Index =< arity(Tuple),
    arg(Index, Tuple, `ChannelName),
    PublicDescriptors ? ChannelName(BaseRate, Weighter),
    Index++ :
      Tail ! ChannelName(`ChannelName, Weighter, BaseRate) |
	self;

    otherwise,
    arg(1, Tuple, ProcedureName),
    arg(2, Prefix, PublicPairList) :
      Index = _,
      PublicDescriptors = _,
      Tail = [],
      Initializer = (computation # Prefix, ProcedureName) |
	unify_without_failure(PublicPairList, List).


/*
** serve_process_scope/8+6
**
** Input:
**
**   In is the request input stream:
**
**      call(Body1, Body2)
**
**      end_clause
**
**      error(Diagnostic) - add  Name-Diagnostic  to Errors stream.
**
**      guard_compare(Guard, ChannelList, NextChannelList, Comparer)
**
**      guard_receive(Channel, Message, Index, Guards)
**
**      guard_send(Channel, Message, Index, Guards)
**
**      lhss(Outer, Inner) - return ProcessDefinition's
**                           OuterLHS and InnerLHS.
**
**      logix_variables(Variables)
**
**      lookup_functor(Functor, CallType, CallDefinition)
**
**          Lookup process Functor and return:
**               CallType in {none, outer, inner}
**               CallDefinition = {Name, Arity, ChannelNames,
**                                 {OuterLHS, InnerLHS},
**                                 Mode(SendRHS, ProcessRHS)}
**
**      new_scope_id(Id)
**
**      process(PiLHS, Status, ProcessScope)
**
**          Analyze PiLHS (Left-hand-side) of process declaration and return
**               Status is one of  {single, double, nil} ;
**               ProcessScope - a stream to a process context handler.
**
**      remote_call(Call1, Call2)
**
**   NextOut, NextErrors are continuation streams.
**
** Passed Input: (passed up to serve_empty_scope)
**
**      ambient(Export) :
**
**      call_sum(_Name, _Sum, _Call)
**
**      procedure(_Notes, _Arity, _LHSS) 
**
** Output:
**
**   Out is a list of lookup_functor/3 requests which cannot be satisfied
**   at this level.
**
**   Notes is a list of optimisation information - see optimize.cp .
**
**   Errors is defined in serve_empty_scope.
**
** Local: see serve_empty_scope/6+5 above.
*/

serve_process_scope(In, ProcessDefinition, WeightRate, Notes,
			Out, NextOut, Errors, NextErrors) +
		(IdIndex = 1, Primes = [], Privates = [], Progeny = [],
		 Refs = AddRef?, AddRef) :-

    In ? lhss(Outer, Inner),
    ProcessDefinition =?= {_Name, _Arity, _ChannelNames, LHSS, _CodeTuple},
    LHSS = {OuterLHS, InnerLHS} :
      Outer = OuterLHS,
      Inner = InnerLHS |
	self;

    In ? call(Body1, Body2) |
	call#make_local_call(ProcessDefinition, Privates, Primes, Body1, Body2,
				In'', In'?, Errors, Errors'?, CallDefinition),
	add_call(CallDefinition, Body2, Notes, Notes'),
	self;

    In ? error(Description),
    arg(1, ProcessDefinition, Name) :
      Errors ! (Name - Description) |
	self;

    In ? end_clause :
      Privates = _,
      Primes = _ |
	self + (Privates = [], Primes = []);

    In ? guard_compare(Guard, ChannelList, NextChannelList, Comparer),
    Guard =?= {Operator, C1, C2},
    ProcessDefinition =?= {Name, _Arity, ChannelNames, _LHSS, _CodeTuple} :
      Notes ! variables(List?) |
	message_to_channels({C1, C2}, Name, ChannelNames, Privates, false, List,
				Errors, Errors'?),
	compare_channels_ok,
	self;

    In ? guard_receive(Channel, Message, Index, Guards),
    ProcessDefinition =?= {Name, _Arity, ChannelNames, _LHSS, _CodeTuple} :
      Primes = _,
      Guards = {Guard?, BIO_MESSAGE = ChannelList?},
      Notes ! variables(ChannelName?) |
	parse_message(Message, Message', Multiplier, Name, Errors, Errors'?),
	verify_multiplier(Name, Multiplier?, Multiplier', ChannelNames,
				Errors', Errors''?),
	utilities#verify_communication_channel(Name, Channel,
						ChannelNames, Privates,
						Locus, ChannelName,
						Errors'', Errors'''?),
	parse_message(Name, ChannelNames, Message'?, MsChannelNames,
			Privates', Primes', Errors''', Errors''''?),
	call#prime_local_channels(Primes'?, MsChannelNames?, MsChannelNames'),
	utilities#names_to_channel_list(MsChannelNames'?, ChannelList),
	make_communication_guard + (Type = receive),
	self;

    In ? guard_send(Channel, Message, Index, Guards),
    ProcessDefinition =?= {Name, _Arity, ChannelNames, _LHSS, _CodeTuple} :
      Guards = {Guard?, BIO_MESSAGE = ChannelList},
      Notes ! variables([ChannelName'? | MsChannelNames'?]) |
	parse_message(Message, Message', Multiplier, Name, Errors, Errors'?),
	verify_multiplier(Name, Multiplier?, Multiplier', ChannelNames,
				Errors', Errors''?),
	utilities#verify_communication_channel(Name, Channel,
						ChannelNames, Privates,
						Locus, ChannelName,
						Errors'', Errors'''?),
	message_to_channels(Message'?, Name, ChannelNames, Privates, false,
				MsChannelNames, Errors''', Errors''''?),
	call#prime_local_channels(Primes, [ChannelName? | MsChannelNames?],
					  [ChannelName' | MsChannelNames']),
	utilities#names_to_channel_list(MsChannelNames'?, ChannelList),
	make_communication_guard + (Type = send),
	self;

    In ? logix_variables(Variables) :
      Notes ! variables(Variables) |
	self;

    In ? lookup_functor(Functor, CallType, CallDefinition),
    arg(1, ProcessDefinition, Functor) :
      CallType = inner,
      CallDefinition = ProcessDefinition |
	self;

    In ? lookup_functor(Functor, CallType, CallDefinition),
    arg(1, ProcessDefinition, Name), Name =\= Functor :
      AddRef ! lookup_functor(Functor, CallType, CallDefinition) |
	self;

    In ? new_scope_id(Id),
    convert_to_string(IdIndex, SId),
    string_to_dlist(SId, Suffix, []),
    arg(1, ProcessDefinition, Name),
    string_to_dlist(Name, DL, [CHAR_DOT | Suffix]),
    IdIndex++ :
      Notes ! call(Id) | /* Can't hurt! */
	list_to_string(DL, Id),
	self;

    In ? next_scope_id(Id),
    convert_to_string(IdIndex, SId),
    string_to_dlist(SId, Suffix, []),
    arg(1, ProcessDefinition, Name),
    string_to_dlist(Name, DL, [CHAR_DOT | Suffix]) |
	list_to_string(DL, Id),
	self;

    In ? process(PiLHS, PLHSS, NewChannelList, ProcessScope),
    ProcessDefinition =?= {_Name, _Arity, ChannelNames, _LHSS, _CodeTuple} |
	utilities#concatenate_lists([Privates, ChannelNames], PublicNames),
	make_process_scope(PiLHS, WeightRate, ProcessScope, PublicNames,
				NewChannelList,
		In'', In'?, NewDefinition?, NewDefinition, Errors, Errors'?),
	add_process_definition(NewDefinition?, PLHSS, Progeny, Progeny'),
	self;

    In ? remote_call(Call1, Call2),
    ProcessDefinition =?= {Name, _Arity, ChannelNames, _LHSS, _CodeTuple} :
      Notes ! variables(LogixVars?) |
	call#make_remote_call(Name, ChannelNames, Privates, Primes, Call1, Call2,
				Errors, Errors'?),
	utilities#find_logix_variables(Call2?, LogixVars, []),
	self;

    /* Code for communication procedures */
    In ? code(Mode, SendRHS, ProcessRHS),
    ProcessDefinition =?= {_Name, _Arity, _ChannelNames, _LHSS, CodeTuple} :
      CodeTuple = Mode?(SendRHS?, ProcessRHS?) |
	self;

    /* Pass along */
    In ? CallSum, CallSum = call_sum(_Name, _Sum, _Call) :
      Out ! CallSum |
	self;

    /* Pass along */
    In ? Ambient, Ambient = ambient(_Ambient) :
      Out ! Ambient |
	self;

    /* Pass along */
    In ? Procedure, Procedure  = procedure(_Notes, _Arity, _LHSS) :
      Out ! Procedure |
	self;

    In = [],
    ProcessDefinition =?= {_Name, _Arity, _ChannelNames, _LHSS, CodeTuple} :
      WeightRate = _,
      IdIndex = _,
      Privates = _,
      Primes = _,
      Notes = [],
      AddRef = [],
      Errors = NextErrors |
	unify_without_failure(CodeTuple, none([], [])),
	find_process_refs(Refs, Progeny, Out, NextOut).

  verify_multiplier(Name, Multiplier, IntegerMultiplier, ChannelNames,
			Errors, Errors') :-

    integer(Multiplier), Multiplier > 0 :
      Name = _,
      ChannelNames = _,
      IntegerMultiplier = Multiplier,
      Errors = Errors';

    Multiplier =?= `_ :
      IntegerMultiplier = `OkChannelName |
	utilities#verify_channel(Name, Multiplier, ChannelNames, [],
				OkChannelName, Errors, Errors');

    otherwise :
      ChannelNames = _,
      IntegerMultiplier = 1,
      Errors ! (Name - invalid_multiplier(Multiplier)).      

parse_message(Message, Message', Multiplier, Name, Errors, NextErrors) :-

    Message =?= [] :
      Name = _,
      Message' = Message,
      Multiplier = 1,
      Errors = NextErrors;

    tuple(Message), Message =\= (_ * _) :
      Name = _,
      Message' = Message,
      Multiplier = 1,
      Errors = NextErrors;

    Message =?= M * [] :
      Name = _,
      Message' = [],
      Multiplier = M,
      Errors = NextErrors;

    Message =?= [] * M :
      Name = _,
      Message' = [],
      Multiplier = M,
      Errors = NextErrors;

    Message =?= M * Tuple :
      Name = _,
      Message' = Tuple,
      Multiplier = M,
      Errors = NextErrors;

    otherwise :
      Message' = [],
      Multiplier = 1,
      Errors = [Name - invalid_message(Message) | NextErrors].

  add_call(CallDefinition, Goal, Notes, NextNotes) :-

    CallDefinition =?= [] :
      Goal = _,
      Notes = NextNotes;

    CallDefinition =\= [] :
      Notes = [call(Goal) | NextNotes].

  compare_channels_ok(Operator, List, Comparer,
			ChannelList, NextChannelList) :-

    List =?= [C1, C2], C1 =\= NULL, C2 =\= NULL :
      Comparer = {Operator, `C1, `C2} |
	compare_channel(C1, ChannelList, ChannelList'),
	compare_channel(C2, ChannelList'?, NextChannelList);

    otherwise :
      Operator = _,
      List = _,
      NextChannelList = ChannelList,
      Comparer = true.

  compare_channel(C, CL, NCL) :-

    CL ? Ch, C =\= Ch :
      NCL ! Ch |
	self;

    CL ? Ch, C =?= Ch :
      CL' = _,
      NCL = CL;

    CL = [] :
      NCL = [C].


/*
** find_process_refs/4
**
** Search for referenced processes, as part of a scope server
** termination.
**
** Input:
**
**    Refs is a stream of lookup_functor requests which have deferred until
**    the scope's definition has been completed.
**
**    Progeny is a list of child ProcessDefinitions.
**
**    NextOut is a continuation stream.
**
** Output:
**
**    Out is a stream of requests which cannot be satisfied at this level.
*/

find_process_refs(Refs, Progeny, Out, NextOut) :-

    Refs ? lookup_functor(Functor, CallType, CallDefinition) |
	search_progeny(Functor, Progeny, CallType, CallDefinition, Out, Out'),
	self;

    Refs = [] :
      Progeny = _,
      Out = NextOut.

/*
** search_progeny/6
**
** Search Progeny for a process whose Name is Functor.
**
** Input: Functor, Progeny, NextOut
**
** Output:
**
**   CallType is in {none, outer, inner}
**
**   CallDefinition is the ProcessDefinition of the found process.
**
**   Out relays unsatisied request, except at the base level.
*/

search_progeny(Functor, Progeny, CallType, CallDefinition, Out, NextOut) :-

    Progeny ? ProcessDefinition, arg(1, ProcessDefinition, Functor) :
      Progeny' = _,
      CallType = outer,
      CallDefinition = ProcessDefinition,
      Out = NextOut;


    Progeny ? ProcessDefinition, arg(1, ProcessDefinition, Name),
    Name =\= Functor |
	self;

    Progeny = [] :
      Out = [lookup_functor(Functor, CallType, CallDefinition) | NextOut];

    Progeny = [],
    Out = [] :
      Functor = _,
      CallType = none,
      CallDefinition = [],
      NextOut = [].

/*
** make_process_scope/11
**
** Analyze PiLHS, producing a ProcessDefinition and a process scope server,
** serve_process_scope.
**
** Input:
**
**   PiLHS is from a process/4 request.
**
**   WeightRate is the module type and the default base rate.
**
**   PublicNames is a list of channel descriptors (<stochactic_channel_list>)
**   which are public to the module, and which may be shared by other modules.
**
**   NewDefinition is the ProcessDefinition used by the new scope.
**
**   NextOut, NextErrors are continuation streams.
**
** Output:
**
**   ProcessScope is a stream to serve_process_scope.
**
**   Out is a stream to relay lookup_functor/3 requests.
**
**   ProcessDefinition is defined in make_process_definition.
**
**   Errors is defined in serve_empty_scope.
*/

make_process_scope(PiLHS, WeightRate, ProcessScope, PublicNames,
		NewChannelList1, Out, NextOut,
		NewDefinition, ProcessDefinition, Errors, NextErrors) :-

    true :
      ProcessDefinition = {Name?, Arity?, ChannelNames?, LHSS, _CodeTuple},
      LHSS = {OuterLHS?, InnerLHS?},
      Notes ! variables(Parameters?) |
	parse_lhs(PiLHS, WeightRate, Name, Arity,
			ParamList, ChannelList, NewChannelList,
				Errors, Errors'?),
	diagnose_duplicates(ParamList?, ParamList1,
			Name?, duplicate_parameter,
				Errors', Errors''?),
	utilities#sort_out_duplicates([ChannelList?], ChannelList1, DChs),
	diagnose_replies(DChs?, Name?, duplicate_channel,
				Errors'', Errors'''?),
	utilities#sort_out_duplicates([NewChannelList?], NewChannelList1, _),
	utilities#concatenate_lists([ParamList1?, ChannelList1?], PrivateList),
	diagnose_duplicates(PrivateList?, PrivateList1,
			Name?, channel_duplicates_parameter,
				Errors''', Errors''''?),
	correct_for_duplication(PrivateList?, PrivateList1?, ParamList,
				ChannelList1?, ChannelList2),
	remove_ambient_channel(PublicNames, PublicNames1),
	make_lhs_tuples(Name?, ParamList1?, PublicNames1, ChannelList2?,
				ChannelNames, OuterLHS, InnerLHS),
	extract_parameters(NewChannelList1?, Parameters, ParameterNames),
	atom_to_arguments(OuterLHS, Arguments),
	utilities#subtract_list(ParameterNames, Arguments, Undeclared),
	diagnose_replies(Undeclared, Name, undeclared_parameter,
				Errors'''', Errors'''''?),
	optimize_procedures(NewDefinition, Notes, Out, Out'?),
	serve_process_scope(ProcessScope?, NewDefinition, WeightRate, Notes',
				Out', NextOut, Errors''''', NextErrors).

  correct_for_duplication(L1, L2, P, C1, C2) :-

    L1 =?= L2 :
      P = _,
      C2 = C1;

    otherwise :
      L1 = _,
      C1 = _ |
	utilities#subtract_list(L2, P, C2).

  atom_to_arguments(Atom, Arguments) + (Index = 2) :-

    Index++ =< arity(Atom),
    arg(Index, Atom, `Name) :
      Arguments ! Name |
	self;

    Index > arity(Atom) :
      Arguments = [].

  remove_ambient_channel(PublicNames, PublicNames1) :-

    PublicNames ? AmbientName,
    string_to_dlist(AmbientName, AL, []),
    string_to_dlist("Ambient-", ADL, _) :
      AL = ADL,
      PublicNames1 = PublicNames';

    otherwise :
      PublicNames1 = PublicNames.

optimize_procedures(ProcessDefinition, Notes, Out, NextOut) :-

    ProcessDefinition =?= {_Name, _Arity, _ChannelNames, LHSS, _CodeTuple},
    LHSS = {OuterLHS, _InnerLHS},
    OuterLHS =?= [] :
      Notes = _,
      Out = NextOut;

    ProcessDefinition =?= {_Name, Arity, _ChannelNames, LHSS, _CodeTuple},
    LHSS = {OuterLHS, InnerLHS},
    OuterLHS =\= [],
    OuterLHS =\= InnerLHS,
    arg(1, InnerLHS, Name) :
      Out = [procedure([call(Name)], Arity, OuterLHS),
	     procedure(Notes, Arity, InnerLHS) | NextOut];
% | screen#display(outer(PName) - inner(Name));

    ProcessDefinition =?= {_Name, Arity, _ChannelNames, LHSS, _CodeTuple},
    LHSS = {OuterLHS, InnerLHS},
    OuterLHS =\= [],
    OuterLHS =?= InnerLHS :
      Out = [procedure(Notes, Arity, InnerLHS) | NextOut].
% | screen#display(only(Name, Arity)).
    

export_process(ProcessDefinition, Exported, Export, Exports, NextExports) :-

    ProcessDefinition =?= {Name, Arity, _ChannelNames, _LHSS, _CodeTuple},
    Name =\= NULL,
    Exported =?= all :
      Export = true,
      Exports ! (Name/Arity),
      NextExports = Exports';

    ProcessDefinition =?= {Name, Arity, _ChannelNames, _LHSS, _CodeTuple},
    Name =\= NULL,
    Exported =\= all |
	exported_procedure;	

    otherwise :
      ProcessDefinition = _,
      Exported = _,
      Export = _,
      Exports = NextExports.

  exported_procedure(Name, Arity, Exported, Export, Exports, NextExports) :-

    Exported = Name :
      Export = true,
      Exports ! (Name/Arity),
      NextExports = Exports';

    Exported = (Name/Arity) :
      Export = true,
      Exports ! (Name/Arity),
      NextExports = Exports';

    Exported ? Name :
      Exported' = _,
      Export = true,
      Exports ! (Name/Arity),
      NextExports = Exports';

    Exported ? (Name/Arity) :
      Exported' = _,
      Export = true,
      Exports ! (Name/Arity),
      NextExports = Exports';

    Exported ? _Other,
    otherwise |
	self;

    Exported = [] :
      Name = _,
      Arity = _,
      Export = false,
      NextExports = Exports.

add_process_definition(ProcessDefinition, PLHSS, Progeny, NewProgeny) :-

    ProcessDefinition =?= {Name, _Arity, _ChannelNames, LHSS, _CodeTuple},
    Name =\= NULL :
      PLHSS = LHSS,
      NewProgeny = [ProcessDefinition | Progeny];

    otherwise :
      ProcessDefinition = _,
      PLHSS = [],
      NewProgeny = Progeny.

extract_parameters(Declarations, Parameters, Names) :-

    Declarations ? String, string(String) |
	self;

    Declarations ? _ChannelName(BaseRate),
    BaseRate =?= infinite |
	self;

    Declarations ? _ChannelName(BaseRate),
    BaseRate =\= infinite |
	validate_arguments([BaseRate], Parameters, Parameters'?,
				Names, Names'?),
	self;

    Declarations ? _ChannelName(BaseRate, Weighter), string(Weighter) |
	validate_arguments([BaseRate], Parameters, Parameters'?,
				Names, Names'?),
	self;

    Declarations ? _ChannelName(BaseRate, Weighter),
    Weighter =?= `Name :
      Parameters ! Weighter,
      Names ! Name |
	validate_arguments([BaseRate], Parameters', Parameters''?,
				Names', Names''?),
	self;

    Declarations ? _ChannelName(BaseRate, Weighter),
    Weighter =\= `_,
    tuple(Weighter) |
	utils#tuple_to_dlist(Weighter, [Functor, _ | Arguments], []),
	validate_weighter_functor(Functor, Parameters, Parameters'?,
					Names, Names'?),
	validate_arguments([BaseRate | Arguments?], Parameters', Parameters''?,
				Names', Names''?),
	self;

    Declarations ? _ChannelName(_BaseRate, Weighter),
    otherwise :
      Names ! Weighter |
	self;

    Declarations = [] :
      Parameters = [],
      Names = [].

  validate_weighter_functor(Functor, Parameters, NextParameters,
				Names, NextNames) :-

    string(Functor) :
      Parameters = NextParameters,
      Names = NextNames;

    Functor = `Name :
      Parameters = [Functor | NextParameters],
      Names = [Name | NextNames];

    otherwise :
      Parameters = NextParameters,
      Names = [Functor | NextNames].

  validate_arguments(Arguments, Parameters, NextParameters,
			Names, NextNames) :-

    Arguments ? Number, number(Number) |
	self;

    Arguments ? Variable, Variable =?= `Name :
      Parameters ! Variable,
      Names ! Name |
	self;

    Arguments ? Other,
    otherwise :
      Names ! Other |
	self;

    Arguments = [] :
      Parameters = NextParameters,
      Names = NextNames.


parse_lhs(PiLHS, WeightRate, Name, Arity, ParamList,
		ChannelList, NewChannelList, Errors, NextErrors) :-

    PiLHS =?= `Functor, string(Functor), Functor =\= EMPTY :
      WeightRate = _,
      Name = Functor, Arity = 0,
      ParamList = [],
      Errors = NextErrors |
	unify_without_failure(ChannelList, []),
	unify_without_failure(NewChannelList, []);

    PiLHS = PiLHS' + Channels,
    PiLHS' =\= (_ + _) :
      ChannelList' = ChannelList?,
      NewChannelList' = NewChannelList? |
	utilities#untuple_predicate_list(',', Channels, Channels'),
	extract_channel_list(Channels', WeightRate, ChannelList,
				NewChannelList,	Errors, Errors'),
	self;

    arg(1, PiLHS, `Functor), string(Functor), Functor =\= EMPTY,
    arity(PiLHS, A), A-- :
      WeightRate = _,
      Name = Functor,
      Arity = A' |
	extract_arglist(PiLHS, ParamList, Errors, NextErrors),
	unify_without_failure(ChannelList, []),
	unify_without_failure(NewChannelList, []);

    otherwise :
      Errors ! improperly_formed_left_hand_side(PiLHS),
      PiLHS' = BIO_NULL |
	self.

extract_channel_list(Channels, WeightRate, ChannelList, NewChannelList,
			Errors, NextErrors) :-

    Channels ? ChannelName,
    string(ChannelName),
    nth_char(1, ChannelName, C), ascii(a) =< C, C =< ascii(z),
    WeightRate =?= SPI_DEFAULT_WEIGHT_NAME(BaseRate) :
      ChannelList ! ChannelName,
      NewChannelList ! ChannelName(BaseRate) |
	self;

    Channels ? ChannelName,
    string(ChannelName),
    nth_char(1, ChannelName, C), ascii(a) =< C, C =< ascii(z),
    WeightRate = Weighter(BaseRate),
    Weighter =\= SPI_DEFAULT_WEIGHT_NAME :
      ChannelList ! ChannelName,
      NewChannelList ! ChannelName(BaseRate, WeightRate) |
	self;

    Channels ? `VariableName,
    nth_char(1, VariableName, C), ascii('A') =< C, C =< ascii('Z'),
    string(VariableName), VariableName =\= NULL, VariableName =\= EMPTY :
      ChannelList ! VariableName,
      NewChannelList ! VariableName |
	self;

    Channels ? ChannelName(BaseRate),
    string(ChannelName),
    nth_char(1, ChannelName, C), ascii(a) =< C, C =< ascii(z),
    number(BaseRate), BaseRate >= 0,
    WeightRate = SPI_DEFAULT_WEIGHT_NAME(_BaseRate) :
      ChannelList ! ChannelName,
      NewChannelList ! ChannelName(BaseRate) |
	self;

    Channels ? ChannelName(BaseRate),
    string(ChannelName),
    nth_char(1, ChannelName, C), ascii(a) =< C, C =< ascii(z),
    number(BaseRate), BaseRate >= 0,
    WeightRate = Weighter(_BaseRate),
    Weighter =\= SPI_DEFAULT_WEIGHT_NAME :
      ChannelList ! ChannelName,
      NewChannelList ! ChannelName(BaseRate, Weighter) |
	self;

    Channels ? ChannelName(BaseRate),
    string(ChannelName),
    nth_char(1, ChannelName, C), ascii(a) =< C, C =< ascii(z),
    BaseRate =?= infinite :
      ChannelList ! ChannelName,
      NewChannelList ! ChannelName(BaseRate) |
	self;

    Channels ? ChannelName(BaseRate),
    string(ChannelName),
    nth_char(1, ChannelName, C), ascii(a) =< C, C =< ascii(z),
    BaseRate =?= `_ :
      ChannelList ! ChannelName,
      NewChannelList ! ChannelName(BaseRate) |
	self;

    Channels ? ChannelName(BaseRate, Weighter),
    string(ChannelName),
    nth_char(1, ChannelName, C), ascii(a) =< C, C =< ascii(z),
    number(BaseRate), BaseRate >= 0 :
      ChannelList ! ChannelName,
      NewChannelList ! ChannelName(BaseRate, NewWeighter?) |
	validate_new_weighter(Weighter, NewWeighter, Errors, Errors'?),
	self;

    Channels ? ChannelName(BaseRate, Weighter),
    string(ChannelName),
    nth_char(1, ChannelName, C), ascii(a) =< C, C =< ascii(z),
    BaseRate = `_ :
      ChannelList ! ChannelName,
      NewChannelList ! ChannelName(BaseRate, NewWeighter?) |
	validate_new_weighter(Weighter, NewWeighter, Errors, Errors'?),
	self;

    Channels ? Other,  
    otherwise :
      Errors ! invalid_item_in_new_channel_list(Other) |
	self;

    Channels =?= [] :
      WeightRate = _,
      ChannelList = [],
      NewChannelList = [],
      Errors = NextErrors.

  validate_new_weighter(Weighter, NewWeighter, Errors, NextErrors) :-

    string(Weighter), nth_char(1, Weighter, C),
    ascii('a') =< C, C =< ascii('z') :
      NewWeighter = Weighter,
      Errors = NextErrors;

    Weighter = `Name,
    string(Name), nth_char(1, Name, C),
    ascii('A') =< C, C =< ascii('Z') :
      NewWeighter = Weighter(BIO_NULL),
      Errors = NextErrors;

    tuple(Weighter),
    arg(1, Weighter, Name),
    string(Name), nth_char(1, Name, C),
    ascii('a') =< C, C =< ascii('z') |
	utils#tuple_to_dlist(Weighter, [Name | Args], []),
	validate_new_weighter_params,
	utils#list_to_tuple([Name, BIO_NULL | Params], NewWeighter);

    tuple(Weighter), Weighter =\= `_ |
	utils#tuple_to_dlist(Weighter, [Variable | Args], []),
	validate_new_weighter_params,
	utils#list_to_tuple([Variable, BIO_NULL | Params], NewWeighter);

    otherwise :
      Errors = [invalid_new_weighter(Weighter) | NextErrors],
      NewWeighter = SPI_DEFAULT_WEIGHT_NAME.

  validate_new_weighter_params(Args, Params, Errors, NextErrors) :-

    Args ? Arg, number(Arg) :
      Params ! Arg |
	self;

    Args ? Arg, Arg = `_ :
      Params ! Arg |
	self;

    Args ? Arg, otherwise :
      Errors ! invalid_default_weighter_parameter(Arg) |
	self;

    Args = [] :
      Params = [],
      Errors = NextErrors.

extract_arglist(PiLHS, ParamList, Errors, NextErrors) + 
			(Index = 2) :-
    arity(PiLHS) < Index :
      ParamList = [],
      Errors = NextErrors;

    arity(PiLHS) >= Index,
    arg(Index, PiLHS, ChannelName), Index++,
    string(ChannelName), ChannelName =\= EMPTY :
      ParamList ! ChannelName |
	self;

    arity(PiLHS) >= Index,
    arg(Index, PiLHS, `ChannelName), Index++,
    string(ChannelName), ChannelName =\= EMPTY :
      ParamList ! ChannelName |
	self;

    otherwise,
    arg(Index, PiLHS, NotString), Index++ :
      Errors ! invalid_parameter(NotString) |
	self.


make_lhs_tuples(Name, ParamList, PublicNames, ChannelList,
			ChannelNames, OuterLHS, InnerLHS) :-

    Name =?= NULL :
      ParamList = _,
      PublicNames = _,
      ChannelList = _,
      OuterLHS = [],
      InnerLHS = [],
      ChannelNames = [];

    Name =\= NULL,
    ChannelList =?= [] :
      InnerLHS = OuterLHS?|
	utilities#subtract_list(PublicNames, ParamList, PublicNames1),
	utilities#concatenate_lists([ParamList, PublicNames1?],	ChannelNames),
	construct_lhs_tuple(Name, EMPTY, ChannelNames?, OuterLHS);

    Name =\= NULL,
    ChannelList =\= [],
    PublicNames =?= [] |
	construct_lhs_tuple(Name, EMPTY, ParamList, OuterLHS),
	utilities#concatenate_lists([ParamList, ChannelList?], ChannelNames),
	construct_lhs_tuple(Name, DOT, ChannelNames?, InnerLHS);

    Name =\= NULL,
    ChannelList =\= [],
    PublicNames =\= [] |
	utilities#subtract_list(PublicNames, ParamList, PublicNames1),
	utilities#subtract_list(PublicNames1, ChannelList, PublicNames2),
	utilities#concatenate_lists([ParamList, PublicNames2?], OuterList),
	construct_lhs_tuple(Name, EMPTY, OuterList?, OuterLHS),
	utilities#concatenate_lists([OuterList?, ChannelList], ChannelNames),
	construct_lhs_tuple(Name, DOT, ChannelNames?, InnerLHS).


construct_lhs_tuple(Name, Suffix, ChannelNames, Tuple) :-

    string_to_dlist(Name, NameCs, NameEnd),
    string_to_dlist(Suffix, SuffixCs, []) :
      NameEnd = SuffixCs |
	list_to_string(NameCs, Name'),
	utilities#make_lhs_tuple(Name'?, ChannelNames, Tuple).



make_communication_guard(Type, Locus, ChannelName, Multiplier, Index, Guard) :-
			
    true :
      Guard = {vector(`ChannelName),
		request(Type, ChannelName, Multiplier, Index, Locus)}.


message_to_channels(Message, Name, ChannelNames, Privates, Underscore,
			MsChannelNames, Errors, NextErrors) + (Index = 1) :-

    Message = [] :
      Name = _,
      ChannelNames = _,
      Privates = _,
      Underscore = _,
      Index = _,
      MsChannelNames = [],
      Errors = NextErrors;

    arg(Index, Message, Channel),
    string(Channel),
    Underscore =\= true, Index++ :
      MsChannelNames ! OkChannelName? |
	utilities#verify_channel(Name, Channel, ChannelNames, Privates,
				OkChannelName, Errors, Errors'),
	self;

    arg(Index, Message, Var), Var =?= `Channel,
    Underscore =\= true, Var =\= BIO_SCHEDULER, Index++ :
      MsChannelNames ! OkChannelName? |
	utilities#verify_channel(Name, Channel, ChannelNames, Privates,
					OkChannelName, Errors, Errors'),
	self;

    arg(Index, Message, Var), Var =?= `Channel,
    Underscore =\= true, Var =?= BIO_SCHEDULER, Index++ :
      MsChannelNames ! Channel |
	self;

    arg(Index, Message, _),
    Underscore =?= true, Index++ :
      MsChannelNames ! NULL |
	self;

    Index > arity(Message) :
      Message = _,
      Name = _,
      ChannelNames = _,
      Privates = _,
      Underscore = _,
      MsChannelNames = [],
      Errors = NextErrors;

    otherwise,
    arg(Index, Message, Channel),
    Index++ :
      Errors ! (Name - invalid_channel_in_message(Message, Channel)),
      MsChannelNames ! [] |
	self;

    otherwise :
      ChannelNames = _,
      Privates = _,
      Underscore = _,
      Index = _,
      MsChannelNames = [],
      Errors = [(Name - invalid_channel_list(Message)) | NextErrors].


parse_message(Name, ChannelNames, Message, MsChannelNames, Privates, Primes,
			Errors, NextErrors) :-

    Message = [] :
      Name = _,
      ChannelNames = _,
      MsChannelNames = [],
      Privates = [],
      Primes = [],
      Errors = NextErrors;

    tuple(Message) :
      Index = 1 |
	receive_channel_names(Index, Message, Name, Strings, MsChannelNames,
					Errors, Errors'?),
	diagnose_duplicates(Strings, UniqueNames,
			Name, duplicate_receive_channel,
			Errors', NextErrors),
	instantiate_channels;

    otherwise :
      ChannelNames = _,
      MsChannelNames = [],
      Privates = [],
      Primes = [],
      Errors = [(Name - invalid_message(Message)) | NextErrors].


  receive_channel_names(Index, Message, Name, Strings, MsChannelNames,
				Errors, NextErrors) :-

    arg(Index, Message, ChannelName),
    string(ChannelName), ChannelName =\= EMPTY, ChannelName =\= NULL,
    Index++ :
      Strings ! ChannelName,
      MsChannelNames ! ChannelName |
	self;

    arg(Index, Message, `ChannelName),
    string(ChannelName), ChannelName =\= EMPTY, ChannelName =\= NULL,
    Index++ :
      Strings ! ChannelName,
      MsChannelNames ! ChannelName |
	self;

    arg(Index, Message, BIO_NULL),
    Index++ :
      MsChannelNames ! NULL |
	self;

    Index > arity(Message) :
      Name = _,
      Strings = [],
      MsChannelNames = [],
      Errors = NextErrors;

    otherwise,
    arg(Index, Message, ChannelName),
    Index++ :
      Errors ! (Name - invalid_receive_channel(ChannelName)) |
	self.

  instantiate_channels(UniqueNames, ChannelNames, Privates, Primes) :-

    UniqueNames ? ChannelName |
	private_or_prime(ChannelName, ChannelNames,
		Privates, Privates'?, Primes, Primes'?),
	self;

    UniqueNames = [] :
      ChannelNames = _,
      Privates = [],
      Primes = [];

    ChannelNames = [] :
      UniqueNames = _,
      Privates = [],
      Primes = [].

  private_or_prime(ChannelName, ChannelNames, 
	Privates, NextPrivates, Primes, NextPrimes) :-

    ChannelNames = [ChannelName |Å†_],
    string_to_dlist(ChannelName, CL, Prime) :
      Prime = [39],
      Privates = NextPrivates,
      Primes = [{ChannelName, ChannelNamePrime?} | NextPrimes] |
	list_to_string(CL, ChannelNamePrime);

    ChannelNames ? Other, Other =\= ChannelName |
	self;

    ChannelNames = [] :
      Privates = [ChannelName | NextPrivates],
      Primes = NextPrimes.


diagnose_duplicates(List1, List2, Name, Diagnostic, Errors, NextErrors) :-

	utilities#remove_duplicate_strings(List1, List2, Reply),
	diagnose_replies.


diagnose_replies(Reply, Name, Diagnostic, Errors, NextErrors) :-

    Reply ? Erroneous :
      Errors ! (Name - Diagnostic(Erroneous)) |
	self;

    Reply =?= [] :
      Name = _,
      Diagnostic = _,
      Errors = NextErrors.
