/*
Precompiler for Pi Calculus procedures - servers.

Bill Silverman, December 1999.

Last update by		$Author: bill $
		       	$Date: 2000/11/22 12:50:12 $
Currently locked by 	$Locker:  $
			$Revision: 1.8 $
			$Source: /home/qiana/Repository/PsiFcp/psifcp/servers.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language([evaluate, compound, colon]).
-export([serve_empty_scope/6]).

-include(psi_constants).

/*
** serve_empty_scope/6+5
**
** Serve requests from first-level processes.  See serve_process_scope/8+6
**
** Input:
**
**   In is the request input stream:
**
**      error(Diagnostic) - copy Diagnostic to Errors stream.
**
**      lookup_functor(Functor, CallType, CallDefinition)
**
**          Lookup process Functor and return:
**               CallType in {none, outer, inner}
**               CallDefinition = {Name, Arity, ChannelNames,
**                                 {OuterLHS, InnerLHS},
**				   Mode(SendRHS, ProcessRHS)}
**
**      process(PiLHS, Status, ProcessScope)
**
**          Analyze PiLHS (Left-hand-side) of process declaration and return
**               Status is one of  {single, double, nil} ;
**               ProcessScope - a stream to a process context handler.
**
**   Controls = {Exported, GlobalDescriptors, WeightRate}
**
**          Exported is "all" or a list of exported  Name  or  Name/Arity.
**
**          GlobalDescriptors is a list of global channels, Name(BaseRate).
**
**          WeightRate = Weighter(BaseRate) rate for new channels,
**
**          where Weighter is in {none, stochastic}.
**
** Output:
**
**   Exports is a list of exported procedures, name/arity.
**
**   Entries is a list of generated processes for first level procedure entry.
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
*/

serve_empty_scope(In, Controls, Exports, Entries, Optimize, Errors) +
		(Progeny = [], Refs = AddRef?, AddRef,
		 Sums, Summed = Sums?) :-

    In ? error(Error) :
      Errors ! Error |
	self;

    In ? lookup_functor(Functor, CallType, CallDefinition) :
      AddRef ! lookup_functor(Functor, CallType, CallDefinition) |
	self;

    In ? process(PiLHS, LHSS, NewChannelList, ProcessScope),
    Controls = {Exported, GlobalDescriptors, GlobalNames, WeightRate} |
	make_process_scope(PiLHS, WeightRate, ProcessScope, [],
				NewChannelList, In'', In'?,
		NewDefinition?, ProcessDefinition, Errors, Errors'?),
	export_process(ProcessDefinition?, Exported, Export,
				Exports, Exports'?),
	make_prefix_call(Export, GlobalNames, Prefix),
	create_entry(GlobalDescriptors, GlobalNames, Prefix?,
			ProcessDefinition?, NewDefinition,
			Entries, Entries'?, Optimize, Optimize'?),
	add_process_definition(NewDefinition?, LHSS, Progeny, Progeny'),
	self;

    In ? call_sum(Name, Sum, Call) :
      Sums ! Name(Sum, Call) |
	self;

    In ? procedure(Notes, Arity, LHSS) :
      Optimize ! procedure(Notes, Arity, LHSS, _Value) |
	self;

    In = [] :
      Controls = _,
      AddRef = [],
      Exports = [],
      Sums = [] |
	find_process_refs(Refs, Progeny, [], []),
	/* sum_procedures. */
	call#sum_procedures(Summed, Entries, Optimize, [/*develope*/], Errors).

  make_prefix_call(Export, GlobalNames, Prefix) :-

    Export =?= true,
    GlobalNames = [] |
      Prefix = scheduler(`"Scheduler.");

    Export =?= true,
    GlobalNames =\= [] |
      Prefix = global_channels(_GlobalPairList, `"Scheduler.");

    otherwise :
      Export = _,
      GlobalNames = _,
      Prefix = [].


create_entry(GlobalDescriptors, GlobalNames, Prefix,
		ProcessDefinition, NewDefinition,
		Entries, NextEntries, Optimize, NextOptimize) :-

    ProcessDefinition =?= {Name, Arity, ChannelNames, LHSS, CodeTuple},
    Name =\= "_",
    LHSS = {OuterLHS, _InnerLHS},
    tuple(Prefix),
    Index := arity(OuterLHS),
    Index++,
    string_to_dlist(Name, NL, []) :
      ascii('.', Period),
      Entries ! export(OuterLHS, Initializer?, []),
      NextEntries = Entries',
      Optimize ! procedure([call(Name'?)], Arity, OuterLHS, _Value),
      NextOptimize = Optimize',
      NewDefinition = {Name, Arity, ChannelNames'?, NewLHS?, CodeTuple},
      NewLHS = {OuterLHS'?, InnerLHS'?} |
	list_to_string([Period|NL], Name'),
	split_channels(1, Index, ChannelNames, ParamList, ChannelList),
	make_lhs_tuples,
	initialize_global_channels(Index', OuterLHS'?, GlobalDescriptors,
					Initializer, Prefix);

    ProcessDefinition =?= {Name, Arity, ChannelNames, LHSS, CodeTuple},
    LHSS = {OuterLHS, _InnerLHS},
    Name =\= "_",
    Prefix = [],
    GlobalNames =\= [],
    Index := arity(OuterLHS) :
      GlobalDescriptors = _,
      NextEntries = Entries,
      NextOptimize = Optimize,
      NewDefinition = {Name, Arity, ChannelNames'?, NewLHS?, CodeTuple},
      NewLHS = {OuterLHS'?, InnerLHS'?} |
	split_channels(1, Index, ChannelNames, ParamList, ChannelList),
	make_lhs_tuples;
	
    otherwise :
      GlobalDescriptors = _,
      GlobalNames = _,
      Prefix = _,
      NewDefinition = ProcessDefinition,
      Entries = NextEntries,
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


  initialize_global_channels(Index, Tuple, GlobalDescriptors,
				Initializer, Prefix) + (List = Tail?, Tail) :-

    Index =< arity(Tuple),
    arg(Index, Tuple, `ChannelName),
    GlobalDescriptors ? DuplicateName(_BaseRate),
    DuplicateName =\= ChannelName |
	self;

    Index =< arity(Tuple),
    arg(Index, Tuple, `ChannelName),
    GlobalDescriptors ? DuplicateName(_BaseRate, _Weighter),
    DuplicateName =\= ChannelName |
	self;

    Index =< arity(Tuple),
    arg(Index, Tuple, `ChannelName),
    GlobalDescriptors ? ChannelName(BaseRate),
    Index++ :
      Tail ! ChannelName(`ChannelName, BaseRate) |
	self;

    Index =< arity(Tuple),
    arg(Index, Tuple, `ChannelName),
    GlobalDescriptors ? ChannelName(BaseRate, Weighter),
    Index++ :
      Tail ! ChannelName(`ChannelName, Weighter, BaseRate) |
	self;

    otherwise,
    arg(1, Tuple, ProcedureName),
    arg(2, Prefix, GlobalPairList) :
      Index = _,
      GlobalDescriptors = _,
      Tail = [],
      Initializer = (psi_monitor#Prefix, ProcedureName) |
	unify_without_failure(GlobalPairList, List).


/*
** serve_process_scope/8+6
**
** Input:
**
**   In is the request input stream:
**
**      lhss(Outer, Inner) - return ProcessDefinition's
**                           OuterLHS and InnerLHS.
**
**      body_receive(Channel, Body, Goal)
**
**      body_send(Message, Channel, Goal)
**
**          
**
**      error(Diagnostic) - add  Name-Diagnostic  to Errors stream.
**
**      lookup_functor(Functor, CallType, CallDefinition)
**
**          Lookup process Functor and return:
**               CallType in {none, outer, inner}
**               CallDefinition = {Name, Arity, ChannelNames,
**                                 {OuterLHS, InnerLHS},
**                                 Mode(SendRHS, ProcessRHS)}
**
**      process(PiLHS, Status, ProcessScope)
**
**          Analyze PiLHS (Left-hand-side) of process declaration and return
**               Status is one of  {single, double, nil} ;
**               ProcessScope - a stream to a process context handler.
**
**   NextOut, NextErrors are continuation streams.
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
		(IdIndex = 1, Primes = [], Locals = [], Progeny = [],
		 Refs = AddRef?, AddRef) :-

    In ? lhss(Outer, Inner),
    ProcessDefinition =?= {_Name, _Arity, _ChannelNames, LHSS, _CodeTuple},
    LHSS = {OuterLHS, InnerLHS} :
      Outer = OuterLHS,
      Inner = InnerLHS |
	self;

    In ? call(Body1, Body2) |
	call#make_local_call(ProcessDefinition, Locals, Primes, Body1, Body2,
				In'', In'?, Errors, Errors'?, CallDefinition),
	add_call(CallDefinition, Body2, Notes, Notes'),
	self;

    In ? error(Description),
    arg(1, ProcessDefinition, Name) :
      Errors ! (Name - Description) |
	self;

    In ? end_clause :
      Locals = _,
      Primes = _ |
	self + (Locals = [], Primes = []);

    In ? guard_compare(Guard, ChannelList, NextChannelList, Comparer),
    Guard =?= {Operator, C1, C2},
    ProcessDefinition =?= {Name, _Arity, ChannelNames, _LHSS, _CodeTuple} :
      Notes ! variables(List?) |
	message_to_channels({C1, C2}, Name, ChannelNames, Locals, false, List,
				Errors, Errors'?),
	compare_channels_ok,
	self;

    In ? guard_receive(Channel, Message, Index, Guards),
    ProcessDefinition =?= {Name, _Arity, ChannelNames, _LHSS, _CodeTuple} :
      Primes = _,
      Guards = {Guard?, `"Message." = ChannelList?},
      Notes ! variables(ChannelName?) |
	parse_message(Message, Message', Multiplier, Name, Errors, Errors'?),
	verify_multiplier(Name, Multiplier?, Multiplier', ChannelNames,
				Errors', Errors''?),
	utilities#verify_channel(Name, Channel, ChannelNames, Locals,
				ChannelName, Errors'', Errors'''?),
	parse_message(Name, ChannelNames, Message'?, MsChannelNames,
			Locals', Primes', Errors''', Errors''''?),
	call#prime_local_channels(Primes'?, MsChannelNames?, MsChannelNames'),
	utilities#names_to_channel_list(MsChannelNames'?, ChannelList),
	make_communication_guard + (Type = receive),
	self;

    In ? guard_send(Channel, Message, Index, Guards),
    ProcessDefinition =?= {Name, _Arity, ChannelNames, _LHSS, _CodeTuple} :
      Guards = {Guard?, `"Message." = ChannelList},
      Notes ! variables([ChannelName'? | MsChannelNames'?]) |
	parse_message(Message, Message', Multiplier, Name, Errors, Errors'?),
	verify_multiplier(Name, Multiplier?, Multiplier', ChannelNames,
				Errors', Errors''?),
	utilities#verify_channel(Name, Channel, ChannelNames, Locals,
				ChannelName, Errors'', Errors'''?),
	message_to_channels(Message'?, Name, ChannelNames, Locals, false,
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
    arg(1, ProcessDefinition, Name),
    string_to_dlist(Name, DL, Tail),
    string_to_dlist(".", Dot, Suffix),
    convert_to_string(IdIndex, SI),
    IdIndex++,
    string_to_dlist(SI, SCs, []) :
      Tail = Dot,
      Suffix = SCs,
      Notes ! call(Id) | /* Can't hurt! */
	list_to_string(DL, Id),
	self;

    In ? process(PiLHS, PLHSS, NewChannelList, ProcessScope),
    ProcessDefinition =?= {_Name, _Arity, ChannelNames, _LHSS, _CodeTuple} |
	utilities#concatenate_lists([Locals, ChannelNames], GlobalNames),
	make_process_scope(PiLHS, WeightRate, ProcessScope, GlobalNames,
				NewChannelList,
		In'', In'?, NewDefinition?, NewDefinition, Errors, Errors'?),
	add_process_definition(NewDefinition?, PLHSS, Progeny, Progeny'),
	self;

    In ? remote_call(Call1, Call2),
    ProcessDefinition =?= {Name, _Arity, ChannelNames, _LHSS, _CodeTuple} :
      Notes ! variables(LogixVars?) |
	call#make_remote_call(Name, ChannelNames, Locals, Primes, Call1, Call2,
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
    In ? Procedure, Procedure  = procedure(_Notes, _Arity, _LHSS) :
      Out ! Procedure |
	self;

    In = [],
    ProcessDefinition =?= {_Name, _Arity, _ChannelNames, _LHSS, CodeTuple} :
      WeightRate = _,
      IdIndex = _,
      Locals = _,
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

    List =?= [C1, C2], C1 =\= "_", C2 =\= "_" :
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
**   GlobalNames is a list of channel descriptors (<stochactic_channel_list>)
**   which are global to the module, and which may be shared by other modules.
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

make_process_scope(PiLHS, WeightRate, ProcessScope, GlobalNames,
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
	utilities#concatenate_lists([ParamList1?, ChannelList1?], LocalList),
	diagnose_duplicates(LocalList?, LocalList1,
			Name?, channel_duplicates_parameter,
				Errors''', Errors''''?),
	correct_for_duplication(LocalList?, LocalList1?, ParamList,
				ChannelList1?, ChannelList2),
	make_lhs_tuples(Name?, ParamList1?, GlobalNames, ChannelList2?,
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
    Name =\= "_",
    Exported =?= all :
      Export = true,
      Exports ! (Name/Arity),
      NextExports = Exports';

    ProcessDefinition =?= {Name, Arity, _ChannelNames, _LHSS, _CodeTuple},
    Name =\= "_",
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
    Name =\= "_" :
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

    PiLHS =?= `Functor, string(Functor), Functor =\= "" :
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

    arg(1, PiLHS, `Functor), string(Functor), Functor =\= "",
    arity(PiLHS, A), A-- :
      WeightRate = _,
      Name = Functor,
      Arity = A' |
	extract_arglist(PiLHS, ParamList, Errors, NextErrors),
	unify_without_failure(ChannelList, []),
	unify_without_failure(NewChannelList, []);

    otherwise :
      Errors ! improperly_formed_left_hand_side(PiLHS),
      PiLHS' = `"_" |
	self.

extract_channel_list(Channels, WeightRate, ChannelList, NewChannelList,
			Errors, NextErrors) :-

    Channels ? ChannelName,
    string(ChannelName),
    nth_char(1, ChannelName, C), ascii(a) =< C, C =< ascii(z),
    WeightRate =?= PSI_DEFAULT_WEIGHT_NAME(BaseRate) :
      ChannelList ! ChannelName,
      NewChannelList ! ChannelName(BaseRate) |
	self;

    Channels ? ChannelName,
    string(ChannelName),
    nth_char(1, ChannelName, C), ascii(a) =< C, C =< ascii(z),
    WeightRate = Weighter(BaseRate),
    Weighter =\= PSI_DEFAULT_WEIGHT_NAME :
      ChannelList ! ChannelName,
      NewChannelList ! ChannelName(BaseRate, WeightRate) |
	self;

    Channels ? `VariableName,
    nth_char(1, VariableName, C), ascii('A') =< C, C =< ascii('Z'),
    string(VariableName), VariableName =\= "_", VariableName =\= "" :
      ChannelList ! VariableName,
      NewChannelList ! VariableName |
	self;

    Channels ? ChannelName(BaseRate),
    string(ChannelName),
    nth_char(1, ChannelName, C), ascii(a) =< C, C =< ascii(z),
    number(BaseRate), BaseRate >= 0,
    WeightRate = PSI_DEFAULT_WEIGHT_NAME(_BaseRate) :
      ChannelList ! ChannelName,
      NewChannelList ! ChannelName(BaseRate) |
	self;

    Channels ? ChannelName(BaseRate),
    string(ChannelName),
    nth_char(1, ChannelName, C), ascii(a) =< C, C =< ascii(z),
    number(BaseRate), BaseRate >= 0,
    WeightRate = Weighter(_BaseRate),
    Weighter =\= PSI_DEFAULT_WEIGHT_NAME :
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
      NewWeighter = Weighter(`"_"),
      Errors = NextErrors;

    tuple(Weighter),
    arg(1, Weighter, Name),
    string(Name), nth_char(1, Name, C),
    ascii('a') =< C, C =< ascii('z') |
	utils#tuple_to_dlist(Weighter, [Name | Args], []),
	validate_new_weighter_params,
	utils#list_to_tuple([Name, `"_" | Params], NewWeighter);

    tuple(Weighter), Weighter =\= `_ |
	utils#tuple_to_dlist(Weighter, [Variable | Args], []),
	validate_new_weighter_params,
	utils#list_to_tuple([Variable, `"_" | Params], NewWeighter);

    otherwise :
      Errors = [invalid_new_weighter(Weighter) | NextErrors],
      NewWeighter = PSI_DEFAULT_WEIGHT_NAME.

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
    string(ChannelName), ChannelName =\= "" :
      ParamList ! ChannelName |
	self;

    arity(PiLHS) >= Index,
    arg(Index, PiLHS, `ChannelName), Index++,
    string(ChannelName), ChannelName =\= "" :
      ParamList ! ChannelName |
	self;

    otherwise,
    arg(Index, PiLHS, NotString), Index++ :
      Errors ! invalid_parameter(NotString) |
	self.


make_lhs_tuples(Name, ParamList, GlobalNames, ChannelList,
			ChannelNames, OuterLHS, InnerLHS) :-

    Name =?= "_" :
      ParamList = _,
      GlobalNames = _,
      ChannelList = _,
      OuterLHS = [],
      InnerLHS = [],
      ChannelNames = [];

    Name =\= "_",
    ChannelList =?= [] :
      InnerLHS = OuterLHS?|
	utilities#subtract_list(GlobalNames, ParamList, GlobalNames1),
	utilities#concatenate_lists([ParamList, GlobalNames1?],	ChannelNames),
	construct_lhs_tuple(Name, "", ChannelNames?, OuterLHS);

    Name =\= "_",
    ChannelList =\= [],
    GlobalNames =?= [] |
	construct_lhs_tuple(Name, "", ParamList, OuterLHS),
	utilities#concatenate_lists([ParamList, ChannelList?], ChannelNames),
	construct_lhs_tuple(Name, ".", ChannelNames?, InnerLHS);

    Name =\= "_",
    ChannelList =\= [],
    GlobalNames =\= [] |
	utilities#subtract_list(GlobalNames, ParamList, GlobalNames1),
	utilities#subtract_list(GlobalNames1, ChannelList, GlobalNames2),
	utilities#concatenate_lists([ParamList, GlobalNames2?], OuterList),
	construct_lhs_tuple(Name, "", OuterList?, OuterLHS),
	utilities#concatenate_lists([OuterList?, ChannelList], ChannelNames),
	construct_lhs_tuple(Name, ".", ChannelNames?, InnerLHS).


construct_lhs_tuple(Name, Suffix, ChannelNames, Tuple) :-

    string_to_dlist(Name, NameCs, NameEnd),
    string_to_dlist(Suffix, SuffixCs, []) :
      NameEnd = SuffixCs |
	list_to_string(NameCs, Name'),
	utilities#make_lhs_tuple(Name'?, ChannelNames, Tuple).



make_communication_guard(Type, ChannelName, Multiplier, Index, Guard) :-
			
    true :
      Guard = {vector(`ChannelName),
		request(Type, ChannelName, Multiplier, Index)}.


message_to_channels(Message, Name, ChannelNames, Locals, Underscore,
			MsChannelNames, Errors, NextErrors) + (Index = 1) :-

    Message = [] :
      Name = _,
      ChannelNames = _,
      Locals = _,
      Underscore = _,
      Index = _,
      MsChannelNames = [],
      Errors = NextErrors;

    arg(Index, Message, Channel),
    Underscore =\= true, Index++ :
      MsChannelNames ! OkChannelName? |
	utilities#verify_channel(Name, Channel, ChannelNames, Locals,
				OkChannelName, Errors, Errors'),
	self;

    arg(Index, Message, `Channel),
    Underscore =\= true, Index++ :
      MsChannelNames ! OkChannelName? |
	utilities#verify_channel(Name, Channel, ChannelNames, Locals,
				OkChannelName, Errors, Errors'),
	self;

    arg(Index, Message, _),
    Underscore =?= true, Index++ :
      MsChannelNames ! "_" |
	self;

    Index > arity(Message) :
      Message = _,
      Name = _,
      ChannelNames = _,
      Locals = _,
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
      Locals = _,
      Underscore = _,
      Index = _,
      MsChannelNames = [],
      Errors = [(Name - invalid_channel_list(Message)) | NextErrors].


parse_message(Name, ChannelNames, Message, MsChannelNames, Locals, Primes,
			Errors, NextErrors) :-

    Message = [] :
      Name = _,
      ChannelNames = _,
      MsChannelNames = [],
      Locals = [],
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
      Locals = [],
      Primes = [],
      Errors = [(Name - invalid_message(Message)) | NextErrors].


  receive_channel_names(Index, Message, Name, Strings, MsChannelNames,
				Errors, NextErrors) :-

    arg(Index, Message, ChannelName),
    string(ChannelName), ChannelName =\= "", ChannelName =\= "_",
    Index++ :
      Strings ! ChannelName,
      MsChannelNames ! ChannelName |
	self;

    arg(Index, Message, `ChannelName),
    string(ChannelName), ChannelName =\= "", ChannelName =\= "_",
    Index++ :
      Strings ! ChannelName,
      MsChannelNames ! ChannelName |
	self;

    arg(Index, Message, `"_"),
    Index++ :
      MsChannelNames ! "_" |
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

  instantiate_channels(UniqueNames, ChannelNames, Locals, Primes) :-

    UniqueNames ? ChannelName |
	local_or_prime(ChannelName, ChannelNames,
		Locals, Locals'?, Primes, Primes'?),
	self;

    UniqueNames = [] :
      ChannelNames = _,
      Locals = [],
      Primes = [];

    ChannelNames = [] :
      UniqueNames = _,
      Locals = [],
      Primes = [].

  local_or_prime(ChannelName, ChannelNames, 
	Locals, NextLocals, Primes, NextPrimes) :-

    ChannelNames = [ChannelName |Å†_],
    string_to_dlist(ChannelName, CL, Prime) :
      Prime = [39],
      Locals = NextLocals,
      Primes = [{ChannelName, ChannelNamePrime?} | NextPrimes] |
	list_to_string(CL, ChannelNamePrime);

    ChannelNames ? Other, Other =\= ChannelName |
	self;

    ChannelNames = [] :
      Locals = [ChannelName | NextLocals],
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
