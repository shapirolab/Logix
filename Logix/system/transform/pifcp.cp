/*
Precompiler for Pi Calculus procedures.

Bill Silverman, December 1999.

Last update by		$Author: bill $
		       	$Date: 2000/02/10 10:50:20 $
Currently locked by 	$Locker:  $
			$Revision: 1.5 $
			$Source: /home/qiana/Repository/Logix/system/transform/Attic/pifcp.cp,v $

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


/*
** serve_empty_scope/6+5
**
** Serve requests from first-level processes.  See serve_process_scope/6+6
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
**               CallDefinition = {Name, Arity, Channels, OuterLHS, InnerLHS,
**					Mode(SendRHS, ProcessRHS)}
**
**      process(PiLHS, Status, ProcessScope)
**
**          Analyze PiLHS (Left-hand-side) of process declaration and return
**               Status is one of  {single, double, nil} ;
**               ProcessScope - a stream to a process context handler.
**
**   GlobalList is a list of global channels.
**
**   Exported is a list of exported  Name  or  Name/Arity.
**
** Output:
**
**   Entries is a list of generated processes for first level procedure entry.
**
**   Errors is a list of error diagnostics.
**
** Local:
**
**   Progeny is a list of child ProcessDefinitions.
**
**   Refs, AddRef is a list of deferred lookup_functor/3 requests.
*/

serve_empty_scope(In, GlobalList, Exported, Exports, Entries, Errors) +
		(Progeny = [], Refs = AddRef?, AddRef,
		 Sums, Summed = Sums?) :-

    In ? error(Error) :
      Errors ! Error |
	self;

    In ? lookup_functor(Functor, CallType, CallDefinition) :
      AddRef ! lookup_functor(Functor, CallType, CallDefinition) |
	self;

    In ? process(PiLHS, Status, ProcessScope) |
	make_process_scope(PiLHS, ProcessScope, [], In'', In'?,
		NewDefinition?, ProcessDefinition, Errors, Errors'?),
	compute_status,
	export_process(ProcessDefinition?, Exported, Export,
				Exports, Exports'?),
	create_entry(GlobalList, Export?, ProcessDefinition?, NewDefinition,
			Entries, Entries'?),
	add_process_definition(NewDefinition?, Progeny, Progeny'),
	self;

    In ? call_sum(Name, Sum, Call) :
      Sums ! Name(Sum, Call) |
	self;

    In = [] :
      GlobalList = _,
      Exported = _,
      AddRef = [],
      Exports = [],
      Sums = [] |
	find_process_refs(Refs, Progeny, [], []),
	/* sum_procedures. */
	sum_procedures(Summed, Entries, Errors).


create_entry(GlobalList, Export, ProcessDefinition, NewDefinition,
		Entries, NextEntries) :-


    ProcessDefinition =?= {Name, Arity, Channels, OuterLHS, _InnerLHS,
					CodeTuple},
    Name =\= "_",
    GlobalList =\= [],
    Export = true,
    Index := arity(OuterLHS),
    Index++,
    string_to_dlist(Name, NL, []) :
      ascii(' ', Space),
      Entries ! (Atom? :- Initializer?),
      NextEntries = Entries',
      NewDefinition = {Name, Arity, Channels'?, OuterLHS'?, InnerLHS'?,
					CodeTuple} |
	tuple_to_atom(OuterLHS, Atom),
	list_to_string([Space|NL], Name'),
	split_channels(1, Index, Channels, ParamList, ChannelList),
	make_lhs_tuples,
	initialize_global_channels(Index', OuterLHS'?, Initializer);

    ProcessDefinition =?= {Name, Arity, Channels, OuterLHS, _InnerLHS,
					CodeTuple},
    Name =\= "_",
    GlobalList =\= [],
    Export = false,
    Index := arity(OuterLHS) :
      NextEntries = Entries,
      NewDefinition = {Name, Arity, Channels'?, OuterLHS'?, InnerLHS'?,
					CodeTuple} |
	split_channels(1, Index, Channels, ParamList, ChannelList),
	make_lhs_tuples;
	
    otherwise :
      GlobalList = _,
      Export = _,
      NewDefinition = ProcessDefinition,
      Entries = NextEntries.

  split_channels(I, Index, Channels, ParamList, ChannelList) :-

    Channels ? Channel,
    I < Index,
    I++ :
      ParamList ! Channel |
	self;

    Channels ? Channel,
    I >= Index,
    I++ :
      ChannelList ! Channel |
	self;

    Channels =?= [] :
      I = _,
      Index = _,
      ParamList = [],
      ChannelList = [].


  initialize_global_channels(Index, Atom, Initializer)
				+ (List = Tail?, Tail) :-

    Index =< arity(Atom),
    arg(Index, Atom, `Channel),
    Index++ :
      Tail ! Channel(`Channel) |
	self;

    Index > arity(Atom),
    arg(1, Atom, PName) :
      Tail = [],
      Initializer = (pi_monitor#global_channels(List), PName).


/*
** serve_process_scope/6+3
**
** Serve requests from first-level processes.  See serve_process_scope/6+6
**
** Input:
**
**   In is the request input stream:
**
**      atoms(Outer, Inner) - return ProcessDefinition's
**                            OuterLHS and InnerLHS.
**
**      body_receive(Channel, Body, ChannelVar, ChannelList)
**
**      body_send(Message, Channel, Sender, ChannelList, ChannelVar)
**
**          
**
**      error(Diagnostic) - add  Name-Diagnostic  to Errors stream.
**
**      lookup_functor(Functor, CallType, CallDefinition)
**
**          Lookup process Functor and return:
**               CallType in {none, outer, inner}
**               CallDefinition = {Name, Arity, Channels, OuterLHS, InnerLHS,
**					 Mode(SendRHS, ProcessRHS)}
**
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
**   Errors is defined in serve_empty_scope.
**
** Local: see serve_empty_scope/6+3 above.
*/

serve_process_scope(In, ProcessDefinition, Out, NextOut, Errors, NextErrors) +
		(IdIndex = 1, Primes = [], Locals = [], Progeny = [],
		 Refs = AddRef?, AddRef) :-

    In ? atoms(Outer, Inner),
    ProcessDefinition =?= {_Name, _Arity, _Channels, OuterLHS, InnerLHS,
					_CodeTuple} :
      Outer = OuterLHS,
      Inner = InnerLHS |
	self;

    In ? body_receive(Channel, Message, ChannelVar, ChannelList),
    ProcessDefinition =?= {Name, _Arity, Channels, _OuterLHS, _InnerLHS,
					_CodeTuple} :
      ChannelVar = `ChannelName' |
	verify_channel(Name, Channel, Channels, Locals,
			ChannelName, Errors, Errors'?),
	instantiated_local_channels(Primes, [ChannelName], [ChannelName']),
	message_to_channels(Message, Name, Channels, Locals, true, MChannels,
				Errors', Errors''?),
	channels_to_variables(MChannels?, Variables, N),
	make_channel_list,
	self;

    In ? body_send(Message, Channel, Sender, ChannelList, ChannelVar),
    ProcessDefinition =?= {Name, _Arity, Channels, _OuterLHS, _InnerLHS,
					_CodeTuple} :
      ChannelVar = `ChannelName'? |
	verify_channel(Name, Channel, Channels, Locals,
			ChannelName, Errors, Errors'?),
	make_sender,
	message_to_channels(Message, Name, Channels, Locals, false, MChannels,
				Errors', Errors''?),
	instantiated_local_channels(Primes,
		[ChannelName? | MChannels?], [ChannelName' | MChannels']),
	channels_to_variables(MChannels'?, Variables, N),
	make_channel_list,
	self;

    In ? call(Body1, Body2) |
	make_local_call(ProcessDefinition, Locals, Primes, Body1, Body2,
			In'', In'?, Errors, Errors'?, _CallDefinition),
	self;

    In ? channels(Cs),
    ProcessDefinition =?= {_Name, _Arity, Channels, _OuterLHS, _InnerLHS,
					_CodeTuple} :
      Cs = Channels |
	self;

    In ? error(Description),
    arg(1, ProcessDefinition, Name) :
      Errors ! (Name - Description) |
	self;

    In ? guard_compare(Guard, ChannelList, NextChannelList, Comparer),
    Guard =?= {Operator, C1, C2},
    ProcessDefinition =?= {Name, _Arity, Channels, _OuterLHS, _InnerLHS,
					_CodeTuple} |
	message_to_channels({C1, C2}, Name, Channels, Locals, false, List,
				Errors, Errors'?),
	compare_channels_ok,
	self;

    In ? guard_receive(Channel, Message, SendId, Iterates, Consume),
    ProcessDefinition =?= {Name, _Arity, Channels, _OuterLHS, _InnerLHS,
					_CodeTuple} :
      Primes = _ |
	verify_channel(Name, Channel, Channels, Locals,
			Channel', Errors, Errors'?),
	parse_message(Name, Channels, Message, ChannelNames,
			Locals', Primes', Errors', Errors''?),
	instantiated_local_channels(Primes'?, ChannelNames?, ChannelNames'),
	channels_to_variables(ChannelNames', Variables, N),
	make_channel_list,
	make_guard_receive,
	self;

    In ? guard_send(Channel, Message, SendId, SendIndex, Guard),
    ProcessDefinition =?= {Name, _Arity, Channels, _OuterLHS, _InnerLHS,
					_CodeTuple} |
	verify_channel(Name, Channel, Channels, Locals,
			ChannelName, Errors, Errors'?),
	message_to_channels(Message, Name, Channels, Locals, false, MChannels,
				Errors', Errors''?),
	instantiated_local_channels(Primes,
		[ChannelName? | MChannels?], [ChannelName' | MChannels']),
	/* Check channels */
	channels_to_variables(MChannels'?, Variables, N),
	make_channel_list,
	make_guard_sender,
	make_guard_send,
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
      Suffix = SCs |
	list_to_string(DL, Id),
	self;

    In ? process(PiLHS, Status, ProcessScope),
    ProcessDefinition =?= {_Name, _Arity, Channels, _OuterLHS, _InnerLHS,
					_CodeTuple} |
	concatenate_lists([Locals, Channels], GlobalList),
	make_process_scope(PiLHS, ProcessScope, GlobalList,
		In'', In'?, NewDefinition?, NewDefinition, Errors, Errors'?),
	compute_status,
	add_process_definition(NewDefinition?, Progeny, Progeny'),
	self;

    In ? remote_call(Call1, Call2),
    ProcessDefinition =?= {Name, _Arity, Channels, _OuterLHS, _InnerLHS,
					_CodeTuple} |
	extract_arguments_or_substitutes(Name, Call1, Arguments, _Substitutes,
			Errors, Errors'?, 2, channel),
	verify_call_channels(Name, Call1, Channels, Locals,
				Errors', Errors''?),
	instantiated_local_channels(Primes, Arguments?, Arguments'),
	complete_remote_call(ProcessDefinition, Call1, Arguments'?, Call2,
				Errors'', Errors'''?),
	self;

    /* Code for communication procedures */
    In ? code(Mode, SendRHS, ProcessRHS),
    ProcessDefinition =?= {_Name, _Arity, _Channels, _OuterLHS, _InnerLHS,
					CodeTuple} :
      CodeTuple = Mode?(SendRHS?, ProcessRHS?) |
	self;

    /* Pass along */
    In ? CallSum, CallSum = call_sum(_Name, _Sum, _Call) :
      Out ! CallSum |
	self;

    In = [],
    ProcessDefinition =?= {_Name, _Arity, _Channels, _OuterLHS, _InnerLHS,
					CodeTuple} :
      IdIndex = _,
      Locals = _,
      Primes = _,
      AddRef = [],
      Errors = NextErrors |
	unify_without_failure(CodeTuple, none([], [])),
	find_process_refs(Refs, Progeny, Out, NextOut).

  compare_channels_ok(Operator, List, Comparer,
			ChannelList, NextChannelList) :-

    List =?= [C1, C2], C1 =\= "_", C2 =\= "_" :
      Compare = {Operator, `pinch(C1), `pinch(C2)} |
	compare_channel(C1, Extract1, ChannelList, ChannelList'),
	compare_channel(C2, Extract2, ChannelList'?, NextChannelList),
	make_comparer;

    otherwise :
      Operator = _,
      List = _,
      NextChannelList = ChannelList,
      Comparer = true.

  compare_channel(C, Extract, CL, NCL) :-

    CL ? Ch, C =\= Ch :
      NCL ! Ch |
	self;

    CL ? Ch, C =?= Ch :
      CL' = _,
      Extract = true,
      NCL = CL;

    CL = [] :
      Extract = (`C = {`"_", `pinch(C), `"_"}),
      NCL = [C].
    
  make_comparer(Extract1, Extract2, Compare, Comparer) :-

    Extract1 =?= Extract2,
    Extract1 =?= true :
      Comparer = Compare;

    /* Somebody wrote  a =?= a  or  a =\= a  */
    Extract1 =?= Extract2,
    Extract1 =\= true :
      Comparer = (Extract1, Compare);

    Extract1 =\= true,
    Extract2 =?= true :
      Comparer = (Extract1, Compare);

    Extract1 =?= true,
    Extract2 =\= true :
      Comparer = (Extract2, Compare);

    Extract1 =\= true,
    Extract2 =\= true,
    Extract1 =\= Extract2 :
      Comparer = (Extract1, Extract2, Compare).


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
** make_process_scope/9
**
** Analyze PiLHS, producing a ProcessDefinition and a process scope server,
** serve_process_scope.
**
** Input:
**
**   PiLHS is from a process/2 request.
**
**   GlobalList is a list of channel names which are global to the process.
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

make_process_scope(PiLHS, ProcessScope, GlobalList,
	Out, NextOut, NewDefinition, ProcessDefinition, Errors, NextErrors) :-

    true :
      ProcessDefinition =?= {Name?, Arity?, Channels?, OuterLHS?, InnerLHS?,
					_CodeTuple} |
	parse_lhs(PiLHS, Name, Arity, ParamList, ChannelList, Errors, Errors'?),
	check_for_duplicates(ParamList?, ParamList1,
				{Name?, duplicate_parameter},
					Errors', Errors''?),
	check_for_duplicates(ChannelList?, ChannelList1,
				{Name?, duplicate_channel},
					Errors'', Errors'''?),
	concatenate_lists([ParamList1?, ChannelList1?], LocalList),
	check_for_duplicates(LocalList?, LocalList1,
				{Name?, channel_duplicates_parameter},
					Errors''', Errors''''?),
	correct_for_duplication(LocalList?, LocalList1?, ParamList,
				ChannelList1?, ChannelList2),
	make_lhs_tuples(Name?, ParamList1?, GlobalList, ChannelList2?,
				Channels, OuterLHS, InnerLHS),
	serve_process_scope(ProcessScope?, NewDefinition,
				Out, NextOut, Errors'''', NextErrors).

  correct_for_duplication(L1, L2, P, C1, C2) :-

    L1 =?= L2 :
      P = _,
      C2 = C1;

    otherwise :
      L1 = _,
      C1 = _ |
	subtract_list(L2, P, C2).
	

compute_status(NewDefinition, Status) :-

    NewDefinition =?= {_Name, _Arity, _Channels, OuterLHS, _InnerLHS,
					_CodeTuple},
    OuterLHS =?= [] :
      Status = nil;    

    NewDefinition =?= {_Name, _Arity, _Channels, OuterLHS, InnerLHS,
					_CodeTuple},
    OuterLHS =\= [], OuterLHS =?= InnerLHS :
      Status = single;

    otherwise :
      NewDefinition = _,
      Status = double.

export_process(ProcessDefinition, Exported, Export, Exports, NextExports) :-

    ProcessDefinition =?= {Name, Arity, _Channels, _OuterLHS, _InnerLHS,
					_CodeTuple},
    Name =\= "_",
    Exported =?= all :
      Export = true,
      Exports ! (Name/Arity),
      NextExports = Exports';

    ProcessDefinition =?= {Name, Arity, _Channels, _OuterLHS, _InnerLHS,
					_CodeTuple},
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


add_process_definition(ProcessDefinition, Progeny, NewProgeny) :-

    ProcessDefinition =?= {Name, _Arity, _Channels, _OuterLHS, _InnerLHS,
					_CodeTuple},
    Name =\= "_" :
      NewProgeny = [ProcessDefinition | Progeny];

    otherwise :
      ProcessDefinition = _,
      NewProgeny = Progeny.


parse_lhs(PiLHS, Name, Arity, ParamList, ChannelList, Errors, NextErrors) :-

    PiLHS =?= `Functor, string(Functor), Functor =\= "" :
      Name = Functor, Arity = 0,
      ParamList = [],
      Errors = NextErrors |
	unify_without_failure(ChannelList, []);

    PiLHS = PiLHS' + Channels,
    writable(ChannelList) :
      ChannelList' = ChannelList? |
	extract_channel_list(Channels, ChannelList, Errors, Errors'),
	self;

    arg(1, PiLHS, `Functor), string(Functor), Functor =\= "",
    arity(PiLHS, A), A-- :
      Name = Functor,
      Arity = A' |
	extract_arglist(PiLHS, ParamList, Errors, NextErrors),
	unify_without_failure(ChannelList, []);

    otherwise :
      Errors ! improperly_formed_left_hand_side(PiLHS),
      PiLHS' = `"_" |
	self.

extract_channel_list(Channels, ChannelList, Errors, NextErrors) :-

    string(Channels), Channels =\= "_", Channels =\= "" :
      ChannelList = [Channels],
      Errors = NextErrors;

    Channels = `ChannelName,
    string(Channels), Channels =\= "_", Channels =\= "" :
      ChannelList = [ChannelName],
      Errors = NextErrors;

    Channels = (ChannelName, Channels'),
    string(ChannelName), ChannelName =\= "_", ChannelName =\= "" :
      ChannelList ! ChannelName |
	self;

    Channels = (`ChannelName, Channels'),
    string(ChannelName), ChannelName =\= "_", ChannelName =\= "" :
      ChannelList ! ChannelName |
	self;

    Channels = (NotChannel, Channels'),
    otherwise :
      Errors ! invalid_channel_in_channel_list(NotChannel) |
	self;

    otherwise :
      Errors = [invalid_channel_in_channel_list(Channels) | NextErrors],
      ChannelList = [].


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


make_lhs_tuples(Name, ParamList, GlobalList, ChannelList,
		Channels, OuterLHS, InnerLHS) :-

    Name =?= "_" :
      ParamList = _,
      GlobalList = _,
      ChannelList = _,
      OuterLHS = [],
      InnerLHS = [],
      Channels = [];

    Name =\= "_",
    ChannelList =?= [] :
      InnerLHS = OuterLHS?|
	subtract_list(GlobalList, ParamList, GlobalList1),
	concatenate_lists([ParamList, GlobalList1?], Channels),
	construct_lhs_tuple(Name, "", Channels?, OuterLHS);

    Name =\= "_",
    ChannelList =\= [],
    GlobalList =?= [] |
	construct_lhs_tuple(Name, "", ParamList, OuterLHS),
	concatenate_lists([ParamList, ChannelList?], Channels),
	construct_lhs_tuple(Name, ".", Channels?, InnerLHS);

    Name =\= "_",
    ChannelList =\= [],
    GlobalList =\= [] |
	subtract_list(GlobalList, ParamList, GlobalList1),
	subtract_list(GlobalList1, ChannelList, GlobalList2),
	concatenate_lists([ParamList, GlobalList2?], OuterList),
	construct_lhs_tuple(Name, "", OuterList?, OuterLHS),
	concatenate_lists([OuterList?, ChannelList], Channels),
	construct_lhs_tuple(Name, ".", Channels?, InnerLHS).


construct_lhs_tuple(Name, Suffix, Channels, Atom) :-

    string_to_dlist(Name, NameCs, NameEnd),
    string_to_dlist(Suffix, SuffixCs, []) :
      NameEnd = SuffixCs |
	list_to_string(NameCs, Name'),
	channels_to_variables(Channels, Channels', Count),
	make_lhs_tuple(Count?, Name', Channels'?, Atom).

channels_to_variables(ChannelNames, Variables, Count) +
				(Counter = 0) :-
    ChannelNames ? ChannelName,
    Counter++ :
      Variables ! `ChannelName |
	self;

    ChannelNames =?= [] :
      Variables = [],
      Count = Counter.

make_lhs_tuple(N, Name, Channels, Atom) :-

    N =< 0 :
      Atom = {Name},
      Channels = _;
    
    N > 0,
    N++,
    make_tuple(N', Tuple),
    arg(1, Tuple, Name) |
	channels_to_atom(Channels, Tuple, 2, Atom).

make_channel_list(N, Variables, ChannelList) :-

    N =< 0 :
      ChannelList = [],
      Variables = _;
    
    N > 0,
    make_tuple(N, Tuple) |
	channels_to_atom(Variables, Tuple, 1, ChannelList).

channels_to_atom(Cs, T, I, Atom) :-

    Cs ? C,
    arg(I, T, A),
    I++ :
      A = C |
	self;

    Cs = [] :
      I = _,
      Atom = T.


make_guard_receive(Channel, ChannelList, SendId, Iterates, Consume) :-
    string_to_dlist(Channel, CL, Prime) :
      Prime = [39],

      Iterates = {(`Channel = {`pifcp(id), `pifcp(cv), `pifcp(mss)},
		   `pifcp(mss) = [{`"_", `"_", `"_", `pifcp(choice)}
				 | `pifcp(mssp)],
		   not_we(`pifcp(choice)) :
		     `ChannelP = {`pifcp(id), `pifcp(cv), ?pifcp(mssp)} |
			self),
		  (`Channel = {`pifcp(id), `pifcp(cv), `pifcp(mss)},
		   `pifcp(mss) =?= [{`SendId, `"_", `"_", `pifcp(chosen)}
				   | `pifcp(mssp)] :
		     `ChannelP = {`pifcp(id), `pifcp(cv), ?pifcp(mssp)} |
			self)},

      Consume = (`Channel = {`"_", `"_", `pifcp(mss)},
		 `pifcp(mss) =?=
			[{`Sender, ChannelList, `pifcp(tag), `pifcp(choose)}
			| `"_" /*pifcp(mssp)*/],
		  ExcludeSender? :
/* This may only be done if we can handle multiple primes - e.g.
** when the message is a <channel_list> which includes  Channel .
**		    `ChannelP = {`pifcp(id), `pifcp(cv), ?pifcp(mssp)},
*/
		    CancelSends?),
      Choose = (`pifcp(choose) = `pifcp(tag)),

      TestWe = we(`pifcp(choose)) |

	list_to_string(CL, ChannelP),
	receive_sender.

  receive_sender(SendId, Sender, TestWe, ExcludeSender, Choose, CancelSends) :-

    SendId =?= "_" :
      Sender = "_",
      ExcludeSender = TestWe,
      CancelSends = Choose;

    otherwise :
      Sender = pifcp(sender),
      ExcludeSender = (`SendId =\= `Sender, TestWe),
      CancelSends = (`pifcp(chosen) = 0, Choose).


make_guard_send(ChannelName, ChannelList, Sender, SendIndex, Guard) :-

    true :
      VN = pinch(ChannelName),
      Guard = {`ChannelName = {`"_", `VN, `"_"},
		write_channel({Sender, ChannelList, SendIndex, `pifcp(chosen)},
				`VN)} .

  make_guard_sender(SendId, Name, ChannelName, Sender) :-

    SendId =?= "_" |
	make_sender;

    otherwise :
      Name = _,
      ChannelName = _,
      Sender = ?SendId.

  make_sender(Name, ChannelName, Sender) :-

    string_to_dlist(Name, NL, NC),
    string_to_dlist(ChannelName, CL, []) :
      NC = [46 | CL] |
	list_to_string(NL, Sender).


message_to_channels(Message, Name, Channels, Locals, Underscore, MChannels,
			Errors, NextErrors) + (Index = 1) :-

    Message = [] :
      Name = _,
      Channels = _,
      Locals = _,
      Underscore = _,
      Index = _,
      MChannels = [],
      Errors = NextErrors;

    arg(Index, Message, Channel), string(Channel), Channel =\= "",
    Underscore =\= true, Index++ :
      MChannels ! OkChannel? |
	verify_channel(Name, Channel, Channels, Locals, OkChannel,
			Errors, Errors'),
	self;

    arg(Index, Message, `Channel), string(Channel),
    Underscore =\= true, Index++ :
      MChannels ! OkChannel? |
	verify_channel(Name, Channel, Channels, Locals, OkChannel,
			Errors, Errors'),
	self;

    arg(Index, Message, _),
    Underscore =?= true, Index++ :
      MChannels ! "_" |
	self;

    Index > arity(Message) :
      Message = _,
      Name = _,
      Channels = _,
      Locals = _,
      Underscore = _,
      MChannels = [],
      Errors = NextErrors;

    otherwise,
    arg(Index, Message, Channel),
    Index++ :
      Errors ! (Name - invalid_channel_in_message(Message, Channel)),
      MChannels ! [] |
	self;

    otherwise :
      Channels = _,
      Locals = _,
      Underscore = _,
      Index = _,
      MChannels = [],
      Errors = [(Name - invalid_channel_list(Message)) | NextErrors].


verify_channel(Name, ChannelName, ChannelNames, Locals, OkChannelName,
		Errors, NextErrors) :-

    string(ChannelName), ChannelName =\= "_", ChannelName =\= "" |
	defined_channel;

    ChannelName = `ChannelName',
    string(ChannelName'), ChannelName' =\= "_", ChannelName' =\= "" |
	defined_channel;

    otherwise :
      ChannelNames = _,
      Locals = _,
      OkChannelName = "_",
      Errors = [Name - invalid_channel(ChannelName) | NextErrors].

  defined_channel(Name, ChannelName, ChannelNames, Locals, OkChannelName,
		Errors, NextErrors) :-

    ChannelNames ? Other, ChannelName =\= Other |
	self;

    ChannelNames ? ChannelName :
      ChannelNames' = _,
      Locals = _,
      Name = _,
      OkChannelName = ChannelName,
      Errors = NextErrors;

    ChannelNames = [], Locals =\= [] :
      ChannelNames' = Locals,
      Locals' = [] |
	self;

    otherwise :
      ChannelNames = _,
      Locals = _,
      OkChannelName = "_",
      Errors = [Name - undefined_channel(ChannelName) | NextErrors].


parse_message(Name, Channels, Message, ChannelNames, Locals, Primes,
			Errors, NextErrors) :-

    Message = [] :
      Name = _,
      Channels = _,
      ChannelNames = [],
      Locals = [],
      Primes = [],
      Errors = NextErrors;

    tuple(Message) :
      Index = 1 |
	extract_receive_channels(Index, Message, Name, Strings, ChannelNames,
					Errors, Errors'?),
	check_for_duplicates(Strings, UniqueStrings,
		{Name, duplicate_receive_channel}, Errors', NextErrors),
	instantiate_channels;

    otherwise :
      Channels = _,
      ChannelNames = [],
      Locals = [],
      Primes = [],
      Errors = [(Name - invalid_message(Message)) | NextErrors].


  extract_receive_channels(Index, Message, Name, Strings, ChannelNames,
				Errors, NextErrors) :-

    arg(Index, Message, Channel),
    string(Channel), Channel =\= "", Channel =\= "_",
    Index++ :
      Strings ! Channel,
      ChannelNames ! Channel |
	self;

    arg(Index, Message, `Channel),
    string(Channel), Channel =\= "", Channel =\= "_",
    Index++ :
      Strings ! Channel,
      ChannelNames ! Channel |
	self;

    arg(Index, Message, `"_"),
    Index++ :
      ChannelNames ! "_" |
	self;

    Index > arity(Message) :
      Name = _,
      Strings = [],
      ChannelNames = [],
      Errors = NextErrors;

    otherwise,
    arg(Index, Message, Channel),
    Index++ :
      Errors ! (Name - invalid_receive_channel(Channel)) |
	self.

  instantiate_channels(UniqueStrings, Channels, Locals, Primes) :-

    UniqueStrings ? Channel |
	locals_primes(Channel, Channels, Locals, Locals'?, Primes, Primes'?),
	self;

    UniqueStrings = [] :
      Channels = _,
      Locals = [],
      Primes = [];

    Channels = [] :
      UniqueStrings = _,
      Locals = [],
      Primes = [].

  locals_primes(Channel, Channels, Locals, NextLocals, Primes, NextPrimes) :-

    Channels ? Channel,
    string_to_dlist(Channel, CL, Prime) :
      Channels' = _,
      Prime = [39],
      Locals = NextLocals,
      Primes = [{Channel, ChannelP?} | NextPrimes] |
	list_to_string(CL, ChannelP);

    Channels ? Other, Other =\= Channel |
	self;

    Channels = [] :
      Locals = [Channel | NextLocals],
      Primes = NextPrimes.

      
make_local_call(ProcessDefinition, Locals, Primes, Body1, Body2,
		In, NextIn, Errors, NextErrors, CallDefinition) :-

    Body1 = true :
      ProcessDefinition = _,
      Locals = _,
      Primes = _,
      In = NextIn,
      Errors = NextErrors,
      Body2 = true,
      CallDefinition = [];

    Body1 =?= self,
    arg(1, ProcessDefinition, Name) :
      Body1' = `Name |
	self;

    arity(Body1) > 1, arg(1, Body1, self),
    arg(1, ProcessDefinition, Name) |
	copy_goal_args(Body1, `Name, Body1'),
	self;

    arity(Body1) > 1, arg(1, Body1, `Functor), string(Functor),
    ProcessDefinition =?= {Name, _Arity, Channels, _OuterLHS, _InnerLHS,
					_CodeTuple} |
	extract_arguments_or_substitutes(Name, Body1, Arguments, Substitutes,
						Errors, Errors'?),
	verify_call_channels(Name, Body1, Channels, Locals,
				Errors', Errors''?),
	instantiated_local_channels(Primes, Arguments?, Arguments'),
	substituted_local_channels(Primes, Substitutes?, Substitutes'),
	lookup_call_functor,
	complete_local_call;

    Body1 = `Functor,
    arg(1, ProcessDefinition, Name) :
      Locals = _,
      Primes = _,
      Arguments = [],
      Substitutes = [] |
	lookup_call_functor,
	complete_local_call;

    Body1 = _ + _ :
      CallDefinition = [] |
	make_summed_call(ProcessDefinition, Locals, Primes, Body1, Body2,
			 In, NextIn, Errors, NextErrors);

    otherwise,
    tuple(Body1),
    arity(Body1, Arity) :
      ProcessDefinition = _,
      Locals = _,
      Primes = _,
      In = NextIn,
      Errors = NextErrors,
      CallDefinition = [],
      Index = 1 |
	copy_predicates(Body1, Index, Arity, Body2);	

    otherwise,
    arg(1, ProcessDefinition, Name) :
      Locals = _,
      Primes = _,
      Body2 = true,
      In = NextIn,
      Errors = [(Name - invalid_local_call(Body1)) | NextErrors],
      CallDefinition = [].

  copy_predicates(Predicates, Index, Arity, Body) :-

    Index++ < Arity,
    arg(Index, Predicates, Predicate) :
      Body = (Predicate, Body') |
	self;

    Index =:= Arity,
    arg(Index, Predicates, Predicate) :
      Body = Predicate.

  substituted_local_channels(Primes, Substitutes, Primed) :-

    Primes ? {Channel, ChannelP} |
	prime_substitutes(Channel, ChannelP, Substitutes, Substitutes'),
	self;

    Primes =?= [] :
      Primed = Substitutes.

  prime_substitutes(Channel, ChannelP, SubsIn, SubsOut) :-

    SubsIn ? (Substitute = Sub), Sub =?= Channel :
      SubsOut ! (Substitute = ChannelP) |
	self;

    SubsIn ?  (Substitute = Sub), Sub =\= Channel :
      SubsOut !  (Substitute = Sub) |
	self;

    SubsIn =?= [] :
      Channel = _,
      ChannelP = _,
      SubsOut = [].


verify_call_channels(Name, Goal, Channels, Locals, Errors, NextErrors)
			+ (Index = 2) :-

    arg(Index, Goal, (_ = String)), string =\= "_",
    Index++ |
	verify_channel(Name, String, Channels, Locals, _, Errors, Errors'?),
	self;

    arg(Index, Goal, String), string =\= "_",
    Index++ |
	verify_channel(Name, String, Channels, Locals, _, Errors, Errors'?),
	self;

    Index =< arity(Goal),
    otherwise,
    Index++ |
	self;

    Index > arity(Goal) :
      Name = _,
      Channels = _,
      Locals = _,
      Errors = NextErrors.
    

complete_local_call(CallType, CallDefinition, Arguments, Substitutes, Name,
				Body1, Body2, Errors, NextErrors) :-

    CallType =?= none :
      CallDefinition = _,
      Arguments = _,
      Substitutes = _,
      Name = _,
      Body2 = true,
      Errors = [Name - unknown_local_process(Body1) | NextErrors];

    CallType =\= none,
    list(Arguments),
    CallDefinition =?= {_Name, Arity, _Channels, Atom, _InnerLHS,
					_CodeTuple} :
      Substitutes = _,
      Name = _,
      Body1 = _ |
	substitute_arguments+(Index = 1, Substitutes = Substitutes'),
	call_with_substitutes;

    CallType =?= outer,
    Arguments =?= [],
    CallDefinition =?= {_Name, _Arity, _Channels, Atom, _InnerLHS,
					_CodeTuple} :
      Errors = NextErrors,
      Name = _,
      Body1 = _ |
	call_with_substitutes;

    CallType =?= inner,
    Arguments =?= [],
    CallDefinition =?= {_Name, _Arity, _Channels, _OuterLHS, Atom,
					_CodeTuple} :
      Name = _,
      Body1 = _,
      Errors = NextErrors |
	call_with_substitutes.


substitute_arguments(Index, Atom, Arity, Arguments, Substitutes, Name, Body1,
			Errors, NextErrors) :-

    Index++ =< Arity,
    arg(Index', Atom, S),
    Arguments ? C :
      Substitutes ! {S, C} |
	self;

    Index > Arity,
    Arguments = [] :
      Atom = _,
      Name = _,
      Body1 = _,
      Substitutes = [],
      Errors = NextErrors;

    Index > Arity,
    Arguments =\= [] :
      Atom = _,
      Substitutes = [],
      Errors = [Name - too_many_arguments(Body1) | NextErrors];

    Index =< Arity,
    Arguments =?= [] :
      Atom = _,
      Substitutes = [],
      Errors = [Name - not_enough_arguments(Body1) | NextErrors].


call_with_substitutes(Atom, Substitutes, Body2) :-

    Substitutes =?= [],
    arg(1, Atom, Name) :
      Body2 = Name;

    Substitutes =\= [],
    arg(1, Atom, Name) :
      Body2 = Name + Added |
	add_substitutes.

  add_substitutes(Substitutes, Added) :-

    Substitutes =?= [{S, C}] :
      Added = (S = `C);

    Substitutes ? {S, C},
    Substitutes' =\= [] :
      Added = (S = `C, Added') |
	self.


lookup_call_functor(ProcessDefinition, Functor,
		CallType, CallDefinition, In, NextIn) :-

    arg(1, ProcessDefinition, Name),
    Functor =?= Name :
      CallType = inner,
      CallDefinition = ProcessDefinition,
      In = NextIn;

    otherwise :
      ProcessDefinition = _,
      In = [lookup_functor(Functor, CallType, CallDefinition) | NextIn].

extract_arguments_or_substitutes(Name, Tuple, Arguments, Substitutes,
				Errors, NextErrors) + (Index = 2, Type = _) :-

    arg(Index, Tuple, Channel), string(Channel), Channel =\= "_",
    Index++ :
      Type = channel,
      Arguments ! Channel |
	self;

    arg(Index, Tuple, `Channel), string(Channel), Channel =\= "_",
    Index++ :
      Type = channel,
      Arguments ! Channel |
	self;

    arg(Index, Tuple, S = C), string(S), S =\= "_", string(C),
    Index++ :
      Type = substitute,
      Substitutes ! {`S, C} |
	self;

    arg(Index, Tuple, S = `C), string(S), S =\= "_", string(C),
    Index++ :
      Type = substitute,
      Substitutes ! {`S, C} |
	self;

    arg(Index, Tuple, `S = C), string(S), S =\= "_", string(C),
    Index++ :
      Type = substitute,
      Substitutes ! {`S, C} |
	self;

    arg(Index, Tuple, `S = `C), string(S), S =\= "_", string(C),
    Index++ :
      Type = substitute,
      Substitutes ! {`S, C} |
	self;

    arity(Tuple) < Index :
      Name = _,
      Type = _,
      Arguments = [],
      Substitutes = [],
      Errors = NextErrors;

    otherwise,
    Index =:= 2 :
      Type = _,
      Arguments = [],
      Substitutes = [],
      Errors = [Name - first_argument_invalid(Tuple) | NextErrors];

    otherwise,
    Type = channel,
    arg(Index, Tuple, Arg),
    Index++ :
      Arguments ! "_",
      Errors ! (Name - invalid_channel_argument(Arg, Tuple)) |
	self;

    otherwise,
    Type = substitute,
    arg(Index, Tuple, Arg),
    Index++ :
      Errors ! (Name - invalid_substitute(Arg, Tuple)) |
	self.


complete_remote_call(ProcessDefinition, Call1, Arguments, Call2,
			Errors, NextErrors) :-

    arg(1, Call1, `Functor), string(Functor), Arguments =\= [] :
      ProcessDefinition = _,
      Errors = NextErrors |
	channels_to_variables(Arguments, Variables, N),
	make_lhs_tuple(N, Functor, Variables?, Call2);

    arg(1, Call1, `Functor), string(Functor), Arguments =?= [] :
      ProcessDefinition = _,
      Call2 = Functor,
      Errors = NextErrors;

    otherwise,
    arg(1, ProcessDefinition, Name) :
      Arguments = _,
      Call2 = [],
      Errors = [Name - invalid_remote_call(Call1) | NextErrors].


instantiated_local_channels(Primes, Arguments, Primed) :-

    Primes ? {Channel, ChannelP} |
	prime_arguments(Channel, ChannelP, Arguments, Arguments'),
	self;

    Primes =?= [] :
      Primed = Arguments.

 prime_arguments(Channel, ChannelP, ArgsIn, ArgsOut) :-

    ArgsIn ? Arg, Arg =?= Channel :
      ArgsOut ! ChannelP |
	self;

    ArgsIn ? Arg, Arg =\= Channel :
      ArgsOut ! Arg |
	self;

    ArgsIn =?= [] :
      Channel = _,
      ChannelP = _,
      ArgsOut = [].


/************************* Program Transformations ***************************/

program(Source, GlobalList, Exported, Exports, Terms, Errors) :-

	serve_empty_scope(Scope?, GlobalList, Exported, Exports,
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
      Scope ! atoms(OuterLHS, InnerLHS),    
      Nested ! (Atom? :- Initializer?),
      Status' = initialized |
	tuple_to_atom(OuterLHS?, Atom),
	Index := arity(OuterLHS) + 1,
	arg(1, OuterLHS, Name),
	initialize_channels(Index, InnerLHS, Name, Initializer),
	self;

    Status =\= nil, Status =\= double, /* single or initialized */
    RHSS =\= (_|_), RHSS =\= (_;_)  :
      Scope = [atoms(_OuterLHS, InnerLHS), code(no_guard, [], []) | Scope'?],
      Process = (Atom? :- RHSS'?) |
	tuple_to_atom(InnerLHS?, Atom),
	transform_body(RHSS, RHSS', Nested, [], Scope', []);

    Status =\= nil, Status =\= double, /* single or initialized */
    otherwise :
      Scope ! atoms(_OuterLHS, InnerLHS),
      Process = (Atom? :- RHSS'?) |
	tuple_to_atom(InnerLHS?, Atom),
	guarded_clauses(RHSS, RHSS', Process, Nested, Scope').

  initialize_channels(Index, Atom, Name, Initializer) +
			(MakeAll = More?, More) :-

    Index < arity(Atom),
    arg(Index, Atom, `Channel),
    Index++ :
      More = (MakeChannel?, NameChannel?, More'?) |
	make_and_name_channel,
	self;

    Index =:= arity(Atom),
    arg(Index, Atom, `Channel),
    arg(1, Atom, PName) :
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
	update_process_mode(Mode, GuardMode, Mode'),
	make_right_hand_side + (Index = 1),
	make_rhs2 + (Scope = Scope', NextScope = Scope''?),
	code_reply;

    RHS1 =?= (Guarded ; RHS1'), Guarded =?= (_|_),
    Index++ :
      NextRHSS ! Clauses? |
	guarded_clause(Guarded, GuardMode(SendId?, Index'), Clauses,
			Nested, Nested'?, Scope, Scope'?),
	update_process_mode(Mode, GuardMode, Mode'),
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

  update_process_mode(Mode, GuardMode, NewMode) :-

    GuardMode =?= none :
      NewMode = Mode;

    Mode =?= none :
      NewMode = GuardMode;

    Mode =?= GuardMode :
      NewMode = GuardMode;

    Mode =?= compare,
    GuardMode =?= otherwise :
      NewMode = compared;

    Mode =?= send,
    GuardMode =?= receive :
      NewMode = mixed;

    Mode =?= receive,
    GuardMode =?= send :
      NewMode = mixed;

    Mode =?= mixed,
    GuardMode =?= receive :
      NewMode = mixed;

    Mode =?= mixed,
    GuardMode =?= send :
      NewMode = mixed;

    otherwise :
      Mode = _,
      GuardMode = _,
      NewMode = conflict.


  make_rhs2(Mode, SendId, ClauseList, Sends, RHS2,
	SendProcedure, Scope, NextScope) :-

    Mode =?= send :
      SendId = "_",
      RHS2 = (Writes | SendChoices),
      SendProcedure = (ChoiceAtom? :- FcpClauses?),
      Scope = [atoms(OuterLHS, InnerLHS) | NextScope] |
	arg(1, OuterLHS, PName),
	make_choice_name(PName, ".sends", SendChoices),
	make_predicate_list(';', ClauseList, FcpClauses),
	sends_to_writes(Sends, Writes),
	make_choice_atom+(Name = SendChoices, ChoiceVars = [`pifcp(chosen)]);

    Mode =?= mixed :
      SendId = pifcp(sendid),
      RHS2 = (Writes | pi_monitor#unique_sender(PName?, `SendId),
			MixedChoices),
      SendProcedure = (ChoiceAtom? :- FcpClauses?),
      Scope = [atoms(OuterLHS, InnerLHS) | NextScope] |
	arg(1, OuterLHS, PName),
	make_choice_name(PName?, ".mixed", MixedChoices),
	make_predicate_list(';', ClauseList, FcpClauses),
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
	make_predicate_list(';', ClauseList, FcpClauses);

    Mode =?= compare :
      SendId = "_",
      Sends = [],
      RHS2 = FcpClauses?,
      SendProcedure = [],
      Scope = [error("missing_otherwise") | NextScope] |
	make_predicate_list(';', ClauseList, FcpClauses);

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
	make_predicate_list(',', Goals?, Body2).

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


/**** Utilities ****/

tuple_to_atom(LHS, Atom) :-

    LHS = {String} :
      Atom = String;

    otherwise :
      Atom = LHS.


concatenate_lists(Lists, Out) :-

    Lists = [List | Rest],
    List ? Item, Item =\= [] :
      Out ! Item,
      Lists' = [List' | Rest] |
	self;

    Lists ? [] |
	concatenate_lists;

    Lists =?= [] :
      Out = [].

subtract_list(List1, List2, List3) :-

    List2 ? Item |
	remove_item(Item, List1, List1'),
	self;

    List2 = [] :
      List3 = List1.


remove_item(Item, List1, List2) :-

    List1 =?= [] :
      Item = _,
      List2 = [];

    List1 ? Item |
	self;

    List1 ? Other, Other =\= Item :
      List2 ! Other |
	self.


check_for_duplicates(List1, List2, ErrorCode, Errors, NextErrors) :-

    List1 ? Item,
    Item =\= "_" :
      List2 ! Item |
	search_for_duplicate(Item, List1', List1'',
			ErrorCode, Errors, Errors'?),
	self;

    List1 ? Item,
    Item =?= "_" :
      List2 ! Item |
	self;

    List1 = [] :
      List2 = [],
      ErrorCode = _,
      Errors = NextErrors.

  search_for_duplicate(Item, ListIn, ListOut,
		ErrorCode, Errors, NextErrors) :-

    ListIn ? I,
    Item =?= I,
    ErrorCode =?= {Name, Code} :
      Errors ! (Name: Code(Item)) |
	self;

    ListIn ? I,
    Item =?= I,
    ErrorCode =?= "" |
	self;

    ListIn ? I,
    Item =\= I :
      ListOut ! I |
	self;

    ListIn =?= [] :
      Item = _,
      ErrorCode = _,
      Errors = NextErrors,
      ListOut = [].


make_predicate_list(Operator, List, Predicates) :-

    List =?= [] |
      Operator = _,
      Predicates = true;

    List ? true,
    List' =\= [] |
      self;

    List ? Predicate, Predicate =\= true,
    List' ? true :
      List''' = [Predicate | List''] |
	self;

    List ? Predicate, Predicate =\= true,
    List' =\= [], List' =\= [true | _] :
      Predicates = {Operator, Predicate, Predicates'?} |
	self;

    List =?= [Predicate] :
      Operator = _,
      Predicates = Predicate.


sort_out_duplicates(In, Out, Reply) :-

	concatenate_lists(In, List),
	ordered_merger(Merger, Reply, ok),
	utils#binary_sort_merge(List, Out, Merger).    

ordered_merger(In, Left, Right) :-
    In ? ordered_merge(In1, In2, Out) |
	ordered_merge(In1, In2, Out, Left, Left'),
	ordered_merger;

    In = [] :
      Left = Right.

ordered_merge(In1, In2, Out, Left, Right) :-

    In1 ? I1, In2 = [I2 | _],
    string(I1), string(I2),
    I1 @< I2 :
      Out ! I1 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    string(I1), string(I2),
    I2 @< I1 :
      Out ! I2 |
	ordered_merge;

    In1 = [I | _], In2 ? I :
      Right = _,
      Left = duplicate,
      Right' = _ |
	ordered_merge;

    In1 ? I1, In2 = [I2 | _],
    arg(1, I1, A1), arg(1, I2, A2),
    A1 @< A2 :
      Out ! I1 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    arg(1, I1, A1), arg(1, I2, A2),
    A2 @< A1 :
      Out ! I2 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    arg(1, I1, A), arg(1, I2, A) :
      Right = _,
      Left = duplicate,
      Right' = _ |
	ordered_merge;

    In1 = [] :
      In2 = Out,
      Left = Right ;

    In2 = [] :
      In1 = Out,
      Left = Right .

/***************** Summation Process server predicates. **********************/

make_summed_call(ProcessDefinition, Locals, Primes, Sum, Call,
			In, NextIn, Errors, NextErrors) :-

    true :
      In ! call_sum(Name?, Procedures?, Call) |
	utils#binary_sort_merge(Names?, NameList),
	concatenated_sum_name(NameList?, Name),
	summed_call(ProcessDefinition, Locals, Primes, Sum, Names, Procedures,
			In', NextIn, Errors, NextErrors).

  concatenated_sum_name(NameList, Name) + (NLH = NLT?, NLT) :-


    NameList ? N :
      ascii("+", Plus),
      NLT' ! Plus |
	string_to_dlist(N, NLT, NLT'),
	self;

    NameList =?= [N],
    string_to_dlist(N, NH, []) :
      NLT = NH |
	list_to_string(NLH, Name);

    NameList = [] :
      NLH = _,
      NLT = _,
      Name = false.


summed_call(ProcessDefinition, Locals, Primes, Sum, Names, Procedures,
		In, NextIn, Errors, NextErrors) :-

    Sum = `Name + Sum', string(Name) :
      Names ! Name,
      Procedures ! {Call?, CallDefinition?} |
	make_local_call(ProcessDefinition, Locals, Primes, `Name, Call,
			In, In', Errors, Errors'?, CallDefinition),
	self;

    Sum = Sum' + `Name, string(Name) :
      Names ! Name,
      Procedures ! {Call?, CallDefinition?} |
	make_local_call(ProcessDefinition, Locals, Primes, `Name, Call,
			In, In', Errors, Errors'?, CallDefinition),
	self;

    Sum = `Name, string(Name) :
      Names = [Name],
      Procedures = [{Call?, CallDefinition?}] |
	make_local_call(ProcessDefinition, Locals, Primes, `Name, Call,
			In, NextIn, Errors, NextErrors, CallDefinition);

    Sum = Tuple + Sum', arg(1,Tuple,`Name), string(Name) :
      Names ! Name,
      Procedures ! {Call?, CallDefinition?} |
	make_local_call(ProcessDefinition, Locals, Primes, Tuple, Call,
			In, In', Errors, Errors'?, CallDefinition),
	self;

    Sum = Sum' + Tuple, arg(1,Tuple,`Name), string(Name) :
      Names ! Name,
      Procedures ! {Call?, CallDefinition?} |
	make_local_call(ProcessDefinition, Locals, Primes, Tuple, Call,
				In, In', Errors, Errors'?, CallDefinition),
	self;

    arg(1,Sum,`Name), string(Name) :
      Names = [Name],
      Procedures = [{Call?, CallDefinition?}] |
	make_local_call(ProcessDefinition, Locals, Primes, Sum, Call,
			In, NextIn, Errors, NextErrors, CallDefinition);
    otherwise,
    arg(1, ProcessDefinition, Name) :
      Locals = _,
      Primes = _,
      Errors = [Name - illegal_process_summation(Sum) | NextErrors],
      Names = [],
      Procedures = false,
      In = NextIn.

/****************** Summation  Empty server predicates. **********************/

sum_procedures(Summed, Entries, Errors) + (Cumulated = []) :-

    Summed ? Name(Procedures, Call) |
	cumulate,
	extract_procedure_parts(Procedures, Calls, Channels, CodeTuples),
	cumulated;

    Summed = [] :
      Cumulated = _,
      Entries = [],
      Errors = [].

  extract_procedure_parts(Procedures, Calls, Channels, CodeTuples) :-

    Procedures ? {Call, ProcedureDefinition},
    ProcedureDefinition = {_Name, _Arity, PChs, _OAtom, _IAtom, CodeTuple} :
      Calls ! Call,
      Channels ! PChs,
      CodeTuples ! CodeTuple |
	self;

    Procedures ? _Ignore,
    otherwise |
	self;

    Procedures = [] :
      Calls = [],
      Channels = [],
      CodeTuples = [].

  cumulate(Name, Cumulated, Reply) :-

    Cumulated = [Name | _] :
      Reply = found;

    Cumulated ? Other,
    Other =\= Name |
	self;

    Cumulated = [] :
      Name = _,
      Reply = new.

  cumulated(Summed, Entries, Errors, Cumulated,
	Name, Calls, Channels, CodeTuples, Call, Reply) :-

    Reply =?= found :
      Channels = _,
      CodeTuples = _ |
	make_sum_call(Name, Calls, Call, Errors, Errors'?),
	sum_procedures;

    Reply =?= new :
      Cumulated' = [Name | Cumulated] |
	make_summed_rhs(Name, Calls, CodeTuples, 1, Sends, Code, FinalMode,
				Errors, Errors'?),
	sort_out_duplicates(Channels?, SumChannels, _Reply),
	make_named_list(Sends?, Writes, Name-duplicate_send_channel_in_sum,
				Errors', Errors''?),
	make_named_guard(Writes?, Ask, Tell),
	make_named_list(Code?, RHS, Name-duplicate_receive_channel_in_sum,
				Errors'', Errors'''?),
	make_named_predicates(';', RHS, RHS'),
	channels_to_variables(SumChannels, Variables, N),
	make_lhs_tuple(N?, Name, Variables, Tuple),
	make_sum_procedure(FinalMode?, Name, (Ask? : Tell?), RHS'?, Tuple?,
				Entries, Entries'?),
	make_sum_call(Name, Calls, Call, Errors''', Errors''''?),
	sum_procedures.


make_sum_call(Name, Calls, Call, Errors, NextErrors)
		+ (NamedArgs = AddArgs?, AddArgs) :-

    Calls ? String, string(String) |
	self;

    Calls ? (_Name + Arguments) |
	extract_named_arguments(Arguments, AddArgs, AddArgs'?),
	self;

    Calls = [] :
      AddArgs = [] |
	make_named_list(NamedArgs, ArgList,
		Name-duplicate_channel_substitution, Errors, NextErrors),
	complete_sum_call.

  complete_sum_call(Name, ArgList, Call) :-

    ArgList =?= [] :
      Call = Name;

    ArgList =\= [] :
      Call = Name+(Substitutes) |
	make_named_predicates(',', ArgList, Substitutes).


make_summed_rhs(Name, Calls, CodeTuples, Index, Sends, Code, FinalMode,
		Errors, NextErrors) + (Mode = none, Sender = _) :-

    CodeTuples ? ProcessMode(SendRHS, ProcessRHS),
    Calls ? _,
    SendRHS =?= (Idents : Writes | _Relay) |
	add_sends_and_receives(Idents, Writes, ProcessRHS, Sender?,
			Index, Index', Sends, Sends'?, Code, Code'?),
	update_process_mode(Mode, ProcessMode, Mode'),
	self;

    CodeTuples ? ProcessMode([], ProcessRHS), ProcessRHS =\= [],
    Calls ? _ |
	add_receives(ProcessRHS, Sender, Code, Code'?),
	update_process_mode(Mode, ProcessMode, Mode'),
	self;

    CodeTuples ? ProcessMode([], []),
    Calls ? Call :
      Errors ! (Name-invalid_mode_in_summation(Call? - ProcessMode)) |
	update_process_mode(Mode, ProcessMode, Mode'),
	self;

    CodeTuples = [] :
      Calls = _,
      Index = _,
      Sends = [],
      Code = [],
      FinalMode = Mode,
      Errors = NextErrors |
	final_process_mode(Name, Mode, Sender).

  final_process_mode(Name, Mode, Sender) :-

    Mode =?= mixed :
      Name = _,
      Sender = `pifcp(sendid);

    Mode =?= send :
      Sender = Name;

    /* receive or none */
    otherwise :
      Name = _,
      Mode = _,
      Sender = [].

  add_sends_and_receives(Idents, Writes, ProcessRHS, Sender,
	Index, NewIndex, Sends, NextSends, Code, NextCode) :-

    Idents =?= (Identify, Idents'),
    Identify = (`ChannelName = _Tuple),
    Writes =?= (Write, Writes'),
    Write =?= write_channel(_,_),
    ProcessRHS =?= (Sent, ProcessRHS'),
    Sent =?= (`pifcp(chosen) = _Index : Tell | Body),
    Index++ :
      Sends ! ChannelName(Identify, Write'?),
      Code ! Index((`pifcp(chosen) = Index : Tell | Body)) |
	reindex_write(Write, Sender, Index, Write'),
	self;

    Idents = (`ChannelName = _Tuple), Writes =?= write_channel(_,_),
    ProcessRHS =?= (`pifcp(chosen) = _Index : Tell | Body),
    Index++ :
      NewIndex = Index',
      Sends = [ChannelName(Idents, Write?) | NextSends],
      Code = [Index((`pifcp(chosen) = Index : Tell | Body)) | NextCode] |
	reindex_write(Writes, Sender, Index, Write);

    Idents = (`ChannelName = _Tuple), Writes =?= write_channel(_,_),
    ProcessRHS =?= (`pifcp(chosen) = _Index | Body),
    Index++ :
      NewIndex = Index',
      Sends = [ChannelName(Idents, Write?) | NextSends],
      Code = [Index((`pifcp(chosen) = Index | Body)) | NextCode] |
	reindex_write(Writes, Sender, Index, Write);

    ProcessRHS =?= (Receive; ProcessRHS'),
    otherwise |
	add_receives(Receive, Sender, Code, Code'?),
	self;

    otherwise :
      Writes = _,
      Idents = _,
      NewIndex = Index,
      NextSends = Sends |
	add_receives(ProcessRHS, Sender, Code, NextCode).

  reindex_write(Write, Sender, Index, NewWrite) :-

    Write = write_channel({_Sender, ChannelList, _SendIndex, Chosen}, VN) :
      NewWrite = write_channel({Sender, ChannelList, Index, Chosen}, VN).

add_receives(Receives, Sender, Code, NextCode) :-

    Receives = (Cdr ; Receives'),
    Cdr =?= ((`ChannelName = _, _) : _ | self) |
	analyze_receives.

  analyze_receives(Receives, Sender, ChannelName, Cdr, Code, NextCode) :-

    Sender =?= [],
    Receives =\= (_ ; _) :
      Code = [ChannelName(Cdr, Receives) | NextCode];

    Sender =?= [],
    Receives =?= (Consume ; Receives') :
      Code = [ChannelName(Cdr, Consume) | Code'] |
	add_receives;

    /* Otherwise, the Sum is mixed. */
    Sender =\= [],
    Receives =\= (_ ; _) :
      Code = [ChannelName(Cdr, Iterate?, Consume?) | NextCode] |
	invent_iterate(ChannelName, Receives, Iterate, Consume);

    Sender =\= [],
    Receives =?= (Consume; Receives'), Consume =\= (_ | self) :
      Code = [ChannelName(Cdr, Iterate?, Consume'?) | Code'] |
	invent_iterate(ChannelName, Consume, Iterate, Consume'),
	add_receives;

    /* The Process was already mixed. */
    Sender =\= [],
    Receives =?= (Iterate; Consume),
    Iterate =?= (_ | self), Consume =\= (_ ; _) :
      Code = [ChannelName(Cdr, Iterate, Consume) | Code'] |
	add_receives;

    Sender =\= [],
    Receives =?= (Iterate; Consume ; Receives'),
    Iterate =?= (_ | self) :
      Code = [ChannelName(Cdr, Iterate, Consume) | Code'] |
	add_receives.

  invent_iterate(ChannelName, Consume, Iterate, NewConsume) :-

    Consume = ( _ChMsg,
		_Mss =?= [{_Sender, ChannelList, _Tag, _Choose} | _],
		_We : _Tell | _Body) |
	make_guard_receive(ChannelName, ChannelList, pifcp(sendid),
					{_Cdr, Iterate}, NewConsume).

/* Compare to make_RHS2 */
make_sum_procedure(Mode, Name, Writes, RHS, Tuple, Entries, NextEntries) :-

    Mode =?= send :
      Entries = [(Atom? :- (Writes | SendChoices?)), (ChoiceAtom? :- RHS)
		| NextEntries] |
	tuple_to_atom(Tuple, Atom),
	make_choice_name(Name, ".sends", SendChoices),
	make_choice_atom(Atom, SendChoices?, [`pifcp(chosen)],
				ChoiceAtom);

    Mode =?= mixed :
      Sender = `pifcp(sendid),
      Entries = [(Atom? :- Writes? |
			pi_monitor#unique_sender(Name, Sender),
			MixedChoices?),
		 (ChoiceAtom? :- RHS?)
		| NextEntries] |
	tuple_to_atom(Tuple, Atom),
	make_choice_name(Name, ".mixed", MixedChoices),
	make_choice_atom(Atom, MixedChoices, [`pifcp(chosen), Sender],
				ChoiceAtom);

    /* receive, none */
    Mode =\= send, Mode =\= mixed :
      Name = _,
      Writes = _,
      Entries = [(Atom? :- RHS) | NextEntries] |
	tuple_to_atom(Tuple, Atom).



/************************** Summation Utilities ******************************/

make_named_guard(Writes, Ask, Tell) :-

    Writes ? _Name(Idents, Write), Writes' =\= [] :
      Ask = (Idents, Ask'?),
      Tell = (Write, Tell'?) |
	self;

    Writes = [_Name(Idents, Write)] :
      Ask = Idents,
      Tell = Write;

    Writes = [] :
      Ask = true,
      Tell = true.


make_named_list(NamedClauses, Clauses, Diagnostic, Errors, NextErrors) :-

	sort_out_duplicates([NamedClauses], Clauses, Reply),
	diagnose_duplicate.

  diagnose_duplicate(Reply, Diagnostic, Errors, NextErrors) :-

    Reply = ok :
      Diagnostic = _,
      Errors = NextErrors;

    Reply =\= ok :
      Errors = [Diagnostic | NextErrors].

make_named_predicates(Operator, List, PredicateList) :-

    List =?= [] :
      Operator = _,
      PredicateList = true;

    List ? Name(Predicate1, Predicate2, Predicate3) :
      PredicateList = {Operator, Predicate1,
				{Operator, Predicate2, PredicateList'}},
      List'' = [Name(Predicate3) | List'] |
	self;

    List ? Name(Predicate1, Predicate2) :
      PredicateList = {Operator, Predicate1, PredicateList'},
      List'' = [Name(Predicate2) | List'] |
	self;

    List ? _Name(Predicate),
    List' =\= [] :
      PredicateList = {Operator, Predicate, PredicateList'?} |
	self;

    List =?= [_Name(Predicate)] :
      Operator = _,
      PredicateList = Predicate.

extract_named_arguments(Arguments, Args, NextArgs) :-

    Arguments = (Substitution, Arguments'),
    Substitution =?= (`Name = _Value) :
      Args ! Name(Substitution) |
	self;

    Arguments =\= (_ , _),
    Arguments =?= (`Name = _Value) :
      Args = [Name(Arguments) | NextArgs].
