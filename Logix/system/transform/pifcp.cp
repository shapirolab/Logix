/*
Precompiler for Pi Calculus procedures.

Bill Silverman, December 1999.

Last update by		$Author: bill $
		       	$Date: 2000/01/27 12:17:27 $
Currently locked by 	$Locker:  $
			$Revision: 1.3 $
			$Source: /home/qiana/Repository/Logix/system/transform/Attic/pifcp.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([transform/5]).
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

	/* Exclude declared export from Attributes. */ 
	/* Extract (all) Global channel declarations */
	filter_attributes(Attributes1, [], GlobalList, Attributes1', Exported,
				Errors, Errors'?),
	Attributes2 = [export(Exports?) | Attributes1'?],
	program.

  filter_attributes(In, GlobalList, NewGlobalList, Out, Exported,
			Errors, NextErrors) :-

    In ? export(Es) :
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

    GlobalList =?= [] :
      Tail = Old,
      Errors = NextErrors |
	utils#binary_sort_merge(Head, New).


/*
** serve_empty_scope/6+3
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
**               CallDefinition = {Name, Arity, Channels, OuterAtom, InnerAtom)
**
**      process(LHS, ProcessScope)
**
**          Analyze LHS (Left-hand-side) of process declaration and return
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
		(Progeny = [], Refs = AddRef?, AddRef) :-

    In ? error(Error) :
      Errors ! Error |
	self;

    In ? lookup_functor(Functor, CallType, CallDefinition) :
      AddRef ! lookup_functor(Functor, CallType, CallDefinition) |
	self;

    In ? process(LHS, Status, ProcessScope) |
	make_process_scope(LHS, ProcessScope, [], In'', In'?,
		NewDefinition?, ProcessDefinition, Errors, Errors'?),
	compute_status,
	export_process(ProcessDefinition?, Exported, Export,
				Exports, Exports'?),
	create_entry(GlobalList, Export?, ProcessDefinition?, NewDefinition,
			Entries, Entries'?),
	add_process_definition(NewDefinition?, Progeny, Progeny'),
	self;

    In = [] :
      GlobalList = _,
      Exported = _,
      AddRef = [],
      Entries = [],
      Errors = [],
      Exports = [] |
	find_process_refs(Refs, Progeny, [], []).


create_entry(GlobalList, Export, ProcessDefinition, NewDefinition,
		Entries, NextEntries) :-


    ProcessDefinition =?= {Name, Arity, Channels, OuterAtom, _InnerAtom},
    Name =\= "_",
    GlobalList =\= [],
    Export = true,
    Index := arity(OuterAtom),
    Index++,
    string_to_dlist(Name, NL, []) :
      ascii(' ', Space),
      Entries ! (OuterAtom :- Initializer?),
      NextEntries = Entries',
      NewDefinition = {Name, Arity, Channels'?, OuterAtom'?, InnerAtom'?} |
	list_to_string([Space|NL], Name'),
	split_channels(1, Index, Channels, ParamList, ChannelList),
	construct_lhs_atoms,
	initialize_channels(Index', OuterAtom'?, global, Initializer);

    ProcessDefinition =?= {Name, Arity, Channels, OuterAtom, _InnerAtom},
    Name =\= "_",
    GlobalList =\= [],
    Export = false,
    Index := arity(OuterAtom) :
      NextEntries = Entries,
      NewDefinition = {Name, Arity, Channels'?, OuterAtom'?, InnerAtom'?} |
	split_channels(1, Index, Channels, ParamList, ChannelList),
	construct_lhs_atoms;
	
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
**                            OuterAtom and InnerAtom.
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
**               CallDefinition = {Name, Arity, Channels, OuterAtom, InnerAtom)
**
**      process(LHS, ProcessScope)
**
**          Analyze LHS (Left-hand-side) of process declaration and return
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
    ProcessDefinition =?= {_Name, _Arity, _Channels, OuterAtom, InnerAtom} :
      Outer = OuterAtom,
      Inner = InnerAtom |
	self;

    In ? body_receive(Channel, Message, ChannelVar, ChannelList),
    ProcessDefinition =?= {Name, _Arity, Channels, _OuterAtom, _InnerAtom} :
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
    ProcessDefinition =?= {Name, _Arity, Channels, _OuterAtom, _InnerAtom} :
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
	make_local_call(ProcessDefinition, Primes, Body1, Body2,
			In'', In'?, Errors, Errors'?),
	self;

    In ? channels(Cs),
    ProcessDefinition =?= {_Name, _Arity, Channels, _OuterAtom, _InnerAtom} :
      Cs = Channels |
	self;

    In ? error(Description),
    arg(1, ProcessDefinition, Name) :
      Errors ! (Name - Description) |
	self;

    In ? guard_compare(Guard, ChannelList, NextChannelList, Comparer),
    Guard =?= {Operator, C1, C2},
    ProcessDefinition =?= {Name, _Arity, Channels, _OuterAtom, _InnerAtom} |
	message_to_channels({C1, C2}, Name, Channels, Locals, false, List,
				Errors, Errors'?),
	compare_channels_ok,
	self;

    In ? guard_receive(Channel, Message, SendId, Iterates, Consume),
    ProcessDefinition =?= {Name, _Arity, Channels, _OuterAtom, _InnerAtom} :
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
    ProcessDefinition =?= {Name, _Arity, Channels, _OuterAtom, _InnerAtom} |
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

    In ? process(LHS, Status, ProcessScope),
    ProcessDefinition =?= {_Name, _Arity, Channels, _OuterAtom, _InnerAtom} |
	concatenate(Locals, Channels, GlobalList),
	make_process_scope(LHS, ProcessScope, GlobalList,
		In'', In'?, NewDefinition?, NewDefinition, Errors, Errors'?),
	compute_status,
	add_process_definition(NewDefinition?, Progeny, Progeny'),
	self;

    In ? remote_call(Call1, Call2),
    arg(1, ProcessDefinition, Name) |
	extract_arguments_or_substitutes(Name, Call1, Arguments, _Substitutes,
			Errors, Errors'?, 2, channel),
	instantiated_local_channels(Primes, Arguments?, Arguments'),
	complete_remote_call(ProcessDefinition, Call1, Arguments'?, Call2,
				Errors', Errors''?),
	self;

    In = [] :
      ProcessDefinition = _,
      IdIndex = _,
      Locals = _,
      Primes = _,
      AddRef = [],
      Errors = NextErrors |
	find_process_refs(Refs, Progeny, Out, NextOut).

  compare_channels_ok(Operator, List, Comparer,
			ChannelList, NextChannelList) :-

    List =?= [C1, C2], C1 =\= "_", C2 =\= "_",
    string_to_dlist(C1, CL1, []),
    string_to_dlist(C2, CL2, []) :
      Compare = {Operator, `FcpChannel1?, `FcpChannel2?} |
	list_to_string([86 | CL1], FcpChannel1),
	list_to_string([86 | CL2], FcpChannel2),
	compare_channel(C1, FcpChannel1?, Extract1, ChannelList, ChannelList'),
	compare_channel(C2, FcpChannel2?, Extract2,
			ChannelList'?, NextChannelList),
	make_comparer;

    otherwise :
      Operator = _,
      List = _,
      NextChannelList = ChannelList,
      Comparer = true.

  compare_channel(C, FC, Extract, CL, NCL) :-

    CL ? Ch, C =\= Ch :
      NCL ! Ch |
	self;

    CL ? Ch, C =?= Ch :
      FC = _,
      CL' = _,
      Extract = true,
      NCL = CL;

    CL = [] :
      Extract = (`C = {`"_", `FC, `"_"}),
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
** Analyze LHS, producing a ProcessDefinition and a process scope server,
** serve_process_scope.
**
** Input:
**
**   LHS is from a process/2 request.
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

make_process_scope(LHS, ProcessScope, GlobalList,
	Out, NextOut, NewDefinition, ProcessDefinition, Errors, NextErrors) :-

    true :
      ProcessDefinition =?= {Name?, Arity?, Channels?, OuterAtom?, InnerAtom?}|
	parse_lhs(LHS, Name, Arity, ParamList, ChannelList, Errors, Errors'?),
	check_for_duplicates(ParamList?, ParamList1,
				{Name?, duplicate_parameter},
					Errors', Errors''?),
	check_for_duplicates(ChannelList?, ChannelList1,
				{Name?, duplicate_channel},
					Errors'', Errors'''?),
	concatenate(ParamList1?, ChannelList1?, LocalList),
	check_for_duplicates(LocalList?, LocalList1,
				{Name?, channel_duplicates_parameter},
					Errors''', Errors''''?),
	correct_for_duplication(LocalList?, LocalList1?, ParamList,
				ChannelList1?, ChannelList2),
	construct_lhs_atoms(Name?, ParamList1?, GlobalList, ChannelList2?,
				Channels, OuterAtom, InnerAtom),
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

    NewDefinition =?= {_Name, _Arity, _Channels, OuterAtom, _InnerAtom},
    OuterAtom =?= [] :
      Status = nil;    

    NewDefinition =?= {_Name, _Arity, _Channels, OuterAtom, InnerAtom},
    OuterAtom =\= [], OuterAtom =?= InnerAtom :
      Status = single;

    otherwise :
      NewDefinition = _,
      Status = double.

export_process(ProcessDefinition, Exported, Export, Exports, NextExports) :-

    ProcessDefinition =?= {Name, Arity, _Channels, _OuterAtom, _InnerAtom},
    Name =\= "_",
    Exported =?= all :
      Export = true,
      Exports ! (Name/Arity),
      NextExports = Exports';

    ProcessDefinition =?= {Name, Arity, _Channels, _OuterAtom, _InnerAtom},
    Name =\= "_" |
	exported_procedure;	

    otherwise :
      ProcessDefinition = _,
      Exported = _,
      Export = _,
      Exports = NextExports.

  exported_procedure(Name, Arity, Exported, Export, Exports, NextExports) :-

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

    ProcessDefinition =?= {Name, _Arity, _Channels, _OuterAtom, _InnerAtom},
    Name =\= "_" :
      NewProgeny = [ProcessDefinition | Progeny];

    otherwise :
      ProcessDefinition = _,
      NewProgeny = Progeny.


parse_lhs(LHS, Name, Arity, ParamList, ChannelList, Errors, NextErrors) :-

    LHS =?= `Functor, string(Functor), Functor =\= "" :
      Name = Functor, Arity = 0,
      ParamList = [],
      Errors = NextErrors |
	unify_without_failure(ChannelList, []);

    LHS = LHS' + Channels,
    writable(ChannelList) :
      ChannelList' = ChannelList? |
	extract_channel_list(Channels, ChannelList, Errors, Errors'),
	self;

    arg(1, LHS, `Functor), string(Functor), Functor =\= "",
    arity(LHS, A), A-- :
      Name = Functor,
      Arity = A' |
	extract_arglist(LHS, ParamList, Errors, NextErrors),
	unify_without_failure(ChannelList, []);

    otherwise :
      Errors ! improperly_formed_left_hand_side(LHS),
      LHS' = `"_" |
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


extract_arglist(LHS, ParamList, Errors, NextErrors) + 
			(Index = 2) :-
    arity(LHS) < Index :
      ParamList = [],
      Errors = NextErrors;

    arity(LHS) >= Index,
    arg(Index, LHS, ChannelName), Index++,
    string(ChannelName), ChannelName =\= "" :
      ParamList ! ChannelName |
	self;

    arity(LHS) >= Index,
    arg(Index, LHS, `ChannelName), Index++,
    string(ChannelName), ChannelName =\= "" :
      ParamList ! ChannelName |
	self;

    otherwise,
    arg(Index, LHS, NotString), Index++ :
      Errors ! invalid_parameter(NotString) |
	self.


construct_lhs_atoms(Name, ParamList, GlobalList, ChannelList,
			Channels, OuterAtom, InnerAtom) :-

    Name =?= "_" :
      ParamList = _,
      GlobalList = _,
      ChannelList = _,
      OuterAtom = [],
      InnerAtom = [],
      Channels = [];

    Name =\= "_",
    ChannelList =?= [] :
      InnerAtom = OuterAtom?|
	subtract_list(GlobalList, ParamList, GlobalList1),
	concatenate(ParamList, GlobalList1?, Channels),
	construct_atom(Name, "", Channels?, OuterAtom);

    Name =\= "_",
    ChannelList =\= [],
    GlobalList =?= [] |
	construct_atom(Name, "", ParamList, OuterAtom),
	concatenate(ParamList, ChannelList?, Channels),
	construct_atom(Name, ".", Channels?, InnerAtom);

    Name =\= "_",
    ChannelList =\= [],
    GlobalList =\= [] |
	subtract_list(GlobalList, ParamList, GlobalList1),
	subtract_list(GlobalList1, ChannelList, GlobalList2),
	concatenate(ParamList, GlobalList2?, OuterList),
	construct_atom(Name, "", OuterList?, OuterAtom),
	concatenate(OuterList?, ChannelList, Channels),
	construct_atom(Name, ".", Channels?, InnerAtom).


construct_atom(Name, Suffix, Channels, Atom) :-

    string_to_dlist(Name, NameCs, NameEnd),
    string_to_dlist(Suffix, SuffixCs, []) :
      NameEnd = SuffixCs |
	list_to_string(NameCs, Name'),
	channels_to_variables(Channels, Channels', Count),
	make_atom(Count?, Name', Channels'?, Atom).

channels_to_variables(ChannelNames, Variables, Count) +
				(Counter = 0) :-
    ChannelNames ? ChannelName,
    Counter++ :
      Variables ! `ChannelName |
	self;

    ChannelNames =?= [] :
      Variables = [],
      Count = Counter.

make_atom(N, Name, Channels, Atom) :-

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

      
make_local_call(ProcessDefinition, Primes, Body1, Body2,
		In, NextIn, Errors, NextErrors) :-

    Body1 = true :
      ProcessDefinition = _,
      Primes = _,
      In = NextIn,
      Errors = NextErrors,
      Body2 = true;

    arity(Body1) > 1, arg(1, Body1, `Functor), string(Functor),
    arg(1, ProcessDefinition, Name) |
	extract_arguments_or_substitutes(Name, Body1, Arguments, Substitutes,
						Errors, Errors'?),
	instantiated_local_channels(Primes, Arguments?, Arguments'),
	substituted_local_channels(Primes, Substitutes?, Substitutes'),
	lookup_call_functor,
	complete_local_call;

    Body1 = `Functor,
    arg(1, ProcessDefinition, Name) :
      Primes = _,
      Arguments = [],
      Substitutes = [] |
	lookup_call_functor,
	complete_local_call;

    otherwise,
    tuple(Body1),
    arity(Body1, Arity) :
      ProcessDefinition = _,
      Primes = _,
      In = NextIn,
      Errors = NextErrors,
      Index = 1 |
	copy_predicates(Body1, Index, Arity, Body2);	

    otherwise,
    arg(1, ProcessDefinition, Name) :
      Primes = _,
      Body2 = true,
      Errors = [(Name - invalid_local_call(Body1)) | NextErrors],
      In = NextIn.

  copy_predicates(Predicates, Index, Arity, Body) :-

    Index++ < Arity,
    arg(Index, Predicates, Predicate) :
      Body = (Predicate, Body') |
	self;

    Index =:= Arity,
    arg(Index, Predicates, Predicate) :
      Body = Predicate.


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
    CallDefinition =?= {_Name, Arity, _Channels, Atom, _InnerAtom} :
      Substitutes = _,
      Name = _,
      Body1 = _ |
	substitute_arguments+(Index = 1, Substitutes = Substitutes'),
	call_with_substitutes;

    CallType =?= outer,
    Arguments =?= [],
    CallDefinition =?= {_Name, _Arity, _Channels, Atom, _InnerAtom} :
      Errors = NextErrors,
      Name = _,
      Body1 = _ |
	call_with_substitutes;

    CallType =?= inner,
    Arguments =?= [],
    CallDefinition =?= {_Name, _Arity, _Channels, _OuterAtom, Atom} :
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
	make_atom(N, Functor, Variables?, Call2);

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


/************************* Program Transformations ***************************/

program(Source, GlobalList, Exported, Exports, Terms, Errors) :-

	serve_empty_scope(Scope?, GlobalList, Exported, Exports,
				NextTerms, Errors),
	process_definitions+(Processes = [], NextScope = []).

process_definitions(Source, Processes, Terms, NextTerms, Scope, NextScope) :-

    Source ? (LHS :- RHSS) :
      Scope ! process(LHS, Status, ProcessScope) |
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
      Scope ! atoms(OuterAtom, InnerAtom),    
      Nested ! (OuterAtom? :- Initializer?),
      Status' = initialized |
	Index := arity(OuterAtom) + 1,
	arg(1, OuterAtom, Name),
	initialize_channels(Index, InnerAtom, Name, Initializer),
	self;

    Status =\= nil, Status =\= double, /* single or initialized */
    RHSS =\= (_|_), RHSS =\= (_;_)  :
      Scope ! atoms(_OuterAtom, InnerAtom),
      Process = (InnerAtom? :- RHSS'?) |
	transform_body(RHSS, RHSS', Nested, [], Scope', []);

    Status =\= nil, Status =\= double, /* single or initialized */
    otherwise :
      Scope ! atoms(_OuterAtom, InnerAtom),
      Process = (InnerAtom? :- RHSS'?) |
	guarded_clauses(RHSS, RHSS', Nested, Scope').

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

guarded_clauses(RHS1, RHS2, Nested, Scope) +
		(Mode = none, SendId = _, NextNested = [], NextScope = [],
			 Index = 0, NextRHSS, RHSS = NextRHSS?,
			 NextSends = [], FinalMode = _) :-

    RHS1 =?= (_ | _),
    Index++ :
      NextRHSS = [Clauses?],
      FinalMode = Mode'? |
	guarded_clause(RHS1, GuardMode(SendId?, Index'), Clauses,
			Nested, Nested'?, Scope, Scope'?),
	update_process_mode(Mode, GuardMode, Mode'),
	make_right_hand_side + (Index = 1),
	make_rhs2;

    RHS1 =?= (Guarded ; RHS1'), Guarded =?= (_|_),
    Index++ :
      NextRHSS ! Clauses? |
	guarded_clause(Guarded, GuardMode(SendId?, Index'), Clauses,
			Nested, Nested'?, Scope, Scope'?),
	update_process_mode(Mode, GuardMode, Mode'),
	self;

    otherwise :
      Index = _,
      Scope ! error(invalid_guarded_clause(RHS1)),
      NextRHSS = [],
      FinalMode = none |
	make_right_hand_side + (Index = 1),
	make_rhs2.

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

    otherwise :
      Mode = _,
      GuardMode = _,
      NewMode = conflict.


  make_rhs2(Mode, SendId, ClauseList, Sends, RHS2,
	Nested, NextNested, Scope, NextScope) :-

    Mode =?= send :
      SendId = "_",
      RHS2 = (Writes | SendChoices),
      Nested = [(ChoiceAtom? :- FcpClauses?) | NextNested],
      Scope = [atoms(OuterAtom, InnerAtom) | NextScope] |
	arg(1, OuterAtom, PName),
	make_choice_name(PName, ".sends", SendChoices),
	make_predicate_list(';', ClauseList, FcpClauses),
	sends_to_writes(Sends, Writes),
	make_choice_atom+(Name = SendChoices, ChoiceVars = [`pifcp(chosen)]);

    Mode =?= mixed :
      SendId = pifcp(sendid),
      RHS2 = (Writes | pi_monitor#unique_sender(PName?, `SendId),
			MixedChoices),
      Nested = [(ChoiceAtom? :- FcpClauses?) | NextNested],
      Scope = [atoms(OuterAtom, InnerAtom) | NextScope] |
	arg(1, OuterAtom, PName),
	make_choice_name(PName?, ".mixed", MixedChoices),
	make_predicate_list(';', ClauseList, FcpClauses),
	sends_to_writes(Sends, Writes),
	make_choice_atom+(Name = MixedChoices,
			ChoiceVars = [`pifcp(chosen), `SendId]);

    /* receive, compared, logix, none */
    Mode =\= conflict, Mode =\= compare :
      Sends = [],
      SendId = "_",
      RHS2 = FcpClauses?,
      Nested = NextNested,
      Scope = NextScope |
	make_predicate_list(';', ClauseList, FcpClauses);

    Mode =?= compare :
      SendId = "_",
      Sends = [],
      RHS2 = FcpClauses?,
      Nested = NextNested,
      Scope = [error("missing_otherwise") | NextScope] |
	make_predicate_list(';', ClauseList, FcpClauses);

    Mode =?= conflict :
      SendId = "_",
      ClauseList = [],
      Sends = [],
      RHS2 = true,
      Nested = NextNested,
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

  make_choice_atom(InnerAtom, Name, ChoiceVars, ChoiceAtom) :-
	utils#tuple_to_dlist(InnerAtom, [_|Channels], ChoiceVars),
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
	process_definitions([(LHS :- Body)], Processes, Nested, NextNested,
				Scope', NextScope).

  make_new_lhs(Id, Channels, LHS) :-

    Channels =?= [] :
      LHS = `Id;

    Channels =\= [] :
      LHS = `Id + Channels.


parse_remote_call(Call1, Call2, Scope, NextScope) :-

    Call1 =?= Name # Call1', string(Name) :
      Call2 = Name # Call2' |
	self;

    Call1 =\= _ # _,
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


concatenate(List1, List2, List3) :-

    List1 ? Item :
      List3 ! Item |
	self;

    List1 =?= [] :
      List3 = List2.

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
