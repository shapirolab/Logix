/*
Precompiler for Pi Calculus procedures - call management.

Bill Silverman, December 1999.

Last update by		$Author: bill $
		       	$Date: 2000/04/06 08:42:13 $
Currently locked by 	$Locker:  $
			$Revision: 1.8 $
			$Source: /home/qiana/Repository/PiFcp/pifcp/call.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language(compound).
-export([make_local_call/10, make_remote_call/8,
	 prime_local_channels/3, sum_procedures/3]).
-mode(interpret).
      
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
    ProcessDefinition =?= {Name, _Arity, ChannelNames, _OuterLHS, _InnerLHS,
					_CodeTuple} |
	extract_arguments_or_substitutes(Name, Body1, Arguments, Substitutes,
						Errors, Errors'?),
	verify_call_channels(Name, Body1, ChannelNames, Locals,
				Errors', Errors''?),
	prime_local_channels(Primes, Arguments?, Arguments'),
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


verify_call_channels(Name, Goal, ChannelNames, Locals, Errors, NextErrors)
			+ (Index = 2) :-

    arg(Index, Goal, (_ = String)), string =\= "_",
    Index++ |
	piutils#verify_channel(Name, String, ChannelNames, Locals, _OkChannel,
				Errors, Errors'?),
	self;

    arg(Index, Goal, String), string =\= "_",
    Index++ |
	piutils#verify_channel(Name, String, ChannelNames, Locals, _OkChannel,
				Errors, Errors'?),
	self;

    Index =< arity(Goal),
    otherwise,
    Index++ |
	self;

    Index > arity(Goal) :
      Name = _,
      ChannelNames = _,
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
    CallDefinition =?= {_Name, Arity, _ChannelNames, Atom, _InnerLHS,
					_CodeTuple} :
      Substitutes = _,
      Name = _,
      Body1 = _ |
	substitute_arguments+(Index = 1, Substitutes = Substitutes'),
	call_with_substitutes;

    CallType =?= outer,
    Arguments =?= [],
    CallDefinition =?= {_Name, _Arity, _ChannelNames, Atom, _InnerLHS,
					_CodeTuple} :
      Errors = NextErrors,
      Name = _,
      Body1 = _ |
	call_with_substitutes;

    CallType =?= inner,
    Arguments =?= [],
    CallDefinition =?= {_Name, _Arity, _ChannelNames, _OuterLHS, Atom,
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


make_remote_call(Name, PiCall, ChannelNames, Locals, Primes, CompoundCall,
				Errors, NextErrors) :-

	extract_arguments_or_substitutes(Name, PiCall, Arguments, _Subs,
			Errors, Errors'?, 2, channel),
	verify_call_channels(Name, PiCall, ChannelNames, Locals,
				Errors', Errors''?),
	prime_local_channels(Primes, Arguments?, Arguments'),
	complete_remote_call(Name, PiCall, Arguments'?, CompoundCall,
				Errors'', NextErrors).


complete_remote_call(Name, PiCall, Arguments, CompoundCall,
			Errors, NextErrors) :-

    arg(1, PiCall, `Functor), string(Functor), Arguments =\= [] :
      Name = _,
      Errors = NextErrors |
	piutils#make_lhs_tuple(Functor, Arguments, CompoundCall);

    arg(1, PiCall, `Functor), string(Functor), Arguments =?= [] :
      Name = _,
      CompoundCall = Functor,
      Errors = NextErrors;

    otherwise :
      Arguments = _,
      CompoundCall = [],
      Errors = [Name - invalid_remote_call(PiCall) | NextErrors].


prime_local_channels(Primes, Arguments, Primed) :-

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
    ProcedureDefinition =?= {_Name, _Arity, ChannelNames, _OuterLHS, _InnerLHS,
				CodeTuple} :
      Calls ! Call,
      Channels ! ChannelNames,
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
	make_summed_rhs(Name, Calls, CodeTuples, 1, Sends, Code, Cdrs,
				FinalMode, Errors, Errors'?),
	piutils#sort_out_duplicates(Channels?, SumChannels, _Reply),
	make_named_list(Sends?, Writes, Name-duplicate_send_channel_in_sum,
				Errors', Errors''?),
	make_named_guard(Writes?, Ask, Tell),
	make_named_list(Code?, Code', Name-duplicate_receive_channel_in_sum,
				Errors'', Errors'''?),
	piutils#sort_out_duplicates([Cdrs], Cdrs', _),
	piutils#concatenate_lists([Code'?, Cdrs'?], RHS),
	make_named_predicates(';', RHS, RHS'),
	piutils#make_lhs_tuple(Name, SumChannels, Tuple),
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


make_summed_rhs(Name, Calls, CodeTuples, Index, Sends, Code, Cdrs, FinalMode,
		Errors, NextErrors) + (Mode = none, Sender = _) :-

    CodeTuples ? ProcessMode(SendRHS, ProcessRHS),
    Calls ? _,
    SendRHS =?= (Idents : Writes | _Relay) |
	add_sends_and_receives(Idents, Writes, ProcessRHS, Sender?,
		Index, Index', Sends, Sends'?, Code, Code'?, Cdrs, Cdrs'),
	piutils#update_process_mode(Mode, ProcessMode, Mode'),
	self;

    CodeTuples ? ProcessMode([], ProcessRHS), ProcessRHS =\= [],
    Calls ? _ |
	add_receives(ProcessRHS, Sender, Code, Code'?, Cdrs, Cdrs'),
	piutils#update_process_mode(Mode, ProcessMode, Mode'),
	self;

    CodeTuples ? ProcessMode([], []),
    Calls ? Call :
      Errors ! (Name-invalid_mode_in_summation(Call? - ProcessMode)) |
	piutils#update_process_mode(Mode, ProcessMode, Mode'),
	self;

    CodeTuples = [] :
      Calls = _,
      Index = _,
      Sends = [],
      Code = [],
      Cdrs = [],
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
	Index, NewIndex, Sends, NextSends, Code, NextCode, Cdrs, NextCdrs) :-

    Idents =?= (Identify, Idents'),
    Identify = (`ChannelName = _Tuple),
    Writes =?= (Write, Writes'),
    Write =?= write_channel(_,_),
    ProcessRHS =?= (Sent ; ProcessRHS'),
    Sent =?= (`pifcp(chosen) = _Index : Tell | Body),
    Index++ :
      Sends ! ChannelName(Identify, Write'?),
      Code ! Index((`pifcp(chosen) = Index : Tell | Body)) |
	reindex_write(Write, Sender, Index, Write'),
	self;

    Idents =?= (Identify, Idents'),
    Identify = (`ChannelName = _Tuple),
    Writes =?= (Write, Writes'),
    Write =?= write_channel(_,_),
    ProcessRHS =?= (Sent ; ProcessRHS'),
    Sent =?= (`pifcp(chosen) = _Index | Body),
    Index++ :
      Sends ! ChannelName(Identify, Write'?),
      Code ! Index((`pifcp(chosen) = Index | Body)) |
	reindex_write(Write, Sender, Index, Write'),
	self;

    Idents = (`ChannelName = _Tuple), Writes =?= write_channel(_,_),
    ProcessRHS =?= ((`pifcp(chosen) = _Index : Tell | Body) ; Receives),
    Index++ :
      Sends ! ChannelName(Idents, Write?),
      NewIndex = Index',
      NextSends = Sends',
      Code  ! Index((`pifcp(chosen) = Index : Tell | Body)) |
	reindex_write(Writes, Sender, Index, Write),
	add_receives;

    Idents = (`ChannelName = _Tuple), Writes =?= write_channel(_,_),
    ProcessRHS =?= (`pifcp(chosen) = _Index : Tell | Body),
    Index++ :
      NewIndex = Index',
      Sends = [ChannelName(Idents, Write?) | NextSends],
      Code = [Index((`pifcp(chosen) = Index : Tell | Body)) | NextCode],
      Cdrs = NextCdrs |
	reindex_write(Writes, Sender, Index, Write);

    Idents = (`ChannelName = _Tuple), Writes =?= write_channel(_,_),
    ProcessRHS =?= ((`pifcp(chosen) = _Index | Body) ; Receives),
    Index++ :
      Sends ! ChannelName(Idents, Write?),
      NewIndex = Index',
      NextSends = Sends',
      Code ! Index((`pifcp(chosen) = Index | Body)) |
	reindex_write(Writes, Sender, Index, Write),
	add_receives;

    Idents = (`ChannelName = _Tuple), Writes =?= write_channel(_,_),
    ProcessRHS =?= (`pifcp(chosen) = _Index | Body),
    Index++ :
      NewIndex = Index',
      Sends = [ChannelName(Idents, Write?) | NextSends],
      Code = [Index((`pifcp(chosen) = Index | Body)) | NextCode],
      Cdrs = NextCdrs |
	reindex_write(Writes, Sender, Index, Write);

    Idents = [] :
      ProcessRHS = _,
      Sender = _,
      Writes = _,
      Idents = _,
      NewIndex = Index,
      NextSends = Sends,
      Code = NextCode,
      Cdrs = NextCdrs;

    otherwise,
    ProcessRHS = (Cdr ; Receive),
    Cdr =?= ((Assign, _) : _ | self) |
	analyze_receive_or_cdr(Receive, Sender, Assign,
			Cdr, Code, Code',Cdrs, Cdrs', ProcessRHS'),
	self.

  reindex_write(Write, Sender, Index, NewWrite) :-

    Write = write_channel({_Sender, ChannelList, _SendIndex, Chosen}, VN) :
      NewWrite = write_channel({Sender, ChannelList, Index, Chosen}, VN).


add_receives(Receives, Sender, Code, NextCode, Cdrs, NextCdrs) :-

    Receives = (Cdr ; Receive),
    Cdr =?= (Assign, _ : _ | self) |
	analyze_receive_or_cdr(Receive, Sender, Assign, Cdr,
				Code, Code', Cdrs, Cdrs', Receives'),
	self;

    Receives =?= (`ChannelName = _, _ : _ | self) :
      Sender = _,
      Code = NextCode,
      Cdrs = [ChannelName(Receives) | NextCdrs];

    Receives =?= [] :
      Sender = _,
      Code = NextCode,
      Cdrs = NextCdrs.


analyze_receive_or_cdr(Receive, Sender, Assign, Cdr,
		Code, NextCode, Cdrs, NextCdrs, NextRHS) :-

    Assign =?= (`ChannelName = {`picdr(_), _, _, _, _}) :
      Sender = _,
      Cdrs = [ChannelName(Cdr) | NextCdrs],
      Code = NextCode,
      NextRHS = Receive;

    otherwise,
    Assign = (`ChannelName = _) :
      Cdrs = NextCdrs |
	analyze_receive.

analyze_receive(Receive, Sender, ChannelName, Cdr, Code, NextCode, NextRHS) :-

    Receive =\= (_ ; _),
    Sender =?= [] :
      Code = [ChannelName(Cdr, Receive) | NextCode],
      NextRHS = [];

    Receive =?= (Consume ; Receive'), Consume =\= (_ | self),
    Sender =?= [] :
      Code = [ChannelName(Cdr, Consume) | NextCode],
      NextRHS = Receive';

    /* The summation is mixed. */
    Receive =\= (_ ; _),
    Sender =\= [] :
      Code = [ChannelName(Cdr, Iterate?, Consume?) | NextCode],
      NextRHS = [] |
	invent_iterate(ChannelName, Receive, Iterate, Consume);

    Receive =?= (Consume; Receive'), Consume =\= (_ | self),
    Sender =\= [] :
      Code = [ChannelName(Cdr, Iterate?, Consume'?) | NextCode],
      NextRHS = Receive'|
	invent_iterate(ChannelName, Consume, Iterate, Consume');

    /* This Process was already mixed. */
    Receive =?= (Iterate; Consume),
    Iterate =?= (_ | self), Consume =\= (_ ; _) :
      Sender = _,
      Code = [ChannelName(Cdr, Iterate, Consume) | NextCode],
      NextRHS = [];

    Receive =?= (Iterate; Consume ; Receive'),
    Iterate =?= (_ | self) :
      Sender = _,
      Code = [ChannelName(Cdr, Iterate, Consume) | NextCode],
      NextRHS = Receive'.

  invent_iterate(ChannelName, Consume, Iterate, NewConsume) :-

    Consume = ( _ChMsg,
		_Mss =?= [{_Sender, ChannelList, _Tag, _Choose} | _],
		_We : _Tell | Body ) :
      NewConsume = (Consume'? | Body) |
	servers#make_guard_receive(ChannelName, ChannelList, pifcp(sendid),
					{_Cdr, Iterate}, Consume').


/* Compare to make_RHS2 */
make_sum_procedure(Mode, Name, Writes, RHS, Tuple, Entries, NextEntries) :-

    Mode =?= send :
      Entries = [Mode(Atom?, (Writes | SendChoices?), (ChoiceAtom? :- RHS))
		| NextEntries] |
	piutils#tuple_to_atom(Tuple, Atom),
	make_choice_name(Name, ".send", SendChoices),
	make_choice_atom(Atom, SendChoices?, [`pifcp(chosen)],
				ChoiceAtom);

    Mode =?= mixed :
      Sender = `pifcp(sendid),
      Entries = [Mode(Atom?, (Writes? |
				pi_monitor#unique_sender(Name, Sender),
				MixedChoices?),
		 (ChoiceAtom? :- RHS?))
		| NextEntries] |
	piutils#tuple_to_atom(Tuple, Atom),
	make_choice_name(Name, ".mixed", MixedChoices),
	make_choice_atom(Atom, MixedChoices, [`pifcp(chosen), Sender],
				ChoiceAtom);

    /* receive, conflict */
    Mode =\= send, Mode =\= mixed :
      Name = _,
      Writes = _,
      Entries = [Mode(Atom?, RHS, []) | NextEntries] |
	piutils#tuple_to_atom(Tuple, Atom).

  make_choice_name(Prefix, Suffix, Name) :-
    string_to_dlist(Prefix, PL, PS),
    string_to_dlist(Suffix, SL, []) :
      PS = SL |
	list_to_string(PL, Name).

  make_choice_atom(InnerLHS, Name, ChoiceVars, ChoiceAtom) :-
	utils#tuple_to_dlist(InnerLHS, [_ | ChannelVariables], ChoiceVars),
	utils#list_to_tuple([Name | ChannelVariables], ChoiceAtom).

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

	piutils#sort_out_duplicates([NamedClauses], Clauses, Reply),
	diagnose_duplicate.

  diagnose_duplicate(Reply, Diagnostic, Errors, NextErrors) :-

    Reply ? Duplicate,
    Diagnostic = Name - String :
      Errors ! (Name - String(Duplicate)) |
	self;

    Reply =?= [] :
      Diagnostic = _,
      Errors = NextErrors;

    otherwise :
      Reply = _,
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
