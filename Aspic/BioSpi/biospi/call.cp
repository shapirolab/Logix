/*
Precompiler for Stic Pi Calculus procedures - call management.

Bill Silverman, December 1999.

Last update by		$Author: bill $
		       	$Date: 2003/03/21 07:07:41 $
Currently locked by 	$Locker:  $
			$Revision: 1.6 $
			$Source: /home/qiana/Repository/Aspic/BioSpi/biospi/call.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language([evaluate, compound, colon]).
-export([make_local_call/10, make_remote_call/8,
	 prime_local_channels/3, sum_procedures/5]).

-include(spi_constants).
-include(bio_constants).
      
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

    Body1 =?= self :
      Body1' = `Name |
	extract_id(ProcessDefinition, Name, _Arity),
	self;

    string(Body1), Body1 =\= true, Body1 =\= self,
    nth_char(1, Body1, C), CHAR_a =< C, C =< CHAR_z :
      Primes = _,
      Name = Body1,
      CallDefinition = [],
      NextIn = In |
	extract_id(ProcessDefinition, ProcessName, _Arity),
	macro_call + (Arguments = [], ChannelNames = _),
	macroed_call + (MacroArguments = MacroedArguments);

    arity(Body1) > 1, arg(1, Body1, self) |
	extract_id(ProcessDefinition, Name, _Arity),
	copy_goal_args(Body1, `Name?, Body1'),
	self;

    arity(Body1) > 1, arg(1, Body1, `Functor), string(Functor) |
	extract_id(ProcessDefinition, Name, _Arity),
	extract_lhs_parts(ProcessDefinition, ChannelNames, _Outer, _Inner),
	extract_arguments_or_substitutes(Name, Body1, Arguments, Substitutes,
						Errors, Errors'?),
	verify_call_channels(Name, Body1, ChannelNames, Locals,
				Errors', Errors''?),
	prime_local_channels(Primes, Arguments?, Arguments'),
	substituted_local_channels(Primes, Substitutes?, Substitutes'),
	lookup_call_functor,
	complete_local_call;

    arity(Body1) > 1,
    arg(1, Body1, Name), string(Name), Name =\= self,
    nth_char(1, Name, C), CHAR_a =< C, C =< CHAR_z :
      CallDefinition = [],
      In = [logix_variables(Primed?) | NextIn] |
	extract_id(ProcessDefinition, ProcessName, _Arity),
	extract_lhs_parts(ProcessDefinition, ChannelNames, _Outer, _Inner),
	utils#tuple_to_dlist(Body1, [_ | Arguments], []),
	macro_call,
	prime_macro_arguments(Primes, MacroedArguments?, MacroArguments,
					Primed),
	macroed_call;

    Body1 = `Functor :
      Locals = _,
      Primes = _,
      Arguments = [],
      Substitutes = [] |
	extract_id(ProcessDefinition, Name, _Arity),
	lookup_call_functor,
	complete_local_call;

    Body1 = _ + _ :
      CallDefinition = {Name, 0, [/*fake_channels*/], {{fake_lhs}, {fake_lhs}},
				sum(fake_rhs, fake_communication_rhs)}|
	make_summed_call(ProcessDefinition, Locals, Primes, Body1, Body2, Name,
			 In, NextIn, Errors, NextErrors);

    otherwise,
    tuple(Body1),
    arity(Body1, 1),
    arg(1, Body1, Goals) :
      ProcessDefinition = _,
      Locals = _,
      Primes = _,
      Body2 = Goals,
      In = [logix_variables(LogixVars?) | NextIn],
      Errors = NextErrors,
      CallDefinition = [] |
	utilities#find_logix_variables(Body1, LogixVars, []);	

    otherwise :
      Locals = _,
      Primes = _,
      Body2 = true,
      In = NextIn,
      Errors = [(Name? - invalid_local_call(Body1)) | NextErrors],
      CallDefinition = [] |
	extract_id(ProcessDefinition, Name, _Arity).

  substituted_local_channels(Primes, Substitutes, Primed) :-

    Primes ? {Channel, ChannelP} |
	prime_substitutes(Channel, ChannelP, Substitutes, Substitutes'),
	self;

    Primes =?= [] :
      Primed = Substitutes.

  prime_substitutes(Channel, ChannelP, SubsIn, SubsOut) :-

    SubsIn ? {Substitute, Sub}, Sub =?= Channel :
      SubsOut ! {Substitute, ChannelP} |
	self;

    SubsIn ?  {Substitute, Sub}, Sub =\= Channel :
      SubsOut !  {Substitute, Sub} |
	self;

    SubsIn =?= [] :
      Channel = _,
      ChannelP = _,
      SubsOut = [].

  macro_call(ChannelNames, Locals, Name, Arguments, MacroedArguments) :-

    Name =?= set_base_rate |
	verify_macro + (ArgTypes = [real | channels]);

    Name =?= randomize_messages |
	verify_macro + (ArgTypes = channels);

    Name =?= serialize_messages |
	verify_macro + (ArgTypes = channels);

    Name =?= get_channel_status,
    Arguments = [Channel | StatusList] :
      Arguments' = [Channel | VerifiedStatusList] |
	verify_status_list,
	verify_macro +
	  (ArgTypes = [channel | strings]);

    otherwise :
      Arguments = _,
      ChannelNames = _,
      Locals = _,
      Name = _,
      MacroedArguments = unrecognized_macro_call.

  verify_status_list(StatusList, VerifiedStatusList) :-

    StatusList = [all, Reply], Reply = `Name, string(Name) :
      VerifiedStatusList =
	[blocked, type, baserate, refences, messages, weight, name, Reply];

    otherwise |
	utilities#subtract_list(StatusList,
		[blocked, type, rate, references,
		 messages, sends, receives, dimers,
		 weight, sendweight, receiveweight, dimerweight, 
		 name], Remnant),
	check_status_remnant.

  check_status_remnant(StatusList, VerifiedStatusList, Remnant) :-

    Remnant = [`Name], string(Name) :
      VerifiedStatusList = StatusList;

    Remnant = [] :
      VerifiedStatusList = StatusList;

    otherwise :
      StatusList = _,
      VerifiedStatusList = error(invalid_arguments(Remnant)).

  verify_macro(ChannelNames, Locals, ArgTypes, Arguments,
		 MacroedArguments) :-
	verify_macro_arguments + (OkArgs, OkArguments = OkArgs).

  verify_macro_arguments(ChannelNames, Locals, ArgTypes, Arguments,
			  OkArguments, OkArgs, MacroedArguments) :-

    ArgTypes ? _Type,
    Arguments ? ReadOnly, ReadOnly =?= ?_ :
      OkArgs ! ReadOnly |
	self;

    ArgTypes ? string,
    Arguments ? Arg, string(Arg) :
      OkArgs ! Arg |
	self;

    ArgTypes ? real,
    Arguments ? Arg, number(Arg),
    /* herewith a kluge due to problem in assembler$real_kind_code */
    convert_to_string(Arg, Arg') :
      OkArgs ! Arg' |
	self;

    ArgTypes ? integer,
    Arguments ? Arg, integer(Arg) :
      OkArgs ! Arg |
	self;

    ArgTypes ? nil,
    Arguments ? [] :
      OkArgs ! [] |
	self;

    ArgTypes ? channel,
    Arguments ? Arg, string(Arg) :
      OkArgs ! `Arg |
	utilities#verify_channel("", Arg, ChannelNames, Locals, _,  Err, []),
	verified_macro_channel;

    ArgTypes =?= strings,
    Arguments ? Arg, string(Arg) :
      OkArgs ! [Arg | Strings],
      OkArgs'' = [Strings | OkArgs'] |
	self;

    ArgTypes =?= channels,
    Arguments ? Arg, string(Arg) :
      OkArgs ! [`Arg | OkChannelNames],
      OkArgs'' = [OkChannelNames | OkArgs'] |
	utilities#verify_channel("", Arg, ChannelNames, Locals, _,  Err, []),
	verified_macro_channel;

    string(ArgTypes),
    Arguments = [`Variable], string(Variable) :
      ChannelNames = _,
      Locals = _,
      OkArgs = [[] | Arguments] |
	verified_macro_arguments(OkArguments, MacroedArguments);

    string(ArgTypes),
    Arguments = [] :
      ChannelNames = _,
      Locals = _,
      OkArgs = [[], `"_"] |
	verified_macro_arguments(OkArguments, MacroedArguments);

    ArgTypes =?= [],
    Arguments = [`Variable], string(Variable) :
      ChannelNames = _,
      Locals = _,
      OkArgs = Arguments |
	verified_macro_arguments(OkArguments, MacroedArguments);

    ArgTypes =?= [],
    Arguments = [] :
      ChannelNames = _,
      Locals = _,
      OkArgs = [`"_"] |
	verified_macro_arguments(OkArguments, MacroedArguments);

    otherwise :
      MacroedArguments = argument_error([Message | Mss]),
      MacroedArguments' = argument_error(Mss) |
	invalid_macro_argument.

  invalid_macro_argument(ChannelNames, Locals, ArgTypes, Arguments,
			 OkArguments, OkArgs, MacroedArguments, Message) :-

    Arguments ? Arg,
    ArgTypes ? Type,
    string_to_dlist(Type, TL, []),
    string_to_dlist("not_a_", NL, TL) :
      Message = NotA?(Arg) |
	list_to_string(NL, NotA),
	verify_macro_arguments;

    Arguments ? Arg,
    ArgTypes = [] :
      Message = excess_argument(Arg) |
	verify_macro_arguments;

    Arguments =?= error(Error) :
      ArgTypes = _,
      Message = Error,
      Arguments' = [],
      ArgTypes' = [] |
	verify_macro_arguments;

    Arguments =?= [],
    ArgTypes ? Type,
    string_to_dlist(Type, TL, []),
    string_to_dlist("missing_", NL, TL) |
	list_to_string(NL, Message),
	verify_macro_arguments.

  macroed_call(ProcessName, Name, Body1, Body2, Errors, NextErrors,
		MacroArguments) :-

    list(MacroArguments) :
      Body1 = _,
      ProcessName = _,
      NextErrors = Errors |
	utils#list_to_tuple([Name | MacroArguments?], Body2);

    otherwise :
      Name = _,
      Body2 = true,
      Errors = [ProcessName-Body1-MacroArguments | NextErrors].

  verified_macro_channel(ChannelNames, Locals, ArgTypes, Arguments,
			  OkArguments, OkArgs, MacroedArguments, Arg, Err) :-

    Err =?= [] :
      Arg = _ |
	verify_macro_arguments;	

    Err =\= [] :
      MacroedArguments = argument_error([invalid_macro_channel(Arg) | Mss]),
      MacroedArguments' = argument_error(Mss) |
	verify_macro_arguments.
	
  verified_macro_arguments(OkArguments, OkArguments?^).
  verified_macro_arguments(_OkArguments, argument_error([]^)).


verify_call_channels(Name, Goal, ChannelNames, Locals, Errors, NextErrors)
			+ (Index = 2) :-

    arg(Index, Goal, (_ = String)), String =\= NULL,
    Index++ |
	utilities#verify_channel(Name, String, ChannelNames, Locals, _OkChannel,
				Errors, Errors'?),
	self;

    arg(Index, Goal, String), String =\= NULL,
    Index++ |
	utilities#verify_channel(Name, String, ChannelNames, Locals, _OkChannel,
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
    list(Arguments) :
      Substitutes = _,
      Name = _,
      Body1 = _ |
	extract_id(CallDefinition, _Name, Arity),
	extract_lhs_parts(CallDefinition, _ChannelNames, Atom, _InnerLHS),
	substitute_arguments+(Index = 1, Substitutes = Substitutes'),
	call_with_substitutes;

    CallType =?= outer,
    Arguments =?= [] :
      Errors = NextErrors,
      Name = _,
      Body1 = _ |
	extract_lhs_parts(CallDefinition, _ChannelNames, Atom, _InnerLHS),
	call_with_substitutes;

    CallType =?= inner,
    Arguments =?= [] :
      Name = _,
      Body1 = _,
      Errors = NextErrors |
	extract_lhs_parts(CallDefinition, _ChannelNames, _OuterLHS, Atom),
	call_with_substitutes.


substitute_arguments(Index, Atom, Arity, Arguments, Substitutes, Name, Body1,
			Errors, NextErrors) :-

    Index++ =< Arity,
    arg(Index', Atom, S),
    Arguments ? C, S =\= `C :
      Substitutes ! {S, C} |
	self;

    Index++ =< Arity,
    arg(Index', Atom, S),
    Arguments ? C, S =?= `C |
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


lookup_call_functor(Name, ProcessDefinition, Functor,
		CallType, CallDefinition, In, NextIn) :-

    Functor =?= Name :
      CallType = inner,
      CallDefinition = ProcessDefinition,
      In = NextIn;

    otherwise :
      Name = _,
      ProcessDefinition = _,
      In = [lookup_functor(Functor, CallType, CallDefinition) | NextIn].

extract_arguments_or_substitutes(Name, Tuple, Arguments, Substitutes,
				Errors, NextErrors) + (Index = 2, Type = _) :-

    arg(Index, Tuple, Channel), string(Channel), Channel =\= NULL,
    Index++ :
      Type = channel,
      Arguments ! Channel |
	self;

    arg(Index, Tuple, `Channel), string(Channel), Channel =\= NULL,
    Index++ :
      Type = channel,
      Arguments ! Channel |
	self;

    arg(Index, Tuple, S = C), string(S), S =\= NULL, string(C),
    Index++ :
      Type = substitute,
      Substitutes ! {`S, C} |
	self;

    arg(Index, Tuple, S = `C), string(S), S =\= NULL, string(C),
    Index++ :
      Type = substitute,
      Substitutes ! {`S, C} |
	self;

    arg(Index, Tuple, `S = C), string(S), S =\= NULL, string(C),
    Index++ :
      Type = substitute,
      Substitutes ! {`S, C} |
	self;

    arg(Index, Tuple, `S = `C), string(S), S =\= NULL, string(C),
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
      Arguments ! NULL,
      Errors ! (Name - invalid_channel_argument(Arg, Tuple)) |
	self;

    otherwise,
    Type = substitute,
    arg(Index, Tuple, Arg),
    Index++ :
      Errors ! (Name - invalid_substitute(Arg, Tuple)) |
	self.


make_remote_call(Name, ChannelNames, Locals, Primes, PiCall, CompoundCall,
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
	utilities#make_lhs_tuple(Functor, Arguments, CompoundCall);

    arg(1, PiCall, `Functor), string(Functor), Arguments =?= [] :
      Name = _,
      CompoundCall = Functor,
      Errors = NextErrors;

    otherwise :
      Arguments = _,
      CompoundCall = [],
      Errors = [Name - invalid_remote_call(PiCall) | NextErrors].


prime_macro_arguments(Primes, MacroedArguments, PrimedArguments, Variables) :-

    MacroedArguments ? Constant, constant(Constant) :
      PrimedArguments ! Constant |
	self;

    MacroedArguments ? ?Variable :
      PrimedArguments ! ?Variable',
      Variables ! Variable |
	prime_local_channels(Primes, [Variable], [Variable']),
	self;

    MacroedArguments ? `Channel :
      PrimedArguments ! `Channel',
      Variables ! Channel |
	prime_local_channels(Primes, [Channel], [Channel']),
	self;

    MacroedArguments ? List, List = [`_ | _] :
      Variables = [Reply? | List'?],
      PrimedArguments = [List'?, Reply] |
	prime_local_channels(Primes, List, List'),
	prime_local_channels(Primes, MacroedArguments', [Reply]);

    MacroedArguments ? List, List =\= [`_ | _], List =\= _(_) :
      Variables = [Reply],
      PrimedArguments = [List, Reply] |
	prime_local_channels(Primes, MacroedArguments', [Reply]);

    MacroedArguments =?= [`_] :
      Variables = PrimedArguments? |
	prime_local_channels(Primes, MacroedArguments, PrimedArguments);

    MacroedArguments =?= _(_) :
      Primes = _,
      PrimedArguments = MacroedArguments,
      Variables = [].      


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

    ArgsIn ? `Arg, Arg =?= Channel :
      ArgsOut ! `ChannelP |
	self;

    ArgsIn ? Arg, Arg =\= Channel, Arg =\= `Channel :
      ArgsOut ! Arg |
	self;

    ArgsIn =?= [] :
      Channel = _,
      ChannelP = _,
      ArgsOut = [].

/***************** Summation Process server predicates. **********************/

make_summed_call(ProcessDefinition, Locals, Primes, Sum, Call, Name,
			In, NextIn, Errors, NextErrors) :-

    true :
      In ! call_sum(Name?, Procedures?, Call) |
	utils#binary_sort_merge(Names?, NameList),
	concatenated_sum_name(NameList?, Name),
	summed_call(ProcessDefinition, Locals, Primes, Sum, Names, Procedures,
			In', NextIn, Errors, NextErrors).

  concatenated_sum_name(NameList, Name) + (NLH = NLT?, NLT) :-


    NameList ? N :
      NLT' ! CHAR_PLUS |
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
    otherwise :
      Locals = _,
      Primes = _,
      Errors = [Name - illegal_process_summation(Sum) | NextErrors],
      Names = [],
      Procedures = false,
      In = NextIn |
	extract_id(ProcessDefinition, Name, _Arity).

/****************** Summation  Empty server predicates. **********************/

sum_procedures(Summed, Generated, Optimize, NextOptimize, Errors) +
			(Cumulated = []) :-

    Summed ? Name(Procedures, Call) |
	cumulate,
	extract_procedure_parts(Procedures,
		Names, Calls, Channels, CodeTuples),
	optimize_sum(Name, Names, Optimize, Optimize'?),
	cumulated;

    Summed = [] :
      Cumulated = _,
      Generated = [],
      Optimize = NextOptimize,
      Errors = [].

  extract_procedure_parts(Procedures, Names, Calls, Channels, CodeTuples) :-

    Procedures ? {Call, ProcedureDefinition},
    ProcedureDefinition =?= {Name, _Arity, ChannelNames, _LHS, CodeTuple} :
      Names ! Name,
      Calls ! Call,
      Channels ! ChannelNames,
      CodeTuples ! CodeTuple |
	self;

    Procedures ? _Ignore,
    otherwise |
	self;

    Procedures = false :
      Names = [],
      Calls = [],
      Channels = [],
      CodeTuples = [];

    Procedures = [] :
      Names = [],
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

  cumulated(Summed, Generated, Optimize, NextOptimize, Errors, Cumulated,
	Name, Calls, Channels, CodeTuples, Call, Reply) :-

    Reply =?= found :
      Channels = _,
      CodeTuples = _ |
	make_sum_call(Name, Calls, Call, Errors, Errors'?),
	sum_procedures;

    Reply =?= new :
      Cumulated' = [Name | Cumulated] |
	make_summed_rhs(Name, Calls, CodeTuples, 1, Prepares, Code,
			FinalMode, Errors, Errors'?),
	utilities#sort_out_duplicates(Channels?, SumChannels, _Reply),
	make_named_list(Prepares?, _Requests,
		Name-duplicate_channel_in_sum, Errors', Errors''?),
	make_named_guard(Prepares?, Ask, Tell),
	/* Eliminate duplicate stream names. */
	make_named_predicates(';', Code?, RHS),
	utilities#make_lhs_tuple(Name, SumChannels, Tuple),
	make_sum_procedure(FinalMode?, (Ask? : Tell?), RHS?, Tuple?,
				Generated, Generated'?),
	make_sum_call(Name, Calls, Call, Errors'', Errors'''?),
	sum_procedures.


optimize_sum(Name, Names, Optimize, NextOptimize) :-

    Names =?= [] :
      Name = _,
      Optimize = NextOptimize;

    Names =\= [] :
      Optimize ! procedure(Notes?, 0, {Name?}, _Value) |
% screen#display(sum(Name, Value)),
	add_calls_and_channels.

  add_calls_and_channels(Names, Optimize, NextOptimize, Notes) :-

    Names ? Name :
      Optimize ! procedure([], 0, {Name}, {Calls, Channels}),
      Notes ! call(Calls),
      Notes' ! variables(Channels) |
% screen#display(add(Name, Calls, Channels)),
	self;

    Names = [] :
      Optimize = NextOptimize,
      Notes = [].


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


make_summed_rhs(Name, Calls, CodeTuples, Index, Prepares, Code,
			FinalMode, Errors, NextErrors) +
		(Mode = none, Sender = _) :-

    CodeTuples ? ProcessMode(SendRHS, ProcessRHS),
    Calls ? _,
    SendRHS =?= (Idents : Tells | _Relay) |
	utilities#untuple_predicate_list(",", Idents, Asks),
	utilities#untuple_predicate_list(",", Tells, Tells'),
	utilities#untuple_predicate_list(";", ProcessRHS, ClauseList),
	add_sends_and_receives(Asks?, Tells'?, ClauseList?, Name,
				Index, Index', Prepares, Prepares'?,
				Code, Code'?),
	utilities#update_process_mode(Mode, ProcessMode, Mode'),
	self;

    CodeTuples ? ProcessMode([], []),
    Calls ? Call :
      Errors ! (Name-invalid_mode_in_summation(Call? - ProcessMode)) |
	utilities#update_process_mode(Mode, ProcessMode, Mode'),
	self;

    CodeTuples = [] :
      Calls = _,
      Index = _,
      Prepares = [],
      Code = [],
      FinalMode = Mode,
      Errors = NextErrors |
	final_process_mode(Name, Mode, Sender).

  final_process_mode(Name, Mode, Sender) :-

    Mode =?= mixed :
      Name = _,
      Sender = `spifcp(sendid);

    Mode =?= send :
      Sender = Name;

    /* receive or none */
    otherwise :
      Name = _,
      Mode = _,
      Sender = [].

  add_sends_and_receives(Asks, Tells, ClauseList, Name,
			Index, NewIndex, Prepares, NextPrepares,
			Code, NextCode) :-
 
    Asks ? Identify,
    Identify =?= vector(`ChannelName),
    Tells ? Request,
    Request =?= request(Type,_,_,_,_),
    ClauseList ? Communication,
    Communication =?= (BIO_CHOSEN = _Index : Result = Tuple | Body),
    Index++ :
      Prepares ! {Type(ChannelName), Identify, Request'?},
      Code ! Index((BIO_CHOSEN = Index : Result = Tuple | Body)) |
	reindex_request(Request, ChannelName, Index, Request'),
	self;

    Asks ? Identify,
    Identify =?= vector(`ChannelName),
    Tells ? Request,
    Request =?= request(Type,_,_,_,_),
    ClauseList ? Communication,
    Communication =?= (BIO_CHOSEN = _Index, Result = Tuple | Body),
    Index++ :
      Prepares ! {Type(ChannelName), Identify, Request'?},
      Code ! Index((BIO_CHOSEN = Index, Result = Tuple | Body)) |
	reindex_request(Request, ChannelName, Index, Request'),
	self;

    ClauseList = [] :
      Name = _,
      Asks = _,
      Tells = _,
      NewIndex = Index,
      NextPrepares = Prepares,
      Code = NextCode.

  reindex_request(Request, ChannelName, Index, NewRequest) :-

    Request = request(Type, ChannelName, Multiplier, _Index, Locus) :
      NewRequest = request(Type, ChannelName, Multiplier, Index, Locus).


/* Compare to make_rhs2 */
make_sum_procedure(Mode, Requests, RHS, Tuple, Generated, NextGenerated) :-

    Mode =?= communicate :
      Generated = [Mode(Atom?, (Requests | Chooser?),
		      (CommunicationAtom? :- RHS))
		  | NextGenerated] |
	utilities#tuple_to_atom(Tuple, Atom),
	utilities#make_communicator(Atom, Chooser, CommunicationAtom);

    /* conflict */
    otherwise :
      Requests = _,
      Generated = [Mode(Atom?, RHS, []) | NextGenerated] |
	utilities#tuple_to_atom(Tuple, Atom).


extract_id(Definition, Name, Arity) :-
    true :
      Definition = {Name, Arity, _ChannelNames, _LHS, _CodeTuple}.

extract_lhs_parts(Definition, ChannelNames, OuterLHS, InnerLHS) :-
    true :
      Definition = {_Name, _Arity, ChannelNames, LHS, _CodeTuple},
      LHS = {OuterLHS, InnerLHS}.

/************************** Summation Utilities ******************************/

make_named_guard(Requests, Ask, Tell) :-

    Requests ? _Name(Idents, Request), Requests' =\= [] :
      Ask = (Idents, Ask'?),
      Tell = (Request, Tell'?) |
	self;

    Requests = [_Name(Idents, Request)] :
      Ask = Idents,
      Tell = Request;

    Requests = [] :
      Ask = true,
      Tell = true.

make_named_list(NamedClauses, Clauses, Diagnostic, Errors, NextErrors) :-

	utilities#sort_out_duplicates([NamedClauses], Clauses, Reply),
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
