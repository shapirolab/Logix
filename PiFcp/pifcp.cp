/*
Precompiler for compound procedures.

Bill Silverman, December 1999.

Last update by		$Author: bill $
		       	$Date: 1999/12/13 13:20:33 $
Currently locked by 	$Locker:  $
			$Revision: 1.3 $
			$Source: /home/qiana/Repository/PiFcp/Attic/pifcp.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

% -export([transform/5]).
% -mode(trust).
-language(compound).

Source ::= [Any].

Term ::= Any.
Terms ::= [Term].
Errors ::= [Any].
TermList ::= [Any | Terms].

procedure transform(Any, Terms, Any, Terms, Errors).

transform(Attributes1, Source, Attributes2, Terms, Errors) :-

	/* Exclude declared export from Attributes. */ 
	Attributes2 = [-export(Exports?) | Attributes1],
	program.


Tree ::= {Any, Tree, Tree} ; [].
LookupTree ::= {String(Any), Any, Any}.
Server, AddClauses ::= true ; false.
PID ::= String/Integer.
Predicate ::= Tuple ; String.
Variable ::= {`"`", Any}.
Descriptor ::= {Integer, VTree}.
FindVReply ::= found(VData) ; [].
FindReply ::= FindVReply.
FindDescriptor ::= Descriptor ; [].
VTree ::= {String(VData), VTree, VTree} ; [].
VData ::= Integer.
Duped ::= unique(VTree) ; ambiguous(VTree).
Defs ::= Refs ; [lookup(String, Duped, Duped, (new ; old)) | Defs].
Refs ::= [member(String, Duped, (true ; false))].

serve_empty_scope(In, Exports, Errors) +
		(Progeny = [], Refs = AddRef?, AddRef) :-

    In ? error(Name, Error) :
      Errors ! (Name + Error) |
	self;

    In ? lookup_functor(Functor, CallType, CallDefinition) :
      AddRef ! lookup_functor(Functor, CallType, CallDefinition) |
	self;

    In ? process(LHS, ProcessScope) |
	make_process_scope(LHS, ProcessScope, [],
			In'', In'?, ProcessDefinition, Errors, Errors'?),
	export_process(ProcessDefinition?, Exports, Exports'?),
	add_process_definition(ProcessDefinition?, Progeny, Progeny'),
	self;

    In = [] :
      AddRef = [],
      Errors = [],
      Exports = [] |
	find_process_refs(Refs, Progeny, [], []).

find_process_refs(Refs, Progeny, Out, EndOut) :-

    Refs ? lookup_functor(Functor, CallType, CallDefinition) |
	search_progeny(Functor, Progeny, CallType, CallDefinition, Out, Out'),
	self;

    Refs = [] :
      Progeny = _,
      Out = EndOut.

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

make_process_scope(LHS, ProcessScope, GlobalList,
		Out, NextOut, ProcessDefinition, Errors, NextErrors) :-

    true :
      ProcessDefinition = {Name?, Arity?, Channels?, OuterAtom?, InnerAtom?} |
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
	construct_lhs_atoms(Name?, ParamList1?, LocalList1?, GlobalList,
				Channels, OuterAtom, InnerAtom),
	serve_process_scope(ProcessScope?, ProcessDefinition?,
				Out, NextOut, Errors'''', NextErrors).


compute_status(OuterAtom, InnerAtom, Status) :-

    OuterAtom =?= [] :
      InnerAtom = _,
      Status = nil;    

    OuterAtom =\= [], OuterAtom =?= InnerAtom :
      Status = single;

    otherwise :
      OuterAtom = _,
      InnerAtom = _,
      Status = double.

export_process(ProcessDefinition, Exports, NextExports) :-

    ProcessDefinition =?= {Name, Arity, _Channels, _OuterAtom, _InnerAtom},
    Name =\= "_" :
      Exports ! (Name/Arity),
      NextExports = Exports';

    otherwise :
      ProcessDefinition = _,
      Exports = NextExports.

add_process_definition(ProcessDefinition, Progeny, NewProgeny) :-

    ProcessDefinition =?= {Name, _Arity, _Channels, _OuterAtom, _InnerAtom},
    Name =\= "_" :
      NewProgeny = [ProcessDefinition | Progeny];

    otherwise :
      ProcessDefinition = _,
      NewProgeny = Progeny.


parse_lhs(LHS, Name, Arity, ParamList, ChannelList, Errors, NextErrors) :-

    LHS =?= `Functor, string(Functor) :
      Name = Functor, Arity = 0,
      ParamList = [],
      Errors = NextErrors |
	unify_without_failure(ChannelList, []);

    LHS = LHS' + Channels,
    writable(ChannelList) :
      ChannelList' = ChannelList? |
	extract_channel_list(Channels, ChannelList, Errors, Errors'),
	self;

    arg(1, LHS, `Functor), string(Functor), arity(LHS, A),
    A-- :
      Name = Functor,
      Arity = A' |
	extract_arglist(LHS, ParamList, Errors, NextErrors),
	unify_without_failure(ChannelList, []);

    otherwise :
      Errors ! improperly_formed_left_hand_side(LHS),
      LHS' = `"_" |
	self.

extract_channel_list(Channels, ChannelList, Errors, NextErrors) :-

    string(Channels) :
      ChannelList = [Channels],
      Errors = NextErrors;

    Channels = (String, Channels'), string(String) :
      ChannelList ! String |
	self;

    Channels = (NotChannel, Channels'),
    otherwise :
      Errors !invalid_channel(NotChannel) |
	self;

    otherwise :
      Errors = [invalid_channel(Channels) | NextErrors],
      ChannelList = [].


extract_arglist(LHS, ParamList, Errors, NextErrors) + 
			(Index = 2) :-
    arity(LHS) < Index :
      ParamList = [],
      Errors = NextErrors;

    arity(LHS) >= Index,
    arg(Index, LHS, ChannelName), string(ChannelName), Index++ :
      ParamList ! ChannelName |
	self;

    otherwise,
    arg(Index, LHS, NotString), Index++ :
      Errors ! invalid_parameter(NotString) |
	self.


construct_lhs_atoms(Name, ParamList, LocalList, GlobalList,
			Channels, OuterAtom, InnerAtom) :-

    Name =?= "_" :
      ParamList = _,
      LocalList = _,
      GlobalList = _,
      OuterAtom = [],
      InnerAtom = [],
      Channels = [];

    Name =\= "_",
    ParamList =?= LocalList :
      InnerAtom = OuterAtom?|
	concatenate(LocalList, GlobalList, List),
	check_for_duplicates(List?, Channels, "", _, _),
	construct_atom(Name, "", Channels?, OuterAtom);

    Name =\= "_",
    ParamList =\= LocalList,
    GlobalList =?= [] :
      Channels = LocalList |
	construct_atom(Name, "", ParamList, OuterAtom),
	construct_atom(Name, ".", LocalList, InnerAtom);

    Name =\= "_",
    ParamList =\= LocalList,
    GlobalList =\= [] |
	concatenate(ParamList, GlobalList, OuterList),
	check_for_duplicates(OuterList?, OuterList1, "", _, _),
	construct_atom(Name, "", OuterList1?, OuterAtom),
	concatenate(LocalList, GlobalList, InnerList),
	check_for_duplicates(InnerList?, InnerList1, "", _, _),
	partial_ordered_difference(LocalList, ParamList, NewList),
	partial_ordered_difference(InnerList1, LocalList, GlobalList1),
	concatenate(ParamList, GlobalList1?, OuterList2),
	concatenate(OuterList2?, NewList?, Channels),
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

make_channel_list(N, Channels, ChannelList) :-

    N =< 0 :
      ChannelList = [],
      Channels = _;
    
    N > 0,
    make_tuple(N, Tuple) |
	channels_to_atom(Channels, Tuple, 1, ChannelList).

channels_to_atom(Cs, T, I, Atom) :-

    Cs ? C,
    arg(I, T, A),
    I++ :
      A = C |
	self;

    Cs = [] :
      I = _,
      Atom = T.


serve_process_scope(In, ProcessDefinition, Out, EndOut, Errors, NextErrors) +
		(IdIndex = 1, Primes = [], Locals = [], Progeny = [],
		 Refs = AddRef?, AddRef) :-

    In ? atoms(Outer, Inner),
    ProcessDefinition =?= {_Name, _Arity, _Channels, OuterAtom, InnerAtom} :
      Outer = OuterAtom,
      Inner = InnerAtom |
	self;

    In ? body_receive(Channel, Body),
    ProcessDefinition = {Name, _Arity, Channels, _OuterAtom, _InnerAtom} |
	identify_channel(Name, Channel, Channels, Channel', Errors, Errors'?),
	make_body_receive,
	self;

    In ? body_send(Message, Via, Channel, PiMessage) :
      PiMessage = {Sender?, ChannelList?, 1, `"_"} |
	communication_channel(ProcessDefinition, Via, Channel,
				Errors, Errors'?),
	message_to_channel_list(ProcessDefinition, Message, Channels,
				Errors', Errors''?),
	/* Check channels */
	make_sender(ProcessDefinition, Channel, Sender),
	channels_to_variables(Channels?, Variables, N),
	make_channel_list(N?, Variables?, ChannelList),
	self;

    In ? call(Body1, Body2),
    Body1 =\= (_#_) |
	make_local_call(ProcessDefinition, Body1, Body2,
			In'', In'?, Errors, Errors'?),
	self;

    In ? channels(Cs),
    ProcessDefinition =?= {_Name, _Arity, Channels, _OuterAtom, _InnerAtom} :
      Cs = Channels |
	self;

    In ? guard_receive(Channel, Message, SendId, Iterates, Consume),
    ProcessDefinition =?= {Name, _Arity, Channels, _OuterAtom, _InnerAtom} :
      Locals = _, Primes = _ |
	identify_channel(Name, Channel, Channels, Channel', Errors, Errors'?),
	parse_message(ProcessDefinition, Message, ChannelList,
			Locals', Primes', Errors', Errors''?),
	make_guard_receive,
	self;

    In ? error(Description),
    arg(1, ProcessDefinition, Name) :
      Errors ! (Name - Description) |
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

    In ? process(LHS, ProcessScope),
    ProcessDefinition =?= {_Name, _Arity, Channels, _OuterAtom, _InnerAtom} |
	concatenate(Locals, Channels, GlobalList),
	make_process_scope(LHS, ProcessScope, GlobalList,
			In'', In'?, CallDefinition, Errors, Errors'?),
	add_process_definition(CallDefinition?, Progeny, Progeny'),
	self;

    In ? remote_call(Call1, Call2),
    arg(1, ProcessDefinition, Name) |
	extract_arguments_or_substitutes(Name, Call1, Arguments, _Substitutes,
			Errors, Errors'?, 2, channels),
	complete_remote_call(ProcessDefinition, Call1, Arguments, Call2,
				Errors', Errors''?),
	self;
	
    In ? status(Status),
    ProcessDefinition =?= {_Name, _Arity, _Channels, OuterAtom, InnerAtom} |
	compute_status,
	self;

    In = [] :
      ProcessDefinition = _,
      IdIndex = _,
      Locals = _,
      Primes = _,
      AddRef = [],
      Errors = NextErrors |
	find_process_refs(Refs, Progeny, Out, EndOut).

communication_channel(Name, Via, Channel, Errors, NextErrors) :-

    string(Via), Via =\= "_" :
      Name = _,
      Channel = Via,
      Errors = NextErrors;

    otherwise :
      Channel = "_",
      Errors = [Name - invalid_channel(Via) | NextErrors].

make_guard_receive(Channel, ChannelList, SendId, Iterates, Consume) :-

    string_to_dlist(Channel, CL, Prime) :
      Prime = [39],

      Iterates = {(`Channel = {`"CId", `"CV", `"Mss"},
		   `"Mss" ? {`"_", `"_", `"_", `"Chosen"},
		   not_we(`"Chosen") :
		     `ChannelP = {`"CId", `"CV", `"Mss'"} |
			self),
		  (`Channel = {`"CId", `"CV", `"Mss"},
		   `"Mss" ? {`SendId, `"_", `"_", `"Chosen"} :
		     `ChannelP = {`"CId", `"CV", `"Mss'"} |
			self)},

      Consume = (`Channel = {`"_", `"_", `"Mss"},
		 `"Mss" ? {`Sender, ChannelList, `"ChoiceTag", `"Choose"},
		 ExcludeSender? :
		   `"Choose" = `"ChoiceTag"),

      TestWe = we(`"Choose") |

	list_to_string(CL, ChannelP),
	exclude_sender.

  exclude_sender(SendId, Sender, TestWe, ExcludeSender) :-

    SendId =?= "_" :
      Sender = "_",
      ExcludeSender = TestWe;

    otherwise :
      Sender = "Sender",
      ExcludeSender = (`SendId =\= `Sender, TestWe).


make_sender(ProcessDefinition, Channel, Sender) :-

    arg(1, ProcessDefinition, Name),
    string_to_dlist(Name, NL, NC),
    string_to_dlist(Channel, CL, []) :
      NC = [46 | CL] |
	list_to_string(NL, Sender).

message_to_channel_list(ProcessDefinition, Message, ChannelList,
				Errors, NextErrors) + (Index = 1) :-

    Message = [] :
      ProcessDefinition = _,
      Index = _,
      ChannelList = [],
      Errors = NextErrors;

    arg(Index, Message, Channel), string(Channel), Channel =\= "",
    Index++ :
      ChannelList ! Channel |
	self;

    arg(Index, Message, `Anonymous), Anonymous =?= "_",
    Index++ :
      ChannelList ! Anonymous |
	self;

    Index > arity(Message) :
      ProcessDefinition = _,
      Message = _,
      ChannelList = [],
      Errors = NextErrors;

    otherwise,
    arg(Index, Message, Channel),
    arg(1, ProcessDefinition, Name),
    Index++ :
      Errors ! (Name - invalid_channel_in_message(Message, Channel)),
      ChannelList ! [] |
	self;

    otherwise,
    arg(1, ProcessDefinition, Name) :
      Index = _,
      ChannelList = [],
      Errors = [(Name - invalid_channel_list(Message)) | NextErrors].


identify_channel(Name, Channel, Channels, OkChannel, Errors, NextErrors) :-

    Channels ? Channel :
      Channels' = _,
      Name = _,
      OkChannel = Channel,
      Errors = NextErrors;

    otherwise :
      Channels = _,
      OkChannel = "_",
      Errors = [Name - illegal_communication_channel(Channel) | NextErrors].


make_body_receive(Channel, Body) :-

    Channel =\= "_" :
      /*
      ** Generate code to discard one message;
      ** it does not update receive stream pointer.
      */
      Body = pi_utils#receive(`Channel, `"_");

    otherwise :
      Channel = _,
      Body = true.


parse_message(ProcessDefinition, Message, ChannelList, Locals, Primes,
			Errors, NextErrors) :-

    Message = [] :
      ProcessDefinition = _,
      ChannelList = [],
      Locals = [],
      Primes = [],
      Errors = NextErrors;

    tuple(Message),
    arg(1, ProcessDefinition, Name) :
      Index = 1 | /*,
      LocalsTail = Locals,
      PrimesTail = Primes | */
	extract_receive_channels(Index, Message, Name, Strings, ChannelNames,
					Errors, Errors'?),
	check_for_duplicates(Strings, _UniqueStrings,
		{Name, duplicate_receive_channel}, Errors', NextErrors),
Locals = [],
Primes = [],
	channels_to_variables(ChannelNames, Channels, N),
	make_channel_list.

extract_receive_channels(Index, Message, Name, Strings, ChannelNames,
				Errors, NextErrors) :-

    arg(Index, Message, Channel), string(Channel),
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


make_local_call(ProcessDefinition, Body1, Body2,
		In, NextIn, Errors, NextErrors) :-

    Body1 = true :
      ProcessDefinition = _,
      In = NextIn,
      Errors = NextErrors,
      Body2 = true;

    arity(Body1) > 1, arg(1, Body1, `Functor), string(Functor),
    arg(1, ProcessDefinition, Name) |
	extract_arguments_or_substitutes(Name, Body1, Arguments, Substitutes,
						Errors, Errors'?),
	lookup_call_functor,
	complete_local_call;

    Body1 = `Functor,
    arg(1, ProcessDefinition, Name) :
      Arguments = [],
      Substitutes = [] |
	lookup_call_functor,
	complete_local_call;

    otherwise,
    arg(1, ProcessDefinition, Name) :
      Body2 = true,
      Errors = [(Name - invalid_local_call(Body1)) | NextErrors],
      In = NextIn.

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
    CallDefinition = {_Name, Arity, _Channels, Atom, _InnerAtom} :
      Substitutes = _,
      Name = _,
      Body1 = _ |
	substitute_arguments+(Index = 1, Substitutes = Substitutes'),
	call_with_substitutes;

    CallType =?= outer,
    Arguments =?= [],
    CallDefinition = {_Name, _Arity, _Channels, Atom, _InnerAtom} :
      Errors = NextErrors,
      Name = _,
      Body1 = _ |
	call_with_substitutes;

    CallType =?= inner,
    Arguments =?= [],
    CallDefinition = {_Name, _Arity, _Channels, _OuterAtom, Atom} :
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

    arg(Index, Tuple, S = C), string(S), S =\= "_", string(C), C =\= "_",
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


program(Source, Exports, Terms, Errors) :-

	serve_empty_scope(Scope?, Exports, Errors),
	process_definitions+(Processes = [], EndScope = [], NextTerms=[]).

process_definitions(Source, Processes, Scope, EndScope, Terms, NextTerms) :-

    Source ? (LHS :- RHSS) :
      Scope ! process(LHS, ProcessScope),
      ProcessScope ! status(Status) |
	process_definitions(Processes, [], ProcessScope', ProcessScope''?,
				Nested, Nested'?),
	process(Status?, RHSS, ProcessScope'', Process, Nested'),
	nested_procedures(Process, Nested?, Terms, Terms'?),
	self;

    Source ? P,
    P =\= (_ :- _) :
      Scope ! error(invalid_process_definition(P)) |
	self;

    Source = [] :
      Processes = _,
      Terms = NextTerms,
      Scope = EndScope.


RHSS ::= RHS ; {`";", RHS, RHSS}.
RHS ::= CommaList ; (Guard | CommaList).
Guard ::= CommaList ; (CommaList : CommaList).
CommaList, Ask, Tell, Body ::= Any ; (Any , CommaList).


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
	arg(1, InnerAtom, Name),
	initialize_channels(Index, InnerAtom, Name, Initializer),
	self;

    Status =\= nil, Status =\= double, /* single or initialized */
    RHSS =?= (_|_) :
      Scope ! atoms(_, InnerAtom),
      Process = (InnerAtom? :- RHSS'?) |
	guarded_clause(RHSS, RHSS', _("_"), Nested, [], Scope', []);

    Status =\= nil, Status =\= double, /* single or initialized */
    RHSS =?= (_;_) :
      Scope ! atoms(_, InnerAtom),
      Process = (InnerAtom? :- RHSS'?) |
	guarded_clauses(RHSS, RHSS', Scope', Nested);

    Status =\= nil, Status =\= double, /* single or initialized */
    RHSS =\= (_|_), RHSS =\= (_;_)  :
      Scope ! atoms(_, InnerAtom),
      Process = (InnerAtom? :- RHSS''?) |
	transform_body(RHSS, RHSS', Nested, [], Scope', [], Guard, []),
	guard_from_body(Guard, RHSS'?, RHSS'').

guard_from_body(Guard, RHSS1, RHSS2) :-

    Guard =?= [] :
      RHSS2 = RHSS1;

    Guard =\= [] :
      RHSS2 = (Ask? : Tell? | RHSS1) |
	complete_ask_tell.

complete_ask_tell(Guard, Ask, Tell) :-

    Guard = [(A : T)] :
      Ask = A,
      Tell = T;

    Guard ? (A : T), Guard' =\= [] :
      Ask = (A, Ask'?),
      Tell = (T, Tell'?) |
	self.


initialize_channels(Index, Atom, Name, Initializer) +
			(MakeAll = More?, More) :-

    Index < arity(Atom),
    arg(Index, Atom, `Channel),
    Index++ :
      More = (MakeChannel?, NameChannel?, More'?) |
	make_and_name_channel,
	self;

    Index =:= arity(Atom),
    arg(Index, Atom, `Channel) :
      More = (MakeChannel?, NameChannel?),
      Initializer = (true : MakeAll | Name) |
	make_and_name_channel.

make_and_name_channel(Name, Channel, MakeChannel, NameChannel) :-

    string_to_dlist(Channel, Suffix, []),
    string_to_dlist(Name, PH, PS),
    string_to_dlist("V", VH, VS),
    string_to_dlist("Ms", MsH, MsS) :
      MakeChannel = make_channel(`VA?, `MsA?),
      NameChannel = (`Channel = ChannelId?(`VA?, `MsA?)),
      PS = Suffix?,
      VS = Suffix?,
      MsS = Suffix? |
	list_to_string(PH, ChannelId),
	list_to_string(VH, VA),
	list_to_string(MsH, MsA).


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


guarded_clauses(RHS1, RHS2, Nested, Scope) :-
	RHS2 = guard(RHS1),
	Nested = [],
	Scope = [].

guarded_clause(RHS1, RHS2, Type, Nested, NextNested, Scope, NextScope) :-
	
    RHS1 =?= (Channel ? Message | Body1) :
      Type = receive(SendId),
      Scope ! guard_receive(Channel, Message, SendId, Iterates, Consume),
      Clause = (Ask? : Tell? | Body2?) |
	receive_in_guard_iterates,
	complete_ask_tell([Consume | Guards], Ask, Tell),
	transform_body(Body1, Body2, Nested, NextNested,
			Scope', NextScope, Guards, []);
/*
    RHS1 =?= (Channel ! Message | RHS1') :
      Type = send(SendId),
      Scope ! guard_send(Channel, Message, SendId, Sender) |
	send_in_guard,
	complete_ask_tell([Consume | Guards], Ask, Tell),
	transform_body(Body1, Body2, Nested, NextNested,
			Scope', NextScope, Guards, []);
*/
    otherwise :
      RHS1 = (Guard | _),
      RHS2 = true,
      Type = none(_),
      Nested = NextNested,
      Scope = [error(invalid_guard(Guard)) | NextScope].


receive_in_guard_iterates(SendId, Iterates, Clause, RHS2) :-

    SendId =?= "_",
    Iterates =?= {Iterate, _Mixed} :
      RHS2 = (Iterate ; Clause);

    SendId =\= "_",
    Iterates =?= {Cdr, Mixed} :
      RHS2 = (Cdr ; Mixed ; Clause).
      

transform_body(Body1, Body2, Nested, EndNested,
		Scope, EndScope, Guards, EndGuards) :-

    Body1 = (B1, Body1') :
      Body2 = (B2?, Body2'?) |
	transform_body(B1, B2, Nested, Nested'?, Scope, Scope'?, Guards, Guards'?),
	self;

    Body1 = (Channel ! Message) :
      Body2 = true,
      Scope ! body_send(Message, Channel, Channel', PiMessage),
      Nested = EndNested,
      Scope' = EndScope |
	send_from_body;

    Body1 = (Channel ? _Message) :
      Scope ! body_receive(Channel, Body2),
      Nested = EndNested,
      Scope' = EndScope,
      Guards = EndGuards;

    list(Body1) :
      Guards = EndGuards |
	new_scope;

    Body1 =?= Name # Call1, string(Name) :
      Body2 = (Name # Call2) |
	parse_remote_call(Call1, Call2, Scope, Scope'?),
	self;

    otherwise :
      Scope ! call(Body1, Body2),
      Nested = EndNested,
      Scope' = EndScope,
      Guards = EndGuards.

new_scope(Body1, Body2, Nested, EndNested, Scope, EndScope) :-

    Body1 =?= [(_ :- _) | _] :
      Body2 = true,
      Nested = EndNested,
      Scope = [error(incomplete_new_scope(Body1)) | EndScope];

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
		Nested, EndNested, Scope, EndScope) :-
    true :
      Scope ! new_scope_id(Id),
      Body2 = Id? |
	make_new_lhs,
	process_definitions([(LHS :- Body)], Processes,
			Scope', EndScope, Nested, EndNested).


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

send_from_body(Channel, PiMessage, Guards, EndGuards) :-

    Channel =?= "_" :
      PiMessage = _,
      Guards = EndGuards;

    Channel =\= "_",
    string_to_dlist(Channel, CL, []) :
      VL = [86 | CL],
      Guards = [(`Channel = {(`"_"), `VC, (`"_")} :
		write_channel(PiMessage, `VC)) | EndGuards] |
	list_to_string(VL, VC).

/**** Utilities ****/

concatenate(List1, List2, List3) :-

    List1 ? Item :
      List3 ! Item |
	self;

    List1 =?= [] :
      List3 = List2.

partial_ordered_difference(List1, List2, List3) :-

    List1 ? Item,
    List2 ? Item |
	self;

    List2 =?= [] |
      List3 = List1;

    List1 ? Item,
    otherwise :
      List3 ! Item |
	self.


check_for_duplicates(List1, List2, ErrorCode, Errors, NextErrors) :-

    List1 ? Item :
      List2 ! Item |
	search_for_duplicate(Item, List1', List1'',
			ErrorCode, Errors, Errors'?),
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
