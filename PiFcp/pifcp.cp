/*
Precompiler for compound procedures.

Bill Silverman, December 1999.

Last update by		$Author: bill $
		       	$Date: 1999/12/07 09:27:37 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
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
	check_for_duplicates(LocalList?, LocalList',
				{Name?, channel_duplicates_parameter},
					Errors''', Errors''''?),
	construct_lhs_atoms(Name?, ParamList1?, LocalList'?, GlobalList,
				Channels, OuterAtom, InnerAtom),
	serve_process_scope(ProcessScope?, ProcessDefinition?,
				Out, NextOut, Errors'''', NextErrors).


compute_status(OuterAtom, InnerAtom, Status) :-

    OuterAtom =?= "_" :
      InnerAtom = _,
      Status = nil;    

    OuterAtom =?= InnerAtom :
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
    ParamList =\= LocalList |
	concatenate(ParamList, GlobalList, OuterList),
	check_for_duplicates(OuterList?, OuterList', "", _, _),
	construct_atom(Name, "", OuterList'?, OuterAtom),
	concatenate(LocalList, GlobalList, InnerList),
	check_for_duplicates(InnerList?, Channels, "", _, _),
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
      Atom = Name,
      Channels = _;
    
    N > 0,
    N++,
    make_tuple(N', Tuple),
    arg(1, Tuple, Name) |
	channels_to_atom(Channels, Tuple, 2, Atom).

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
		(Substitutes = [], Progeny = [], Refs = AddRef?, AddRef) :-

    In ? atoms(Outer, Inner),
    ProcessDefinition =?= {_Name, _Arity, _Channels, OuterAtom, InnerAtom} :
      Outer = OuterAtom,
      Inner = InnerAtom |
	self;

    In ? body_receive(Channel, Body) |
	make_receive(ProcessDefinition, "_", Channel, body(Body),
				Errors, Errors'?),
	self;

    In ? body_send(Message, Channel, Body) |
	make_send(ProcessDefinition, Message, Channel, body(Body),
				Errors, Errors'?),
	self;

    In ? call(Body1, Body2),
    Body1 =?= (_#_) |
	make_remote_call(ProcessDefinition, Body1, Body2),
	self;

    In ? call(Body1, Body2),
    Body1 =\= (_#_) |
	make_local_call(ProcessDefinition, Body1, Body2,
			Out, Out'?, Errors, Errors'?),
	self;

    In ? channels(Cs),
    ProcessDefinition =?= {_Name, _Arity, Channels, _OuterAtom, _InnerAtom} :
      Cs = Channels |
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

    In ? process(LHS, ProcessScope),
    ProcessDefinition =?= {_Name, _Arity, Channels, _OuterAtom, _InnerAtom} |
	make_process_scope(LHS, ProcessScope, Channels,
			In'', In'?, ProcessDefinition, Errors, Errors'?),
	add_process_definition(ProcessDefinition?, Progeny, Progeny'),
	self;
	
    In ? status(Status),
    ProcessDefinition =?= {_Name, _Arity, _Channels, OuterAtom, InnerAtom} |
	compute_status,
	self;

    In = [] :
      ProcessDefinition = _,
      Substitutes = _,
      AddRef = [],
      Errors = NextErrors |
	find_process_refs(Refs, Progeny, Out, EndOut).

make_send(ProcessDefinition, Message, Channel, Output, Errors, NextErrors) :-

    string(Channel), Channel =\= "_",
    arg(1, ProcessDefinition, Name),
    Output = body(Body) :
      Body = pi_utils#send(PiMessage?, `Channel) |
      message_to_pi_message;

    otherwise,
    arg(1, ProcessDefinition, Name),
    arg(2, Output, Guard) :
      Guard = true,
      Errors = [Name - not_yet_implemented(Channel!Message) | NextErrors].

message_to_pi_message(ProcessDefinition, Message, PiMessage,
				Errors, NextErrors) :-

    Message = [] :
      ProcessDefinition = _,
      PiMessage = [],
      Errors = NextErrors;

    tuple(Message),
    A := arity(Message),
    make_tuple(A, PM) :
      Index = 1 |
	channel_list_to_vars.

channel_list_to_vars(ProcessDefinition, Index, Message, PM, PiMessage,
				Errors, NextErrors) :-

    arg(Index, Message, Channel), string(Channel),
    arg(Index, PM, C),
    Index++ :
      C = `Channel |
	self;

    Index > arity(Message) :
      ProcessDefinition = _,
      Message = _,
      PiMessage = PM,
      Errors = NextErrors;

    otherwise,
    arg(1, ProcessDefinition, Name),
    arg(Index, PM, C),
    Index++ :
      Errors ! (Name - invalid_channel_list(Message)),
      C = `"_" |
	self.


make_receive(ProcessDefinition, Message, Channel, Output,
			Errors, NextErrors) :-

    string(Channel), Channel =\= "_",
    Output = body(Body) :
      ProcessDefinition = _,
      Message = _,
      /*
      ** Generate code to discard one message;
      ** it does not update receive stream pointer.
      */
      Body = pi_utils#receive(`Channel, `"_"),
      Errors = NextErrors;

    arg(1, ProcessDefinition, Name),
    arg(1, Output, guard),
    arg(2, Output, Guard) |
      Guard = true,
      Errors = [Name - not_yet_implemented(Channel?Message) | NextErrors];

    otherwise,
    arg(1, ProcessDefinition, Name),
    arg(2, Output, Transform) :
      Transform = true,
      Errors = [Name - illegal_send(Channel!Message) | NextErrors].

make_local_call(ProcessDefinition, Body1, Body2,
		Out, NextOut, Errors, NextErrors) :-

    Body1 = true :
      ProcessDefinition = _,
      Out = NextOut,
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
      Out = NextOut.

complete_local_call(CallType, CallDefinition, Arguments, Substitutes, Name,
				Body1, Body2, Errors, NextErrors) :-

    CallType =?= none :
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
    CallDefinition = {_Name, _Arity, _Channels, _OuterAtomAtom, Atom} :
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
		CallType, CallDefinition, Out, NextOut) :-

    arg(1, ProcessDefinition, Name),
    Functor =?= Name :
      CallType = inner,
      CallDefinition = ProcessDefinition,
      Out = NextOut;

    otherwise :
      ProcessDefinition = _,
      Out = [lookup_functor(Functor, CallType, CallDefinition) | NextOut].

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

      
make_remote_call(ProcessDefinition, Body1, Body2) :-
    /*** Need to discriminate between logix and pifcp calls. ***/
    true :
      ProcessDefinition = _ |
	Body2 = Body1.


program(Source, Exports, Terms, Errors) :-

	serve_empty_scope(Scope?, Exports, Errors),
	process_definitions+(NextTerms=[]).

process_definitions(Source, Scope, Terms, NextTerms) :-

    Source ? (LHS :- RHSS) :
      Scope ! process(LHS, ProcessScope),
      ProcessScope ! status(Status) | 
	process(Status?, RHSS, ProcessScope', Process, Nested),
	nested_processes(Process, Nested, Terms, Terms'?),
	self;

    Source ? P,
    P =\= (_ :- _) :
      Scope ! error(invalid_process_definition(P)) |
	self;

    Source = [] :
      Terms = NextTerms,
      Scope = [].


RHSS ::= RHS ; {`";", RHS, RHSS}.
RHS ::= CommaList ; (Guard | CommaList).
Guard ::= CommaList ; (CommaList : CommaList).
CommaList, Ask, Tell, Body ::= Any ; (Any , CommaList).


process(Status, RHSS, ProcessScope, Process, Nested) :-

    Status = nil :
      RHSS = _,
      ProcessScope = [],
      Process = [],
      Nested = [];

    Status = double :
      ProcessScope ! atoms(OuterAtom, InnerAtom),    
      Nested ! (OuterAtom? :- Initializer?),
      Status' = initialized |
	arg(1, InnerAtom, Name),
	index_new_channels,
	initialize_channels(Index, InnerAtom, Name, Initializer),
	self;

    Status =\= nil, Status =\= double, /* single or initialized */
    RHSS =?= (_|_) :
      ProcessScope ! atoms(_, InnerAtom),
      Process = (InnerAtom? :- RHSS'?) |
	guarded_clauses(RHSS, RHSS', ProcessScope', Nested);

    Status =\= nil, Status =\= double, /* single or initialized */
    RHSS =\= (_|_) :
      ProcessScope ! atoms(_, InnerAtom),
      Process = (InnerAtom? :- RHSS'?) |
	transform_body(RHSS, RHSS', Nested, [], ProcessScope', []).


index_new_channels(OuterAtom, Index) :-

    string(OuterAtom) :
      Index = 2;

    I := arity(OuterAtom) + 1 :
      Index = I.

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


nested_processes(Process, Nested, Terms, NextTerms) :-

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

transform_body(Body1, Body2, Nested, EndNested, Scope, EndScope) :-

    Body1 = (B1, Body1') :
      Body2 = (B2?, Body2'?) |
	transform_body(B1, B2, Nested, Nested'?, Scope, Scope'?),
	self;

    Body1 = (Channel ! Message) :
      Scope ! body_send(Message, Channel, Body2),
      Nested = EndNested,
      Scope' = EndScope;

    Body1 = (Channel ? _Message) :
      Scope ! body_receive(Channel, Body2),
      Nested = EndNested,
      Scope' = EndScope;

    list(Body1) |
	new_scope;

    otherwise :
      Scope ! call(Body1, Body2),
      Nested = EndNested,
      Scope' = EndScope.

new_scope(Body1, Body2, Nested, EndNested, Scope, EndScope) :-

    Scope ! error(no_new_scope_yet(Body1)),
      Body2 = true,
      Nested = EndNested,
      Scope' = EndScope.

/**** Utilities ****/

concatenate(List1, List2, List3) :-

    List1 ? Item :
      List3 ! Item |
	self;

    List1 =?= [], List2 ? Item :
      List3 ! Item |
	self;

    List1 =?= [], List2 =?= [] :
      List3 = [].

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
