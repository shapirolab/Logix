-monitor(serve).
-language([evaluate,compound,colon]).
-export([get_global_channels/1, global_channels/1, global_channels/2,
	 new_channel/3,
	 options/2, reset/0, scheduler/1]).

RAN =>  4.					/* uniform 0..1 variate */
LN  =>  9.					/* natural logarithm */

REALTIME => 12.

MAXTIME => 99999999999999999999999999999999999999999999999999999999999.0.

DEBUG(Note) => write_channel(debug_note(Note), NotesChannel).

%STOPPED => DEBUG((ChannelName:stopped!)).
STOPPED => ChannelName = _, NotesChannel = _.


serve(In) + (Options = []) :-

    In =?= [] :
      Options = _ |
	true;

    In =\= [] |
	server(In, Options, Scheduler),
	processor#link(lookup(math, Offset), Ok),
	start_scheduling(Scheduler, Offset, Ok).

server(In, Options, Scheduler) + (Globals = [{0, _, -1}, {[], _, -1}]) :-

    In ? global_channels(List) |
	merge_global_channels(List, Globals, Globals', "", Scheduler),
	self;

    In ? global_channels(List, Scheduler^) |
	merge_global_channels(List, Globals, Globals', "", Scheduler),
	self;

    In ? get_global_channels(List),
    we(List) :
      List = List'? |
	copy_global(Globals, List', _),
	self;

    In ? New , New = new_channel(_Creator, _Channel, _BaseRate) :
      write_channel(New, Scheduler) |
	self;

    In ? reset :
      Globals = _,
      close_channel(Scheduler) |
	serve;

    In ? options(New, Old) :
      Options' = New? |
	unify_without_failure(Options, Old),
	self;

    In ? debug(Debug^) :
      write_channel(debug(Debug), Scheduler) |
	self;

    In ? scheduler(Scheduler^) |
	self;

    In ? Other,
    otherwise |
	self,
	fail(Other, unknown);

    In =?= [] :
      Options = _,
      Globals = _,
      close_channel(Scheduler).


merge_global_channels(List, Globals, NewGlobals, Last, Scheduler) :-

    List =?= [] :
      Last = _,
      Scheduler = _,
      NewGlobals = Globals;

    List ? Name(NewChannel, BaseRate),
    Last @< Name,
    we(NewChannel),
    Globals ? Global, Global = Name(PsiChannel, BaseRate) :
      NewChannel = PsiChannel,
      NewGlobals ! Global,
      Last = _,
      Last' = Name |
	self;

    List ? Name(_NewChannel, BaseRate),
    Globals ? Name(_PsiChannel, OtherBaseRate),
    BaseRate =\= OtherBaseRate |
	fail(global_channel(rate_conflict(Name - BaseRate =\= OtherBaseRate))),
	self;

    List = [Name(_NewChannel, _BaseRate) | _], string(Name),
    Globals ? Entry,
    Entry = Last'(_, _),
    Last' @< Name :
      Last = _,
      NewGlobals ! Entry |
	self;

    List ? Name(NewChannel, BaseRate),
    we(NewChannel),
    Globals =?= [Name1(_, _) | _],
    string(Name),
    Last @< Name, Name @< Name1,
    string_to_dlist("global.", GL, GT),
    string_to_dlist(Name, NL, []) :
      NewChannel = NewChannel'?,
      List'' = [Name(NewChannel', BaseRate) | List'],
      Globals' = [Name(PsiChannel?, BaseRate) | Globals],
      write_channel(global_channel(Id?, PsiChannel, BaseRate), Scheduler),
      GT = NL |
	list_to_string(GL, Id),
	self;

    otherwise :
      Last = _,
      Scheduler = _,
      NewGlobals = Globals |
	fail(merge_global_channels(List)).


copy_global(Globals, List, Ends) :-
    Globals ? Head :
      Ends ! Head |
	copy_interior.

  copy_interior(Globals, List, Ends) :-

    Globals ? Entry,
    Globals' =\= [] :
      List ! Entry |
	self;

    Globals = [_] :
      List = [],
      Ends = Globals.


start_scheduling(Scheduler, Offset, Ok) :-

    Ok =?= true,
    info(REALTIME, Start_real_time),
    convert_to_real(0, Zero),
    convert_to_real(MAXTIME, MaxTime) :
      make_channel(Scheduler, Schedule),
      execute(Offset, {RAN, 0, Uniform}),
      execute(Offset, {LN, Uniform, NegativeExponential}),
      execute(Offset, {RAN, 0, Uniform'}) |
	processor#Waiter?,
	scheduling(Schedule, Offset, NegativeExponential, Uniform', Waiter,
				PutCommand, PutCommand?,
				Scheduler, _Recording, _Debug,
				Zero, false, _Wakeup, _Halt,
				MaxTime, Start_real_time);

    Ok =\= true :
      Scheduler = _,
      Offset = _ |
	fail(math_offset(Ok)).

/*
** scheduling monitors the stream generated using the scheduler channel.
**
** It recognises:
**
**    cutoff(Time)
**    global_channel(Creator, Channel, BaseRate)
**    input(Schedule?, Schedule')
**    new_channel(Creator, Channel, BaseRate)
**    pause(Continue)
**    record(Record?)
**    end_record(Record'?)
**    start(Token)
**    done(Token1, Token2)
**
** and the debugging aids:
**
**    debug(Debug?)
**
** Processing:
**
** Maintain time  Now  and a short-circuit psi-channel command list.
**
** Whenever the system becomes idle and the short-cicuit produces a non-
** zero weighted sum, use the short-circuit to select a channel which will
** complete a communication.
**
** Record:
**
**   Changes to  Now .
**   Selected processes .
*/

scheduling(Schedule, Offset, NegativeExponential, Uniform, Waiter,
		PutCommand, GetCommand,
		Scheduler, Record, Debug,
		Now, Waiting, Wakeup, Halt,
		Cutoff, Start_real_time) :-

    Schedule =?= [] :
      Cutoff = _,
      GetCommand = _,
      NegativeExponential = _,
      Now = _,
      Offset = _,
      Scheduler = _,
      Start_real_time = _,
      Uniform = _,
      Waiting = _,
      Wakeup = _,
      PutCommand = [],
      Halt = halt,
      Waiter = [],
      Record = [],
      Debug = [];

    /* Set the time limit - maximum value for Now */
    Schedule ? cutoff(Cutoff'), Cutoff' >= 0,
    info(REALTIME, Start_real_time') :
      Cutoff = _,
      Start_real_time = _ |
	continue_waiting;

    /* Splice input filter. */
    Schedule ? input(Schedule'', Schedule'^) |
	self;

    /* Create a new channel. */
    Schedule ? New , New = global_channel(Creator, Channel, BaseRate) |
	new_channel(Creator, Channel, BaseRate, Halt, global,
			Scheduler, PutCommand'?, PutCommand),
	continue_waiting;

    /* Create a new channel. */
    Schedule ? New , New = new_channel(Creator, Channel, BaseRate) |
	new_channel(Creator, Channel, BaseRate, Halt, stop,
			Scheduler, PutCommand'?, PutCommand),
	continue_waiting;

    /* Pause processing until Continue is set. */
    Schedule ? pause(Continue) |
	pause_scheduling(Continue,
			 Schedule', Schedule'',
			 GetCommand, GetCommand',
			 Waiting, Waiting',
			 Wakeup, Wakeup',
			 Start_real_time, Start_real_time'),
	self;

    /* Return the current head of the recording stream. */
    Schedule ? record(Stream) :
      Stream = Record? |
	self;

    /* Close the recording stream, and start a new one. */
    Schedule ? end_record(Stream),
    convert_to_real("0.0", Now') :
      Now = _,
      Record = [],
      Stream = Record'? |
	self;

    /* Record the start of a communication process. */
    Schedule ? Start, Start =?= start(Token, _) :
      Record ! start(Token),
      Debug ! Start |
	continue_waiting;
    Schedule ? Start, Start =?= start(_Token) :
      Record ! Start,
      Debug ! Start |
	continue_waiting;

    /* Record the end of an instantaneous communication. */
    Schedule ? Terminate, Terminate = done(Token1, Token2) :
      Record ! end(Token1),
      Record' ! end(Token2),
      Debug ! Terminate |
	self;

/***************************** Testing sum/select ****************************/

    Schedule ? step :
      PutCommand ! sum(0) |
	self;

    /* Bad news if this happens */
    GetCommand ? Select, Select = select(_, _) :
      Debug ! ((pi_monitor:Select)) |
	fail(scheduling-Select),
	self;

    GetCommand ? sum(S),
    number(S), S =< 0 :
      Debug ! idle(Now) |
	self;

    GetCommand ? sum(S),
    number(S), S > 0,
    Now -= NegativeExponential/S,
    Residue := Uniform*S :
      Wakeup = _,
      PutCommand ! select(Residue, Wakeup'),
      Debug ! now(Now'),
      Record ! Now',
      execute(Offset, {RAN, 0, Uniform'}),
      execute(Offset, {LN, Uniform', NegativeExponential'}),
      execute(Offset, {RAN, 0, Uniform''}) |
	self;

    Wakeup = done :
      Waiting = _,
      Waiting' = false,
      PutCommand ! sum(0) |
	self + (Wakeup = select);

    Wakeup =?= done(Sent, Received) :
      Waiting = _,
      Waiter ! machine(idle_wait(Wakeup'), _Ok),
      Waiting' = true,
      Record ! end(Sent),
      Record' ! end(Received),
      Debug ! Wakeup |
	self;

    Wakeup =?= retry(_) :
      Wakeup' = done,
      Debug ! Wakeup |
	self;

/**************************** Debugging code ********************************/

    Schedule ? debug(Stream) :
      Stream = Debug? |
	self;

    Schedule ? debug_note(Note) :
      Debug ! Note |
	self;

    Schedule ? end_debug :
      Debug = [],
      Debug' = _ |
	self;

    Schedule ? Other,
    otherwise |
	fail(Other),
	self;

/***************************************************************************/

    Now >= Cutoff,
    info(REALTIME, End_real_time),
    Real_time := End_real_time - Start_real_time  :
      GetCommand = _,
      NegativeExponential = _,
      Offset = _,
      Schedule = _,
      Scheduler = _,
      Uniform = _,
      Waiting = _,
      Wakeup = _,
      Waiter = [machine(idle_wait(Done), _Ok)],
      Record = [],
      Debug = [] |
	computation#display((done @ Now:
		seconds = Real_time)),
	wait_done.

  wait_done(Done, Halt, PutCommand) :-

    known(Done) :
      Halt = halt,
      PutCommand = [] |
	self#reset.

continue_waiting(Schedule, Offset, NegativeExponential, Uniform, Waiter,
		PutCommand, GetCommand,
		Scheduler, Record, Debug,
		Now, Waiting, Wakeup, Halt,
		Cutoff, Start_real_time) :-

    Waiting =?= true |
	scheduling;

    Waiting =\= true :
      Wakeup = _,
      Waiter ! machine(idle_wait(Wakeup'), _Ok),
      Waiting' = true |
	scheduling;

    /* check for paused. */
    unknown(Waiting) |
	scheduling.


pause_scheduling(Continue,
		 Schedule, ResetSchedule,
		 GetCommand, ResetGetCommand,
		 Waiting, ResetWaiting,
		 Wakeup, ResetWakeup,
		 Start_real_time, Reset_real_time) :-

    info(REALTIME, Pause_real_time),
    Biased_real_time := Start_real_time + Pause_real_time :
      SavedInput = SaveInput? |
	pause_continue.

  pause_continue(Continue,
		 Schedule, ResetSchedule,
		 GetCommand, ResetGetCommand,
		 Waiting, ResetWaiting,
		 Wakeup, ResetWakeup,
		 Biased_real_time, Reset_real_time,
		 SavedInput, SaveInput) :-

    Schedule ? Input :
      SaveInput ! Input |
	self;

    Continue =?= resume,
    info(REALTIME, Resume_real_time),
    Reset_real_time^ := Biased_real_time - Resume_real_time :
      ResetSchedule = SavedInput,
      SaveInput = Schedule,
      ResetGetCommand = GetCommand,
      ResetWaiting = Waiting,
      Wakeup = ResetWakeup.


new_channel(Creator, Channel, BaseRate, Halt, Terminator,
		NotesChannel, CommandIn, CommandOut) :-

    we(Channel) :
      make_channel(InputChannel, Input),
      Channel = Creator(InputChannel, {Stop, Terminator}) |
	prototype_or_instantaneous + (ChannelName = Creator);

    otherwise :
      Halt = _,
      Terminator = _,
      DEBUG(failed_new(Creator, Channel, BaseRate)),
      CommandOut = CommandIn |
	fail(new(Creator, Channel, BaseRate));

    /* wait for the BaseRate to be instantiated */
    CommandIn ? Command :
      CommandOut ! Command |
	self;

    CommandIn =?= [] :
      Creator = _,
      Channel = _,
      BaseRate = _,
      Halt = _,
      NotesChannel = _,
      Terminator = _,
      CommandOut = [].

  
  prototype_or_instantaneous(ChannelName, Input, InputChannel, BaseRate, Halt,
			NotesChannel, CommandIn, CommandOut, Stop) :-

    BaseRate =< 0 :
      Stop = _,
      CommandOut = CommandIn,
      DEBUG((ChannelName: sink)) |
	sink_channel + (Count = 0);

    BaseRate = test :
      Halt = _,
      DEBUG((ChannelName: test)) |
	test_channel + (Weight = 0);

    BaseRate =?= infinite :
      CommandOut = CommandIn |
	instantaneous_channel;

    BaseRate > 0,
    Input =?= [Dimer | _],
    arg(1, Dimer, dimer),
    BaseRate' := real(BaseRate) :
      Halt = _ |
	dimerized_channel;

    BaseRate > 0,
    Input =?= [Send | _],
    arg(1, Send, send),
    BaseRate' := real(BaseRate) :
      Halt = _ |
	bimolecular_channel;

    BaseRate > 0,
    Input =?= [Receive | _],
    arg(1, Receive, receive),
    BaseRate' := real(BaseRate) :
      Halt = _ |
	bimolecular_channel;

    Input ? inspect(Head, Tail) :
      Head ! indeterminate(ChannelName(BaseRate), []),
      Head' = Tail |
	self;

    otherwise :
      Halt = _,
      Input = _,
      NotesChannel = _,
      Stop = _,
      CommandOut = CommandIn,
      close_channel(InputChannel) |
	fail(("invalid base rate" : ChannelName - BaseRate));

    /* wait for first message */
    CommandIn ? Command :
      CommandOut ! Command |
	self;

    Stop =?= stop :
      BaseRate = _,
      ChannelName = _,
      Halt = _,
      Input = _,
      NotesChannel = _,
      Stop = _,
      CommandOut = CommandIn,
      close_channel(InputChannel);

    CommandIn =?= [] :
      BaseRate = _,
      ChannelName = _,
      Halt = _,
      Input = _,
      NotesChannel = _,
      Stop = _,
      CommandOut = [],
      close_channel(InputChannel).


sink_channel(ChannelName, Input, InputChannel, NotesChannel,
			Halt, Stop, Count) :-

    Input ? Request, tuple(Request), arity(Request) =:= 6,
    arg(1, Request, Functor), string(Functor),
    Count++ |
	self;

    Input ? Request, Request =?= withdraw(_, _),
    Count-- |
	self;

    Input ? inspect(Head, Tail) :
      Head ! test(ChannelName(Count), []),
      Head' = Tail |
	self;

    Input ? Other,
    otherwise :
      DEBUG((ChannelName:unknown - Other)) |
	self;

    Input =?= [] :
      ChannelName = _,
      Count = _,
      Halt = _,
      Stop = _,
      NotesChannel = _,
      close_channel(InputChannel);

    Halt = halt :
      ChannelName = _,
      Count = _,
      Input = _,
      NotesChannel = _,
      Stop = _ ,
      close_channel(InputChannel);

    unknown(Input),
    Stop =?= stop :
      ChannelName = _,
      Count = _,
      Halt = _,
      NotesChannel = _,
      close_channel(InputChannel).
	

test_channel(ChannelName, Input, InputChannel,
	NotesChannel, CommandIn, CommandOut, Stop, Weight) :-

    Input ? Request, tuple(Request), arity(Request) =:= 6,
    arg(1, Request, Functor), string(Functor),
    arg(5, Request, N), integer(N),
    Weight += N :
      DEBUG((ChannelName:request = Request)) |
	self;

    Input ? Request, Request =?= withdraw(_, N),
    Weight -= N :
      DEBUG((ChannelName:request = Request)) |
	self;

    Input ? inspect(Head, Tail) :
      Head ! test(ChannelName(Weight), []),
      Head' = Tail |
	self;

    Input ? Other,
    otherwise :
      DEBUG((ChannelName:unknown - Other)) |
	self;

    Input =?= [] :
      Stop = _,
      CommandOut = CommandIn,
      close_channel(InputChannel),
      DEBUG((ChannelName:closed - Weight));

    unknown(Input),
    Stop =?= stop :
      Input = _,
      Weight = _,
      CommandOut = CommandIn,
      close_channel(InputChannel),
      DEBUG((ChannelName:stopped!));

    CommandIn ? Command :
      CommandOut ! Command,
      DEBUG((ChannelName:command = Command)) |
	self;

    CommandIn = [] :
      ChannelName = _,
      Weight = _,
      Input = _,
      InputChannel = _,
      NotesChannel = _,
      Stop = _,
      CommandOut = [].


dimerized_channel(ChannelName, Input, InputChannel, BaseRate,
		NotesChannel, CommandIn, CommandOut, Stop) :-    
	dimerized + (Requests = AddRequests?, Weight = 0, Blocked = false).
dimerized(ChannelName, Input, InputChannel, BaseRate,
	  NotesChannel, CommandIn, CommandOut, Stop,
	  Requests, AddRequests, Weight, Blocked) :-

    Input ? Request, Request =?= dimer(_, _, _, Multiplier, _),
    Weight += Multiplier :
      Blocked = _,
      Blocked' = false,
      AddRequests ! Request |
	self;

    Input ? withdraw(dimer, N),
    Weight -= N |
	self;

    Input ? inspect(Head, Tail),
    RateWeight := BaseRate*(Weight*(Weight-1))/2 :
      Head ! dimerized(ChannelName(RateWeight), Content?),
      Head' = Tail |
	copy_requests(Requests, Content, []),
	self;

    Input ? Request, arg(1, Request, send) |
	fail(("send on dimerized channel" : ChannelName - Request)),
	self;

    Input ? Request, arg(1, Request, receive) |
	fail(("receive on dimerized channel" : ChannelName - Request)),
	self;

    Input ? Other,
    otherwise |
	fail(("unrecognized request" : ChannelName - Other)),
	self;

    Input = [] :
      BaseRate = _,
      Stop = _,
      InputChannel = _,
      Weight = _,
      Blocked = _,
      CommandOut = CommandIn,
      AddRequests = [],
      DEBUG((ChannelName:closed(Requests)));

    unknown(Input),
    Stop =?= stop :
      BaseRate = _,
      Weight = _,
      Blocked = _,
      CommandOut = CommandIn,
      close_channel(InputChannel),
      AddRequests = [],
      DEBUG((ChannelName:stopped(Requests)));

    CommandIn ? sum(Cumulant),
    Blocked =?= false,
    Cumulant' := Cumulant + BaseRate*(Weight*(Weight-1))/2 :
      CommandOut ! sum(Cumulant') |
	self;

    CommandIn ? sum(Cumulant),
    Blocked =?= true :
      CommandOut ! sum(Cumulant) |
	self;

    CommandIn ? select(Cumulant, Selected),
    Blocked =?= false,
    Cumulant' := Cumulant - BaseRate*(Weight*(Weight-1))/2,
    Cumulant' >= 0 :
      CommandOut ! select(Cumulant', Selected) |
	self;

    CommandIn ? select(Cumulant, Selected),
    Blocked =?= true :
      CommandOut ! select(Cumulant, Selected) |
	self;

    CommandIn ? select(Cumulant, Selected),
    Blocked =?= false,
    Cumulant' := Cumulant - BaseRate*(Weight*(Weight-1))/2,
    Cumulant' < 0 |
	get_active_request(Requests, Requests', Request),
	dimerized_transmit + (Requests1 = AddRequests1?);

    CommandIn = [] :
      ChannelName = _,
      BaseRate = _,
      Stop = _,
      Input = _,
      NotesChannel = _,
      Requests = _,
      AddRequests = _,
      Weight = _,
      Blocked = _,
      close_channel(InputChannel),
      CommandOut = CommandIn.

  dimerized_transmit(ChannelName, Input, InputChannel, BaseRate,
		NotesChannel, CommandIn, CommandOut, Stop,
		Requests, AddRequests, Weight, Blocked,
		Requests1, AddRequests1, Request, Selected) :-

    /* Maybe we should randomize the choice of send/receive. */
    Requests ? _(ReceiveId, ReceiveMessage, {_SendTag, ReceiveTag},
			ReceiveMultiplier, ReceiveReply),
    we(ReceiveReply),
    Request =?= _(SendId, SendMessage, {SendTag, _ReceiveTag},
			SendMultiplier, SendReply),
    we(SendReply),
    Weight -= ReceiveMultiplier + SendMultiplier :
      AddRequests1 = Requests'?,
      ReceiveMessage = SendMessage,
      ReceiveReply = ReceiveTag,
      SendReply = SendTag,
      Selected = done(SendId, ReceiveId) |
	dimerized + (Requests = Requests1);

    /* Both have the same Reply - unlikely, but possible. */
    Requests ? Request1, Request1 =?= _(_, _, _, _, Reply1),
    we(Reply1),
    Request = _(_, _, _, _, Reply2),
    we(Reply2),
    Reply1 =?= Reply2 :
      AddRequests1 ! Request1 |
	self;

    /* Discard withdrawn requests. */ 
    Requests ? _(_, _, _, _, Reply),
    not_we(Reply) |
	self;

    /* Request already withdrawn. */
    Request =?= _(_, _,  _, _, Reply), not_we(Reply) :
      AddRequests1 = Requests,
      Selected = retry(ChannelName) |
	dimerized + (Requests = Requests1);

    /* All requests from the same process instance. */
    Request =\= [],
    unknown(Requests) :
      Blocked = _,
      AddRequests = _,
      Selected = retry(ChannelName),
      Blocked' = true |
	dimerized +
		(Requests = [Request | Requests1], AddRequests = AddRequests1);

    /* Never found an active request in the first place. */
    Request =?= [],
    unknown(Requests) :
      Requests' = Requests1,
      AddRequests1 = Requests,
      Selected = retry(ChannelName) |
	self.

	
bimolecular_channel(ChannelName, Input, InputChannel, BaseRate,
		NotesChannel, CommandIn, CommandOut, Stop) :-    
	bimolecular + (Sends = AddSends?, Receives = AddReceives?,
		SendWeight = 0, ReceiveWeight = 0, Blocked = false).
bimolecular(ChannelName, Input, InputChannel, BaseRate,
	    NotesChannel, CommandIn, CommandOut, Stop,
	    Sends, AddSends, SendWeight,
	    Receives, AddReceives, ReceiveWeight, Blocked) :-

    Input ? Request, Request =?= send(_, _, _, Multiplier, _),
    SendWeight += Multiplier :
      Blocked = _,
      Blocked' = false,
      AddSends ! Request |
	self;

    Input ? Request, Request =?= receive(_, _, _, Multiplier, _),
    ReceiveWeight += Multiplier :
      Blocked = _,
      Blocked' = false,
      AddReceives ! Request |
	self;

    Input ? withdraw(send, N),
    SendWeight -= N |
	self;

    Input ? withdraw(receive, N),
    ReceiveWeight -= N |
	self;

    Input ? inspect(Head, Tail),
    RateWeight := BaseRate*SendWeight*ReceiveWeight :
      Head ! bimolecular(ChannelName(RateWeight), Content?),
      Head' = Tail |
	copy_requests(Sends, Content, Content'?),
	copy_requests(Receives, Content', []),
	self;

    Input ? Other,
    otherwise |
	fail(("unrecognized request" : ChannelName - Other)),
	self;

    Input = [] :
      BaseRate = _,
      Blocked = _,
      InputChannel = _,
      ReceiveWeight = _,
      SendWeight = _,
      Stop = _,
      CommandOut = CommandIn,
      AddSends = Receives,
      AddReceives = [],
      DEBUG((ChannelName:closed(Sends)));

    unknown(Input),
    Stop =?= stop :
      BaseRate = _,
      Blocked = _,
      ReceiveWeight = _,
      SendWeight = _,
      CommandOut = CommandIn,
      close_channel(InputChannel),
      AddSends = Receives,
      AddReceives = [],
      DEBUG((ChannelName:stopped(Sends)));

    CommandIn ? sum(Cumulant),
    Blocked =?= false,
    Cumulant' := Cumulant + BaseRate*SendWeight*ReceiveWeight :
      CommandOut ! sum(Cumulant') |
	self;

    CommandIn ? sum(Cumulant),
    Blocked =?= true :
      CommandOut ! sum(Cumulant) |
	self;

    CommandIn ? select(Cumulant, Selected),
    Blocked =?= false,
    Cumulant' := Cumulant - BaseRate*SendWeight*ReceiveWeight,
    Cumulant' >= 0 :
      CommandOut ! select(Cumulant', Selected) |
	self;

    CommandIn ? select(Cumulant, Selected),
    Blocked =?= true :
      CommandOut ! select(Cumulant, Selected) |
	self;

    CommandIn ? select(Cumulant, Selected),
    Blocked = false,
    Cumulant' := Cumulant - BaseRate*SendWeight*ReceiveWeight,
    Cumulant' < 0 |
	get_active_request(Sends, Sends', Send),
	bimolecular_send + (Receives1 = AddReceives1?);

    CommandIn = [] :
      AddReceives = _,
      AddSends = _,
      BaseRate = _,
      Blocked = _,
      ChannelName = _,
      Input = _,
      NotesChannel = _,
      Receives = _,
      ReceiveWeight = _,
      Sends = _,
      SendWeight = _,
      Stop = _,
      close_channel(InputChannel),
      CommandOut = CommandIn.


bimolecular_send(ChannelName, Input, InputChannel, BaseRate,
		 NotesChannel, CommandIn, CommandOut, Stop,
		 Sends, AddSends, SendWeight,
		 Receives, AddReceives, ReceiveWeight, Blocked,
		 Receives1, AddReceives1, Send, Selected) :-


    /* This shouldn't happen. */
    Send =?= _(_, _, _, _, Reply), not_we(Reply) :
      AddReceives1 = Receives,
      Selected = retry(ChannelName) |
	bimolecular + (Receives = Receives1);

    /* This shouldn't happen either. */
    Send =?= _(_, _, _, _, Reply), we(Reply),
    unknown(Receives),
    unknown(Receives1) :
      AddReceives1 = _,
      Blocked = _,
      Selected = retry(ChannelName),
      Blocked' = true |
	bimolecular + (Sends = [Send | Sends]);

    Send =?= _(_, _, _, _, Reply), we(Reply),
    unknown(Receives),
    known(Receives1) :
      AddReceives = _ |
	get_active_request(Receives1, Receives', Receive),
	bimolecular_receive +
		(Sends1 = [Send | AddSends1?], AddReceives = AddReceives1);

    Receives ? _(_, _, _, _, Reply), not_we(Reply) |
	self;

    Receives ? _(Id1, Ms1, Tag1, ReceiveMultiplier, Reply1), we(Reply1),
    Send =?=  _(Id2, Ms2, Tag2, SendMultiplier, Reply2), we(Reply2),
    SendWeight -= SendMultiplier,
    ReceiveWeight -= ReceiveMultiplier :
      Ms1 = Ms2,
      Reply1 = Tag1,
      Reply2 = Tag2,
      Selected = done(Id1, Id2),
      AddReceives1 = Receives'? |
	bimolecular + (Receives = Receives1);

    /* This Receive has the same Reply as the Send.
       Maybe some other receive will do the trick. */
    Receives ? Receive, Receive = _(_, _, _, _, Reply),
    Send =?= _(_, _, _, _, Reply) :
      AddReceives1 ! Receive |
	self;

    /* Never found an active send in the first place. */
    Send =?= [] :
      AddReceives1 = Receives?,
      Selected = retry(ChannelName) |
	bimolecular + (Receives = Receives1).


bimolecular_receive(ChannelName, Input, InputChannel, BaseRate,
		    NotesChannel, CommandIn, CommandOut, Stop,
		    Sends, AddSends, SendWeight,
		    Receives, AddReceives, ReceiveWeight, Blocked,
		    Sends1, AddSends1, Receive, Selected) :-

    /* This shouldn't happen. */
    Receive =?= _(_, _, _, _, Reply), not_we(Reply) :
      AddSends1 = Sends,
      Selected = retry(ChannelName) |
	bimolecular + (Sends = Sends1);

    Receive =?= _(_, _, _, _, Reply), we(Reply),
    unknown(Sends) :
      AddSends = _,
      Blocked = _,
      Selected = retry(ChannelName),
      Blocked' = true |
	bimolecular + (Sends = Sends1, AddSends = AddSends1,
			Receives = [Receive | Receives]);

    Sends ? _(_, _, _, _, Reply), not_we(Reply) |
	self;

    Sends ? _(Id1, Ms1, Tag1, SendMultiplier, Reply1), we(Reply1),
    Receive =?=  _(Id2, Ms2, Tag2, ReceiveMultiplier, Reply2), we(Reply2),
    SendWeight -= SendMultiplier,
    ReceiveWeight -= ReceiveMultiplier :
      Ms1 = Ms2,
      Reply1 = Tag1,
      Reply2 = Tag2,
      Selected = done(Id1, Id2),
      AddSends1 = Sends'? |
	bimolecular + (Sends = Sends1);

    /* This Send has the same Reply as the Receive - mixed communication. */
    Sends ? Send, Send = _(_, _, _, _, Reply),
    Receive =?= _(_, _, _, _, Reply) :
      AddSends1 ! Send |
	self;

    /* Never found an active receive in the first place. */
    Receive =?= [] :
      AddSends1 = Sends,
      Selected = retry(ChannelName) |
	bimolecular + (Sends = Sends1).



instantaneous_channel(ChannelName, Input, InputChannel, NotesChannel,
					Halt, Stop) :-
	instantaneous + (Receives = AddReceives?, Sends = AddSends?).

instantaneous(ChannelName, Input, InputChannel,	NotesChannel,
				Halt, Stop,
		Receives, AddReceives, Sends, AddSends) :-

    Input ? Send, arg(1, Send, send) |
	instantaneous_send + (Receives1 = AddReceives1?);

    Input ? Receive, arg(1, Receive, receive) |
	instantaneous_receive + (Sends1 = AddSends1?);

    Input ? Request, arg(1, Request, dimer) |
	fail(("dimerized operation on instantaneous channel" : ChannelName)),
	self;

    /* Ignore withdraw requests. */
    Input ? withdraw(_, _) |
	self;

    Input ? inspect(Head, Tail) :
      Head ! instantaneous(ChannelName(infinite), Content?),
      Head' = Tail |
	copy_requests(Sends, Content, Content'?),
	copy_requests(Receives, Content', []),
	self;

    Input ? Other,
    otherwise |
	fail(("unrecognized request" : ChannelName - Other)),
	self;

    Input = [] :
      ChannelName = _,
      InputChannel = _,
      NotesChannel = _,
      Halt = _,
      Stop = _,
      Receives = _,
      AddReceives = _,
      Sends = _,
      AddSends = _;

    unknown(Input),
    Stop =?= stop :
      Halt = _,
      AddSends = Receives,
      AddReceives = [],
      close_channel(InputChannel),
      DEBUG((ChannelName:stopped(Sends)));

    Halt = halt :
      ChannelName = _,
      Input = _,
      NotesChannel = _,
      Stop = _,
      Receives = _,
      AddReceives = _,
      Sends = _,
      AddSends = _,
      close_channel(InputChannel).


instantaneous_receive(ChannelName, Input, InputChannel, NotesChannel,
					Halt, Stop,
	Receives, AddReceives, Sends, AddSends, Receive, Sends1, AddSends1) :-

    /* This shouldn't happen. */
    Receive = _(_, _, _, _, Reply), not_we(Reply) :
      Sends' = Sends1,
      AddSends1 = Sends |
	instantaneous;

    /* This shouldn't happen either. */
    Receive = _(_, _, _, _, Reply), we(Reply),
    unknown(Sends) :
      Sends' = Sends1,
      AddSends1 = Sends |
	instantaneous + (Receives = [Receive | Receives]);

    Sends ? _(_, _, _, _, Reply), not_we(Reply) |
	self;

    Sends ? _(Id1, Ms1, Tag1, _, Reply1), we(Reply1),
    Receive =?=  _(Id2, Ms2, Tag2, _, Reply2), we(Reply2) :
      Ms1 = Ms2,
      Reply1 = Tag1,
      Reply2 = Tag2,
      write_channel(done(Id1, Id2), NotesChannel),
      Sends'' = Sends1,
      AddSends1 = Sends'? |
	instantaneous;

    /* This Send has the same Reply as the Receive - mixed communication. */
    Sends ? Send, Send = _(_, _, _, _, Reply),
    Receive =?= _(_, _, _, _, Reply) :
      AddSends1 ! Send |
	self.



instantaneous_send(Send, ChannelName, Input, InputChannel, NotesChannel,
				Halt, Stop,
	Receives, AddReceives, Sends, AddSends, Receives1, AddReceives1) :-

    /* This shouldn't happen. */
    Send =?= _(_, _, _, _, Reply), not_we(Reply) :
      Receives' = Receives1,
      AddReceives1 = Receives |
	instantaneous;

    /* This shouldn't happen either. */
    Send =?= _(_, _, _, _, Reply), we(Reply),
    unknown(Receives) :
      Receives' = Receives1,
      AddReceives1 = Receives |
	instantaneous + (Sends = [Send | Sends]);

    Receives ? _(_, _, _, _, Reply), not_we(Reply) |
	self;

    Receives ? _(Id1, Ms1, Tag1, _, Reply1), we(Reply1),
    Send =?=  _(Id2, Ms2, Tag2, _, Reply2), we(Reply2) :
      Ms1 = Ms2,
      Reply1 = Tag1,
      Reply2 = Tag2,
      write_channel(done(Id1, Id2), NotesChannel),
      Receives'' = Receives1,
      AddReceives1 = Receives'? |
	instantaneous;

    /* This Receive has the same Reply as the Send - mixed communication. */
    Receives ? Receive, Receive = _(_, _, _, _, Reply),
    Send =?= _(_, _, _, _, Reply) :
      AddReceives1 ! Receive |
	self.


/***************************** Utilities ************************************/

copy_requests(Requests, Head, Tail) :-

    Requests ? Request :
      Head ! Request |
	self;

    unknown(Requests) :
      Head = Tail.


get_active_request(Requests, NextRequests, Request) :-

    Requests ? R, A := arity(R),
    arg(A, R, Reply), unknown(Reply) :
      NextRequests = Requests',
      Request = R;

    Requests ? R, A := arity(R),
    arg(A, R, Reply), known(Reply) |
	self;

    unknown(Requests) :
      NextRequests = Requests,
      Request = [].
