-monitor(serve).
-language([evaluate,compound,colon]).
-export([get_global_channels/1, global_channels/1, global_channels/2,
	 new_channel/3, new_channel/4,
	 options/2, reset/0, scheduler/1]).
-include(psi_constants).

RAN =>  4.					/* uniform 0..1 variate */
LN  =>  9.					/* natural logarithm */

REALTIME => 12.

MAXTIME => 99999999999999999999999999999999999999999999999999999999999.0.

DEBUG(Note) => write_channel(debug_note(Note), Scheduler).

%STOPPED => DEBUG(stopped(Reply)).
STOPPED => Reply = _.

/*
** Arguments of PSIOFFSETS correspond to the C-executable (pcicomm.c)
** sub-functions, Close, Post, Step, Index.
**
** To test one or more C-executables, set the other arguments "unbound" - e.g. 
** 
** PSIOFFSETS => {unbound, unbound, PsiOffset, PsiOffset}
**
** to allow the monitor to call the C step function and the C index function,
** and to execute the other operations with fcp code.
*/

PSIOFFSETS => {PsiOffset, PsiOffset, PsiOffset, PsiOffset}.

serve(In) + (Options = []) :-

    In =?= [] :
      Options = _;

    In =\= [] :
      PsiOffset = _,
      Ordinal = 1 |
	server(In, Options, Scheduler),
	processor#link(lookup(math, Offset), Ok),
	processor#link(lookup(psicomm, PsiOffset), _Ok),
	start_scheduling(Scheduler, Offset, Ordinal, PSIOFFSETS,
		PSI_DEFAULT_WEIGHT_NAME(PSI_DEFAULT_WEIGHT_INDEX), Ok).

server(In, Options, Scheduler) +
	(Globals = [{0, _, -1, ""}, {[], _, -1, ""}]) :-

    In ? debug(Debug) :
      Debug = Debug'?,
      write_channel(debug(Debug'), Scheduler) |
	self;

    In ? end_debug :
      write_channel(end_debug, Scheduler) |
	self;

    In ? end_record(Stream) :
      Stream = Stream'?,
      write_channel(end_record(Stream'), Scheduler) |
	self;

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

    In ? New , New = new_channel(_Creator, _Channel, _ComputeWeight,
					_BaseRate) :
      write_channel(New, Scheduler) |
	self;

    In ? options(New, Old) :
      Options' = New? |
	unify_without_failure(Options, Old),
	self;

    In ? psifunctions(List) :
      write_channel(psifunctions(List), Scheduler) |
	self;

    In ? record(Record^) :
      write_channel(record(Record), Scheduler) |
	self;

    In ? reset :
      Globals = _,
      close_channel(Scheduler) |
	serve;

    In ? scheduler(Scheduler^) |
	self;

    In ? status(Status) :
      Status = Status'?,
      write_channel(status(Status'), Scheduler) |
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
    Globals ? Global, Global = Name(PsiChannel, _ComputeWeight, BaseRate),
    vector(PsiChannel),
    read_vector(PSI_CHANNEL_REFS, PsiChannel, References),
    References++ :
      Last = _,
      NewChannel = PsiChannel,
      store_vector(PSI_CHANNEL_REFS, References', PsiChannel),
      NewGlobals ! Global,
      Last' = Name |
	self;

    List ? Name(_NewChannel, BaseRate),
    Globals ? Entry, Entry = Name(_PsiChannel, _ComputeWeight, OtherBaseRate),
    BaseRate =\= OtherBaseRate :
      NewGlobals ! Entry |
	fail(global_channel(rate_conflict(Name - BaseRate =\= OtherBaseRate))),
	self;

    List = [Name(_NewChannel, _BaseRate) | _], string(Name),
    Globals ? Entry,
    Entry = Last'(_, _, _),
    Last' @< Name :
      Last = _,
      NewGlobals ! Entry |
	self;

    List ? Name(NewChannel, BaseRate),
    we(NewChannel),
    Globals =?= [Name1(_, _, _) | _],
    string(Name),
    Last @< Name, Name @< Name1,
    string_to_dlist("global.", GL, GT),
    string_to_dlist(Name, NL, []) :
      NewChannel = NewChannel'?,
      List'' = [Name(NewChannel', BaseRate) | List'],
      Globals' =
	[Name(PsiChannel?, PSI_DEFAULT_WEIGHT_NAME, BaseRate) | Globals],
      write_channel(new_channel(Id?, PsiChannel, PSI_DEFAULT_WEIGHT_NAME,
				BaseRate), Scheduler),
      GT = NL |
	list_to_string(GL, Id),
	self;

    List ? Name(NewChannel, CW, BaseRate),
    Last @< Name,
    we(NewChannel),
    Globals ? Global, Global = Name(PsiChannel, ComputeWeight, BaseRate),
    vector(PsiChannel),
    read_vector(PSI_CHANNEL_REFS, PsiChannel, References),
    References++ :
      Last = _,
      CW = ComputeWeight?,
      NewChannel = PsiChannel,
      store_vector(PSI_CHANNEL_REFS, References', PsiChannel),
      NewGlobals ! Global,
      Last' = Name |
	self;

    List ? Name(_NewChannel, BaseRate, _),
    Globals ? Entry, Entry = Name(_PsiChannel, OtherBaseRate, _),
    BaseRate =\= OtherBaseRate :
      NewGlobals ! Entry |
	fail(global_channel(rate_conflict(Name - BaseRate =\= OtherBaseRate))),
	self;

    List ? Name(_NewChannel, ComputeWeight, _),
    Globals ? Entry, Entry = Name(_PsiChannel, OtherComputeWeight, _),
    ComputeWeight =\= OtherComputeWeight :
      NewGlobals ! Entry |
	fail(global_channel(compute_weight_conflict(Name -
				ComputeWeight =\= OtherComputeWeight))),
	self;

    List = [Name(_NewChannel, _ComputeWeight, _BaseRate) | _], string(Name),
    Globals ? Entry,
    Entry = Last'(_, _, _),
    Last' @< Name :
      Last = _,
      NewGlobals ! Entry |
	self;

    List ? Name(NewChannel, ComputeWeight, BaseRate),
    we(NewChannel),
    Globals =?= [Name1(_, _, _) | _],
    string(Name),
    Last @< Name, Name @< Name1,
    string_to_dlist("global.", GL, GT),
    string_to_dlist(Name, NL, []) :
      NewChannel = NewChannel'?,
      List'' = [Name(NewChannel', ComputeWeight, BaseRate) | List'],
      Globals' = [Name(PsiChannel?, ComputeWeight, BaseRate) | Globals],
      write_channel(new_channel(Id?, PsiChannel, ComputeWeight, BaseRate),
			Scheduler),
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

/***************************** Scheduling ***********************************/ 

start_scheduling(Scheduler, Offset, Ordinal, PsiOffsets, DefaultWeighter, Ok):-

    Ok =?= true,
    info(REALTIME, Start_real_time),
    convert_to_real(0, Zero),
    convert_to_real(MAXTIME, MaxTime) :
      execute(Offset, {RAN, 0, Uniform}),
      execute(Offset, {LN, Uniform, NegativeExponential}),
      execute(Offset, {RAN, 0, Uniform'}),
      make_channel(Scheduler, Schedule) |
	make_channel_anchor(based, BasedAnchor),
	make_channel_anchor(instantaneous, InstantaneousAnchor),
	processor#Waiter?,
	scheduling(Schedule, Offset, Ordinal, PsiOffsets, Waiter,
				NegativeExponential, Uniform',
				BasedAnchor, InstantaneousAnchor,
				Scheduler, _Recording, _Debug,
				Zero, false, _Wakeup,
				DefaultWeighter, MaxTime, Start_real_time);

    Ok =\= true :
      DefaultWeighter = _,
      Scheduler = _,
      Offset = _,
      Ordinal = _,
      PsiOffsets = _ |
	fail(math_offset(Ok)).


make_channel_anchor(Name, Anchor) :-

    convert_to_real(0, Zero) :
      make_vector(CHANNEL_SIZE, Anchor, _),
      store_vector(PSI_BLOCKED, FALSE, Anchor),
      store_vector(PSI_CHANNEL_TYPE, PSI_CHANNEL_ANCHOR, Anchor),
      store_vector(PSI_CHANNEL_RATE, Zero, Anchor),
      store_vector(PSI_CHANNEL_REFS, 1, Anchor),
      store_vector(PSI_SEND_ANCHOR, SendAnchor, Anchor),
       make_channel(NextS, _),
       make_channel(PrevS, _),
       SendAnchor = {PSI_MESSAGE_ANCHOR, "", [], 0, 0, [], NextS, PrevS},
       store_vector(1, SendAnchor, NextS),
       store_vector(1, SendAnchor, PrevS),
      store_vector(PSI_SEND_WEIGHT, 0, Anchor),
      store_vector(PSI_RECEIVE_ANCHOR, ReceiveAnchor, Anchor),
       make_channel(NextR, _),
       make_channel(PrevR, _),
       ReceiveAnchor = {PSI_MESSAGE_ANCHOR, "", [], 0, 0, [], NextR, PrevR},
       store_vector(1, ReceiveAnchor, NextR),
       store_vector(1, ReceiveAnchor, PrevR),
      store_vector(PSI_RECEIVE_WEIGHT, 0, Anchor),
      store_vector(PSI_WEIGHT_TUPLE, 
		PSI_DEFAULT_WEIGHT_NAME(PSI_DEFAULT_WEIGHT_INDEX), Anchor),
      store_vector(PSI_NEXT_CHANNEL, Anchor, Anchor),
      store_vector(PSI_PREVIOUS_CHANNEL, Anchor, Anchor),
      store_vector(PSI_CHANNEL_NAME, Name, Anchor).

/*
** scheduling monitors the stream generated using the scheduler channel.
**
** It recognises:
**
**    close(Tuple)
**    cutoff(Time)
**    input(Schedule?^, Schedule')
**    new_channel(ChannelName, Channel, BaseRate)
**    new_channel(ChannelName, Channel, ComputeWeight, BaseRate)
**    ordinal(Old?^, New)
**    pause(Continue)
**    record(Record?^)
**    end_record(Record?^)
**    start(String, OpList, Value, Chosen)
**    status(List^)
**    step(Continue)
**
** and the debugging aids:
**
**    debug(Debug?^)
**    end_debug
**
** Processing:
**
** Maintain time  Now
**
** Whenever the system becomes idle, execute(PsiOffset, {PSI_STEP, ...})
** to select and  complete a transmission.
**
** Record:
**
**   Changes to  Now .
**   Selected processes .
*/

scheduling(Schedule, Offset, Ordinal, PsiOffsets, Waiter,
		NegativeExponential, Uniform,
		BasedAnchor, InstantaneousAnchor,
		Scheduler, Record, Debug,
		Now, Waiting, Wakeup,
		DefaultWeighter, Cutoff, Start_real_time) :-

    Schedule =?= [] :
      Cutoff = _,
      BasedAnchor = _,
      DefaultWeighter = _,
      InstantaneousAnchor = _,
      NegativeExponential = _,
      Now = _,
      Offset = _,
      Ordinal = _,
      PsiOffsets = _,
      Scheduler = _,
      Start_real_time = _,
      Uniform = _,
      Waiting = _,
      Wakeup = _,
      Waiter = [],
      Record = [],
      Debug = [];

    /* Set the time limit - maximum value for Now */
    Schedule ? cutoff(Cutoff'), Cutoff' >= 0,
    info(REALTIME, Start_real_time') :
      Cutoff = _,
      Start_real_time = _ |
	continue_waiting + (Reply = true);

    /* Close channels - i.e. decrement counts and release when unreferenced. */
    Schedule ? close(Channels),
    arg(PSI_CLOSE, PsiOffsets, PsiOffset),
    PsiOffset =?= unbound :
      STOPPED |
	execute(Offset, close(Channels, Reply)),
	self;

    Schedule ? close(Channels),
    arg(PSI_CLOSE, PsiOffsets, PsiOffset),
    PsiOffset =\= unbound :
      execute(PsiOffset, {PSI_CLOSE, Channels, Reply}),
      STOPPED |
	self;

    Schedule ? default_weighter(Weighter),
    arg(PSI_INDEX, PsiOffsets, PsiOffset),
    PsiOffset =\= unbound |
	reset_default_weighter(PsiOffset, Weighter, DefaultWeighter,
				DefaultWeighter'),
	self;

    /* Splice input filter. */
    Schedule ? input(Schedule'', Schedule'^) |
	self;

    Schedule ? ordinal(Ordinal', Ordinal^) |
	self;

    /* Create a new channel. */
    Schedule ? new_channel(ChannelName, Channel, BaseRate) |
	index_channel_name(ChannelName, Ordinal, ChannelName', Ordinal'),
	new_channel + (ComputeWeight = DefaultWeighter),
	continue_waiting;

    Schedule ? new_channel(ChannelName, Channel, ComputeWeight, BaseRate),
    string(ComputeWeight) |
	index_channel_name(ChannelName, Ordinal, ChannelName', Ordinal'),
	new_channel + (ComputeWeight = ComputeWeight(_)),
	continue_waiting;

    Schedule ? new_channel(ChannelName, Channel, ComputeWeight, BaseRate),
    tuple(ComputeWeight), arity(ComputeWeight) > 1 |
	index_channel_name(ChannelName, Ordinal, ChannelName', Ordinal'),
	new_channel,
	continue_waiting;

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

    /* Start a transmission process. */
    Schedule ? Start, Start =?= start(PId, OpList, Value, Chosen),
    arg(PSI_POST, PsiOffsets, PsiOffset),
    PsiOffset =?= unbound,
    string(PId) :
      Record ! start(PId),
      Debug ! start(PId) |
	execute(Offset, post(PId, OpList, Value, Chosen, Reply)),
	continue_waiting;

    Schedule ? Start, Start =?= start(PId, OpList, Value, Chosen),
    arg(PSI_POST, PsiOffsets, PsiOffset),
    PsiOffset =?= unbound,
    tuple(PId), arg(1, PId, PName) :
      Record ! start(PName),
      Debug ! start(PId) |
	execute(Offset, post(PId, OpList, Value, Chosen, Reply)),
	continue_waiting;

    Schedule ? Start, Start =?= start(PId, OpList, Value, Chosen),
    arg(PSI_POST, PsiOffsets, PsiOffset),
    PsiOffset =\= unbound,
    string(PId) :
      execute(PsiOffset, {PSI_POST, PId, OpList, Value, Chosen, Reply}),
      Record ! start(PId),
      Debug ! start(PId) |
	continue_waiting;

    Schedule ? Start, Start =?= start(PId, OpList, Value, Chosen),
    arg(PSI_POST, PsiOffsets, PsiOffset),
    PsiOffset =\= unbound,
    tuple(PId), arg(1, PId, PName) :
      execute(PsiOffset, {PSI_POST, PId, OpList, Value, Chosen, Reply}),
      Record ! start(PName),
      Debug ! start(PId) |
	continue_waiting;

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

    Schedule ? psifunctions(List),
    make_tuple(3, PsiOffsets'),
    arg(PSI_CLOSE, PsiOffsets', Close),
    arg(PSI_POST, PsiOffsets', Post),
    arg(PSI_STEP, PsiOffsets', Step) :
      PsiOffsets = _,
      Close = Close'?,
      Post = Post'?,
      Step = Step'? |
	processor#link(lookup(psicomm, PsiOffset), _Ok),
	psifunctions,
	self;

/**************************** Debugger aids *********************************/

    /* Pause processing until Continue is set. */
    Schedule ? pause(Continue),
    unknown(Wakeup) |
	pause_scheduling(Continue,
			 Schedule', Schedule'',
			 Waiting, Waiting',
			 Wakeup, Wakeup',
			 Start_real_time, Start_real_time'),
	self;

    Schedule ? status(Status) :
      Status = [anchors([BasedAnchor, InstantaneousAnchor]),
		cutoff(Cutoff), debug(Debug?), weighter(DefaultWeighter),
		now(Now), record(Record?), waiting(Waiting)] |
	self;		

    /* Step and resume */
    Schedule ? step,
    unknown(Wakeup) :
      Schedule'' = [pause(resume) | Schedule'],
      Wakeup' = done |
	self;

    /* Step and pause processing until Continue is set. */
    Schedule ? step(Continue),
    unknown(Wakeup) :
      Schedule'' = [pause(Continue) | Schedule'],
      Wakeup' = done |
	self;

/***************************************************************************/

    Schedule ? Other,
    otherwise,
    unknown(Wakeup) |
	fail("unrecognized request" - Other),
	self;

    Wakeup = done,
    arg(PSI_STEP, PsiOffsets, PsiOffset),
    PsiOffset =?= unbound :
      Waiting = _,
      Waiting' = false |
	sum_weights(BasedAnchor, 0, Total),
	total_weight1(Offset, BasedAnchor, Now, Total, Wakeup', Now',
			Uniform, NegativeExponential,
			Uniform', NegativeExponential'),
	self;

    Wakeup =?= done,
    arg(PSI_STEP, PsiOffsets, PsiOffset),
    PsiOffset =\= unbound :
      Waiting = _,
      execute(PsiOffset, {PSI_STEP, Now, BasedAnchor, Now', Wakeup'}),
      Waiting' = false |
	self;

/*
 * RCId refers to Request Channel Id (as of now, a string).
 * CNID refers to Channel Name Id (as of now a string or a 2-tuple).
 * CH refers to a psi-channel (as of now, a vector with 12 sub-channels).
 * PName refers to a process name (the originator of a request).
 */

    Wakeup =?= true(PName1, RCId1, PName2, RCId2),
    string(PName1), string(PName2) :
      Waiting = _,
      Wakeup' = _,
      Waiter ! machine(idle_wait(Wakeup'), _Ok),
      Waiting' = true,
      Record = [Now, end(PName1(RCId1)), end(PName2(RCId2)) | Record'?],
      Debug ! done(Now, PName1(RCId1), PName2(RCId2)) |
	self;

    Wakeup =?= true(PId1, RCId1, PName2, RCId2),
    tuple(PId1), arg(1, PId1, PName1),
    string(PName2) :
      Waiting = _,
      Wakeup' = _,
      Waiter ! machine(idle_wait(Wakeup'), _Ok),
      Waiting' = true,
      Record = [Now, end(PName1(RCId1)), end(PName2(RCId2)) | Record'?],
      Debug ! done(Now, PId1(RCId1), PName2(RCId2)) |
	self;

    Wakeup =?= true(PName1, RCId1, PId2, RCId2),
    string(PName1),
    tuple(PId2), arg(1, PId2, PName2) :
      Waiting = _,
      Wakeup' = _,
      Waiter ! machine(idle_wait(Wakeup'), _Ok),
      Waiting' = true,
      Record = [Now, end(PName1(RCId1)), end(PName2(RCId2)) | Record'?],
      Debug ! done(Now, PName1(RCId1), PId2(RCId2)) |
	self;

    Wakeup =?= true(PId1, RCId1, PId2, RCId2),
    tuple(PId1), arg(1, PId1, PName1),
    tuple(PId2), arg(1, PId2, PName2) :
      Waiting = _,
      Wakeup' = _,
      Waiter ! machine(idle_wait(Wakeup'), _Ok),
      Waiting' = true,
      Record = [Now, end(PName1(RCId1)), end(PName2(RCId2)) | Record'?],
      Debug ! done(Now, PId1(RCId1), PId2(RCId2)) |
	self;

    Wakeup =?= true(PName1, RCId1, CH1, PName2, RCId2, CH2),
    string(PName1), string(PName2),
    read_vector(PSI_CHANNEL_NAME, CH1, CNId1),
    read_vector(PSI_CHANNEL_NAME, CH2, CNId2) :
      Waiting = _,
      Wakeup' = _,
      Waiter ! machine(idle_wait(Wakeup'), _Ok),
      Waiting' = true,
      Record = [Now, end(PName1(RCId1, "->", CNId1)),
		     end(PName2(RCId2, "<-", CNId2)) | Record'?],
      Debug ! done(Now, PName1(RCId1, CNId1), PName2(RCId2, CNId2)) |
	self;

    Wakeup =?= true(PId1, RCId1, CH1, PName2, RCId2, CH2),
    tuple(PId1), arg(1, PId1, PName1),
    string(PName2),
    read_vector(PSI_CHANNEL_NAME, CH1, CNId1),
    read_vector(PSI_CHANNEL_NAME, CH2, CNId2) :
      Waiting = _,
      Wakeup' = _,
      Waiter ! machine(idle_wait(Wakeup'), _Ok),
      Waiting' = true,
      Record = [Now, end(PName1(RCId1, "->", CNId1)),
		     end(PName2(RCId2, "<-", CNId2)) | Record'?],
      Debug ! done(Now, PId1(RCId1, CNId1), PName2(RCId2, CNId2)) |
	self;

    Wakeup =?= true(PName1, RCId1, CH1, PId2, RCId2, CH2),
    string(PName1),
    tuple(PId2), arg(1, PId2, PName2),
    read_vector(PSI_CHANNEL_NAME, CH1, CNId1),
    read_vector(PSI_CHANNEL_NAME, CH2, CNId2) :
      Waiting = _,
      Wakeup' = _,
      Waiter ! machine(idle_wait(Wakeup'), _Ok),
      Waiting' = true,
      Record = [Now, end(PName1(RCId1, "->", CNId1)),
		     end(PName2(RCId2, "<-", CNId2)) | Record'?],
      Debug ! done(Now, PName1(RCId1, CNId1), PId2(RCId2, CNId2)) |
	self;

    Wakeup =?= true(PId1, RCId1, CH1, PId2, RCId2, CH2),
    tuple(PId1), arg(1, PId1, PName1),
    tuple(PId2), arg(1, PId2, PName2),
    read_vector(PSI_CHANNEL_NAME, CH1, CNId1),
    read_vector(PSI_CHANNEL_NAME, CH2, CNId2) :
      Waiting = _,
      Wakeup' = _,
      Waiter ! machine(idle_wait(Wakeup'), _Ok),
      Waiting' = true,
      Record = [Now, end(PName1(RCId1, "->", CNId1)),
		     end(PName2(RCId2, "<-", CNId2)) | Record'?],
      Debug ! done(Now, PId1(RCId1, CNId1), PId2(RCId2, CNId2)) |
	self;

    Wakeup =?= true :
      Waiting = _,
      Wakeup' = _,
      Waiting' = false,
      Idle = idle(Now),
      Debug ! Idle |
	self;

    Now >= Cutoff,
    info(REALTIME, End_real_time),
    Real_time := End_real_time - Start_real_time  :
      BasedAnchor = _,
      DefaultWeighter = _,
      InstantaneousAnchor = _,
      NegativeExponential = _,
      Offset = _,
      Ordinal = _,
      PsiOffsets = _,
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

  wait_done(Done) :-

    known(Done) |
	self#reset.

continue_waiting(Schedule, Offset, Ordinal, PsiOffsets, Waiter,
		NegativeExponential, Uniform,
		BasedAnchor, InstantaneousAnchor,
		Scheduler, Record, Debug,
		Now, Waiting, Wakeup,
		DefaultWeighter, Cutoff, Start_real_time, Reply) :-

    Reply =?= true(PName1, RCId1, PName2, RCId2),
    string(PName1), string(PName2) :
      Record = [Now, end(PName1(RCId1)), end(PName2(RCId2)) | Record'?],
      Debug ! done(Now, PName1(RCId1), PName2(RCId2)) |
	scheduling;

    Reply =?= true(PId1, RCId1, PName2, RCId2),
    tuple(PId1), arg(1, PId1, PName1),
    string(PName2) :
      Record = [Now, end(PName1(RCId1)), end(PName2(RCId2)) | Record'?],
      Debug ! done(Now, PId1(RCId1), PName2(RCId2)) |
	scheduling;

    Reply =?= true(PName1, RCId1, PId2, RCId2),
    string(PName1),
    tuple(PId2), arg(1, PId2, PName2) :
      Record = [Now, end(PName1(RCId1)), end(PName2(RCId2)) | Record'?],
      Debug ! done(Now, PName1(RCId1), PId2(RCId2)) |
	scheduling;

    Reply =?= true(PId1, RCId1, PId2, RCId2),
    tuple(PId1), arg(1, PId1, PName1),
    tuple(PId2), arg(1, PId2, PName2) :
      Record = [Now, end(PName1(RCId1)), end(PName2(RCId2)) | Record'?],
      Debug ! done(Now, PId1(RCId1), PId2(RCId2)) |
	scheduling;

    Reply =?= true(PName1, RCId1, CH1, PName2, RCId2, CH2),
    string(PName1), string(PName2),
    read_vector(PSI_CHANNEL_NAME, CH1, CNId1),
    read_vector(PSI_CHANNEL_NAME, CH2, CNId2) :
      Record = [Now, end(PName1(RCId1, "->", CNId1)),
		     end(PName2(RCId2, "<-", CNId2)) | Record'?],
      Debug ! done(Now, PName1(RCId1, CNId1), PName2(RCId2, CNId2)) |
	scheduling;

    Reply =?= true(PId1, RCId1, CH1, PName2, RCId2, CH2),
    tuple(PId1), arg(1, PId1, PName1),
    string(PName2),
    read_vector(PSI_CHANNEL_NAME, CH1, CNId1),
    read_vector(PSI_CHANNEL_NAME, CH2, CNId2) :
      Record = [Now, end(PName1(RCId1, "->", CNId1)),
		     end(PName2(RCId2, "<-", CNId2)) | Record'?],
      Debug ! done(Now, PId1(RCId1, CNId1), PName2(RCId2, CNId2)) |
	scheduling;

    Reply =?= true(PName1, RCId1, CH1, PId2, RCId2, CH2),
    string(PName1),
    tuple(PId2), arg(1, PId2, PName2),
    read_vector(PSI_CHANNEL_NAME, CH1, CNId1),
    read_vector(PSI_CHANNEL_NAME, CH2, CNId2) :
      Record = [Now, end(PName1(RCId1, "->", CNId1)),
		     end(PName2(RCId2, "<-", CNId2)) | Record'?],
      Debug ! done(Now, PName1(RCId1, CNId1), PId2(RCId2, CNId2)) |
	scheduling;

    Reply =?= true(PId1, RCId1, CH1, PId2, RCId2, CH2),
    tuple(PId1), arg(1, PId1, PName1),
    tuple(PId2), arg(1, PId2, PName2),
    read_vector(PSI_CHANNEL_NAME, CH1, CNId1),
    read_vector(PSI_CHANNEL_NAME, CH2, CNId2) :
      Record = [Now, end(PName1(RCId1, "->", CNId1)),
		     end(PName2(RCId2, "<-", CNId2)) | Record'?],
      Debug ! done(Now, PId1(RCId1, CNId1), PId2(RCId2, CNId2)) |
	scheduling;

    otherwise,
    Reply =\= true :
      Debug ! Reply |
	fail(continue_waiting - Reply),
	scheduling;

    Waiting =?= true, Reply =?= true |
	scheduling;

    Waiting =\= true, Reply =?= true :
      Wakeup = _,
      Waiter ! machine(idle_wait(Wakeup'), _Ok),
      Waiting' = true |
	scheduling;

    /* check for paused. */
    Reply =?= true,
    unknown(Waiting) |
	scheduling.


pause_scheduling(Continue,
		 Schedule, ResetSchedule,
		 Waiting, ResetWaiting,
		 Wakeup, ResetWakeup,
		 Start_real_time, Reset_real_time) :-

    info(REALTIME, Pause_real_time),
    Biased_real_time := Start_real_time + Pause_real_time :
      SavedInput = SaveInput? |
	pause_continue.

  pause_continue(Continue,
		 Schedule, ResetSchedule,
		 Waiting, ResetWaiting,
		 Wakeup, ResetWakeup,
		 Biased_real_time, Reset_real_time,
		 SavedInput, SaveInput) :-

    Schedule ? Input,
    Input =?= status(_) :
      ResetSchedule ! Input |
	self;

    Schedule ? Input,
    Input =\= status(_),
    unknown(Continue) :
      SaveInput ! Input |
	self;

    Schedule =?= [],
    unknown(Continue) :
      SavedInput = _,
      SavedInput' = [],
      Continue' = resume |
	self;

    Continue = step(Continue') :
      SaveInput ! pause(Continue'),
      Continue'' = resume |
	self;

    Continue =?= resume,
    info(REALTIME, Resume_real_time),
    Reset_real_time^ := Biased_real_time - Resume_real_time :
      ResetSchedule = SavedInput,
      SaveInput = Schedule,
      ResetWaiting = Waiting,
      Wakeup = ResetWakeup.

index_channel_name(Name, Ordinal, Name', Ordinal') :-

    unknown(Name) :
      Name' = Name,
      Ordinal' = Ordinal;

    string(Name),
    string_to_dlist(Name, CS1, []),
    string_to_dlist("global.", CS2, _Tail) :
      CS1 = CS2,
      Ordinal' = Ordinal,
      Name' = Name;

    string(Name),
    otherwise,
    integer(Ordinal),
    Ordinal'^ := Ordinal + 1 :
      Name' = Name(Ordinal);

    string(Name),
    otherwise,
    real(Ordinal),
    Ordinal'^ := Ordinal + 0.000001 :
      Name' = Name(Ordinal);

    otherwise :
      Name' = Name,
      Ordinal' = Ordinal.

psifunctions(List, PsiOffset, Close, Post, Step) :-

    List ? C,
    nth_char(1, C, Char),
    Char =:= ascii('c') :
      Close = PsiOffset? |
	self;

    List ? P,
    nth_char(1, P, Char),
    Char =:= ascii('P') :
      Post = PsiOffset? |
	self;

    List ? S,
    nth_char(1, S, Char),
    Char =:= ascii('s') :
      Step = PsiOffset? |
	self;

    List ? Other,
    otherwise |
	fail(not_a_function(Other)),
	self;

    List =\= [_|_], List =\= [] :
      List' = [List] |
	self;

    List =?= [] :
      PsiOffset = _ |
	unify_without_failure(unbound, Close),
	unify_without_failure(unbound, Post),
	unify_without_failure(unbound, Step).


new_channel(ChannelName, Channel, BaseRate, ComputeWeight, Scheduler, Reply,
		 BasedAnchor, InstantaneousAnchor, PsiOffsets) :-

    arg(PSI_INDEX, PsiOffsets, PsiOffset), PsiOffset =\= unbound,
    we(Channel),
    arg(1, ComputeWeight, WeighterName),
    convert_to_real(0, Zero) :
      execute(PsiOffset, {PSI_INDEX, WeighterName, WeighterIndex, Result}),
      make_vector(CHANNEL_SIZE, Channel, _),
      store_vector(PSI_BLOCKED, FALSE, Channel),
      store_vector(PSI_CHANNEL_TYPE, PSI_UNKNOWN, Channel),
      store_vector(PSI_CHANNEL_RATE, Zero, Channel),
      store_vector(PSI_CHANNEL_REFS, 1, Channel),
      store_vector(PSI_SEND_ANCHOR, SendAnchor, Channel),
       make_vector(2, LinksS, _),
       SendAnchor = {PSI_MESSAGE_ANCHOR, "", [], 0, 0, 0, [], LinksS},
       store_vector(PSI_NEXT_MS, SendAnchor, LinksS),
       store_vector(PSI_PREVIOUS_MS, SendAnchor, LinksS),
      store_vector(PSI_SEND_WEIGHT, 0, Channel),
      store_vector(PSI_RECEIVE_ANCHOR, ReceiveAnchor, Channel),
       make_vector(2, LinksR, _),
       ReceiveAnchor = {PSI_MESSAGE_ANCHOR, "", [], 0, 0, 0, [], LinksR},
       store_vector(PSI_NEXT_MS, ReceiveAnchor, LinksR),
       store_vector(PSI_PREVIOUS_MS, ReceiveAnchor, LinksR),
      store_vector(PSI_RECEIVE_WEIGHT, 0, Channel),
      store_vector(PSI_WEIGHT_TUPLE, WeighterTuple?, Channel),
      store_vector(PSI_NEXT_CHANNEL, Channel, Channel),
      store_vector(PSI_PREVIOUS_CHANNEL, Channel, Channel),
      store_vector(PSI_CHANNEL_NAME, ChannelName, Channel) |
	based_or_instantaneous,
	complete_weighter_tuple;

    convert_to_real(0, Zero),
    arg(PSI_INDEX, PsiOffsets, unbound),
    ComputeWeight =?= PSI_DEFAULT_WEIGHT_NAME(_),
    we(Channel) :
      make_vector(CHANNEL_SIZE, Channel, _),
      store_vector(PSI_BLOCKED, FALSE, Channel),
      store_vector(PSI_CHANNEL_TYPE, PSI_UNKNOWN, Channel),
      store_vector(PSI_CHANNEL_RATE, Zero, Channel),
      store_vector(PSI_CHANNEL_REFS, 1, Channel),
      store_vector(PSI_SEND_ANCHOR, SendAnchor, Channel),
       make_vector(2, LinksS, _),
       SendAnchor = {PSI_MESSAGE_ANCHOR, "", [], 0, 0, 0, [], LinksS},
       store_vector(PSI_NEXT_MS, SendAnchor, LinksS),
       store_vector(PSI_PREVIOUS_MS, SendAnchor, LinksS),
      store_vector(PSI_SEND_WEIGHT, 0, Channel),
      store_vector(PSI_RECEIVE_ANCHOR, ReceiveAnchor, Channel),
       make_vector(2, LinksR, _),
       ReceiveAnchor = {PSI_MESSAGE_ANCHOR, "", [], 0, 0, 0, [], LinksR},
       store_vector(PSI_NEXT_MS, ReceiveAnchor, LinksR),
       store_vector(PSI_PREVIOUS_MS, ReceiveAnchor, LinksR),
      store_vector(PSI_RECEIVE_WEIGHT, 0, Channel),
      store_vector(PSI_WEIGHT_TUPLE,
		PSI_DEFAULT_WEIGHT_NAME(PSI_DEFAULT_WEIGHT_INDEX), Channel),
      store_vector(PSI_NEXT_CHANNEL, Channel, Channel),
      store_vector(PSI_PREVIOUS_CHANNEL, Channel, Channel),
      store_vector(PSI_CHANNEL_NAME, ChannelName, Channel) |
	based_or_instantaneous + (Result = true);

    otherwise :
      BasedAnchor = _,
      InstantaneousAnchor = _,
      PsiOffsets = _,
      Scheduler = _,
      Reply = error(new_channel(ChannelName, Channel, BaseRate, ComputeWeight)).
  
  based_or_instantaneous(ChannelName, BaseRate, Scheduler, Result, Reply,
			 Channel, BasedAnchor, InstantaneousAnchor) :-

    Result =?= true,
    BaseRate =?= infinite :
      BasedAnchor = _,
      store_vector(PSI_CHANNEL_TYPE, PSI_INSTANTANEOUS, Channel),
      Reply = Result,
      DEBUG((ChannelName: instantaneous)) |
	queue_channel(Channel, InstantaneousAnchor);

    Result =?= true,
    number(BaseRate),
    BaseRate =< 0 :
      BasedAnchor = _,
      InstantaneousAnchor = _,
      store_vector(PSI_CHANNEL_TYPE, PSI_SINK, Channel),
      Reply = true,
      DEBUG((ChannelName: sink));

    Result =?= true,
    number(BaseRate),
    BaseRate > 0,
    convert_to_real(BaseRate, BaseRate') :
      InstantaneousAnchor = _,
      store_vector(PSI_CHANNEL_RATE, BaseRate', Channel),
      Reply = Result,
      DEBUG((ChannelName: based_channel)) |
	queue_channel(Channel, BasedAnchor);

    Result =?= true,
    otherwise :
      BasedAnchor = _,
      Channel = _,
      InstantaneousAnchor = _,
      Scheduler = _,
      store_vector(PSI_CHANNEL_TYPE, PSI_SINK, Channel),
      Reply = "invalid base rate"(ChannelName - BaseRate);

    Result =\= true :
      BasedAnchor = _,
      BaseRate = _,
      Channel = _,
      ChannelName = _,
      InstantaneousAnchor = _,
      Scheduler = _,
      store_vector(PSI_CHANNEL_TYPE, PSI_SINK, Channel),
      Reply = Result.

  complete_weighter_tuple(Reply, ComputeWeight, WeighterIndex, WeighterTuple) :-

    Reply = true,
    arg(2, ComputeWeight, Index) :
      Index = WeighterIndex,
      WeighterTuple = ComputeWeight;

    Reply = true,
    otherwise :
      WeighterIndex = _,
      WeighterTuple = PSI_DEFAULT_WEIGHT_NAME(PSI_DEFAULT_WEIGHT_INDEX) |
	fail(invalid_weighter_index(ComputeWeight));

    Reply =\= true :
      ComputeWeight = _,
      WeighterIndex = _,
      WeighterTuple = PSI_DEFAULT_WEIGHT_NAME(PSI_DEFAULT_WEIGHT_INDEX).


queue_channel(Channel, Anchor) :-

    read_vector(PSI_PREVIOUS_CHANNEL, Anchor, OldLast) :
      store_vector(PSI_PREVIOUS_CHANNEL, OldLast, Channel),
      store_vector(PSI_NEXT_CHANNEL, Anchor, Channel),
      store_vector(PSI_NEXT_CHANNEL, Channel, OldLast),
      store_vector(PSI_PREVIOUS_CHANNEL, Channel, Anchor).

reset_default_weighter(PsiOffset, New, Old, Default) :-

    string(New) :
      execute(PsiOffset, {PSI_INDEX, New, Index, Reply}) |
	check_new_default + (New = New(_));

    tuple(New),
    arg(1, New, Name) :
      execute(PsiOffset, {PSI_INDEX, Name, Index, Reply}) |
	check_new_default;

    otherwise :
      PsiOffset = _,
      Default = Old |
	fail(invalid_new_default_weighter(New)).

  check_new_default(Reply, Index, New, Old, Default) :-

    Reply =?= true,
    arg(2, New, Ix) :
      Old = _,
      Ix = Index,
      Default = New;

    Reply =?= true,
    otherwise :
      Default = Old |
	fail(mismatch_new_default_weighter_index(New - Index));

    Reply =\= true :
      Index = _,
      Default = Old |
	fail(Reply(New)).

/************************* execute - testing *********************************/

execute(Offset, Arguments) :-

    Arguments = post(PId, OpList, Value, Chosen, Reply) :
      Offset = _,
      Common = {PId, Messages, Value, Chosen},
      Messages = AddMessages? |
	post_pass1,
	post ;

    Arguments = step(Now, Anchor, NewNow, Reply) |
	sum_weights(Anchor, 0, Total),
	total_weight;

    Arguments = close(Channels, Reply),
    tuple(Channels),
    N := arity(Channels) :
      Offset = _ |
	close_channels(Channels, N, Reply).


/************************** close procedures *********************************/

close_channels(Channels, N, Reply) :-

    N > 0,
    arg(N, Channels, Channel),
    N--,
    vector(Channel),
    read_vector(PSI_CHANNEL_REFS, Channel, Refs),
    Refs--,
    Refs' > 0 :
      store_vector(PSI_CHANNEL_REFS, Refs', Channel) |
	self;

    N > 0,
    arg(N, Channels, Channel),
    N--,
    vector(Channel),
    read_vector(PSI_CHANNEL_REFS, Channel, Refs),
    Refs--,
    Refs' =< 0,
    read_vector(PSI_CHANNEL_NAME, Channel, Name),
    read_vector(PSI_NEXT_CHANNEL, Channel, Next),
    read_vector(PSI_PREVIOUS_CHANNEL, Channel, Previous) :
      store_vector(PSI_CHANNEL_REFS, 0, Channel),
      store_vector(PSI_NEXT_CHANNEL, Next, Previous),
      store_vector(PSI_PREVIOUS_CHANNEL, Previous, Next),
      store_vector(PSI_NEXT_CHANNEL, Channel, Channel),
      store_vector(PSI_PREVIOUS_CHANNEL, Channel, Channel),
      Reply ! Name |
	self;

    N =< 0 :
      Channels  = _,
      Reply = [];

    otherwise :
      Reply = close_failed(N, Channels).


/************************ post procedures ********************************/

post_pass1(OpList, Common, Ok) :-

    OpList ? Operation,
    Operation = {MessageType, _RCId, Channel , Multiplier, _Tags},
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(PSI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= PSI_INSTANTANEOUS,
    MessageType =?= PSI_SEND,
    read_vector(PSI_RECEIVE_ANCHOR, Channel, Anchor) |
	do_instantaneous_transmit + (MTX = PSI_RECEIVE_TAG, Message = Anchor);

    OpList ? Operation,
    Operation = {MessageType, _RCId, Channel , Multiplier, _Tags},
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(PSI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= PSI_INSTANTANEOUS,
    MessageType =?= PSI_RECEIVE,
    read_vector(PSI_SEND_ANCHOR, Channel, Anchor) |
	do_instantaneous_transmit + (MTX = PSI_SEND_TAG, Message = Anchor);

    OpList ? Operation,
    Operation =?= {MessageType, _RCId, Channel , Multiplier, _Tags},
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(PSI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= PSI_BIMOLECULAR,
    MessageType =?= PSI_SEND :
      store_vector(PSI_BLOCKED, FALSE, Channel) |
	self;

    OpList ? Operation,
    Operation =?= {MessageType, _RCId, Channel , Multiplier, _Tags},
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(PSI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= PSI_BIMOLECULAR,
    MessageType =?= PSI_RECEIVE :
      store_vector(PSI_BLOCKED, FALSE, Channel) |
	self;

    OpList ? Operation,
    Operation =?= {MessageType, _RCId, Channel , Multiplier, _Tags},
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(PSI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= PSI_HOMODIMERIZED,
    MessageType =?= PSI_DIMER :
      store_vector(PSI_BLOCKED, FALSE, Channel) |
	self;

    OpList ? Operation,
    Operation =?= {MessageType, _RCId, Channel, Multiplier, _Tags},
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(PSI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= PSI_UNKNOWN,
    MessageType =?= PSI_SEND :
      store_vector(PSI_CHANNEL_TYPE, PSI_BIMOLECULAR, Channel) |
	self;

    OpList ? Operation,
    Operation =?= {MessageType, _RCId, Channel, Multiplier, _Tags},
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(PSI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= PSI_UNKNOWN,
    MessageType =?= PSI_RECEIVE :
      store_vector(PSI_CHANNEL_TYPE, PSI_BIMOLECULAR, Channel) |
	self;

    OpList ? Operation,
    Operation =?= {MessageType, _RCId, Channel, Multiplier, _Tags},
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(PSI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= PSI_UNKNOWN,
    MessageType =?= PSI_DIMER :
      store_vector(PSI_CHANNEL_TYPE, PSI_HOMODIMERIZED, Channel) |
	self;

    OpList ? Operation,
    Operation =?= {_MessageType, _RCId, Channel, Multiplier, _Tags},
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(PSI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= PSI_SINK |
	self;

    OpList =?= [] :
      Common = _,
      Ok = true;

    otherwise :
      Common = _,
      Ok = invalid_transmission - OpList.


post(OpList, Reply, Common, Messages, AddMessages, Ok) :-

    Ok =?= true,
    OpList ? Operation,
    Operation =?= {MessageType, RCId, Channel, Multiplier, Tags},
    read_vector(PSI_CHANNEL_TYPE, Channel, Type),
    Type =\= PSI_SINK :
      AddMessages ! Message? |
	queue_message,
	self;

    Ok =?= true,
    OpList ? Operation,
    Operation =?= {_MessageType, _RCId, Channel, _Multiplier, _Tags},
    read_vector(PSI_CHANNEL_TYPE, Channel, Type),
    Type =?= PSI_SINK |
	self;

    Ok =?= true,
    OpList =?= [] :
      Common = _,
      Messages = _,
      AddMessages = [],
      Reply = true;      

    otherwise :
      Common = _,
      Messages = _,
      OpList = _,
      Reply = Ok,
      AddMessages = [].

/*
 * Suffix Q refers to a channel queue element.
 * Suffix R refers to a message request element.
 */ 
do_instantaneous_transmit(Operation, MTX, Anchor, Message, Common, OpList,
					Ok) :-

    arg(PSI_MESSAGE_LINKS, Message, MessageLinks),
    read_vector(PSI_NEXT_MS, MessageLinks, Message'),
    Message' =\= Anchor,
    arg(PSI_COMMON, Message', CommonQ),
    arg(PSI_OP_CHOSEN, CommonQ, Chosen), not_we(Chosen) |
	self;

    arg(PSI_MESSAGE_LINKS, Message, MessageLinks),
    read_vector(PSI_NEXT_MS, MessageLinks, Message'),
    Message' =\= Anchor,
    Message' =?= {_, RCIdQ, _, _, _, _, CommonQ, _},
    arg(MTX, Message', MessageTag),
    CommonQ = {PIdQ, MsList, ValueQ, ChosenQ},
    Common = {PIdR, _MsList, ValueR, ChosenR},
    Operation =?= {_, RCIdR, _, _, OperationTag} :
      Anchor = _,
      OpList = _,
      ValueQ = ValueR,
      ChosenQ = MessageTag,
      ChosenR = OperationTag,
      Ok = true(PIdQ, RCIdQ, PIdR, RCIdR) |
	discount(MsList);

    % This can happen (to the monitor). 
    Common = {_, _, _, Chosen}, not_we(Chosen) :
      Anchor = _,
      Operation = _,
      Message = _,
      MTX = _ |
	post_pass1;

  % Test for end of list
    arg(PSI_MESSAGE_LINKS, Message, MessageLinks),
    read_vector(PSI_NEXT_MS, MessageLinks, Message'),
    Message' =?= Anchor :
      Operation = _,
      MTX = _ |
	post_pass1;

    % This Message has the same Chosen as the Operation -
    % mixed communications which ARE NOT homodimerized.
    arg(PSI_MESSAGE_LINKS, Message, MessageLinks),
    read_vector(PSI_NEXT_MS, MessageLinks, Message'),
    Message' =?= {_, _, _, _, _, _, CommonQ, _},
    CommonQ = {_, _, _, Chosen},
    Common = {_, _, _, Chosen} |
	self.


/*************************** step procedures *********************************/

sum_weights(Channel, Sum, Total) :-

    read_vector(PSI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(PSI_CHANNEL_TYPE, Channel', Type),
    Type =?= PSI_CHANNEL_ANCHOR :
      Total = Sum;

    read_vector(PSI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(PSI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(PSI_CHANNEL_TYPE, Channel', Type),
    Type =?= PSI_BIMOLECULAR,
    read_vector(PSI_CHANNEL_RATE, Channel', Rate),
    read_vector(PSI_SEND_WEIGHT, Channel', SendWeight),
    read_vector(PSI_RECEIVE_WEIGHT, Channel', ReceiveWeight),
    Sum += Rate*SendWeight*ReceiveWeight |
	self;    

    read_vector(PSI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(PSI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(PSI_CHANNEL_TYPE, Channel', Type),
    Type =?= PSI_HOMODIMERIZED,
    read_vector(PSI_DIMER_ANCHOR, Channel', Anchor),
    arg(PSI_MESSAGE_LINKS, Anchor, FirstLink),
    read_vector(PSI_NEXT_MS, FirstLink, Message),
    arg(PSI_MESSAGE_LINKS, Message, DimerLink),
    read_vector(PSI_NEXT_MS, DimerLink, DimerMs),
    arg(PSI_MS_TYPE, DimerMs, MsType),
    MsType =\= PSI_MESSAGE_ANCHOR,
    read_vector(PSI_CHANNEL_RATE, Channel', Rate),
    read_vector(PSI_DIMER_WEIGHT, Channel', DimerWeight),
    Sum += Rate*DimerWeight*(DimerWeight-1)/2 |
	self;

    read_vector(PSI_NEXT_CHANNEL, Channel, Channel'),
    otherwise |
	self.


total_weight(Offset, Anchor, Now, Total, Reply, NewNow) :-

    Total =< 0 :
      Anchor = _,
      Offset = _,
      NewNow = Now,
      Reply = true;

    Total > 0 :
      execute(Offset, {RAN, 0, Uniform}),
      execute(Offset, {LN, Uniform, NegativeExponential}),
      execute(Offset, {RAN, 0, Uniform'}) |
	Residue := Uniform'*Total,
	select_channel + (Channel = Anchor),
	NewNow := Now - NegativeExponential/Total.

total_weight1(Offset, Anchor, Now, Total, Reply, NewNow, U, NE, NU, NNE) :-

    Total =< 0 :
      Anchor = _,
      Offset = _,
      NewNow = Now,
      NU = U,
      NNE = NE,
      Reply = true;

    Total > 0,
    Residue := U*Total,
    Now' := Now - NE/Total :
      NewNow = Now',
      execute(Offset, {RAN, 0, Uniform}),
      execute(Offset, {LN, Uniform, NNE}),
      execute(Offset, {RAN, 0, NU}) |
	select_channel + (Channel = Anchor).

select_channel(Residue, Channel, Reply) :-

    read_vector(PSI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(PSI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(PSI_CHANNEL_TYPE, Channel', Type),
    Type =?= PSI_BIMOLECULAR,
    read_vector(PSI_CHANNEL_RATE, Channel', Rate),
    read_vector(PSI_SEND_WEIGHT, Channel', SendWeight),
    SendWeight > 0,
    read_vector(PSI_RECEIVE_WEIGHT, Channel', ReceiveWeight),
    ReceiveWeight > 0,
    Residue -= Rate*SendWeight*ReceiveWeight,
    Residue' =< 0 |
	do_bimolecular_transmit;

    read_vector(PSI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(PSI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(PSI_CHANNEL_TYPE, Channel', Type),
    Type =?= PSI_BIMOLECULAR,
    read_vector(PSI_CHANNEL_RATE, Channel', Rate),
    read_vector(PSI_SEND_WEIGHT, Channel', SendWeight),
    SendWeight > 0,
    read_vector(PSI_RECEIVE_WEIGHT, Channel', ReceiveWeight),
    ReceiveWeight > 0,
    Residue -= Rate*SendWeight*ReceiveWeight,
    Residue' > 0 |
	self;

    read_vector(PSI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(PSI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(PSI_CHANNEL_TYPE, Channel', Type),
    Type =?= PSI_HOMODIMERIZED,
    read_vector(PSI_DIMER_ANCHOR, Channel', Anchor),
    arg(PSI_MESSAGE_LINKS, Anchor, FirstLink),
    read_vector(PSI_NEXT_MS, FirstLink, Message),
    arg(PSI_MESSAGE_LINKS, Message, DimerLink),
    read_vector(PSI_NEXT_MS, DimerLink, DimerMs),
    arg(PSI_MS_TYPE, DimerMs, MsType),
    MsType =\= PSI_MESSAGE_ANCHOR,
    read_vector(PSI_CHANNEL_RATE, Channel', Rate),
    read_vector(PSI_DIMER_WEIGHT, Channel', DimerWeight),
    Residue -= Rate*DimerWeight*(DimerWeight-1)/2,
    Residue' =< 0 |
	do_homodimerized_transmit;

    read_vector(PSI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(PSI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(PSI_CHANNEL_TYPE, Channel', Type),
    Type =?= PSI_HOMODIMERIZED,
    read_vector(PSI_DIMER_ANCHOR, Channel', Anchor),
    arg(PSI_MESSAGE_LINKS, Anchor, FirstLink),
    read_vector(PSI_NEXT_MS, FirstLink, Message),
    arg(PSI_MESSAGE_LINKS, Message, DimerLink),
    read_vector(PSI_NEXT_MS, DimerLink, DimerMs),
    arg(PSI_MS_TYPE, DimerMs, MsType),
    MsType =\= PSI_MESSAGE_ANCHOR,
    read_vector(PSI_CHANNEL_RATE, Channel', Rate),
    read_vector(PSI_DIMER_WEIGHT, Channel', DimerWeight),
    Residue -= Rate*DimerWeight*(DimerWeight-1)/2,
    Residue' > 0 |
	self;

    read_vector(PSI_NEXT_CHANNEL, Channel, Channel'),
    otherwise |
	self.

/****** based transmit - complete a transmission for a pair of messages ******/

do_bimolecular_transmit(Channel, Reply) :-

    read_vector(PSI_SEND_ANCHOR, Channel, SendAnchor),
    arg(PSI_MESSAGE_LINKS, SendAnchor, SendLinks),
    read_vector(PSI_NEXT_MS, SendLinks, Send),
    read_vector(PSI_RECEIVE_ANCHOR, Channel, ReceiveAnchor),
    arg(PSI_MESSAGE_LINKS, ReceiveAnchor, ReceiveLinks),
    read_vector(PSI_NEXT_MS, ReceiveLinks, Receive) |
	do_bimolecular_send(Channel, Reply, Send, Receive).

do_bimolecular_send(Channel, Reply, Send, Receive) :-

    Send =?= {PSI_SEND, SendRCId, _, _, SendTag, _, SendCommon, _},
    Receive =?= {PSI_RECEIVE, ReceiveRCId, _, _, _,
					ReceiveTag, ReceiveCommon, _},
    SendCommon =?= {SendPId, SendList, SendValue, SendChosen},
    ReceiveCommon =?= {ReceivePId, ReceiveList, ReceiveValue, ReceiveChosen} :
      Channel = _,
      SendChosen = SendTag,
      ReceiveChosen = ReceiveTag,
      ReceiveValue = SendValue?,
      Reply = true(SendPId, SendRCId, ReceivePId, ReceiveRCId) |
	discount(SendList),
	discount(ReceiveList);

    Send =?= {PSI_SEND, _, _, _, _, _, SendCommon, SendLinks},
    Receive =?= {PSI_RECEIVE, _, _, _, _, _, ReceiveCommon, _},
    % This Send has the same Chosen as the Receive -
    % mixed communications which ARE NOT homodimerized.
    SendCommon =?= {_, _, _, Chosen},
    ReceiveCommon =?= {_, _, _, Chosen},
    read_vector(PSI_NEXT_MS, SendLinks, Send') |
	self;

    Send =?= {PSI_MESSAGE_ANCHOR, _, _, _, _, _, _, SendLinks},
    Receive =?= {PSI_RECEIVE, _, _, _, _, _, _, ReceiveLinks},
    read_vector(PSI_NEXT_MS, SendLinks, Send'),
    read_vector(PSI_NEXT_MS, ReceiveLinks, Receive') |
	self;

    Receive =?= {PSI_MESSAGE_ANCHOR, _, _, _, _, _, _, _} :
      Send = _,
      store_vector(PSI_BLOCKED, TRUE, Channel),
      Reply = done.


do_homodimerized_transmit(Channel, Reply) :-

    read_vector(PSI_DIMER_ANCHOR, Channel, DimerAnchor),
    arg(PSI_MESSAGE_LINKS, DimerAnchor, DimerLinks),
    read_vector(PSI_NEXT_MS, DimerLinks, Receive),
    arg(PSI_MESSAGE_LINKS, Receive, Links),
    read_vector(PSI_NEXT_MS, Links, Dimer) |
	do_homodimerized_send(Channel, Reply, Receive, Dimer).

do_homodimerized_send(Channel, Reply, Receive, Dimer) :-

    Receive =?= {PSI_DIMER, ReceiveRCId, _, _, _,
					ReceiveTag, ReceiveCommon, _},
    Dimer =?= {PSI_DIMER, DimerRCId, _, _, DimerTag, _, DimerCommon, _},
    ReceiveCommon =?= {ReceivePId, ReceiveList, ReceiveValue, ReceiveChosen},
    we(ReceiveChosen),
    DimerCommon =?= {DimerPId, DimerList, DimerValue, DimerChosen},
    we(DimerChosen) :
      Channel = _,
      ReceiveChosen = ReceiveTag,
      DimerChosen = DimerTag,
      DimerValue = ReceiveValue?,
      Reply = true(ReceivePId, ReceiveRCId, DimerPId, DimerRCId) |
	discount(ReceiveList),
	discount(DimerList);

    Receive =?= {PSI_DIMER, _, _, _, _, _, ReceiveCommon, _},
    Dimer =?= {PSI_DIMER, _, _, _, _, _, DimerCommon, Links},
    % This Receive has the same Chosen as the Send -
    % communications which ARE homodimerized AND mixed.
    ReceiveCommon =?= {_, _, _, Chosen},
    DimerCommon =?= {_, _, _, Chosen},
    arg(PSI_MESSAGE_LINKS, Links, NextLink),
    read_vector(PSI_NEXT_MS, NextLink, Dimer') |
	self;

    Dimer =?= {PSI_MESSAGE_ANCHOR, _, _, _, _, _, _, _} :
      Receive = _,
      store_vector(PSI_BLOCKED, TRUE, Channel),
      Reply = done.


/***************************** Utilities ************************************/

discount(MsList) :-

    MsList ? Message,
    Message = {MessageType, _, Channel, Multiplier, _, _, _, Links},
    read_vector(PSI_NEXT_MS, Links, NextMessage),
    arg(PSI_MESSAGE_LINKS, NextMessage, NextLinks),
    read_vector(PSI_PREVIOUS_MS, Links, PreviousMessage),
    arg(PSI_MESSAGE_LINKS, PreviousMessage, PreviousLinks),
    MessageType = PSI_SEND,
    read_vector(PSI_SEND_WEIGHT, Channel, Weight),
    Weight -= Multiplier :
      store_vector(PSI_SEND_WEIGHT, Weight', Channel),
      store_vector(PSI_NEXT_MS, NextMessage, PreviousLinks),
      store_vector(PSI_PREVIOUS_MS, PreviousMessage, NextLinks) |
	self;

    MsList ? Message,
    Message = {MessageType, _, Channel, Multiplier, _, _, _, Links},
    read_vector(PSI_NEXT_MS, Links, NextMessage),
    arg(PSI_MESSAGE_LINKS, NextMessage, NextLinks),
    read_vector(PSI_PREVIOUS_MS, Links, PreviousMessage),
    arg(PSI_MESSAGE_LINKS, PreviousMessage, PreviousLinks),
    MessageType =?= PSI_RECEIVE,
    read_vector(PSI_RECEIVE_WEIGHT, Channel, Weight),
    Weight -= Multiplier :
      store_vector(PSI_RECEIVE_WEIGHT, Weight', Channel),
      store_vector(PSI_NEXT_MS, NextMessage, PreviousLinks),
      store_vector(PSI_PREVIOUS_MS, PreviousMessage, NextLinks) |
	self;

    MsList ? Message,
    Message =?= {MessageType, _, Channel, Multiplier, _, _, _, Links},
    read_vector(PSI_NEXT_MS, Links, NextMessage),
    arg(PSI_MESSAGE_LINKS, NextMessage, NextLinks),
    read_vector(PSI_PREVIOUS_MS, Links, PreviousMessage),
    arg(PSI_MESSAGE_LINKS, PreviousMessage, PreviousLinks),
    MessageType =?= PSI_DIMER,
    read_vector(PSI_DIMER_WEIGHT, Channel, Weight),
    Weight -= Multiplier :
      store_vector(PSI_DIMER_WEIGHT, Weight', Channel),
      store_vector(PSI_NEXT_MS, NextMessage, PreviousLinks),
      store_vector(PSI_PREVIOUS_MS, PreviousMessage, NextLinks) |
	self;

    MsList ? Other,
    otherwise |
show#stuff(item, Other),
show#stuff(end, MsList'),
arg(3, Other, Channel),
show#stuff(channel, Channel),
	self;

    MsList =?= [] |
	true.


queue_message(MessageType, RCId, Channel, Multiplier, Tags,
			Common, Message) :-

    MessageType =?= PSI_SEND,
    read_vector(PSI_SEND_ANCHOR, Channel, Anchor),
    read_vector(PSI_SEND_WEIGHT, Channel, Weight),
    Weight += Multiplier :
      store_vector(PSI_SEND_WEIGHT, Weight', Channel),
      Message = {MessageType, RCId, Channel, Multiplier,
			Tags, 0, Common, Links?} |
	queue_to_anchor;

    MessageType =?= PSI_RECEIVE,
    read_vector(PSI_RECEIVE_ANCHOR, Channel, Anchor),
    read_vector(PSI_RECEIVE_WEIGHT, Channel, Weight),
    Weight += Multiplier :
      store_vector(PSI_RECEIVE_WEIGHT, Weight', Channel),
      Message = {MessageType, RCId, Channel, Multiplier,
			0, Tags, Common, Links?} |
	queue_to_anchor;

    MessageType =?= PSI_DIMER,
    read_vector(PSI_DIMER_ANCHOR, Channel, Anchor),
    Tags = {SendTag, ReceiveTag},
    read_vector(PSI_DIMER_WEIGHT, Channel, Weight),
    Weight += Multiplier :
      store_vector(PSI_DIMER_WEIGHT, Weight', Channel),
      Message = {MessageType, RCId, Channel, Multiplier,
			SendTag, ReceiveTag, Common, Links?} |
	queue_to_anchor.

queue_to_anchor(Message, Anchor, Links) :-

    arg(PSI_MESSAGE_LINKS, Anchor, AnchorLinks),
    read_vector(PSI_PREVIOUS_MS, AnchorLinks, PreviousMessage),
    arg(PSI_MESSAGE_LINKS, PreviousMessage, PreviousLinks) :
      make_vector(2, Links, _),
      store_vector(PSI_PREVIOUS_MS, PreviousMessage, Links),
      store_vector(PSI_NEXT_MS, Anchor, Links),
      store_vector(PSI_NEXT_MS, Message, PreviousLinks),
      store_vector(PSI_PREVIOUS_MS, Message, AnchorLinks).
