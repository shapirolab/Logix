-monitor(serve).
-language([evaluate,compound,colon]).
-export([get_global_channels/1, global_channels/1, global_channels/2,
	 new_channel/3, new_channel/4,
	 options/2, reset/0, scheduler/1]).
-include(spi_constants).

RAN =>  4.					/* uniform 0..1 variate */
LN  =>  9.					/* natural logarithm */

REALTIME => 12.

MAXTIME => 99999999999999999999999999999999999999999999999999999999999.0.

DEBUG(Note) => write_channel(debug_note(Note), Scheduler).

%STOPPED => DEBUG(stopped(Reply)).
STOPPED => Reply = _.

/*
** Arguments of SPIOFFSETS correspond to the C-executable (spicomm.c)
** sub-functions: Post, Close, Step, Index (see spi_constants.cp).
**
** To test one or more C-executables, set the other arguments "unbound" - e.g. 
** 
** SPIOFFSETS => {unbound, unbound, SpiOffset, SpiOffset}
**
** to allow the monitor to call the C step function and the C index function,
** and to execute the other operations with fcp code.
*/

%SPIOFFSETS => {SpiOffset, SpiOffset, SpiOffset, SpiOffset}.
SPIOFFSETS => {unbound, unbound, unbound, SpiOffset}.
%SPIOFFSETS => {unbound, unbound, unbound, unbound}.

serve(In) + (Options = []) :-

    In =?= [] :
      Options = _;

    In =\= [] :
      SpiOffset = _,
      Ordinal = 1 |
	server(In, Options, Scheduler),
	processor#link(lookup(math, MathOffset), Ok),
	processor#link(lookup(spicomm, SpiOffset), _Ok),
	start_scheduling(Scheduler, MathOffset, Ordinal, SPIOFFSETS,
		SPI_DEFAULT_WEIGHT_NAME(SPI_DEFAULT_WEIGHT_INDEX), Ok).

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

    In ? spifunctions(List) :
      write_channel(spifunctions(List), Scheduler) |
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
    Globals ? Global, Global = Name(SpiChannel, _ComputeWeight, BaseRate),
    vector(SpiChannel),
    read_vector(SPI_CHANNEL_REFS, SpiChannel, References),
    References++ :
      Last = _,
      NewChannel = SpiChannel,
      store_vector(SPI_CHANNEL_REFS, References', SpiChannel),
      NewGlobals ! Global,
      Last' = Name |
	self;

    List ? Name(_NewChannel, BaseRate),
    Globals ? Entry, Entry = Name(_SpiChannel, _ComputeWeight, OtherBaseRate),
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
	[Name(SpiChannel?, SPI_DEFAULT_WEIGHT_NAME, BaseRate) | Globals],
      write_channel(new_channel(Id?, SpiChannel, SPI_DEFAULT_WEIGHT_NAME,
				BaseRate), Scheduler),
      GT = NL |
	list_to_string(GL, Id),
	self;

    List ? Name(NewChannel, CW, BaseRate),
    Last @< Name,
    we(NewChannel),
    Globals ? Global, Global = Name(SpiChannel, ComputeWeight, BaseRate),
    vector(SpiChannel),
    read_vector(SPI_CHANNEL_REFS, SpiChannel, References),
    References++ :
      Last = _,
      CW = ComputeWeight?,
      NewChannel = SpiChannel,
      store_vector(SPI_CHANNEL_REFS, References', SpiChannel),
      NewGlobals ! Global,
      Last' = Name |
	self;

    List ? Name(_NewChannel, _, BaseRate),
    Globals ? Entry, Entry = Name(_SpiChannel, _, OtherBaseRate),
    BaseRate =\= OtherBaseRate :
      NewGlobals ! Entry |
	fail(global_channel(rate_conflict(Name - BaseRate =\= OtherBaseRate))),
	self;

    List ? Name(_NewChannel, ComputeWeight, _),
    Globals ? Entry, Entry = Name(_SpiChannel, OtherComputeWeight, _),
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
      Globals' = [Name(SpiChannel?, ComputeWeight, BaseRate) | Globals],
      write_channel(new_channel(Id?, SpiChannel, ComputeWeight, BaseRate),
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

start_scheduling(Scheduler, MathOffset, Ordinal, SpiOffsets, DefaultWeighter, Ok):-

    Ok =?= true,
    info(REALTIME, Start_real_time),
    convert_to_real(0, Zero),
    convert_to_real(MAXTIME, MaxTime) :
      execute(MathOffset, {RAN, 0, Uniform}),
      execute(MathOffset, {LN, Uniform, NegativeExponential}),
      execute(MathOffset, {RAN, 0, Uniform'}),
      make_channel(Scheduler, Schedule) |
	make_channel_anchor(based, BasedAnchor),
	make_channel_anchor(instantaneous, InstantaneousAnchor),
	processor#Waiter?,
	scheduling(Schedule, MathOffset, Ordinal, SpiOffsets, Waiter,
				NegativeExponential, Uniform',
				BasedAnchor, InstantaneousAnchor,
				Scheduler, _Recording, _Debug,
				Zero, false, _Wakeup,
				DefaultWeighter, MaxTime, Start_real_time);

    Ok =\= true :
      DefaultWeighter = _,
      Scheduler = _,
      MathOffset = _,
      Ordinal = _,
      SpiOffsets = _ |
	fail(math_offset(Ok)).


make_channel_anchor(Name, Anchor) :-

    convert_to_real(0, Zero) :
      make_vector(CHANNEL_SIZE, Anchor, _),
      store_vector(SPI_BLOCKED, FALSE, Anchor),
      store_vector(SPI_CHANNEL_TYPE, SPI_CHANNEL_ANCHOR, Anchor),
      store_vector(SPI_CHANNEL_RATE, Zero, Anchor),
      store_vector(SPI_CHANNEL_REFS, 1, Anchor),
      store_vector(SPI_SEND_ANCHOR, SendAnchor, Anchor),
       make_channel(NextS, _),
       make_channel(PrevS, _),
       SendAnchor = {SPI_MESSAGE_ANCHOR, "", [], 0, 0, [], NextS, PrevS, []},
       store_vector(1, SendAnchor, NextS),
       store_vector(1, SendAnchor, PrevS),
      store_vector(SPI_SEND_WEIGHT, 0, Anchor),
      store_vector(SPI_RECEIVE_ANCHOR, ReceiveAnchor, Anchor),
       make_channel(NextR, _),
       make_channel(PrevR, _),
       ReceiveAnchor = {SPI_MESSAGE_ANCHOR, "", [], 0, 0, [], NextR, PrevR, []},
       store_vector(1, ReceiveAnchor, NextR),
       store_vector(1, ReceiveAnchor, PrevR),
      store_vector(SPI_RECEIVE_WEIGHT, 0, Anchor),
      store_vector(SPI_WEIGHT_TUPLE, 
		SPI_DEFAULT_WEIGHT_NAME(SPI_DEFAULT_WEIGHT_INDEX), Anchor),
      store_vector(SPI_NEXT_CHANNEL, Anchor, Anchor),
      store_vector(SPI_PREVIOUS_CHANNEL, Anchor, Anchor),
      store_vector(SPI_CHANNEL_NAME, Name, Anchor).

/*
** scheduling monitors the stream generated using the scheduler channel.
**
** It recognises:
**
**    close(Tuple)
**    close(Tuple, Reply)
**    cutoff(Time)
**    input(Schedule?^, Schedule')
**    new_channel(ChannelName, Channel, BaseRate)
**    new_channel(ChannelName, Channel, ComputeWeight, BaseRate)
**    ordinal(Old?^, New)
**    pause(Continue)
**    record(Record?^)
**    record_item(Item)
**    end_record(Record?^)
**    start(Signature, OpList, Value, Chosen)
**    start(Signature, OpList, Value, Chosen, Prefix)
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
** Whenever the system becomes idle, execute(SpiOffset, {SPI_STEP, ...})
** to select and  complete a transmission.
**
** Record:
**
**   Changes to  Now .
**   Selected processes .
*/

scheduling(Schedule, MathOffset, Ordinal, SpiOffsets, Waiter,
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
      MathOffset = _,
      Ordinal = _,
      SpiOffsets = _,
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
    arg(SPI_CLOSE, SpiOffsets, SpiOffset),
    SpiOffset =?= unbound :
      STOPPED |
	execute(MathOffset, close(Channels, Reply)),
	self;

    Schedule ? close(Channels),
    arg(SPI_CLOSE, SpiOffsets, SpiOffset),
    SpiOffset =\= unbound :
      execute(SpiOffset, {SPI_CLOSE, Channels, Reply}),
      STOPPED |
	self;

    Schedule ? close(Channels, Reply),
    arg(SPI_CLOSE, SpiOffsets, SpiOffset),
    SpiOffset =?= unbound :
      STOPPED |
	execute(MathOffset, close(Channels, Reply)),
	self;

    Schedule ? close(Channels, Reply),
    arg(SPI_CLOSE, SpiOffsets, SpiOffset),
    SpiOffset =\= unbound :
      execute(SpiOffset, {SPI_CLOSE, Channels, Reply}),
      STOPPED |
	self;

    Schedule ? default_weighter(Weighter),
    arg(SPI_INDEX, SpiOffsets, SpiOffset),
    SpiOffset =\= unbound |
	reset_default_weighter(SpiOffset, Weighter, DefaultWeighter,
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

    Schedule ? record_item(Item) :
      Record ! Item,
      Debug ! Item |
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
    arg(SPI_POST, SpiOffsets, SpiOffset),
    SpiOffset =?= unbound,
    tuple(PId), arg(1, PId, PName) :
      Record ! start(PName),
      Debug ! start(PId) |
	execute(MathOffset, post(PId, OpList, Value, Chosen, Reply)),
	continue_waiting;

    Schedule ? Start, Start =?= start(PName, OpList, Value, Chosen),
    arg(SPI_POST, SpiOffsets, SpiOffset),
    SpiOffset =?= unbound,
    string(PName) :
      Record ! start(PName),
      Debug ! start(PName) |
	execute(MathOffset, post({PName}, OpList, Value, Chosen, Reply)),
	continue_waiting;

    Schedule ? Start, Start =?= start(PId, OpList, Value, Chosen, Prefix),
    arg(SPI_POST, SpiOffsets, SpiOffset),
    SpiOffset =?= unbound,
    tuple(PId), arg(1, PId, PName),
    string(PName),
    string_to_dlist(PName, PNL, []),
    convert_to_string(Prefix, Prefix'),
    string_to_dlist(Prefix', PL, [CHAR_COLON | PNL]),
    list_to_string(PL, PrefixedId) :
      PostId = (Prefix' : PId),
      Record ! start(PrefixedId),
      Debug ! start(PostId) |
	execute(MathOffset, post(PostId, OpList, Value, Chosen, Reply)),
	continue_waiting;

    Schedule ? Start, Start =?= start({PName}, OpList, Value, Chosen, Prefix),
    arg(SPI_POST, SpiOffsets, SpiOffset),
    SpiOffset =?= unbound,
    string(PName),
    string_to_dlist(PName, PNL, []),
    convert_to_string(Prefix, Prefix'),
    string_to_dlist(Prefix', PL, [CHAR_COLON | PNL]),
    list_to_string(PL, PrefixedId) :
      PostId = (Prefix' : {PName}),
      Record ! start(PrefixedId),
      Debug ! start(PostId) |
	execute(MathOffset, post(PostId, OpList, Value, Chosen, Reply)),
	continue_waiting;

    Schedule ? Start, Start =?= start(PId, OpList, Value, Chosen),
    arg(SPI_POST, SpiOffsets, SpiOffset),
    SpiOffset =\= unbound,
    tuple(PId), arg(1, PId, PName),
    string(PName) :
      execute(SpiOffset, {SPI_POST, PId, OpList, Value, Chosen, Reply}),
      Record ! start(PName),
      Debug ! start(PId) |
	continue_waiting;

    Schedule ? Start, Start =?= start(PName, OpList, Value, Chosen),
    arg(SPI_POST, SpiOffsets, SpiOffset),
    SpiOffset =\= unbound,
    string(PName) :
      execute(SpiOffset, {SPI_POST, {PName}, OpList, Value, Chosen, Reply}),
      Record ! start(PName),
      Debug ! start(PName) |
	continue_waiting;

    Schedule ? Start, Start =?= start(PId, OpList, Value, Chosen, Prefix),
    arg(SPI_POST, SpiOffsets, SpiOffset),
    SpiOffset =\= unbound,
    tuple(PId), arg(1, PId, PName),
    string(PName),
    string_to_dlist(PName, PNL, []),
    convert_to_string(Prefix, Prefix'),
    string_to_dlist(Prefix', PL, [CHAR_COLON | PNL]),
    list_to_string(PL, PrefixedId) :
      PostId = (Prefix' : PId),
      execute(SpiOffset, {SPI_POST, PostId, OpList, Value, Chosen, Reply}),
      Record ! start(PrefixedId),
      Debug ! start(PostId) |
	continue_waiting;

    Schedule ? Start, Start =?= start(PName, OpList, Value, Chosen, Prefix),
    arg(SPI_POST, SpiOffsets, SpiOffset),
    SpiOffset =\= unbound,
    string(PName),
    string_to_dlist(PName, PNL, []),
    convert_to_string(Prefix, Prefix'),
    string_to_dlist(Prefix', PL, [CHAR_COLON | PNL]),
    list_to_string(PL, PrefixedId) :
      PostId = (Prefix' : {PName}),
      execute(SpiOffset, {SPI_POST, PostId, OpList, Value, Chosen, Reply}),
      Record ! start(PrefixedId),
      Debug ! start(PostId) |
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

    Schedule ? spifunctions(List),
    arity(SpiOffsets, Arity),
    make_tuple(Arity, SpiOffsets'),
    arg(SPI_CLOSE, SpiOffsets', Close),
    arg(SPI_POST, SpiOffsets', Post),
    arg(SPI_STEP, SpiOffsets', Step),
    arg(SPI_INDEX, SpiOffsets', Index) :
      SpiOffsets = _,
      Close = Close'?,
      Post = Post'?,
      Step = Step'?,
      Index = Index'? |
	processor#link(lookup(spicomm, SpiOffset), _Ok),
	spifunctions,
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
    arg(SPI_STEP, SpiOffsets, SpiOffset),
    SpiOffset =?= unbound :
      Waiting = _,
      Waiting' = false |
	sum_weights(BasedAnchor, 0, Total),
	total_weight1(MathOffset, BasedAnchor, Now, Total, Wakeup', Now',
			Uniform, NegativeExponential,
			Uniform', NegativeExponential'),
	self;

    Wakeup =?= done,
    arg(SPI_STEP, SpiOffsets, SpiOffset),
    SpiOffset =\= unbound :
      Waiting = _,
      execute(SpiOffset, {SPI_STEP, Now, BasedAnchor, Now', Wakeup'}),
      Debug ! step(Waiting, Wakeup, Wakeup'),
      Waiting' = false |
	self;

/*
 * RCId refers to Request Channel Id (as of now, a string).
 * CNID refers to Channel Name Id (as of now a string or a 2-tuple).
 * CH refers to a spi-channel (as of now, a vector with 12 sub-channels).
 * PId refers to a process signature (the originator of a request).
 */

    Wakeup =?= true(PId1, RCId1, CH1, PId2, RCId2, CH2),
    PId1 =\= (_ : _), PId2 =\= (_ : _),
    arg(1, PId1, PName1),
    arg(1, PId2, PName2),
    read_vector(SPI_CHANNEL_NAME, CH1, CNId1),
    read_vector(SPI_CHANNEL_NAME, CH2, CNId2) :
      Waiting = _,
      Wakeup' = _,
      Waiter ! machine(idle_wait(Wakeup'), _Ok),
      Waiting' = true,
      Record = [Now, end(PName1(RCId1, "->", CNId1)),
		     end(PName2(RCId2, "<-", CNId2)) | Record'?],
      Debug ! done(Now, PId1(RCId1, CNId1), PId2(RCId2, CNId2)) |
	self;

    Wakeup =?= true(PId1, RCId1, CH1, PId2, RCId2, CH2),
    PId1 = (Prefix1 : SId1), arg(1, SId1, PName1),
    string_to_dlist(PName1, PNL1, []),
    string_to_dlist(Prefix1, PX1, [CHAR_COLON | PNL1]),
    list_to_string(PX1, PrefixedName1),
    PId2 = (Prefix2 : SId2), arg(1, SId2, PName2),
    string_to_dlist(PName2, PNL2, []),
    string_to_dlist(Prefix2, PX2, [CHAR_COLON | PNL2]),
    list_to_string(PX2, PrefixedName2),
    read_vector(SPI_CHANNEL_NAME, CH1, CNId1),
    read_vector(SPI_CHANNEL_NAME, CH2, CNId2) :
      Waiting = _,
      Wakeup' = _,
      Waiter ! machine(idle_wait(Wakeup'), _Ok),
      Waiting' = true,
      Record = [Now, end(PrefixedName1(RCId1, "->", CNId1)),
		     end(PrefixedName2(RCId2, "<-", CNId2)) | Record'?],
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
      MathOffset = _,
      Ordinal = _,
      SpiOffsets = _,
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

  /* 
   * Supply default replies to selected requests.
   * (Could improve (?) close(...) to actually close the channels.)
   */
  wait_done(Schedule, SpiOffsets, Done) :-

    Schedule ? new_channel(ChannelName, Channel, _BaseRate) :
      make_channel(Dummy, _) |
	new_channel(ChannelName, Channel, 0,
		    SPI_DEFAULT_WEIGHT_NAME(SPI_DEFAULT_WEIGHT_INDEX),
		    Dummy, _, _, _, SpiOffsets),
        self;
	     
    Schedule ? new_channel(ChannelName, Channel, _ComputeWeight, _BaseRate) :
      make_channel(Dummy, _) |
	new_channel(ChannelName, Channel, 0,
		    SPI_DEFAULT_WEIGHT_NAME(SPI_DEFAULT_WEIGHT_INDEX),
		    Dummy, _, _, _, SpiOffsets),
        self;

    Schedule ? close(_, Reply) :
      Reply = [] |
	self;

    Schedule ? Other,
    Other =\= close(_, _),
    Other =\= new_channel(_, _, _),
    Other =\= new_channel(_, _, _, _) |
	self;

    unknown(Schedule),
    known(Done) :
      SpiOffsets = _ |
	self#reset.

continue_waiting(Schedule, MathOffset, Ordinal, SpiOffsets, Waiter,
		NegativeExponential, Uniform,
		BasedAnchor, InstantaneousAnchor,
		Scheduler, Record, Debug,
		Now, Waiting, Wakeup,
		DefaultWeighter, Cutoff, Start_real_time, Reply) :-

    /* Reply for spifcp transmission */
    Reply =?= true(PId1, RCId1, CH1, PId2, RCId2, CH2),
    PId1 =\= (_ : _), PId2 =\= (_ : _),
    tuple(PId1), arg(1, PId1, PName1),
    tuple(PId2), arg(1, PId2, PName2),
    read_vector(SPI_CHANNEL_NAME, CH1, CNId1),
    read_vector(SPI_CHANNEL_NAME, CH2, CNId2) :
      Record = [Now, end(PName1(RCId1, "->", CNId1)),
		     end(PName2(RCId2, "<-", CNId2)) | Record'?],
      Debug ! done(Now, PId1(RCId1, CNId1), PId2(RCId2, CNId2)) |
	scheduling;

    /* Reply for biospi transmission */
    Reply =?= true(PId1, RCId1, CH1, PId2, RCId2, CH2),
    PId1 = (Prefix1 : SId1), arg(1, SId1, PName1),
    string_to_dlist(PName1, PNL1, []),
    string_to_dlist(Prefix1, PX1, [CHAR_COLON | PNL1]),
    list_to_string(PX1, PrefixedName1),
    PId2 = (Prefix2 : SId2), arg(1, SId2, PName2),
    string_to_dlist(PName2, PNL2, []),
    string_to_dlist(Prefix2, PX2, [CHAR_COLON | PNL2]),
    list_to_string(PX2, PrefixedName2),
    read_vector(SPI_CHANNEL_NAME, CH1, CNId1),
    read_vector(SPI_CHANNEL_NAME, CH2, CNId2) :
      Record = [Now, end(PrefixedName1(RCId1, "->", CNId1)),
		     end(PrefixedName2(RCId2, "<-", CNId2)) | Record'?],
      Debug ! done(Now, PId1(RCId1, CNId1), PId2(RCId2, CNId2)) |
	scheduling;

    otherwise,
    Reply =\= true :
      Debug ! Reply |
	fail(continue_waiting - Reply),
	scheduling;

    Waiting =?= true, Reply =?= true :
      Debug ! ("Waiting = true, Reply = true, Wakeup" = Wakeup)|
	scheduling;

    Waiting =\= true, Reply =?= true :
      Wakeup = _,
      Waiter ! machine(idle_wait(Wakeup'), _Ok),
      Waiting' = true,
      Debug ! ("Reply = true", "Waiting" = Waiting, "Wakeup'" = Wakeup') |
	scheduling;

    /* check for paused. */
    Reply =?= true,
    unknown(Waiting) :
      Debug ! ("Reply = true, unknown(Waiting), Waiting" = Waiting) |
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
    /* "global" for spifcp */
    string_to_dlist("global.", CS2, _Tail) :
      CS1 = CS2,
      Ordinal' = Ordinal,
      Name' = Name;

    string(Name),
    string_to_dlist(Name, CS1, []),
    /* "public" for biospi */
    string_to_dlist("public.", CS2, _Tail) :
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

spifunctions(List, SpiOffset, Close, Index, Post, Step) :-

    List ? C,
    C =:= ascii('c') :
      Close = SpiOffset? |
	self;

    List ? I,
    I =:= ascii('i') :
      Index = SpiOffset? |
	self;

    List ? P,
    P =:= ascii('p') :
      Post = SpiOffset? |
	self;

    List ? S,
    S =:= ascii('s') :
      Step = SpiOffset? |
	self;

    List ? Other,
    otherwise,
    Q := ascii('"'),
    list_to_string([Q,Other,Q], String) |
	fail(not_a_function - String),
	self;

    string(List),
    string_to_dlist(List, List', []) |
	self;

    List =?= [] :
      SpiOffset = _ |
	unify_without_failure(unbound, Close),
	unify_without_failure(unbound, Post),
	unify_without_failure(unbound, Step),
	unify_without_failure(unbound, Index);

    otherwise :
      List' = [] |
	fail(not_a_function(List)),
	self.


new_channel(ChannelName, Channel, BaseRate, ComputeWeight, Scheduler, Reply,
		 BasedAnchor, InstantaneousAnchor, SpiOffsets) :-

    arg(SPI_INDEX, SpiOffsets, SpiOffset), SpiOffset =\= unbound,
    we(Channel),
    arg(1, ComputeWeight, WeighterName),
    convert_to_real(0, Zero) :
      execute(SpiOffset, {SPI_INDEX, WeighterName, WeighterIndex, Result}),
      make_vector(CHANNEL_SIZE, Channel, _),
      store_vector(SPI_BLOCKED, FALSE, Channel),
      store_vector(SPI_CHANNEL_TYPE, SPI_UNKNOWN, Channel),
      store_vector(SPI_CHANNEL_RATE, Zero, Channel),
      store_vector(SPI_CHANNEL_REFS, 1, Channel),
      store_vector(SPI_SEND_ANCHOR, SendAnchor, Channel),
       make_vector(2, LinksS, _),
       SendAnchor = {SPI_MESSAGE_ANCHOR, "", [], 0, 0, 0, [], LinksS, []},
       store_vector(SPI_NEXT_MS, SendAnchor, LinksS),
       store_vector(SPI_PREVIOUS_MS, SendAnchor, LinksS),
      store_vector(SPI_SEND_WEIGHT, 0, Channel),
      store_vector(SPI_RECEIVE_ANCHOR, ReceiveAnchor, Channel),
       make_vector(2, LinksR, _),
       ReceiveAnchor = {SPI_MESSAGE_ANCHOR, "", [], 0, 0, 0, [], LinksR, []},
       store_vector(SPI_NEXT_MS, ReceiveAnchor, LinksR),
       store_vector(SPI_PREVIOUS_MS, ReceiveAnchor, LinksR),
      store_vector(SPI_RECEIVE_WEIGHT, 0, Channel),
      store_vector(SPI_WEIGHT_TUPLE, WeighterTuple?, Channel),
      store_vector(SPI_NEXT_CHANNEL, Channel, Channel),
      store_vector(SPI_PREVIOUS_CHANNEL, Channel, Channel),
      store_vector(SPI_CHANNEL_NAME, ChannelName, Channel) |
	based_or_instantaneous,
	complete_weighter_tuple;

    convert_to_real(0, Zero),
    arg(SPI_INDEX, SpiOffsets, unbound),
    ComputeWeight =?= SPI_DEFAULT_WEIGHT_NAME(_),
    we(Channel) :
      make_vector(CHANNEL_SIZE, Channel, _),
      store_vector(SPI_BLOCKED, FALSE, Channel),
      store_vector(SPI_CHANNEL_TYPE, SPI_UNKNOWN, Channel),
      store_vector(SPI_CHANNEL_RATE, Zero, Channel),
      store_vector(SPI_CHANNEL_REFS, 1, Channel),
      store_vector(SPI_SEND_ANCHOR, SendAnchor, Channel),
       make_vector(2, LinksS, _),
       SendAnchor = {SPI_MESSAGE_ANCHOR, "", [], 0, 0, 0, [], LinksS, []},
       store_vector(SPI_NEXT_MS, SendAnchor, LinksS),
       store_vector(SPI_PREVIOUS_MS, SendAnchor, LinksS),
      store_vector(SPI_SEND_WEIGHT, 0, Channel),
      store_vector(SPI_RECEIVE_ANCHOR, ReceiveAnchor, Channel),
       make_vector(2, LinksR, _),
       ReceiveAnchor = {SPI_MESSAGE_ANCHOR, "", [], 0, 0, 0, [], LinksR, []},
       store_vector(SPI_NEXT_MS, ReceiveAnchor, LinksR),
       store_vector(SPI_PREVIOUS_MS, ReceiveAnchor, LinksR),
      store_vector(SPI_RECEIVE_WEIGHT, 0, Channel),
      store_vector(SPI_WEIGHT_TUPLE,
		SPI_DEFAULT_WEIGHT_NAME(SPI_DEFAULT_WEIGHT_INDEX), Channel),
      store_vector(SPI_NEXT_CHANNEL, Channel, Channel),
      store_vector(SPI_PREVIOUS_CHANNEL, Channel, Channel),
      store_vector(SPI_CHANNEL_NAME, ChannelName, Channel) |
	based_or_instantaneous + (Result = true);

    otherwise :
      BasedAnchor = _,
      InstantaneousAnchor = _,
      SpiOffsets = _,
      Scheduler = _,
      Reply = error(new_channel(ChannelName, Channel, BaseRate, ComputeWeight)).
  
  based_or_instantaneous(ChannelName, BaseRate, Scheduler, Result, Reply,
			 Channel, BasedAnchor, InstantaneousAnchor) :-

    Result =?= true,
    BaseRate =?= infinite :
      BasedAnchor = _,
      store_vector(SPI_CHANNEL_TYPE, SPI_INSTANTANEOUS, Channel),
      Reply = Result,
      DEBUG((ChannelName: instantaneous)) |
	queue_channel(Channel, InstantaneousAnchor);

    Result =?= true,
    number(BaseRate),
    BaseRate =< 0 :
      BasedAnchor = _,
      InstantaneousAnchor = _,
      store_vector(SPI_CHANNEL_TYPE, SPI_SINK, Channel),
      Reply = true,
      DEBUG((ChannelName: sink));

    Result =?= true,
    number(BaseRate),
    BaseRate > 0,
    convert_to_real(BaseRate, BaseRate') :
      InstantaneousAnchor = _,
      store_vector(SPI_CHANNEL_RATE, BaseRate', Channel),
      Reply = Result,
      DEBUG((ChannelName: based_channel)) |
	queue_channel(Channel, BasedAnchor);

    Result =?= true,
    otherwise :
      BasedAnchor = _,
      Channel = _,
      InstantaneousAnchor = _,
      Scheduler = _,
      store_vector(SPI_CHANNEL_TYPE, SPI_SINK, Channel),
      Reply = "invalid base rate"(ChannelName - BaseRate);

    Result =\= true :
      BasedAnchor = _,
      BaseRate = _,
      Channel = _,
      ChannelName = _,
      InstantaneousAnchor = _,
      Scheduler = _,
      store_vector(SPI_CHANNEL_TYPE, SPI_SINK, Channel),
      Reply = Result.

  complete_weighter_tuple(Reply, ComputeWeight, WeighterIndex, WeighterTuple) :-

    Reply = true,
    arg(2, ComputeWeight, Index) :
      Index = WeighterIndex,
      WeighterTuple = ComputeWeight;

    Reply = true,
    otherwise :
      WeighterIndex = _,
      WeighterTuple = SPI_DEFAULT_WEIGHT_NAME(SPI_DEFAULT_WEIGHT_INDEX) |
	fail(invalid_weighter_index(ComputeWeight));

    Reply =\= true :
      ComputeWeight = _,
      WeighterIndex = _,
      WeighterTuple = SPI_DEFAULT_WEIGHT_NAME(SPI_DEFAULT_WEIGHT_INDEX).


queue_channel(Channel, Anchor) :-

    read_vector(SPI_PREVIOUS_CHANNEL, Anchor, OldLast) :
      store_vector(SPI_PREVIOUS_CHANNEL, OldLast, Channel),
      store_vector(SPI_NEXT_CHANNEL, Anchor, Channel),
      store_vector(SPI_NEXT_CHANNEL, Channel, OldLast),
      store_vector(SPI_PREVIOUS_CHANNEL, Channel, Anchor).

reset_default_weighter(SpiOffset, New, Old, Default) :-

    string(New) :
      execute(SpiOffset, {SPI_INDEX, New, Index, Reply}) |
	check_new_default + (New = New(_));

    tuple(New),
    arg(1, New, Name) :
      execute(SpiOffset, {SPI_INDEX, Name, Index, Reply}) |
	check_new_default;

    otherwise :
      SpiOffset = _,
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

execute(MathOffset, Arguments) :-

    Arguments = post(PId, OpList, Value, Chosen, Reply) :
      MathOffset = _,
      Common = {PId, Messages, Value, Chosen},
      Messages = AddMessages? |
	post_pass1,
	post_pass2 ;

    Arguments = step(Now, Anchor, NewNow, Reply) |
	sum_weights(Anchor, 0, Total),
	total_weight;

    Arguments = close(Channels, Reply),
    tuple(Channels),
    N := arity(Channels) :
      MathOffset = _ |
	close_channels(Channels, N, Reply).


/************************** close procedures *********************************/

close_channels(Channels, N, Reply)  :-

    N > 0,
    arg(N, Channels, Channel),
    N--,
    vector(Channel),
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Refs--,
    Refs' > 0 :
      store_vector(SPI_CHANNEL_REFS, Refs', Channel) |
	self;

    N > 0,
    arg(N, Channels, Channel),
    N--,
    vector(Channel),
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Refs--,
    Refs' =:= 0,
    read_vector(SPI_NEXT_CHANNEL, Channel, Next),
    read_vector(SPI_PREVIOUS_CHANNEL, Channel, Previous) :
      store_vector(SPI_CHANNEL_REFS, 0, Channel),
      store_vector(SPI_NEXT_CHANNEL, Next, Previous),
      store_vector(SPI_PREVIOUS_CHANNEL, Previous, Next),
      store_vector(SPI_NEXT_CHANNEL, Channel, Channel),
      store_vector(SPI_PREVIOUS_CHANNEL, Channel, Channel),
      Reply ! N |
	self;

    N > 0,
    arg(N, Channels, Channel),
    N--,
    vector(Channel),
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Refs--,
    Refs' < 0 |
	self;

    N =< 0 :
      Channels  = _,
      Reply = [];

    otherwise :
      Reply = close_failed(N, Channels).


/************************ post procedures ********************************/

post_pass1(OpList, Common, Ok) :-

    OpList ? Operation,
    arg(SPI_MS_TYPE, Operation, MessageType),
    arg(SPI_MS_CHANNEL, Operation, Channel),
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(SPI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= SPI_INSTANTANEOUS,
    MessageType =?= SPI_SEND,
    read_vector(SPI_RECEIVE_ANCHOR, Channel, Anchor),
    arity(Operation) =:= SPI_MS_SIZE |
	do_instantaneous_transmit
		+ (MTX = SPI_RECEIVE_TAG, Message = Anchor, Ambient = []);

    OpList ? Operation,
    arg(SPI_MS_TYPE, Operation, MessageType),
    arg(SPI_MS_CHANNEL, Operation, Channel),
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(SPI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= SPI_INSTANTANEOUS,
    MessageType =?= SPI_SEND,
    read_vector(SPI_RECEIVE_ANCHOR, Channel, Anchor),
    arity(Operation) =:= SPI_AMBIENT_MS_SIZE,
    arg(SPI_MS_AMBIENT, Operation, Ambient) |
	do_instantaneous_transmit
		+ (MTX = SPI_RECEIVE_TAG, Message = Anchor);

    OpList ? Operation,
    arg(SPI_MS_TYPE, Operation, MessageType),
    arg(SPI_MS_CHANNEL, Operation, Channel),
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(SPI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= SPI_INSTANTANEOUS,
    MessageType =?= SPI_RECEIVE,
    read_vector(SPI_SEND_ANCHOR, Channel, Anchor),
    arity(Operation) =:= SPI_MS_SIZE |
	do_instantaneous_transmit
		+ (MTX = SPI_SEND_TAG, Message = Anchor, Ambient = []);

    OpList ? Operation,
    arg(SPI_MS_TYPE, Operation, MessageType),
    arg(SPI_MS_CHANNEL, Operation, Channel),
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(SPI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= SPI_INSTANTANEOUS,
    MessageType =?= SPI_RECEIVE,
    read_vector(SPI_SEND_ANCHOR, Channel, Anchor),
    arity(Operation) =:= SPI_AMBIENT_MS_SIZE,
    arg(SPI_MS_AMBIENT, Operation, Ambient) |
	do_instantaneous_transmit
		+ (MTX = SPI_SEND_TAG, Message = Anchor);

    OpList ? Operation,
    arg(SPI_MS_TYPE, Operation, MessageType),
    arg(SPI_MS_CHANNEL, Operation, Channel),
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(SPI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= SPI_BIMOLECULAR,
    MessageType =?= SPI_SEND :
      store_vector(SPI_BLOCKED, FALSE, Channel) |
	self;

    OpList ? Operation,
    arg(SPI_MS_TYPE, Operation, MessageType),
    arg(SPI_MS_CHANNEL, Operation, Channel),
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(SPI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= SPI_BIMOLECULAR,
    MessageType =?= SPI_RECEIVE :
      store_vector(SPI_BLOCKED, FALSE, Channel) |
	self;

    OpList ? Operation,
    arg(SPI_MS_TYPE, Operation, MessageType),
    arg(SPI_MS_CHANNEL, Operation, Channel),
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(SPI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= SPI_HOMODIMERIZED,
    MessageType =?= SPI_DIMER :
      store_vector(SPI_BLOCKED, FALSE, Channel) |
	self;

    OpList ? Operation,
    arg(SPI_MS_TYPE, Operation, MessageType),
    arg(SPI_MS_CHANNEL, Operation, Channel),
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(SPI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= SPI_UNKNOWN,
    MessageType =?= SPI_SEND :
      store_vector(SPI_CHANNEL_TYPE, SPI_BIMOLECULAR, Channel) |
	self;

    OpList ? Operation,
    arg(SPI_MS_TYPE, Operation, MessageType),
    arg(SPI_MS_CHANNEL, Operation, Channel),
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(SPI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= SPI_UNKNOWN,
    MessageType =?= SPI_RECEIVE :
      store_vector(SPI_CHANNEL_TYPE, SPI_BIMOLECULAR, Channel) |
	self;

    OpList ? Operation,
    arg(SPI_MS_TYPE, Operation, MessageType),
    arg(SPI_MS_CHANNEL, Operation, Channel),
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), Multiplier > 0,
    read_vector(SPI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= SPI_UNKNOWN,
    MessageType =?= SPI_DIMER :
      store_vector(SPI_CHANNEL_TYPE, SPI_HOMODIMERIZED, Channel) |
	self;

    OpList ? Operation,
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    integer(Multiplier), Multiplier =< 0 |
        self;

    OpList ? Operation,
    arg(SPI_MS_CHANNEL, Operation, Channel),
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    read_vector(SPI_CHANNEL_TYPE, Channel, ChannelType),
    ChannelType =?= SPI_SINK |
	self;

    OpList =?= [] :
      Common = _,
      Ok = true;

    otherwise :
      Common = _,
      Ok = invalid_transmission - OpList.


post_pass2(OpList, Reply, Common, Messages, AddMessages, Ok) :-

    Ok =?= true,
    OpList ? Operation,
    arg(SPI_MS_CHANNEL, Operation, Channel),
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    Type =\= SPI_SINK,
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    Multiplier > 0,
    arg(SPI_MS_TYPE, Operation, MessageType),
    arg(SPI_MS_CID, Operation, RCId),
    arg(SPI_MS_TAGS, Operation, Tags),
    arity(Operation) =:= SPI_MS_SIZE :
      AddMessages ! Message? |
	queue_message + (Ambient = []),
	self;

    Ok =?= true,
    OpList ? Operation,
    arg(SPI_MS_CHANNEL, Operation, Channel),
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    Type =\= SPI_SINK,
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    Multiplier > 0,
    arg(SPI_MS_TYPE, Operation, MessageType),
    arg(SPI_MS_CID, Operation, RCId),
    arg(SPI_MS_TAGS, Operation, Tags),
    arity(Operation) =:= SPI_AMBIENT_MS_SIZE,
    arg(SPI_MS_AMBIENT, Operation, Ambient) :
      AddMessages ! Message? |
	queue_message,
	self;

    Ok =?= true,
    OpList ? Operation,
    arg(SPI_MS_CHANNEL, Operation, Channel),
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    Type =\= SPI_SINK,
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    Multiplier =< 0 |
	self;

    Ok =?= true,
    OpList ? Operation,
    arg(SPI_MS_CHANNEL, Operation, Channel),
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    Type =?= SPI_SINK |
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
					Ambient, Ok) :-

    arg(SPI_MESSAGE_LINKS, Message, MessageLinks),
    read_vector(SPI_NEXT_MS, MessageLinks, Message'),
    Message' =\= Anchor,
    arg(SPI_COMMON, Message', CommonQ),
    arg(SPI_OP_CHOSEN, CommonQ, Chosen), not_we(Chosen) |
	self;

    arg(SPI_MESSAGE_LINKS, Message, MessageLinks),
    read_vector(SPI_NEXT_MS, MessageLinks, Message'),
    Message' =\= Anchor,
    Ambient =\= [],
    arg(SPI_AMBIENT_CHANNEL, Message', AmbientQ),
    Ambient =?= AmbientQ |
	self;

    arg(SPI_MESSAGE_LINKS, Message, MessageLinks),
    read_vector(SPI_NEXT_MS, MessageLinks, Message'),
    Message' =\= Anchor,
    arg(SPI_MS_CID, Message', RCIdQ),
    arg(SPI_MS_CHANNEL, Message', ChannelQ),
    arg(SPI_COMMON, Message', CommonQ),
    arg(MTX, Message', MessageTag),
    CommonQ = {PIdQ, MsList, ValueQ, ChosenQ},
    Common = {PIdR, _MsList, ValueR, ChosenR},
    arg(SPI_MS_CID, Operation, RCIdR),
    arg(SPI_MS_CHANNEL, Operation, ChannelR),
    arg(SPI_MS_TAGS, Operation, OperationTag),
    Ambient =?= [] :
      Anchor = _,
      OpList = _,
      ValueQ = ValueR,
      ChosenQ = MessageTag,
      ChosenR = OperationTag,
      Ok = true(PIdQ, RCIdQ, ChannelQ, PIdR, RCIdR, ChannelR) |
	discount(MsList);

    arg(SPI_MESSAGE_LINKS, Message, MessageLinks),
    read_vector(SPI_NEXT_MS, MessageLinks, Message'),
    Message' =\= Anchor,
    arg(SPI_MS_CID, Message', RCIdQ),
    arg(SPI_MS_CHANNEL, Message', ChannelQ),
    arg(SPI_COMMON, Message', CommonQ),
    arg(MTX, Message', MessageTag),
    CommonQ = {PIdQ, MsList, ValueQ, ChosenQ},
    Common = {PIdR, _MsList, ValueR, ChosenR},
    arg(SPI_MS_CID, Operation, RCIdR),
    arg(SPI_MS_CHANNEL, Operation, ChannelR),
    arg(SPI_MS_TAGS, Operation, OperationTag),
    Ambient =\= [],
    arg(SPI_AMBIENT_CHANNEL, Message', AmbientQ),
    Ambient =\= AmbientQ :
      Anchor = _,
      OpList = _,
      ValueQ = ValueR,
      ChosenQ = MessageTag,
      ChosenR = OperationTag,
      Ok = true(PIdQ, RCIdQ, ChannelQ, PIdR, RCIdR, ChannelR) |
	discount(MsList);

    % This can happen (to the monitor). 
    Common = {_, _, _, Chosen}, not_we(Chosen) :
      Ambient = _,
      Anchor = _,
      Operation = _,
      Message = _,
      MTX = _ |
	post_pass1;

  % Test for end of list
    arg(SPI_MESSAGE_LINKS, Message, MessageLinks),
    read_vector(SPI_NEXT_MS, MessageLinks, Message'),
    Message' =?= Anchor :
      Ambient = _,
      Operation = _,
      MTX = _ |
	post_pass1;

    % This Message has the same Chosen as the Operation -
    % mixed communications which ARE NOT homodimerized.
    arg(SPI_MESSAGE_LINKS, Message, MessageLinks),
    read_vector(SPI_NEXT_MS, MessageLinks, Message'),
    arg(SPI_COMMON, Message', CommonQ),
    CommonQ = {_, _, _, Chosen},
    Common = {_, _, _, Chosen} |
	self.


/*************************** step procedures *********************************/

sum_weights(Channel, Sum, Total) :-

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    Type =?= SPI_CHANNEL_ANCHOR :
      Total = Sum;

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(SPI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    Type =?= SPI_BIMOLECULAR,
    read_vector(SPI_CHANNEL_RATE, Channel', Rate),
    read_vector(SPI_SEND_WEIGHT, Channel', SendWeight),
    read_vector(SPI_RECEIVE_WEIGHT, Channel', ReceiveWeight),
    Sum += Rate*SendWeight*ReceiveWeight |
	self;    

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(SPI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    Type =?= SPI_HOMODIMERIZED,
    read_vector(SPI_DIMER_ANCHOR, Channel', Anchor),
    arg(SPI_MESSAGE_LINKS, Anchor, FirstLink),
    read_vector(SPI_NEXT_MS, FirstLink, Message),
    arg(SPI_MESSAGE_LINKS, Message, DimerLink),
    read_vector(SPI_NEXT_MS, DimerLink, DimerMs),
    arg(SPI_MS_TYPE, DimerMs, MsType),
    MsType =\= SPI_MESSAGE_ANCHOR,
    read_vector(SPI_CHANNEL_RATE, Channel', Rate),
    read_vector(SPI_DIMER_WEIGHT, Channel', DimerWeight),
    Sum += Rate*DimerWeight*(DimerWeight-1)/2 |
	self;

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    otherwise |
	self.


total_weight(MathOffset, Anchor, Now, Total, Reply, NewNow) :-

    Total =< 0 :
      Anchor = _,
      MathOffset = _,
      NewNow = Now,
      Reply = true;

    Total > 0 :
      execute(MathOffset, {RAN, 0, Uniform}),
      execute(MathOffset, {LN, Uniform, NegativeExponential}),
      execute(MathOffset, {RAN, 0, Uniform'}) |
	Residue := Uniform'*Total,
	select_channel + (Channel = Anchor),
	NewNow := Now - NegativeExponential/Total.

total_weight1(MathOffset, Anchor, Now, Total, Reply, NewNow, U, NE, NU, NNE) :-

    Total =< 0 :
      Anchor = _,
      MathOffset = _,
      NewNow = Now,
      NU = U,
      NNE = NE,
      Reply = true;

    Total > 0,
    Residue := U*Total,
    Now' := Now - NE/Total :
      NewNow = Now',
      execute(MathOffset, {RAN, 0, Uniform}),
      execute(MathOffset, {LN, Uniform, NNE}),
      execute(MathOffset, {RAN, 0, NU}) |
	select_channel + (Channel = Anchor).

select_channel(Residue, Channel, Reply) :-

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(SPI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    Type =?= SPI_BIMOLECULAR,
    read_vector(SPI_CHANNEL_RATE, Channel', Rate),
    read_vector(SPI_SEND_WEIGHT, Channel', SendWeight),
    SendWeight > 0,
    read_vector(SPI_RECEIVE_WEIGHT, Channel', ReceiveWeight),
    ReceiveWeight > 0,
    Residue -= Rate*SendWeight*ReceiveWeight,
    Residue' =< 0 |
	do_bimolecular_transmit;

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(SPI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    Type =?= SPI_BIMOLECULAR,
    read_vector(SPI_CHANNEL_RATE, Channel', Rate),
    read_vector(SPI_SEND_WEIGHT, Channel', SendWeight),
    SendWeight > 0,
    read_vector(SPI_RECEIVE_WEIGHT, Channel', ReceiveWeight),
    ReceiveWeight > 0,
    Residue -= Rate*SendWeight*ReceiveWeight,
    Residue' > 0 |
	self;

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(SPI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    Type =?= SPI_HOMODIMERIZED,
    read_vector(SPI_DIMER_ANCHOR, Channel', Anchor),
    arg(SPI_MESSAGE_LINKS, Anchor, FirstLink),
    read_vector(SPI_NEXT_MS, FirstLink, Message),
    arg(SPI_MESSAGE_LINKS, Message, DimerLink),
    read_vector(SPI_NEXT_MS, DimerLink, DimerMs),
    arg(SPI_MS_TYPE, DimerMs, MsType),
    MsType =\= SPI_MESSAGE_ANCHOR,
    read_vector(SPI_CHANNEL_RATE, Channel', Rate),
    read_vector(SPI_DIMER_WEIGHT, Channel', DimerWeight),
    Residue -= Rate*DimerWeight*(DimerWeight-1)/2,
    Residue' =< 0 |
	do_homodimerized_transmit;

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(SPI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    Type =?= SPI_HOMODIMERIZED,
    read_vector(SPI_DIMER_ANCHOR, Channel', Anchor),
    arg(SPI_MESSAGE_LINKS, Anchor, FirstLink),
    read_vector(SPI_NEXT_MS, FirstLink, Message),
    arg(SPI_MESSAGE_LINKS, Message, DimerLink),
    read_vector(SPI_NEXT_MS, DimerLink, DimerMs),
    arg(SPI_MS_TYPE, DimerMs, MsType),
    MsType =\= SPI_MESSAGE_ANCHOR,
    read_vector(SPI_CHANNEL_RATE, Channel', Rate),
    read_vector(SPI_DIMER_WEIGHT, Channel', DimerWeight),
    Residue -= Rate*DimerWeight*(DimerWeight-1)/2,
    Residue' > 0 |
	self;

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    otherwise |
	self.

/****** based transmit - complete a transmission for a pair of messages ******/

do_bimolecular_transmit(Channel, Reply) :-

    read_vector(SPI_SEND_ANCHOR, Channel, SendAnchor),
    arg(SPI_MESSAGE_LINKS, SendAnchor, SendLinks),
    read_vector(SPI_NEXT_MS, SendLinks, Send),
    read_vector(SPI_RECEIVE_ANCHOR, Channel, ReceiveAnchor),
    arg(SPI_MESSAGE_LINKS, ReceiveAnchor, ReceiveLinks),
    read_vector(SPI_NEXT_MS, ReceiveLinks, Receive) |
	do_bimolecular_send(Channel, Reply, Send, Receive).

do_bimolecular_send(Channel, Reply, Send, Receive) :-

    arg(SPI_MS_TYPE, Send, SPI_SEND),
    arg(SPI_MS_CID, Send, SendRCId),
    arg(SPI_MS_CHANNEL, Send, SendChannel),
    arg(SPI_SEND_TAG, Send, SendTag),
    arg(SPI_COMMON, Send, SendCommon),
    arg(SPI_AMBIENT_CHANNEL, Send, []),
    arg(SPI_MS_TYPE, Receive, SPI_RECEIVE),
    arg(SPI_MS_CID, Receive, ReceiveRCId),
    arg(SPI_MS_CHANNEL, Receive, ReceiveChannel),
    arg(SPI_RECEIVE_TAG, Receive, ReceiveTag),
    arg(SPI_COMMON, Receive, ReceiveCommon),
    SendCommon =?= {SendPId, SendList, SendValue, SendChosen},
    ReceiveCommon =?= {ReceivePId, ReceiveList, ReceiveValue, ReceiveChosen} :
      Channel = _,
      SendChosen = SendTag,
      ReceiveChosen = ReceiveTag,
      ReceiveValue = SendValue?,
      Reply = true(SendPId, SendRCId, SendChannel,
		   ReceivePId, ReceiveRCId, ReceiveChannel) |
	discount(SendList),
	discount(ReceiveList);

    arg(SPI_MS_TYPE, Send, SPI_SEND),
    arg(SPI_MS_CHANNEL, Send, SendChannel),
    arg(SPI_SEND_TAG, Send, SendTag),
    arg(SPI_MS_CID, Send, SendRCId),
    arg(SPI_COMMON, Send, SendCommon),
    arg(SPI_AMBIENT_CHANNEL, Send, SendAmbient),
    SendAmbient =\= [],
    arg(SPI_MS_TYPE, Receive, SPI_RECEIVE),
    arg(SPI_MS_CID, Receive, ReceiveRCId),
    arg(SPI_MS_CHANNEL, Receive, ReceiveChannel),
    arg(SPI_RECEIVE_TAG, Receive, ReceiveTag),
    arg(SPI_COMMON, Receive, ReceiveCommon),
    ReceiveCommon =?= {ReceivePId, ReceiveList, ReceiveValue, ReceiveChosen},
    SendCommon =?= {SendPId, SendList, SendValue, SendChosen},
    arg(SPI_AMBIENT_CHANNEL, Receive, ReceiveAmbient),
    SendAmbient =\= ReceiveAmbient :
      Channel = _,
      SendChosen = SendTag,
      ReceiveChosen = ReceiveTag,
      ReceiveValue = SendValue?,
      Reply = true(SendPId, SendRCId, SendChannel,
		   ReceivePId, ReceiveRCId, ReceiveChannel) |
	discount(SendList),
	discount(ReceiveList);

    arg(SPI_MS_TYPE, Send, SPI_SEND),
    arg(SPI_AMBIENT_CHANNEL, Send, SendAmbient),
    SendAmbient =\= [],
    arg(SPI_AMBIENT_CHANNEL, Receive, ReceiveAmbient),
    SendAmbient =?= ReceiveAmbient |
	self;

    arg(SPI_MS_TYPE, Send, SPI_SEND),
    arg(SPI_COMMON, Send, SendCommon),
    arg(SPI_MESSAGE_LINKS, Send, SendLinks),
    arg(SPI_MS_TYPE, Receive, SPI_RECEIVE),
    arg(SPI_COMMON, Receive, ReceiveCommon),
    % This Send has the same Chosen as the Receive -
    % mixed communications which ARE NOT homodimerized.
    SendCommon =?= {_, _, _, Chosen},
    ReceiveCommon =?= {_, _, _, Chosen},
    read_vector(SPI_NEXT_MS, SendLinks, Send') |
	self;

    arg(SPI_MS_TYPE, Send, SPI_MESSAGE_ANCHOR),
    arg(SPI_MESSAGE_LINKS, Send, SendLinks),
    arg(SPI_MS_TYPE, Receive, SPI_RECEIVE),
    arg(SPI_MESSAGE_LINKS, Receive, ReceiveLinks),
    read_vector(SPI_NEXT_MS, SendLinks, Send'),
    read_vector(SPI_NEXT_MS, ReceiveLinks, Receive') |
	self;

    arg(SPI_MS_TYPE, Receive, SPI_MESSAGE_ANCHOR) :
      Send = _,
      store_vector(SPI_BLOCKED, TRUE, Channel),
      Reply = done.


do_homodimerized_transmit(Channel, Reply) :-

    read_vector(SPI_DIMER_ANCHOR, Channel, DimerAnchor),
    arg(SPI_MESSAGE_LINKS, DimerAnchor, DimerLinks),
    read_vector(SPI_NEXT_MS, DimerLinks, Receive),
    arg(SPI_MESSAGE_LINKS, Receive, ReceiveLinks),
    read_vector(SPI_NEXT_MS, ReceiveLinks, Dimer) |
	do_homodimerized_send(Channel, Reply, Receive, Dimer).

do_homodimerized_send(Channel, Reply, Receive, Dimer) :-

    arg(SPI_MS_TYPE, Receive, SPI_DIMER),
    arg(SPI_MS_TYPE, Dimer, SPI_DIMER),
    arg(SPI_MS_CID, Receive, ReceiveRCId),
    arg(SPI_MS_CHANNEL, Receive, ReceiveChannel),
    arg(SPI_RECEIVE_TAG, Receive, ReceiveTag),
    arg(SPI_COMMON, Receive, ReceiveCommon),
    arg(SPI_MS_CID, Dimer, DimerRCId),
    arg(SPI_MS_CHANNEL, Dimer, DimerChannel),
    arg(SPI_DIMER_TAG, Dimer, DimerTag),
    arg(SPI_COMMON, Dimer, DimerCommon),
    ReceiveCommon =?= {ReceivePId, ReceiveList, ReceiveValue, ReceiveChosen},
    we(ReceiveChosen),
    DimerCommon =?= {DimerPId, DimerList, DimerValue, DimerChosen},
    we(DimerChosen),
    arg(SPI_AMBIENT_CHANNEL, Receive, []) :
      Channel = _,
      ReceiveChosen = ReceiveTag,
      DimerChosen = DimerTag,
      ReceiveValue = DimerValue?,
      Reply = true(ReceivePId, ReceiveRCId, ReceiveChannel,
		   DimerPId, DimerRCId, DimerChannel) |
	discount(ReceiveList),
	discount(DimerList);

    arg(SPI_MS_TYPE, Receive, SPI_DIMER),
    arg(SPI_MS_TYPE, Dimer, SPI_DIMER),
    arg(SPI_MS_CID, Receive, ReceiveRCId),
    arg(SPI_MS_CHANNEL, Receive, ReceiveChannel),
    arg(SPI_RECEIVE_TAG, Receive, ReceiveTag),
    arg(SPI_COMMON, Receive, ReceiveCommon),
    arg(SPI_MS_CID, Dimer, DimerRCId),
    arg(SPI_MS_CHANNEL, Dimer, DimerChannel),
    arg(SPI_DIMER_TAG, Dimer, DimerTag),
    arg(SPI_COMMON, Dimer, DimerCommon),
    ReceiveCommon =?= {ReceivePId, ReceiveList, ReceiveValue, ReceiveChosen},
    we(ReceiveChosen),
    DimerCommon =?= {DimerPId, DimerList, DimerValue, DimerChosen},
    we(DimerChosen),
    arg(SPI_AMBIENT_CHANNEL, Receive, ReceiveAmbient),
    ReceiveAmbient =\= [],
    arg(SPI_AMBIENT_CHANNEL, Dimer, DimerAmbient),
    ReceiveAmbient =\= DimerAmbient :
      Channel = _,
      ReceiveChosen = ReceiveTag,
      DimerChosen = DimerTag,
      ReceiveValue = DimerValue?,
      Reply = true(ReceivePId, ReceiveRCId, ReceiveChannel,
		   DimerPId, DimerRCId, DimerChannel) |
	discount(ReceiveList),
	discount(DimerList);

    arg(SPI_MS_TYPE, Receive, SPI_DIMER),
    arg(SPI_MS_TYPE, Dimer, SPI_DIMER),
    arg(SPI_AMBIENT_CHANNEL, Receive, ReceiveAmbient),
    ReceiveAmbient =\= [],
    arg(SPI_AMBIENT_CHANNEL, Dimer, DimerAmbient),
    ReceiveAmbient =?= DimerAmbient,
    arg(SPI_MESSAGE_LINKS, Dimer, DimerLinks),
    read_vector(SPI_NEXT_MS, DimerLinks, Dimer') |
	self;

    arg(SPI_MS_TYPE, Receive, SPI_DIMER),
    arg(SPI_MS_TYPE, Dimer, SPI_DIMER),
    arg(SPI_COMMON, Receive, ReceiveCommon),
    arg(SPI_COMMON, Dimer, DimerCommon),
    % This Receive has the same Chosen as the Send -
    % communications which ARE homodimerized AND mixed.
    ReceiveCommon =?= {_, _, _, Chosen},
    DimerCommon =?= {_, _, _, Chosen},
    arg(SPI_MESSAGE_LINKS, Dimer, DimerLinks),
    read_vector(SPI_NEXT_MS, DimerLinks, Dimer') |
	self;

    arg(SPI_MS_TYPE, Dimer, SPI_MESSAGE_ANCHOR) :
      Receive = _,
      store_vector(SPI_BLOCKED, TRUE, Channel),
      Reply = done.


/***************************** Utilities ************************************/

discount(MsList) :-

    MsList ? Message,
    arg(SPI_MS_TYPE, Message, MessageType),
    arg(SPI_MS_CHANNEL, Message, Channel),
    arg(SPI_MS_MULTIPLIER, Message, Multiplier),
    arg(SPI_MESSAGE_LINKS, Message, Links),
    read_vector(SPI_NEXT_MS, Links, NextMessage),
    arg(SPI_MESSAGE_LINKS, NextMessage, NextLinks),
    read_vector(SPI_PREVIOUS_MS, Links, PreviousMessage),
    arg(SPI_MESSAGE_LINKS, PreviousMessage, PreviousLinks),
    MessageType = SPI_SEND,
    read_vector(SPI_SEND_WEIGHT, Channel, Weight),
    Weight -= Multiplier :
      store_vector(SPI_SEND_WEIGHT, Weight', Channel),
      store_vector(SPI_NEXT_MS, NextMessage, PreviousLinks),
      store_vector(SPI_PREVIOUS_MS, PreviousMessage, NextLinks) |
	self;

    MsList ? Message,
    arg(SPI_MS_TYPE, Message, MessageType),
    arg(SPI_MS_CHANNEL, Message, Channel),
    arg(SPI_MS_MULTIPLIER, Message, Multiplier),
    arg(SPI_MESSAGE_LINKS, Message, Links),
    read_vector(SPI_NEXT_MS, Links, NextMessage),
    arg(SPI_MESSAGE_LINKS, NextMessage, NextLinks),
    read_vector(SPI_PREVIOUS_MS, Links, PreviousMessage),
    arg(SPI_MESSAGE_LINKS, PreviousMessage, PreviousLinks),
    MessageType =?= SPI_RECEIVE,
    read_vector(SPI_RECEIVE_WEIGHT, Channel, Weight),
    Weight -= Multiplier :
      store_vector(SPI_RECEIVE_WEIGHT, Weight', Channel),
      store_vector(SPI_NEXT_MS, NextMessage, PreviousLinks),
      store_vector(SPI_PREVIOUS_MS, PreviousMessage, NextLinks) |
	self;

    MsList ? Message,
    arg(SPI_MS_TYPE, Message, MessageType),
    arg(SPI_MS_CHANNEL, Message, Channel),
    arg(SPI_MS_MULTIPLIER, Message, Multiplier),
    arg(SPI_MESSAGE_LINKS, Message, Links),
    read_vector(SPI_NEXT_MS, Links, NextMessage),
    arg(SPI_MESSAGE_LINKS, NextMessage, NextLinks),
    read_vector(SPI_PREVIOUS_MS, Links, PreviousMessage),
    arg(SPI_MESSAGE_LINKS, PreviousMessage, PreviousLinks),
    MessageType =?= SPI_DIMER,
    read_vector(SPI_DIMER_WEIGHT, Channel, Weight),
    Weight -= Multiplier :
      store_vector(SPI_DIMER_WEIGHT, Weight', Channel),
      store_vector(SPI_NEXT_MS, NextMessage, PreviousLinks),
      store_vector(SPI_PREVIOUS_MS, PreviousMessage, NextLinks) |
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


queue_message(MessageType, RCId, Channel, Multiplier, Tags, Ambient,
			Common, Message) :-

    MessageType =?= SPI_SEND,
    read_vector(SPI_SEND_ANCHOR, Channel, Anchor),
    read_vector(SPI_SEND_WEIGHT, Channel, Weight),
    Weight += Multiplier :
      store_vector(SPI_SEND_WEIGHT, Weight', Channel),
      Message = {MessageType, RCId, Channel, Multiplier,
			Tags, 0, Common, Links?, Ambient} |
	queue_to_anchor;

    MessageType =?= SPI_RECEIVE,
    read_vector(SPI_RECEIVE_ANCHOR, Channel, Anchor),
    read_vector(SPI_RECEIVE_WEIGHT, Channel, Weight),
    Weight += Multiplier :
      store_vector(SPI_RECEIVE_WEIGHT, Weight', Channel),
      Message = {MessageType, RCId, Channel, Multiplier,
			0, Tags, Common, Links?, Ambient} |
	queue_to_anchor;

    MessageType =?= SPI_DIMER,
    read_vector(SPI_DIMER_ANCHOR, Channel, Anchor),
    Tags = {SendTag, ReceiveTag},
    read_vector(SPI_DIMER_WEIGHT, Channel, Weight),
    Weight += Multiplier :
      store_vector(SPI_DIMER_WEIGHT, Weight', Channel),
      Message = {MessageType, RCId, Channel, Multiplier,
			SendTag, ReceiveTag, Common, Links?, Ambient} |
	queue_to_anchor.

queue_to_anchor(Message, Anchor, Links) :-

    arg(SPI_MESSAGE_LINKS, Anchor, AnchorLinks),
    read_vector(SPI_PREVIOUS_MS, AnchorLinks, PreviousMessage),
    arg(SPI_MESSAGE_LINKS, PreviousMessage, PreviousLinks) :
      make_vector(2, Links, _),
      store_vector(SPI_PREVIOUS_MS, PreviousMessage, Links),
      store_vector(SPI_NEXT_MS, Anchor, Links),
      store_vector(SPI_NEXT_MS, Message, PreviousLinks),
      store_vector(SPI_PREVIOUS_MS, Message, AnchorLinks).
