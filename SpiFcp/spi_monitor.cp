-monitor(initialize).
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
** sub-functions: Post, Close, Step, Index, Rate (see spi_constants.cp).
**
** To test one or more C-executables, set the other arguments "unbound" - e.g. 
** 
** SPIOFFSETS => {unbound, unbound, SpiOffset, SpiOffset, unbound}
**
** to allow the monitor to call the C step function and the C index function,
** and to execute the other operations with fcp code.  Note that the
** C index function and the C rate function are not implemented in fcp for
** non-standard weighters.
*/

%SPIOFFSETS => {SpiOffset, SpiOffset, SpiOffset, SpiOffset, SpiOffset}.
SPIOFFSETS => {SpiOffset, SpiOffset, SpiOffset, SpiOffset, SpiOffset}.
%SPIOFFSETS => {unbound, unbound, unbound, unbound, unbound}.

initialize(In) :-

    In =?= [] :
      true;

    In =\= [] :
      SpiOffset = _,
      Options = [3],		% preset depth - others default
      Ordinal = 1,
      SpiOffsets = SPIOFFSETS,
      DefaultWeighter = SPI_DEFAULT_WEIGHT_NAME(SPI_DEFAULT_WEIGHT_INDEX) |
	processor#link(lookup(spicomm, SpiOffset), OkComm),
	check_spicomm(OkComm, SpiOffsets, SpiOffsets'),
	serve.

  check_spicomm(Ok, DefaultSpiOffsets, SpiOffsets) :-

    Ok =\= true :
      DefaultSpiOffsets = _,
      SpiOffsets = {unbound, unbound, unbound, unbound, unbound} |
	computation#comment(("No spicomm" : "No non-standard weighters."));

    Ok =?= true :
      SpiOffsets = DefaultSpiOffsets.

/*
** server monitors the input stream (In).
**
** It recognises:
**
**    get_global_channels(List?^)
**    global_channels(List)
**    global_channels(List, Scheduler^)
**    new_channel(ChannelName, Channel, BaseRate)
**    new_channel(ChannelName, Channel, ComputeWeight, BaseRate)
**    options(New, Old?^)
**    record(List?^)
**    reset
**    reset(DefaultWeighter, SpiOffsets, Ordinal)
**    scheduler(Scheduler^)
**    spifunctions(List)
**    status(List?^)
**
** and the debugging aids:
**
**    debug(Debug?^)
**    end_debug
**
** State
**
**   Options - for spi_utils functions
**   Globals - sorted (bounded) list of global channel {name,baserate}
**   Scheduler - Channel to Scheduling input
**
** Side-effects
**
**   Close Scheduler at end of In or reset command.
*/

serve(In, Options, Ordinal, SpiOffsets, DefaultWeighter) :-
	Globals = [{0, _, -1, ""}, {[], _, -1, ""}],
	processor#link(lookup(math, MathOffset), OkMath),
	server,
	start_scheduling.

server(In, Options, Scheduler, Globals) :-

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
	merge_global_channels(List, Globals, Globals', "", Scheduler, _),
	self;

    In ? global_channels(List, ReadyScheduler) |
	merge_global_channels(List, Globals, Globals', "",
			      Scheduler, ReadyScheduler),
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

    In ? record(Record) :
      Record = Record'?,
      write_channel(record(Record'), Scheduler) |
	self;

    In ? reset :
      write_channel(state(DefaultWeighter, SpiOffsets, _Ordinal), Scheduler),
      In'' = [reset(DefaultWeighter, SpiOffsets, 1) | In'] |
	self;

    In ? reset(DefaultWeighter, SpiOffsets, Ordinal) :
      Globals = _,
      close_channel(Scheduler) |
	serve;

    In ? scheduler(Scheduler^) |
	self;

    In ? spifunctions(List) :
      write_channel(spifunctions(List), Scheduler) |
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

/***************************** Utilities ************************************/

/* merge_global_channels
**
** Input:
**
**   List - sorted list in {Name, Channel^, BaseRate}.
**   Globals - (bounded) sorted list of {Name, BaseRate}.
**   Last - Name of previous element of Globals - initially "".
**   Scheduler - FCP channel to scheduling monitor.
**
** Output:
**
**   NewGlobals - updated global list.
**
** Processing:
**
**   Merge new Name Channels to NewGlobals.
**   Call scheduling to create each new Channel.
*/

merge_global_channels(List, Globals, NewGlobals, Last,
		      Scheduler, ReadyScheduler) :-

    List =?= [] :
      Last = _,
      ReadyScheduler = Scheduler,
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
      ReadyScheduler = Scheduler,
      NewGlobals = Globals |
	fail(merge_global_channels(List)).

/* copy_globals
**
** Input:
**
**   Globals - (bounded) list of {Name, Channel, BaseRate}
**
** Output:
**
**   List - Globals exluding bounding entries
**   Ends - bounding entries of Globals
*/

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

/***************************** Scheduling ***********************************/ 

start_scheduling(Scheduler, MathOffset, Ordinal, SpiOffsets, DefaultWeighter,
			OkMath):-

    OkMath =\= true :
      DefaultWeighter = _,
      MathOffset = _,
      Ordinal = _,
      SpiOffsets = _,
      make_channel(Scheduler, _) |
	fail(math_offset(OkMath));

    OkMath =?= true,
    info(REALTIME, Start_real_time),
    convert_to_real(0, Zero),
    convert_to_real(MAXTIME, MaxTime) :
      make_channel(Scheduler, Schedule) |
	make_channel_anchor(based, BasedAnchor),
	make_channel_anchor(instantaneous, InstantaneousAnchor),
	processor#Waiter?,
	scheduling(Schedule, MathOffset, Ordinal, SpiOffsets, Waiter,
				Scheduler, _Recording, _Debug,
				Zero, false, _Wakeup,
				DefaultWeighter, MaxTime, Start_real_time,
				BasedAnchor, InstantaneousAnchor).

  make_channel_anchor(Name, Anchor) :-

    convert_to_real(0, Zero) :
      make_vector(CHANNEL_SIZE, Anchor, _),
      store_vector(SPI_BLOCKED, FALSE, Anchor),
      store_vector(SPI_CHANNEL_TYPE, SPI_CHANNEL_ANCHOR, Anchor),
      store_vector(SPI_CHANNEL_RATE, Zero, Anchor),
      store_vector(SPI_CHANNEL_REFS, 1, Anchor),
      store_vector(SPI_SEND_ANCHOR, SendAnchor, Anchor),
       make_vector(2, LinksS, _),
       SendAnchor = {SPI_MESSAGE_ANCHOR, "", [], 0, 0, 0, [], LinksS, []},
       store_vector(SPI_NEXT_MS, SendAnchor, LinksS),
       store_vector(SPI_PREVIOUS_MS, SendAnchor, LinksS),
      store_vector(SPI_SEND_WEIGHT, 0, Anchor),
      store_vector(SPI_RECEIVE_ANCHOR, ReceiveAnchor, Anchor),
       make_vector(2, LinksR, _),
       ReceiveAnchor = {SPI_MESSAGE_ANCHOR, "", [], 0, 0, 0, [], LinksR, []},
       store_vector(SPI_NEXT_MS, ReceiveAnchor, LinksR),
       store_vector(SPI_PREVIOUS_MS, ReceiveAnchor, LinksR),
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
**    close(ChannelTuple)
**    close(ChannelTuple, Reply)
**    cutoff(Now')
**    default_weighter(Weighter)
**    input(Schedule?^, Schedule')
**    new_channel(ChannelName, Channel^, BaseRate)
**    new_channel(ChannelName, Channel^, ComputeWeight, BaseRate)
**    ordinal(Old?^, New)
**    record(Record?^)
**    record_item(Item)
**    end_record(Record'?^)
**    start(Signature, OpList, Value, Chosen)
**    start(Signature, OpList, Value, Chosen, Prefix)
**
** and the debugging aids:
**
**    debug(Debug?^)
**    end_debug
**    diagnostic(ApplicationDiagnostic)
**    spifunctions(CharacterCodes)
**    pause(Continue)
**    state(DefaultWeighter^, SpiOffsets^, Ordinal^)
**    status(ListOf NamedDetails^)
**    step
**    step(Resume)
**
** State:
**
**   DefaultWeighter - assigned to new Channel, unless Weighter provide
**   Ordinal - Unique index assigned to a non - "public."/".global" file
**     also in Status
**   SpiOffsets - magic numbers (or "unknown") for C-coded functions
**
** Status:
**
**   BasedAnchor - anchor for (circular) chain of based-rate Channels
**   Cutoff - least upper bound of internal timer (:Now) - initially large
**   InstantaneousAnchor - anchor for (circular) list of infinite rate Channels
**   Now - 0-based internal clock
**   Start_real_time - real time when Cutoff set (Now = 0) - not in statuslist
**   Waiting - waiting for Wakeup signal = true/false
**
** Signal:
**
**   Wakeup - (usually) idle indicator; also communication completion
**
** Output (Streams):
**
**   Debug  - scheduling debugging info
**   Record - recorded actions for analysis
**   Waiter - Logix system requests to wait for idle
**
** Processing:
**
**   Maintain time  Now
**
** Whenever the system becomes idle, execute(SpiOffset, {SPI_STEP, ...})
** to select and complete a transmission (internal execute is used when
** the SPI function is unknown).
**
** Record:
**
**   Changes to  Now
**   Start Communication
**   Complete Transmission
*/

STATUS => [anchors([BasedAnchor, InstantaneousAnchor]),
	   cutoff(Cutoff), debug(Debug?), ordinal(Ordinal),
	   now(Now), record(Record?), waiting(Waiting)].


scheduling(Schedule, MathOffset, Ordinal, SpiOffsets, Waiter,
		Scheduler, Record, Debug,
		Now, Waiting, Wakeup,
		DefaultWeighter, Cutoff, Start_real_time,
		BasedAnchor, InstantaneousAnchor) :-

    Schedule =?= [] :
      Cutoff = _,
      BasedAnchor = _,
      DefaultWeighter = _,
      InstantaneousAnchor = _,
      Now = _,
      MathOffset = _,
      Ordinal = _,
      Scheduler = _,
      SpiOffsets = _,
      Start_real_time = _,
      Waiting = _,
      Wakeup = _,
      Waiter = [],
      Record = [],
      Debug = [];

    /* Close channels - i.e. decrement counts and release when unreferenced. */
    Schedule ? close(Channels),
    known(Channels),
    arg(SPI_CLOSE, SpiOffsets, SpiOffset),
    SpiOffset =?= unbound :
      STOPPED |
	execute_close(Channels, Reply),
	self;

    Schedule ? close(Channels),
    known(Channels),
    arg(SPI_CLOSE, SpiOffsets, SpiOffset),
    SpiOffset =\= unbound :
      execute(SpiOffset, {SPI_CLOSE, Channels, Reply}),
      STOPPED |
	self;

    Schedule ? close(Channels, Reply?^),
    known(Channels),
    arg(SPI_CLOSE, SpiOffsets, SpiOffset),
    SpiOffset =?= unbound :
      STOPPED |
	execute_close(Channels, Reply),
	self;

    Schedule ? close(Channels, Reply?^),
    known(Channels),
    arg(SPI_CLOSE, SpiOffsets, SpiOffset),
    SpiOffset =\= unbound :
      execute(SpiOffset, {SPI_CLOSE, Channels, Reply}),
      STOPPED |
	self;

    /* Set the time limit - maximum value for Now */
    Schedule ? cutoff(Cutoff'), Cutoff' >= 0,
    info(REALTIME, Start_real_time') :
      Cutoff = _,
      Start_real_time = _ |
	continue_waiting + (Reply = true);

    Schedule ? default_weighter(Weighter),
    arg(SPI_INDEX, SpiOffsets, SpiOffset),
    SpiOffset =\= unbound |
	reset_default_weighter(SpiOffset, Weighter, DefaultWeighter,
				DefaultWeighter'),
	self;

    /* Splice input filter. */
    Schedule ? input(Schedule'', Schedule'^) |
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

    Schedule ? ordinal(Ordinal', Ordinal^) |
	self;

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
    convert_to_real(0, Now') :
      Now = _,
      Record = [],
      Stream = Record'? |
	self;

    /* Start a transmission process. */

    Schedule ? Start, Start =?= start(PostId, OpList, Value, Chosen),
    known(OpList),
    arg(SPI_POST, SpiOffsets, SpiOffset),
    SpiOffset =?= unbound,
    tuple(PostId), arg(1, PostId, PName) :
      Record ! start(PName),
      Debug ! start(PostId) |
	execute_post,
	continue_waiting;

    Schedule ? Start, Start =?= start(PName, OpList, Value, Chosen),
    known(OpList),
    arg(SPI_POST, SpiOffsets, SpiOffset),
    SpiOffset =?= unbound,
    string(PName) :
      Record ! start(PName),
      Debug ! start(PName) |
	execute_post + (PostId = {PName}),
	continue_waiting;

    Schedule ? Start, Start =?= start(PId, OpList, Value, Chosen, Prefix),
    known(OpList),
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
	execute_post,
	continue_waiting;

    Schedule ? Start, Start =?= start({PName}, OpList, Value, Chosen, Prefix),
    known(OpList),
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
	execute_post,
	continue_waiting;

    Schedule ? Start, Start =?= start(PId, OpList, Value, Chosen),
    known(OpList),
    arg(SPI_POST, SpiOffsets, SpiOffset),
    SpiOffset =\= unbound,
    tuple(PId), arg(1, PId, PName),
    string(PName) :
      execute(SpiOffset, {SPI_POST, PId, OpList, Value, Chosen, Reply}),
      Record ! start(PName),
      Debug ! start(PId) |
	continue_waiting;

    Schedule ? Start, Start =?= start(PName, OpList, Value, Chosen),
    known(OpList),
    arg(SPI_POST, SpiOffsets, SpiOffset),
    SpiOffset =\= unbound,
    string(PName) :
      execute(SpiOffset, {SPI_POST, {PName}, OpList, Value, Chosen, Reply}),
      Record ! start(PName),
      Debug ! start(PName) |
	continue_waiting;

    Schedule ? Start, Start =?= start(PId, OpList, Value, Chosen, Prefix),
    known(OpList),
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
    known(OpList),
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

/**************************** Debugging aids ********************************/

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

    Schedule ? diagnostic(Diagnostic) :
      Debug ! Diagnostic |
	screen#display(("Application Diagnostic" -> Diagnostic)),
	self;

    Schedule ? spifunctions(List),
    arity(SpiOffsets, Arity),
    make_tuple(Arity, SpiOffsets'),
    arg(SPI_CLOSE, SpiOffsets', Close),
    arg(SPI_POST, SpiOffsets', Post),
    arg(SPI_STEP, SpiOffsets', Step),
    arg(SPI_INDEX, SpiOffsets', Index),
    arg(SPI_RATE, SpiOffsets', Rate) :
      SpiOffsets = _,
      Close = Close'?,
      Post = Post'?,
      Step = Step'?,
      Index = Index'?,
      Rate = Rate'? |
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

    Schedule ? state(DefaultWeighter^, SpiOffsets^, Ordinal^) |
	self;		

    Schedule ? status(Status) :
      Status = STATUS |
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

    Wakeup =?= done,
    arg(SPI_STEP, SpiOffsets, SpiOffset),
    SpiOffset =?= unbound,
    arg(SPI_RATE, SpiOffsets, RateOffset) :
      Waiting = _,
      Waiting' = false |
	sum_weights(BasedAnchor, RateOffset, 0, Total),
	logix_total(MathOffset, BasedAnchor, Now, Total,
		    RateOffset, Now', Wakeup'),
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
      Record = [Now, end(PName1(RCId1, SENT_ARROW, CNId1)),
		     end(PName2(RCId2, RECEIVED_ARROW, CNId2)) | Record'?],
      Debug ! done(Now, PId1(RCId1, CH1), PId2(RCId2, CH2)) |
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
      Record = [Now, end(PrefixedName1(RCId1, SENT_ARROW, CNId1)),
		     end(PrefixedName2(RCId2, RECEIVED_ARROW, CNId2)) | Record'?],
      Debug ! done(Now, PId1(RCId1, CH1), PId2(RCId2, CH2)) |
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
      InstantaneousAnchor = _,
      MathOffset = _,
      Schedule = _,
      Scheduler = _,
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
  wait_done(Schedule, DefaultWeighter, SpiOffsets, Ordinal, Done) :-

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

    Schedule ? diagnostic(_) |
	self;

    Schedule ? Other,
    Other =\= close(_, _),
    Other =\= new_channel(_, _, _),
    Other =\= new_channel(_, _, _, _) |
	self;

    unknown(Schedule),
    known(Done) |
	self#reset(DefaultWeighter, SpiOffsets, Ordinal).

continue_waiting(Schedule, MathOffset, Ordinal, SpiOffsets, Waiter,
		Scheduler, Record, Debug,
		Now, Waiting, Wakeup,
		DefaultWeighter, Cutoff, Start_real_time,
		BasedAnchor, InstantaneousAnchor,
		Reply) :-

    /* Reply for spifcp transmission */
    Reply =?= true(PId1, RCId1, CH1, PId2, RCId2, CH2),
    PId1 =\= (_ : _), PId2 =\= (_ : _),
    tuple(PId1), arg(1, PId1, PName1),
    tuple(PId2), arg(1, PId2, PName2),
    read_vector(SPI_CHANNEL_NAME, CH1, CNId1),
    read_vector(SPI_CHANNEL_NAME, CH2, CNId2) :
      Record = [Now, end(PName1(RCId1, SENT_ARROW, CNId1)),
		     end(PName2(RCId2, RECEIVED_ARROW, CNId2)) | Record'?],
      Debug ! done(Now, PId1(RCId1, CH1), PId2(RCId2, CH2)) |
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
      Record = [Now, end(PrefixedName1(RCId1, SENT_ARROW, CNId1)),
		     end(PrefixedName2(RCId2, RECEIVED_ARROW, CNId2))
	       | Record'?],
      Debug ! done(Now, PId1(RCId1, CH1), PId2(RCId2, CH2)) |
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
    Input =?= status(Status) :
      Status ! pausing,
      ResetSchedule ! status(Status') |
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

spifunctions(List, SpiOffset, Close, Post, Step, Index, Rate) :-

    List ? CHAR_c :
      Close = SpiOffset? |
	self;

    List ? CHAR_p :
      Post = SpiOffset? |
	self;

    List ? CHAR_s :
      Step = SpiOffset? |
	self;

    List ? CHAR_i :
      Index = SpiOffset? |
	self;

    List ? CHAR_r :
      Rate = SpiOffset? |
	self;

    List ? Other,
    otherwise,
    list_to_string([CHAR_DOUBLE_QUOTE,Other,CHAR_DOUBLE_QUOTE], String) |
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
	unify_without_failure(unbound, Index),
	unify_without_failure(unbound, Rate);

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
	complete_weighter_tuple(Result, ComputeWeight, WeighterIndex,
				WeighterTuple, Result'),
	based_or_instantaneous;

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
      Scheduler = _,
      SpiOffsets = _,
      Reply = error(new_channel(ChannelName, Channel, BaseRate, ComputeWeight)).
  
  based_or_instantaneous(ChannelName, BaseRate, Result, Scheduler, Reply,
			 Channel, BasedAnchor, InstantaneousAnchor) :-

    Result =?= true,
    BaseRate =?= infinite :
      BasedAnchor = _,
      store_vector(SPI_CHANNEL_TYPE, SPI_INSTANTANEOUS, Channel),
      Reply = Result,
      DEBUG((ChannelName: instantaneous)) |
	queue_channel(Channel, InstantaneousAnchor);

    Result =?= true,
    convert_to_real(BaseRate, RealRate),
    RealRate =< 0 :
      BasedAnchor = _,
      InstantaneousAnchor = _,
      store_vector(SPI_CHANNEL_TYPE, SPI_SINK, Channel),
      Reply = true,
      DEBUG((ChannelName: sink(unnatural(BaseRate))));

    Result =?= true,
    convert_to_real(BaseRate, RealRate),
    RealRate > 0 :
      InstantaneousAnchor = _,
      store_vector(SPI_CHANNEL_RATE, RealRate, Channel),
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
      Reply = "invalid base rate"(ChannelName - BaseRate),
      DEBUG((ChannelName: sink(invalid(BaseRate))));

    Result =\= true :
      BasedAnchor = _,
      BaseRate = _,
      Channel = _,
      ChannelName = _,
      InstantaneousAnchor = _,
      Scheduler = _,
      store_vector(SPI_CHANNEL_TYPE, SPI_SINK, Channel),
      Reply = Result,
      DEBUG((ChannelName : sink(Result))).

  complete_weighter_tuple(Reply, ComputeWeight, WeighterIndex,
			  WeighterTuple, Reply1) :-

    Reply = true,
    arg(2, ComputeWeight, Index) :
      Index = WeighterIndex,
      WeighterTuple = ComputeWeight,
      Reply1 = Reply;

    Reply = true,
    otherwise :
      WeighterIndex = _,
      WeighterTuple = SPI_DEFAULT_WEIGHT_NAME(SPI_DEFAULT_WEIGHT_INDEX),
      Reply1 = Reply |
	fail(invalid_weighter_index(ComputeWeight));

    Reply =\= true :
      ComputeWeight = _,
      WeighterIndex = _,
      WeighterTuple = SPI_DEFAULT_WEIGHT_NAME(SPI_DEFAULT_WEIGHT_INDEX),
      Reply1 = Reply.


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

execute_post(MathOffset, PostId, OpList, Value, Chosen, Reply) :-

    true :
      Common = {PostId, Messages, Value, Chosen},
      Messages = AddMessages? |
	post_pass1,
	post_pass2.

execute_close(Channels, Reply) :-

    tuple(Channels),
    N := arity(Channels) |
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
    Refs' < 0 :
      Reply = "Error - Problem in ReferenceCount"(Channel),
      Reply' = _ |
	self;

    N =< 0 :
      Channels  = _,
      Reply = [];

    otherwise :
      Reply = close_failed(N, Channels).


/************************ post procedures ********************************/

post_pass1(MathOffset, OpList, Common, Ok) :-

    OpList ? Operation,
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    integer(Multiplier), Multiplier > 0,
    arg(SPI_MS_CHANNEL, Operation, Channel),
    read_vector(SPI_CHANNEL_TYPE, Channel, ChannelType),
    bitwise_and(ChannelType, SPI_TYPE_MASK, SPI_INSTANTANEOUS) |
	post_instantaneous;

    OpList ? Operation,
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    integer(Multiplier), Multiplier > 0,
    arg(SPI_MS_CHANNEL, Operation, Channel),
    read_vector(SPI_CHANNEL_TYPE, Channel, ChannelType),
    bitwise_and(ChannelType, SPI_TYPE_MASK, SPI_BIMOLECULAR) :
      store_vector(SPI_BLOCKED, FALSE, Channel) |
	self;

    OpList ? Operation,
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    integer(Multiplier), Multiplier > 0,
    arg(SPI_MS_CHANNEL, Operation, Channel),
    read_vector(SPI_CHANNEL_TYPE, Channel, ChannelType),
    bitwise_and(ChannelType, SPI_TYPE_MASK, SPI_HOMODIMERIZED) :
      store_vector(SPI_BLOCKED, FALSE, Channel) |
	self;

    OpList ? Operation,
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    integer(Multiplier), Multiplier > 0,
    arg(SPI_MS_CHANNEL, Operation, Channel),
    read_vector(SPI_CHANNEL_TYPE, Channel, ChannelType),
    bitwise_and(ChannelType, SPI_TYPE_MASK, SPI_UNKNOWN),
    arg(SPI_MS_TYPE, Operation, MessageType),
    read_vector(SPI_WEIGHT_TUPLE, Channel, WeightTuple),
    arg(2, WeightTuple, Index),
    bitwise_and(ChannelType, SPI_RANDOM_FLAG, RandomFlag) :
      store_vector(SPI_CHANNEL_TYPE, FinalType, Channel) |
	update_channel_type,
	self;

    OpList =?= [] :
      Common = _,
      MathOffset = _,
      Ok = true;

    otherwise |
	post_pass1_skip.

  update_channel_type(Index, MessageType, RandomFlag, FinalType) :-

    Index =?= SPI_DEFAULT_WEIGHT_INDEX,
    MessageType =\= SPI_DIMER,
    bitwise_or(SPI_BIMOLECULAR, RandomFlag, ChannelType) :
      FinalType = ChannelType;

    Index =?= SPI_DEFAULT_WEIGHT_INDEX,
    MessageType =?= SPI_DIMER,
    bitwise_or(SPI_HOMODIMERIZED, RandomFlag, ChannelType) :
      FinalType = ChannelType;

    Index =\= SPI_DEFAULT_WEIGHT_INDEX,
    MessageType =\= SPI_DIMER,
    bitwise_or(SPI_BIMOLECULAR_PRIME, RandomFlag, ChannelType) :
      FinalType = ChannelType;

    Index =\= SPI_DEFAULT_WEIGHT_INDEX,
    MessageType =?= SPI_DIMER,
    bitwise_or(SPI_HOMODIMERIZED_PRIME, RandomFlag, ChannelType) :
      FinalType = ChannelType.

  post_pass1_skip(MathOffset, OpList, Common, Ok) :-

    OpList ? Operation,
    arg(SPI_MS_MULTIPLIER, Operation, Multiplier),
    integer(Multiplier), Multiplier =< 0 |
        post_pass1;

    OpList ? Operation,
    arg(SPI_MS_CHANNEL, Operation, Channel),
    read_vector(SPI_CHANNEL_TYPE, Channel, ChannelType),
    bitwise_and(ChannelType, SPI_TYPE_MASK, SPI_SINK) |
	post_pass1;

    otherwise :
      Common = _,
      MathOffset = _,
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

post_instantaneous(MathOffset, OpList, Common, Ok,
		   Operation, Channel, ChannelType) :-

    arity(Operation) =:= SPI_MS_SIZE,
    bitwise_and(ChannelType, SPI_RANDOM_FLAG, 0),
    arg(SPI_MS_TYPE, Operation, SPI_SEND),
    read_vector(SPI_RECEIVE_ANCHOR, Channel, EndR),
    arg(SPI_MESSAGE_LINKS, EndR, MessageLinks),
    read_vector(SPI_NEXT_MS, MessageLinks, Message) |
	do_instantaneous_transmit + (MTX = SPI_RECEIVE_TAG, Ambient = []);

    arity(Operation) =:= SPI_MS_SIZE,
    bitwise_and(ChannelType, SPI_RANDOM_FLAG, 0),
    arg(SPI_MS_TYPE, Operation, SPI_RECEIVE),
    read_vector(SPI_SEND_ANCHOR, Channel, EndR),
    arg(SPI_MESSAGE_LINKS, EndR, MessageLinks),
    read_vector(SPI_NEXT_MS, MessageLinks, Message) |
	do_instantaneous_transmit + (MTX = SPI_SEND_TAG, Ambient = []);

    arity(Operation) =:= SPI_MS_SIZE,
    bitwise_and(ChannelType, SPI_RANDOM_FLAG, SPI_RANDOM_FLAG),
    arg(SPI_MS_TYPE, Operation, SPI_SEND),
    read_vector(SPI_RECEIVE_ANCHOR, Channel, Anchor),
    read_vector(SPI_RECEIVE_WEIGHT, Channel, Weight) :
      execute(MathOffset, {RAN, 0, Uniform}) |
	post_instantaneous_random + (MTX = SPI_RECEIVE_TAG, Ambient = []);

    arity(Operation) =:= SPI_MS_SIZE,
    bitwise_and(ChannelType, SPI_RANDOM_FLAG, SPI_RANDOM_FLAG),
    arg(SPI_MS_TYPE, Operation, SPI_RECEIVE),
    read_vector(SPI_SEND_ANCHOR, Channel, Anchor),
    read_vector(SPI_SEND_WEIGHT, Channel, Weight) :
      execute(MathOffset, {RAN, 0, Uniform}) |
	post_instantaneous_random + (MTX = SPI_SEND_TAG, Ambient = []);

    arity(Operation) =:= SPI_AMBIENT_MS_SIZE,
    arg(SPI_MS_AMBIENT, Operation, Ambient),
    bitwise_and(ChannelType, SPI_RANDOM_FLAG, 0),
    arg(SPI_MS_TYPE, Operation, SPI_SEND),
    read_vector(SPI_RECEIVE_ANCHOR, Channel, EndR),
    arg(SPI_MESSAGE_LINKS, EndR, MessageLinks),
    read_vector(SPI_NEXT_MS, MessageLinks, Message) |
	do_instantaneous_transmit + (MTX = SPI_RECEIVE_TAG);

    arity(Operation) =:= SPI_AMBIENT_MS_SIZE,
    arg(SPI_MS_AMBIENT, Operation, Ambient),
    bitwise_and(ChannelType, SPI_RANDOM_FLAG, 0),
    arg(SPI_MS_TYPE, Operation, SPI_RECEIVE),
    read_vector(SPI_SEND_ANCHOR, Channel, EndR),
    arg(SPI_MESSAGE_LINKS, EndR, MessageLinks),
    read_vector(SPI_NEXT_MS, MessageLinks, Message) |
	do_instantaneous_transmit + (MTX = SPI_SEND_TAG);

    arity(Operation) =:= SPI_AMBIENT_MS_SIZE,
    arg(SPI_MS_AMBIENT, Operation, Ambient),
    bitwise_and(ChannelType, SPI_RANDOM_FLAG, SPI_RANDOM_FLAG),
    arg(SPI_MS_TYPE, Operation, SPI_SEND),
    read_vector(SPI_RECEIVE_ANCHOR, Channel, Anchor),
    read_vector(SPI_RECEIVE_WEIGHT, Channel, Weight) :
      execute(MathOffset, {RAN, 0, Uniform}) |
	post_instantaneous_random + (MTX = SPI_RECEIVE_TAG);

    arity(Operation) =:= SPI_AMBIENT_MS_SIZE,
    arg(SPI_MS_AMBIENT, Operation, Ambient),
    bitwise_and(ChannelType, SPI_RANDOM_FLAG, SPI_RANDOM_FLAG),
    arg(SPI_MS_TYPE, Operation, SPI_RECEIVE),
    read_vector(SPI_SEND_ANCHOR, Channel, Anchor),
    read_vector(SPI_SEND_WEIGHT, Channel, Weight) :
      execute(MathOffset, {RAN, 0, Uniform}) |
	post_instantaneous_random + (MTX = SPI_SEND_TAG);

    otherwise :
      Channel = _,
      ChannelType = _,
      Common = _,
      MathOffset = _,
      OpList = _,
      Ok = invalid_transmission - Operation.

  post_instantaneous_random(MathOffset, OpList, Common, Ok,
			   Operation, MTX, Ambient, Anchor, Uniform, Weight) :-

    Select := Uniform*Weight |
	choose_random_start(Anchor, Select, Message),
	do_instantaneous_transmit + (EndR = Message).


do_instantaneous_transmit(MathOffset, OpList, Common, Ok,
			  Operation, MTX, Ambient, EndR, Message) :-

    arg(SPI_MS_CID, Message, RCIdQ),
    arg(SPI_MS_CHANNEL, Message, ChannelQ),
    arg(SPI_COMMON, Message, CommonQ),
    arg(MTX, Message, MessageTag),
    CommonQ = {PIdQ, MsList, ValueQ, ChosenQ},
    Common = {PIdR, _MsList, ValueR, ChosenR},
    arg(SPI_MS_CID, Operation, RCIdR),
    arg(SPI_MS_CHANNEL, Operation, ChannelR),
    arg(SPI_MS_TAGS, Operation, OperationTag),
    Ambient =?= [] :
      EndR = _,
      MathOffset = _,
      OpList = _,
      ValueQ = ValueR,
      ChosenQ = MessageTag,
      ChosenR = OperationTag,
      Ok = true(PIdQ, RCIdQ, ChannelQ, PIdR, RCIdR, ChannelR) |
	discount(MsList);

    arg(SPI_MS_CID, Message, RCIdQ),
    arg(SPI_MS_CHANNEL, Message, ChannelQ),
    arg(SPI_COMMON, Message, CommonQ),
    arg(MTX, Message, MessageTag),
    CommonQ = {PIdQ, MsList, ValueQ, ChosenQ},
    Common = {PIdR, _MsList, ValueR, ChosenR},
    arg(SPI_MS_CID, Operation, RCIdR),
    arg(SPI_MS_CHANNEL, Operation, ChannelR),
    arg(SPI_MS_TAGS, Operation, OperationTag),
    Ambient =\= [],
    arg(SPI_AMBIENT_CHANNEL, Message, AmbientQ),
    Ambient =\= AmbientQ :
      EndR = _,
      MathOffset = _,
      OpList = _,
      ValueQ = ValueR,
      ChosenQ = MessageTag,
      ChosenR = OperationTag,
      Ok = true(PIdQ, RCIdQ, ChannelQ, PIdR, RCIdR, ChannelR) |
	discount(MsList);

    Ambient =\= [],
    arg(SPI_AMBIENT_CHANNEL, Message, AmbientQ),
    Ambient =?= AmbientQ,
    arg(SPI_MESSAGE_LINKS, Message, MessageLinks),
    read_vector(SPI_NEXT_MS, MessageLinks, Message'),
    Message' =\= EndR |
	self;

    Ambient =\= [],
    arg(SPI_AMBIENT_CHANNEL, Message, AmbientQ),
    Ambient =?= AmbientQ,
    arg(SPI_MESSAGE_LINKS, Message, MessageLinks),
    read_vector(SPI_NEXT_MS, MessageLinks, Message'),
    Message' =?= EndR :
      Operation = _,
      MTX = _ |
	post_pass1;

    arg(SPI_COMMON, Message, CommonQ),
    arg(SPI_OP_CHOSEN, CommonQ, Chosen), not_we(Chosen),
    arg(SPI_MESSAGE_LINKS, Message, MessageLinks),
    read_vector(SPI_NEXT_MS, MessageLinks, Message'),
    Message' =\= EndR |
	self;

    arg(SPI_MS_TYPE, Message, SPI_MESSAGE_ANCHOR),
    arg(SPI_MESSAGE_LINKS, Message, MessageLinks),
    read_vector(SPI_NEXT_MS, MessageLinks, Message'),
    Message' =\= EndR |
	self;

    arg(SPI_MS_TYPE, Message, SPI_MESSAGE_ANCHOR),
    arg(SPI_MESSAGE_LINKS, Message, MessageLinks),
    read_vector(SPI_NEXT_MS, MessageLinks, Message'),
    Message' =?= EndR :
      Ambient = _,
      Operation = _,
      MTX = _ |
	post_pass1;

    % This can happen (to the monitor). 
    Common = {_, _, _, Chosen}, not_we(Chosen) :
      Ambient = _,
      EndR = _,
      Operation = _,
      Message = _,
      MTX = _ |
	post_pass1;

    % This Message has the same Chosen as the Operation -
    % mixed communications which ARE NOT homodimerized.
    arg(SPI_COMMON, Message, CommonQ),
    CommonQ = {_, _, _, Chosen},
    Common = {_, _, _, Chosen},
    arg(SPI_MESSAGE_LINKS, Message, MessageLinks),
    read_vector(SPI_NEXT_MS, MessageLinks, Message'),
    Message' =\= EndR |
	self;

    arg(SPI_COMMON, Message, CommonQ),
    CommonQ = {_, _, _, Chosen},
    Common = {_, _, _, Chosen},
    arg(SPI_MESSAGE_LINKS, Message, MessageLinks),
    read_vector(SPI_NEXT_MS, MessageLinks, Message'),
    Message' =?= EndR :
      Ambient = _,
      Operation = _,
      MTX = _ |
	post_pass1.


/*************************** step procedures *********************************/

sum_weights(Channel, RateOffset, Sum, Total) :-

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    vector(Channel'),
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    Type =?= SPI_CHANNEL_ANCHOR :
      RateOffset = _,
      Total = Sum;

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    vector(Channel'),
    read_vector(SPI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    bitwise_and(Type, SPI_PRIME_MASK, SPI_BIMOLECULAR),
    read_vector(SPI_CHANNEL_RATE, Channel', Rate),
    read_vector(SPI_SEND_WEIGHT, Channel', SendWeight),
    read_vector(SPI_RECEIVE_WEIGHT, Channel', ReceiveWeight),
    Sum += Rate*SendWeight*ReceiveWeight |
	self;    

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    vector(Channel'),
    read_vector(SPI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    bitwise_and(Type, SPI_PRIME_MASK, SPI_HOMODIMERIZED),
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
    vector(Channel'),
    read_vector(SPI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    bitwise_and(Type, SPI_PRIME_FLAG, SPI_PRIME_FLAG) :
      execute(RateOffset, {SPI_RATE, Channel', Addend, Reply}) |
	prime_weight_cumulate(Reply, Addend, Sum, Sum'),
	self;    

    otherwise,
    vector(Channel),
    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    vector(Channel') |
	self.

  prime_weight_cumulate(Reply, Addend, Sum, Cumulated) :-

    Reply =?= true,
    Sum  += Addend :
      Cumulated = Sum'.

logix_total(MathOffset, Anchor, Now, Total, RateOffset, NewNow, Wakeup) :-

    Total =< 0 :
      Anchor = _,
      MathOffset = _,
      RateOffset = _,
      NewNow = Now,
      Wakeup = true;

    Total > 0 :
      execute(MathOffset, {RAN, 0, Uniform1}),
      execute(MathOffset, {RAN, 0, Uniform2}),
      execute(MathOffset, {LN, Uniform2, NegativeExponential}) |
	logix_transmit.

  logix_transmit(RateOffset, Anchor, Now, Total,
		 Uniform1, Uniform2, NegativeExponential,
		 NewNow, Wakeup) :-
    Residue := Uniform1*Total,
    NewNow^ := Now - NegativeExponential/Total |
	select_channel + (Channel = Anchor).


select_channel(RateOffset, Residue, Channel, Uniform1, Uniform2, Wakeup) :-

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(SPI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    bitwise_and(Type, SPI_PRIME_MASK, SPI_BIMOLECULAR),
    read_vector(SPI_CHANNEL_RATE, Channel', Rate),
    read_vector(SPI_SEND_WEIGHT, Channel', SendWeight),
    SendWeight > 0,
    read_vector(SPI_RECEIVE_WEIGHT, Channel', ReceiveWeight),
    ReceiveWeight > 0,
    Residue -= Rate*SendWeight*ReceiveWeight,
    Residue' =< 0 :
      RateOffset = _ |
	do_bimolecular_transmit;

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(SPI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    bitwise_and(Type, SPI_PRIME_MASK, SPI_BIMOLECULAR),
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
    bitwise_and(Type, SPI_PRIME_MASK, SPI_HOMODIMERIZED),
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
    Residue' =< 0 :
      RateOffset = _  |
	do_homodimerized_transmit;

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(SPI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    bitwise_and(Type, SPI_PRIME_MASK, SPI_HOMODIMERIZED),
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
    vector(Channel'),
    read_vector(SPI_BLOCKED, Channel', Blocked),
    Blocked =?= FALSE,
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    bitwise_and(Type, SPI_PRIME_FLAG, SPI_PRIME_FLAG),
    bitwise_and(Type, SPI_PRIME_MASK, Type') :
      execute(RateOffset, {SPI_RATE, Channel', Subtrahend, Reply}) |
	prime_weight_decrement;    

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    otherwise |
	self.

  prime_weight_decrement(RateOffset, Residue, Channel, Uniform1, Uniform2,
			Wakeup, Type, Subtrahend, Reply) :-

    Reply = true,
    Subtrahend =< 0 :
      Type = _ |
	select_channel;

    Reply = true,
    Subtrahend > 0,
    Residue -= Subtrahend,
    Residue' > 0 :
      Type = _ |
	select_channel;

    Reply = true,
    Subtrahend > 0,
    Residue -= Subtrahend,
    Residue' =< 0,
    Type =?= SPI_BIMOLECULAR_PRIME :
      RateOffset = _ |
	do_bimolecular_transmit;

    Reply = true,
    Subtrahend > 0,
    Residue -= Subtrahend,
    Residue' =< 0,
    Type =?= SPI_HOMODIMERIZED_PRIME :
      RateOffset = _ |
	do_homodimerized_transmit.

/****** based transmit - complete a transmission for a pair of messages ******/

do_bimolecular_transmit(Channel, Uniform1, Uniform2, Wakeup) :-

    read_vector(SPI_SEND_ANCHOR, Channel, SendAnchor),
    read_vector(SPI_RECEIVE_ANCHOR, Channel, ReceiveAnchor),
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    bitwise_and(Type, SPI_RANDOM_FLAG, 0),
    arg(SPI_MESSAGE_LINKS, SendAnchor, SendLinks),
    read_vector(SPI_NEXT_MS, SendLinks, Send),
    arg(SPI_MESSAGE_LINKS, ReceiveAnchor, ReceiveLinks),
    read_vector(SPI_NEXT_MS, ReceiveLinks, Receive) :
      Uniform1 = _,
      Uniform2 = _ |
	do_bimolecular_send(Channel, Wakeup, Send, Send, Receive, Receive);

    read_vector(SPI_SEND_ANCHOR, Channel, SendAnchor),
    read_vector(SPI_RECEIVE_ANCHOR, Channel, ReceiveAnchor),
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    bitwise_and(Type, SPI_RANDOM_FLAG, SPI_RANDOM_FLAG),
    read_vector(SPI_SEND_WEIGHT, Channel, SendWeight),
    SendSelect := Uniform1*SendWeight,
    read_vector(SPI_RECEIVE_WEIGHT, Channel, ReceiveWeight),
    ReceiveSelect := Uniform2*ReceiveWeight |
	choose_random_start(SendAnchor, SendSelect, StartS),
	choose_random_start(ReceiveAnchor, ReceiveSelect, StartR),
	do_bimolecular_send(Channel, Wakeup, StartS, StartS, StartR, StartR).

  do_bimolecular_send(Channel, Wakeup, Send, EndSend, Receive, EndReceive) :-

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
      EndReceive = _,
      EndSend = _,
      SendChosen = SendTag,
      ReceiveChosen = ReceiveTag,
      ReceiveValue = SendValue?,
      Wakeup = true(SendPId, SendRCId, SendChannel,
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
      EndReceive = _,
      EndSend = _,
      SendChosen = SendTag,
      ReceiveChosen = ReceiveTag,
      ReceiveValue = SendValue?,
      Wakeup = true(SendPId, SendRCId, SendChannel,
		    ReceivePId, ReceiveRCId, ReceiveChannel) |
	discount(SendList),
	discount(ReceiveList);

    arg(SPI_MS_TYPE, Send, SPI_SEND),
    arg(SPI_AMBIENT_CHANNEL, Send, SendAmbient),
    SendAmbient =\= [],
    arg(SPI_AMBIENT_CHANNEL, Receive, ReceiveAmbient),
    SendAmbient =?= ReceiveAmbient,
    arg(SPI_MESSAGE_LINKS, Send, SendLinks),
    read_vector(SPI_NEXT_MS, SendLinks, Send') |
	bimolecular_send_blocked;
	
    arg(SPI_MS_TYPE, Send, SPI_SEND),
    arg(SPI_COMMON, Send, SendCommon),
    arg(SPI_MESSAGE_LINKS, Send, SendLinks),
    arg(SPI_COMMON, Receive, ReceiveCommon),
    % This Send has the same Chosen as the Receive -
    % mixed communications which ARE NOT homodimerized.
    SendCommon =?= {_, _, _, Chosen},
    ReceiveCommon =?= {_, _, _, Chosen},
    read_vector(SPI_NEXT_MS, SendLinks, Send') |
	bimolecular_send_blocked;

    arg(SPI_MS_TYPE, Send, SPI_MESSAGE_ANCHOR),
    arg(SPI_MESSAGE_LINKS, Send, SendLinks),
    read_vector(SPI_NEXT_MS, SendLinks, Send'),
    Send' =\= EndSend |
	self;

    arg(SPI_MS_TYPE, Send, SPI_MESSAGE_ANCHOR),
    arg(SPI_MESSAGE_LINKS, Send, SendLinks),
    read_vector(SPI_NEXT_MS, SendLinks, Send'),
    Send' =?= EndSend, 
    arg(SPI_MESSAGE_LINKS, Receive, ReceiveLinks),
    read_vector(SPI_NEXT_MS, ReceiveLinks, Receive'),
    Receive' =\= EndReceive,
    arg(SPI_MS_TYPE, Receive', SPI_RECEIVE) |
	self;

    arg(SPI_MS_TYPE, Send, SPI_MESSAGE_ANCHOR),
    arg(SPI_MESSAGE_LINKS, Send, SendLinks),
    read_vector(SPI_NEXT_MS, SendLinks, Send'),
    Send' =?= EndSend,
    arg(SPI_MESSAGE_LINKS, Receive, ReceiveLinks),
    read_vector(SPI_NEXT_MS, ReceiveLinks, Receive'),
    Receive' =\= EndReceive,
    arg(SPI_MS_TYPE, Receive', SPI_MESSAGE_ANCHOR),
    arg(SPI_MESSAGE_LINKS, Receive', ReceiveLinks'),
    read_vector(SPI_NEXT_MS, ReceiveLinks', Receive''),
    Receive'' =\= EndReceive |
	self;

    arg(SPI_MS_TYPE, Send, SPI_MESSAGE_ANCHOR),
    arg(SPI_MESSAGE_LINKS, Send, SendLinks),
    read_vector(SPI_NEXT_MS, SendLinks, Send'),
    Send' =?= EndSend,
    arg(SPI_MESSAGE_LINKS, Receive, ReceiveLinks),
    read_vector(SPI_NEXT_MS, ReceiveLinks, Receive'),
    Receive' =?= EndReceive |
      store_vector(SPI_BLOCKED, TRUE, Channel),
      Wakeup = done;

    arg(SPI_MS_TYPE, Send, SPI_MESSAGE_ANCHOR),
    arg(SPI_MESSAGE_LINKS, Send, SendLinks),
    read_vector(SPI_NEXT_MS, SendLinks, Send'),
    Send' =?= EndSend,
    arg(SPI_MESSAGE_LINKS, Receive, ReceiveLinks),
    read_vector(SPI_NEXT_MS, ReceiveLinks, Receive'),
    Receive' =\= EndReceive,
    arg(SPI_MS_TYPE, Receive', SPI_RECEIVE) |
	self;

    arg(SPI_MS_TYPE, Send, SPI_MESSAGE_ANCHOR),
    arg(SPI_MESSAGE_LINKS, Send, SendLinks),
    read_vector(SPI_NEXT_MS, SendLinks, Send'),
    Send' =?= EndSend,
    arg(SPI_MESSAGE_LINKS, Receive, ReceiveLinks),
    read_vector(SPI_NEXT_MS, ReceiveLinks, Receive'),
    Receive' =\= EndReceive,
    arg(SPI_MS_TYPE, Receive', SPI_MESSAGE_ANCHOR),
    arg(SPI_MESSAGE_LINKS, Receive', ReceiveLinks'),
    read_vector(SPI_NEXT_MS, ReceiveLinks', Receive''),
    Receive'' =\= EndReceive |
	self;

    arg(SPI_MS_TYPE, Send, SPI_MESSAGE_ANCHOR),
    arg(SPI_MESSAGE_LINKS, Send, SendLinks),
    read_vector(SPI_NEXT_MS, SendLinks, Send'),
    Send' =?= EndSend,
    arg(SPI_MESSAGE_LINKS, Receive, ReceiveLinks),
    read_vector(SPI_NEXT_MS, ReceiveLinks, Receive'),
    Receive' =\= EndReceive,
    arg(SPI_MS_TYPE, Receive', SPI_MESSAGE_ANCHOR),
    arg(SPI_MESSAGE_LINKS, Receive', ReceiveLinks'),
    read_vector(SPI_NEXT_MS, ReceiveLinks', Receive''),
    Receive'' =?= EndReceive |
      store_vector(SPI_BLOCKED, TRUE, Channel),
      Wakeup = done

/*
otherwise |
  spi_utils#show_value(channel = Channel,[],C),
  spi_utils#show_value(send = Send, [3], S),
  spi_utils#show_value(receive = Receive, [3], R),
  spi_utils#show_value(endsend = EndSend, [3], ES),
  spi_utils#show_value(endreceive = EndReceive, [3], ER),
  screen#display((C,S,R,ES,ER),length(0))
*/
.

  bimolecular_send_blocked(Channel, Wakeup, Send, EndSend, Receive, EndReceive) :-
    Send =\= EndSend |
	do_bimolecular_send;

    Send =?= EndSend,
    arg(SPI_MESSAGE_LINKS, Receive, ReceiveLinks),
    read_vector(SPI_NEXT_MS, ReceiveLinks, Receive'),
    Receive' =\= EndReceive,
    arg(SPI_MS_TYPE, Receive', SPI_RECEIVE) |
	do_bimolecular_send;

    Send =?= EndSend,
    arg(SPI_MESSAGE_LINKS, Receive, ReceiveLinks),
    read_vector(SPI_NEXT_MS, ReceiveLinks, Receive'),
    Receive' =\= EndReceive,
    arg(SPI_MS_TYPE, Receive', SPI_MESSAGE_ANCHOR),
    arg(SPI_MESSAGE_LINKS, Receive', ReceiveLinks'),
    read_vector(SPI_NEXT_MS, ReceiveLinks', Receive''),
    Receive'' =\= EndReceive |
	do_bimolecular_send;

    otherwise :
      Receive = _,
      Send = _,
      EndReceive = _,
      EndSend = _,
      store_vector(SPI_BLOCKED, TRUE, Channel),
      Wakeup = done.
      
    
do_homodimerized_transmit(Channel, Uniform1, Uniform2, Wakeup) :-

    read_vector(SPI_DIMER_ANCHOR, Channel, DimerAnchor),
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    bitwise_and(Type, SPI_RANDOM_FLAG, 0),
    arg(SPI_MESSAGE_LINKS, DimerAnchor, DimerLinks),
    read_vector(SPI_NEXT_MS, DimerLinks, Receive),
    arg(SPI_MESSAGE_LINKS, Receive, ReceiveLinks),
    read_vector(SPI_NEXT_MS, ReceiveLinks, Dimer) :
      Uniform1 = _,
      Uniform2 = _ |
	do_homodimerized_send(Channel, Wakeup, Receive, Dimer, Dimer);

    read_vector(SPI_DIMER_ANCHOR, Channel, DimerAnchor),
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    bitwise_and(Type, SPI_RANDOM_FLAG, SPI_RANDOM_FLAG),
    read_vector(SPI_DIMER_WEIGHT, Channel, DimerWeight),
    ReceiveSelect := Uniform1*DimerWeight,
    DimerSelect := Uniform2*DimerWeight |
	choose_random_start(DimerAnchor, ReceiveSelect, Receive),
	choose_random_start(DimerAnchor, DimerSelect, StartD),
	do_homodimerized_send(Channel, Wakeup, Receive, StartD, StartD).

  do_homodimerized_send(Channel, Wakeup, Receive, Dimer, EndDimer) :-

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
      EndDimer = _,
      ReceiveChosen = ReceiveTag,
      DimerChosen = DimerTag,
      ReceiveValue = DimerValue?,
      Wakeup = true(ReceivePId, ReceiveRCId, ReceiveChannel,
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
      EndDimer = _,
      ReceiveChosen = ReceiveTag,
      DimerChosen = DimerTag,
      ReceiveValue = DimerValue?,
      Wakeup = true(ReceivePId, ReceiveRCId, ReceiveChannel,
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
    read_vector(SPI_NEXT_MS, DimerLinks, Dimer'),
    Dimer' =\= EndDimer |
	self;

    arg(SPI_MS_TYPE, Receive, SPI_DIMER),
    arg(SPI_MS_TYPE, Dimer, SPI_DIMER),
    arg(SPI_AMBIENT_CHANNEL, Receive, ReceiveAmbient),
    ReceiveAmbient =\= [],
    arg(SPI_AMBIENT_CHANNEL, Dimer, DimerAmbient),
    ReceiveAmbient =?= DimerAmbient,
    arg(SPI_MESSAGE_LINKS, Dimer, DimerLinks),
    read_vector(SPI_NEXT_MS, DimerLinks, Dimer'),
    Dimer' =?= EndDimer :
      store_vector(SPI_BLOCKED, TRUE, Channel),
      Wakeup = done;

    % Test Receive has the same Chosen as Dimer -
    % communications which ARE homodimerized AND mixed.
    arg(SPI_MS_TYPE, Receive, SPI_DIMER),
    arg(SPI_MS_TYPE, Dimer, SPI_DIMER),
    arg(SPI_COMMON, Receive, ReceiveCommon),
    arg(SPI_COMMON, Dimer, DimerCommon),
    ReceiveCommon =?= {_, _, _, Chosen},
    DimerCommon =?= {_, _, _, Chosen},
    arg(SPI_MESSAGE_LINKS, Dimer, DimerLinks),
    read_vector(SPI_NEXT_MS, DimerLinks, Dimer'),
    Dimer' =\= EndDimer |
	self;

    arg(SPI_MS_TYPE, Receive, SPI_DIMER),
    arg(SPI_MS_TYPE, Dimer, SPI_DIMER),
    arg(SPI_COMMON, Receive, ReceiveCommon),
    arg(SPI_COMMON, Dimer, DimerCommon),
    ReceiveCommon =?= {_, _, _, Chosen},
    DimerCommon =?= {_, _, _, Chosen},
    arg(SPI_MESSAGE_LINKS, Dimer, DimerLinks),
    read_vector(SPI_NEXT_MS, DimerLinks, Dimer'),
    Dimer' =?= EndDimer :
      store_vector(SPI_BLOCKED, TRUE, Channel),
      Wakeup = done;

    % Skip the Anchor.
    arg(SPI_MS_TYPE, Receive, SPI_DIMER),
    arg(SPI_MS_TYPE, Dimer, SPI_MESSAGE_ANCHOR),
    arg(SPI_MESSAGE_LINKS, Dimer, DimerLinks),
    read_vector(SPI_NEXT_MS, DimerLinks, Dimer'),
    Dimer' =\= EndDimer |
	self;

    arg(SPI_MS_TYPE, Receive, SPI_DIMER),
    arg(SPI_MS_TYPE, Dimer, SPI_MESSAGE_ANCHOR),
    arg(SPI_MESSAGE_LINKS, Dimer, DimerLinks),
    read_vector(SPI_NEXT_MS, DimerLinks, Dimer'),
    Dimer' =?= EndDimer :
      store_vector(SPI_BLOCKED, TRUE, Channel),
      Wakeup = done
/*
;
otherwise |
  spi_utils#show_value(channel = Channel,[],C),
  spi_utils#show_value(receive = Receive, [3], R),
  spi_utils#show_value(dimer = Dimer, [3], D),
  spi_utils#show_value(enddimer = EndDimer, [3], ED),
  screen#display((C,R,D,ED),length(0))
*/
.


/***************************** Utilities ************************************/

choose_random_start(Message, Select, Start) :-

    arg(SPI_MESSAGE_LINKS, Message, Links),
    read_vector(SPI_NEXT_MS, Links, Message'),
    arg(SPI_MS_MULTIPLIER, Message', Multiplier),
    Select -= Multiplier,
    Select' > 0 |
	self;

    arg(SPI_MESSAGE_LINKS, Message, Links),
    read_vector(SPI_NEXT_MS, Links, Message'),
    arg(SPI_MS_MULTIPLIER, Message', Multiplier),
    Select -= Multiplier,
    Select' =< 0 :
      Start = Message'.


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


/*

do_execute(Id, Offset, Command) :-
    true : execute(Offset, Command) |
	spi_utils#show_value(do_execute(Id, Offset, Command),[5],O),
	screen#display(ok(O),wait(O));
    otherwise |
	spi_utils#show_value(do_execute(Id, Offset, Command),[5],O),
	screen#display(failed(O),wait(O)).
*/
