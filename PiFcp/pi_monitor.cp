-monitor(serve).
-language([evaluate,compound,colon]).
-export([get_global_channels/1, global_channels/1, global_channels/2,
	 options/2, reset/0, scheduler/1]).

SORT(Merger, List, Sorted) =>
	quick_sort_triplets(List, Sorted).

MERGE(Merger, Sorted, List1, List2, Pair) =>
	merge_triplets(Sorted, List1, [Pair | List2]).

RAN =>  4.					/* random real number */
LN  =>  9.					/* natural logarithm */

REALTIME => 12.

DASH => 45.

MAXINT => 3354431.

serve(In) :-

    In =?= [] |
	true;

    In =\= [] |
	server(In, 1, nil, [], [{0, _}, {[], _}], Scheduler),
	processor#link(lookup(math, Offset), Ok),
	start_scheduling(Scheduler, Offset, Ok).

server(In, UID, Tree, Options, Global, Scheduler) :-

    In ? unique_sender(String, Name),
    string_to_dlist(String, Head, [DASH | Tail]),
    convert_to_string(UID++, Suffix),
    string_to_dlist(Suffix, Tail, []),
    list_to_string(Head, Concatenated) :
      Name = Concatenated? |
	self;

    In ? global_channels(List) |
	merge_channels(List, Global, Global', ""),
	self;

    In ? global_channels(List, Scheduler^) |
	merge_channels(List, Global, Global', ""),
	self;

    In ? get_global_channels(List),
    we(List) :
      List = List'? |
	copy_global(Global, List', _),
	self;

    In ? reset :
      UID = _,
      Tree = _,
      Options = _,
      Global = _,
      close_channel(Scheduler) |
	serve;

    In ? options(New, Old) :
      Options' = New? |
	unify_without_failure(Options, Old),
	self;

    In ? scheduler(Scheduler^) |
	self;

    In ? Other,
    otherwise |
	self,
	fail(Other, unknown);

    In = [] :
      UID = _,
      Tree = _,
      Options = _,
      Global = _,
      close_channel(Scheduler).


merge_channels(List, Global, New_Global, Last) :-

    List = [] :
      Last = _,
      New_Global = Global;

    List ? Name(NewChannel, _ReceiveMean, _SendMean), string(Name),
    Last @< Name,
    we(NewChannel),
    Global ? Name(PiChannel) :
      NewChannel = PiChannel,
      New_Global ! Name(NewChannel),
      Last = _,
      Last' = Name |
	self;

    List = [Name(_, _, _)|_], string(Name),
    Global ? Entry,
    Entry = Last'(_GlobalChannel),
    Last' @< Name :
      Last = _,
      New_Global ! Entry |
	self;

    List ? Name(NewChannel, ReceiveMean, SendMean),
    we(NewChannel),
    Global = [Name1(_GlobalChannel) | _],
    string(Name),
    Last @< Name, Name @< Name1,
    string_to_dlist("global.", GL, GT),
    string_to_dlist(Name, NL, []) :
      NewChannel = PiChannel?,
      New_Global ! Name(NewChannel),
      GT = NL,
      Last' = Name |
	list_to_string(GL, Id),
	pi_channel(PiChannel, Id?, ReceiveMean, SendMean),
	self;

    otherwise :
      Last = _,
      New_Global = Global |
	fail(merge_channels(List)).


copy_global(Global, List, Ends) :-
    Global ? Head :
      Ends ! Head |
	copy_interior.

  copy_interior(Global, List, Ends) :-

    Global ? Entry,
    Global' =\= [] :
      List ! Entry |
	self;

    Global = [_] :
      List = [],
      Ends = Global.


start_scheduling(Scheduler, Offset, Ok) :-

    Ok =?= true,
    info(REALTIME, Start_real_time),
    convert_to_real(0, Zero) :
      make_channel(Scheduler, Schedule),
      execute(Offset, {RAN, 0, Uniform}),
      execute(Offset, {LN, Uniform, NegativeExponential}) |
	processor#Waiter?,
	scheduling(Schedule, Offset, NegativeExponential, Waiter,
		Zero, [], [], false, _Wakeup, _Recording, _Debug,
				MAXINT, Start_real_time);

    Ok =\= true :
      Scheduler = _,
      Offset = _ |
	fail(math_offset(Ok)).

/*
** scheduling monitors the stream generated using the scheduler channel.
**
** It recognises:
**
**    schedule(<scheduling_list>)
**    terminate(<token>, Now^)
**    record(Record?)
**    end_record(Record'?)
**
** and the debugging aids:
**
**    input(Schedule?)
**    debug(Debug?)
**
** Processing:
**
** Maintain time  Now  and sorted list of scheduling triplets,  TripletList ,
** and an unsorted auxiliary list,  NewList , of new scheduling triplets:
**
**	{WakeupTime, Vector, WaitVariable}
**
** a. For each schedule(List) item:
**
**	(1) WakeupTime := Now + exponential(Mean),
**	    where exponential(Mean) := Variate is computed by
**
**		execute(MathOffset, {RnadomExponential, Mean, Variate})
**
**	    Initially WakeUpTime is computed by:
**
**		execute(MathOffset, {Random, 0, UniformVariate}),
**		execute(MathOffset, {NaturalLog, UniformVariate, Variate}),
**		WakeupTime := Now - Mean*Variate,
**
**	(2) Add scheduling triplet to NewList (push onto front).
**
** b. Whenever system becomes idle and NewList or TripletList is non-empty:
**
**	(1) Sort NewList and merge with TripletList on WakeupTime,
**	    unifying WaitVariable for equal WaitTime (highly unlikely).
**
**	(2) Unify "true" with first WaitVariable and discard that triplet.
**
** c. Provide a record of:
**
**	WaitList <token>  % (End of WaitList may be any non-list token.)
**      Changes to  Now .
**      Termination <token>.
*/

scheduling(Schedule, Offset, NegativeExponential, Waiter,
	Now, TripletList, NewList, Waiting, Wakeup, Record, Debug,
			Cutoff, Start_real_time) :-


    Schedule ? schedule([receive(Channel, WakeVar) | More]),
    Channel = Id(Vector, {0, _Send}) :
      Debug ! receive(Id, 0),
      WakeVar = Vector,
      Schedule'' = [schedule(More) | Schedule'] |
	self;

    Schedule ? schedule([send(Channel, WakeVar) | More]),
    Channel = Id(Vector, {_Receive, 0}) :
      Debug ! send(Id, 0),
      WakeVar = Vector,
      Schedule'' = [schedule(More) | Schedule'] |
	self;

    Schedule ? schedule([receive(Channel, WakeVar) | More]),
    Channel = Id(Vector, {Mean, _Send}), Mean =\= 0,
    Waiting =\= true,
    WakeupTime := Now - Mean*NegativeExponential :
      Debug ! receive(Id, WakeupTime),
      Wakeup = _,
      Waiter ! machine(idle_wait(Wakeup'), _Ok),
      Waiting' = true,
      Schedule'' = [schedule(More) | Schedule'],
      NewList' = [{WakeupTime, Vector, WakeVar} | NewList],
      execute(Offset, {RAN, 0, Uniform}),
      execute(Offset, {LN, Uniform, NegativeExponential'}) |
	self;

    Schedule ? schedule([send(Channel, WakeVar) | More]),
    Channel = Id(Vector, {_Receive, Mean}), Mean =\= 0,
    Waiting =\= true,
    WakeupTime := Now - Mean*NegativeExponential :
      Debug ! send(Id, WakeupTime),
      Wakeup = _,
      Waiter ! machine(idle_wait(Wakeup'), _Ok),
      Waiting' = true,
      Schedule'' = [schedule(More) | Schedule'],
      NewList' = [{WakeupTime, Vector, WakeVar} | NewList],
      execute(Offset, {RAN, 0, Uniform}),
      execute(Offset, {LN, Uniform, NegativeExponential'}) |
	self;

    Schedule ? schedule([receive(Channel, WakeVar) | More]),
    Channel = Id(Vector, {Mean, _Send}), Mean =\= 0,
    Waiting =?= true,
    WakeupTime := Now - Mean*NegativeExponential :
      Debug ! receive(Id, WakeupTime),
      Schedule'' = [schedule(More) | Schedule'],
      NewList' = [{WakeupTime, Vector, WakeVar} | NewList],
      execute(Offset, {RAN, 0, Uniform}),
      execute(Offset, {LN, Uniform, NegativeExponential'}) |
	self;

    Schedule ? schedule([send(Channel, WakeVar) | More]),
    Channel = Id(Vector, {_Receive, Mean}), Mean =\= 0,
    Waiting =?= true,
    WakeupTime := Now - Mean*NegativeExponential :
      Debug ! send(Id, WakeupTime),
      Schedule'' = [schedule(More) | Schedule'],
      NewList' = [{WakeupTime, Vector, WakeVar} | NewList],
      execute(Offset, {RAN, 0, Uniform}),
      execute(Offset, {LN, Uniform, NegativeExponential'}) |
	self;

    Schedule ? schedule(Token), Token =\= [_|_] :
      Debug ! process(Token),
      Record ! Token |
	self;

    Schedule =?= [] :
      Cutoff = _,
      NegativeExponential = _,
      NewList = _,
      Now = _,
      Offset = _,
      TripletList = _,
      Start_real_time = _,
      Waiting = _,
      Wakeup = _,
      Waiter = [],
      Record = [],
      Debug = [];

    Schedule ? cutoff(Cutoff'), Cutoff' >= 0,
    info(REALTIME, Start_real_time') :
      Cutoff = _,
      Start_real_time = _ |
	self;

    Schedule ? record(Stream) :
      Stream = Record? |
	self;

    Schedule ? end_record(Stream),
    convert_to_real("0.0", Now') :
      Now = _,
      Record = [],
      Stream = Record'? |
	self;

    Schedule ? Terminate, Terminate = terminate(Token, When?) :
      When = Now,
      Record ! Token,
      Debug ! Terminate |
	self;

/**************************** Debuging code ********************************/

    Schedule ? input(Schedule'', Schedule'^) |
	self;

    Schedule ? debug(Stream) :
      Stream = Debug? |
	self;

    Schedule ? Other,
    otherwise |
	fail(Other),
	self;

/***************************************************************************/

    Now >= Cutoff,
    info(REALTIME, End_real_time),
    Real_time := End_real_time - Start_real_time  :
      Offset = _,
      NegativeExponential = _,
      NewList = _,
      TripletList = _,
      Schedule = _,
      Waiting = _,
      Wakeup = _,
      Waiter = [machine(idle_wait(Done), _Ok)],
      Record = [],
      Debug = [] |
	computation#display((done @ Now:
		seconds = Real_time)),
	wait_done;

    Wakeup =?= done,
    TripletList =?= [],
    NewList =?= [] :
      Debug ! idle(Now),
      Waiting = _,
      Waiting' = false,
      Wakeup' = _ |
	self;

    Wakeup =?= done,
    TripletList ? {Now', Vector, WakeVar}, TripletList' =?= [],
    NewList =?= [] :
      Debug ! time(Now'),
      Record ! Now',
      Now = _,
      Waiting = _,
      WakeVar = Vector,
      Waiting' = false |
	self;

    Wakeup =?= done,
    TripletList ? {Now', Vector, WakeVar}, TripletList' =\= [],
    NewList =?= [] :
      Debug ! time(Now'),
      Record ! Now',
      Now = _,
      Waiter ! machine(idle_wait(Wakeup'), _Ok),
      WakeVar = Vector |
	self;

    Wakeup =?= done,
    NewList =\= [] :
      Debug ! time(Now'?),
      Now = _,
      Record ! Now',
      Triplet = {Now', Vector, Vector},
      Waiter ! machine(idle_wait(Wakeup'), _Ok),
      NewList' = [] |
	SORT(Merger1, NewList, Sorted),
	MERGE(Merger2, Sorted, TripletList, TripletList', Triplet),
	self.

  wait_done(Done) :-

    known(Done) |
	pi_monitor#reset.


quick_sort_triplets(List, Sorted) + (Tail = []) :-

    List ? Triplet, Triplet = {Time, Vector, Event} |
	partition_triplets(Time, Vector, Event, List', Small, Large),
	quick_sort_triplets(Small?, Sorted, [Triplet | LSorted]),
	quick_sort_triplets(Large?, LSorted, Tail);

    List ? _,
    otherwise |
	self;

    List = [] :
      Sorted = Tail.


merge_triplets(S1, S2, S3) :-

    S1 ? Triplet, Triplet =?= {X, _V1, _W1},
    S2 = [{Y, _V2, _W2} | _],
    X < Y :
      S3 ! Triplet |
	self;

    S1 =?= [{X, _V1, _W1} | _],
    S2 ? Triplet, Triplet =?= {Y, _V2, _W2},
    Y < X :
      S3 ! Triplet |
	self;

    S1 ? Triplet, Triplet =?= {X, V1, _W1},
    S2 = [{X, V2, _W2} | _],
    V1 =\= V2 :
      S3 ! Triplet |
	self;

    S1 ? Triplet,
    S2 ? Match :
      Triplet = Match,
      S3 ! Triplet |
	self;

    S1 = [] :
      S3 = S2;

    S2 = [] :
      S3 = S1.

  partition_triplets(X, V, E, List, Small, Large) :-

    List ? Triplet, Triplet =?= {Y, _V, _W},
    X > Y :
      Small ! Triplet |
	self;

    List ? Triplet, Triplet =?= {Y, _V, _W},
    X < Y :
      Large ! Triplet |
	self;

    List ? Triplet, Triplet =?= {X, NV, _W},
    V =\= NV :
      Small ! Triplet |
	self;

    List ? Triplet, Triplet =?= {X, V, W} :
      W = E |
	self;

    List = [] :
      X = _,
      V = _,
      E = _,
      Small = [],
      Large = [].
