-monitor(serve).
-language([evaluate,compound,colon]).
-export([get_global_channels/1, global_channels/1, global_channels/2,
	 debug/1, record/1, scheduler/1,
	 options/2, reset/1]).

/*
SORT(Merger, List, Sorted) => 
	ordered_merger(Merger1),
	binary_sort_merge(List, Sorted, Merger).

MERGE(Merger, Sorted, List1, List2, Pair) =>
	ordered_merger(Merger),
	binary_merge([Sorted, List1], [Pair | List2], Merger).
*/

SORT(Merger, List, Sorted) =>
	quick_sort_pairs(List, Sorted).

MERGE(Merger, Sorted, List1, List2, Pair) =>
	merge_pairs(Sorted, List1, [Pair | List2]).

RAN =>  4.					/* random real number */
LN  =>  9.					/* natural logarithm */

serve(In) :-

    In =?= [] |
	true;

    In =\= [],
    Dash := ascii('-') |
	server(In, 1, Dash, nil, [], [{0, _}, {[], _}], Scheduler),
	processor#link(lookup(math, Offset), Ok),
	start_scheduling(Scheduler, Offset, Ok).

server(In, UID, Dash, Tree, Options, Global, Scheduler) :-

    In ? unique_sender(String, Name),
    string_to_dlist(String, Head, [Dash | Tail]),
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

    In ? reset(List),
    we(List) :
      List = List'? |
	copy_global(Global, List', Global'),
	self;

    In ? options(New, Old) :
      Options' = New? |
	unify_without_failure(Options, Old),
	self;

    In ? scheduler(Scheduler^) |
	self;

    In ? debug(Stream?) :
      write_channel(debug(Stream), Scheduler) |
	self;

    In ? record(Stream?) :
      write_channel(record(Stream), Scheduler) |
	self;

    In ? Other,
    otherwise |
	self,
	fail(Other, unknown);

    In = [] :
      UID = _,
      Dash = _,
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
    Global ? Name(PiChannel),
    PiChannel = Id(Vector, Stream, ReceiveMean, SendMean) :
      NewChannel = Id(Vector, Stream'?, ReceiveMean, SendMean),
      New_Global ! Name(NewChannel),
      Last = _,
      Last' = Name |
	cdr_past_msgs(Stream, Stream'),
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
	pi_channel(PiChannel, Id?, _, _, ReceiveMean, SendMean),
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


cdr_past_msgs(In, Out) :-

    In ? _Sender(_Msg, _Tag, Choice),
    known(Choice) |
	self;

    In = [_Sender(_Msg, _Tag, Choice) | _],
    unknown(Choice) :
      Out = In;

    unknown(In) :
      Out = In.

start_scheduling(Scheduler, Offset, Ok) :-

    Ok =?= true,
    convert_to_real(0, Zero) :
      make_channel(Scheduler, Schedule),
      execute(Offset, {RAN, 0, Uniform}),
      execute(Offset, {LN, Uniform, NegativeExponential}) |
	scheduling(Schedule, Offset, NegativeExponential,
			Zero, [], [], false, _Wakeup, _Recording, _Debug);

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
** Maintain time  Now  and sorted list of scheduling pairs,  PairList ,
** and an unsorted auxiliary list,  NewList , of new scheduling pairs:
**
**	{WakeupTime, WaitVariable}
**
** a. For each schedule(List) item:
**
**	(1) WakeupTime := integer(Now + exponential(Mean)),
**	    where exponential(Mean) := Variate is computed by
**
**		execute(MathOffset, {MathExponential, Mean, Variate})
**
**	    Initially WakeUpTime is computed by:
**
**		execute(MathOffset, {Random, 0, UniformVariate}),
**		execute(MathOffset, {NaturalLog, UniformVariate, Variate}),
**		WakeupTime := Now - Mean*Variate,
**
**	(2) Add scheduling pair to an NewList (push onto front).
**
** b. Whenever system becomes idle and NewList or PairList is non-empty:
**
**	(1) Sort NewList and mege with PairList on WakeupTime,
**	    unifying WaitVariable for equal WaitTime.
**
**	(2) Unify "true" with first WaitVariable and discard that pair.
**
** c. Provide a record of:
**
**	WaitList <token>  % (End of WaitList may be any non-list token.)
**      Changes to  Now .
**      Termination <token>.
*/

scheduling(Schedule, Offset, NegativeExponential,
	Now, PairList, NewList,	Waiting, Wakeup, Record, Debug) :-


    Schedule ? schedule([receive(Channel, WakeVar) | More]),
    Channel = Ic(_Vc, _Sc, 0, _Send) :
      Debug ! receive(Ic, 0),
      WakeVar = true,
      Schedule'' = [schedule(More) | Schedule'] |
	self;

    Schedule ? schedule([send(Channel, WakeVar) | More]),
    Channel = Ic(_Vc, _Sc, _Receive, 0) :
      Debug ! send(Ic, 0),
      WakeVar = true,
      Schedule'' = [schedule(More) | Schedule'] |
	self;

    Schedule ? schedule([receive(Channel, WakeVar) | More]),
    Channel = Ic(_Vc, _Sc, Mean, _Send), Mean =\= 0,
    Waiting =\= true,
    WakeupTime := Now - Mean*NegativeExponential :
      Debug ! receive(Ic, WakeupTime),
      Wakeup = _,
      Waiting' = true,
      Schedule'' = [schedule(More) | Schedule'],
      NewList' = [{WakeupTime, WakeVar} | NewList],
      execute(Offset, {RAN, 0, Uniform}),
      execute(Offset, {LN, Uniform, NegativeExponential'}) |
	processor#machine(idle_wait(Wakeup'), _Ok),
	self;

    Schedule ? schedule([send(Channel, WakeVar) | More]),
    Channel = Ic(_Vc, _Sc, _Receive, Mean), Mean =\= 0,
    Waiting =\= true,
    WakeupTime := Now - Mean*NegativeExponential :
      Debug ! send(Ic, WakeupTime),
      Wakeup = _,
      Waiting' = true,
      Schedule'' = [schedule(More) | Schedule'],
      NewList' = [{WakeupTime, WakeVar} | NewList],
      execute(Offset, {RAN, 0, Uniform}),
      execute(Offset, {LN, Uniform, NegativeExponential'}) |
	processor#machine(idle_wait(Wakeup'), _Ok),
	self;

    Schedule ? schedule([receive(Channel, WakeVar) | More]),
    Channel = Ic(_Vc, _Sc, Mean, _Send), Mean =\= 0,
    Waiting =?= true,
    WakeupTime := Now - Mean*NegativeExponential :
      Debug ! receive(Ic, WakeupTime),
      Schedule'' = [schedule(More) | Schedule'],
      NewList' = [{WakeupTime, WakeVar} | NewList],
      execute(Offset, {RAN, 0, Uniform}),
      execute(Offset, {LN, Uniform, NegativeExponential'}) |
	self;

    Schedule ? schedule([send(Channel, WakeVar) | More]),
    Channel = Ic(_Vc, _Sc, _Receive, Mean), Mean =\= 0,
    Waiting =?= true,
    WakeupTime := Now - Mean*NegativeExponential :
      Debug ! send(Ic, WakeupTime),
      Schedule'' = [schedule(More) | Schedule'],
      NewList' = [{WakeupTime, WakeVar} | NewList],
      execute(Offset, {RAN, 0, Uniform}),
      execute(Offset, {LN, Uniform, NegativeExponential'}) |
	self;

    Schedule ? schedule(Token), Token =\= [_|_] :
      Debug ! process(Token),
      Record ! Token |
	self;

    Schedule =?= [] :
      Offset = _,
      NegativeExponential = _,
      Now = _,
      PairList = _,
      NewList = _,
      Waiting = _,
      Wakeup = _,
      Record = [],
      Debug = [];

    Schedule ? record(Stream) :
      Stream = Record? |
	self;

    Schedule ? end_record(Stream) :
      Record = [],
      Stream = Record'? |
	self;

    Schedule ? terminate(Token, When?) :
      When = Now,
      Record ! Token |
	self;

/**************************** Debuging code ********************************/

    Schedule ? input(Stream) :
      Stream = Schedule'? |
	self;

    Schedule ? debug(Stream) :
      Stream = Debug? |
	self;

    Schedule ? Other,
    otherwise |
	fail(Other),
	self;

/***************************************************************************/

    Wakeup =?= done,
    PairList =?= [],
    NewList =?= [] :
      Debug ! idle(Now),
      Waiting = _,
      Waiting' = false,
      Wakeup' = _ |
	self;

    Wakeup =?= done,
    PairList ? {Now', Signal}, PairList' =?= [],
    NewList =?= [] :
      Debug ! time(Now'),
      Record ! Now',
      Now = _,
      Waiting = _,
      Signal = true, % done(Now, Now'),
      Waiting' = false |
%     Now'' = 0 |
	self;

    Wakeup =?= done,
    PairList ? {Now', Signal}, PairList' =\= [],
    NewList =?= [] :
      Debug ! time(Now'),
      Record ! Now',
      Now = _,
      Signal = true | % done(Now, Now') |
	processor#machine(idle_wait(Wakeup'), _Ok),
	self;

    Wakeup =?= done,
    NewList =\= [] :
      Debug ! time(Now'),
      Now = _,
      Record ! Now',
      Pair = {Now', true}, % done(Now, Now')},
      NewList' = [] |
	SORT(Merger1, NewList, Sorted),
	MERGE(Merger2, Sorted, PairList, PairList', Pair),
	processor#machine(idle_wait(Wakeup'), _Ok),
	self.

quick_sort_pairs(List, Sorted) + (Tail = []) :-

    List ? Pair, Pair = {Time, Event} |
	partition_pairs(Time, Event, List', Small, Large),
	quick_sort_pairs(Small?, Sorted, [Pair | LSorted]),
	quick_sort_pairs(Large?, LSorted, Tail);

    List ? _,
    otherwise |
	self;

    List = [] :
      Sorted = Tail.


merge_pairs(S1, S2, S3) :-

    S1 ? Pair, Pair =?= {X, _V1},
    S2 = [{Y, _V2} | _],
    X < Y :
      S3 ! Pair |
	self;

    S1 =?= [{X, _V1} | _],
    S2 ? Pair, Pair =?= {Y, _V2},
    Y < X :
      S3 ! Pair |
	self;

    S1 ? Pair,
    S2 ? Match :
      Pair = Match,
      S3 ! Pair |
	self;

    S1 = [] :
      S3 = S2;

    S2 = [] :
      S3 = S1.

  partition_pairs(X, E, List, Small, Large) :-

    List ? Pair, Pair =?= {Y, _V},
    X > Y :
      Small ! Pair |
	self;

    List ? Pair, Pair =?= {Y, _V},
    X < Y :
      Large ! Pair |
	self;

    List ? Pair, Pair =?= {X, V} :
      V = E |
	self;

    List = [] :
      X = _,
      E = _,
      Small = [],
      Large = [].

/********************* Borrowed from Logix: utils.cp *************************/
/*************************  Slower Sort/Merge ********************************
ordered_merger(In) :-

    In ? ordered_merge(In1, In2, Out) |
	ordered_merge(In1, In2, Out),
	ordered_merger;

    In = [] |
	true.

ordered_merge(In1, In2, Out) :-

    In1 ? T1, T1 = {I1, _}, In2 = [{I2, _} | _],
    I1 < I2 :
      Out ! T1 |
	ordered_merge;

    In1 = [{I1, _} | _], In2 ? T2, T2 = {I2, _},
    I2 < I1 :
      Out ! T2 |
	ordered_merge;

    In1 = [T1 | _], In2 ? T2 :
      T1 = T2 |
	ordered_merge;

    In1 = [] :
      In2 = Out;

    In2 = [] :
      In1 = Out.

binary_merge(In, Out, Merger) :-
    true :
      make_channel(CH, Merger) |
	level1_merge_server(In, Odds, Evens, PairList),
	binary_merger(Odds, Evens, PairList, Out, CH, done, Done),
	close_merger(Done, CH).

level1_merge_server(In, Odds, Evens, PairList) :-

    In ? Odds^ |
	level1_merge_server(In', Evens, PairList);

    otherwise : In = _,
      Odds = [], Evens = [],
      PairList = [] .

level1_merge_server(In, Evens, PairList) :-

    In ? Evens^ :
      PairList ! {One, Two} |
	level1_merge_server(In', One, Two, PairList');

    otherwise : In = _,
      Evens = [],
      PairList = [] .


binary_sort_merge(In, Out, Merger) :-
    true :
      make_channel(CH, Merger) |
	level1_sort_server(In, Odds, Evens, PairList),
	binary_merger(Odds, Evens, PairList, Out, CH, done, Done),
	close_merger(Done, CH).

level1_sort_server(In, Odds, Evens, PairList) :-

    In ? Odd :
      Odds = [Odd] |
	level1_sort_server(In', Evens, PairList);

    otherwise : In = _,
      Odds = [], Evens = [],
      PairList = [] .

level1_sort_server(In, Evens, PairList) :-

    In ? Even :
      Evens = [Even],
      PairList ! {One, Two} |
	level1_sort_server(In', One, Two, PairList');

    otherwise : In = _,
      Evens = [],
      PairList = [] .


binary_merger(In1, In2, PairList, Out, CH, Left, Right) :-

    list(In2) :
      write_channel(ordered_merge(In1, In2, In1'), CH) |
	binary_merger2(PairList, In2', PairList', CH, Left, Left'),
	binary_merger;

    In2 = [] : PairList = _, CH = _, 
      In1 = Out,
      Left = Right .

binary_merger1(PairList, UpList, CH, Left, Right) :-

    PairList ? {In1, In2} :
      UpList ! {Out1, Out2},
      write_channel(ordered_merge(In1, In2, Out1), CH) |
	binary_merger2(PairList', Out2, UpList', CH, Left, Right);

    PairList = [] : CH = _,
      UpList = [],
      Left = Right .

binary_merger2(PairList, Out, UpList, CH, Left, Right) :-

    PairList ? {In1, In2} :
      write_channel(ordered_merge(In1, In2, Out), CH) |
	binary_merger1(PairList', UpList, CH, Left, Right);

    PairList = [] : CH = _,
      Out = [],
      UpList = [],
      Left = Right .


close_merger(done, CH) :-
    true :
      close_channel(CH) .
*****************************************************************************/
