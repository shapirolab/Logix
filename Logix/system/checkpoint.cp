/*

Checkpoint monitor

William Silverman

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:02:52 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/system/checkpoint.cp,v $

Copyright (C) 1992, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language(dfcp).
-mode(trust).
-export([checkpoint/3]).

/*
	Checkpoint Facility Specification
	
	checkpoint # checkpoint(FileName, Frequency, Reply)

   where:

   input 	FileName ::= String
   input	Frequency ::=[ minutes(Minutes) | hours(Hours) |
			      days(Days) | weeks(Weeks) ]
   output	Reply ::= true | false(String)

		Minutes, Hours, Days, Weeks ::= Integer

		(The Integer must be at least 1.)

   Semantics:

		An immediate checkpoint is written to FileName_Oldest,
		and an appropriate Reply is returned.  Checkpoints
		are taken at the specified frequency thereafter.
		Checkpointing is terminated by aborting the computation.

   The checkpoint file may be restarted like any saved system - e.g. if
   the original system was started by the command:

	~fcp/Logix/Emulator/fcp  ~fcp/Logix/saved_logix

   and was checkpointed to ~/intermediate_state, it may be restarted (as
   of the last checkpoint) by the command:

	~fcp/Logix/Emulator/fcp  ~/intermediate_state

   To avoid overwriting the checkpoint file, the saved system is actually
   written to FileName.next; then the unix commands:

        mv FileName FileName_Oldest.prev
	mv FileName.next FileName_Oldest

   are executed.  The initial Reply is instantiated after the second
   command is completed.  Thus in the stable state of the process, there
   are two files, FileName_Type.prev and FileName_Type, each of which
   contains a saved system, separated by one Type period.

   A file write failure during check-pointing is logged:

        checkpoint_failed(FileName, Type Reason)

   A file move failure during check-pointing is logged:

        move_failed(Name, Type, Reason)

*/

checkpoint(Name, Frequencies, Reply) :-

    string(Name), Name =\= "", 
    Frequencies =\= [] |
	string_to_dlist(Name, NXL, Next?),
	list_to_string(NXL?, NameNext),
	string_to_dlist(".next", Next, []),
	string_to_dlist(Name, NLU, UL?),
	UnderLine := ascii('_'),
	UL = [UnderLine?],
	list_to_string(NLU?, Name'),
        transform_delays(Frequencies, Frequencies1, Result),
	processor # Processor?,
        gcd(Frequencies1?, Frequencies', Timer),
	start(Result?, Timer?, Reply, Processor, Processor'?,  Ticks),
	server;

    string(Name), Name =\= "",
    Frequencies = [] |
	Name = _,
	Reply = false(no_frequencies);

    otherwise |
	Frequencies = _,
	Reply=false(bad_name(Name)).


gcd(List, Frequencies, Timer) :-
    listener(List) |
	Timer = Timer'?,
	gcd1,
	div(List, Timer'??, Frequencies).

div(List, Timer, ListOut) :-

    List = [] |
	Timer = _,
	ListOut = [];

    List ? Type(T1) ,
    T1' := T1/Timer |
	ListOut ! Type(T1', "", 0),
	self.


gcd1(List, Timer) :-
	
     List = [Item],
     Item = _(Delay) |
	Timer = Delay;

     List = [_(D1), _(D2) | List'] |
	gcd2(D1, D2, D3),
	List'' = [_(D3?) | List'],
	self.

gcd2(D1, D2, D3) :-

     D1 > D2 |
     gcd3(D1, D2, D3);

     D1 < D2 |
     gcd3(D2, D1, D3);

     D1 = D2 |
	D3 = D1.

gcd3(A, B, C) :-

     B = 0 |
	 C = A;

     B =\= 0,
     B' := A \ B |
	gcd2(B, B', C).


transform_delays(Frequencies, Frequencies1, Result) :-
    
    Frequencies ? Frequency,
    Frequency = minutes(N),
    integer(N), 
    N > 0 |
	Frequencies1 ! minutes(N),
	self;

    Frequencies ? Frequency,
    Frequency = hours(N), 
    integer(N), 0 < N,
    N' := N * 60 |
	Frequencies1 ! hours(N'),
	self;

    Frequencies ? Frequency,
    Frequency = days(N), 
    integer(N), 0 < N,
    N' := N*24*60 |
	Frequencies1 ! days(N'),
	self;

    Frequencies ? Frequency,
    Frequency = weeks(N),
    integer(N), 0 < N,
    N' := integer(N*14*24*60) |
	Frequencies1 ! weeks(N'),
	self;

    Frequencies = _(_) |
	Frequencies' = [Frequencies],
	self;

    Frequencies = [] |
	Frequencies1 = [],
	Result = true;

    otherwise |
	Frequencies1 = [],
      	Result = false(Frequencies) .


start(Result, Timer, Reply, Processor1, Processor2, Ticks) :-

    Result =\= true |
	Timer = _,
	Reply = Result,
	Processor1 = Processor2,
	Ticks = [] ;

    Result = true |
	Reply = Reply'?,
	Processor1 = [time(signals(real, minutes(Timer), Ticks1), Reply')
		     | Processor2],
	started(Reply'??, Ticks, Ticks1?).

started(Reply, Ticks1, Ticks2) :-

    Reply = true :
	Ticks1 = [tick | Ticks2] ;

    Reply =\= true :
	Ticks1 = Ticks2 .


server(Frequencies, Name, NameNext, Ticks, Processor) :-

    string(NameNext),
    Ticks ? tick |
	checkpoint_frequencies;

    Ticks = [] :
	Frequencies = _,
	Name = _,
	NameNext = _,
	Processor = [] .


checkpoint_frequencies(Frequencies, Name, NameNext, Ticks, Processor)
 + (Frequencies1, Oldest) :-

    initially |
	Frequencies1 = [],
        Oldest = none;

    Frequencies ? Frequency, 
    Frequency = Type(Delay, Last, Tics--),
    Tics' =< 0,
    Oldest = none,
    string(Type) |
	Oldest' = Type(Delay, Last),
	self;

    Frequencies ? Frequency, 
    Frequency = Type(Delay, Last, Tics--),
    Tics' =< 0,
    Oldest = OldType(OldDelay, OldLast),
    OldDelay < Delay,
    string(Type) |
	Oldest' = Type(Delay, Last),
	Frequency1 = OldType(OldDelay, OldLast, OldDelay),
	Frequencies1' = [Frequency1? | Frequencies1],
	self;

    Frequencies ? Frequency, 
    Frequency = Type(Delay, Last, Tics--),
    Oldest = _OldType(OldDelay, _OldLast),
    OldDelay >= Delay,
    Tics' =< 0 |
	Frequency1 = Type(Delay, Last, Delay),
	Frequencies1' = [Frequency1? | Frequencies1],
	self;

    Frequencies ? Frequency, 
    Frequency = Type(Delay, Last, Tics--),
    Tics' > 0 |
	Frequency1 = Type(Delay, Last, Tics'),
	Frequencies1' = [Frequency1? | Frequencies1],
	self;

    Frequencies = [],
    Oldest = Type(Delay, Last),
    string(Type),
    integer(Delay),
    string(Last),
    string(NameNext) |
	Frequencies' = [Type(Delay, Type, Delay) | Frequencies1],
	Processor ! machine(boot_wait(Restarted)),
	Processor' ! machine(request(save_state(NameNext), Done)),
	checkpoint_done;

  Frequencies = [],
  Oldest = none |
	Frequencies' = Frequencies1,
	server.
    

checkpoint_done(Frequencies, Name, NameNext, Ticks, Processor,
		Restarted, Done, Type, Last
) :-

    Done = done, unknown(Restarted),
    Type = Last,
    string(Last),
    string(Name) |
	string_to_dlist("mv ", ML, NL?),
	string_to_dlist(Name, NL, LL?),
	string_to_dlist(Last, LL, SNL?),
	Space := ascii(' '),
	SNL ! Space?,
	string_to_dlist(Name, SNL', LL1?),
	string_to_dlist(Last, LL1, PL?),
	string_to_dlist(".prev", PL, []),
	list_to_string(ML?, MoveLast),
	Processor ! interface(unix_call(MoveLast?), Ok),
	move_done;

    Done = done, unknown(Restarted),
    Type =\= Last,
    string(Name),
    string(NameNext),
    string(Type) |
	string_to_dlist("mv ", ML, NXL?),
	string_to_dlist(NameNext, NXL, SNL?),
	Space := ascii(' '),
	SNL ! Space?,
	string_to_dlist(Name, SNL', PL?),
	string_to_dlist(Type, PL, []),
	list_to_string(ML?, MoveNext),
	Processor ! interface(unix_call(MoveNext?), Ok),
	checkpoint_stable(Name, Type, Ok?, Processor', Processor''?),
       	server;

    Done =\= done,
    string(Name),
    Frequencies ? Type(Delay, _Last, Delay) |
	Restarted = _,
	Processor ! event(checkpoint_failed(Name, Type, Done)),
	Frequencies'' = [Type(Delay, Last, Delay) | Frequencies'],
	server;

    known(Restarted) |
	Done = _,
	Type = _,
	Last = _,
	Processor ! device(restart),
	server.

move_done(Frequencies, Name, NameNext, Ticks, Processor,
		Restarted, Done, Type, Ok
) :-

    Ok =\= false(_) |
	Last = "",
	checkpoint_done;

    Ok = false(Reason),
    string(Name),
    string(Type) |
	Processor ! event(move_failed(Name, Type, Reason)),
	Last = "",
	checkpoint_done.

checkpoint_stable(Name, Type, Ok, Processor, Processor1) :-

    Ok = true |
	Name = _, Type = _,
	Processor = Processor1 ;

    Ok = false(Reason) |
        Processor = [event(move_failed(".next to", Name, Type, Reason))
		    | Processor1] .
