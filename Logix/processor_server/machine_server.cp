/*

William Silverman 04/08/85
modified: Avshalom Houri 12/87

Machine manager process

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:39 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/processor_server/machine_server.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-mode(trust).
-export([start/2]).
-language(compound).


procedure start(Input, Channel).

Input ::= {(poller(Input) ; Command), Ok, Common}.

start(Input, Defer) :-
    true :
      machine_output(Machine),
      request(idle_output(0), _) |
	filter(Input, In, Defer),
	server(In, Machine, _Faults).

procedure filter(Input, In, Channel).

filter(Input, In, Defer) :-

    Input ? {poller(Polls), true^, Common} |
	polling(Polls?, Common, Defer),
	filter;

    Input ? {IdleQueue, Ok, Common},
    IdleQueue = idle_queue(_, Priority),
    unknown(Priority) :
      write_channel(defer(Priority, {machine(IdleQueue, Ok), Common}), Defer) |
	filter;

    Input ? {Request, Ok, Common},
    Request = request(Tuple, _),
    unknown(Tuple) :
      write_channel(defer(Tuple, {machine(Request, Ok), Common}), Defer) |
	filter;

    Input ? {Poll, Ok, Common},
    Poll = poll(_, Delay),
    unknown(Delay) :
      write_channel(defer(Delay, {machine(Poll, Ok), Common}), Defer) |
	filter;

    Input ? Other,
    otherwise :
      In ! Other |
	filter;

    Input = [] : Defer = _,
      In = [] .


polling(Polls, Common, Defer) :-

    Polls ? Request :
      Common = fork(Common1, Common'),
      write_channel({machine(Request, _Ok), Common1}, Defer) |
	self;

    Polls ? _,
    otherwise : Polls' = _, Defer = _,
      Common = exception(blocked, Polls) ;

    Polls = [] : Defer = _,
      Common = done ;

    Polls =\= [_|_], Polls =\= [] |
	self + (Polls = [Polls]);

    known(Common) : Polls = _, Defer = _ .


Faults ::= [Fault].
Fault ::= Reason(Any, Any).
Reason ::= lost ; failed.

procedure server(In, Machine, Faults).
procedure server(In, Machine, Faults, Queue, BootVar, GCs).

In ::= [{Command, Ok, Common}].
Command ::= boot_wait(BootVar) ;
	    collections(GCs) ;
	    faults(Faults, Faults) ;
	    idle_queue(done, Number) ;
	    idle_wait(done) ;
	    poll(Poll, Number) ;
	    request(Tuple, String).

Machine ::= [MachineSignal].
Queue ::= NonEmptyQueue ; [].
NonEmptyQueue ::= [waiter(done, Number) | Queue].
BootVar ::= booted.
GCs ::= [GC].
Poll ::= block ; continue.

server(In, Machine, Faults) + (Queue = [], BootVar = _, GCs = _) :-

    In ? {request(Request, Answer), true^, done^} :
      request(Request, Answer) |
	server;

    In ? {idle_wait(Reply?), true^, done^} |
	enter_queue(100, Reply, Queue, Queue', In', In''),
	server;

    In ? {idle_queue(Reply?, Priority), true^, done^},
    integer(Priority) |
	enter_queue(Priority, Reply, Queue, Queue', In', In''),
	server;

    In ? {collections(Collections), true^, done^} :
      Collections = GCs? |
	server;

    In ? {boot_wait(Booted), true^, done^} :
      Booted = BootVar? |
	server;

    In ? {faults(Faults'?, Old), true^, done^} :
      Faults = Old? |
	server;

    In ? {poll(Poll?, Delay), true^, done^},
    integer(Delay),
    info(5, Reds) :
      deschedule |
	poll(In', Machine, Faults, Queue, BootVar, GCs,
		Poll, Delay, Reds, 0
	);

    In ? _Other(false(unknown)^, true^),
    otherwise |
	server;

    In ? _Any(false(aborted)^, Abort),
    known(Abort) |
	server;

    Machine ? MachineSignal |
	check_machine(In, Machine', Faults, Queue, BootVar, GCs,
			MachineSignal
	);

    In = [], Queue = [] : Machine = _, BootVar = _,
      Faults = [],
      GCs = [] .


procedure poll(In, Machine, Faults, Queue, BootVar, GCs,
		Poll, Integer, Integer, Integer
).

poll(In, Machine, Faults, Queue, BootVar, GCs,
	Poll, Delay, Reds, Decrement
) :-

    unknown(In), unknown(Machine),
    Decrement =\= 1,
    Delay > 0,
    Delay' := Delay - Decrement,
    info(5, Reds'),
    Decrement' := Reds' - Reds :
      deschedule |
	poll;

    unknown(In), unknown(Machine),
    Decrement =:= 1 : Delay = _, Reds = _ |
	block(In, Machine, Faults, Queue, BootVar, GCs, Poll);

    otherwise : Delay = _, Reds = _, Decrement = _,
      Poll = continue |
	server(In, Machine, Faults, Queue, BootVar, GCs).


procedure block(In, Machine, Faults, Queue, BootVar, GCs, Poll).

block(In, Machine, Faults, Queue, BootVar, GCs, Poll) :-

    Queue = [] :
      Poll = block |
	server(In, Machine, Faults, Queue, BootVar, GCs);

    Queue ? waiter(done^, _) :
      Poll = continue |
	check_queue(Queue', Queue'', In, In'),
	server(In', Machine, Faults, Queue'', BootVar, GCs).


procedure enter_queue(Integer, done, Queue, NonEmptyQueue, In, In).

enter_queue(Priority, IdleReply, Queue, NonEmptyQueue, In1, In2) :-

    Queue = [] :
      NonEmptyQueue = [waiter(IdleReply, Priority)],
      In2 = [{request(idle_output(1), _), true, _Common} | In1] ;

    Queue ? waiter(R,P),
    Priority =< P :
      NonEmptyQueue ! waiter(R,P) |
	enter_queue;

    Queue = [waiter(_,P) | _],
    Priority > P :
      NonEmptyQueue = [waiter(IdleReply, Priority) | Queue],
      In1 = In2 .


MachineSignal ::= idle ;
		  failed(Process) ;
		  GC ;
		  booted.

GC ::= gc_done(Processes).

procedure check_machine(In, Machine, Faults, Queue, BootVar, GCs,
				MachineSignal
).

check_machine(In, Machine, Faults, Queue, BootVar, GCs,
		MachineSignal
) :-

    MachineSignal = idle,
    Queue ? waiter(done^, _) |
	check_queue(Queue', Queue'', In, In'),
	server(In', Machine, Faults, Queue'', BootVar, GCs);

    MachineSignal = idle,
    Queue = [] :
      request(idle_output(0), _) |
	server(In, Machine, Faults, Queue, BootVar, GCs);

    MachineSignal = failed(Process) |
	display_process(failed, Process, Faults, Faults'),
	server(In, Machine, Faults', Queue, BootVar, GCs);

    MachineSignal = gc_done(Lost) :
      GCs ! MachineSignal |
	display_lost_list(Lost, Faults, Faults'),
	server(In, Machine, Faults', Queue, BootVar, GCs');

    MachineSignal = booted :
      BootVar = MachineSignal |
	server(In, Machine, Faults, Queue, _BootVar, GCs);

    otherwise |
	fault(unknown_signal(MachineSignal)),
	server(In, Machine, Faults, Queue, BootVar, GCs).

fault(ng).

procedure check_queue(Queue, Queue, In, In).

check_queue(Queue1, Queue2, In1, In2) :-

    Queue1 = [] :
      Queue1 = Queue2,
      In2 = [{request(idle_output(0), _), true, _Common} | In1] ;

    otherwise :
      Queue1 = Queue2,
      In1 = In2 .


procedure display_lost_list(Processes, Faults, Faults).

Processes ::= [Process].
Process ::= Tuple ; [].

display_lost_list(Processes, Faults1, Faults2) :-

    Processes ? Process |
	display_process(lost, Process, Faults1, Faults1'),
	display_lost_list;

    Processes = [] :
      Faults1 = Faults2 .


procedure display_process(Reason, Process, Faults, Faults).

display_process(Reason, Process, Faults1, Faults2) :-

    arg(1, Process, Procedure), arg(2, Process, MODULE),
    code_info(Procedure, CodeInfo) :
      Faults1 = [Reason(MODULE, Descriptor?) | Faults2] |
	display_descriptor(CodeInfo, Process, Descriptor);

    Process = [] : Reason = _,
      Faults1 = Faults2 .


procedure display_descriptor(CodeInfo, Tuple, Atom).

CodeInfo ::= {String} ; module(String) ;
	     procedure(ObjectName, ObjectName, Integer) ;
	     ctl(ObjectName, ObjectName, Integer, Integer) .
Atom ::= String ; Tuple.

display_descriptor(Info, Process, Atom) :-

    Info = {native_code},
%   This clause is true when we are in "select"
    Process = {ModuleIndex, _, ProcedureIndex,
	       export(Goal), procedures(_, _, Events)} |
      Atom = (ModuleIndex : ProcedureIndex) # rpc(Goal, Events) ;

    Info = {native_code},
    arg(1, Process, ModuleIndex),
    arg(3, Process, ProcedureIndex),
    Arity := arity(Process) ,
    Arity1 := Arity - 2, % subtract the first 3 words and add 1 for the arity
    make_tuple(Arity1, Goal1),
    arg(1, Goal1, ( ModuleIndex : ProcedureIndex)) |
    make_goal(4, Arity, Process, 0, 2, Goal1, Goal2),
    display_rpc(native_code, Goal2, Atom); 	

    Info = {Description},
    otherwise,
    Arity := arity(Process) - 2 |
	display_goal(Description, Arity, Arity, Process, Atom);

    Info = module(ModuleName) : Process = _,
      Atom = ModuleName ;

    Info = procedure(ObjectModule, ObjectProcedure, Arity) |
	display_name(ObjectProcedure, ProcedureName),
	display_name(ObjectModule, ModuleName),
	display_goal(ProcedureName, Arity, 8, Process, Goal),
	display_rpc(ModuleName, Goal, Atom);

    Info = ctl(ObjectModule, ObjectProcedure, Arity, _Index) |
	display_name(ObjectProcedure, ProcedureName),
	display_name(ObjectModule, ModuleName),
	display_goal(ProcedureName, Arity, 256, Process, Goal),
	display_rpc(ModuleName, Goal, Atom);

    otherwise : Atom = _ |
	computation # display(term,
			["machine - display_descriptor: unexpected args ",
			 Info, Process]).


procedure display_name(ObjectName, String).

ObjectName ::= [] ; String.

display_name(ObjectName, Name) :-

    ObjectName = [] :
      Name = '' ;

    otherwise :
      ObjectName = Name .


procedure display_rpc(String, Any, RPC).

RPC ::= {`'#', String, Any}.

display_rpc(Name, Goal, RPC) :-
    known(Name), known(Goal) :
      RPC = Name # Goal .


procedure display_goal(String, Integer, Integer, Tuple, Any).

display_goal(Name, Arity, Bound, Process, Goal2) :-
    known(Name),
    A := Arity + 1,
    make_tuple(A, Goal1),
    arg(1, Goal1, Name) |
	limit_rest(Arity, Bound, M, Rest),
	make_goal(3, M, Process, Rest?, 2, Goal1, Goal2).


procedure limit_rest(Integer, Integer, Integer, Integer).

limit_rest(Arity, Bound, Limit, Rest) :-

    Arity =< Bound,
    Limit^ := Arity + 2 :
      Rest = 0 ;

    Arity > Bound,
    Limit^ := Bound + 1,
    Rest^ := Arity - Bound + 1 |
	true.


procedure make_goal(Integer, Integer, Tuple, Integer, Integer, Any, Any).

make_goal(From, Limit, Process, Rest, To, Goal1, Goal2) :-

    From =< Limit,
    arg(From, Process, A),
    arg(To, Goal1, A^),
    From' := From + 1, To' := To + 1 |
	make_goal;

    From > Limit,
    Rest = 0 : Process = _, To = _,
      Goal1 = Goal2 ;

    otherwise,
    arg(10, Process, Residue) : From = _, Limit = _ |
	make_goal(1, Rest, Residue, 0, To, Goal1, Goal2).
