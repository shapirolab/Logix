/*

Real Time monitor

William Silverman

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:38 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/processor_server/time_server.cp,v $

Copyright (C) 1992, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language([evaluate,compound,colon]).
-mode(trust).
-export(start).

MaxInt => 33554431.

/*
   Signals:

	time(signals(Type, Period, Ticks))		or
	time(signals(Type, Period, Times, Ticks))
	time(ticks(Type, Period, Resets, Ticks))

   Call to receive a stream of Ticks each Rate units, indefinitely,
   or Times iterations; Resets is a stream of "reset" tokens.

*/

TimerState ::= closed ;
	       Status(machine_timer # Commands) .
Status ::= reset ; set .
States ::= {TimerState, TimerState, TimerState} .

start(Input, Defer) :-
	filter,
	server(In, {closed, closed, closed}).

filter(Input, In, Defer) :-

    Input ? {Request, Ok, Common},
    arg(2, Request, Type),
    Type =\= real, Type =\= virtual, Type =\= profiling :
      Ok = false(invalid_type) |
	self,
	done;

    Input ? {Request, Ok, Common},
    arg(2, Request, Type),
    unknown(Type) :
      write_channel(defer(Type, {time(Request, Ok), Common}), Defer) |
	self;

    Input ? {Request, Ok, Common},
    Request =\= counts(_, _),
    arg(3, Request, Rate),
    unknown(Rate) :
      write_channel(defer(Rate, {time(Request, Ok), Common}), Defer) |
	self;

    Input ? {Request, Ok, Common},
    Request =\= counts(_, _),
    arg(3, Request, Rate),
    Rate = Units(_Value), unknown(Units) :
      write_channel(defer(Units, {time(Request, Ok), Common}), Defer) |
	self;

    Input ? {Request, Ok, Common},
    Request =\= counts(_, _),
    arg(3, Request, Rate),
    Rate = _Units(Value), unknown(Value) :
      write_channel(defer(Value, {time(Request, Ok), Common}), Defer) |
	self;

    Input ? {Request, Ok, Common},
    Request = signals(_, _, Times, _),
    unknown(Times) :
      write_channel(defer(Times, {time(Request, Ok), Common}), Defer) |
	self;

    otherwise,
    Input ? Term :
      In ! Term |
	self;

    Input = [] : Defer = _,
      In = [] .


In ::= [Request] ; [].

Type ::= real ; virtual ; profiling.
Request ::= open(Type);
	    close(Type);
	    set(Type, Rate);
	    signals(Type, Rate, Ticks);
	    signals(Type, Rate, Times, Ticks);
	    ticks(Type, Rate, Resets, Ticks);
	    counts(Type, Counts).
Resets ::= [reset].
Ticks ::= [tick].
Counts ::= [Integer].
Rate ::= weeks(Integer) ; days(Integer) ; hours(Integer) ; minutes(Integer) ;
	 seconds(Integer) ; tenths(Integer) ; hundredths(Integer) ;
	 milliseconds(Integer).

server(In, States) :-

    In ? {open(Type), Ok, Common} |
	timer_type(Type, States, States', OldState, NewState?),
	open_timer,
	self;

    In ? {set(Type, Rate), Ok, Common} |
	timer_type(Type, States, States', OldState, NewState?),
	check_rate,
	set_timer,
	self;

    In ? {close(Type), Ok, Common} |
	timer_type(Type, States, States', State, closed),
	close_timer,
	done,
	self;

    In ? {counts(Type, Counts?), Ok, Common},
    unknown(Common) |
	timer_type(Type, States, States', OldState, NewState?),
	timer_counts,
	self;

    In ? {signals(Type, Rate, Ticks?), Ok, Common},
    unknown(Common) :
      Resets = _,
      Times = 1,
      Decr = 0 |
	timer_type(Type, States, States', OldState, NewState?),
	check_rate,
	signals,
	self;

    In ? {signals(Type, Rate, Times, Ticks?), Ok, Common},
    unknown(Common),
    integer(Times) :
      Resets = _,
      Decr = 1 |
	timer_type(Type, States, States', OldState, NewState?),
	check_rate,
	signals,
	self;

    In ? {ticks(Type, Rate, Resets, Ticks?), Ok, Common},
    unknown(Common),
    read_only(Resets) :
      Times = 1,
      Decr = 1 |
	timer_type(Type, States, States', OldState, NewState?),
	check_rate,
	signals,
	self;

    In ? _Other(Ok, Common),
    unknown(Common),
    otherwise :
      Ok = false(invalid_request) |
	done,
	self;

    In ? _Request(Ok, Common),
    known(Common) :
      Ok = false(aborted) |
	self;

    In = [] :
      States = {Real, Virtual, Profiling} |
	close_timer(Real?, _),
	close_timer(Virtual?, _),
	close_timer(Profiling?, _).

timer_type(real,
	   {Real, Virtual, Profiling}, {Real'?, Virtual, Profiling}^, 
	   Real?, Real'
).
timer_type(virtual,
	   {Real, Virtual, Profiling}, {Real, Virtual'?, Profiling}^, 
	   Virtual?, Virtual'
).
timer_type(profiling,
	   {Real, Virtual, Profiling}, {Real, Virtual, Profiling'?}^, 
	   Profiling?, Profiling'
).


open_timer(Type, OldState, NewState, Ok, Common) :-

    OldState = closed :
      NewState = reset(Commands),
      Ok = true |
	machine_timer # open(Type, Commands?),
	done;

    OldState = reset(_) : Type = _,
      NewState = OldState,
      Ok = false(already) |
	done;

    OldState = set(Commands) : Type = _,
      Commands ! get(_Remnant, Period),
      NewState = set(Commands'),
      Ok = false(already(Period)) |
	done.


set_timer(OldState, NewState, Delay, Ok, Common) :-

    string(Delay) :
      NewState = OldState,
      Ok = false(bad_period) |
	done;

    OldState = closed : Delay = _,
      Ok = false(not_open),
      NewState = OldState |
	done;

    OldState = set(Commands) : Delay = _,
      Commands ! get(_Remnant, Period),
      Ok = false(already(Period)),
      NewState = set(Commands') |
	done;

    Delay > MaxInt,
    OldState = reset(_) :
      NewState = OldState,
      Ok = false(too_long) |
	done;

    Delay =< MaxInt,
    OldState = reset(Commands) :
      Ok = true,
      Commands ! set(Delay, Delay),
      NewState = set(Commands') |
	done.


check_rate(Rate, Delay, Common) :-

    Rate = milliseconds(N), integer(N) > 0,
    Delay^ := N /real(1000) :
      Common = _ ;

    Rate = hundredths(N), integer(N) > 0,
    Delay^ := N /real(100) :
      Common = _ ;

    Rate = tenths(N), integer(N) > 0,
    Delay^ := N /real(10) :
      Common = _ ;

    Rate = seconds(N), integer(N) > 0,
    Delay^ := real(N) :
      Common = _ ;

    Rate = minutes(N), integer(N) > 0,
    Delay^ := real(60) * N :
      Common = _ ;

    Rate = hours(N), integer(N) > 0, 
    Delay^ := real(3600) * N :
      Common = _ ;

    Rate = days(N), integer(N) > 0,
    Delay^ := real(86400) * N :
      Common = _ ;

    Rate = weeks(N), integer(N) > 0,
    Delay^ := real(604800) * N :
      Common = _ ;

    otherwise : Rate = _,
      Delay = illegal_rate,
      Common = _ ;

    known(Common) : Rate = _,
      Delay = aborted .


timer_counts(Counts, OldState, NewState, Ok, Common) :-

    OldState =\= _(_) :
      Ok = false(closed),
      NewState = closed,
      Counts = [] |
	done;

    OldState = Set(Commands) :
      Ok = true,
      Commands ! stream(Counts),
      NewState = Set(Commands') |
	done.


signals(Times, Resets, Decr, Delay, Ticks, OldState, NewState, Ok, Common) :-

    OldState = closed : Times = _, Resets = _, Decr = _, Delay = _, 
      Ok = false(closed),
      NewState = OldState,
      Ticks = [] |
	done;

    OldState = reset(_) : Times = _, Resets = _, Decr = _, Delay = _,
      Ok = false(period_unset),
      NewState = OldState,
      Ticks = [] |
	done;

    number(Delay),
    OldState = set(Commands) :
      Ok = true,
      NewState = set(Commands'),
      Commands = [reset(Remnant, Period), stream(Counts),
		  set(Remnant'?, Period) | Commands'?] |
	remnant(Remnant, Period, Remnant', Delay, Residue),
	serve_signals;

    string(Delay) : Times = _, Resets = _, Decr = _, OldState = _,
      Ok = false(Delay),
      NewState = OldState,
      Ticks = [] |
	done.


remnant(Remnant, Period, Remnant', Delay, Residue) :-

    real(Remnant) > real(0),
    Residue^ := Delay - (Period - Remnant) :
      Remnant' = Remnant;

    real(Remnant) =< real(0) :
      Residue = Delay,
      Remnant' = Period .


serve_signals(Counts, Times, Resets, Decr, Delay, Ticks, Period, Residue, Common) :-

    unknown(Resets),
    unknown(Common),
    Counts ? Count,
    Residue -= Period*Count,
    Residue' =< 0 :
      Ticks ! tick |
	iterate_signals;

    unknown(Resets),
    unknown(Common),
    Counts ? Count,
    Residue -= Period*Count,
    Residue' > 0 |
	self;

    Counts = [] : Times = _, Resets = _, Decr = _, 
		  Delay = _, Period = _, Residue = _,
      Ticks = [] |
	done;

    unknown(Common),
    Resets ? reset,
    Residue' := Delay + Period :
      Residue = _ |
	self;

    Resets =\= [reset | _] : Counts = _, Times = _,
			     Decr = _, Delay = _, Period = _, Residue = _,
      Ticks = [] |
	done;

    known(Common) : Counts = _, Times = _, Resets = _,
		    Decr = _, Delay = _, Period = _, Residue = _,
      Ticks = [] .

iterate_signals(Counts, Times, Resets, Decr, Delay, Ticks, Period, Common) :-

    unknown(Common),
    Times > Decr,
    Times -= Decr :
      Residue = Delay |
	serve_signals;

    otherwise : Counts = _, Times = _, Resets = _, Decr = _, Delay = _, Period = _,
      Ticks = [] |
	done.


close_timer(State, Ok) :-

    State = closed :
      Ok = true ;

    State = _([]^) :
      Ok = true .

done(Common) :-

    var(Common) : Common = done ;

    known(Common) | true.
