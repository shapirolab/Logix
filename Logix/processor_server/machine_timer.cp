/*

machine_time monitor.

avshalom houri - 9/88
bill silverman

Last update by		$Author: bill $
			$Date: 1999/07/09 07:03:38 $
Currently locked by	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/processor_server/machine_timer.cp,v $
 
Copyright (C) 1988, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export(open/2).
-language(compound).

procedure open(Type, In).

In ::= [Request] ; [].

Type ::= real ; virtual ; profiling.
Request ::= close ;
	    get(Period, Delay);
	    reset(Period, Delay);
	    set(Period, Delay);
	    stream(Counts).
Delay, Period ::= Number.
Counts ::= [Integer].


procedure open(Type, In).

open(Type, In) :-
	processor # [device(signals(Devices)), link(lookup(timer, Offset))],
	start.

Devices ::= Stream.
Offset ::= Integer.
Signals ::= [signal].

procedure start(Type, In, Devices, Offset).

start(Type, In, Devices, Offset) :-
    string(Type),
    Reset := real(0) :
      execute(Offset, open(Type, Signals)),
      Signals' = Signals?,
      Counts = _ |
	server.

procedure server(Type, In, Devices, Offset, Signals, Period, Counts).

server(Type, In, Devices, Offset, Signals, Reset, Counts) :-

    In ? get(Delay, Period) :
      execute(Offset, get_timer(Type, {{PS, PM}, {DS, DM}})) |
	self,
	Delay := DS + real(DM)/1000000, Period := PS + real(PM)/1000000;

    In ? reset(Delay, Period),
    Reset' := real(0) : Reset = _,
      execute(Offset, get_timer(Type, {{PS, PM}, {DS, DM}})) |
	self,
	Delay := DS + real(DM)/1000000, Period := PS + real(PM)/1000000,
	execute(Offset, set_timer(Type, {{0, 0}, {0, 0}}), Offset');

    In ? set(Delay, Period),
    Delay >=0, Period >= 0,
    DS := integer(Delay),  DM := integer(1000000*(Delay - DS)),
    PS := integer(Period), PM := integer(1000000*(Period - PS)) : Reset = _,
      Reset' = Period,
      execute(Offset, set_timer(Type, {{PS, PM}, {DS, DM}})) |
	self;
				% Counts should start more-or-less
				% in sync with the signals.
    In ? stream(RealOut) :
      RealOut = Counts? |
	self;
				% Stay at end of signal stream.
    Signals ? signal :
      execute(Offset, read_timer(Type, Counts, Counts'?)) |
	self;

    unknown(Signals),
    Devices ? restart,
    Reset' := real(0) :
/* The timer was opened and not closed before the save.  The delay **
** could be recalculated from a sample taken just before the save. */
      execute(Offset, open(Type, Signals')),
      In' = [set(Reset, Reset) | In] |
	self;

    Signals = [] : Type = _, In = _, Devices = _, Offset = _, Reset = _,
      Counts = [] ;

    In = [] : Devices = _, Offset = _, Reset = _, Signals = _,
      Counts = [] |
	close_timer(Offset, Type).


procedure close_timer(Offset, Type).

close_timer(Offset, Type) :-
    true :
      execute(Offset, set_timer(Type, {{0,0}, {0,0}})) |
	execute(Offset, close(Type), _).

procedure execute(Offset, Tuple, Offset).

execute(Offset, Command, Offset^) :-
    true :
      execute(Offset, Command) |
	true.
