-language(compound).
-mode(interrupt).
-export(until/2).

/* until(Then, Reply)
**
** Wait until the monitor's internal time (Now) passes Then:
** set Reply = "true".
**
** If internal time is reset, indicated by the end of the debug stream:
** set Reply = "false".
**
** Track internal time from the monitor's debug stream.
**
*/

until(Then, Reply) :-
	spi_status#debug(S),
	wait_until.

  wait_until(S, Then, Reply) :-

    S ? Done, arg(1, Done, done),
    arg(2, Done, Now), Then =< Now :
      S' = _,
      Reply = true;

    S ? _,
    otherwise |
	self;

    S =?= [] :
      Then = _,
      Reply = false.
