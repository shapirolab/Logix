/*
** PiFcp
**
**   Make(R)+a ::= psi_utils#SPC(a).
**
*/

-language([evaluate, compound, colon]).
-export("Make"/0).

BASERATE => infinite.

/*
** Compound Fcp
*/

  "Make" :-
	psi_monitor#scheduler(Scheduler),
	".Make".

   ".Make"(Scheduler) :-
    true :
      write_channel(new_channel("Make.a", "_var"(a), BASERATE), Scheduler) |
	psi_utils#"SPC"("_var"(a)).
