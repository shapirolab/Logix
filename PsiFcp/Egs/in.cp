-language([evaluate,compound,colon]).
-export([test/2, "P"/2]).
-include(psi_constants).

/*
** PsiFcp
**
**  P(a,B) ::= a?[] , {B = received}.
**
*/

/*
** Generated Compound Fcp
*/

  "P"("_var"(a), B) :-
	psi_monitor#scheduler(Scheduler),
	".P".


  ".P"("_var"(a), B, Scheduler) :-
    vector("_var"(a)) :
      write_channel(start("P", [{PSI_RECEIVE, a, "_var"(a), 1, 1}],
				Value, Chosen), Scheduler) |
	"P.comm".

  "P.comm"("_var"(a), B, Scheduler, Chosen, Value) :-
    Chosen = 1,
    Value = [] :
      write_channel(close({"_var"(a)}), Scheduler) |
      B = received.

/****  Code for testing the generated program ****/

BASERATE => infinite.

test(A, B) :-
	psi_utils#make_channel(A, "test.a", BASERATE),
	psi_utils#send([],A),
	"P"(A, B).
