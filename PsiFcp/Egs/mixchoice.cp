-language([evaluate, compound, colon]).
-export(["Mix"/2, testin/2, testout/2]).
-include(psi_constants).

/*
** PiFcp
**
**   Mix(a, b) ::=
**      a ! [] , psi_utils#SPC(a)
**      b ? [] , psi_utils#SPC(b).
**
*/

/*
** Compound Fcp
*/

  "Mix"("_var"(a), "_var"(b)) :-
	psi_monitor#scheduler(Scheduler),
	".Mix".

  ".Mix"("_var"(a), "_var"(b), Scheduler) :-
    vector("_var"(a)),
    vector("_var"(b)) :
      /* Offer a send on channel a and a receive on channel b. */
      write_channel(start("Mix", [{PSI_SEND, a, "_var"(a), 1, 1},
				  {PSI_RECEIVE, b, "_var"(b), 1, 2}],
					Value, Chosen), Scheduler) |
        "Mix.comm".

  "Mix.comm"("_var"(a), "_var"(b), Scheduler, Chosen, Value) :-

    /* Test send offer accepted. */
    Chosen = 1 :
      Value = [],
      write_channel(close({"_var"(b)}), Scheduler) |
	psi_utils#"SPC"("_var"(a));	

    /* Test receive offer accepted. */
    Chosen = 2,
    Value = [] :
      write_channel(close({"_var"(a)}), Scheduler) |
	psi_utils#"SPC"("_var"(b)).

/****  Code for testing the generated program ****/

BASERATE => infinite.

testout(A, B) :-
	psi_utils#make_channel(A, "testout.a", BASERATE),
	psi_utils#make_channel(B, "testout.b", BASERATE),
	"Mix"(A,B),
	psi_utils#receive(A, _).

testin(A, B) :-
	psi_utils#make_channel(A, "testout.a", BASERATE),
	psi_utils#make_channel(B, "testout.b", BASERATE),
	"Mix"(A,B),
	psi_utils#send([], B).
