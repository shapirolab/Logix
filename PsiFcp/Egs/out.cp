-language([evaluate, compound, colon]).
-export(["Main"/1, test/1]).
-include(psi_constants).

/*
** PiFcp
**
**   Main(a) ::= a![], psi_utils#SPC(a).
**
*/

/*
** Compound Fcp
*/

  "Main"("_var"(a)) :-
	psi_monitor#scheduler(Scheduler),
	".Main".

  ".Main"("_var"(a), Scheduler) :-
    vector("_var"(a)) :
      write_channel(start("Main",[{PSI_SEND, a, "_var"(a), 1, 1}],
				Value, Chosen), Scheduler) | 
	"Main.comm".

  "Main.comm"("_var"(a), Chosen, Value) :-
    Chosen = 1 :
      Value = [] |
	psi_utils#"SPC"("_var"(a)).


/****  Code for testing the generated program ****/

BASERATE => infinite.

test(A) :-
	psi_utils#make_channel(A, "test.A", BASERATE),
	"Main"(A),
	psi_utils#receive(A, _).
