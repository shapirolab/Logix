-language([evaluate, compound, colon]).
-export([testa/3, testb/3, "Choose"/2]).
-include(psi_constants).

/*
** PiFcp
**
**   Choose(A, B)+(C, D) ::= A!{C}, 0;
**                           B!{D}, 0;
**
*/

/*
** Compound Fcp
*/

  "Choose"("_var"(a), "_var"(b)) :-
	psi_monitor#scheduler(Scheduler),
	".Choose".

  ".Choose"("_var"(a),"_var"(b), Scheduler) :-
    true :
      write_channel(new_channel("Choose.c", "_var"(c), BASERATE), Scheduler),
      write_channel(new_channel("Choose.d", "_var"(d), BASERATE), Scheduler) |
        "Choose.".

  "Choose."("_var"(a), "_var"(b), "_var"(c), "_var"(d), Scheduler) :-
    vector("_var"(a)),
    vector("_var"(b)) :
      /* Offer the sends. */
      write_channel(start("Choose", [{PSI_SEND, a, "_var"(a), 1, 1},
				     {PSI_SEND, b, "_var"(b), 1, 2}],
						Value, Chosen), Scheduler) |
        "Choose.comm".

  "Choose.comm"("_var"(a), "_var"(b), "_var"(c), "_var"(d), Scheduler,
				Chosen, Value) :-

    /* Commit to the one which is read. */
    Chosen = 1 :
      Value = {"_var"(c)},
      write_channel(close({"_var"(a), "_var"(b), "_var"(d)}), Scheduler);

    Chosen = 2 :
      Value = {"_var"(d)},
      write_channel(close({"_var"(a), "_var"(b), "_var"(c)}), Scheduler).


/****  Code for testing the generated program ****/

BASERATE => infinite.

testa(A, B, Ms) :-
	make_channels_and_go,
	psi_utils#receive(A, Ms).

testb(A, B, Ms) :-
	make_channels_and_go,
	psi_utils#receive(B, Ms).

make_channels_and_go(A, B) :-
	psi_utils#make_channel(A, "testb.a", BASERATE),
	psi_utils#make_channel(B, "testb.b", BASERATE),
	"Choose"(A, B).
