-language([evaluate, compound, colon]).
-export([testa/3, testb/3, "Choose"/2]).

BASERATE => infinite.

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

  "Choose."("_var"(a), "_var"(b), "_var"(c), "_var"(d)) :-
    "_var"(a) = _(VA, _),
    "_var"(b) = _(VB, _) :
      /* Offer the messages. */
      write_channel(send("Choose"(a), {"_var"(c)}, 1, 1, Chosen), VA),
      write_channel(send("Choose"(b), {"_var"(d)}, 2, 1, Chosen), VB) |
        "Choose.send".

  "Choose.send"("_var"(a), "_var"(b), "_var"(c), "_var"(d), Chosen) :-

    /* Commit to the one which is read. */
    Chosen = 1,
    "_var"(a) = _(_, {LA, RA}),
    "_var"(b) = _(VB, {LB, RB}),
    "_var"(d) = _(_, {LD, RD}) :
      write_channel(withdraw(send, 1), VB),
      LA = RA,
      LB = RB,
      LD = RD ;
    Chosen = 2,
    "_var"(a) = _(VA, {LA, RA}),
    "_var"(b) = _(_, {LB, RB}),
    "_var"(c) = _(_, {LC, RC}) :
      write_channel(withdraw(send, 1), VA),
      LA = RA,
      LB = RB,
      LC = RC.


/****  Code for testing the generated program ****/

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
