-language([evaluate, compound, colon]).
-export(["Mix"/2, testin/2, testout/2]).

BASERATE => infinite.

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
    "_var"(a) = _(VA, _),
    "_var"(b) = _(VB, _) :
      /* Offer a send on channel A. */
      write_channel(send("Mix"(a), [], 1, 1, Chosen), VA),
      /* Offer a receive on channel B. */
      write_channel(receive("Mix"(b), MsB, 2, 1, Chosen), VB) |
        "Mix.mixed".

  "Mix.mixed"("_var"(a), "_var"(b), Chosen, MsB) :-
    /* Test send offer accepted. */
    Chosen = 1,
    "_var"(b) = _(VB, {LB, RB}) :
      /* Withdraw the offered receive. */
      write_channel(withdraw(receive, 1), VB),
      /* Close the circuit segment. */
      LB = RB |
	psi_utils#"SPC"("_var"(a));	

    /* Test receive offer accepted. */
    Chosen = 2,
    "_var"(a) = _(VA, {LA, RA}) :
      MsB = [],
      /* Withdraw the offered send. */
      write_channel(withdraw(send, 1), VA),
      /* Close the circuit segment. */
      LA = RA |
	psi_utils#"SPC"("_var"(b)).

/****  Code for testing the generated program ****/

testout(A, B) :-
	psi_utils#make_channel(A, "testout.a"),
	psi_utils#make_channel(B, "testout.b"),
	"Mix"(A,B),
	psi_utils#receive(A, _).

testin(A, B) :-
	psi_utils#make_channel(A, "testout.a", BASERATE),
	psi_utils#make_channel(B, "testout.b", BASERATE),
	"Mix"(A,B),
	psi_utils#send([], B).
