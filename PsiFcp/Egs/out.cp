-language([evaluate, compound, colon]).
-export(["Main"/1, test/1]).

BASERATE => infinite.

/*
** PiFcp
**
**   Main(a) ::= a![], psi_utils#SPC(A).
**
*/

/*
** Compound Fcp
*/

  "Main"("_var"(a)) :-
    "_var"(a) = _(VA, _) :
      write_channel(send("Main"(a), [], 1, 1, Chosen), VA) |
	"Main.send".

  "Main.send"("_var"(a), Chosen) :-
    Chosen = 1 |
	psi_utils#"SPC"("_var"(a)).


/****  Code for testing the generated program ****/

test(A) :-
	psi_utils#make_channel(A, "test.A", BASERATE),
	"Main"(A),
	psi_utils#receive(A, _).
