/*
** PiFcp
**
**   Main(A) ::= A![].
**
** Compound Fcp
*/
-language(compound).

/****  Code for testing the generated program ****/

test(A) :-
	pi_utils#make_channel(A, "test.A"),
	"Main".

/*************************************************/

  "Main"(A) :-
	/* A send in the body is not synchronized. */
	pi_send("Main.A", [], A).
