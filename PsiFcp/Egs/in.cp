-language([evaluate,compound,colon]).
-export([test/2, test/3, "P"/2]).

/*
** PsiFcp
**
**  P(a,B) ::= a?[] , {B = received}.
**
*/

/*
** Generated Compound Fcp
*/

  "P"("_var"(a),B) :-
    "_var"(a) = _(VA, _) :
      write_channel(receive("P"(a), MsA, 1, 1, Chosen), VA) |
	"P.receive".

  "P.receive"("_var"(a), B, Chosen, MsA) :-
    Chosen = 1,
    "_var"(a) = _(_, {LA, RA}) :
      MsA = [],
      LA = RA |
	B = received.

/****  Code for testing the generated program ****/

BASERATE => infinite.

test(A, B) + (N = 1) :-
	psi_utils#make_channel(A, "test.a", BASERATE),
	sends(A, B, N).

sends(A, B, N) + (Counter = 0, HoldA = A):-
  Counter++ < N |
	psi_utils#send([], A),
	psi_utils#receive(A, _),
	self;
  Counter >= N |
	psi_utils#send([], A),
	"P"(HoldA, B).
