/*
** PiFcp
**
**   Make+A ::= pi_utils#SPC(A).
**
** Compound Fcp
*/

-language(compound).

  "Make" :-
    true :
      make_vector(2, VA, Streams),
      Streams = {MsA, _},
      store_vector(2, MsA, VA),
      A = "Make.A"(VA, {0, 0}) |
	"Make.".

  "Make." :-
	pi_utils#SPC(A).
