/*
** PiFcp
**
**   make(a) :- screen#display(`a, [known(`a)]).
**
** Compound Fcp
*/

-language(compound).

  make(A) :-
    true :
      make_vector(2, VA, Streams),
      Streams = {MsA, _},
      store_vector(2, MsA, VA),
      A = "pichannel.a"(VA, {0, 0}) |
	screen#display(A, [known(A)]).
