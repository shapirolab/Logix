/*
** PiFcp
**
**   make(a) :- screen#display(`a, [known(`a)]).
**
** Compound Fcp
*/

-language(compound).

  make+(A) :-
    true :
      make_channel(VA, MsA), A = "pichannel.a"(VA, MsA) |
	screen#display(A, [known(A)]).
