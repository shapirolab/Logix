/*
** PiFcp
**
**   main(A) :- A![].
**
** Compound Fcp
*/
-language(compound).

/****  Code for testing the generated program ****/

test(A) :-
	pi_utils#make_channel(A, "test.A", _, _),
	main.

/*************************************************/

  main(A) :-
    A = _(VA, _) :
      /* A send in the body is not synchronized.
         But it is moved to the guard.           */
      write_channel("main.A"([], 1, _), VA).
