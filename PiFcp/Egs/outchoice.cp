/*
** PiFcp
**
**   Choose(a, b)+(c, d) :- a![] | b!{c};
**                          b![] | a!{d}.
**
** Compound Fcp
*/

-language(compound).

/****  Code for testing the generated program ****/

testA(A, B) :-
	pi_utils#make_channel(A, "testA.A", _, _),
	pi_utils#make_channel(B, "testA.B", _, _),
	choose(A, B),
	pi_utils#receive(A, _).

testB(A, B) :-
	pi_utils#make_channel(A, "testB.A", _, _),
	pi_utils#make_channel(B, "testB.B", _, _),
	choose(A, B),
	pi_utils#receive(B, _).

/*************************************************/

  choose(A, B) :-
    true :
      make_vector(2, VC, SC), SC = {MsC, _}, store_vector(2, MsC, VC),
      C = "Choose.c"(VC, {0, 0}),
      make_vector(2, VD, SD), SD = {MsD, _}, store_vector(2, MsD, VD),
      D = "Choose.d"(VD, {0, 0}) |
        "Choose.".

  "Choose."(A, B, C, D) :-
    A = _(VA, _),
    B = _(VB, _) :
      /* Offer the messages. */
      write_vector(1, "Choose.a"({C}, 1, Choice), VA),
      write_vector(1, "Choose.b"({D}, 2, Choice), VB) |
        "Choose.send".

  "Choose.send"(A, B, C, D, Choice) :-

    /* Commit to the one which is read. */
    Choice = 1,
    A = _(VA, _) :
      write_vector(1, "Choose.a"({C}, 1, _), VA);
    Choice = 2,
    B = _(VB, _) :
      write_vector(1, "Choose.b"({D}, 2, _), VB).
