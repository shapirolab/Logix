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
      make_channel(VC, MsC), C = "Choose.c"(VC, MsC),
      make_channel(VD, MsD), D = "Choose.d"(VD, MsD) |
        "Choose.".

  "Choose."(A, B, C, D) :-
    A = _(VA, _),
    B = _(VB, _) :
      /* Offer the messages. */
      write_channel("Choose."({C}, 1, Choice), VA),
      write_channel("Choose."({D}, 2, Choice), VB) |
        "Choose.choice".

  "Choose.choice"(A, B, C, D, Choice) :-

    /* Commit to the one which is read. */
    Choice = 1,
    A = _(VA, _) :
      write_channel("Choose.b"({C}, 1, _), VA);
    Choice = 2,
    B = _(VB, _) :
      write_channel("Choose.a"({D}, 2, _), VB).
