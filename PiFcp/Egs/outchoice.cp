/*
** PiFcp
**
**   Choose(A, B)+(C, D) ::= A![] , B!{C};
**                           B![] , A!{D}.
**
** Compound Fcp
*/

-language(compound).

/****  Code for testing the generated program ****/

testA(A, B) :-
	pi_utils#make_channel(A, "testA.A", _, _),
	pi_utils#make_channel(B, "testA.B", _, _),
	"Choose"(A, B),
	pi_utils#receive(A, _).

testB(A, B) :-
	pi_utils#make_channel(A, "testB.A", _, _),
	pi_utils#make_channel(B, "testB.B", _, _),
	"Choose"(A, B),
	pi_utils#receive(B, _).

/*************************************************/

  "Choose"(A, B) :-
    true :
      make_vector(2, VC, SC), SC = {MsC, _}, store_vector(2, MsC, VC),
      C = "Choose.C"(VC, {0, 0}),
      make_vector(2, VD, SD), SD = {MsD, _}, store_vector(2, MsD, VD),
      D = "Choose.D"(VD, {0, 0}) |
        "Choose.".

  "Choose."(A, B, C, D) :-
    A = _(VA, _),
    B = _(VB, _) :
      /* Offer the messages. */
      write_vector(1, "Choose.a"({C}, 1, Chosen), VA),
      write_vector(1, "Choose.b"({D}, 2, Chosen), VB) |
        "Choose.send".

  "Choose.send"(A, B, C, D, Chosen) :-

    /* Commit to the one which is read. */
    Chosen = 1,
    A = _(VA, _) :
      write_vector(1, "Choose.A"({C}, 1, _), VA);
    Chosen = 2,
    B = _(VB, _) :
      write_vector(1, "Choose.B"({D}, 2, _), VB).
