/*
** PiFcp
**
**  P(a,b) :- a?[] | b![].
**
**Compound Fcp
*/
-language(compound).

/****  Code for testing the generated program ****/

test(A, B) + (N = 1) :-
	pi_utils#make_channel(A, "test.A", _, _),
	pi_utils#make_channel(B, "test.B", _, _),
	sends(A, B, N).

sends(A, B, N) + (Counter = 0, HoldA = A):-
  Counter++ < N |
	pi_utils#send([], A),
	pi_utils#receive(A, _),
	self;
  Counter >= N |
	pi_utils#send([], A),
	p(HoldA, B).

/*************************************************/

  p(A, B) :-

    /* Cdr down the stream to find an unconsumed message. */
    A = NCA(VA, MsA),
    MsA ? _(_, _, Choice),
    not_we(Choice) :
      A' = NCA(VA, MsA') |
        p;
    A = _(_, MsA),
    B = _(VB, _), channel(VB),
    MsA ? _([], N, Choice),
    we(Choice) :
      /* Consume the message by unifying the choice_tag with the choice. */
      Choice = N |
	write_channel(p([], 1, _), VB).
