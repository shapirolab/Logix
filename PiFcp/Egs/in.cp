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
    A = _(VA, _),
    read_vector(2, VA, MsA) |
	"P.receive"(A,B,MsA).

  "P.receive"(A, B, MsA) :-
    /* Cdr down the stream to find an unconsumed message. */
    MsA ? _(_, _, Choice),
    not_we(Choice) |
        self;
    MsA ? _([], N, Choice),
    we(Choice),
    A = _(VA, _) :

      /* Consume the message by unifying the choice_tag with the choice. */
      Choice = N,

      /* Update channel A. */
      store_vector(2, MsA', VA) |

	pi_send("P.b", [], B).
