/*
** PiFcp
**
**   Mix(A, B, C) ::=
**      A ! [] , C ! {A};
**      B ? [] , C ! {B}.
**
** Compound Fcp
*/

-language(compound).

/****  Code for testing the generated program ****/

testout(A, B, C) :-
	pi_utils#make_channel(A, "testout.A", _, _),
	pi_utils#make_channel(B, "testout.B", _, _),
	pi_utils#make_channel(C, "testout.C", _, _),
	"Mix",
	pi_utils#receive(A, _).

testin(A, B, C) :-
	pi_utils#make_channel(A, "testout.A", _, _),
	pi_utils#make_channel(B, "testout.B", _, _),
	pi_utils#make_channel(C, "testout.C", _, _),
	"Mix",
	pi_utils#send([], B).

/*************************************************/

  "Mix"(A, B, C) :-
    A = _(VA, _),
    /* Get the receive stream of B. */
    B = _(VB, _),
    read_vector(2, VB, MsB) :
      /* Offer a message on channel A. */
      write_vector(1, Sender?([], 1, Chosen), VA) |
        pi_monitor#unique_sender("Mix", Sender),
        "Mix.mixed".

  "Mix.mixed"(A, B, C, Chosen, Sender, MsB) :-

    /* Test offer accepted. */
    Chosen = 1 |
        pi_send("Mix.c", {B}, C);

    /* Skip messages that have already been consumed. */
    
    MsB ? _(_, _, Choice),
    not_we(Choice) |
	self;
    /* Ignore any message offered by the associated reduction of Mix. */
    MsB ? Sender(_, _, _) |
        self;
    /* Consume the independantly offered message. */
    MsB ? MsgSender([], N, Choice),
    we(Choice),
    MsgSender =\= Sender,
    B = _(VB, _) :
      /* Update Channel B. */
      store_vector(2, MsB', VB),
      Choice = N,
      /* Withdraw the offered message. */
      Chosen = 0 |

        pi_send("Mix.C", {A}, C).
