/*
** PiFcp
**
**   Mix(a, b, c) :- 
**      a ! [] | c ! {a};
**      b ? [] | c ! {b}.
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
    A = _(VA, _) :
      /* Offer a message on channel a. */
      write_channel(Sender?([], 1, Choice), VA) |
        pi_monitor#unique_sender("Mix", Sender),
        "Mix.mixed".

  "Mix.mixed"(A, B, C, Chosen, Sender) :-

    /* Test offer accepted. */
    Choice = 1 |

        pi_send("Mix.c"({B}, 1, _), VC);

    /* Skip messages that have already been consumed. */    
    B = NCB(VB, MsB), MsB ? _(_, _, Choice),
    not_we(Choice) :
      B' = NCB(VB, MsB') |
	self;
    /* Ignore any message offered by the associated reduction of Mix. */
    B = NCB(VB, MsB), MsB ? Sender(_, _, _) :
      B' = NCB(VB, MsB') |
        "Mix.mixed";
    /* Consume the independantly offered message. */
    B = NCB(VB, MsB), MsB ? NSB([], N, Choice),
    we(Choice),
    NSB =\= Sender :

      Choice = N,
      /* Withdraw the offered message. */
      Chosen = 0 |

        pi_send("Mix.c", {A}, C).
