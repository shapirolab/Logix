/*
** PiFcp
**
**   Mix(a, b, c) :- 
**      a ! [] | c ! a;
**      b ? [] | c ! b.
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

  "Mix.mixed"(A, B, C, Choice, Sender) :-

    /* Test offer accepted. */
    Choice = 1,

    C = _(VC, _) :

      write_channel("Mix"({B}, 1, _), VC);

    /* Skip messages that have already been consumed. */    
    B = NCB(VB, MsB), MsB ? _(_, _, Choice1),
    not_we(Choice1) :
      B' = NCB(VB, MsB') |
	self;
    /* Ignore any message offered by the associated reduction of Mix. */
    B = NCB(VB, MsB), MsB ? Sender(_, _, _) :
      B' = NCB(VB, MsB') |
        "Mix.mixed";
    /* Consume the independantly offered message. */
    B = NCB(VB, MsB), MsB ? NSB([], N, Choice1),
    we(Choice1),
    NSB =\= Sender,

    C = _(VC, _) :
      Choice1 = N,
      /* Withdraw the offered message. */
      Choice = 0,

      write_channel("Mix"({A}, 1, _), VC).
