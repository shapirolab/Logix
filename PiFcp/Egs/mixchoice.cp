/*
** PiFcp
**
**   Mix(a, b, c) :- 
**      a ! [] | P(c, a, b) ;
**      b ? [] | P(c, b, a) .
**   P(x, y, z) :-
**      x ? [] | z ! {x, y}.
**
** Compound Fcp
*/

-language(compound).

/****  Code for testing the generated program ****/

testout(A, B, C) :-
	pi_utils#make_channel(A, "testout.A", _, _),
	pi_utils#make_channel(B, "testout.B", _, _),
	pi_utils#make_channel(C, "testout.C", _, _),
	mix,
	pi_utils#receive(A, _),
	pi_utils#send([], C).

testin(A, B, C) :-
	pi_utils#make_channel(A, "testout.A", _, _),
	pi_utils#make_channel(B, "testout.B", _, _),
	pi_utils#make_channel(C, "testout.C", _, _),
	mix,
	pi_utils#send([], B),
	pi_utils#send([], C).

/*************************************************/

  mix(A, B, C) :-
    A = _(VA, _) :
      /* Offer a message on channel a. */
      write_channel(Sender?([], 1, Choice), VA) |
        pi_monitor#unique_sender("Mix", Sender),
        "Mix.mixed".

  "Mix.mixed"(A, B, C, Choice, Sender) :-

    /* Test offer accepted. */
    Choice = 1 |
      p(C, A, B);

    /* Skip messages that have already been consumed. */    
    B = NCB(VB, MsB), MsB ? _(_, _, Choice1),
    not_we(Choice1) :
      B' = NCB(VB, MsB') |
        "Mix.mixed";
    /* Ignore any message offered by the associated reduction of Mix. */
    B = NCB(VB, MsB), MsB ? Sender(_, _, _) :
      B' = NCB(VB, MsB') |
        "Mix.mixed";
    /* Consume the independantly offered message. */
    B = NCB(VB, MsB), MsB ? NSB([], N, Choice1),
    we(Choice1),
    NSB =\= Sender :
      Choice1 = N,
      /* Withdraw the offered message. */
      Choice = 0,
      /* And advance past the consumed message. */
      B' = NCB(VB, MsB') |
        p(C, B, A).


  p(X, Y, Z) :-

    X = NCX(VX, MsX), MsX ? _(_, _, Choice),
    not_we(Choice) :
      X' = NCX(VX, MsX') |
        p;
    X = _(_, MsX),
    MsX ? _([], N, Choice),
    we(Choice),
    Z = _(VZ, _) :
      Choice = N,
      write_channel("P"({X, Y}, 1, _), VZ).
