/* $Header: /home/qiana/Repository/Logix/system/decode/self.cp,v 1.1 1999/07/09 07:03:31 bill Exp $ */
-mode(trust).
-language(compound).
-export([asm/2, asm/3]).

/* * * * * * * * * * * * * * * * * * A S M * * * * * * * * * * * * * * * * * */

StringRefs ::= [String(Integer)].
RQs ::= server # RQs .

procedure asm(Items, [Any]).
procedure asm(Items, [Any], StringRefs).

asm(Input, Pretty) + (StringDefs = _) :-

	output # server(RQs, Pretty, StringRefs),
	ordered_merger(Merger1),
	ordered_merger(Merger2),
	utils # [binary_sort_merge(StringRefs, SortedSRs, Merger1),
		 binary_merge([StringDefs, SortedSRs], _, Merger2)],
	items # lines(Input, RQs, StringDefs).

/*************************** A U X I L I A R Y *******************************/

ordered_merger(In) :-

    In ? ordered_merge(In1, In2, Out) |
	merge_keys(In1, In2, Out),
	ordered_merger;

    In = [] | true.

merge_keys(In1, In2, Out) :-

    In1 ? I1, In2 = [_(N2) | _],
    I1 = _(N1),
    N1 < N2 :
      Out ! I1 |
	merge_keys;

    In1 = [_(N1) | _], In2 ? I2,
    I2 = _(N2),
    N2 < N1 :
      Out ! I2 |
	merge_keys;

    In1 = [S1(N) | _], In2 ? S2(N) :
      S1 = S2 |
	merge_keys;

    In1 = [] :
      In2 = Out ;

    In2 = [] :
      In1 = Out .
