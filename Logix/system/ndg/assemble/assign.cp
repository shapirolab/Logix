/* $Header: /home/qiana/Repository/Logix/system/ndg/assemble/assign.cp,v 1.1 1999/07/09 07:02:58 bill Exp $ */
-export([multiple_copy/6, assignment/6, multiple_assignment/7]).
-mode(trust).
-include(dg_opcodes).
-include(registers).
-language([evaluate,compound,colon]).

Copy_R => FCP_dg_copy_Rs_Rd.
Copy_P => FCP_dg_copy_CpIs_Rd.
Copy_I => FCP_dg_copy_SRs_Rd.
Asgn_I => FCP_dg_asgn_Rs_SRd.
Assign_I => FCP_dg_assign_Rs_SRd.
Assign_Com => FCP_dg_assign_com_Rs_SRd.
Assign_ComTr => FCP_dg_assign_com_tr_Rs_SRd.

/* * * * * * * * * * * * * * * M U L T I P L E  * * * * * * * * * * * * * * */

procedure multiple_copy(FromTerms, ToRegisters, Requests, Requests,
			SItems, Items
).

multiple_copy(Source, Destination, Requests1, Requests2, Items1, Items2) :-
	copies(Source, Destination, Copies),
	combine(Copies, Multiples),
	copy_multiples(Multiples, Requests1, Requests2, Items1, Items2).

copies(Source, Destination, Copies) :-

    Source ? S, Destination ? D :
      Copies ! Mnemonic(Items, EndItems, Requests, EndRequests,
				Args, K, OpCode
		    ) |
	copies,
	copy(S, D,
	     [instruction(Mnemonic, Args, K) | Requests], EndRequests,
	     [OpCode | Items], EndItems
	);

    Source = [], Destination = [] :
      Copies = [] .

combine(Copies, Multiples) :-

    Copies ? Mnemonic(Items, EndItems, Requests, EndRequests,
				Assign, K, OpCode
		  ) :
      Multiples ! Mnemonic([OpCode | Items], EndItems',
			   Requests, EndRequests',
			   [Assign | Assigns'], K, N
		  ) |
	sequential(Mnemonic, Copies', Copies'',
		   EndItems, EndItems', EndRequests, EndRequests',
		   Assigns', N
	),
	combine;

    Copies = [] :
      Multiples = [] .

sequential(Mnemonic, As1, As2, Is1, Is2, Rs1, Rs2, As, N) + (M = 1) :-

    As1 ? Mnemonic(Items, Is1', Requests, Rs1',
			Assign, _, _
	  ),
    M' := M + 1 :
      Is1 = Items, Rs1 = Requests,
      As ! Assign |
	sequential;

    otherwise : Mnemonic = _,
      As1 = As2,
      Is1 = Is2, Rs1 = Rs2,
      As = [],
      N = M .

copy_multiples(Multiples, Requests1, Requests2, Items1, Items2) :-

    Multiples ? Mnemonic(Items, Items1', Requests, Requests1', Args, K, N),
    N > 1,
    K' := 3 + N*K :	% K =\= 0 iff all pairs consist of K bytes
      Items1 = [FCP_dg_mult_operation, N | Items],
      Requests1 = [instruction(multiple, (N, Mnemonic, Args), K')
		  | Requests] |
	copy_multiples;

    Multiples ? Mnemonic(Items, Items1', Requests, Requests1', [Assign], K, 1),
    K' := K + 1 :
      Items = Items1,
      Requests1 = [instruction(Mnemonic, Assign, K') | Requests] |
	copy_multiples;

    Multiples = [] :
      Items1 = Items2,
      Requests1 = Requests2 .


copy(Source, Destination, Requests1, Requests2, Items1, Items2) :-

    Destination = a(J) |
	source_operand(Source, Copy_R, Requests1, Requests2, Items1,
		[RegE(J) | Items2], copy, r, J, 0, 0
	);

    Destination = "PR"(J) |
	source_operand(Source, Copy_P, Requests1, Requests2, Items1,
		[PRegE(J) | Items2], copy, p, J, 0, 0
	);

    Destination = '*'(a(J)) |
	source_operand(Source, Copy_I, Requests1, Requests2, Items1,
		[RegE(J) | Items2], copy, i, J, 0, 0
	).


procedure multiple_assignment(FromTerms, ToRegArgs, Requests, Request,
				SItems, Items, Kind).

multiple_assignment(Source, Destination, Requests1, Requests2,
	                                               Items1, Items2, Kind) :-
	assigns(Source, Destination, Assigns, Kind),
	combine(Assigns, Multiples),
	copy_multiples(Multiples, Requests1, Requests2, Items1, Items2).

assigns(Source, Destination, Assigns, Kind) :-

    Source ? S, Destination ? D :
      Assigns ! Mnemonic(Items, EndItems, Requests, EndRequests,
				Args, K, OpCode
		    ) |
	assigns,
	assign(Kind, S, D,
	     [instruction(Mnemonic, Args, K) | Requests], EndRequests,
	     [OpCode | Items], EndItems
	);

    Source = [], Destination = [] :
      Kind = _,
      Assigns = [].

assign(Kind, Source, Destination, Requests1, Requests2, Items1, Items2) :-

    Kind = assign,
    Destination = a(J) |
	source_operand(Source, Assign_I, Requests1, Requests2, Items1,
		[RegE(J) | Items2], assign, '', J, 0, 1);

    Kind = assign_and_commit,
    Destination = a(J) |
	source_operand(Source, Assign_Com, Requests1, Requests2, Items1,
		[RegE(J) | Items2], assign_com, '', J, 0, 1);

    Kind = assign_and_commit_trail,
    Destination = a(J) |
	source_operand(Source, Assign_ComTr, Requests1, Requests2, Items1,
		[RegE(J) | Items2], assign_com_tr, '', J, 0, 1).

procedure assignment(AssignValue, Register, Requests, Requests, SItems, Items).

assignment(Source, J, Requests1, Requests2, Items1, Items2) :-
	source_operand(Source, Asgn_I,
			Requests1, Requests2, Items1,
			[RegE(J) | Items2], assignment, '', J, 1, 1
	).

/* * * * * * * * * * * * * * * O P E R A N D * * * * * * * * * * * * * * * * */

source_operand(Source, OpCode, Requests1, Requests2, Items1, Items2,
		Prefix, Suffix, J, P, Op
) :-

    Source = a(I),
    string_to_dlist(Prefix, PrefixR, [AR | SD]),
    string_to_dlist(Suffix, SD, []),
    P' := P + 2 : Op = _,
      Items1 = [OpCode, RegE(I) | Items2],
      Requests1 = [instruction(Mnemonic, (I = J), P') | Requests2],
      ascii(r, AR) |
	list_to_string(PrefixR, Mnemonic);

    Source = "PR"(I),
    string_to_dlist(Prefix, PrefixP, [AP | SD]),
    string_to_dlist(Suffix, SD, []),
    P' := P + 2,
    OpCode' := OpCode + 3 - 2*Op :
      Items1 = [OpCode', PRegE(I) | Items2],
      Requests1 = [instruction(Mnemonic, (I = J), P') | Requests2],
      ascii(p, AP) |
	list_to_string(PrefixP, Mnemonic);

    Source = "*"(a(I)),
    string_to_dlist(Prefix, PrefixI, [AI | SD]),
    string_to_dlist(Suffix, SD, []),
    P' := P + 2,
    OpCode' := OpCode + 6 - 4*Op :
      Items1 = [OpCode', RegE(I) | Items2],
      Requests1 = [instruction(Mnemonic, (I = J), P') | Requests2],
      ascii(i, AI) |
	list_to_string(PrefixI, Mnemonic);

    Source = {a(I), IX},
    string_to_dlist(Prefix, PrefixX, [AX | SD]),
    string_to_dlist(Suffix, SD, []),
    P' := P + 3,
    OpCode' := OpCode + 9 - 6*Op :
      Items1 = [OpCode', RegE(I), IndexE(IX) | Items2],
      Requests1 = [instruction(Mnemonic, ((I, IX) = J), P') | Requests2],
      ascii(x, AX) |
	list_to_string(PrefixX, Mnemonic);

    Source = ro(a(I)),
    string_to_dlist(Prefix, PrefixQ, [AQ | SD]),
    string_to_dlist(Suffix, SD, []),
    P' := P + 2,
    OpCode' := OpCode + 12 - 8*Op :
      Items1 = [OpCode', RegE(I) | Items2],
      Requests1 = [instruction(Mnemonic, (I = J), P') | Requests2],
      ascii(q, AQ) |
	list_to_string(PrefixQ, Mnemonic);

    Source = car(a(I)),
    string_to_dlist(Prefix, PrefixC, [AC | SD]),
    string_to_dlist(Suffix, SD, []),
    P' := P + 2,
    OpCode' := OpCode + 15 - 10*Op :
      Items1 = [OpCode', RegE(I) | Items2],
      Requests1 = [instruction(Mnemonic, (I = J), P') | Requests2],
      ascii(c, AC) |
	list_to_string(PrefixC, Mnemonic);

    Source = "&"({a(I), IX}),
    string_to_dlist(Prefix, PrefixA, [AA | SD]),
    string_to_dlist(Suffix, SD, []),
    P' := P + 3,
    OpCode' := OpCode + 18 - 12*Op :
      Items1 = [OpCode', RegE(I), IndexE(IX) | Items2],
      Requests1 = [instruction(Mnemonic, ((I, IX) = J), P') | Requests2],
      ascii(a, AA) |
	list_to_string(PrefixA, Mnemonic);

    Source = "&"(ro({a(I), IX})),
    string_to_dlist(Prefix, PrefixB, [AB | SD]),
    string_to_dlist(Suffix, SD, []),
    P' := P + 3,
    OpCode' := OpCode + 21 - 14*Op :
      Items1 = [OpCode', RegE(I), IndexE(IX) | Items2],
      Requests1 = [instruction(Mnemonic, ((I, IX) = J), P') | Requests2],
      ascii(b, AB) |
	list_to_string(PrefixB, Mnemonic);

    Source = ro("&"({a(I), IX})),
    string_to_dlist(Prefix, PrefixB, [AB | SD]),
    string_to_dlist(Suffix, SD, []),
    P' := P + 3,
    OpCode' := OpCode + 21 - 14*Op :
      Items1 = [OpCode', RegE(I), IndexE(IX) | Items2],
      Requests1 = [instruction(Mnemonic, ((I, IX) = J), P') | Requests2],
      ascii(b, AB) |
	list_to_string(PrefixB, Mnemonic);

    Source = [],
    string_to_dlist(Prefix, PrefixN, [AN | SD]),
    string_to_dlist(Suffix, SD, []),
    P' := P + 1,
    OpCode' := OpCode + 24 - 16*Op :
      Items1 = [OpCode' | Items2],
      Requests1 = [instruction(Mnemonic, J, P') | Requests2],
      ascii(n, AN) |
	list_to_string(PrefixN, Mnemonic);

    integer(Source),
    string_to_dlist(Prefix, PrefixW, [AW | SD]),
    string_to_dlist(Suffix, SD, []),
    OpCode' := OpCode + 27 - 18*Op :
      Items1 = [OpCode', Escape | Items2],
      Data = integer(Source),
      Requests1 = [instruction(Mnemonic, (Data = J), P),
		   data_escape(Data, Escape), increment(1)
		  | Requests2],
      ascii(w, AW) |
	list_to_string(PrefixW, Mnemonic);

    Source = integer(_),
    string_to_dlist(Prefix, PrefixW, [AW | SD]),
    string_to_dlist(Suffix, SD, []),
    OpCode' := OpCode + 27 - 18*Op :
      Items1 = [OpCode', Escape | Items2],
      Requests1 = [instruction(Mnemonic, (Source = J), P),
		   data_escape(Source, Escape), increment(1)
		  | Requests2],
      ascii(w, AW) |
	list_to_string(PrefixW, Mnemonic);

    Source = tuple(_),
    string_to_dlist(Prefix, PrefixW, [AW | SD]),
    string_to_dlist(Suffix, SD, []),
    OpCode' := OpCode + 27 - 18*Op :
      Items1 = [OpCode', Escape | Items2],
      Requests1 = [instruction(Mnemonic, (Source = J), P),
		   data_escape(Source, Escape), increment(1)
		  | Requests2],
      ascii(w, AW) |
	list_to_string(PrefixW, Mnemonic);

    real(Source),
    string_to_dlist(Prefix, PrefixF, [AF | SD]),
    string_to_dlist(Suffix, SD, []),
    OpCode' := OpCode + 30 - 20*Op :
      Items1 = [OpCode', Escape | Items2],
      Data = real(Source),
      Requests1 = [instruction(Mnemonic, (Data = J), P),
		   data_escape(Data, Escape), increment(1)
		  | Requests2],
      ascii(f, AF) |
	list_to_string(PrefixF, Mnemonic);

    string(Source),
    string_to_dlist(Prefix, PrefixS, [AS | SD]),
    string_to_dlist(Suffix, SD, []),
    OpCode' := OpCode + 33 - 22*Op :
      Items1 = [OpCode', Reference | Items2],
      Requests1 = [instruction(Mnemonic, (string(Source) = J), P),
		   string_reference(Source, Reference), increment(1)
		  | Requests2],
      ascii(s, AS) |
	list_to_string(PrefixS, Mnemonic);

    Source = {"_"},
    string_to_dlist(Prefix, PrefixC, [AV | SD]),
    string_to_dlist(Suffix, SD, []),
    P' := P + 1,
    OpCode' := OpCode + 36 - 24*Op :
      Items1 = [OpCode' | Items2],
      Requests1 = [instruction(Mnemonic, J, P') | Requests2],
      ascii(v, AV) |
	list_to_string(PrefixC, Mnemonic).
