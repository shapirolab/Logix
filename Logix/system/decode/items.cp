/* $Header: /home/qiana/Repository/Logix/system/decode/items.cp,v 1.1.1.1 1999/07/09 07:03:32 bill Exp $ */
-language([evaluate,compound,colon]).
-export([lines/3]).
-include(dg_opcodes).
-include(escapes).
-mode(trust).

procedure lines(Items, RQs, StringDefs).

lines(Items, RQs, StringDefs) :-

    Items ? T, tuple(T) :
      RQs ! escape(T) |
	lines;

    Items ? C, integer(C) |
	instruction(C, Items', Items'', RQs, RQs'),
	lines;

    Items ? S, string(S) : Items' = _,
      RQs = strings(Base) |
	define(Items, Base, StringDefs).

/********************* D E F I N E   S T R I N G S ***************************/

define(Strings, Offset, Defs) :-

    Strings ? String,
    Offset' := Offset + 12 + 4*(string_length(String)/4) :
      Defs ! String(Offset) |
	define;

    Strings = [] : Offset = _,
      Defs = [] .

/************************* I N S T R U C T I O N ****************************/

instruction(Code, I1, I2, R1, R2) :-

    Code = FCP_dg_mult_operation, I1 = [N, AOP | I1'] :
      R1 ! instruction(multiple, (N, Mnemonic, Assignments), 3) |
	assignments(AOP, I1', I2, R1', R2, N, Mnemonic, Assignments);

% deref

    Code = FCP_dg_deref_3, I1 = [RegE(I), RegE(J), RegE(K) | I2^],
    integer(I), integer(J), integer(K) :
      R1 = [instruction(deref, (I, J, K), 4) | R2] ;

    Code = FCP_dg_deref_2, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J) :
      R1 = [instruction(deref_value, (I, J), 3) | R2] ;

    Code = FCP_dg_deref_subarg_3,
    I1 = [RegE(I), IndexE(IX), RegE(J), RegE(K) | I2^],
    integer(I), integer(IX), integer(J), integer(K) :
      R1 = [instruction(deref, ((I, IX), J, K), 5) | R2] ;

    Code = FCP_dg_deref_subarg_2, I1 = [RegE(I), IndexE(IX), RegE(K) | I2^],
    integer(I), integer(IX), integer(K) :
      R1 = [instruction(deref, ((I, IX), K), 4) | R2] ;

    Code = FCP_dg_deref_car_3, I1 = [RegE(I), RegE(J), RegE(K) | I2^],
    integer(I), integer(J), integer(K) :
      R1 = [instruction(deref_car, (I, J, K), 4) | R2] ;

    Code = FCP_dg_deref_car_2, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J) :
      R1 = [instruction(deref_car, (I, J), 3) | R2] ;

% allocate

    Code = FCP_dg_allocate_var, I1 = [RegE(I) | I2^],
    integer(I) :
      R1 = [instruction(allocate_var, I, 2) | R2] ;

    Code = FCP_dg_allocate_tuple, I1 = [RegE(I), N | I1'],
    integer(I), N > 0 :
      R1 ! instruction(allocate_tuple, (I, N, Args), 3) |
	auxiliary # arguments(Args, I1', I2, R1', R2, N);

    Code = FCP_dg_allocate_list_cell, I1 = [RegE(I) | I1'],
    integer(I) :
      R1 ! instruction(allocate_list_cell, (I, Args), 2) |
	auxiliary # arguments(Args, I1', I2, R1', R2, 2);

    Code = FCP_dg_allocate_list_we, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J) :
      R1 = [instruction(allocate_list_we, (I, J), 3)| R2] ;

    Code = FCP_dg_allocate_listN, I1 = [RegE(I), N | I1'],
    integer(I), N > 0 :
      R1 ! instruction(allocate_listN, (I, N, Args), 3) |
	auxiliary # arguments(Args, I1', I2, R1', R2, N);

    Code = FCP_dg_allocate_pr, I1 = [RegE(I), N | I1'],
    integer(I), N > 0 :
      R1 ! instruction(allocate_PR, (I, N, Args), 3) |
	auxiliary # arguments(Args, I1', I2, R1', R2, N);

% arguments

    Code = FCP_dg_fetch, I1 = [RegE(I), String |  I2^],
    integer(I) :
      R1 = [instruction(fetch, (I, Melted), 2),
	    escape(String, Frozen)
	   | R2] |
	melt(Frozen, Melted, _) ;

% copying

    Code = FCP_dg_copy_Rs_Rd, I1 = [RegE(S), RegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(copyrr, (S = D), 3) | R2] ;

    Code = FCP_dg_copy_Rs_CpId, I1 = [RegE(S), PRegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(copyrp, (S = D), 3) | R2] ;

    Code = FCP_dg_copy_Rs_SRd, I1 = [RegE(S), RegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(copyri, (S = D), 3) | R2] ;

    Code = FCP_dg_copy_CpIs_Rd, I1 = [PRegE(S), RegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(copypr, (S = D), 3) | R2] ;

    Code = FCP_dg_copy_CpIs_CpId, I1 = [PRegE(S), PRegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(copypp, (S = D), 3) | R2] ;

    Code = FCP_dg_copy_CpIs_SRd, I1 = [PRegE(S), RegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(copypi, (S = D), 3) | R2] ;

    Code = FCP_dg_copy_SRs_Rd, I1 = [RegE(S), RegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(copyir, (S = D), 3) | R2] ;

    Code = FCP_dg_copy_SRs_CpId, I1 = [RegE(S), PRegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(copyip, (S = D), 3) | R2] ;

    Code = FCP_dg_copy_SRs_SRd, I1 = [RegE(S), RegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(copyii, (S = D), 3) | R2] ;

    Code = FCP_dg_copy_RsIs_Rd, I1 = [RegE(S), IndexE(SX), RegE(D) | I2^],
    integer(S), integer(SX), integer(D) :
      R1 = [instruction(copyxr, ((S, SX) = D), 4) | R2] ;

    Code = FCP_dg_copy_RsIs_CpId, I1 = [RegE(S), IndexE(SX), PRegE(D) | I2^],
    integer(S), integer(SX), integer(D) :
      R1 = [instruction(copyxp, ((S, SX) = D), 4) | R2] ;

    Code = FCP_dg_copy_RsIs_SRd, I1 = [RegE(S), IndexE(SX), RegE(D) | I2^],
    integer(S), integer(SX), integer(D) :
      R1 = [instruction(copyxi, ((S, SX) = D), 4) | R2] ;

    Code = FCP_dg_copy_RRs_Rd, I1 = [RegE(S), RegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(copyqr, (S = D), 3) | R2] ;

    Code = FCP_dg_copy_RRs_CpId, I1 = [RegE(S), PRegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(copyqp, (S = D), 3) | R2] ;

    Code = FCP_dg_copy_RRs_SRd, I1 = [RegE(S), RegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(copyqi, (S = D), 3) | R2] ;

    Code = FCP_dg_copy_CRs_Rd, I1 = [RegE(S), RegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(copycr, (S = D), 3) | R2] ;

    Code = FCP_dg_copy_CRs_CpId, I1 = [RegE(S), PRegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(copycp, (S = D), 3) | R2] ;

    Code = FCP_dg_copy_CRs_SRd, I1 = [RegE(S), RegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(copyci, (S = D), 3) | R2] ;

    Code = FCP_dg_copy_ARsIs_Rd, I1 = [RegE(S), IndexE(SX), RegE(D) | I2^],
    integer(S), integer(D), integer(SX) :
      R1 = [instruction(copyar, ((S, SX) = D), 4) | R2] ;

    Code = FCP_dg_copy_ARsIs_CpId, I1 = [RegE(S), IndexE(SX), PRegE(D) | I2^],
    integer(S), integer(D), integer(SX) :
      R1 = [instruction(copyap, ((S, SX) = D), 4) | R2] ;

    Code = FCP_dg_copy_ARsIs_SRd, I1 = [RegE(S), IndexE(SX), RegE(D) | I2^],
    integer(S), integer(D), integer(SX) :
      R1 = [instruction(copyai, ((S, SX) = D), 4) | R2] ;

    Code = FCP_dg_copy_RARsIs_Rd, I1 = [RegE(S), IndexE(SX), RegE(D) | I2^],
    integer(S), integer(D), integer(SX) :
      R1 = [instruction(copybr, ((S, SX) = D), 4) | R2] ;

    Code = FCP_dg_copy_RARsIs_CpId, I1 = [RegE(S), IndexE(SX), PRegE(D) | I2^],
    integer(S), integer(D), integer(SX) :
      R1 = [instruction(copybp, ((S, SX) = D), 4) | R2] ;

    Code = FCP_dg_copy_RARsIs_SRd, I1 = [RegE(S), IndexE(SX), RegE(D) | I2^],
    integer(S), integer(D), integer(SX) :
      R1 = [instruction(copybi, ((S, SX) = D), 4) | R2] ;

    Code = FCP_dg_copy_Nil_Rd, I1 = [RegE(D) | I2^],
    integer(D) :
      R1 = [instruction(copynr, D, 2) | R2] ;

    Code = FCP_dg_copy_Nil_CpId, I1 = [PRegE(D) | I2^],
    integer(D) :
      R1 = [instruction(copynp, D, 2) | R2] ;

    Code = FCP_dg_copy_Nil_SRd, I1 = [RegE(D) | I2^],
    integer(D) :
      R1 = [instruction(copyni, D, 2) | R2] ;

    Code = FCP_dg_copy_Word_Rd :
      R1 = [instruction(copywr, (S = D), 1) | R1'] |
	auxiliary # escape(S, I1, [RegE(D) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_copy_Word_CpId :
      R1 = [instruction(copywp, (S = D), 1) | R1'] |
	auxiliary # escape(S, I1, [PRegE(D) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_copy_Word_SRd :
      R1 = [instruction(copywi, (S = D), 1) | R1'] |
	auxiliary # escape(S, I1, [RegE(D) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_copy_Real_Rd :
      R1 = [instruction(copyfr, (S = D), 1) | R1'] |
	auxiliary # escape(S, I1, [RegE(D) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_copy_Real_CpId :
      R1 = [instruction(copyfp, (S = D), 1) | R1'] |
	auxiliary # escape(S, I1, [PRegE(D) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_copy_Real_SRd :
      R1 = [instruction(copyfi, (S = D), 1) | R1'] |
	auxiliary # escape(S, I1, [RegE(D) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_copy_String_Rd :
      R1 = [instruction(copysr, (S = D), 1) | R1'] |
	auxiliary # escape(S, I1, [RegE(D) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_copy_String_CpId :
      R1 = [instruction(copysp, (S = D), 1) | R1'] |
	auxiliary # escape(S, I1, [PRegE(D) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_copy_String_SRd :
      R1 = [instruction(copysi, (S = D), 1) | R1'] |
	auxiliary # escape(S, I1, [RegE(D) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_copy_WeVar_Rd, I1 = [RegE(D) | I2^],
    integer(D) :
      R1 = [instruction(copyvr, D, 2) | R2] ;

    Code = FCP_dg_copy_WeVar_CpId, I1 = [PRegE(D) | I2^],
    integer(D) :
      R1 = [instruction(copyvp, D, 2) | R2] ;

    Code = FCP_dg_copy_WeVar_SRd, I1 = [RegE(D) | I2^],
    integer(D) :
      R1 = [instruction(copyvi, D, 2) | R2] ;

% assign

    Code = FCP_dg_asgn_Rs_SRd, I1 = [RegE(S), RegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(assignr, (S = D), 3) | R2] ;

    Code = FCP_dg_asgn_CpIs_SRd, I1 = [RegE(S), PRegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(assignp, (S = D), 3) | R2] ;

    Code = FCP_dg_asgn_SRs_SRd, I1 = [RegE(S), RegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(assigni, (S = D), 3) | R2] ;

    Code = FCP_dg_asgn_RsIs_SRd, I1 = [RegE(S), IndexE(SX), RegE(D) | I2^],
    integer(S), integer(SX), integer(D) :
      R1 = [instruction(assignx, ((S, SX) = D), 4) | R2] ;

    Code = FCP_dg_asgn_RRs_SRd, I1 = [RegE(S), RegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(assignq, (S = D), 3) | R2] ;

    Code = FCP_dg_asgn_CRs_SRd, I1 = [RegE(S), RegE(D) | I2^],
    integer(S), integer(D) :
      R1 = [instruction(assignc, (S = D), 3) | R2] ;

    Code = FCP_dg_asgn_ARsIs_SRd, I1 = [RegE(S), IndexE(SX), RegE(D) | I2^],
    integer(S), integer(D), integer(SX) :
      R1 = [instruction(assigna, ((S, SX) = D), 4) | R2] ;

    Code = FCP_dg_asgn_RARsIs_SRd, I1 = [RegE(S), IndexE(SX), RegE(D) | I2^],
    integer(S), integer(D), integer(SX) :
      R1 = [instruction(assignb, ((S, SX) = D), 4) | R2] ;

    Code = FCP_dg_asgn_Nil_SRd, I1 = [RegE(D) | I2^],
    integer(D) :
      R1 = [instruction(assignn, D, 2) | R2] ;

    Code = FCP_dg_asgn_Word_SRd :
      R1 = [instruction(assignw, (S = D), 1) | R1'] |
	auxiliary # escape(S, I1, [RegE(D) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_asgn_Real_SRd :
      R1 = [instruction(assignf, (S = D), 1) | R1'] |
	auxiliary # escape(S, I1, [RegE(D) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_asgn_String_SRd :
      R1 = [instruction(assigns, (S = D), 1) | R1'] |
	auxiliary # escape(S, I1, [RegE(D) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_asgn_WeVar_SRd, I1 = [RegE(D) | I2^],
    integer(D) :
      R1 = [instruction(assignv, D, 2) | R2] ;

% goto

    Code = FCP_dg_goto_there, I1 = [Reference | I2^] :
      R1 = [instruction(goto, Address, 1),
	    escape(Reference, Address)
	   | R2] ;

% conditional (tag)

    Code = FCP_dg_if_not_reference, I1 = [RegE(I), Reference | I2^],
		     integer(I) :
      R1 = [instruction(if_not_reference, (I, then(Address)), 2),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_not_variable, I1 = [RegE(I), Reference | I2^],
		     integer(I) :
      R1 = [instruction(if_not_variable, (I, then(Address)), 2),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_not_writable, I1 = [RegE(I), Reference | I2^],
		     integer(I) :
      R1 = [instruction(if_not_we, (I, then(Address)), 2),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_not_read_only, I1 = [RegE(I), Reference | I2^],
		     integer(I) :
      R1 = [instruction(if_not_ro, (I, then(Address)), 2),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_not_integer, I1 = [RegE(I), Reference | I2^],
		     integer(I) :
      R1 = [instruction(if_not_integer, (I, then(Address)), 2),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_not_real, I1 = [RegE(I), Reference | I2^],
		     integer(I) :
      R1 = [instruction(if_not_real, (I, then(Address)), 2),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_not_string, I1 = [RegE(I), Reference | I2^],
		     integer(I) :
      R1 = [instruction(if_not_string, (I, then(Address)), 2),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_not_nil, I1 = [RegE(I), Reference | I2^],
		     integer(I) :
      R1 = [instruction(if_not_nil, (I, then(Address)), 2),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_not_list, I1 = [RegE(I), Reference | I2^],
		     integer(I) :
      R1 = [instruction(if_not_list, (I, then(Address)), 2),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_not_tuple, I1 = [RegE(I), Reference | I2^],
		     integer(I) :
      R1 = [instruction(if_not_tuple, (I, then(Address)), 2),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_not_vector, I1 = [RegE(I), Reference | I2^],
		     integer(I) :
      R1 = [instruction(if_not_vector, (I, then(Address)), 2),
	    escape(Reference, Address)
	   | R2] ;

% conditional (compare values)

    Code = FCP_dg_if_int_lt, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_integerLT, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_int_bt, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_integerGT, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_int_le, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_integerLE, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_int_be, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_integerGE, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_int_eq, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_integerEQ, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_int_neq, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_integerNE, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_real_lt, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_realLT, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_real_bt, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_realGT, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_real_le, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_realLE, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_real_be, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_realGE, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_real_eq, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_realEQ, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_real_neq, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_realNE, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_str_lt, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_stringLT, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_str_bt, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_stringGT, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_str_le, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_stringLE, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_str_be, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_stringGE, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_str_eq, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_stringEQ, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_str_neq, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_stringNE, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_tuple_lt, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_tupleLT, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_tuple_bt, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_tupleGT, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_tuple_le, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_tupleLE, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_tuple_be, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_tupleGE, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_tuple_eq, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_tupleEQ, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_if_tuple_neq, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_tupleNE, (I, J, then(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

% branches

    Code = FCP_dg_switch_on_tag, I1 = [RegE(I), N, Data | I1'],
    integer(I), integer(N),
    N' := N + 1 :
      R1 = [instruction(switch_on_tag, (I, N, bits(Value), Labels), 3),
	    escape(Data, Value)
	   | R1'] |
	auxiliary # labels(Labels, I1', I2, R1', R2, N');

    Code = FCP_dg_branch_integer, I1 = [RegE(I), MinData, MaxData | I1'],
    integer(I) :
      R1 = [instruction(branch_integer, (I, min(Min), max(Max), Labels), 2),
	    escape(MinData, Min), escape(MaxData, Max)
	   | R1'] |
	N := Max - Min + 2,
	auxiliary # labels(Labels, I1', I2, R1', R2, N);

    Code = FCP_dg_branch_real, I1 = [RegE(I), MinData, MaxData | I1'],
    integer(I) :
      R1 = [instruction(branch_real, (I, min(Min), max(Max), Labels), 2),
	    escape(MinData, Min), escape(MaxData, Max)
	   | R1'] |
	N := Max - Min + 2,
	auxiliary # labels(Labels, I1', I2, R1', R2, N);

    Code = FCP_dg_branch_tuple, I1 = [RegE(I), MinData, MaxData | I1'],
    integer(I) :
      R1 = [instruction(branch_tuple, (I, min(Min), max(Max), Labels), 2),
	    escape(MinData, Min), escape(MaxData, Max)
	   | R1'] |
	N := Max - Min + 2,
	auxiliary # labels(Labels, I1', I2, R1', R2, N);

    Code = FCP_dg_case_hash_integer, I1 = [RegE(I), M | I1'],
    integer(I), integer(M),
    N := M + 1 :
      R1 ! instruction(case_hash_integer, (I, M, Labels), 3) |
	auxiliary # labels(Labels, I1', I2, R1', R2, N);

    Code = FCP_dg_case_hash_string, I1 = [RegE(I), M | I1'],
    integer(I), integer(M),
    N := M + 1 :
      R1 ! instruction(case_hash_string, (I, M, Labels), 3) |
	auxiliary # labels(Labels, I1', I2, R1', R2, N);

% processes

    Code = FCP_dg_enqueue, I1 = [RegE(I), Procedure | I2^],
    integer(I) :
      R1 = [instruction(enqueue, (I, 'procedure' @ Address), 2),
	    escape(Procedure, Address)
	   | R2] ;


    Code = FCP_dg_iterate, I1 = [Iterative | I2^] :
      R1 = [instruction(iterate, Address, 1),
	    escape(Iterative, Address)
	   | R2] ;

    Code = FCP_dg_execute, I1 = [Procedure, Iterative | I2^] :
      R1 = [instruction(execute, (initial(PAddr), iterative(IAddr)), 1),
	    escape(Procedure, PAddr), escape(Iterative, IAddr)
	   | R2] ;

    Code = FCP_dg_halt :
      I2 = I1,
      R1 = [instruction(halt, '', 1) | R2] ;

    Code = FCP_dg_commit1, I1 = [Reference | I2^] :
      R1 = [instruction(commit, else(Address), 1),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_commit0 :
      I2 = I1,
      R1 = [instruction(commit, '', 1) | R2] ;

    Code = FCP_dg_set_cp_arity, I1 = [N | I2^], integer(N) :
      R1 = [instruction(set_cp_arity, N, 2) | R2] ;

    Code = FCP_dg_suspend2, I1 = [Reference1, Reference2 | I2^] :
      R1 = [instruction(suspend, (else(Address1), then(Address2)), 1),
	    escape(Reference1, Address1), escape(Reference2, Address2)
	   | R2] ;

    Code = FCP_dg_suspend1, I1 = [Reference | I2^] :
      R1 = [instruction(suspend, else(Address), 1),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_suspend0 :
      I2 = I1,
      R1 = [instruction(suspend, '', 1) | R2] ;

    Code = FCP_dg_suspend_on, I1 = [RegE(I) | I2^],
    integer(I) :
      R1 = [instruction(suspend_on, I, 2) | R2] ;

    Code = FCP_dg_set_HBT :
      I2 = I1,
      R1 = [instruction(set_HBT, '', 1) | R2] ;

    Code = FCP_dg_undo :
      I2 = I1,
      R1 = [instruction(undo, '', 1) | R2] ;

    Code = FCP_dg_drf2_if_int_neq,
    I1 = [RegE(I), RegE(K), Word, Reference | I2^],
    integer(I), integer(K) :
      R1 = [instruction(deref_integer,
			(I, K =\= integer(Integer), then(Address)),
			3
	    ),
	    escape(Word, Integer),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_drf2_if_real_neq,
    I1 = [RegE(I), RegE(K), Words, Reference | I2^],
    integer(I), integer(K) :
      R1 = [instruction(deref_real,
			(I, K =\= real(Real), then(Address)),
			3
	    ),
	    escape(Words, Real),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_drf2_if_str_neq,
    I1 = [RegE(I), RegE(K), Word, Reference | I2^],
    integer(I), integer(K) :
      R1 = [instruction(deref_string,
			(I, K =\= string(String), then(Address)),
			3
	    ),
	    escape(Word, String),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_drf2_if_not_nil, I1 = [RegE(I), RegE(K), Reference | I2^],
    integer(I), integer(K) :
      R1 = [instruction(deref_nil, (I, K, else(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_drf2_if_not_list, I1 = [RegE(I), RegE(K), Reference | I2^],
    integer(I), integer(K) :
      R1 = [instruction(old_deref_list, (I, K, else(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_drf2_if_tuple_neq,
    I1 = [RegE(I), RegE(K), Word, Reference | I2^],
    integer(I), integer(K) :
      R1 = [instruction(deref_tuple,
			(I, K =\= tuple(Arity), then(Address)),
			3
	    ),
	    escape(Word, Arity),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_drf2_switch_on_tag,
    I1 = [RegE(I), RegE(K), N, TagBitsWord | I1'],
    integer(I), integer(K),
    N > 0,
    N' := N + 1 :
      R1 = [instruction(deref_on_tag, (I, K, N, mask(TagBits), Labels), 4),
	    escape(TagBitsWord, TagBits)
	   | R1'] |
	auxiliary # labels(Labels, I1', I2, R1', R2, N');

    Code = FCP_dg_drf2_branch_integer,
    I1 = [RegE(I), RegE(K), MinData, MaxData | I1'],
    integer(I), integer(K) :
      R1 = [instruction(deref_branch_integer,
			(I, K, min(Min), max(Max), Labels), 3),
	    escape(MinData, Min), escape(MaxData, Max)
	   | R1'] |
	N := Max - Min + 2,
	auxiliary # labels(Labels, I1', I2, R1', R2, N);

    Code = FCP_dg_drf2_branch_tuple,
    I1 = [RegE(I), RegE(K), MinData, MaxData | I1'],
    integer(I), integer(K) :
      R1 = [instruction(deref_branch_tuple,
			(I, K, min(Min), max(Max), Labels), 3),
	    escape(MinData, Min), escape(MaxData, Max)
	   | R1'] |
	N := Max - Min + 2,
	auxiliary # labels(Labels, I1', I2, R1', R2, N);

    Code = FCP_dg_drf2_hash_integer, I1 = [RegE(I), RegE(K), M | I1'],
    integer(I), integer(K), integer(M),
    N := M + 2 :
      R1 ! instruction(deref_hash_integer, (I, K, M, Labels), 4) |
	auxiliary # labels(Labels, I1', I2, R1', R2, N);

    Code = FCP_dg_drf2_hash_string, I1 = [RegE(I), RegE(K), M | I1'],
    integer(I), integer(K), integer(M),
    N := M + 1 :
      R1 ! instruction(deref_hash_string, (I, K, M, Labels), 4) |
	auxiliary # labels(Labels, I1', I2, R1', R2, N);

    Code = FCP_dg_if_var_suspend, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J) :
      R1 = [instruction(if_var, (I, J), 3) | R2] ;

    Code = FCP_dg_drf1_if_var_suspend, I1 = [I, Reference | I2^], integer(I) :
      R1 = [instruction(deref_known, (I, else(Address)), 2),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_alloc_pr_enqu, I1 = [N, Procedure | I1'], N > 0 :
      R1 = [instruction(allocate_enqueue, (N, procedure(Address), Args), 2),
	    escape(Procedure, Address)
	   | R1'] |
	auxiliary # arguments(Args, I1', I2, R1', R2, N);

    Code = FCP_dg_alloc_pr_regs_enqu, I1 = [N, Procedure | I1'], N > 0 :
      R1 = [instruction(allocate_regs_enqueue,
			(N, 'procedure' @ Address, Regs),
			2
	    ),
	    escape(Procedure, Address),
	    increment(N)
	   | R2] |
	allocated_regs(I1', I2, N, Regs);

    Code = FCP_dg_deref_list, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J):
      R1 = [instruction(deref_list, (I, J, else(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_deref_car_list,
    I1 = [RegE(I), RegE(J), RegE(K), Reference | I2^],
	integer(I), integer(J), integer(K):
      R1 = [instruction(deref_car_list, (I, J, K, else(Address)), 4),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_deref_sub_list,
    I1 = [RegE(I), IndexE(IX), RegE(J), RegE(K), Reference | I2^],
    integer(I), integer(IX), integer(J), integer(K):
      R1 = [instruction(deref_sub_list, (I, IX, J, K, else(Address)), 5),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_deref_vars, I1 = [N | I1'], integer(N), Count := N + 2 :
      R1 = [instruction(deref_vars, (N, Arguments, else(Address)), Count),
	    escape(Reference, Address)
	   | R2] |
	auxiliary#do_vars(N, Arguments, [Reference | I2], I1');

    Code = FCP_dg_cmp_int_lt, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J):
      R1 = [instruction(cmp_int_LT, (I, J, else(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_cmp_int_bt, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J):
      R1 = [instruction(cmp_int_GT, (I, J, else(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_cmp_int_le, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J):
      R1 = [instruction(cmp_int_LE, (I, J, else(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_cmp_int_be, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J):
      R1 = [instruction(cmp_int_GE, (I, J, else(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_cmp_int_eq, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J):
      R1 = [instruction(cmp_int_EQ, (I, J, else(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_cmp_int_ne, I1 = [RegE(I), RegE(J), Reference | I2^],
    integer(I), integer(J):
      R1 = [instruction(cmp_int_NE, (I, J, else(Address)), 3),
	    escape(Reference, Address)
	   | R2] ;

    Code = FCP_dg_unify_args, I1 = [N, Reference | I2^], integer(N) :
      R1 = [instruction(unify, (N, else(Address)), 2),
	    escape(Reference, Address)
	   | R2];

    Code = FCP_dg_unify_reg_reg, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(unify_reg_reg, (I, J), 3)
	   | R2] ;

    Code = FCP_dg_unify_reg_xreg, I1 = [RegE(I), RegE(J), IndexE(IX) | I2^],
    integer(I), integer(J), integer(IX):
      R1 = [instruction(unify_reg_xreg, (I, J, IX), 4)
	   | R2] ;

    Code = FCP_dg_unify_reg_axreg, I1 = [RegE(I), RegE(J), IndexE(IX) | I2^],
    integer(I), integer(J), integer(IX):
      R1 = [instruction(unify_reg_axreg, (I, J, IX), 4)
	   | R2] ;

    Code = FCP_dg_unify_reg_roreg, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(unify_reg_roreg, (I, J), 3)
	   | R2] ;

    Code = FCP_dg_unify_reg_roaxreg, I1 = [RegE(I), RegE(J), IndexE(IX) | I2^],
    integer(I), integer(J), integer(IX):
      R1 = [instruction(unify_reg_roaxreg, (I, J, IX), 4)
	   | R2] ;

    Code = FCP_dg_unify_reg_carreg, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(unify_reg_carreg, (I, J), 3)
	   | R2] ;

    Code = FCP_dg_unify_reg_word, I1 = [RegE(I), Data | I2^],
    integer(I), Data = {IntegerE, _}:
      R1 = [instruction(unify_reg_word, (I, integer(Value)), 2),
	    escape(Data, Value)
	   | R2] ;

    Code = FCP_dg_unify_reg_string, I1 = [RegE(I), Data | I2^],
    integer(I), Data = {StringE, _}:
      R1 = [instruction(unify_reg_string, (I, Value), 2),
	    escape(Data, Value)
	   | R2] ;

    Code = FCP_dg_unify_reg_real, I1 = [RegE(I), Data | I2^],
    integer(I), Data = {RealE, _}:
      R1 = [instruction(unify_reg_real, (I, real(Value)), 2),
	    escape(Data, Value)
	   | R2] ;

    Code = FCP_dg_unify_reg_nil, I1 = [RegE(I)| I2^],
    integer(I):
      R1 = [instruction(unify_reg_nil, I, 2)
	   | R2];

    Code = FCP_dg_unify_xreg_reg, I1 = [RegE(I), IndexE(IX), RegE(J) | I2^],
    integer(I), integer(IX), integer(J):
      R1 = [instruction(unify_xreg_reg, (I, IX, J), 4)
	   | R2];

    Code = FCP_dg_unify_xreg_xreg,
    I1 = [RegE(I),IndexE(IX),RegE(J),IndexE(JX)| I2^],
    integer(I), integer(IX), integer(J), integer(JX):
      R1 = [instruction(unify_xreg_xreg,(I,IX,J,JX), 5)
	   | R2] ;

    Code = FCP_dg_unify_xreg_axreg,
    I1 = [RegE(I), IndexE(IX), RegE(J), IndexE(JX) | I2^],
    integer(I), integer(IX), integer(J), integer(JX):
      R1 = [instruction(unify_xreg_axreg,(I,IX,J,JX), 5)
	   | R2] ;

    Code = FCP_dg_unify_xreg_roreg, I1 = [RegE(I), IndexE(IX), RegE(J) | I2^],
    integer(I), integer(IX), integer(J):
      R1 = [instruction(unify_xreg_roreg,(I,IX,J), 4)
	   | R2] ;

    Code = FCP_dg_unify_xreg_roaxreg,
    I1 = [RegE(I), IndexE(IX), RegE(J), IndexE(JX)| I2^],
    integer(I), integer(IX), integer(J), integer(JX):
      R1 = [instruction(unify_xreg_roaxreg,(I,IX,J,JX), 5)
	   | R2] ;

    Code = FCP_dg_unify_xreg_carreg, I1 = [RegE(I), IndexE(IX), RegE(J) | I2^],
    integer(I), integer(IX), integer(J):
      R1 = [instruction(unify_xreg_carreg,(I,IX,J), 4)
	   | R2] ;

    Code = FCP_dg_unify_xreg_word, I1 = [RegE(I), IndexE(IX),Data | I2^],
    integer(I), integer(IX), Data = {IntegerE, _}:
      R1 = [instruction(unify_xreg_word, (I, IX, integer(Value)), 3),
	    escape(Data, Value)
	   | R2] ;

    Code = FCP_dg_unify_xreg_string, I1 = [RegE(I), IndexE(IX), Data | I2^],
    integer(I), integer(IX), Data = {StringE, _}:
      R1 = [instruction(unify_xreg_string, (I, IX, Value), 3),
	    escape(Data, Value)
	   | R2] ;

    Code = FCP_dg_unify_xreg_real, I1 = [RegE(I),IndexE(IX), Data | I2^],
    integer(I),integer(IX), Data = {RealE, _}:
      R1 = [instruction(unify_xreg_real, (I, IX, real(Value)), 3),
	    escape(Data, Value)
	   | R2] ;

    Code = FCP_dg_unify_xreg_nil, I1 = [RegE(I), IndexE(IX) | I2^],
    integer(I), integer(IX):
      R1 = [instruction(unify_xreg_nil, I, 3)
	   | R2];

    otherwise |
	instruction2(Code, I1, I2, R1, R2).

% the procedure is too big for ndg so split it

instruction2(Code, I1, I2, R1, R2):-

    Code = FCP_dg_decrement_2_reg, I1 = [RegE(I), RegE(J)| I2^],
    integer(I), integer(J):
      R1 = [instruction(decrement_2_reg, (I, J), 3)
	   | R2];

    Code = FCP_dg_decrement_2_xreg, I1 = [RegE(I), RegE(J), IndexE(IX)| I2^],
    integer(I), integer(J):
      R1 = [instruction(decrement_2_xreg, (I, J, IX), 4)
	   | R2];

    Code = FCP_dg_decrement, I1 = [RegE(I) | I2^],
    integer(I):
      R1 = [instruction(decrement, I, 2)
	   | R2];

    Code = FCP_dg_increment_2_reg, I1 = [RegE(I), RegE(J)| I2^],
    integer(I), integer(J):
      R1 = [instruction(increment_2_reg, (I, J), 3)
	   | R2];

    Code = FCP_dg_increment_2_xreg, I1 = [RegE(I), RegE(J), IndexE(IX)| I2^],
    integer(I), integer(J):
      R1 = [instruction(increment_2_xreg, (I, J, IX), 4)
	   | R2];

    Code = FCP_dg_increment, I1 = [RegE(I) | I2^],
    integer(I):
      R1 = [instruction(increment, I, 2)
	   | R2];

    Code = FCP_dg_plus_reg_reg_reg, I1 = [RegE(I), RegE(J), RegE(K) | I2^],
    integer(I), integer(J), integer(K):
      R1 = [instruction(plus_reg_reg_reg, (I, J, K), 4)
	   | R2];

    Code = FCP_dg_plus_reg_reg_xreg,
    I1 = [RegE(I), RegE(J), RegE(K), IndexE(KX) | I2^],
    integer(I), integer(J), integer(K), integer(KX):
      R1 = [instruction(plus_reg_reg_xreg, (I, J, K, KX), 5)
	   | R2];

    Code = FCP_dg_plus_reg_int_reg, I1 ? RegE(I):
      R1 = [instruction(plus_reg_int_reg, (I, Integer, J), 2) | R1'] |
	auxiliary#escape(Integer,I1', [RegE(J) | I2], R1',
			 [increment(1) | R2]);

    Code = FCP_dg_plus_reg_int_xreg, I1 ? RegE(I) :
      R1 = [instruction(plus_reg_int_xreg, (I, Integer, J, JX), 2) | R1'] |
	auxiliary#escape(Integer,I1', [RegE(J), IndexE(JX) | I2], R1',
			 [increment(2) | R2]);

    Code = FCP_dg_plus_int_reg_reg:
      R1 = [instruction(plus_int_reg_reg, (Integer, I, J), 1) | R1'] |
	auxiliary#escape(Integer,I1, [RegE(I), RegE(J)| I2], R1',
			 [increment(2) | R2]);

    Code = FCP_dg_plus_int_reg_xreg:
      R1 = [instruction(plus_int_reg_xreg, (Integer, I, J, JX), 1) | R1'] |
	auxiliary#escape(Integer,I1, [RegE(I), RegE(J), IndexE(JX) | I2], R1',
			 [increment(3) | R2]);

    Code = FCP_dg_plus_int_int_reg:
      R1 = [instruction(plus_int_int_reg, (Integer1, Integer2, I), 1) | R1'] |
	auxiliary#escape(Integer1, I1, I1', R1', R1''),
	auxiliary#escape(Integer2, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_plus_int_int_xreg:
      R1 = [instruction(plus_int_int_xreg,(Integer1,Integer2,I,IX),1) | R1'] |
	auxiliary#escape(Integer1, I1, I1', R1', R1''),
	auxiliary#escape(Integer2, I1', [RegE(I), IndexE(IX) | I2], R1'',
			 [increment(2) | R2]);

    Code = FCP_dg_plusnum_reg_reg, I1 = [RegE(I), RegE(J), RegE(K) | I2^],
    integer(I), integer(J), integer(K):
      R1 = [instruction(plusnum_reg_reg, (I, J, K), 4)
	   | R2];

    Code = FCP_dg_plusnum_reg_int, I1 ? RegE(I):
      R1 = [instruction(plusnum_reg_int, (I, Integer, J), 2) | R1'] |
	auxiliary#escape(Integer,I1', [RegE(J) | I2], R1',
			 [increment(1) | R2]);

    Code = FCP_dg_plusnum_reg_real, I1 ? RegE(I):
      R1 = [instruction(plusnum_reg_real, (I, Real, J), 2) | R1'] |
	auxiliary#escape(Real, I1', [RegE(J) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_plusnum_int_reg: 
      R1 = [instruction(plusnum_int_reg, (Integer, I, J), 1) | R1'] |
	auxiliary#escape(Integer,I1, [RegE(I), RegE(J) | I2], R1',
			 [increment(2) | R2]);

    Code = FCP_dg_plusnum_int_int: 
      R1 = [instruction(plusnum_int_int, (Integer1, Integer2, I), 1) | R1'] |
	auxiliary#escape(Integer1, I1, I1', R1', R1''),
	auxiliary#escape(Integer2, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_plusnum_int_real: 
      R1 = [instruction(plusnum_int_real, (Integer, Real, I), 1) | R1'] |
	auxiliary#escape(Integer, I1, I1', R1', R1''),
	auxiliary#escape(Real, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_plusnum_real_reg:
      R1 = [instruction(plusnum_real_reg, (Real, I, J), 1) | R1'] |
	auxiliary#escape(Real,I1, [RegE(I), RegE(J) | I2], R1',
			 [increment(2) | R2]);

    Code = FCP_dg_plusnum_real_int: 
      R1 = [instruction(plusnum_real_int, (Real, Integer, I), 1) | R1'] |
	auxiliary#escape(Real, I1, I1', R1', R1''),
	auxiliary#escape(Integer, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_plusnum_real_real: 
      R1 = [instruction(plusnum_real_real, (Real1, Real2, I), 1) | R1'] |
	auxiliary#escape(Real1, I1, I1', R1', R1''),
	auxiliary#escape(Real2, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_diff_reg_reg_reg, I1 = [RegE(I), RegE(J), RegE(K) | I2^],
    integer(I), integer(J), integer(K):
      R1 = [instruction(diff_reg_reg_reg, (I, J, K), 4)
	   | R2];

    Code = FCP_dg_diff_reg_reg_xreg,
    I1 = [RegE(I), RegE(J), RegE(K), IndexE(KX) | I2^],
    integer(I), integer(J), integer(K), integer(KX):
      R1 = [instruction(diff_reg_reg_xreg, (I, J, K, KX), 5)
	   | R2];

    Code = FCP_dg_diff_reg_int_reg, I1 ? RegE(I):
      R1 = [instruction(diff_reg_int_reg, (I, Integer, J), 2) | R1'] |
	auxiliary#escape(Integer,I1', [RegE(J) | I2], R1',
			 [increment(1) | R2]);

    Code = FCP_dg_diff_reg_int_xreg, I1 ? RegE(I) :
      R1 = [instruction(diff_reg_int_xreg, (I, Integer, J, JX), 2) | R1'] |
	auxiliary#escape(Integer,I1', [RegE(J), IndexE(JX) | I2], R1',
			 [increment(2) | R2]);

    Code = FCP_dg_diff_int_reg_reg:
      R1 = [instruction(diff_int_reg_reg, (Integer, I, J), 1) | R1'] |
	auxiliary#escape(Integer,I1, [RegE(I), RegE(J)| I2], R1',
			 [increment(2) | R2]);

    Code = FCP_dg_diff_int_reg_xreg:
      R1 = [instruction(diff_int_reg_xreg, (Integer, I, J, JX), 1) | R1'] |
	auxiliary#escape(Integer,I1, [RegE(I), RegE(J), IndexE(JX) | I2], R1',
			 [increment(3) | R2]);

    Code = FCP_dg_diff_int_int_reg:
      R1 = [instruction(diff_int_int_reg, (Integer1, Integer2, I), 1) | R1'] |
	auxiliary#escape(Integer1, I1, I1', R1', R1''),
	auxiliary#escape(Integer2, I1', [RegE(I) | I2], R1'',
						[increment(1) | R2]);

    Code = FCP_dg_diff_int_int_xreg:
      R1 = [instruction(diff_int_int_xreg,(Integer1,Integer2,I,IX),1) | R1'] |
	auxiliary#escape(Integer1, I1, I1', R1', R1''),
	auxiliary#escape(Integer2, I1', [RegE(I), IndexE(IX) | I2], R1'',
			 [increment(2) | R2]);

    Code = FCP_dg_diffnum_reg_reg, I1 = [RegE(I), RegE(J), RegE(K) | I2^],
    integer(I), integer(J), integer(K):
      R1 = [instruction(diffnum_reg_reg, (I, J, K), 4)
	   | R2];

    Code = FCP_dg_diffnum_reg_int, I1 ? RegE(I):
      R1 = [instruction(diffnum_reg_int, (I, Integer, J), 2) | R1'] |
	auxiliary#escape(Integer,I1', [RegE(J) | I2], R1',
			 [increment(1) | R2]);

    Code = FCP_dg_diffnum_reg_real, I1 ? RegE(I):
      R1 = [instruction(diffnum_reg_real, (I, Real, J), 2) | R1'] |
	auxiliary#escape(Real, I1', [RegE(J) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_diffnum_int_reg:
      R1 = [instruction(diffnum_int_reg, (Integer, I, J), 1) | R1'] |
	auxiliary#escape(Integer,I1, [RegE(I), RegE(J) | I2], R1',
			 [increment(2) | R2]);

    Code = FCP_dg_diffnum_int_int: 
      R1 = [instruction(diffnum_int_int, (Integer1, Integer2, I), 1) | R1'] |
	auxiliary#escape(Integer1, I1, I1', R1', R1''),
	auxiliary#escape(Integer2, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_diffnum_int_real: 
      R1 = [instruction(diffnum_int_real, (Integer, Real, I), 1) | R1'] |
	auxiliary#escape(Integer, I1, I1', R1', R1''),
	auxiliary#escape(Real, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_diffnum_real_reg:
      R1 = [instruction(diffnum_real_reg, (Real, I, J), 1) | R1'] |
	auxiliary#escape(Real,I1, [RegE(I), RegE(J) | I2], R1',
			 [increment(2) | R2]);

    Code = FCP_dg_diffnum_real_int: 
      R1 = [instruction(diffnum_real_int, (Real, Integer, I), 1) | R1'] |
	auxiliary#escape(Real, I1, I1', R1', R1''),
	auxiliary#escape(Integer, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_diffnum_real_real: 
      R1 = [instruction(diffnum_real_real, (Real1, Real2, I), 1) | R1'] |
	auxiliary#escape(Real1, I1, I1', R1', R1''),
	auxiliary#escape(Real2, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_execute2, I1 = [Reference1, Reference2 | I2^]:
      R1 = [instruction(execute2,  (Addr1, Addr2), 1),
	    escape(Reference1, Addr1), escape(Reference2, Addr2)
	   | R2] ;

    Code = FCP_dg_iterate1, I1 = [Reference | I2^]:
      R1 = [instruction(iterate1,  (Addr), 1),
	    escape(Reference, Addr)
	   | R2] ;

    Code = FCP_dg_execute1, I1 = [Reference | I2^]:
      R1 = [instruction(execute1,  (Addr), 1),
	    escape(Reference, Addr)
	   | R2] ;

    Code = FCP_dg_deref_var1, I1 = [RegE(I), Reference | I2^], integer(I):
      R1 = [instruction(deref_var1, (I, else(Address)), 2),
	    escape(Reference, Address)
	   | R2];

    Code = FCP_dg_allocate_vars, I1 = [N | I1'], integer(N), Count := N + 2 :
      R1 = [instruction(allocate_vars, (N, Arguments), Count) | R2] |
	auxiliary#do_vars(N, Arguments, I2, I1');

    Code = FCP_dg_list_assign_with_check, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(list_assign_with_check, (I, J), 3) | R2] ;

    Code = FCP_dg_list_assign, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(list_assign, (I, J), 3) | R2] ;

    Code = FCP_dg_deref_2_addr, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(deref, (I, J), 3) | R2] ;

    Code = FCP_dg_load_car, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(load_car, (I, J), 3) | R2] ;

    Code = FCP_dg_decrement_pointer, I1 = [RegE(I) | I2^], integer(I):
      R1 = [instruction(decrement_pointer, (I), 2) | R2] ;

    Code = FCP_dg_decrement_2_pointer, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(decrement_pointer, (I, J), 3) |R2] ;

    Code = FCP_dg_increment_pointer, I1 = [RegE(I) | I2^], integer(I):
      R1 = [instruction(increment_pointer, (I), 2) | R2] ;

    Code = FCP_dg_increment_2_pointer, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(increment_pointer, (I, J), 3) |R2] ;

    Code = FCP_dg_deref_list3, I1 = [RegE(I), RegE(J), RegE(K), Reference | I2^],
				integer(I), integer(J), integer(K):
      R1 = [instruction(deref_list, (I, J, K, else(Address)), 4),
	escape(Reference, Address)
	| R2];

    Code = FCP_dg_commit_nolabel: I2 = I1,
      R1 = [instruction("commit(label_ignored)", '', 1) |R2] ;

    Code = FCP_dg_assign_Rs_SRd,
    I1 = [RegE(I), RegE(J) | I2^], integer(I), integer(J):
      R1 = [instruction(assignR, (I, J), 3) |R2] ;

    Code = FCP_dg_assign_CpIs_SRd,
    I1 = [RegE(I), RegE(J) | I2^], integer(I), integer(J):
      R1 = [instruction(assignP, (I, J), 3) |R2] ;

    Code = FCP_dg_assign_SRs_SRd,
    I1 = [RegE(I), RegE(J) | I2^], integer(I), integer(J):
      R1 = [instruction(assignI, (I, J), 3) |R2] ;

    Code = FCP_dg_assign_RsIs_SRd, I1 = [RegE(I), IndexE(IX), RegE(J) | I2^],
    integer(I), integer(IX), integer(J):
      R1 = [instruction(assignX, (I, J), 4) |R2] ;

    Code = FCP_dg_assign_RRs_SRd,
    I1 = [RegE(I), RegE(J) | I2^], integer(I), integer(J):
      R1 = [instruction(assignQ, (I, J), 3) |R2] ;

    Code = FCP_dg_assign_CRs_SRd,
    I1 = [RegE(I), RegE(J) | I2^], integer(I), integer(J):
      R1 = [instruction(assignC, (I, J), 3) |R2] ;

    Code = FCP_dg_assign_ARsIs_SRd, I1 = [RegE(I), IndexE(IX), RegE(J) | I2^],
    integer(I), integer(IX), integer(J):
      R1 = [instruction(assignA, (I, J), 4) |R2] ;

    Code = FCP_dg_assign_RARsIs_SRd, I1 = [RegE(I), IndexE(IX), RegE(J) | I2^],
    integer(I), integer(IX), integer(J):
      R1 = [instruction(assignB, (I, J), 4) |R2] ;

    Code = FCP_dg_assign_Nil_SRd, I1 = [RegE(I) | I2^], integer(I):
      R1 = [instruction(assignN, (I), 2) |R2] ;

    Code = FCP_dg_assign_Word_SRd:
      R1 = [instruction(assignW, (I, J), 1) | R1'] |
	auxiliary # escape(I, I1, [RegE(J) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_assign_Real_SRd:
      R1 = [instruction(assignF, (I, J), 1) | R1'] |
        auxiliary # escape(I, I1, [RegE(J) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_assign_String_SRd :
      R1 = [instruction(assignS, (I, J), 1) | R1'] |
        auxiliary # escape(I, I1, [RegE(J) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_assign_WeVar_SRd, I1 = [RegE(I) | I2^],
    integer(I) :
      R1 = [instruction(assignV, I, 2) | R2] ;

    Code = FCP_dg_assign_com_Rs_SRd, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(com_assignR, (I, J), 3) |R2] ;

    Code = FCP_dg_assign_com_CpIs_SRd, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(com_assignP, (I, J), 3) |R2] ;

    Code = FCP_dg_assign_com_SRs_SRd, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(com_assignI, (I, J), 3) |R2] ;

    Code = FCP_dg_assign_com_RsIs_SRd,
    I1 = [RegE(I), IndexE(IX), RegE(J) | I2^],
    integer(I), integer(IX), integer(J):
      R1 = [instruction(com_assignX, (I, J), 4) |R2] ;

    Code = FCP_dg_assign_com_RRs_SRd, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(com_assignQ, (I, J), 3) |R2] ;

    Code = FCP_dg_assign_com_CRs_SRd, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(com_assignC, (I, J), 3) |R2] ;

    Code = FCP_dg_assign_com_ARsIs_SRd,
    I1 = [RegE(I), IndexE(IX), RegE(J) | I2^],
    integer(I), integer(IX), integer(J):
      R1 = [instruction(com_assignA, (I, J), 4) |R2] ;

    Code = FCP_dg_assign_com_RARsIs_SRd,
    I1 = [RegE(I), IndexE(IX), RegE(J) | I2^],
    integer(I), integer(IX), integer(J):
      R1 = [instruction(com_assignB, (I, J), 4) |R2] ;

    Code = FCP_dg_assign_com_Nil_SRd, I1 = [RegE(I) | I2^], integer(I):
      R1 = [instruction(com_assignN, (I), 2) |R2] ;

    Code = FCP_dg_assign_com_Word_SRd:
      R1 = [instruction(com_assignW, (I, J), 1) | R1'] |
	auxiliary # escape(I, I1, [RegE(J) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_assign_com_Real_SRd:
      R1 = [instruction(com_assignF, (I, J), 1) | R1'] |
        auxiliary # escape(I, I1, [RegE(J) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_assign_com_String_SRd :
      R1 = [instruction(com_assignS, (I, J), 1) | R1'] |
        auxiliary # escape(I, I1, [RegE(J) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_assign_com_WeVar_SRd, I1 = [RegE(I) | I2^],
    integer(I) :
      R1 = [instruction(com_assignV, I, 2) | R2] ;

    Code = FCP_dg_assign_com_tr_Rs_SRd, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(tr_com_assignR, (I, J), 3) |R2] ;

    Code = FCP_dg_assign_com_tr_CpIs_SRd, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(tr_com_assignP, (I, J), 3) |R2] ;

    Code = FCP_dg_assign_com_tr_SRs_SRd, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(tr_com_assignI, (I, J), 3) |R2] ;

    Code = FCP_dg_assign_com_tr_RsIs_SRd,
    I1 = [RegE(I), IndexE(IX), RegE(J) | I2^],
    integer(I), integer(IX), integer(J):
      R1 = [instruction(tr_com_assignX, (I, J), 4) |R2] ;

    Code = FCP_dg_assign_com_tr_RRs_SRd, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(tr_com_assignQ, (I, J), 3) |R2] ;

    Code = FCP_dg_assign_com_tr_CRs_SRd, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(tr_com_assignC, (I, J), 3) |R2] ;

    Code = FCP_dg_assign_com_tr_ARsIs_SRd,
    I1 = [RegE(I), IndexE(IX), RegE(J) | I2^],
    integer(I), integer(IX), integer(J):
      R1 = [instruction(tr_com_assignA, (I, J), 4) |R2] ;

    Code = FCP_dg_assign_com_tr_RARsIs_SRd,
    I1 = [RegE(I), IndexE(IX), RegE(J) | I2^],
    integer(I), integer(IX), integer(J):
      R1 = [instruction(tr_com_assignB, (I, J), 4) |R2] ;

    Code = FCP_dg_assign_com_tr_Nil_SRd, I1 = [RegE(I) | I2^], integer(I):
      R1 = [instruction(tr_com_assignN, (I), 2) |R2] ;

    Code = FCP_dg_assign_com_tr_Word_SRd:
      R1 = [instruction(tr_com_assignW, (I, J), 1) | R1'] |
	auxiliary # escape(I, I1, [RegE(J) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_assign_com_tr_Real_SRd:
      R1 = [instruction(tr_com_assignF, (I, J), 1) | R1'] |
        auxiliary # escape(I, I1, [RegE(J) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_assign_com_tr_String_SRd :
      R1 = [instruction(tr_com_assignS, (I, J), 1) | R1'] |
        auxiliary # escape(I, I1, [RegE(J) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_assign_com_tr_WeVar_SRd, I1 = [RegE(I) | I2^],
    integer(I) :
      R1 = [instruction(tr_com_assignV, I, 2) | R2] ;

    Code = FCP_dg_assign_and_inc, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(assign_and_increment, (I, J), 3) |R2] ;

    Code = FCP_dg_assign_inc_com, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(assign_increment_and_commit, (I, J), 3) |R2] ;

    Code = FCP_dg_assign_inc_com_trail, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(assign_increment_and_commit_trail, (I, J), 3) |R2] ;

	
    Code = FCP_dg_cmt_plus_reg_reg_reg, I1 = [RegE(I), RegE(J), RegE(K) | I2^],
    integer(I), integer(J), integer(K):
      R1 = [instruction(cmt_plus_reg_reg_reg, (I, J, K), 4)
	   | R2];

    Code = FCP_dg_cmt_plus_reg_reg_xreg,
    I1 = [RegE(I), RegE(J), RegE(K), IndexE(KX) | I2^],
    integer(I), integer(J), integer(K), integer(KX):
      R1 = [instruction(cmt_plus_reg_reg_xreg, (I, J, K, KX), 5)
	   | R2];

    Code = FCP_dg_cmt_plus_reg_int_reg, I1 ? RegE(I):
      R1 = [instruction(cmt_plus_reg_int_reg, (I, Integer, J), 2) | R1'] |
	auxiliary#escape(Integer,I1', [RegE(J) | I2], R1',
			 [increment(1) | R2]);

    Code = FCP_dg_cmt_plus_reg_int_xreg, I1 ? RegE(I) :
      R1 = [instruction(cmt_plus_reg_int_xreg, (I, Integer, J, JX), 2) | R1'] |
	auxiliary#escape(Integer,I1', [RegE(J), IndexE(JX) | I2], R1',
			 [increment(2) | R2]);

    Code = FCP_dg_cmt_plus_int_reg_reg:
      R1 = [instruction(cmt_plus_int_reg_reg, (Integer, I, J), 1) | R1'] |
	auxiliary#escape(Integer,I1, [RegE(I), RegE(J)| I2], R1',
			 [increment(2) | R2]);

    Code = FCP_dg_cmt_plus_int_reg_xreg:
      R1 = [instruction(cmt_plus_int_reg_xreg, (Integer, I, J, JX), 1) | R1'] |
	auxiliary#escape(Integer,I1, [RegE(I), RegE(J), IndexE(JX) | I2], R1',
			 [increment(3) | R2]);

    Code = FCP_dg_cmt_plus_int_int_reg:
      R1 = [instruction(cmt_plus_int_int_reg,
			(Integer1, Integer2, I), 1) | R1'] |
	auxiliary#escape(Integer1, I1, I1', R1', R1''),
	auxiliary#escape(Integer2, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_cmt_plus_int_int_xreg:
      R1 = [instruction(cmt_plus_int_int_xreg,
			(Integer1,Integer2,I,IX),1) | R1'] |
	auxiliary#escape(Integer1, I1, I1', R1', R1''),
	auxiliary#escape(Integer2, I1', [RegE(I), IndexE(IX) | I2], R1'',
			 [increment(2) | R2]);

    Code = FCP_dg_cmt_plusnum_reg_reg, I1 = [RegE(I), RegE(J), RegE(K) | I2^],
    integer(I), integer(J), integer(K):
      R1 = [instruction(cmt_plusnum_reg_reg, (I, J, K), 4)
	   | R2];

    Code = FCP_dg_cmt_plusnum_reg_int, I1 ? RegE(I):
      R1 = [instruction(cmt_plusnum_reg_int, (I, Integer, J), 2) | R1'] |
	auxiliary#escape(Integer,I1', [RegE(J) | I2], R1',
			 [increment(1) | R2]);

    Code = FCP_dg_cmt_plusnum_reg_real, I1 ? RegE(I):
      R1 = [instruction(cmt_plusnum_reg_real, (I, Real, J), 2) | R1'] |
	auxiliary#escape(Real, I1', [RegE(J) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_cmt_plusnum_int_reg: 
      R1 = [instruction(cmt_plusnum_int_reg, (Integer, I, J), 1) | R1'] |
	auxiliary#escape(Integer,I1, [RegE(I), RegE(J) | I2], R1',
			 [increment(2) | R2]);

    Code = FCP_dg_cmt_plusnum_int_int: 
      R1 = [instruction(cmt_plusnum_int_int,
			(Integer1, Integer2, I), 1) | R1'] |
	auxiliary#escape(Integer1, I1, I1', R1', R1''),
	auxiliary#escape(Integer2, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_cmt_plusnum_int_real: 
      R1 = [instruction(cmt_plusnum_int_real, (Integer, Real, I), 1) | R1'] |
	auxiliary#escape(Integer, I1, I1', R1', R1''),
	auxiliary#escape(Real, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_cmt_plusnum_real_reg:
      R1 = [instruction(cmt_plusnum_real_reg, (Real, I, J), 1) | R1'] |
	auxiliary#escape(Real,I1, [RegE(I), RegE(J) | I2], R1',
			 [increment(2) | R2]);

    Code = FCP_dg_cmt_plusnum_real_int: 
      R1 = [instruction(cmt_plusnum_real_int, (Real, Integer, I), 1) | R1'] |
	auxiliary#escape(Real, I1, I1', R1', R1''),
	auxiliary#escape(Integer, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_cmt_plusnum_real_real: 
      R1 = [instruction(cmt_plusnum_real_real, (Real1, Real2, I), 1) | R1'] |
	auxiliary#escape(Real1, I1, I1', R1', R1''),
	auxiliary#escape(Real2, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_cmt_diff_reg_reg_reg, I1 = [RegE(I), RegE(J), RegE(K) | I2^],
    integer(I), integer(J), integer(K):
      R1 = [instruction(cmt_diff_reg_reg_reg, (I, J, K), 4)
	   | R2];

    Code = FCP_dg_cmt_diff_reg_reg_xreg,
    I1 = [RegE(I), RegE(J), RegE(K), IndexE(KX) | I2^],
    integer(I), integer(J), integer(K), integer(KX):
      R1 = [instruction(cmt_diff_reg_reg_xreg, (I, J, K, KX), 5)
	   | R2];

    Code = FCP_dg_cmt_diff_reg_int_reg, I1 ? RegE(I):
      R1 = [instruction(cmt_diff_reg_int_reg, (I, Integer, J), 2) | R1'] |
	auxiliary#escape(Integer,I1', [RegE(J) | I2], R1',
			 [increment(1) | R2]);

    Code = FCP_dg_cmt_diff_reg_int_xreg, I1 ? RegE(I) :
      R1 = [instruction(cmt_diff_reg_int_xreg, (I, Integer, J, JX), 2) | R1'] |
	auxiliary#escape(Integer,I1', [RegE(J), IndexE(JX) | I2], R1',
			 [increment(2) | R2]);

    Code = FCP_dg_cmt_diff_int_reg_reg:
      R1 = [instruction(cmt_diff_int_reg_reg, (Integer, I, J), 1) | R1'] |
	auxiliary#escape(Integer,I1, [RegE(I), RegE(J)| I2], R1',
			 [increment(2) | R2]);

    Code = FCP_dg_cmt_diff_int_reg_xreg:
      R1 = [instruction(cmt_diff_int_reg_xreg, (Integer, I, J, JX), 1) | R1'] |
	auxiliary#escape(Integer,I1, [RegE(I), RegE(J), IndexE(JX) | I2], R1',
			 [increment(3) | R2]);

    Code = FCP_dg_cmt_diff_int_int_reg:
      R1 = [instruction(cmt_diff_int_int_reg,
			(Integer1, Integer2, I), 1) | R1'] |
	auxiliary#escape(Integer1, I1, I1', R1', R1''),
	auxiliary#escape(Integer2, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_cmt_diff_int_int_xreg:
      R1 = [instruction(cmt_diff_int_int_xreg,
			(Integer1,Integer2,I,IX),1) | R1'] |
	auxiliary#escape(Integer1, I1, I1', R1', R1''),
	auxiliary#escape(Integer2, I1', [RegE(I), IndexE(IX) | I2], R1'',
			 [increment(2) | R2]);

    Code = FCP_dg_cmt_diffnum_reg_reg, I1 = [RegE(I), RegE(J), RegE(K) | I2^],
    integer(I), integer(J), integer(K):
      R1 = [instruction(cmt_diffnum_reg_reg, (I, J, K), 4)
	   | R2];

    Code = FCP_dg_cmt_diffnum_reg_int, I1 ? RegE(I):
      R1 = [instruction(cmt_diffnum_reg_int, (I, Integer, J), 2) | R1'] |
	auxiliary#escape(Integer,I1', [RegE(J) | I2], R1',
			 [increment(1) | R2]);
    Code = FCP_dg_cmt_diffnum_reg_real, I1 ? RegE(I):
      R1 = [instruction(cmt_diffnum_reg_real, (I, Real, J), 2) | R1'] |
	auxiliary#escape(Real, I1', [RegE(J) | I2], R1', [increment(1) | R2]);

    Code = FCP_dg_cmt_diffnum_int_reg:
      R1 = [instruction(cmt_diffnum_int_reg, (Integer, I, J), 1) | R1'] |
	auxiliary#escape(Integer,I1, [RegE(I), RegE(J) | I2], R1',
			 [increment(2) | R2]);

    Code = FCP_dg_cmt_diffnum_int_int: 
      R1 = [instruction(cmt_diffnum_int_int,
			(Integer1, Integer2, I), 1) | R1'] |
	auxiliary#escape(Integer1, I1, I1', R1', R1''),
	auxiliary#escape(Integer2, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_cmt_diffnum_int_real: 
      R1 = [instruction(cmt_diffnum_int_real, (Integer, Real, I), 1) | R1'] |
	auxiliary#escape(Integer, I1, I1', R1', R1''),
	auxiliary#escape(Real, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_cmt_diffnum_real_reg:
      R1 = [instruction(cmt_diffnum_real_reg, (Real, I, J), 1) | R1'] |
	auxiliary#escape(Real,I1, [RegE(I), RegE(J) | I2], R1',
			 [increment(2) | R2]);

    Code = FCP_dg_cmt_diffnum_real_int: 
      R1 = [instruction(cmt_diffnum_real_int, (Real, Integer, I), 1) | R1'] |
	auxiliary#escape(Real, I1, I1', R1', R1''),
	auxiliary#escape(Integer, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_cmt_diffnum_real_real: 
      R1 = [instruction(cmt_diffnum_real_real, (Real1, Real2, I), 1) | R1'] |
	auxiliary#escape(Real1, I1, I1', R1', R1''),
	auxiliary#escape(Real2, I1', [RegE(I) | I2], R1'',
			 [increment(1) | R2]);

    Code = FCP_dg_increment_and_commit, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(increment_and_commit, (I, J), 3) |R2] ;

    Code = FCP_dg_decrement_and_commit, I1 = [RegE(I), RegE(J) | I2^],
    integer(I), integer(J):
      R1 = [instruction(decrement_and_commit, (I, J), 3) |R2] ;

    Code = FCP_dg_deref_integer1, I1 = [RegE(I),  Reference1, Reference2 | I2^],
    integer(I):
      R1 = [instruction(deref_integer, (I, else(Address1),
                                        variable(Address2)), 2),
        escape(Reference1, Address1),
        escape(Reference2, Address2)
        | R2];
/* ... */

    otherwise :
      R1 = [instruction(Predicate, Else, N),
	    escape(Ref1, Address1), escape(Ref2, Address2)
	   | R2] |
	auxiliary # guard(Code, Predicate, N, I1, [Ref1, Ref2 | I2]),
	guard_else(Address1, Address2, Else).

guard_else(Address1, Address2, Else) :-

    Address1 = Address2 :
      Else = else(Address1) ;

    Address1 =\= Address2 :
      Else = (fail(Address1), suspend(Address2)) .


assignments(AOP, I1, I2, R1, R2, N, Mnemonic, Assignments) :-

    N > 0,
    N' := N - 1 :
      Assignments ! Pair,
      R1 ! increment(K') |
	assignments,
	instruction(AOP, I1, I1', [_(Mnemonic, Pair, K) | R1'], R1''),
	K' := K - 1;

    N = 0 : AOP = _, Mnemonic = _,
      Assignments = [],
      I2 = I1,
      R1 = R2 .


allocated_regs(I1, I2, N, Regs) :-

    N > 0, I1 ? RegE(I),
    integer(I),
    N' := N - 1 :
      Regs ! I |
	allocated_regs;

    N = 0 :
      Regs = [],
      I2 = I1.
