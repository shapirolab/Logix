/* $Header: /home/qiana/Repository/Logix/system/decode/auxiliary.cp,v 1.1.1.1 1999/07/09 07:03:31 bill Exp $ */
-language([evaluate,compound,colon]).
-mode(trust).
-export([labels/6, arguments/6, escape/5, guard/5, do_vars/4]).
-include(dg_opcodes).
-include(escapes).

procedure labels(Labels, Items, Items, RQs, RQs, Count).

labels(Labels, I1, I2, R1, R2, N) :-

    N > 0, I1 ? Reference,
    N' := N - 1 :
      R1 ! escape(Reference, Address),
      Labels ! Address |
	labels;

    N = 0 :
      I1 = I2,
      R1 = R2,
      Labels = [] .


procedure arguments(Arguments, Items, Items, RQs, RQs, Count).

arguments(Args, I1, I2, R1, R2, N) :-

    N > 0,
    N' := N - 1 :
      Args ! Arg |
	argument(Arg, I1, I1', R1, R1'),
	arguments;

    N = 0 :
      Args = [],
      I1 = I2,
      R1 = R2.


procedure escape(Argument, Items, Items, RQs, RQs).

escape(Arg, I1, I2, R1, R2) :-

    I1 ? String, String = {StringE, _} :
      I2 = I1',
      Arg = string(S),
      R1 = [escape(String, S) | R2] ;

    I1 ? Integer, Integer = {IntegerE, _} :
      I2 = I1',
      Arg = integer(I),
      R1 = [escape(Integer, I) | R2] ;

    I1 ? Tuple, Tuple = {TupleE, _} :
      I2 = I1',
      Arg = tuple(T),
      R1 = [escape(Tuple, T) | R2] ;

    I1 ? Real, Real = {RealE, _} :
      I2 = I1',
      Arg = real(R),
      R1 = [escape(Real, R) | R2] .      
      

procedure argument(Argument, Items, Items, RQs, RQs).

argument(Arg, I1, I2, R1, R2) :-

    I1 = [FCP_dg_load_reg, RegE(I) | I2^], integer(I) :
      Arg = a(I),
      R1 = [increment(2) | R2] ;

    I1 = [FCP_dg_load_pr_arg, PRegE(I) | I2^], integer(I) :
      Arg = "PR"(I),
      R1 = [increment(2) | R2] ;

    I1 = [FCP_dg_load_reg_indirect, RegE(I) | I2^], integer(I) :
      Arg = "*a"(I),
      R1 = [increment(2) | R2] ;

    I1 = [FCP_dg_load_subarg, RegE(I), IndexE(IX) | I2^],
    integer(I), integer(IX) :
      Arg = a(I, IX),
      R1 = [increment(3) | R2] ;

    I1 = [FCP_dg_load_ro_of_reg, RegE(I) | I2^], integer(I) :
      Arg = ro(I),
      R1 = [increment(2) | R2] ;

    I1 = [FCP_dg_load_ref_to_ro_of_reg, RegE(I) | I2^], integer(I) :
      Arg = ro(I),
      R1 = [increment(2) | R2] ;

    I1 = [FCP_dg_load_car_of_reg, RegE(I) | I2^], integer(I) :
      Arg = car(I),
      R1 = [increment(2) | R2] ;

    I1 = [FCP_dg_load_ref_to_subarg, RegE(I), IndexE(IX) | I2^],
    integer(I), integer(IX) :
      Arg = "&a"(I, IX),
      R1 = [increment(3) | R2] ;

    I1 = [FCP_dg_load_ro_of_subarg, RegE(I), IndexE(IX) | I2^],
    integer(I), integer(IX) :
      Arg = "&"(ro(I, IX)),
      R1 = [increment(3) | R2] ;

    I1 = [FCP_dg_load_ref_to_ro_of_subarg, RegE(I), IndexE(IX) | I2^],
    integer(I), integer(IX) :
      Arg = "&"(ro(I, IX)),
      R1 = [increment(3) | R2] ;

    I1 = [FCP_dg_load_nil | I2^] :
      Arg = [],
      R1 = [increment(1) | R2] ;

    I1 = [FCP_dg_load_word, Integer | I2^] :
      Arg = integer(I),
      R1 = [increment(1), escape(Integer, I) | R2] ;

    I1 = [FCP_dg_load_real, Real | I2^] :
      Arg = real(R),
      R1 = [increment(1), escape(Real, R) | R2] ;

    I1 = [FCP_dg_load_ref_to_real, Real | I2^] :
      Arg = real(R),
      R1 = [increment(1), escape(Real, R) | R2] ;

    I1 = [FCP_dg_load_ref_to_string, String | I2^] :
      Arg = string(S),
      R1 = [increment(1), escape(String, S) | R2] ;

    I1 = [FCP_dg_load_we_var | I2^] :
      Arg = we,
      R1 = [increment(1) | R2] ;

    I1 = [FCP_dg_load_ref_to_we_var | I2^] :
      Arg = we,
      R1 = [increment(1) | R2] .


procedure guard(Index, (String ; Tuple), Integer, Items, Items).

guard(FCP_dg_otherwise, otherwise^, 1^, Items, Items^).
guard(FCP_dg_deschedule, deschedule^, 1^, Items, Items^).

guard(FCP_dg_is_ro, ro(I)^, 2^, [{RegE, I} | Items], Items^).

guard(FCP_dg_is_var, var(I)^, 2^, [{RegE, I} | Items], Items^).

guard(FCP_dg_is_list, list(I)^, 2^, [{RegE, I} | Items], Items^).
guard(FCP_dg_is_real ,real(I)^, 2^, [{RegE, I} | Items], Items^).

guard(FCP_dg_is_known, known(I)^, 2^, [{RegE, I} | Items], Items^).
guard(FCP_dg_is_tuple, tuple(I)^, 2^, [{RegE, I} | Items], Items^).
guard(FCP_dg_is_string, string(I)^, 2^, [{RegE, I} | Items], Items^).
guard(FCP_dg_grounded, ground(I)^, 2^, [{RegE, I} | Items], Items^).

guard(FCP_dg_is_nonvar, nonvar(I)^, 2^, [{RegE, I} | Items], Items^).
guard(FCP_dg_is_module, module(I)^, 2^, [{RegE, I} | Items], Items^).
guard(FCP_dg_is_vector, vector(I)^, 2^, [{RegE, I} | Items], Items^).
guard(FCP_dg_is_number, number(I)^, 2^, [{RegE, I} | Items], Items^).
guard(FCP_dg_ask_unknown, unknown(I)^, 2^, [{RegE, I} | Items], Items^).

guard(FCP_dg_is_integer, integer(I)^, 2^, [{RegE, I} | Items], Items^).
guard(FCP_dg_is_compound, compound(I)^, 2^, [{RegE, I} | Items], Items^).
guard(FCP_dg_is_constant, constant(I)^, 2^, [{RegE, I} | Items], Items^).

guard(FCP_dg_exceptions, exceptions(I)^, 2^, [{RegE, I} | Items], Items^).

guard(FCP_dg_ttyput_byte, ttyput_byte(I)^, 2^, [{RegE, I} | Items], Items^).
guard(FCP_dg_ttyput_string, ttyput_string(I)^, 2^, [{RegE, I} | Items], Items^).
guard(FCP_dg_ttyput_integer, ttyput_integer(I)^, 2^, [{RegE, I} | Items], Items^).
guard(FCP_dg_machine_output, machine_output(I)^, 2^, [{RegE, I} | Items], Items^).

guard(FCP_dg_lt, '<'(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_equals, '='(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_identical, '=='(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_not_identical, '\='(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_is_less, '@<'(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_le, '=<'(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_wait_equals, '=?='(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_wait_not_equals, '=\='(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_info, info(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_link, link(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_arity, arity(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_debug, debug(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_request, request(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_do_execute, execute(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_priority, priority(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_code_info, code_info(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_make_tuple, make_tuple(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_bitwise_not, bitwise_not(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_string_hash, string_hash(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_close_vector, close_vector(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_string_length, string_length(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_bind_ro_g, eventual_unify(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_cnv_to_real, convert_to_real(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_cnv_to_string, convert_to_string(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_cnv_to_integer, convert_to_integer(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).
guard(FCP_dg_make_invalid_g, make_invalid(I, J)^, 3^, [{RegE, I}, {RegE, J} | Items], Items^).

guard(FCP_dg_div, div(I, J, K)^, 4^, [{RegE, I}, {RegE, J}, {RegE, K} | Items], Items^).
guard(FCP_dg_mod, mod(I, J, K)^, 4^, [{RegE, I}, {RegE, J}, {RegE, K} | Items], Items^).
guard(FCP_dg_arg, arg(I, J, K)^, 4^, [{RegE, I}, {RegE, J}, {RegE, K} | Items], Items^).
guard(FCP_dg_plus, plus(I, J, K)^, 4^, [{RegE, I}, {RegE, J}, {RegE, K} | Items], Items^).
guard(FCP_dg_diff, diff(I, J, K)^, 4^, [{RegE, I}, {RegE, J}, {RegE, K} | Items], Items^).
guard(FCP_dg_melt, melt(I, J, K)^, 4^, [{RegE, I}, {RegE, J}, {RegE, K} | Items], Items^).
guard(23, times(I, J, K)^, 4^, [{RegE, I}, {RegE, J}, {RegE, K} | Items], Items^).
guard(FCP_dg_activate, activate(I, J, K)^, 4^, [{RegE, I}, {RegE, J}, {RegE, K} | Items], Items^).
guard(FCP_dg_bitwise_or, bitwise_or(I, J, K)^, 4^, [{RegE, I}, {RegE, J}, {RegE, K} | Items], Items^).
guard(FCP_dg_bitwise_and, bitwise_and(I, J, K)^, 4^, [{RegE, I}, {RegE, J}, {RegE, K} | Items], Items^).
guard(FCP_dg_read_vector, read_vector(I, J, K)^, 4^, [{RegE, I}, {RegE, J}, {RegE, K} | Items], Items^).
guard(FCP_dg_make_vector, make_vector(I, J, K)^, 4^, [{RegE, I}, {RegE, J}, {RegE, K} | Items], Items^).
guard(FCP_dg_var_info_g, ask_shared_var(I, J, K)^, 4^,
	[{RegE, I}, {RegE, J}, {RegE, K} | Items], Items^
).
guard(FCP_dg_list_to_string, list_to_string(I, J, K)^, 4^,
	[{RegE, I}, {RegE, J}, {RegE, K} | Items], Items^
).
guard(FCP_dg_string_to_dlist, string_to_dlist(I, J, K)^, 4^,
	[{RegE, I}, {RegE, J}, {RegE, K} | Items], Items^
).
guard(FCP_dg_invalid_g, I^, 2^, [{RegE, I} | Items], Items^).
guard(FCP_dg_make_unshared_g, mak_unshared_var(I, J, K)^, 4^,
	[{RegE, I}, {RegE, J}, {RegE, K} | Items], Items^
).

guard(FCP_dg_freeze, freeze(I, J, K,L)^, 5^,
	[{RegE, I}, {RegE, J}, {RegE, K}, {RegE, L} | Items], Items^
).
guard(FCP_dg_store_vector, store_vector(I, J, K,L)^, 5^,
	[{RegE, I}, {RegE, J}, {RegE, K}, {RegE, L} | Items], Items^
).
guard(FCP_dg_write_vector, write_vector(I, J, K,L)^, 5^,
	[{RegE, I}, {RegE, J}, {RegE, K}, {RegE, L} | Items], Items^
).

procedure do_vars(N, Registers, Items, Items).

do_vars(N, Registers, TailItems, Items) :-

    N > 0, N' := N - 1,
    Items = [RegE(I) | Items'] :
      Registers ! I |
	do_vars;

    N = 0 :
      Items = TailItems,
      Registers = [].
