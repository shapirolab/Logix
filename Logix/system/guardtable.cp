/*

Table of guard kernel predicates for FCP Abstract Instruction Set

Last update by 		$Author: bill $
	       		$Date: 1999/07/09 07:02:55 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/system/guardtable.cp,v $

Copyright 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([dictionary/1]).
-mode(trust).
-include(dg_opcodes).
-language([evaluate, compound, colon]).

FCP_dg_true => 0.
FCP_dg_fail => 0.

/*
** guard_index(functor/arity, index)
**
** guard_analysis(functor/arity, 
**		      ask(pattern, pre-conditions, post-conditions, temps))
** or		     tell(pattern, pre-conditions, post-conditions, temps))
** or		   either(pattern, pre-conditions, post-conditions, temps))
** or		  neither(fail, [1=2], [], []))
*/

Guard(Id, Index, Analysis) => (
    In ? guard_index(Id, Value) :
      Value = Index |
	self;
    In ? guard_analysis(Id, Reply) :
      Analysis |
	self
).

Ask(Skeleton) =>
      (Operation = Skeleton, Reply = ask(Operation, [Operation], [], [])).

Ask(Op, Pre, Post, Temp) =>
      (Reply = ask(Op, Pre, Post, Temp)).

Tell(Skeleton) =>
      (Operation = Skeleton, Reply = tell(Operation, [Operation], [], [])).

Tell(Op, Pre, Post, Temp) =>
      (Reply = tell(Op, Pre, Post, Temp)).

Either(Skeleton) =>
      (Operation = Skeleton, Reply = either(Operation, [Operation], [], [])).

dictionary(In) :-

    Guard('<'/2, FCP_dg_lt,
      (Operation = (X < Y),
	    Ask(Operation,
		[Operation],
		[number(X),number(Y)],[]))
    );

    Guard('='/2, FCP_dg_equals, Reply = either(Unify, [Unify], [], []));

    Guard('>'/2, FCP_dg_lt,
 	    Ask(X > Y,
		[Y < X],
		[number(X),number(Y)],[])
    );

    Guard(we/1, FCP_dg_is_we,
	    Ask(we(V),
		[var(V)],
		[var(V)],[])
    );

    Guard(ro/1, FCP_dg_is_ro, Ask(ro(_)));

    Guard('=='/2, FCP_dg_identical, Ask((_==_)));

    Guard('\='/2, FCP_dg_not_identical, Ask((_\=_)));

    Guard('@<'/2, FCP_dg_is_less, Ask((_@<_)));

    Guard('=<'/2, FCP_dg_le,
      (Operation = (X =< Y),
	    Ask(Operation,


		[Operation],
		[number(X),number(Y)],[]))
    );

    Guard('>='/2, FCP_dg_le,
 	    Ask(X >= Y,
		[Y =< X],
		[number(X),number(Y)],[])
    );

    Guard(var/1, FCP_dg_is_var, Ask(var(_)));

    Guard('=:='/2, FCP_dg_wait_equals,
 	    Ask(X=:=Y,[number(X),X=?=Y],[number(Y)],[])
    );

    Guard('=?='/2, FCP_dg_wait_equals, Ask((_=?=_)));

    Guard('=\='/2, FCP_dg_wait_not_equals, Ask((_=\=_)));

    Guard(div/3, FCP_dg_div,
 	    Ask(div(X,Y,Z),
		[div(X,Y,Z)],
		[number(X),number(Y),number(Z)],[])
    );

    Guard(mod/3, FCP_dg_mod,
      (Operation = mod(X,Y,Z),
	    Ask(Operation,
		[Operation],
		[integer(X),integer(Y),integer(Z)],[]))
    );

    Guard(arg/3, FCP_dg_arg,
 	    Ask(arg(N,T,A),
		[arg(N,T,A1),A1=A],
		[integer(N),N>0],[A1])
    );

    Guard(true/0, FCP_dg_true, Reply = either(true, [], [], []));	% ???

    Guard(list/1, FCP_dg_is_list, Ask(list(_)));

    Guard(plus/3, FCP_dg_plus,
      (Operation = plus(X,Y,Z),
	    Ask(Operation,
		[Operation],
		[number(X),number(Y),number(Z)],[]))
    );

    Guard(diff/3, FCP_dg_diff,
      (Operation = diff(X,Y,Z),
	    Ask(Operation,
		[Operation],
		[number(X),number(Y),number(Z)],[]))
    );

    Guard(real/1, FCP_dg_is_real,
      (Operation = real(X),
	    Ask(Operation,
		[Operation],
		[number(X)],[]))
    );

    Guard(melt/3, FCP_dg_melt, Tell(melt(_,_,_)));

    Guard(fail/0, FCP_dg_fail, Reply = either(fail,[1=2],[],[]));	% ???

    Guard(info/2, FCP_dg_info,
 	    Ask(info(I,X),
		[info(I,X1),X1=X],
		[integer(I)], [X1])
    );

    Guard(link/2, FCP_dg_link, Tell(link(_,_)));

    Guard(arity/2, FCP_dg_arity,
 	    Ask(arity(T,A),
		[arity(T,A1), A1=A],
		[known(A),integer(A),0<A],[A1])
    );

    Guard(tuple/1, FCP_dg_is_tuple, Ask(tuple(_)));

    Guard(times/3, FCP_dg_times,
      (Operation = times(X,Y,Z),
	    Ask(Operation,
		[Operation],
		[number(X),number(Y),number(Z)],[]))
    );

    Guard(debug/2, FCP_dg_debug, Either(debug(_,_)));

    Guard(known/1, FCP_dg_is_known, Ask(known(_)));

    Guard(string/1, FCP_dg_is_string,
      (Operation = string(X),
	    Ask(Operation,
		[Operation],
		[constant(X)],[]))
    );

    Guard(nonvar/1, FCP_dg_is_nonvar,
      (Operation = nonvar(X),
	    Ask(Operation,
		[Operation],
		[known(X)],[]))
    );

    Guard(module/1, FCP_dg_is_module,
      (Operation = module(X),
	    Ask(Operation,
		[Operation],
		[string(X)],[]))
    );

    Guard(vector/1, FCP_dg_is_vector,
      (Operation = vector(X),
	    Ask(Operation,
		[Operation],
		[compound(X)],[]))
    );

    Guard(number/1, FCP_dg_is_number,
      (Operation = number(X),
	    Ask(Operation,
		[Operation],
		[constant(X)],[]))
    );

    Guard(freeze/3, FCP_dg_freeze,
 	    Ask(freeze(T, FT, FVL),
		[freeze(T, [], FT1, FVL1), FT1=FT, FVL1=FVL],
		[],[FT1, FVL1])
    );

    Guard(freeze/4, FCP_dg_freeze,
 	    Ask(freeze(T, Max, FT, FVL),
		[freeze(T, Max, FT1, FVL1), FT1=FT, FVL1=FVL],
		[],[FT1, FVL1])
    );

    Guard(ground/1, FCP_dg_grounded,
      (Operation = ground(X),
	    Ask(Operation,
		[Operation],
		[known(X)],[]))
    );

    Guard(not_we/1, FCP_dg_is_not_we, Ask(not_we(_)));

    Guard(integer/1, FCP_dg_is_integer,
      (Operation = integer(X),
	    Ask(Operation,
		[Operation],
		[number(X)],[]))
    );

    Guard(channel/1, FCP_dg_is_vector,
	    Ask(channel(C),[vector(C)],[],[])
    );

    Guard(unknown/1, FCP_dg_ask_unknown, Ask(unknown(_)));

    Guard(request/2, FCP_dg_request,
      (Operation = request(_,Answer),
	   Tell(Operation,
		[Operation],
		[ro(Answer)],[]))
    );

    Guard(execute/2, FCP_dg_do_execute, Tell(execute(_,_)));

    Guard(dfreeze/4, FCP_dg_dfreeze,
 	    Ask(dfreeze(T, FT, FVL, FTL),
		[dfreeze(T, [], FT1, FVL1, FTL1), FT1=FT, FVL1=FVL, FTL1=FTL],
		[],[FT1, FVL1, FTL1])
    );

    Guard(dfreeze/5, FCP_dg_dfreeze,
 	    Ask(dfreeze(T, OP, FT, FVL, FTL),
		[dfreeze(T, OP, FT1, FVL1, FTL1), FT1=FT, FVL1=FVL, FTL1=FTL],
		[],[FT1, FVL1, FTL1])
    );

    Guard(invalid/1, FCP_dg_invalid_g,
	    Ask(invalid(V),
	     [invalid(V,T1)],
	     [],[T1])
    );

    Guard(invalid/2, FCP_dg_invalid_g,
	    Ask(invalid(V,T),
	 	[invalid(V,T1),T1=T],
	  	[string(T)],[T1])
    );

    Guard(bind_ro/2, FCP_dg_bind_ro_g, Tell(bind_ro(_,_)));

    Guard(writable/1, FCP_dg_is_we,
	    Ask(writable(V),
		[var(V)],
		[var(V)],[])
    );

    Guard(compound/1, FCP_dg_is_compound, Ask(compound(_)));

    Guard(constant/1, FCP_dg_is_constant, Ask(constant(_)));

    Guard(priority/2, FCP_dg_priority, Tell(priority(_, _)));

    Guard(activate/3, FCP_dg_activate,
      (Operation = activate(M,_,_),
	   Tell(Operation,[Operation],[module(M)],[]))
    );

    Guard(nth_char/3, FCP_dg_nth_char,
 	    Ask(nth_char(N,S,C),
		[nth_char(N,S,C1),C1=C],
		[integer(N),N>0,string(S)],[C1])
    );

    Guard(var_info/2, FCP_dg_var_info_g, Ask(var_info(_,_)));

    Guard(read_only/1, FCP_dg_is_ro,
	    Ask(read_only(V),
		[not_we(V)],
		[not_we(V)],[])
    );

    Guard(otherwise/0, FCP_dg_otherwise,
	    Ask(otherwise,[otherwise],[],[])
    );

    Guard(bitwise_or/3, FCP_dg_bitwise_or,
      (Operation = bitwise_or(X,Y,Z),
	    Ask(Operation,
		[Operation],
		[integer(X),integer(Y),known(Z),integer(Z)],[]))
    );

    Guard(code_info/2, FCP_dg_code_info,
	    Ask(code_info(C, I),
		[code_info(C, I1), I1 = I],
		[],[I1])
    );

    Guard(make_tuple/2, FCP_dg_make_tuple,
 	    Ask(make_tuple(N,T),
		[make_tuple(N,T1), T1=T],
		[integer(N),N>0,known(T),tuple(T)],[T1])
    );

    Guard(deschedule/0, FCP_dg_deschedule,
	   Tell(deschedule,[deschedule],[],[])
    );

    Guard(exceptions/1, FCP_dg_exceptions,
  	    Ask(exceptions(L),
		[exceptions(L1), L1 = L],
		[known(L)],[L1])
    );

    Guard(ask_unknown/1, FCP_dg_ask_unknown,
	    Ask(ask_unknown(X),
		[unknown(X)],
		[unknown(X)],[])
    );

    Guard(bitwise_and/3, FCP_dg_bitwise_and,
      (Operation = bitwise_and(X,Y,Z),
	    Ask(Operation,
		[Operation],
		[integer(X),integer(Y),known(Z),integer(Z)],[]))
    );

    Guard(bitwise_not/2, FCP_dg_bitwise_not,
      (Operation = bitwise_not(X,Y),
	    Ask(Operation,
		[Operation],
		[integer(X),known(Y),integer(Y)],[]))
    );

    Guard(string_hash/2, FCP_dg_string_hash,
 	    Ask(string_hash(S,H),
		[string_hash(S,H1), H1 = H],
		[string(S),known(H),integer(H),0=<H,H=<255],[H1])
    );

    Guard(ttyput_byte/1, FCP_dg_ttyput_byte,
      (Operation = ttyput_byte(B),
	   Tell(Operation,
		[Operation],
		[integer(B)],[]))
    );

    Guard(read_vector/3, FCP_dg_read_vector,
 	    Ask(read_vector(N,Ref,Ref'),
		[read_vector(N,Ref,Ref1), Ref1 = Ref'],
		[integer(N),0<N,vector(Ref),vector(Ref')],[Ref1])
    );

    Guard(make_vector/3, FCP_dg_make_vector,
      (Operation = make_vector(N,Ref,Out),
	   Tell(Operation,
		[Operation],
		[integer(N),0<N,vector(Ref),tuple(Out)],[]))
    );
    Guard(make_channel/2, FCP_dg_make_vector,
 	   Tell(make_channel(Ref,Out),
		[make_vector(1,Ref,{Out})],
		[channel(Ref)],[])
    );

    Guard(make_shared/3, FCP_dg_make_shared_g, Tell(make_shared(_,_,_)));

    Guard(make_invalid/2, FCP_dg_make_invalid_g, Tell(make_invalid(_,_)));

    Guard(store_vector/3, FCP_dg_store_vector,
 	   Tell(store_vector(N,Element,Ref),
		[store_vector(N,Element,Ref,Ref')],
		[integer(N),0<N,vector(Ref)],[Ref'])
    );

    Guard(store_vector/4, FCP_dg_store_vector,
      (Operation = store_vector(N,_,Ref,Ref'),
	   Tell(Operation,
		[Operation],
		[integer(N),0<N,vector(Ref),vector(Ref')],[]))
    );

    Guard(write_vector/3, FCP_dg_write_vector,
 	   Tell(write_vector(N,Element,Ref),
		[write_vector(N,Element,Ref,Ref')],
		[integer(N),0<N,vector(Ref)],[Ref'])
    );

    Guard(write_vector/4, FCP_dg_write_vector,
      (Operation = write_vector(N,_,Ref,Ref'),
	   Tell(Operation,
		[Operation],
		[integer(N),0<N,vector(Ref),vector(Ref')],[]))
    );

    Guard(close_vector/2, FCP_dg_close_vector,
      (Operation = close_vector(N,Ref),
	   Tell(Operation,
		[Operation],
		[integer(N),0<N,vector(Ref)],[]))
    );

    Guard(string_length/2, FCP_dg_string_length,
 	    Ask(string_length(S,L),
		[string_length(S,L1), L1 = L],
		[string(S),known(L),integer(L),0=<L],[L1])
    );

    Guard(ttyput_string/1, FCP_dg_ttyput_string,
      (Operation = ttyput_string(S),
	   Tell(Operation,
		[Operation],
		[string(S)],[]))
    );

    Guard(write_channel/2, FCP_dg_write_vector,
 	   Tell(write_channel(Element,Ref),
		[write_vector(1,Element,Ref,Ref')],
		[channel(Ref)],[Ref'])
    );

    Guard(write_channel/3, FCP_dg_write_vector,
 	   Tell(write_channel(Element,Ref,Ref'),
		[write_vector(1,Element,Ref,Ref')],
		[channel(Ref),channel(Ref')],[])
    );

    Guard(close_channel/1, FCP_dg_close_vector,
 	   Tell(close_channel(Ref),
		[close_vector(1,Ref)],
		[channel(Ref)],[])
    );

    Guard(make_unshared/1, FCP_dg_make_unshared_g, Tell(make_unshared(_)));

    Guard(list_to_string/3, FCP_dg_list_to_string,
	    Ask(list_to_string(Size,L,S),
		[list_to_string(Size,L,S1), S1 = S],
		[integer(Size),known(S),string(S)],[S1])
    );

    Guard(list_to_string/2, FCP_dg_list_to_string,
 	    Ask(list_to_string(L,S),
		[list_to_string(0,L,S1), S1 = S],
		[known(S),string(S)],[S1])
    );

    Guard(ttyput_integer/1, FCP_dg_ttyput_integer,
      (Operation = ttyput_integer(I),
	   Tell(Operation,
		[Operation],
		[integer(I)],[]))
    );

    Guard(machine_output/1, FCP_dg_machine_output,
      (Operation = machine_output(S),
	   Tell(Operation,
		[Operation],
		[ro(S)],[]))
    );

    Guard(string_to_dlist/3, FCP_dg_string_to_dlist,
 	    Ask(string_to_dlist(S,H,T),
		[string_to_dlist(S,H1,T), H1 = H],
		[string(S)],[H1])
    );

    Guard(convert_to_real/2, FCP_dg_cnv_to_real,
 	    Ask(convert_to_real(V,R),
		[convert_to_real(V,R1), R1 = R],
		[constant(V),known(R),real(R)],[R1])
    );

    Guard(convert_to_string/2, FCP_dg_cnv_to_string,
 	    Ask(convert_to_string(V,S),
		[convert_to_string(V,S1), S1 = S],
		[constant(V),known(S),string(S)],[S1])
    );

    Guard(convert_to_integer/2, FCP_dg_cnv_to_integer,
 	    Ask(convert_to_integer(V,I),
		[convert_to_integer(V,I1), I1 = I],
		[constant(V),known(I),integer(I)],[I1])
    );

    In ? guard_index(_Other, Index),
    otherwise :
      Index = 0 |
	self;

    In ? guard_analysis(_Other, Reply),
    otherwise :
      Reply = neither(fail, [1=2], [], []) |
	self;

    In =\= [_|_], In =\= [] :
      In'' = [In] |
	self;

    In = [] | true.

/*
 *		Indices In Use
 *
 *   -   1   2   3   4   5   6   7   8   9
 *  10  11  12  13  14  15  16  17  18  19
 *  20  21  22  23  24  25  26  27  28  29
 *  30  31  32  33  34  35  36  37  38  39
 *  40  41  42  43 x44  45  46   -   -   -
 *  50  51  52  53  54  55  56  57  58  59
 *  60  61  62   -   -   -   -   -   -   -
 *   -  71  72  73  74  75   _   _   -   -
 *   -  81  82  83  84  85  86  87   -   -
 *  90  91  92   -   -   -   -   -   -   -
 * 160         243 244x245 246
 *
 *
 *	Anomalies		Obsolescent
 *
 *   var   writable      ~  2	ask_unknown/1
 *   ro   read_only      ~  3	tell_unknown/1
 *   =?=/2 =  =:=/2      ~ 12	\= /2
 *   </2   =   >/2       ~ 20
 *   =</2  =  >=/2       ~ 21
 *   unknown ask_unknown ~ 43
 *   true/0              ~ 58	
 */
