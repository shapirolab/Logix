/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctlopt/peephole.cp,v 1.1.1.1 1999/07/09 07:03:05 bill Exp $ */
-language(compound).
-export([peephole/3]).
-mode(trust).

procedure peephole(Instructions, Optimized, SC).

peephole(Instructions, Optimized, SC) 
:-
	SC = {L,R} |
	optimized_basic_block(Instructions, BasicBlocksOpt, {L,M}),
	peephole_instructions(BasicBlocksOpt, Optimized, {M,R}).

peephole_instructions(Instructions, Optimized, SC)
:-
	Instructions ? I1, I1 = multiple_assign([a(J)],[a(K)]),
	Instructions' ? I2, I2 = multiple_copy([{'&',{a(J),1}}],[a(K)]) :
	Optimized ! assign_and_increment(a(J),a(K)) |
	peephole_instructions ;

	Instructions ? I1, I1 = multiple_assign_and_commit([a(J)],[a(K)]),
	Instructions' ? I2, I2 = multiple_copy([{'&',{a(J),1}}],[a(K)]) :
	Optimized ! assign_increment_and_commit(a(J),a(K)) |
	peephole_instructions ;

	Instructions ? I1, 
	I1 = multiple_assign_and_commit_trail([a(J)],[a(K)]),
	Instructions' ? I2, I2 = multiple_copy([{'&',{a(J),1}}],[a(K)]) :
	Optimized ! assign_increment_and_commit_trail(a(J),a(K)) |
	peephole_instructions ;

	Instructions ? I1, I1 = multiple_assign([a(J)],[a(K)]),
	Instructions' ? I2, I2 = increment_pointer(a(J),a(K)) :
	Optimized ! assign_and_increment(a(J),a(K)) |
	peephole_instructions ;

	Instructions ? I1, I1 = multiple_assign_and_commit([a(J)],[a(K)]),
	Instructions' ? I2, I2 = increment_pointer(a(J),a(K)) :
	Optimized ! assign_increment_and_commit(a(J),a(K)) |
	peephole_instructions ;

	Instructions ? I1, 
	I1 = multiple_assign_and_commit_trail([a(J)],[a(K)]),
	Instructions' ? I2, I2 = increment_pointer(a(J),a(K)) :
	Optimized ! assign_increment_and_commit_trail(a(J),a(K)) |
	peephole_instructions ;

	Instructions ? I1, I1 = multiple_assign([a(J)],[a(K)]),
	Instructions' ? I2, I2 = multiple_copy([{'&',{a(J),1}}|S],[a(K)|D]),
	S =\= [] :
	Optimized ! assign_and_increment(a(J),a(K)),
	Optimized' ! multiple_copy(S,D) |
	peephole_instructions ;

	Instructions ? I1, I1 = multiple_assign_and_commit([a(J)],[a(K)]),
	Instructions' ? I2, I2 = multiple_copy([{'&',{a(J),1}}|S],[a(K)|D]),
	S =\= [] :
	Optimized ! assign_increment_and_commit(a(J),a(K)),
	Optimized' ! multiple_copy(S,D) |
	peephole_instructions ;

	Instructions ? I1, 
	I1 = multiple_assign_and_commit_trail([a(J)],[a(K)]),
	Instructions' ? I2, I2 = multiple_copy([{'&',{a(J),1}}|S],[a(K)|D]) :
	Optimized ! assign_increment_and_commit_trail(a(J),a(K)),
	Optimized' ! multiple_copy(S,D) |
	peephole_instructions ;

	Instructions ? I1, I1 = multiple_copy(Srcs,Dests) |
	reduce_mult_copy_to_incr_ptr(Srcs, Dests, Optimized, 
					Optimized', SrcsO, DestO),
	eliminate_mult_copy(SrcsO, DestO, Optimized', Optimized''),
	peephole_instructions ;

	Instructions = [], SC = {L,R} : Optimized = [], L = R ;
	
/*
	otherwise, Instructions ? I, Instructions' ? I1,
	I = multiple_assign_and_commit(_,_) : 
	Optimized ! I, Optimized' ! I1 | 
	screen#display({I,I1},type(ground)),
	peephole_instructions ;
*/

	otherwise, Instructions ? I : Optimized ! I |  peephole_instructions.

reduce_mult_copy_to_incr_ptr(Srcs, Dests, Optimized, OptimizedO, SrcsO, DestsO)
:-
	Srcs ? {'&',{a(I),1}}, Dests ? a(J), I =\= J :
	Optimized ! increment_pointer(a(I),a(J)) |
	reduce_mult_copy_to_incr_ptr ;

	Srcs ? {'&',{a(I),1}}, Dests ? a(I) :
	Optimized ! increment_pointer(a(I)) |
	reduce_mult_copy_to_incr_ptr ;

	Srcs = [] : OptimizedO = Optimized, SrcsO = [], DestsO = [], Dests = _;

	otherwise :
	OptimizedO = Optimized, SrcsO = Srcs, DestsO = Dests.
	
eliminate_mult_copy(Srcs, Dests, Optimized, OptimizedT)
:-
	Srcs = [] : OptimizedT = Optimized, Dests = _ ;
	Srcs =\= [] : Optimized = [multiple_copy(Srcs,Dests)|OptimizedT].

optimized_basic_block(Instructions, Optimized, SC) 
:-
	Instructions ? I, I = label(_), SC = {L,R} : SC' = {M1,R} |
	basic_block(Instructions', Block, Type, Ar, Instructions'', {L,M}),
	iterate_basic_block([I|Block], Type, Ar, Optimized, Optimized', {M,M1}),
	optimized_basic_block ;

	Instructions ? I, I = body_label, SC = {L,R} : SC' = {M1,R} |
	basic_block(Instructions', Block, Type, Ar, Instructions'', {L,M}),
	iterate_basic_block([I|Block], Type, Ar, Optimized, Optimized', {M,M1}),
	optimized_basic_block ;

	Instructions ? I, I = iterative_label, SC = {L,R} : SC' = {M1,R} |
	basic_block(Instructions', Block, Type, Ar, Instructions'', {L,M}),
	iterate_basic_block([I|Block], Type, Ar, Optimized, Optimized', {M,M1}),
	optimized_basic_block ;

	Instructions = [I|_], 
	I =\= label(_), I =\= body_label, I =\= iterative_label,
	SC = {L,R} : SC' = {M1,R} |
	basic_block(Instructions, Block, Type, Ar, Instructions', {L,M}),
	iterate_basic_block(Block, Type, Ar, Optimized, Optimized', {M,M1}),
	optimized_basic_block ;

	Instructions = [], SC = {L,R} : Optimized = [], L = R.

basic_block(Instructions, Block, Type, Ar, InstTail, SC)+(St=continue)
:-
	St = continue, Instructions = [label(_)|_], SC = {L,R} :
	Block = [], Type = label, InstTail = Instructions, L = R ;

	St = continue, Instructions = [body_label|_], SC = {L,R} :
	Block = [], Type = label, InstTail = Instructions, L = R ;

	St = continue, Instructions = [iterative_label|_], SC = {L,R} :
	Block = [], Type = label, InstTail = Instructions, L = R ;

	St = continue, Instructions ? L, 
	L =\= label(_), L =\= body_label, L =\= iterative_label, 
	arg(1,L,Op), arg(2,L,Pr) :
	Block ! L |
	check_op(Op, St', Pr, Ar),
	basic_block ;
	
	St = continue, Instructions ? L, 
	string(L), L =\= commit :
	Block ! L |
	check_op(L, St', _, _),
	basic_block ;
	
	St = continue, Instructions ? L, 
	L = commit :
	Block ! L |
	basic_block ;

	St =\= continue, SC = {L,R} : 
	Block = [], Type = St, InstTail = Instructions, L = R.

check_op(Op, St, Pr, Ar)
:-
	Op = deref_value : St = continue ;	
	Op = deref_list : St = deref ;	
	Op = deref_car_list : St = deref ;
	Op = deref_car : St = continue ;	
	Op = deref_integer : St = deref ;
	Op = deref_vars : St = deref ;
	Op = deref	: St = continue ;

	Op = allocate_list : St = continue ;
	Op = allocate_tuple : St = continue ;
	Op = allocate_var : St = continue ;
	Op = allocate_pr : St = continue ;

	Op = branch_integer : St = conditional ;
	Op = branch_tuple : St = conditional ;
	Op = case_hash_integer : St = conditional ;
	Op = case_hash_real : St = conditional ;
	Op = case_hash_string : St = conditional ;
	Op = compare_integer : St = conditional ;
	Op = if_integer : St = conditional ;
	Op = if_real : St = conditional ;
	Op = if_string : St = conditional ;
	Op = if_tag : St = conditional ;
	Op = if_tuple : St = conditional ;
	Op = switch_on_tag : St = conditional ;
	Op = call : St = conditional ;
	Op = unify : St = conditional ;

	Op = decrement_pointer : St = continue ;

	Op = decrement : St = continue ;
	Op = increment : St = continue ;
	Op = decrement_and_commit : St = continue ;
	Op = increment_and_commit : St = continue ;
	Op = plus : St = continue ;
	Op = plus_number : St = continue ;
	Op = diff : St = continue ;
	Op = diff_number : St = continue ;
	Op = mult : St = continue ;
	Op = mult_number : St = continue ;
	Op = div_number : St = continue ;
	Op = plus_and_commit : St = continue ;
	Op = plus_number_commit : St = continue ;
	Op = diff_and_commit : St = continue ;
	Op = diff_number_commit : St = continue ;
	Op = mult_and_commit : St = continue ;
	Op = mult_number_commit : St = continue ;
	Op = div_and_commit : St = continue ;
	Op = div_number_commit : St = continue ;
	Op = load_car : St = continue ;
	Op = 'div' : St = continue ;

	Op = deschedule : St = stop ;
	Op = enqueue : St = continue ;
	Op = execute, Pr = {_,A,_,_} : Ar = A, St = iterate ;
	Op = goto : St = goto ;
	Op = halt : St = stop ;
	Op = iterate, Pr = {_,A,_,_} : Ar = A, St = iterate ;

	Op = copy : St = continue ;
	Op = multiple_copy : St = continue ;
	Op = multiple_assign : St = continue ;
	Op = multiple_assign_and_commit : St = continue ;
	Op = multiple_assign_and_commit_trail : St = continue ;

	Op = suspend : St = suspend ;
	Op = suspend_on : St = continue ;

	Op = commit : St = conditional ;
	Op = fetch : St = continue ;
	Op = set_cp_arity : St = continue ;
	Op = set_HBT : St = continue ;
	Op = undo : St = continue.
 
iterate_basic_block(Block, Type, Ar, Optimized, OptTail, SC)
:-
	Type = iterate, 
	Block = [allocate_list([Arg|{_}],a(J)),
		 multiple_assign([a(J)],[a(K)]),
		 multiple_copy([{'&',{a(J),1}}],[a(K)]),
		 Iter],
	SC = {L,R} :
	Optimized = [list_assign_with_check(Arg,a(K)),Iter|OptTail],
	L = R ;

	Type = iterate, 
	Block = [label(Lab1),
		 allocate_list([Arg|{_}],a(J)),
		 multiple_assign([a(J)],[a(K)]),
		 multiple_copy([{'&',{a(J),1}}],[a(K)]),
		 Iter],
	SC = {L,R} :
	Optimized = [label(Lab1),
		     list_assign_with_check(Arg,a(K)),
		     Iter|OptTail],
	L = R ;

	Type = iterate, 
	Block = [multiple_assign([a(J)],[a(K)]),
		 multiple_copy([{'&',{a(J),1}}],[a(K)]),
		 Iter],
	SC = {L,R} :
	Optimized = [assign_and_increment(a(J),a(K)),
		     Iter|OptTail],
	L = R ;

	Type = iterate, 
	Block = [multiple_assign_and_commit([a(J)],[a(K)]),
		 multiple_copy([{'&',{a(J),1}}],[a(K)]),
		 Iter],
	SC = {L,R} :
	Optimized = [assign_increment_and_commit(a(J),a(K)),
		     Iter|OptTail],
	L = R ;

	Type = iterate, 
	Block = [multiple_assign_and_commit_trail([a(J)],[a(K)]),
		 multiple_copy([{'&',{a(J),1}}],[a(K)]),
		 Iter],
	SC = {L,R} :
	Optimized = [assign_increment_and_commit_trail(a(J),a(K)),
		     Iter|OptTail],
	L = R ;

	Type =\= iterate |
	copy_block(Block, Optimized, OptTail, SC) ;

	otherwise : Type = _ |
	break_block(Block, Rest, MultCopy, Iterate),
%	screen#display({Block,Rest,MultCopy,Iterate,Optimized},type(ground)),
	optimize_block(Block, Rest, Ar, 
				MultCopy, Iterate, Optimized, OptTail, SC).

optimize_block(Block, Rest, Ar, MultCopy, Iterate, Optimized, OptTail, SC)
:-
	MultCopy = none : Rest = _, Iterate = _ |
%	screen#display(Block,type(ground)),
	copy_block(Block, Optimized, OptTail, SC) ;

	MultCopy =\= none : Block = _ |
	allocate_list(Rest, MultCopy, Rest', MultCopy'),
	optimized_block(Rest', Ar, MultCopy', Optimized, [Iterate|OptTail], SC).

copy_block(Block, Optimized, OptTail, SC)
:-
	Block ? L : Optimized ! L | copy_block ;
	Block = [], SC = {L,R}  : Optimized = OptTail, L = R.

break_block(Block, Rest, MultCopy, Iterate)
:-
	Block ? I, I = label(_) :
	Inst = {I,[],[]}, Rest ! Inst |
	break_block ;

	Block ? I, I = iterative_label :
	Inst = {I,[],[]}, Rest ! Inst |
	break_block ;
	
	Block ? I, I = body_label :
	Inst = {I,[],[]}, Rest ! Inst |
	break_block ;

	Block ? I, I = deref_value(A,B) :
	Inst = {I,Use,[B]}, Rest ! Inst |
	get_use(A, Use, []),
	break_block ;

	Block ? I, I = deref_car(A,B) :
	Inst = {I,Use,[B]}, Rest ! Inst |
	get_use(A, Use, []),
	break_block ;

	Block ? I, I = load_car(A,B) :
	Inst = {I,Use,[B]}, Rest ! Inst |
	get_use(A, Use, []),
	break_block ;

	Block ? I, I = deref_car(A,B,C) :
	Inst = {I,Use,[B,C]}, Rest ! Inst |
	get_use(A, Use, []),
	break_block ;

	Block ? I, I = deref(A,B) :
	Inst = {I,Use,[B]}, Rest ! Inst |
	get_use(A, Use, []),
	break_block ;

	Block ? I, I = deref(A,B,C) :
	Inst = {I,Use,[B,C]}, Rest ! Inst |
	get_use(A, Use, []),
	break_block ;

	Block ? I, I = allocate_var(A) : 
	Inst = {I, [], [A]}, Rest ! Inst | 
	break_block ;

	Block ? I, I = allocate_list(List, a(J)) :
	Inst = {I, Use, [a(J)]}, Rest ! Inst |
	get_uses(List, Use),
	break_block ;

	Block ? I, I = {Op,Srcs,Dests}, Op = multiple_assign |
	break_multiple(Op, Srcs, Dests, Rest, Rest'),
	break_block ;

	Block ? I, I = {Op,Srcs,Dests}, Op = multiple_assign_and_commit |
	break_multiple(Op, Srcs, Dests, Rest, Rest'),
	break_block ;

	Block ? I, I = {Op,Srcs,Dests}, 
	Op = multiple_assign_and_commit_trail |
	break_multiple(Op, Srcs, Dests, Rest, Rest'),
	break_block ;

	Block ? I, I = allocate_tuple(Tuple, a(J)) :
	Inst = {I,Use,[a(J)]}, Rest ! Inst |
	get_uses(Tuple, Use),
	break_block ;

	Block ? I, I = allocate_pr(Allocs, _) :
	Inst = {I,Use,[]}, Rest ! Inst |
	get_uses(Allocs, Use),
	break_block ;

	Block ? I, I = decrement_pointer(Reg1,Reg2) :
	Inst = {I,[Reg1],[Reg2]}, Rest ! Inst |
	break_block ;

	Block ? I, I = decrement_pointer(Reg) :
	Inst = {I,[Reg],[Reg]}, Rest ! Inst |
	break_block ;

	Block ? I, I = decrement(Src, Dest) :
	Inst = {I,[Src],Define}, Rest ! Inst |
	get_use(Dest, Define, []),
	break_block;

	Block ? I, I = increment(Src, Dest) :
	Inst = {I,[Src],Define}, Rest ! Inst |
	get_use(Dest, Define, []),
	break_block;

	Block ? I, I = decrement_and_commit(Src, Dest) :
	Inst = {I,[Src|Define],[]}, Rest ! Inst |
	get_use(Dest, Define, []),
	break_block;

	Block ? I, I = increment_and_commit(Src, Dest) :
	Inst = {I,[Src|Define],[]}, Rest ! Inst |
	get_use(Dest, Define, []),
	break_block;

	Block ? I, I = plus(A,B,C) :
	Inst = {I,Use,[C]}, Rest ! Inst |
	get_uses([A,B], Use),
	break_block ;

	Block ? I, I = plus_number(A,B,C) :
	Inst = {I,Use,[]}, Rest ! Inst |
	get_uses([A,B,C], Use),
	break_block ;

	Block ? I, I = plus_number_commit(A,B,C) :
	Inst = {I,Use,[]}, Rest ! Inst |
	get_uses([A,B,C], Use),
	break_block ;

	Block ? I, I = plus_and_commit(A,B,C) :
	Inst = {I,Use,[]}, Rest ! Inst |
	get_uses([A,B,C], Use),
	break_block ;

	Block ? I, I = diff(A,B,C) :
	Inst = {I,Use,[C]}, Rest ! Inst |
	get_uses([A,B], Use),
	break_block ;

	Block ? I, I = diff_number(A,B,C) :
	Inst = {I,Use,[]}, Rest ! Inst |
	get_uses([A,B,C], Use),
	break_block ;

	Block ? I, I = diff_number_commit(A,B,C) :
	Inst = {I,Use,[]}, Rest ! Inst |
	get_uses([A,B,C], Use),
	break_block ;

	Block ? I, I = diff_and_commit(A,B,C) :
	Inst = {I,Use,[]}, Rest ! Inst |
	get_uses([A,B,C], Use),
	break_block ;

	Block ? I, I = mult(A,B,C) :
	Inst = {I,Use,[C]}, Rest ! Inst |
	get_uses([A,B], Use),
	break_block ;

	Block ? I, I = mult_number(A,B,C) :
	Inst = {I,Use,[]}, Rest ! Inst |
	get_uses([A,B,C], Use),
	break_block ;

	Block ? I, I = mult_number_commit(A,B,C) :
	Inst = {I,Use,[]}, Rest ! Inst |
	get_uses([A,B,C], Use),
	break_block ;

	Block ? I, I = mult_and_commit(A,B,C) :
	Inst = {I,Use,[]}, Rest ! Inst |
	get_uses([A,B,C], Use),
	break_block ;

	Block ? I, I = div(A,B,C) :
	Inst = {I,Use,[C]}, Rest ! Inst |
	get_uses([A,B], Use),
	break_block ;

	Block ? I, I = div_number(A,B,C) :
	Inst = {I,Use,[]}, Rest ! Inst |
	get_uses([A,B,C], Use),
	break_block ;

	Block ? I, I = div_number_commit(A,B,C) :
	Inst = {I,Use,[]}, Rest ! Inst |
	get_uses([A,B,C], Use),
	break_block ;

	Block ? I, I = div_and_commit(A,B,C) :
	Inst = {I,Use,[]}, Rest ! Inst |
	get_uses([A,B,C], Use),
	break_block ;

	Block ? I, I = div_number(A,B,C) :
	Inst = {I,Use,[]}, Rest ! Inst |
	get_uses([A,B,C], Use),
	break_block ;

	Block ? I, I = fetch(_,Reg) :
	Inst = {I,[],[Reg]}, Rest ! Inst |
	break_block ;

	Block ? I, I = enqueue(_,_) :
	Inst = {I,[],[]}, Rest ! Inst |
	break_block ;

	Block ? I, I = set_cp_arity(_) :
	Inst = {I,[],[]}, Rest ! Inst |
	break_block ;

	Block ? I, I = set_HBT :
	Inst = {I,[],[]}, Rest ! Inst |
	break_block ;

	Block ? I, I = commit :
	Inst = {I,[],[]}, Rest ! Inst |
	break_block ;

	Block ? I, I = undo :
	Inst = {I,[],[]}, Rest ! Inst |
	break_block ;

	Block ? I, I = copy(A,B) :
	Inst = {I,Use,Define}, Rest ! Inst |
	get_use(A, Use, []),
	get_use(B, Define, []),
	break_block ;
	
	Block ? I, I = multiple_copy([Src], [Dest]), Block' =\= [_] :
	Inst = {I,Use,Define}, Rest ! Inst |
	get_use(Src, Use, []),
	get_use(Dest, Define, []),
	break_block ;

	Block ? I1, I1 = multiple_copy(_,_),
	Block' = [I2], I2 = iterate(_) :
	MultCopy = I1, Rest = [], Iterate = I2 ;

	Block ? I1, I1 = multiple_copy(_,_),
	Block' = [I2], arg(1,I2,execute) :
	MultCopy = I1, Rest = [], Iterate = I2 ;
	
	Block = [I], arg(1,I,execute) : 
	MultCopy = none, Rest = [], Iterate = I ;

	Block = [I], arg(1,I,iterate) : 
	MultCopy = none, Rest = [], Iterate = I.

break_multiple(Op, Srcs, Dests, Rest, RestT)
:-
	Srcs ? S, Dests ? D : 
	Rest ! {{Op,[S],[D]},Use,[]} |
	get_uses([S,D], Use),
	break_multiple ;

	Srcs = [], Dests = [] : Rest = RestT, Op = _.

get_uses(Args, Uses)
:-	
	Args ? Arg | get_use(Arg, Uses, Uses'), get_uses ;
	Args = [] : Uses = [] ;
	otherwise | get_use(Args, Uses, []).

get_use(Arg, Uses, UseT)
:-
       Arg = a(_) : Uses = [Arg|UseT] ;
       Arg = {R,_}, R = a(_) : Uses = [R|UseT] ;
       Arg = {'&',{R,_}}, R = a(_) : Uses = [R|UseT] ;
       Arg = car(R), R = a(_) : Uses = [R|UseT] ;
       Arg = ro({'&',{R,_}}), R = a(_) : Uses = [R|UseT] ;
       Arg = ro({R,_}), R = a(_) : Uses = [R|UseT] ;
       Arg = ro(R), R = a(_) : Uses = [R|UseT] ;
       Arg = {'PR',_} : Uses = UseT ;
       Arg = {'_'} : Uses = UseT ;
       constant(Arg) : Uses = UseT.

allocate_list(Rest, MultCopy, RestOut, MultOut)
:-
	Rest ? Inst, Inst = {allocate_list([Arg|{_}],Reg),_,_}, Reg = a(_) |
	get_assign(Reg, Dest, Rest', RestBefore, RestAfter, MultAsgn),
	use_reg(Dest, Reg, RestBefore, {ok,St1}),
	use_reg(Dest, Reg, RestAfter, {St1,St2}),
	use_reg(Dest, Dest, RestAfter, {St2,St}),
	get_copy(St, Reg, Dest, MultCopy, Copy, MultCopy'),
%	screen#display({Reg,Dest,MultCopy,MultCopy'},type(ground)),
	opt_assign_list(Copy, Arg, Dest, Inst, MultAsgn, 
			RestBefore, RestAfter, Rest'', RestOut, RestOut'),
	allocate_list ;

	Rest ? I, I =\= allocate_list([_|{_}],_) : RestOut ! I | allocate_list;

	Rest = [] : RestOut = [], MultOut = MultCopy.

opt_assign_list(Copy, Arg, Dest, Inst, MultAsgn, 
			RestBefore, RestAfter, Rest, RestOut, RestOutT)
:-
	Copy = none, MultAsgn = [] :
	RestOut = [Inst|RestOutT],  Rest = RestBefore, 
	Arg = _, Dest = _, RestAfter = _;

	Copy = none, MultAsgn =\= [] :
	RestOut = [Inst|RestOutT], Arg = _, Dest = _ |
	append(RestBefore, [MultAsgn|RestAfter], Rest) ;

	Copy = incr, Inst = {_,Use,_} :
	RestOut = [{list_assign(Arg,Dest),[Dest|Use],[]}|RestOutT], 
	MultAsgn = _ |
	append(RestBefore, RestAfter, Rest).

get_copy(St, Reg, Dest, MultCopy, Copy, MultCopyO)
:-
	Dest = none :
	MultCopyO = MultCopy, Copy = none, St = _, Reg = _ ;
	
	St = in_use :
	MultCopyO = MultCopy, Copy = none, Dest = _, St = _, Reg = _ ;

	St = ok |
	find_copy(Reg, Dest, MultCopy, MultCopyO, Copy).

find_copy(Reg, Dest, MultCopy, MultCopyO, Copy)
:-
	MultCopy = multiple_copy(Srcs, Dests) : 
	MultCopyO = multiple_copy(SrcsO, DestsO) |
	find_copy1(Reg, Dest, Srcs, Dests, SrcsO, DestsO, Copy).

find_copy1(Reg, Dest, Srcs, Dests, SrcsO, DestsO, Copy)
:-
	Srcs ? Reg : Copy = none, SrcsO = Srcs, DestsO = Dests, 
	Dest = _, Srcs' = _ ;

	Srcs ? Dest : Copy = none, SrcsO = Srcs, DestsO = Dests, 
	Reg = _, Srcs' = _ ;

	Srcs ? {'&',{Reg,1}}, Dests ? Dest : 
	Copy = incr, SrcsO = Srcs', DestsO = Dests' ;

	Srcs ? {'&',{Reg,_}}, Dests ? D, D =\= Dest : 
	Copy = none, SrcsO = Srcs, DestsO = Dests, Srcs' = _, Dests' = _ ;

	Srcs ? {'&',{Dest,_}} : Copy = none, SrcsO = Srcs, DestsO = Dests, 
	Reg = _, Srcs' = _ ;

	Srcs = [], Dests = [] : Copy = none, SrcsO = [], DestsO = [], 
	Reg = _, Dest = _ ;
	
	otherwise, Srcs ? S, Dests ? D : SrcsO ! S, DestsO ! D | find_copy1.

get_assign(Reg, Dest, Rest, RestBefore, RestAfter, MultAsgn)
:-
	Rest ? I, I = {{Op,[Reg],[D]},_,_}, Op = multiple_assign :
	Dest = D, RestBefore = [], RestAfter = Rest', MultAsgn = I ;

	Rest ? I, I = {{Op,[Reg],[D]},_,_}, 
	Op = multiple_assign_and_commit :
	Dest = D, RestBefore = [], RestAfter = Rest', MultAsgn = I ;

	Rest ? I, I = {{Op,[Reg],[D]},_,_}, 
	Op = multiple_assign_and_commit_trail :
	Dest = D, RestBefore = [], RestAfter = Rest', MultAsgn = I ;

	Rest ? I, I = {Inst,_,_}, 
	Inst =\= multiple_assign([Reg],_),
	Inst =\= multiple_assign_and_commit([Reg],_),
	Inst =\= multiple_assign_and_commit_trail([Reg],_) : 
	RestBefore ! I | get_assign ;
	
	Rest = [] : 
	Dest = none, RestBefore = [], RestAfter = [], MultAsgn = [], Reg = _.

use_reg(Ind, Reg, Rest, Sts)
:-
	Ind =\= none, Rest ? I, I = {_,Uses,_}, Sts = {StL,StR} : 
	Sts' = {StM,StR} |
	member(Reg, Uses, {StL,StM}),
	use_reg ;
	
	Ind = none, Sts = {_,R} : R = in_use, Reg = _, Rest = _;

	Ind =\= none, Rest = [], Sts = {L,R} : L = R, Reg = _, Ind = _.

member(Reg, Uses, Sts)
:-
	Uses ? Reg, Sts = {_,R} : R = in_use, Uses' = _ ;
	Uses ? R1, Reg =\= R1 | member ;
	Uses = [], Sts = {L,R} : L = R, Reg = _.

optimized_block(Rest, Ar, MultCopy, Optimized, OptTail, SC)
:-
	MultCopy = multiple_copy(Srcs,Dests), SC = {L,R} |
	optimized_block1(Srcs, Dests, [], Rest, Ar, SrcsO, DestsO, RestO),
	vars(Vars, Alloc, {L,M}),
%	screen#display({RestO,SrcsO,DestsO,MultCopy},type(ground)),
	replace_list_ass(RestO, RestO1),
	copy_rest(RestO1,SrcsO, DestsO, Optimized, OptTail, Vars, Alloc,{M,R}).

replace_list_ass(Rest, RestO)
:-
	count_allocate(Rest, Count),
	replace_list(Count, Rest, RestO).

replace_list(Count, Rest, RestO)
:-
	Count = 0 : RestO = Rest ;
	Count >= 2 : RestO = Rest ;
	Count = 1 | replace_list1(Rest, RestO).

replace_list1(Rest, RestO)
:-
	Rest ? Inst, Inst = {I,Use,Def}, I = list_assign(A,B) :
	RestO = [{list_assign_with_check(A,B),Use,Def}|Rest'] ;

	otherwise, Rest ? I : RestO ! I | replace_list1.

count_allocate(Rest, Count)+(C=0)
:-
	Rest ? Inst, Inst = {I,_,_}, I = list_assign(_,_) |
	C' := C + 1,
	count_allocate ;

	Rest ? Inst, Inst = {I,_,_}, arg(1,I,Op), Op = allocate_list : C = _ |
	C' := 2,
	count_allocate ;

	Rest ? Inst, Inst = {I,_,_}, arg(1,I,Op), Op = allocate_tuple : C = _ |
	C' := 2,
	count_allocate ;

	Rest ? Inst, Inst = {I,_,_}, arg(1,I,Op), Op = allocate_var : C = _ |
	C' := 2,
	count_allocate ;

	Rest ? Inst, Inst = {I,_,_}, arg(1,I,Op), Op = allocate_pr : C = _ |
	C' := 2,
	count_allocate ;

	Rest = [] : Count = C ;

	otherwise, Rest ? _ | count_allocate.

optimized_block1(Srcs, Dests, Used, Rest, Ar, SrcsO, DestsO, RestO)
:-
	Srcs ? S, S = a(I), I > Ar, Dests ? D |
	get_src_def(S, D, Rest, RestBefore, RestAfter, Inst, Use, StI),
%	screen#display({S,Used,Rest,RestBefore, RestAfter,Inst},type(ground)),
	member(D, Use, {StI,St00}),
	member(S, Use, {St00,St0}),
	use_reg(Inst, D, RestAfter, {St0,St1}),
	member(D, Used, {St1,St2}),
	member(S, Used, {St2,St}),
%	screen#display({St,Inst,Used,Rest,Rest'},type(ground)),
	eliminate_copy(St, S, D, Used, Rest, RestBefore, RestAfter, Inst, 
				Used', Rest', SrcsO, DestsO, SrcsO', DestsO'),
	optimized_block1 ;

	Srcs ? S, S = a(I), I =< Ar, Dests ? D : 
	SrcsO ! S, DestsO ! D, Used' = [S|Used] |
	optimized_block1 ;

	Srcs ? S, S =\= a(_), Dests ? D | 
        eliminate_copy(in_use, S, D, Used, Rest, _, _, _, 
				Used', Rest', SrcsO, DestsO, SrcsO', DestsO'),
	optimized_block1 ;

	Srcs = [], Dests = [] :
	RestO = Rest, SrcsO = [], DestsO = [], Used = _.

get_src_def(S, D,  Rest, RestBefore, RestAfter, Inst, Use, StI)
:-
	Rest ? I, I = {_,U,[S]} : 
	RestBefore = [], RestAfter = Rest', Inst = I, Use = U, StI = ok, D = _ ;

	Rest ? I, I =\= {_,_,[S]} : RestBefore ! I | get_src_def ;

	Rest = [] : 
        RestBefore = [], RestAfter = [], Inst = none, Use = [], StI = in_use,
	S = _, D = _.

eliminate_copy(St, S, D, Used, Rest, RestBefore, RestAfter, Inst, 
			  UsedO, RestO, Srcs, Dests, SrcsT, DestsT)
:-
	St = in_use, S = a(_) :
	UsedO = [S|Used], RestO = Rest, Srcs = [S|SrcsT], Dests = [D|DestsT],
	RestBefore = _, RestAfter = _, Inst = _ ;

	St = in_use, S = {'&',{Reg,_}} :
	UsedO = [Reg|Used], RestO = Rest, Srcs = [S|SrcsT], Dests = [D|DestsT],
	RestBefore = _, RestAfter = _, Inst = _ ;

	St = in_use, S = {Reg,_}, Reg = a(_) :
	UsedO = [Reg|Used], RestO = Rest, Srcs = [S|SrcsT], Dests = [D|DestsT],
	RestBefore = _, RestAfter = _, Inst = _ ;

	St = in_use, S = ro({'&',{Reg,_}}) :
	UsedO = [Reg|Used], RestO = Rest, Srcs = [S|SrcsT], Dests = [D|DestsT],
	RestBefore = _, RestAfter = _, Inst = _ ;

	St = in_use, S = car(Reg) :
	UsedO = [Reg|Used], RestO = Rest, Srcs = [S|SrcsT], Dests = [D|DestsT],
	RestBefore = _, RestAfter = _, Inst = _ ;

	St = in_use, S = ro(Reg), Reg = a(_) :
	UsedO = [Reg|Used], RestO = Rest, Srcs = [S|SrcsT], Dests = [D|DestsT],
	RestBefore = _, RestAfter = _, Inst = _ ;

	St = in_use, constant(S) :
	UsedO = Used, RestO = Rest, Srcs = [S|SrcsT], Dests = [D|DestsT],
	RestBefore = _, RestAfter = _, Inst = _ ;

	St = ok : UsedO = Used, Srcs = SrcsT, Dests = DestsT, Rest = _ |
	get_inst(Inst, D, RestAfter', Inst'),
	substitue(S, D, RestAfter, RestAfter'),	
	append(RestBefore, Inst', RestO).

substitue(S, D, Rest, RestO)
:-
	Rest ? I : RestO ! I' | subs(S, D, I, I'), substitue ;
	Rest = []  : RestO = [], S = _, D = _ .

subs(S, D, Inst, InstO)
:-
	Inst = {_, [], _} : InstO = Inst, D = _, S = _ ;

	Inst = {{Op,Src},[Src],[Src]} :
	InstO = {{Op,Src'},[Src'],[Src']} |
	sub(S, D, Src, Src') ;

	Inst = {{Op,Srcs,Dests},Uses,Defs}, Uses =\= [] :
	InstO = {{Op,Srcs',Dests'},Uses',Defs} |
%	screen#display({Op,Srcs,Dests,Uses,Srcs',Dest',Uses'},type(ground)),
	sub(S, D, Srcs, Srcs'),
	sub(S, D, Dests, Dests'),
	sub(S, D, Uses, Uses');

	Inst = {{Op,A,B,C},Uses,Defs}, Uses =\= [] :
	InstO = {{Op,A',B',C'},Uses',Defs} |
%	screen#display({Op,A,B,C}),
	sub(S, D, [A,B,C], [A',B',C']),
	sub(S, D, Uses, Uses').

sub(S, D, In, Out)
:-
	In ? I : Out ! O | 	replace(S, D, I, O), sub ;
	In = [] : Out = [], S = _, D= _ ;
	otherwise | replace(S, D, In, Out).

replace(S, D, Arg, Out)
:-
	Arg = S : Out = D ;
       	Arg = {S,C} : Out = {D,C} ;
       	Arg = {'&',{S,C}} : Out = {'&',{D,C}} ;
       	Arg = car(S) : Out = car(D) ;
       	Arg = ro({'&',{S,C}}) : Out = ro({'&',{D,C}}) ;
       	Arg = ro({S,C}) : Out = ro({D,C}) ;
       	Arg = ro(S) : Out = ro(D) ;
       	Arg = a(_), Arg =\= S : Out = Arg, D = _ ;
       	Arg = {S1,_}, S1 = a(_), S =\= S1 : Out = Arg, D = _ ;
       	Arg = {'&',{S1,_}}, S =\= S1 : Out = Arg, D = _ ;
       	Arg = car(S1), S =\= S1 : Out = Arg, D = _ ;
	Arg = ro({'&',{S1,_}}), S1 = a(_), S =\= S1 : Out = Arg, D = _ ;
	Arg = ro({S1,_}), S1 = a(_), S =\= S1 : Out = Arg, D = _ ;
	Arg = ro(S1), S1 = a(_), S =\= S1 : Out = Arg, D = _ ;
       	Arg = {'_'} : Out = Arg, S = _, D = _ ;
       	Arg = {'PR',_} : Out = Arg, S = _, D = _ ;
       	constant(Arg) : Out = Arg, S = _, D = _.

get_inst(Inst, D, RestAfter, InstO)
:-	
	Inst = none : InstO = RestAfter, D = _ ;

	Inst = {allocate_var(_),Use,_} : 
	InstO = [{allocate_var(D), [D|Use], [D]}|RestAfter] ;

	Inst = {allocate_list(L, _),Use,_} : 
	InstO = [{allocate_list(L,D),[D|Use],[D]}|RestAfter] ;

	Inst = {allocate_tuple(T, _),Use,_} : 
	InstO = [{allocate_tuple(T,D),[D|Use],[D]}|RestAfter] ;

	Inst = {decrement(Src, _),Use,_}, Src =\= D :
	InstO = [{decrement(Src,D),[D|Use],[D]}|RestAfter] ;

	Inst = {decrement(D, _),Use,_} : 
	InstO = [{decrement(D),[D|Use],[D]}|RestAfter] ;

	Inst = {decrement_pointer(Src, _),Use,_}, Src =\= D :
	InstO = [{decrement_pointer(Src,D),[D|Use],[D]}|RestAfter] ;

	Inst = {decrement_pointer(D, _),Use,_} : 
	InstO = [{decrement_pointer(D),[D|Use],[D]}|RestAfter] ;

	Inst = {increment(Src, _),Use,_}, Src =\= D :
	InstO = [{increment(Src,D),[D|Use],[D]}|RestAfter] ;

	Inst = {increment(D, _),Use,_} : 
	InstO = [{increment(D),[D|Use],[D]}|RestAfter] ;

	Inst = {deref(Src, _),Use,_} :
	InstO = [{deref(Src,D),[D|Use],[D]}|RestAfter] ;

	Inst = {deref_value(Src, _),Use,_} :
	InstO = [{deref_value(Src,D),[D|Use],[D]}|RestAfter] ;

	Inst = {deref_car(Src, _),Use,_} :
	InstO = [{deref_car(Src,D),[D|Use],[D]}|RestAfter] ;

	Inst = {load_car(Src, _),Use,_} :
	InstO = [{deref_car(Src,D),[D|Use],[D]}|RestAfter] ;

	Inst = {fetch(T,_),Use,_} : 
        InstO = [{fetch(T,D),[D|Use],[D]}|RestAfter] ; 
	
	Inst = {multiple_copy(Src, _),Use,_} :  
	InstO = [{multiple_copy(Src,[D]),[D|Use],[D]}|RestAfter].

vars(Vars, Alloc, SC)
:-
	Vars = [Reg], SC = {L,R} : Alloc = allocate_var(Reg), L = R ;
	Vars ? _, Vars' =\= [], SC = {L,R} : Alloc = allocate_vars(Vars),L = R;
	Vars = [], SC = {L,R} : L = R, Alloc = _.

copy_rest(Rest, SrcsO, DestsO, Optimized, OptTail, Vars, Alloc, SC)+(Uses=[])
:-
	Rest ? {I,_,_}, Uses = [], 
	I =\= allocate_var(_), 
	I =\= multiple_assign(_,_),
	I =\= multiple_assign_and_commit(_,_),
	I =\= multiple_assign_and_commit_trail(_,_) : 
	Optimized ! I |
	copy_rest ;

	Rest ? {I,Use,_}, Uses =\= [], 
	I =\= allocate_var(_), 
	I =\= multiple_assign(_,_),
	I =\= multiple_assign_and_commit(_,_),
	I =\= multiple_assign_and_commit_trail(_,_) : 
	Optimized ! I |
	append(Uses, Use, Uses'),
	copy_rest ;

	Rest ? {I,_,_}, Uses = [], 
	I = {Op,[Src],[Dest]}, Op = multiple_assign :
	Optimized ! {Op,[Src|Srcs],[Dest|Dests]} |
	get_mult_asgn(Rest', Srcs, Dests, Rest'', _, _),
	copy_rest ;

	Rest ? {I,Use,_}, Uses =\= [], 
	I = {Op,[Src],[Dest]}, Op = multiple_assign :
	Optimized ! {Op,[Src|Srcs],[Dest|Dests]} |
	append(Uses, Use, UsesT),
	get_mult_asgn(Rest', Srcs, Dests, Rest'', Uses', UsesT),
	copy_rest ;

	Rest ? {I,_,_}, Uses = [], 
	I = {Op,[Src],[Dest]}, Op = multiple_assign_and_commit :
	Optimized ! {Op,[Src|Srcs],[Dest|Dests]} |
%	screen#display({Rest,Rest''},type(ground)),
	get_mult_asgn(Rest', Srcs, Dests, Rest'', _, _),
	copy_rest ;

	Rest ? {I,Use,_}, Uses =\= [], 
	I = {Op,[Src],[Dest]}, Op = multiple_assign_and_commit :
	Optimized ! {Op,[Src|Srcs],[Dest|Dests]} |
	append(Uses, Use, UsesT),
%	screen#display({Rest,Rest''},type(ground)),
	get_mult_asgn(Rest', Srcs, Dests, Rest'', Uses', UsesT),
	copy_rest ;

	Rest ? {I,_,_}, Uses = [], 
	I = {Op,[Src],[Dest]}, Op = multiple_assign_and_commit_trail :
	Optimized ! {Op,[Src|Srcs],[Dest|Dests]} |
	get_mult_asgn(Rest', Srcs, Dests, Rest'', _, _),
	copy_rest ;

	Rest ? {I,Use,_}, Uses =\= [], 
	I = {Op,[Src],[Dest]}, Op = multiple_assign_and_commit_trail :
	Optimized ! {Op,[Src|Srcs],[Dest|Dests]} |
	append(Uses, Use, UsesT),
	get_mult_asgn(Rest', Srcs, Dests, Rest'', Uses', UsesT),
	copy_rest ;

	Rest ? {I,_,_}, I = allocate_var(Reg), Uses = [] : 
	Optimized ! Alloc, Vars ! Reg, Uses' = [first] |
	copy_rest ;

	Rest ? {I,_,_}, I = allocate_var(Reg), Uses =\= [] |
	member_v(Reg, Uses, St),
	opt_var(St, Reg, I, Optimized, Optimized', Vars, Vars'),
	copy_rest ;

	Rest = [], SrcsO = [], DestsO = [], SC = {L,R} :
	Optimized = OptTail, Vars = [], L = R, Alloc = _, Uses = _ ;
%	screen#display('end of rest without m_c') ;

	Rest = [], SrcsO = [{'&',{Src,1}}], DestsO = [Dest], Dest =\= Src, 
	SC = {L,R} :
	Optimized = [increment_pointer(Src,Dest)|OptTail], Vars = [], R = L,
	Alloc = _, Uses = _ ;
 
	Rest = [], SrcsO = [{'&',{Dest,1}}], DestsO = [Dest], SC = {L,R} :
	Optimized = [increment_pointer(Dest)|OptTail], Vars = [], R = L,
	Alloc = _, Uses = _ ;
 
	otherwise, Rest = [], SC = {L,R} :
	Optimized = [multiple_copy(SrcsO,DestsO)|OptTail], Vars = [], L = R, 
	Alloc = _, Uses = _ .
%	screen#display('end of rest with m_c').

get_mult_asgn(Rest, Srcs, Dests, RestO, Uses, UsesT)
:-
	Rest ? {multiple_assign([Src],[Dest]),[Use],_} :
	Srcs ! Src, Dests ! Dest, Uses ! Use |
	get_mult_asgn ;
	
	Rest ? {multiple_assign_and_commit([Src],[Dest]),[Use],_} :
	Srcs ! Src, Dests ! Dest, Uses ! Use |
	get_mult_asgn ;
	
	Rest ? {multiple_assign_and_commit_trail([Src],[Dest]),[Use],_} :
	Srcs ! Src, Dests ! Dest, Uses ! Use |
	get_mult_asgn ;
	
	Rest ? {multiple_assign([Src],[Dest]),[Use1,Use2],_} :
	Srcs ! Src, Dests ! Dest, Uses ! Use1, Uses' ! Use2 |
	get_mult_asgn ;
	
	Rest ? {multiple_assign_and_commit([Src],[Dest]),[Use1,Use2],_} :
	Srcs ! Src, Dests ! Dest, Uses ! Use1, Uses' ! Use2 |
	get_mult_asgn ;
	
	Rest ? {multiple_assign_and_commit_trail([Src],[Dest]),[Use1,Use2],_} :
	Srcs ! Src, Dests ! Dest, Uses ! Use1, Uses' ! Use2 |
	get_mult_asgn ;
	
	otherwise :
	Srcs = [], Dests = [], RestO = Rest, Uses = UsesT.
	
opt_var(St, Reg, I, Opt, OptO, Vars, VarsO)
:-
	St = yes : Opt = [I|OptO], Vars = VarsO, Reg = _ ;
	St = no : Opt = OptO, Vars = [Reg|VarsO], I = _.

member_v(M, L, St)
:-
	L ? M : St = yes, L' = _ ;
	L ? M1, M =\= M1 | member_v ;
	L = [] : St = no, M = _.

append(Xs, Ys, Zs)
:-
	Xs ? X : Zs ! X | append ;
	Xs = [] : Zs = Ys .

