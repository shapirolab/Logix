/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/immediate/immediate_activation.cp,v 1.1 1999/07/09 07:03:04 bill Exp $ */
-language(compound).
-export([immediate_activation/8]).
-mode(trust).

procedure immediate_activation(Imm, Bodies, LastBody, Ctl, Dic, BodiesO, Ch, SC).

immediate_activation(Imm, Bodies, LastBody, Ctl, Dic, BodiesO, Ch, SC)+
				(BodiesA=[], Ind = {no,_})
:-
	Bodies ? spawn(Body), Ctl = CtlH\CtlT, Dic = {DicI,DicO}, 
	SC = {L,R}, Ind = {IL,IR} :
	Ctl' = CtlM\CtlT, Dic' = {DicM,DicO}, 
	SC' = {M,R}, Ind' = {IM,IR} |
%	screen#display(Body),
	activate(Imm, Body, LastBody, CtlH\CtlM, {DicI,DicM}, BodiesA, BodiesA', 
						Ch, {IL,IM}, {L,M}),
	immediate_activation ;

	Bodies = [], Ind = {imm,_} : Ind' = {no,_}, BodiesA' = [] |
	reverse(BodiesA, Bodies'), 
	immediate_activation ;

	Bodies = [], Ind = {no,_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicM}, SC = {L,R} :
	CtlH = CtlT, DicI = DicM, L = R, LastBody = _, Ch = _, Imm = _ |
	reverse(BodiesA, BodiesO).

activate(Imm, Body, LastBody, Ctl, Dic, Bodies, BodiesO, Ch, Ind, SC)
:-
	Body = {Pred,A1,A2,A3}, Pred = plus, Dic = {DicI,_} :
	Imm = _, LastBody = _ |
%	screen#display({1,Ent1,Ent2,Ent3,St1,St},type(ground)),
%	screen#display({11,Ent1,Ent2,Ent3,St1,St}),
	get_entry(A1, Ent1, DicI, Ch),
	get_entry(A2, Ent2, DicI, Ch),
	get_entry(A3, Ent3, DicI, Ch),
	check_entries(Ent1, Ent2, Ent3, St1),
	arithmetic(St1, Pred, [{A1,Ent1},{A2,Ent2},{A3,Ent3}], Ctl, Dic, Ch, St),
	eliminate_imm(St, Body, Bodies, BodiesO, Ind, SC) ;

	Body = {Pred,A1,A2,A3}, Pred = diff, Dic = {DicI,_} :
	Imm = _, LastBody = _ |
	get_entry(A1, Ent1, DicI, Ch),
	get_entry(A2, Ent2, DicI, Ch),
	get_entry(A3, Ent3, DicI, Ch),
	check_entries(Ent1, Ent2, Ent3, St1),
	arithmetic(St1, Pred, [{A1,Ent1},{A2,Ent2},{A3,Ent3}], Ctl, Dic, Ch, St),
	eliminate_imm(St, Body, Bodies, BodiesO, Ind, SC) ;

	Body = {Pred,A1,A2}, Pred = '=', Dic = {DicI,_}, SC = {L,R} :
	Imm = _ |
	get_entry(A1, Ent1, DicI, Ch),
	get_entry(A2, Ent2, DicI, Ch),
	filter_lastbody([{A1,Ent1},{A2,Ent2}], LastBody, Ok),
	unify({A1,Ent1}, {A2,Ent2}, Ctl, Dic, Ch, {L,M}, St, Ok),
	eliminate_imm(St, Body, Bodies, BodiesO, Ind, {M,R}) ;

	otherwise, Imm = 0, 
	Ctl = CtlH\CtlT, Ind = {I,O}, SC = {L,R}, Dic = {DI,DO} :
	CtlH = CtlT, I = O, L = R, DI = DO, LastBody = _,
	BodiesO = [spawn(Body)|Bodies], Ch = _.

filter_lastbody(Entries, LastBody, Ok) + (OkI = safe)
:-
	Entries ? {V,{_,_,var,_}} |
        member_l(V, LastBody, {OkI,OkI'}),
	filter_lastbody ;

	Entries ? {V,{_,_,ref(var),_}} |
        member_l(V, LastBody, {OkI,OkI'}),
	filter_lastbody ;

	Entries ? E, E =\= {_,{_,_,var,_}}, E =\= {_,{_,_,ref(var),_}} |
	filter_lastbody ;

	Entries = [] : Ok = OkI, LastBody = _.

member_l(V, Vars, OkL)
:-
	Vars = [V|_], OkL = {_,O} : O = unsafe ;
	Vars ? V1, V =\= V1 | member_l ;
	Vars = [], OkL = {I,O} : O = I, V = _.

get_entry(Psi, Entry, Dic, Ch)
:-
	Psi = integer(I) : Entry = {I,'*',integer,I}, Dic = _, Ch = _;
	Psi = real(I) : Entry = {I,'*',real,I}, Dic = _, Ch = _;
	Psi = psi(P) : write_channel(look(P,Entry,Dic),Ch) ;
	Psi = ro(_) : write_channel(look(Psi,Entry,Dic),Ch) ;
	otherwise, Psi = {Type,_} : Entry = {Psi,'*',Type,'*'}, Dic = _, Ch=_.

check_entries(Ent1, Ent2, Ent3, St)
:-
	Ent1 = {_,_,T1,_}, Ent2 = {_,_,T2,_}, Ent3 = {_,_,T3,_} :
	St = {StInt,StOut} |
	check_int(T1, T2, StInt),
	check_out(T3, StOut).

check_int(Type1, Type2, St)
:-
	Type1 = integer, Type2 = integer : St = integer ;
	Type1 = integer, Type2 = car(integer) : St = integer ;
	Type1 = integer, Type2 = real : St = number ;
	Type1 = integer, Type2 = car(real) : St = number ;
	Type1 = integer, Type2 = number : St = number ;
	Type1 = integer, Type2 = car(number) : St = number ;

	Type1 = car(integer), Type2 = integer : St = integer ;
	Type1 = car(integer), Type2 = car(integer) : St = integer ;
	Type1 = car(integer), Type2 = real : St = number ;
	Type1 = car(integer), Type2 = car(real) : St = number ;
	Type1 = car(integer), Type2 = number : St = number ;
	Type1 = car(integer), Type2 = car(number) : St = number ;

	Type1 = real, Type2 = integer : St = number ;
	Type1 = real, Type2 = car(integer) : St = number ;
	Type1 = real, Type2 = real : St = number ;
	Type1 = real, Type2 = car(real) : St = number ;
	Type1 = real, Type2 = number : St = number ;
	Type1 = real, Type2 = car(number) : St = number ;

	Type1 = car(real), Type2 = integer : St = number ;
	Type1 = car(real), Type2 = car(integer) : St = number ;
	Type1 = car(real), Type2 = real : St = number ;
	Type1 = car(real), Type2 = car(real) : St = number ;
	Type1 = car(real), Type2 = number : St = number ;
	Type1 = car(real), Type2 = car(number) : St = number ;

	Type1 = number, Type2 = integer : St = number ;
	Type1 = number, Type2 = car(integer) : St = number ;
	Type1 = number, Type2 = real : St = number ;
	Type1 = number, Type2 = car(real) : St = number ;
	Type1 = number, Type2 = number : St = number ;
	Type1 = number, Type2 = car(number) : St = number ;

	Type1 = car(number), Type2 = integer : St = number ;
	Type1 = car(number), Type2 = car(integer) : St = number ;
	Type1 = car(number), Type2 = real : St = number ;
	Type1 = car(number), Type2 = car(real) : St = number ;
	Type1 = car(number), Type2 = number : St = number ;
	Type1 = car(number), Type2 = car(number) : St = number ;

	otherwise : St = spawn, Type1 = _, Type2 = _.

check_out(Type, St)
:-
	Type = var : St = Type ;
	Type = new : St = Type ;
	Type = car(T), T = var : St = T	;
	Type =\= var, Type =\= new, Type =\= car(var) : St = spawn.
	
arithmetic(StIn, Pred, Args, Ctl, Dic, Ch, St)
:-
	StIn = {integer,var},	
	Args = [{A1,Ent1},{A2,Ent2},{A3,Ent3}],
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} |
	get_argument(A1, Ent1, CtlH\CtlM1, {DicI,DicM1}, Reg1, Ch),
	get_argument(A2, Ent2, CtlM1\CtlM2, {DicM1,DicM2}, Reg2, Ch),
	get_output(A3, Ent3, integer, CtlM2\CtlM3, {DicM2,DicO}, Reg3, Ch),
	integer_assign(Pred, Reg1, Reg2, Reg3, CtlM3\CtlT, St) ;

	StIn = {integer,new},
	Args = [{A1,Ent1},{A2,Ent2},{A3,_}], A3 = psi(A3Psi),
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} :
	write_channel(get_reg(R3,{DicI,DicM}),Ch),
	write_channel(add(A3Psi,{R3,R3,integer,'*'},{DicM,DicM1}),Ch) |
	get_argument(A1, Ent1, CtlH\CtlM1, {DicM1,DicM2}, Reg1, Ch),
	get_argument(A2, Ent2, CtlM1\CtlM2, {DicM2,DicO}, Reg2, Ch),
	integer_instruction(Pred, Reg1, Reg2, R3, CtlM2\CtlT, St) ;
	
	StIn = {number,var},
	Args = [{A1,Ent1},{A2,Ent2},{A3,Ent3}],
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} :
	St = imm |
	get_argument(A1, Ent1, CtlH\CtlM1, {DicI,DicM1}, Reg1, Ch),
	get_argument(A2, Ent2, CtlM1\CtlM2, {DicM1,DicM2}, Reg2, Ch),
	get_output(A3, Ent3, number, CtlM2\[{PredN,Reg1,Reg2,Reg3}|CtlT], 
						{DicM2,DicO}, Reg3, Ch),
	utils#append_strings([Pred,'_number'],PredN) ;

	StIn = {number,new},
	Args = [{A1,Ent1},{A2,Ent2},{A3,_}], A3 = psi(A3Psi),
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} :
	write_channel(get_reg(R3,{DicM2,DicM3}),Ch),
	write_channel(add(A3Psi,{R3,R3,number,'*'},{DicM3,DicO}),Ch),
	CtlH ! allocate_var(R3),
	St = imm |
	get_argument(A1, Ent1, CtlH'\CtlM1, {DicI,DicM1}, Reg1, Ch),
	get_argument(A2, Ent2, CtlM1\[{PredN,Reg1,Reg2,R3}|CtlT], 
						{DicM1,DicM2}, Reg2, Ch),
	utils#append_strings([Pred,'_number'],PredN) ;

	StIn = {spawn,_},
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT :
	CtlH = CtlT, DicO = DicI, St = no, Args = _, Ch = _, Pred = _ ;
	
	StIn = {_,spawn},
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT :
	CtlH = CtlT, DicO = DicI, St = no, Args = _, Ch = _, Pred = _.
	
get_argument(A, Ent, Ctl, Dic, Reg, Ch)
:-
	Ent = {Reg1,_,car(Type),I}, A = psi(APsi), 
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	write_channel(get_reg(Reg2,{DicM,DicM1}),Ch),
	write_channel(update(APsi,{Reg2,Reg,Type,I},{DicM1,DicO}),Ch),
	CtlH = [deref_car(Reg1,Reg2,Reg)|CtlT] ;

	Ent = {Reg1,'*',Type,I}, Type = integer, A = psi(APsi), Reg1 = a(_),
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	write_channel(update(APsi,{Reg1,Reg,Type,I},{DicM,DicO}),Ch),
	CtlH = [deref_value(Reg1,Reg)|CtlT] ;

	Ent = {Reg1,'*',Type,I}, Type = integer, A = psi(APsi), Reg1 = {a(_),_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	write_channel(get_reg(Reg2,{DicM,DicM1}),Ch),
	write_channel(update(APsi,{Reg,Reg2,Type,I},{DicM1,DicO}),Ch),
	CtlH = [deref(Reg1,Reg,Reg2)|CtlT] ;

	Ent = {_,Reg1,integer,_}, Reg1 = a(_),
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} :
	Reg = Reg1, CtlH = CtlT, DicI = DicO, A = _, Ch = _ ;

	Ent = {Reg1,_,_,_}, integer(Reg1),
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} :
	Reg = Reg1, CtlH = CtlT, DicI = DicO, A = _, Ch = _ ;

	Ent = {Reg1,_,_,_}, real(Reg1),
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} :
	Reg = Reg1, CtlH = CtlT, DicI = DicO, A = _, Ch = _ ;

	otherwise, Ent = {Reg1,_,_,_}, Reg1 = a(_), 
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} :
	Reg = Reg1, CtlH = CtlT, DicI = DicO, A = _, Ch = _;

	otherwise, Ent = {Reg1,_,_,_}, Reg1 = {a(_),_}, 
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} :
	Reg = {'&',Reg1}, CtlH = CtlT, DicI = DicO, A = _, Ch = _.

get_output(A, Ent, Type, Ctl, Dic, Reg, Ch)
:-
	Ent = {Reg1,_,var,_}, A = psi(APsi), Reg1 = a(_),
	Ctl = CtlH\CtlT :
	write_channel(update(APsi,{Reg1,'*',Type,'*'},Dic),Ch),
	Reg = Reg1, CtlH = CtlT ;

	Ent = {Reg1,_,var,_}, A = psi(APsi), Reg1 = {a(_),_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	write_channel(update(APsi,{Reg,'*',Type,'*'},{DicM,DicO}),Ch),
	CtlH = [deref(Reg1,Reg)|CtlT] ;

	Ent = {Reg1,_,ref(var),_}, A = psi(APsi), Reg1 = a(_),
	Ctl = CtlH\CtlT :
        CtlH = [deref(Reg1,Reg1)|CtlT],
	write_channel(update(APsi,{Reg1,'*',Type,'*'},Dic),Ch),
	Reg = Reg1 ;

	Ent = {Reg1,_,ref(var),_}, A = psi(APsi), Reg1 = {a(_),_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	write_channel(update(APsi,{Reg,'*',Type,'*'},{DicM,DicO}),Ch),
	CtlH = [deref(Reg1,Reg)|CtlT] ;

	Ent = {Reg1,_,car(var),_}, A = psi(APsi),
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	write_channel(update(APsi,{Reg,'*',Type,'*'},{DicM,DicO}),Ch),
	CtlH = [deref_car(Reg1,Reg)|CtlT].
	

integer_instruction(Pred, Reg1, Reg2, Reg3, Ctl, St)
:-
	Reg1 = a(_), Reg2 = 1, Ctl = CtlH\CtlT :
	CtlH = [{Opcode,Reg1,Reg3}|CtlT],
	St = imm |
	get_opcode(Pred, Opcode) ;

	Reg1 = 1, Reg2 = a(_), Ctl = CtlH\CtlT :
	CtlH = [{Opcode,Reg2,Reg3}|CtlT],
	St = imm |
	get_opcode(Pred, Opcode) ;

	integer(Reg1), Reg2 = 1, Ctl = CtlH\CtlT :
	CtlH = [multiple_copy([Num],[Reg3])|CtlT],
	St = imm |
	get_num(Pred, Reg1, Num) ;

	Reg1 = 1, integer(Reg2), Ctl = CtlH\CtlT :
	CtlH = [multiple_copy([Num],[Reg3])|CtlT],
	St = imm |
	get_num(Pred, Reg2, Num) ;

	Reg1 =\= 1, Reg2 =\= 1,	Ctl = CtlH\CtlT :
	CtlH = [{Pred,Reg1,Reg2,Reg3}|CtlT],
	St = imm.

integer_assign(Pred, Reg1, Reg2, Reg3, Ctl, St)
:-
	Reg1 = a(_), Reg2 = 1, Ctl = CtlH\CtlT :
	CtlH = [{OpcodeN,Reg1,Reg3}|CtlT],
	St = imm |
	get_opcode(Pred, Opcode),
	utils#append_strings([Opcode,'_and_commit'],OpcodeN) ;

	Reg1 = 1, Reg2 = a(_), Ctl = CtlH\CtlT :
	CtlH = [{OpcodeN,Reg2,Reg3}|CtlT],
	St = imm |
	get_opcode(Pred, Opcode),
	utils#append_strings([Opcode,'_and_commit'],OpcodeN) ;

	Reg1 =\= 1, Reg2 =\= 1,	Ctl = CtlH\CtlT :
	CtlH = [{PredN,Reg1,Reg2,Reg3}|CtlT],
	St = imm |
	utils#append_strings([Pred,'_and_commit'],PredN).

unify(Arg1, Arg2, Ctl, Dic, Ch, SC, St, Safe)
:-
	Safe = safe |         safe_unify(Arg1, Arg2, Ctl, Dic, Ch, SC, St) ;

	Safe = unsafe,	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	CtlH = CtlT, DicO = DicI, L = R, St = no, 
	Arg1 = _, Arg2 = _, Ch = _.

safe_unify(Arg1, Arg2, Ctl, Dic, Ch, SC, St) 
:-
	Arg2 = {A2,Ent2}, Ent2 = {R2,_,var,_}, R2 = a(_), A2 = psi(A2Psi),
	Arg1 = {A1,_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	CtlM = [multiple_assign([R1],[R2])|CtlT],
	St = imm |
        ground_check#allocate_multiple_vars(A1, CtlH\CtlM1,
	                                             {DicI,Dic1}, Ch, {L,M1}),
	tell_term_creation#term_creation(A1, {Dic1,DicM}, R1, CtlM1\CtlM, 
	                                                  _\_, Ent, Ch, {M1,M}),
	update_psi(A2Psi, Ent, R2, {DicM,DicO}, Ch, {M,R}) ;

	Arg2 = {A2,Ent2}, Ent2 = {R2,_,var,_}, R2 = {a(_),_},
	A2 = psi(A2Psi), Arg1 = {A1,_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	CtlM = [deref(R2,Reg),multiple_assign([R1],[Reg])|CtlT],
	write_channel(get_reg(Reg,{DicM,DicM1}),Ch),
	St = imm |
        ground_check#allocate_multiple_vars(A1, CtlH\CtlM1,
	                                             {DicI,Dic1}, Ch, {L,M1}),
	tell_term_creation#term_creation(A1, {Dic1,DicM}, R1, CtlM1\CtlM, 
	                                                  _\_, Ent, Ch, {M1,M}),
	update_psi(A2Psi, Ent, R2, {DicM1,DicO}, Ch, {M,R}) ;

	Arg2 = {A2,Ent2}, Ent2 = {R2,_,ref(var),_}, R2 = a(_), A2 = psi(A2Psi),
	Arg1 = {A1,_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	CtlM = [deref(R2,R2),multiple_assign([R1],[R2])|CtlT],
	St = imm |
        ground_check#allocate_multiple_vars(A1, CtlH\CtlM1,
	                                             {DicI,Dic1}, Ch, {L,M1}),
	tell_term_creation#term_creation(A1, {Dic1,DicM}, R1, CtlM1\CtlM, 
	                                                  _\_, Ent, Ch, {M1,M}),
	update_psi(A2Psi, Ent, R2, {DicM,DicO}, Ch, {M,R}) ;

	Arg2 = {A2,Ent2}, Ent2 = {R2,_,ref(var),_}, R2 = {a(_),_},
	A2 = psi(A2Psi), Arg1 = {A1,_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	CtlM = [deref(R2,Reg),multiple_assign([R1],[Reg])|CtlT],
	write_channel(get_reg(Reg,{DicM,DicM1}),Ch),
	St = imm |
        ground_check#allocate_multiple_vars(A1, CtlH\CtlM1,
	                                             {DicI,Dic1}, Ch, {L,M1}),
	tell_term_creation#term_creation(A1, {Dic1,DicM}, R1, CtlM1\CtlM, 
	                                                  _\_, Ent, Ch, {M1,M}),
	update_psi(A2Psi, Ent, R2, {DicM1,DicO}, Ch, {M,R}) ;

	Arg2 = {A2,Ent2}, Ent2 = {R2,_,car(var),_}, A2 = psi(A2Psi),
	Arg1 = {A1,_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	write_channel(get_reg(Reg,{DicM,DicM1}),Ch),
	CtlM = [deref_car(R2,Reg),multiple_assign([R1],[Reg])|CtlT],
	St = imm |
        ground_check#allocate_multiple_vars(A1, CtlH\CtlM1,
	                                             {DicI,Dic1}, Ch, {L,M1}),
	tell_term_creation#term_creation(A1, {Dic1,DicM}, R1, CtlM1\CtlM, 
	                                                  _\_, Ent, Ch, {M1,M}),
	update_psi(A2Psi, Ent, R2, {DicM1,DicO}, Ch, {M,R}) ;

	Arg2 = {A2,Ent2}, Ent2 = {_,_,new,_}, A2 = psi(A2Psi),	
	Arg1 = {A1,Ent1}, Ent1 =\= {_,_,new,_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	St = imm |
        ground_check#allocate_multiple_vars(A1, CtlH\CtlM,
	                                             {DicI,Dic1}, Ch, {L,M}),
	new_term_creation#new_term(A2Psi, A1, CtlM\CtlT, {Dic1,DicO}, 
	                                                          Ch, {M,R}) ;

	Arg2 = {A2,Ent2}, Ent2 = {_,_,new,_}, A2 = psi(A2Psi),	
	Arg1 = {A1,Ent1}, Ent1 = {_,_,new,_}, A1 = psi(A1Psi),
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	write_channel(get_reg(Reg,{DicI,DicM1}),Ch),
        CtlH = [allocate_var(Reg)|CtlT],
	write_channel(add(A2Psi,{Reg,'*',ref,'*'},{DicM1,DicM2}),Ch),
	write_channel(add(A1Psi,{Reg,'*',ref,'*'},{DicM2,DicO}),Ch),
	St = imm, L = R ; 

	Arg1 = {A2,Ent2}, Ent2 = {R2,_,var,_}, R2 = a(_), A2 = psi(A2Psi),
	Arg2 = {A1,_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	CtlM = [multiple_assign([R1],[R2])|CtlT],
	St = imm |
        ground_check#allocate_multiple_vars(A1, CtlH\CtlM1,
	                                             {DicI,Dic1}, Ch, {L,M1}),
	tell_term_creation#term_creation(A1, {Dic1,DicM}, R1, CtlM1\CtlM, 
	                                                  _\_, Ent, Ch, {M1,M}),
	update_psi(A2Psi, Ent, R2, {DicM,DicO}, Ch, {M,R}) ;

	Arg1 = {A2,Ent2}, Ent2 = {R2,_,var,_}, R2 = {a(_),_},
	A2 = psi(A2Psi), Arg2 = {A1,_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	CtlM = [deref(R2,Reg),multiple_assign([R1],[Reg])|CtlT],
	write_channel(get_reg(Reg,{DicM,DicM1}),Ch),
	St = imm |
        ground_check#allocate_multiple_vars(A1, CtlH\CtlM1,
	                                             {DicI,Dic1}, Ch, {L,M1}),
	tell_term_creation#term_creation(A1, {Dic1,DicM}, R1, CtlM1\CtlM, 
	                                                  _\_, Ent, Ch, {M1,M}),
	update_psi(A2Psi, Ent, R2, {DicM1,DicO}, Ch, {M,R}) ;

	Arg1 = {A2,Ent2}, Ent2 = {R2,_,ref(var),_}, R2 = a(_), A2 = psi(A2Psi),
	Arg2 = {A1,_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	CtlM = [deref(R2,R2),multiple_assign([R1],[R2])|CtlT],
	St = imm |
        ground_check#allocate_multiple_vars(A1, CtlH\CtlM1,
	                                             {DicI,Dic1}, Ch, {L,M1}),
	tell_term_creation#term_creation(A1, {Dic1,DicM}, R1, CtlM1\CtlM, 
	                                                  _\_, Ent, Ch, {M1,M}),
	update_psi(A2Psi, Ent, R2, {DicM,DicO}, Ch, {M,R}) ;

	Arg1 = {A2,Ent2}, Ent2 = {R2,_,ref(var),_}, R2 = {a(_),_},
	A2 = psi(A2Psi), Arg2 = {A1,_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	CtlM = [deref(R2,Reg),multiple_assign([R1],[Reg])|CtlT],
	write_channel(get_reg(Reg,{DicM,DicM1}),Ch),
	St = imm |
        ground_check#allocate_multiple_vars(A1, CtlH\CtlM1,
	                                             {DicI,Dic1}, Ch, {L,M1}),
	tell_term_creation#term_creation(A1, {Dic1,DicM}, R1, CtlM1\CtlM, 
	                                                  _\_, Ent, Ch, {M1,M}),
	update_psi(A2Psi, Ent, R2, {DicM1,DicO}, Ch, {M,R}) ;

	Arg1 = {A2,Ent2}, Ent2 = {R2,_,car(var),_}, A2 = psi(A2Psi),
	Arg2 = {A1,_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	write_channel(get_reg(Reg,{DicM,DicM1}),Ch),
	CtlM = [deref_car(R2,Reg),multiple_assign([R1],[Reg])|CtlT],
	St = imm |
        ground_check#allocate_multiple_vars(A1, CtlH\CtlM1,
	                                             {DicI,Dic1}, Ch, {L,M1}),
	tell_term_creation#term_creation(A1, {Dic1,DicM}, R1, CtlM1\CtlM, 
	                                                  _\_, Ent, Ch, {M1,M}),
	update_psi(A2Psi, Ent, R2, {DicM1,DicO}, Ch, {M,R}) ;

	Arg1 = {A2,Ent2}, Ent2 = {_,_,new,_}, A2 = psi(A2Psi),	
	Arg2 = {A1,Ent1}, Ent1 =\= {_,_,new,_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	St = imm |
        ground_check#allocate_multiple_vars(A1, CtlH\CtlM,
	                                             {DicI,Dic1}, Ch, {L,M}),
	new_term_creation#new_term(A2Psi, A1, CtlM\CtlT, {Dic1,DicO}, 
	                                                          Ch, {M,R}) ;

	otherwise, 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	CtlH = CtlT, DicO = DicI, L = R, St = no, 
	Arg1 = _, Arg2 = _, Ch = _.

update_psi(Psi, TV, Arg, Dic, Ch,  SC)
:-
	TV = {Tp,Vl}, SC = {L,R} :
	write_channel(update(Psi,{Arg,'*',Tp,Vl},Dic),Ch),
	R = L  ;

	TV = {Reg,Tp,Vl}, SC = {L,R} :
	write_channel(update(Psi,{Reg,'*',Tp,Vl},Dic),Ch),
	R = L, Arg = _.

eliminate_imm(St, Body, Bodies, BodiesO, Ind, SC) 
:-
	St = imm, Ind = {_,IR}, SC = {L,R} :
	BodiesO = Bodies, IR = imm, L = R, Body = _;

	St = no, Ind = {IL,IR}, SC = {L,R} :
	BodiesO = [spawn(Body)|Bodies], IR = IL, L = R.

get_opcode(Pred, Opcode)
:-
	Pred = plus : Opcode = increment ;
	Pred = diff : Opcode = decrement.
	
get_num(Pred, Src, Num)
:-
	Pred = plus | Num := Src + 1;
	Pred = diff | Num := Src - 1.
	
reverse(Xs, Ys)
:-
	Xs ? X | reverse(Xs',Zs), append(Zs,[X],Ys) ;
	Xs = [] : Ys =[] .

append(Xs, Ys, Zs)
:-
	Xs ? X : Zs ! X | append ;
	Xs = [] : Zs = Ys.
