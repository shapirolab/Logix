/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/ask_arithmetic.cp,v 1.1 1999/07/09 07:03:00 bill Exp $ */
-language(compound).
-export([arithmetic/16]).
-mode(trust).

Types ::= false ; ow ; ok ; guard.

arithmetic(Types, Pred, Psi1, Entry1, Psi2, Entry2, Psi3, Entry3,
			Labels, BH, Dic, Ctl, Ch, Name, SC, Done)
:-
	Types = false :
	Pred = _, Psi1 = _, Entry1 = _, Psi2 = _, Entry2 = _ ,
	Psi3 = _, Entry3 = _ |
	ask_utils#select_sdgs(Types, Labels, BH, Dic, Ctl, Ch, Name, SC, Done);

	Types = ow, Ctl = CtlH\CtlT, SC = {L,R}, Ch = {_,Ch1} :
	Pred = _, Psi1 = _, Psi2 = _, Psi3 = _ |
	get_suspend_on([{Psi1,Entry1},{Psi2,Entry2},{Psi3,Entry3}], 
				{Dic,Dic1}, CtlH\CtlM, {L,M}, Ch1),
	ask_utils#select_sdgs(Types, Labels, BH, Dic1, 
					CtlM\CtlT, Ch, Name, {M,R}, Done);

	Types = ok,	Entry2 = {_,_,_,1}, Psi3 = psi(Ps3),
	Ctl = CtlH\CtlT, Ch = {_,Ch1}, SC = {L,R} :
	write_channel(get_reg(A3,{Dic1,DicM}),Ch1),
	write_channel(add(Ps3,{A3,A3,integer,'*'},{DicM,DicT}),Ch1),
	Psi2 = _, Entry3 = _ |
        get_entry(Psi1, Entry1, {Dic,Dic1}, A1, 
	                         CtlH\[{OpCode,A1,A3}|CtlM], Ch1, {L,M}),
	get_opcode(Pred, OpCode),
	ask_utils#select_sdgs(true, Labels, BH, [DicT,DicM], CtlM\CtlT, 
							Ch, Name, {M,R}, Done) ;

	Types = ok,	Entry2 =\= {_,_,_,1}, Psi3 = psi(Ps3),
	Ctl = CtlH\CtlT, Ch = {_,Ch1}, SC = {L,R} :
	write_channel(get_reg(A3,{Dic2,DicM}),Ch1),
	write_channel(add(Ps3,{A3,A3,integer,'*'},{DicM,DicT}),Ch1),
	Psi2 = _, Entry3 = _ |
        get_entry(Psi1, Entry1, {Dic,Dic1}, A1, CtlH\CtlM, Ch1, {L,M}),
        get_entry(Psi2, Entry2, {Dic1,Dic2}, A2, 
	                         CtlM\[{Pred,A1,A2,A3}|CtlM1], Ch1, {M,M1}),
	ask_utils#select_sdgs(true, Labels, BH, [DicT,DicM], CtlM1\CtlT, 
							Ch, Name, {M1,R}, Done) ;

	Types = guard,
	Ctl = CtlH\CtlT, SC = {L,R}, Ch = {_,Ch1} |
	create_args([{Psi1,Entry1},{Psi2,Entry2},{Psi3,Entry3}],
		{Dic,DicT}, {Dic,DicO}, Args, CtlH\CtlM, Ch1, {L,M}),
	ask_utils#compile_guard(Pred, Args, both, Labels, BH, DicT, DicO,
					CtlM\CtlT, Ch, Name, {M,R}, Done).

get_entry(Psi, Entry, Dic, Reg, Ctl, Ch, SC)
:-
	Entry = {A,_,_,_}, integer(A),
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} : 
        Reg = A, DicO = DicI, CtlH = CtlT, L = R, Psi = _, Ch = _ ;

	Entry = {_,V,integer,_}, V = a(_),
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} : 
        Reg = V, DicO = DicI, CtlH = CtlT, L = R, Psi = _, Ch = _ ;

	Entry = {A,'*',integer,I}, A = a(_), 
	Psi = psi(Ps), Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
        CtlH = [deref_value(A,Reg)|CtlT],
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	write_channel(update(Ps,{Reg,Reg,integer,I},{DicM,DicO}),Ch),
        L = R ;

	Entry = {A,'*',integer,I}, A = {a(_),_}, 
	Psi = psi(Ps), Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
        write_channel(get_reg(Reg,{DicI,DicM}),Ch),
        write_channel(get_reg(Reg1,{DicM,DicM1}),Ch),
        CtlH = [deref(A,Reg1,Reg)|CtlT],
	write_channel(update(Ps,{Reg1,Reg,integer,I},{DicM1,DicO}),Ch),
        L = R ;

	Entry = {A,'*',car(integer),I}, 
	Psi = psi(Ps), Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
        write_channel(get_reg(Reg,{DicI,DicM}),Ch),
        write_channel(get_reg(Reg1,{DicM,DicM1}),Ch),
        CtlH = [deref_car(A,Reg1,Reg)|CtlT],
	write_channel(update(Ps,{Reg1,Reg,integer,I},{DicM1,DicO}),Ch),
        L = R.

create_args(Psis, DicT, DicF, Args, Ctl, Ch, SC)
:-
	Psis ? {Psi,Entry},	
	DicT = {DicTI,DicTO}, DicF = {DicFI,DicFO}, Ctl = CtlH\CtlT, SC = {L,R} :
	Args ! A,
	DicT' = {DicTM,DicTO}, DicF' = {DicFM,DicFO},
	Ctl' = CtlM\CtlT, SC' = {M,R} |
	create_arg(Psi, Entry, A, CtlH\CtlM, 
	                               {DicTI,DicTM}, {DicFI,DicFM}, Ch, {L,M}),
	create_args ;

	Psis = [],
	DicT = {DicTI,DicTO}, DicF = {DicFI,DicFO}, Ctl = CtlH\CtlT, SC = {L,R} :
	Args = [], DicTI = DicTO, DicFI = DicFO, CtlH = CtlT, L = R, Ch = _.

create_arg(Psi, Entry, Arg, Ctl, DicT, DicO, Ch, SC)
:-
	Entry = {A,_,ref,_}, Psi = psi(Ps), 
	Ctl = CtlH\CtlT, SC = {L,R}, DicO = {DicOI,DicOO} :
	CtlH = CtlT, Arg = A, L = R, DicOO = DicOI,
	write_channel(update_type(Ps,number,DicT),Ch) ;

	Entry = {A,_,car,_}, Psi = psi(Ps),
	DicT = {DicTI,DicTO}, DicO = {DicOI,DicOO}, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Reg,{DicTI,DicTI1}),Ch), 	
	write_channel(get_reg(Reg,{DicOI,DicOI1}),Ch), 	
	CtlH = [deref_car(A,Reg)|CtlT], Arg = Reg, L = R,
	write_channel(update(Ps,{Reg,'*',number,'*'},{DicTI1,DicTO}),Ch),
	write_channel(update(Ps,{Reg,'*',ref,'*'},{DicOI1,DicOO}),Ch) ;

	Entry = {A,_,car(Type),_}, Type =\= integer, Type =\= var, 
	Psi = psi(Ps),
	DicT = {DicTI,DicTO}, DicO = {DicOI,DicOO}, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Reg,{DicTI,DicTI1}),Ch), 	
	write_channel(get_reg(Reg,{DicOI,DicOI1}),Ch), 	
	CtlH = [deref_car(A,Reg)|CtlT], Arg = Reg, L = R,
	write_channel(update(Ps,{Reg,'*',number,'*'},{DicTI1,DicTO}),Ch),
	write_channel(update(Ps,{Reg,'*',Type,'*'},{DicOI1,DicOO}),Ch) ;

	Entry = {A,_,car(Type),I}, Type = integer, Psi = psi(Ps),
	DicT = {DicTI,DicTO}, DicO = {DicOI,DicOO}, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Reg,{DicTI,DicTI1}),Ch), 	
	write_channel(get_reg(Reg,{DicOI,DicOI1}),Ch), 	
	CtlH = [deref_car(A,Reg)|CtlT], Arg = Reg, L = R, 
	write_channel(update(Ps,{Reg,'*',Type,I},{DicTI1,DicTO}),Ch),
	write_channel(update(Ps,{Reg,'*',Type,I},{DicOI1,DicOO}),Ch) ;

	Entry = {A,_,car(Type),_}, Type = var, Psi = psi(Ps),
	DicT = {DicTI,DicTO}, DicO = {DicOI,DicOO}, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Reg,{DicTI,DicTI1}),Ch), 	
	write_channel(get_reg(Reg,{DicOI,DicOI1}),Ch), 	
	CtlH = [deref_car(A,Reg)|CtlT], Arg = Reg, L = R,
	write_channel(update(Ps,{Reg,'*',number,'*'},{DicTI1,DicTO}),Ch),
	write_channel(update(Ps,{Reg,'*',ref,'*'},{DicOI1,DicOO}),Ch) ;

	Entry = {A,_,cdr,_}, Psi = psi(Ps),
	DicT = {DicTI,DicTO}, DicO = {DicOI,DicOO},
	Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Reg,{DicTI,DicTI1}),Ch), 	
	write_channel(get_reg(Reg,{DicOI,DicOI1}),Ch), 	
	CtlH = [deref(A,Reg)|CtlT], Arg = Reg, L = R,
	write_channel(update(Ps,{Reg,'*',number,'*'},{DicTI1,DicTO}),Ch),
        write_channel(update(Ps,{Reg,'*',ref,'*'},{DicOI1,DicOO}),Ch) ;

	Entry = {A,_,sub_arg,_}, Psi = psi(Ps),
	DicT = {DicTI,DicTO}, DicO = {DicOI,DicOO}, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Reg,{DicTI,DicTI1}),Ch), 	
	write_channel(get_reg(Reg,{DicOI,DicOI1}),Ch), 	
	CtlH = [deref(A,Reg)|CtlT], Arg = Reg, L = R,
	write_channel(update(Ps,{Reg,'*',number,'*'},{DicTI1,DicTO}),Ch),
        write_channel(update(Ps,{Reg,'*',ref,'*'},{DicOI1,DicOO}),Ch) ;

	Entry = {A,_,deref,_}, Psi = psi(Ps),
	Ctl = CtlH\CtlT, SC = {L,R}, DicO = {DicOI,DicOO} :
	CtlH = CtlT, Arg = A, L = R, DicOO = DicOI,
	write_channel(update_type(Ps,number,DicT),Ch) ;

	Entry = {A,_,known,_}, Psi = psi(Ps),
	Ctl = CtlH\CtlT, SC = {L,R}, DicO = {DicOI,DicOO} :
	CtlH = CtlT, Arg = A, L = R, DicOO = DicOI,
	write_channel(update_type(Ps,number,DicT),Ch) ;

	Entry = {A,_,constant,_}, Psi = psi(Ps),
	Ctl = CtlH\CtlT, SC = {L,R}, DicO = {DicOI,DicOO} :
	CtlH = CtlT, Arg = A, L = R, DicOO = DicOI,
	write_channel(update_type(Ps,number,DicT),Ch) ;

	Entry = {A,_,integer,_}, 
	Ctl = CtlH\CtlT, SC = {L,R},DicO = {DicOI,DicOO} , DicT = {DicTI,DicTO} :
	CtlH = CtlT, Arg = A, L = R, DicOO = DicOI, DicTO = DicTI,
	Ch = _, Psi = _ ;

	Entry = {A,_,real,_},
	Ctl = CtlH\CtlT, SC = {L,R},DicO = {DicOI,DicOO} , DicT = {DicTI,DicTO} :
	CtlH = CtlT, Arg = A, L = R, DicOO = DicOI, DicTO = DicTI,
	Ch = _, Psi = _ ;

	Entry = {A,_,number,_},
	Ctl = CtlH\CtlT, SC = {L,R},DicO = {DicOI,DicOO} , DicT = {DicTI,DicTO} :
	CtlH = CtlT, Arg = A, L = R, DicOO = DicOI, DicTO = DicTI,
	Ch = _, Psi = _ ;

	Entry = {_,_,new,_}, Psi = psi(Ps),
	DicT = {DicTI,DicTO}, DicO = {DicOI,DicOO},
	Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Reg,{DicTI,DicTI1}),Ch), 	
	write_channel(get_reg(Reg,{DicOI,DicOI1}),Ch), 	
	CtlH = [allocate_var(Reg)|CtlT], Arg = Reg, L = R, 
	write_channel(add(Ps,{Reg,'*',number,'*'},{DicTI1,DicTO}),Ch),
        write_channel(add(Ps,{Reg,'*',ref,'*'},{DicOI1,DicOO}),Ch).

get_opcode(Pred, OpCode)
:-
	Pred = plus : OpCode = increment ;
	Pred = diff : OpCode = decrement.

get_suspend_on(Entries, Dic, Ctl, SC, Ch)
:-
	Entries ? {_,{A,_,var,_}}, A = a(_), Ctl = CtlH\CtlT : 
	CtlH ! suspend_on(A), Ctl' = CtlH'\CtlT | 
	get_suspend_on ;

	Entries ? {psi(Ps),{A,_,var,_}}, A = {a(_),_}, 
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	write_channel(update(Ps,{Reg,'*',var,'*'},{DicM,DicM'}),Ch),
	CtlH = [deref(A,Reg),suspend_on(Reg)|CtlH'], 
	Dic' = {DicM',DicO}, Ctl' = CtlH'\CtlT | 
	get_suspend_on ;

	Entries ? {psi(Ps),{A,_,ref(var),_}}, A = {a(_),_}, 
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	write_channel(update(Ps,{Reg,'*',var,'*'},{DicM,DicM'}),Ch),
	CtlH = [deref(A,Reg),suspend_on(Reg)|CtlH'], 
	Dic' = {DicM',DicO}, Ctl' = CtlH'\CtlT | 
	get_suspend_on ;

	Entries ? {psi(Ps),{A,_,ref(var),_}}, A = a(_), 
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} :
	write_channel(update(Ps,{A,'*',var,'*'},{DicI,DicM'}),Ch),
	CtlH = [deref(A,A),suspend_on(A)|CtlH'], 
	Dic' = {DicM',DicO}, Ctl' = CtlH'\CtlT | 
	get_suspend_on ;

	Entries ? {psi(Ps),{A,_,car(var),_}}, 
	Ctl = CtlH\CtlT, Dic = {DicI,DicO} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	write_channel(update(Ps,{Reg,'*',var,'*'},{DicM,DicM'}),Ch),
	CtlH = [deref_car(A,Reg),suspend_on(Reg)|CtlH'], 
	Dic' = {DicM',DicO}, Ctl' = CtlH'\CtlT | 
	get_suspend_on ;

	Entries ? {_,{_,_,T,_}}, 
	T =\= var, T =\= ref(var), T =\= car(var) | get_suspend_on ;
	
	Entries = [], Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	DicO = DicI, CtlT = CtlH, R = L, Ch = _ .
 
