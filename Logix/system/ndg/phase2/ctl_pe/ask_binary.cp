/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/ask_binary.cp,v 1.1 1999/07/09 07:03:03 bill Exp $ */
-language(compound).
-export([binary_arith/13]).
-mode(trust).

binary_arith(Pred, Psi1, Entry1, Psi2, Entry2, Labels, 
					BH, Dic, Ctl, Ch, Name, SC, Done) 
:-
	Entry1 = {_,_,integer,_}, Entry2 = {_,_,integer,_}, 
	Labels = [true(True),false(False),ow(Ow)], Ow = {Lab,_}, Lab = label(_),
	Ctl = CtlH\CtlT, Ch = {_,Ch1}, SC = {L,R}, Done = {DL,DR} |
        get_entry(Psi1, Entry1, {Dic,Dic1}, A1, CtlH\CtlM, Ch1, {L,M}),
	get_entry(Psi2, Entry2, {Dic1,Dic2}, A2, 
	                  CtlM\[compare_integer(A1,Pred,A2,label(Fl))|CtlM1], 
			  Ch1, {M,M1}),
	get_cont(Lab, BH, BH'),
	ctl#ctl1(True, BH', Dic2, CtlM1\[label(Fl)|CtlM2], 
					Ch, Name, {M1,M2}, {DL,DM1}),
	ctl#ctl1(False, BH', Dic2, CtlM2\CtlM3, Ch, Name, {M2,M3}, {DM1,DM3}),
	ctl#ctl1(Ow, BH, Dic2, CtlM3\CtlT, Ch, Name, {M3,R}, {DM3,DR}) ;
	
	Entry1 = {A1,_,car(integer),I}, Entry2 = {_,_,integer,_}, 
	Labels = [true(True),false(False),ow(Ow)], Ow = {Lab,_}, Lab = label(_),
	Psi1 = psi(Ps), 
	Ctl = CtlH\CtlT, Ch = {_,Ch1}, SC = {L,R}, Done = {DL,DR} :
	write_channel(get_2_regs(Reg,Reg1,Dic1,DicM1),Ch1),
	write_channel(update(Ps,{Reg1,Reg,integer,I},{DicM1,DicM2}),Ch1) |
	get_entry(Psi2, Entry2, {Dic,Dic1}, A2, 
	                  CtlH\[deref_car(A1,Reg1,Reg),
			        compare_integer(Reg,Pred,A2,label(Fl))|CtlM], 
			  Ch1, {L,M1}),
	get_cont(Lab, BH, BH'),
	ctl#ctl1(True, BH', DicM2, CtlM\[label(Fl)|CtlM2], 
					Ch, Name, {M1,M2}, {DL,DM1}),
	ctl#ctl1(False, BH', DicM2, CtlM2\CtlM3, Ch, Name, {M2,M3}, {DM1,DM3}),
	ctl#ctl1(Ow, BH, DicM2, CtlM3\CtlT, Ch, Name, {M3,R}, {DM3,DR}) ;
	
	Entry2 = {A2,_,car(integer),I}, Entry1 = {_,_,integer,_}, 
	Labels = [true(True),false(False),ow(Ow)], Ow = {Lab,_}, Lab = label(_),
	Psi2 = psi(Ps), 
	Ctl = CtlH\CtlT, Ch = {_,Ch1}, SC = {L,R}, Done = {DL,DR} :
	write_channel(get_2_regs(Reg,Reg1,Dic1,DicM1),Ch1),
	write_channel(update(Ps,{Reg1,Reg,integer,I},{DicM1,DicM2}),Ch1) |
	get_entry(Psi1, Entry1, {Dic,Dic1}, A1, 
	                  CtlH\[deref_car(A2,Reg1,Reg),
			        compare_integer(A1,Pred,Reg,label(Fl))|CtlM1], 
			  Ch1, {L,M1}),
	get_cont(Lab, BH, BH'),
	ctl#ctl1(True, BH', DicM2, CtlM1\[label(Fl)|CtlM2], 
					Ch, Name, {M1,M2}, {DL,DM1}),
	ctl#ctl1(False, BH', DicM2, CtlM2\CtlM3, Ch, Name, {M2,M3}, {DM1,DM3}),
	ctl#ctl1(Ow, BH, DicM2, CtlM3\CtlT, Ch, Name, {M3,R}, {DM3,DR}) ;
	
	Entry1 = {A1,_,car(integer),I1}, Entry2 = {A2,_,car(integer),I2},
	Labels = [true(True),false(False),ow(Ow)], Ow = {Lab,_}, Lab = label(_),
	Psi1 = psi(Ps1), Psi2 = psi(Ps2),
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
	write_channel(get_4_regs(Reg1,Reg11,Reg2,Reg22,Dic,DicM3),Ch1),
	CtlH = [deref_car(A1,Reg11,Reg1),
		deref_car(A2,Reg22,Reg2),
		compare_integer(Reg1,Pred,Reg2,label(Fl))|CtlM1],
	write_channel(update(Ps1,{Reg11,Reg1,integer,I1},{DicM3,DicM4}),Ch1),
	write_channel(update(Ps2,{Reg22,Reg2,integer,I2},{DicM4,DicM5}),Ch1) |
	get_cont(Lab, BH, BH'),
	ctl#ctl1(True, BH', DicM5, CtlM1\[label(Fl)|CtlM2], 
					Ch, Name, {L,M2}, {DL,DM1}),
	ctl#ctl1(False, BH', DicM5, CtlM2\CtlM3, Ch, Name, {M2,M3}, {DM1,DM3}),
	ctl#ctl1(Ow, BH, DicM5, CtlM3\CtlT, Ch, Name, {M3,R}, {DM3,DR}) ;
	
	Entry1 = {_,_,integer,_}, Entry2 = {_,_,integer,_}, 
	Labels = [true(True),false(False),ow(Ow)], Ow =\= {label(_),_},
	Ctl = CtlH\CtlT, Ch = {_,Ch1}, SC = {L,R}, Done = {DL,DR} |
        get_entry(Psi1, Entry1, {Dic,Dic1}, A1, CtlH\CtlM, Ch1, {L,M}),
	get_entry(Psi2, Entry2, {Dic1,Dic2}, A2, 
	                  CtlM\[compare_integer(A1,Pred,A2,label(Fl))|CtlM1], 
			  Ch1, {M,M1}),
	ctl#ctl1(True, BH, Dic2, CtlM1\[label(Fl)|CtlM2], 
					Ch, Name, {M1,M2}, {DL,DM1}),
	ctl#ctl1(False, BH, Dic2, CtlM2\CtlT, Ch, Name, {M2,R}, {DM1,DR});
	
	Entry1 = {A1,_,car(integer),I}, Entry2 = {_,_,integer,_}, 
	Labels = [true(True),false(False),ow(Ow)], Ow =\= {label(_),_},
	Psi1 = psi(Ps), 
	Ctl = CtlH\CtlT, Ch = {_,Ch1}, SC = {L,R}, Done = {DL,DR} :
	write_channel(get_2_regs(Reg,Reg1,Dic1,DicM1),Ch1),
	write_channel(update(Ps,{Reg1,Reg,integer,I},{DicM1,DicM2}),Ch1) |
	get_entry(Psi2, Entry2, {Dic,Dic1}, A2, 
	                  CtlH\[deref_car(A1,Reg1,Reg),
			        compare_integer(Reg,Pred,A2,label(Fl))|CtlM], 
			  Ch1, {L,M1}),
	ctl#ctl1(True, BH, DicM2, CtlM\[label(Fl)|CtlM2], 
					Ch, Name, {M1,M2}, {DL,DM1}),
	ctl#ctl1(False, BH, DicM2, CtlM2\CtlT, Ch, Name, {M2,R}, {DM1,DR});
	
	Entry2 = {A2,_,car(integer),I}, Entry1 = {_,_,integer,_}, 
	Labels = [true(True),false(False),ow(Ow)], Ow =\= {label(_),_},
	Psi2 = psi(Ps), 
	Ctl = CtlH\CtlT, Ch = {_,Ch1}, SC = {L,R}, Done = {DL,DR} :
	write_channel(get_2_regs(Reg,Reg1,Dic1,DicM1),Ch1),
	write_channel(update(Ps,{Reg1,Reg,integer,I},{DicM1,DicM2}),Ch1) |
	get_entry(Psi1, Entry1, {Dic,Dic1}, A1, 
	                  CtlH\[deref_car(A2,Reg1,Reg),
			        compare_integer(A1,Pred,Reg,label(Fl))|CtlM1], 
			  Ch1, {L,M1}),
	ctl#ctl1(True, BH, DicM2, CtlM1\[label(Fl)|CtlM2], 
					Ch, Name, {M1,M2}, {DL,DM1}),
	ctl#ctl1(False, BH, DicM2, CtlM2\CtlT, Ch, Name, {M2,R}, {DM1,DR});
	
	Entry1 = {A1,_,car(integer),I1}, Entry2 = {A2,_,car(integer),I2},
	Labels = [true(True),false(False),ow(Ow)], Ow =\= {label(_),_},
	Psi1 = psi(Ps1), Psi2 = psi(Ps2),
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
	write_channel(get_4_regs(Reg1,Reg11,Reg2,Reg22,Dic,DicM3),Ch1),
	CtlH = [deref_car(A1,Reg11,Reg1),
		deref_car(A2,Reg22,Reg2),
		compare_integer(Reg1,Pred,Reg2,label(Fl))|CtlM1],
	write_channel(update(Ps1,{Reg11,Reg1,integer,I1},{DicM3,DicM4}),Ch1),
	write_channel(update(Ps2,{Reg22,Reg2,integer,I2},{DicM4,DicM5}),Ch1) |
	ctl#ctl1(True, BH, DicM5, CtlM1\[label(Fl)|CtlM2], 
					Ch, Name, {L,M2}, {DL,DM1}),
	ctl#ctl1(False, BH, DicM5, CtlM2\CtlT, Ch, Name, {M2,R}, {DM1,DR});
	
	otherwise,
	Ctl = CtlH\CtlT, SC = {L,R}, Ch = {_,Ch1} |
	create_arithmetic_arg(Psi1, Entry1, A1, CtlH\CtlM,
	                                               {Dic,DicM}, Ch1, {L,M}),
	create_arithmetic_arg(Psi2, Entry2, A2, CtlM\CtlM1,
				               {DicM,DicO}, Ch1, {M,M1}),
	ask_utils#compile_guard(Pred, [A1,A2], both, Labels, BH, DicO, DicO,
				CtlM1\CtlT, Ch, Name, {M1,R}, Done).

get_entry(Psi, Entry, Dic, Reg, Ctl, Ch, SC)
:-
	Entry = {I,_,_,_}, integer(I),
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} : 
        Reg = I, DicO = DicI, CtlH = CtlT, L = R, Psi = _, Ch = _ ;

	Entry = {_,V,_,_}, V = a(_),
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} : 
        Reg = V, DicO = DicI, CtlH = CtlT, L = R, Psi = _, Ch = _ ;

	Entry = {_,'*',_,I}, I =\= '*',
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} : 
        Reg = I, DicO = DicI, CtlH = CtlT, L = R, Psi = _, Ch = _ ;

	Entry = {A,'*',_,'*'}, A = a(_), 
	Psi = psi(Ps), Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} : 
	CtlH = [deref_value(A,Reg)|CtlT],
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	write_channel(update(Ps,{A,Reg,integer,'*'},{DicM,DicO}),Ch),
        L = R ;

	Entry = {A,'*',_,'*'}, A = {a(_),_}, 
	Psi = psi(Ps), Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} : 
        write_channel(get_2_regs(Reg,Reg1,DicI,DicM1),Ch),
	CtlH = [deref(A,Reg1,Reg)|CtlT],
	write_channel(update(Ps,{Reg1,Reg,integer,'*'},{DicM1,DicO}),Ch),
        L = R.

create_arithmetic_arg(Psi, Entry, Arg, Ctl, Dic, Ch, SC)
:-
	Entry = {Reg,_,car(Type),I}, Psi = psi(Ps),
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	write_channel(get_reg(Arg,{DicI,DicM}),Ch),
	CtlH = [load_car(Reg,Arg)|CtlT],
	write_channel(update(Ps,{Arg,'*',Type,I},{DicM,DicO}),Ch),
	R = L ;

	Entry = {Reg,_,car,I}, Psi = psi(Ps),
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	write_channel(get_reg(Arg,{DicI,DicM}),Ch),
	CtlH = [load_car(Reg,Arg)|CtlT],
	write_channel(update(Ps,{Arg,'*',deref,I},{DicM,DicO}),Ch),
	R = L ;

	Entry = {Reg,_,Type,I}, Type = integer, Reg = {a(_),_}, Psi = psi(Ps),
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	write_channel(get_reg(Arg,{DicI,DicM1}),Ch),
	CtlH = [multiple_copy([Reg],[Arg])|CtlT],
	write_channel(update(Ps,{Arg,'*',Type,I},{DicM1,DicO}),Ch),
	R = L ;

	Entry = {Reg,_,integer,_}, Reg = a(_),
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	Arg = Reg, CtlH = CtlT, DicO = DicI, R = L, Psi = _, Ch = _ ;

	Entry = {Reg,_,Type,_}, Type =\= integer, Reg = {a(_),_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	Arg = {'&',Reg}, CtlH = CtlT, DicO = DicI, R = L,
	Psi = _, Ch = _ ;

	otherwise, Entry = {Reg,_,_,_}, 
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	Arg = Reg, CtlH = CtlT, DicO = DicI, R = L, Psi = _, Ch = _.

get_cont(Ow, BH, BHO)
:-
	BH = {B,H,_} : BHO = {B,H,goto(Ow)}.
