/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/ask_type_check.cp,v 1.1 1999/07/09 07:03:01 bill Exp $ */
-language(compound).
-export([type_check/11]).
-mode(trust).

procedure type_check(Tp, Psi, Entry, Labels, BH, Dic, Ctl, Ch, Name, SC, Done).

type_check(Tp, Psi, Entry, Labels, BH, Dic, Ctl, Ch, Name, SC, Done) 
:-
	Entry = {Areg,_,car(Tp1),_}, Ctl = CtlH\CtlT, Ch = {_,Ch1} :
	write_channel(get_reg(Reg,{Dic,DicM}),Ch1),
	CtlH = [load_car(Areg,Reg)|CtlH'],
	write_channel(update(Psi,{Reg,'*',Tp1,'*'},{DicM,Dic'}),Ch1),
	Entry' = {Reg,'*',Tp1,'*'}, 
	Ctl' = CtlH'\CtlT | 
        type_check;

	Entry = {Areg,_,ref(Tp1),_}, Areg = {a(_),_}, 
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_reg(Vreg,{Dic,Dic1}),Ch1),
	write_channel(update(Psi,{Vreg,'*',Tp1,'*'},{Dic1,Dic'}),Ch1),
	CtlH ! deref(Areg,Vreg),
	Entry' = {Vreg,'*',Tp1,'*'}, 
	Ctl' = CtlH'\CtlT | 
        type_check;

	Entry = {Areg,_,ref(Tp1),_}, 
	Areg = a(_), Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(update_type(Psi,Tp1,{Dic,Dic'}),Ch1),
	CtlH ! deref(Areg,Areg),
	Entry' = {Areg,'*',Tp1,'*'}, 
	Ctl' = CtlH'\CtlT | 
        type_check;


	Entry = {Areg,_,Type,_}, Type = ref, Tp =\= integer, Ch = {_,Ch1} :
	write_channel(update_type(Psi,Tp1,{Dic,DicT}),Ch1) |
	get_type(Tp, Type, Tp1),
	ask_utils#compile_guard(Tp, [Areg], both, Labels, BH, DicT, Dic, 
					Ctl, Ch, Name, SC, Done) ;

	Entry = {Areg,_,ref,_}, Tp = integer, 
	Labels = [true(True),false(False),ow(Ow)],
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
	CtlH ! deref_integer(Areg,[label(Fl),label(Sus)]),
	write_channel(update(Psi,{Areg,Areg,Tp,'*'},{Dic,DicT}),Ch1) |
           get_cont(Ow, BH, BH'),
	   ctl#ctl1(True, BH', DicT, CtlH'\[label(Fl)|CtlM], 
						Ch, Name, {L,M}, {DL,DM}),
	   ctl#ctl1(False, BH', Dic, CtlM\[label(Sus)|CtlM2],
						Ch, Name, {M,M1}, {DM,DM1}),
	   ctl#ctl1(Ow, BH, Dic, CtlM2\CtlT, Ch, Name, {M1,R}, {DM1,DR}) ;

	Entry = {Areg,_,Type,_}, Type = cdr, Tp =\= integer,
	Ctl = CtlH\CtlT, Ch = {_,Ch1} :
	CtlH ! multiple_copy([Areg],[Reg]),
	write_channel(get_reg(Reg,{Dic,DicM}),Ch1),
	write_channel(update(Psi,{Reg,'*',Tp1,'*'},{DicM,DicT}),Ch1) |
	get_type(Tp, Type, Tp1),
	ask_utils#compile_guard(Tp, [Reg], both, Labels, BH, DicT, DicM, 
					CtlH'\CtlT, Ch, Name, SC, Done) ;

	Entry = {Areg,_,cdr,_}, Tp = integer, 
	Labels = [true(True),false(False),ow(Ow)],
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
	write_channel(get_reg(Reg,{Dic,DicM}),Ch1),
	CtlH = [multiple_copy([Areg],[Reg]),
		deref_integer(Reg,[label(Fl),label(Sus)])|CtlH'],
	write_channel(update(Psi,{Reg,Reg,Tp,'*'},{DicM,DicT}),Ch1) |
           get_cont(Ow, BH, BH'),
	   ctl#ctl1(True, BH', DicT, CtlH'\[label(Fl)|CtlM], 
						Ch, Name, {L,M}, {DL,DM}),
	   ctl#ctl1(False, BH', DicM, CtlM\[label(Sus)|CtlM2],
						Ch, Name, {M,M1}, {DM,DM1}),
	   ctl#ctl1(Ow, BH, DicM, CtlM2\CtlT, Ch, Name, {M1,R}, {DM1,DR}) ;

	Entry = {Areg,_,Type,_}, Type = sub_arg, Tp =\= integer,
	Ctl = CtlH\CtlT, Ch = {_,Ch1} :
	CtlH ! multiple_copy([{'&',Areg}],[Reg]),
	write_channel(get_reg(Reg,{Dic,DicM}),Ch1),
	write_channel(update(Psi,{Reg,'*',Tp1,'*'},{DicM,DicT}),Ch1) |
	get_type(Tp, Type, Tp1),
	ask_utils#compile_guard(Tp, [Reg], both, Labels, BH, DicT, DicM, 
					CtlH'\CtlT, Ch, Name, SC, Done) ;

	Entry = {Areg,_,sub_arg,_}, Tp = integer, 
	Labels = [true(True),false(False),ow(Ow)],
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
	write_channel(get_reg(Reg,{Dic,DicM}),Ch1),
	CtlH = [multiple_copy([{'&',Areg}],[Reg]),
		deref_integer(Reg,[label(Fl),label(Sus)])|CtlH'],
	write_channel(update(Psi,{Reg,Reg,Tp,'*'},{DicM,DicT}),Ch1) |
           get_cont(Ow, BH, BH'),
	   ctl#ctl1(True, BH', DicT, CtlH'\[label(Fl)|CtlM], 
						Ch, Name, {L,M}, {DL,DM}),
	   ctl#ctl1(False, BH', DicM, CtlM\[label(Sus)|CtlM2],
						Ch, Name, {M,M1}, {DM,DM1}),
	   ctl#ctl1(Ow, BH, DicM, CtlM2\CtlT, Ch, Name, {M1,R}, {DM1,DR}) ;

	Entry = {Areg,_,Type,_}, Type = deref, Ch = {_,Ch1} :
	write_channel(update_type(Psi,Tp1,{Dic,DicT}),Ch1) |
	get_type(Tp, Type, Tp1),
	ask_utils#compile_guard(Tp, [Areg], both, Labels, BH, DicT, Dic, Ctl, 
						Ch, Name, SC, Done) ;

	Entry = {Areg,_,Type,_}, Type = car, 
	Tp =\= integer, Ctl = CtlH\CtlT, Ch = {_,Ch1} :
	CtlH ! load_car(Areg,Reg),
	write_channel(get_reg(Reg,{Dic,DicM}),Ch1),
	write_channel(update(Psi,{Reg,'*',Tp1,'*'},{DicM,DicT}),Ch1) |
%	screen#display({Psi,Tp,Dic,DicT,DicO},type(ground)),
	get_type(Tp, Type, Tp1),
	ask_utils#compile_guard(Tp, [Reg], both, Labels, BH, DicT, DicM, 
					CtlH'\CtlT, Ch, Name, SC, Done) ;

	Entry = {Areg,_,car,_}, Tp = integer, 
	Labels = [true(True),false(False),ow(Ow)],
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
	write_channel(get_reg(Reg,{Dic,DicM}),Ch1),
	CtlH = [load_car(Areg,Reg),
		deref_integer(Reg,[label(Fl),label(Sus)])|CtlH'],
	write_channel(update(Psi,{Reg,Reg,Tp,'*'},{DicM,DicT}),Ch1) |
           get_cont(Ow, BH, BH'),
	   ctl#ctl1(True, BH', DicT, CtlH'\[label(Fl)|CtlM], 
						Ch, Name, {L,M}, {DL,DM}),
	   ctl#ctl1(False, BH', DicM, CtlM\[label(Sus)|CtlM2],
						Ch, Name, {M,M1}, {DM,DM1}),
	   ctl#ctl1(Ow, BH, DicM, CtlM2\CtlT, Ch, Name, {M1,R}, {DM1,DR}) ;

	Entry = {_,_,Tp,_} : Psi = _ |
	ask_utils#select_sdgs(true, Labels, BH, Dic, Ctl, Ch, Name, SC, Done);

	Entry = {Areg,_,number,_}, Tp = integer, Areg = a(_),
	Labels = [true(True),false(False),ow(Ow)],
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
	CtlH ! deref_integer(Areg,[label(Fl),label(Sus)]),
	write_channel(update(Psi,{Areg,Areg,Tp,'*'},{Dic,DicT}),Ch1) |
           get_cont(Ow, BH, BH'),
	   ctl#ctl1(True, BH', DicT, CtlH'\[label(Fl)|CtlM], 
						Ch, Name, {L,M}, {DL,DM}),
	   ctl#ctl1(False, BH', Dic, CtlM\[label(Sus)|CtlM2],
						Ch, Name, {M,M1}, {DM,DM1}),
	   ctl#ctl1(Ow, BH, Dic, CtlM2\CtlT, Ch, Name, {M1,R}, {DM1,DR});

	Entry = {Areg,_,number,_}, Tp = integer, Areg = {a(_),_},
	Labels = [true(True),false(False),ow(Ow)],
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
        write_channel(get_reg(Reg,{Dic,Dic1}),Ch1),
	CtlH ! multiple_copy([{'&',Areg}],[Reg]),
	CtlH' ! deref_integer(Reg,[label(Fl),label(Sus)]),
	write_channel(update(Psi,{Reg,Reg,Tp,'*'},{Dic1,DicT}),Ch1) |
           get_cont(Ow, BH, BH'),
	   ctl#ctl1(True, BH', DicT, CtlH''\[label(Fl)|CtlM], 
						Ch, Name, {L,M}, {DL,DM}),
	   ctl#ctl1(False, BH', Dic1, CtlM\[label(Sus)|CtlM2],
						Ch, Name, {M,M1}, {DM,DM1}),
	   ctl#ctl1(Ow, BH, Dic1, CtlM2\CtlT, Ch, Name, {M1,R}, {DM1,DR}) ;

	Entry = {Areg,_,number,_}, Tp = real, Ch = {_,Ch1} : 
	write_channel(update_type(Psi,Tp,{Dic,DicT}),Ch1) |
	ask_utils#compile_guard(Tp, [Areg], both, Labels, BH, DicT, Dic, Ctl, 
						Ch, Name, SC, Done);

	Entry = {Areg,_,constant,_}, Tp = string, Ch = {_,Ch1} : 
	write_channel(update_type(Psi,Tp,{Dic,DicT}),Ch1) |
	ask_utils#compile_guard(Tp, [Areg], both, Labels, BH, DicT, Dic, Ctl, 
						Ch, Name, SC, Done);

	Entry = {Areg,_,constant,_}, Tp = integer, Areg = a(_),
	Labels = [true(True),false(False),ow(Ow)],
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
	CtlH ! deref_integer(Areg,[label(Fl),label(Sus)]),
	write_channel(update(Psi,{Areg,Areg,Tp,'*'},{Dic,DicT}),Ch1) |
           get_cont(Ow, BH, BH'),
	   ctl#ctl1(True, BH', DicT, CtlH'\[label(Fl)|CtlM], 
						Ch, Name, {L,M}, {DL,DM}),
	   ctl#ctl1(False, BH', Dic, CtlM\[label(Sus)|CtlM2],
						Ch, Name, {M,M1}, {DM,DM1}),
	   ctl#ctl1(Ow, BH, Dic, CtlM2\CtlT, Ch, Name, {M1,R}, {DM1,DR}) ;

	Entry = {Areg,_,constant,_}, Tp = integer, Areg = {a(_),_},
	Labels = [true(True),false(False),ow(Ow)],
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
        write_channel(get_reg(Reg,{Dic,Dic1}),Ch1),
	CtlH ! multiple_copy([{'&',Areg}],[Reg]),
	CtlH' ! deref_integer(Reg,[label(Fl),label(Sus)]),
	write_channel(update(Psi,{Reg,Reg,Tp,'*'},{Dic1,DicT}),Ch1) |
           get_cont(Ow, BH, BH'),
	   ctl#ctl1(True, BH', DicT, CtlH''\[label(Fl)|CtlM], 
						Ch, Name, {L,M}, {DL,DM}),
	   ctl#ctl1(False, BH', Dic1, CtlM\[label(Sus)|CtlM2],
						Ch, Name, {M,M1}, {DM,DM1}),
	   ctl#ctl1(Ow, BH, Dic1, CtlM2\CtlT, Ch, Name, {M1,R}, {DM1,DR}) ;

	Entry = {Areg,_,constant,_}, Tp = real, Ch = {_,Ch1} : 
	write_channel(update_type(Psi,Tp,{Dic,DicT}),Ch1) |
	ask_utils#compile_guard(Tp, [Areg], both, Labels, BH, DicT, Dic, Ctl, 
						Ch, Name, SC, Done);

	Entry = {Areg,_,constant,_}, Tp = number, Ch = {_,Ch1} : 
	write_channel(update_type(Psi,Tp,{Dic,DicT}),Ch1) |
	ask_utils#compile_guard(Tp, [Areg], both, Labels, BH, DicT, Dic, Ctl, 
						Ch, Name, SC, Done);

	Entry = {Areg,_,compound,_}, Tp = list, Ch = {_,Ch1} : 
	write_channel(update_type(Psi,Tp,{Dic,DicT}),Ch1) |
	ask_utils#compile_guard(Tp, [Areg], both, Labels, BH, DicT, Dic, Ctl, 
						Ch, Name, SC, Done);

	Entry = {Areg,_,compound,_}, Tp = tuple, Ch = {_,Ch1} : 
	write_channel(update_type(Psi,Tp,{Dic,DicT}),Ch1) |
	ask_utils#compile_guard(Tp, [Areg], both, Labels, BH, DicT, Dic, Ctl, 
						Ch, Name, SC, Done);

	Entry = {Areg,_,Tp1,_}, Tp1 = known, Tp1 =\= Tp, 
	Tp =\= unknown, Tp=\= var, Ch = {_,Ch1} :
	write_channel(update_type(Psi,Tp,{Dic,DicT}),Ch1) |
	ask_utils#compile_guard(Tp, [Areg], both, Labels, BH, DicT, Dic, Ctl, 
						Ch, Name, SC, Done);

	Entry = {Areg,_,Tp1,_}, Tp1 = ground, Tp1 =\= Tp, 
	Tp =\= unknown, Tp=\= var, Ch = {_,Ch1} :
	write_channel(update_type(Psi,Tp,{Dic,DicT}),Ch1) |
	ask_utils#compile_guard(Tp, [Areg], both, Labels, BH, DicT, Dic, Ctl, 
						Ch, Name, SC, Done);

	Entry = {Areg,_,Tp1,_}, Tp1 = var, Tp =\= unknown, Tp =\= Tp1,
	Areg = a(_), Ctl = CtlH\CtlT : CtlH ! suspend_on(Areg), Psi = _ |
	ask_utils#select_sdgs(ow, Labels, BH, Dic, CtlH'\CtlT, Ch, 
	                                                    Name, SC, Done);

	Entry = {Areg,_,Tp1,_}, Tp1 = var, Tp =\= unknown, Tp =\= Tp1,
	Areg = {a(_),_}, Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_reg(Vreg,{Dic,Dic1}),Ch1),
	write_channel(update(Psi,{Vreg,'*',Tp1,'*'},{Dic1,Dic2}),Ch1),
	CtlH ! deref(Areg,Vreg),
        CtlH' ! suspend_on(Vreg) |
	ask_utils#select_sdgs(ow, Labels, BH, Dic2, CtlH''\CtlT, Ch, 
	                                                    Name, SC, Done);

	
	Entry = {Areg,_,list,_}, Areg = {Reg,-1},
	Ctl = CtlH\CtlT, Ch = {_,Ch1} :
	write_channel(get_reg(Reg1,{Dic,DicM}),Ch1),
	CtlH = [decrement_pointer(Reg,Reg1)|CtlH'],
	write_channel(update(Psi,{Reg1,'*',list,'*'},{DicM,Dic'}),Ch1),
	Entry' = {Reg1,'*',list,'*'},
	Ctl' = CtlH'\CtlT |
	type_check;


	otherwise, Entry = {_,_,Type,_} |
        get_relation(Type, Tp, St),
	type_check1(St, Tp, Psi, Entry, Labels, BH, Dic, Ctl, Ch, Name, 
	                                                         SC, Done).

type_check1(St, Tp, Psi, Entry, Labels, BH, Dic, Ctl, Ch, Name, SC, Done)
:-
	St = compile, Entry = {Areg,_,_,_}, Ch = {_,Ch1} : 
	write_channel(update(Psi,{Areg,'*',Tp,'*'},{Dic,DicT}),Ch1) |
	ask_utils#compile_guard(Tp, [Areg], both, Labels, BH, DicT, Dic, Ctl, 
						Ch, Name, SC, Done) ;

	St =\= compile : Tp = _, Psi = _, Entry = _ |
	ask_utils#select_sdgs(St, Labels, BH, Dic, Ctl, Ch, Name, SC, Done).

get_relation(Type, Tp, St)
:-
	Type = integer, Tp = string : St = false ;
	Type = integer, Tp = real : St = false ;
	Type = integer, Tp = number : St = true ;
	Type = integer, Tp = constant : St = true ;
	Type = integer, Tp = tuple : St = false ;
	Type = integer, Tp = list : St = false ;
	Type = integer, Tp = compound : St = false ;
	Type = integer, Tp = ground : St = true ;
	Type = integer, Tp = known : St = true ;

	Type = string, Tp = integer : St = false ;
	Type = string, Tp = real : St = false ;
	Type = string, Tp = number : St = false ;
	Type = string, Tp = constant : St = true ;
	Type = string, Tp = tuple : St = false ;
	Type = string, Tp = list : St = false ;
	Type = string, Tp = compound : St = false ;
	Type = string, Tp = ground : St = true ;
	Type = string, Tp = known : St = true ;

	Type = real, Tp = integer : St = false ;
	Type = real, Tp = string : St = false ;
	Type = real, Tp = number : St = true ;
	Type = real, Tp = constant : St = true ;
	Type = real, Tp = tuple : St = false ;
	Type = real, Tp = list : St = false ;
	Type = real, Tp = compound : St = false ;
	Type = real, Tp = ground : St = true ;
	Type = real, Tp = known : St = true ;

	Type = number, Tp = integer : St = compile ;
	Type = number, Tp = string : St = false ;
	Type = number, Tp = real : St = compile ;
	Type = number, Tp = constant : St = true ;
	Type = number, Tp = tuple : St = false ;
	Type = number, Tp = list : St = false ;
	Type = number, Tp = compound : St = false ;
	Type = number, Tp = ground : St = true ;
	Type = number, Tp = known : St = true ;

	Type = nil, Tp = integer : St = false ;
	Type = nil, Tp = real : St = false ;
	Type = nil, Tp = number : St = false ;
	Type = nil, Tp = string : St = false ;
	Type = nil, Tp = constant : St = true ;
	Type = nil, Tp = tuple : St = false ;
	Type = nil, Tp = list : St = false ;
	Type = nil, Tp = compound : St = false ;
	Type = nil, Tp = ground : St = true ;
	Type = nil, Tp = known : St = true ;

	Type = constant, Tp = integer : St = compile ;
	Type = constant, Tp = string : St = compile ;
	Type = constant, Tp = real : St = compile ;
	Type = constant, Tp = number : St = compile ;
	Type = constant, Tp = tuple : St = false ;
	Type = constant, Tp = list : St = false ;
	Type = constant, Tp = compound : St = false ;
	Type = constant, Tp = ground : St = true ;
	Type = constant, Tp = known : St = true ;

	Type = tuple, Tp = integer : St = false ;
	Type = tuple, Tp = string : St = false ;
	Type = tuple, Tp = real : St = false ;
	Type = tuple, Tp = number : St = false ;
	Type = tuple, Tp = constant : St = false ;
	Type = tuple, Tp = list : St = false ;
	Type = tuple, Tp = compound : St = true ;
	Type = tuple, Tp = ground : St = compile ;
	Type = tuple, Tp = known : St = true ;

	Type = list, Tp = integer : St = false ;
	Type = list, Tp = string : St = false ;
	Type = list, Tp = real : St = false ;
	Type = list, Tp = number : St = false ;
	Type = list, Tp = constant : St = false ;
	Type = list, Tp = tuple : St = false ;
	Type = list, Tp = compound : St = true ;
	Type = list, Tp = ground : St = compile ;
	Type = list, Tp = known : St = true ;

	Type = compound, Tp = integer : St = false ;
	Type = compound, Tp = string : St = false ;
	Type = compound, Tp = real : St = false ;
	Type = compound, Tp = number : St = false ;
	Type = compound, Tp = constant : St = false ;
	Type = compound, Tp = tuple : St = compile ;
	Type = compound, Tp = list : St = compile ;
	Type = compound, Tp = ground : St = compile ;
	Type = compound, Tp = known : St = true ;

	Type = ground, Tp = integer : St = compile ;
	Type = ground, Tp = string : St = compile ;
	Type = ground, Tp = real : St = compile ;
	Type = ground, Tp = number : St = compile ;
	Type = ground, Tp = constant : St = compile ;
	Type = ground, Tp = tuple : St = compile ;
	Type = ground, Tp = list : St = compile ;
	Type = ground, Tp = compound : St = compile ;
	Type = ground, Tp = known : St = true ;

	otherwise : St = compile, Tp = _, Type = _.

get_type(Tp, Type, TpO)
:-
	Tp = unknown, Type = deref : TpO = Type ;
	Tp = unknown, Type =\= deref : TpO = ref ;
	Tp = var, Type = deref : TpO = Type ;
	Tp = var, Type =\= deref : TpO = ref ;
	Tp = ro, Type = deref : TpO = Type ;
	Tp = ro, Type =\= deref : TpO = ref ;
	Tp = not_we, Type = deref : TpO = Type ;
	Tp = not_we, Type =\= deref : TpO = ref ;
	Tp =\= unknown, Tp =\= var, Tp =\= ro, Tp =\= not_we :
		TpO = Tp, Type = _.

get_cont(Ow, BH, BHO)
:-
	Ow = {Lab,_}, Lab = label(_), BH = {B,H,_} : BHO = {B,H,goto(Lab)} ;
	Ow =\= {label(_),_} : BHO = BH.

