/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/case_psi.cp,v 1.1 1999/07/09 07:03:03 bill Exp $ */
-language(compound).
-export([case_psi/10]).
-mode(trust).

procedure case_psi(Dg, Psi, Entry, BH, Dic, Ctl, Ch, Name, SC, Done).

case_psi(Dg, Psi, Entry, BH, Dic, Ctl, Ch, Name, SC, Done)
:-
	get_list_and_ow_dg(Dg, ListDg, OwDg, OwLab, Dg'),
	case_psi1(Dg', ListDg, OwDg, OwLab, 
	                Psi, Entry, BH, Dic, Ctl, Ch, Name, SC, Done).

get_list_and_ow_dg(Dg, ListDg, OwDg, OwLab, DgOut)
:-
	Dg ? L, L = list(_) : ListDg ! L | get_list_and_ow_dg ;

	Dg ? L, L =\= list(_), L =\= ow(_) : DgOut ! L | get_list_and_ow_dg ;

	Dg = [ow(Ow)], Ow =\= {label(_),_} : 
        ListDg = [], OwDg = Ow, OwLab = [], DgOut = [] ;

	Dg = [ow(Ow)], Ow = {Lab,_}, Lab = label(_) : 
        ListDg = [], OwDg = Ow, OwLab = Lab, DgOut = [].

case_psi1(Dg, ListDg, OwDg, OwLab, Psi, Entry, BH, Dic, Ctl, Ch, Name, SC, Done)
:- 
	Entry = {Areg,Vreg,deref,_} |
	     append(ListDg, Dg, Dg'),
	     branches(Dg', OwDg, OwLab, Psi, unknown, {Areg,Vreg,deref,'*'}, 
                                      BH, Dic, Ctl, Ch, Name, SC, Done) ;

	Entry = {_,Vreg,Type,'*'}, Type = integer, 
	Vreg = a(_) : ListDg = _ |
	     get_type_sub_dgs(Type, Dg, SubDgs\[]),
	     branch_on_known_type(SubDgs, OwDg, OwLab, Psi, 
			Entry, BH, Dic, Ctl, Ch, Name, SC, Done) ;

	Entry = {Areg,Vreg,Type,'*'}, Type = integer, 
	Vreg =\= a(_), Areg = a(_),
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_reg(Reg,{Dic,Dic2}),Ch1),
	CtlH ! deref_value(Areg,Reg),
	ListDg = _ |
	     get_type_sub_dgs(Type, Dg, SubDgs\[]),
	     branch_on_known_type(SubDgs, OwDg, OwLab, Psi, 
			Entry, BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done) ;

	Entry = {Areg,Vreg,Type,'*'}, Type = integer, 
	Vreg =\= a(_), Areg = {a(_),_},
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
        write_channel(get_2_regs(Reg,Reg1,Dic,Dic2),Ch1),
	CtlH ! deref(Areg,Reg,Reg1),
	ListDg = _ |
	     get_type_sub_dgs(Type, Dg, SubDgs\[]),
	     branch_on_known_type(SubDgs, OwDg, OwLab, Psi, 
		{Reg,Reg1,Type,'*'}, BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {_,_,Type,Val}, Type = integer, Val =\= '*', 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} : 
	ListDg = _, Psi = _ |
	     get_type_sub_dg({Type,Val}, Dg, SubDg),
	     get_cont(OwLab, BH, BH'),
	     ctl#ctl1(SubDg, BH', Dic, CtlH\[label(_)|CtlM], 
	                               Ch, Name, {L,M1}, {DL,DM}),
	     ctl#ctl1(OwDg, BH, Dic, CtlM\CtlT, Ch, Name, {M1,R}, {DM,DR}) ;

	Entry = {Areg,_,Type,'*'}, Type = string, Areg = a(_),	Ctl = CtlH\CtlT:
        CtlH ! deref(Areg,Areg),
	ListDg = _ |
	     get_type_sub_dgs(Type, Dg, SubDgs\[]),
	     branch_on_known_type(SubDgs, OwDg, OwLab, Psi, 
			Entry, BH, Dic, CtlH'\CtlT, Ch, Name, SC, Done) ;

	Entry = {Areg,_,Type,'*'}, Type = string, Areg = {a(_),_},
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_reg(Vreg,{Dic,Dic1}),Ch1),
	CtlH ! deref(Areg,Vreg),
	ListDg = _ |
	     get_type_sub_dgs(Type, Dg, SubDgs\[]),
	     branch_on_known_type(SubDgs, OwDg, OwLab, Psi, 
		{Vreg,'*',Type,'*'}, BH, Dic1, CtlH'\CtlT, Ch, Name, SC, Done) ;

	Entry = {_,_,Type,Val}, Type = string, Val =\= '*',
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} : 
	ListDg = _, Psi = _ |
	     get_type_sub_dg({Type,Val}, Dg, SubDg),
	     get_cont(OwLab, BH, BH'),
	     ctl#ctl1(SubDg, BH', Dic, CtlH\[label(_)|CtlM], 
	                               Ch, Name, {L,M1}, {DL,DM}),
	     ctl#ctl1(OwDg, BH, Dic, CtlM\CtlT, Ch, Name, {M1,R}, {DM,DR}) ;

	Entry = {Areg,_,Type,Val}, Type = tuple, Areg = a(_), Val =\= '*', 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} : 
	CtlH ! deref(Areg,Areg),
	ListDg = _ |
	     get_type_sub_dg({Type,Val}, Dg, SubDg),
	     update_sub_args(Psi, Areg, Val, {Dic,Dic1}, Ch1, {L,M}),
	     get_cont(OwLab, BH, BH'),
	     ctl#ctl1(SubDg, BH', Dic1, CtlH'\[label(_)|CtlM], 
	                                Ch, Name, {M,M1}, {DL,DM}),
	     ctl#ctl1(OwDg, BH, Dic, CtlM\CtlT, Ch, Name, {M1,R}, {DM,DR}) ;

	Entry = {Areg,_,Type,Val}, Type = tuple, 
	Areg = {a(_),_}, Val =\= '*', 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} : 
	write_channel(get_reg(Reg,{Dic,Dic1}),Ch1),
	CtlH ! deref(Areg,Reg),
	write_channel(update(Psi,{Reg,'*',Type,Val},{Dic1,Dic2}),Ch1),
	ListDg = _ |
	     get_type_sub_dg({Type,Val}, Dg, SubDg),
	     update_sub_args(Psi, Reg, Val, {Dic2,Dic3}, Ch1, {L,M}),
	     get_cont(OwLab, BH, BH'),
	     ctl#ctl1(SubDg, BH', Dic3, CtlH'\[label(_)|CtlM], 
                                        Ch, Name, {M,M1}, {DL,DM}),
	     ctl#ctl1(OwDg, BH, Dic2, CtlM\CtlT, Ch, Name, {M1,R}, {DM,DR});

	Entry = {Areg,Vreg,Type,'*'}, Type = tuple, Areg = a(_), Vreg = '*',
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_reg(Reg,{Dic,Dic1}),Ch1),
	CtlH ! deref_value(Areg,Reg),
	ListDg = _ |
	     get_type_sub_dgs(Type, Dg, SubDgs\[]),
	     branch_on_known_type(SubDgs, OwDg, OwLab, Psi, 
		{Areg,Reg,Type,'*'}, BH, Dic1, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {Areg,Vreg,Type,'*'}, Type = tuple, 
	Areg = {a(_),_}, Vreg = '*',
	Ctl = CtlH\CtlT, Ch = {_,Ch1} :
        write_channel(get_2_regs(Reg1,Reg2,Dic,Dic2),Ch1),
	CtlH ! deref(Areg,Reg1,Reg2),
	ListDg = _ |
	     get_type_sub_dgs(Type, Dg, SubDgs\[]),
	     branch_on_known_type(SubDgs, OwDg, OwLab, Psi, 
		{Reg1,Reg2,Type,'*'}, BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done);
		
	Entry = {_,_,Type,_}, Type = nil,
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} : Psi = _, ListDg = _ |
	     get_type_sub_dg(Type, Dg, SubDg),
	     get_cont(OwLab, BH, BH'),
	     ctl#ctl1(SubDg, BH', Dic, CtlH\[label(_)|CtlM], 
	                               Ch, Name, {L,M}, {DL,DM}),
	     ctl#ctl1(OwDg, BH, Dic, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR}) ;

	Entry = {Areg,_,Type,'*'}, Type = real, Areg = a(_), Ctl = CtlH\CtlT : 
        CtlH ! deref(Areg,Areg),
	ListDg = _ |
	     get_type_sub_dgs(Type, Dg, SubDgs\[]),
	     branch_on_known_type(SubDgs, OwDg, OwLab, Psi, 
			Entry, BH, Dic, CtlH'\CtlT, Ch, Name, SC, Done) ;

	Entry = {Areg,_,Type,'*'}, Type = real, Areg = {a(_),_},
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_reg(Vreg,{Dic,Dic1}),Ch1),
	CtlH ! deref(Areg,Vreg),
	ListDg = _ |
	     get_type_sub_dgs(Type, Dg, SubDgs\[]),
	     branch_on_known_type(SubDgs, OwDg, OwLab, Psi, 
		{Vreg,'*',Type,'*'}, BH, Dic1, CtlH'\CtlT, Ch, Name, SC, Done) ;

	Entry = {_,_,Type,Val}, Type = real, Val =\= '*',
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} : 
	ListDg = _, Psi = _ |
	     get_type_sub_dg({Type,Val}, Dg, SubDg),
	     get_cont(OwLab, BH, BH'),
	     ctl#ctl1(SubDg, BH', Dic, CtlH\[label(_)|CtlM], 
	                               Ch, Name, {L,M1}, {DL,DM}),
	     ctl#ctl1(OwDg, BH, Dic, CtlM\CtlT, Ch, Name, {M1,R}, {DM,DR}) ;

	Entry = {Areg,_,Type,_}, Type = var, Areg = a(_), Ctl = CtlH\CtlT :
	CtlH ! suspend_on(Areg), Psi = _, ListDg = _, OwLab = _, Dg = _ |
	     ctl#ctl1(OwDg, BH, Dic, CtlH'\CtlT, Ch, Name, SC, Done) ;

	Entry = {Areg,_,Type,_}, Type = var, Areg = {a(_),_}, 
	Ctl = CtlH\CtlT, Ch = {_,Ch1} :
	write_channel(get_reg(Vreg,{Dic,Dic1}),Ch1),
	write_channel(update(Psi,{Vreg,'*',Type,'*'},{Dic1,Dic2}),Ch1),
	CtlH ! deref(Areg,Vreg),
	CtlH' ! suspend_on(Vreg), ListDg = _, OwLab = _, Dg = _ |
	     ctl#ctl1(OwDg, BH, Dic2, CtlH''\CtlT, Ch, Name, SC, Done) ;

	Entry = {Areg,_,Type,_}, Type = ref(var), Areg = a(_), 
	Ctl = CtlH\CtlT, Ch = {_,Ch1} :
	write_channel(update_type(Psi,var,{Dic,Dic2}),Ch1),
	CtlH ! deref(Areg,Areg),
	CtlH' ! suspend_on(Areg), ListDg = _, OwLab = _, Dg = _ |
	     ctl#ctl1(OwDg, BH, Dic2, CtlH''\CtlT, Ch, Name, SC, Done) ;

	Entry = {Areg,_,Type,_}, Type = ref(var), Areg = {a(_),_}, 
	Ctl = CtlH\CtlT, Ch = {_,Ch1} :
	write_channel(get_reg(Vreg,{Dic,Dic1}),Ch1),
	write_channel(update(Psi,{Vreg,'*',Type,'*'},{Dic1,Dic2}),Ch1),
	CtlH ! deref(Areg,Vreg),
	CtlH' ! suspend_on(Vreg), Psi = _, ListDg = _, OwLab = _, Dg = _ |
	     ctl#ctl1(OwDg, BH, Dic2, CtlH''\CtlT, Ch, Name, SC, Done) ;

	Entry = {Areg,Vreg,Type,_}, Type = number, Areg = a(_), Vreg = '*',
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_reg(Reg1,{Dic,Dic1}),Ch1),
	CtlH ! deref_value(Areg,Reg1),
	ListDg = _ |
	     get_type_sub_dgs(integer, Dg, SubDgs\SubDgM),
	     get_type_sub_dgs(real, Dg, SubDgM\[]),
	     branches(SubDgs, OwDg, OwLab, Psi, known, {Areg,Reg1,Type,'*'}, 
	                   BH, Dic1, CtlH'\CtlT, Ch, Name, SC, Done) ;

	Entry = {Areg,Vreg,Type,_}, Type = number, 
	Areg = {a(_),_}, Vreg = '*',
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
        write_channel(get_2_regs(Reg1,Reg2,Dic,Dic2),Ch1),
	CtlH ! deref(Areg,Reg1,Reg2),
	ListDg = _ |
	     get_type_sub_dgs(integer, Dg, SubDgs\SubDgM),
	     get_type_sub_dgs(real, Dg, SubDgM\[]),
	     branches(SubDgs, OwDg, OwLab, Psi, known, {Reg1,Reg2,Type,'*'}, 
	                   BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done) ;

	Entry = {_,Vreg,Type,_}, Type = number, Vreg = a(_) : ListDg = _ |
	     get_type_sub_dgs(integer, Dg, SubDgs\SubDgM),
	     get_type_sub_dgs(real, Dg, SubDgM\[]),
	     branches(SubDgs, OwDg, OwLab, Psi, known, Entry, BH, Dic, 
					Ctl, Ch, Name, SC, Done) ;

	Entry = {Areg,Vreg,Type,_}, Type = compound, 
	Areg = {a(_),_}, Vreg = '*',
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
        write_channel(get_2_regs(Reg1,Reg2,Dic,Dic2),Ch1),
	CtlH ! deref(Areg,Reg1,Reg2) |
	     get_type_sub_dgs(tuple, Dg, SubDgM\[]),
	     append(ListDg, SubDgM, SubDgs),
	     branches(SubDgs, OwDg, OwLab, Psi, known, {Reg1,Reg2,Type,'*'}, 
	                       BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {_,Vreg,Type,'*'}, Type = compound, Vreg = a(_) |
	     get_type_sub_dgs(tuple, Dg, SubDgM\[]),
	     append(ListDg, SubDgM, SubDgs),
	     branches(SubDgs, OwDg, OwLab, Psi, known, 
			Entry, BH, Dic, Ctl, Ch, Name, SC, Done);

	Entry = {_,Vreg,Type,'*'}, Type = compound, Vreg = a(_) |
	     get_type_sub_dgs(tuple, Dg, SubDgM\[]),
	     append(ListDg, SubDgM, SubDgs),
	     branches(SubDgs, OwDg, OwLab, Psi, known, 
			Entry, BH, Dic, Ctl, Ch, Name, SC, Done);

	Entry = {Areg,Vreg,Type,_}, Type = constant, Areg = a(_), Vreg = '*',
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_reg(Reg1,{Dic,Dic1}),Ch1),
	CtlH ! deref_value(Areg,Reg1),
	ListDg = _ |
	     get_type_sub_dgs(integer, Dg, SubDgs\SubDg1),
	     get_type_sub_dgs(real, Dg, SubDg1\SubDg2),
	     get_type_sub_dgs(string, Dg, SubDg2\SubDg3),
	     get_type_sub_dgs(nil, Dg, SubDg3\[]),
	     branches(SubDgs, OwDg, OwLab, Psi, known, 
		{Areg,Reg1,Type,'*'}, BH, Dic1, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {Areg,Vreg,Type,_}, Type = constant, 
	Areg = {a(_),_}, Vreg = '*',
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
        write_channel(get_2_regs(Reg1,Reg2,Dic,Dic2),Ch1),
	CtlH ! deref(Areg,Reg1,Reg2),
	ListDg = _ |
	     get_type_sub_dgs(integer, Dg, SubDgs\SubDg1),
	     get_type_sub_dgs(real, Dg, SubDg1\SubDg2),
	     get_type_sub_dgs(string, Dg, SubDg2\SubDg3),
	     get_type_sub_dgs(nil, Dg, SubDg3\[]),
	     branches(SubDgs, OwDg, OwLab, Psi, known, 
		{Reg1,Reg2,type,'*'}, BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {_,Vreg,Type,'*'}, Type = constant, Vreg = a(_) :
	ListDg = _ |
	     get_type_sub_dgs(integer, Dg, SubDgs\SubDg1),
	     get_type_sub_dgs(real, Dg, SubDg1\SubDg2),
	     get_type_sub_dgs(string, Dg, SubDg2\SubDg3),
	     get_type_sub_dgs(nil, Dg, SubDg3\[]),
	     branches(SubDgs, OwDg, OwLab, Psi, known, 
			Entry, BH, Dic, Ctl, Ch, Name, SC, Done);

	Entry = {_,Vreg,Type,'*'}, Type = unknown, Vreg = a(_) |
	     append(ListDg, Dg, SubDgs),
	     branches(SubDgs, OwDg, OwLab, Psi, unknown, 
	                       Entry, BH, Dic, Ctl, Ch, Name, SC, Done);

	Entry = {_,Vreg,Type,'*'}, Type = not_we, Vreg = a(_) |
	     append(ListDg, Dg, SubDgs),
	     branches(SubDgs, OwDg, OwLab, Psi, unknown, 
	                       Entry, BH, Dic, Ctl, Ch, Name, SC, Done);

	Entry = {_,Vreg,Type,'*'}, Type = known, Vreg = a(_) |
	     append(ListDg, Dg, SubDgs),
	     branches(SubDgs, OwDg, OwLab, Psi, known, 
	                       Entry, BH, Dic, Ctl, Ch, Name, SC, Done);

	Entry = {_,Vreg,Type,'*'}, Type = ground, Vreg = a(_) |
	     append(ListDg, Dg, SubDgs),
	     branches(SubDgs, OwDg, OwLab, Psi, known, 
			Entry, BH, Dic, Ctl, Ch, Name, SC, Done);

	Entry = {Areg,_,car(Type),'*'}, Type = integer,
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_2_regs(Vreg,Reg,Dic,Dic2),Ch1),
	CtlH ! deref_car(Areg,Reg,Vreg),
	ListDg = _ |
	     get_type_sub_dgs(Type, Dg, SubDgs\[]),
	     branch_on_known_type(SubDgs, OwDg, OwLab, Psi, 
		{Reg,Vreg,Type,'*'}, BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done) ;

	Entry = {_,_,car(Type),Val}, Type = integer, Val =\= '*', 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} : 
	ListDg = _, Psi = _ |
	     get_type_sub_dg({Type,Val}, Dg, SubDg),
	     get_cont(OwLab, BH, BH'),
	     ctl#ctl1(SubDg, BH', Dic, CtlH\[label(_)|CtlM], 
	                               Ch, Name, {L,M1}, {DL,DM}),
	     ctl#ctl1(OwDg, BH, Dic, CtlM\CtlT, Ch, Name, {M1,R}, {DM,DR}) ;

	Entry = {Areg,_,car(Type),'*'}, Type = string, 
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_reg(Vreg,{Dic,Dic1}),Ch1),
	CtlH ! deref_car(Areg,Vreg),
	ListDg = _ |
	     get_type_sub_dgs(Type, Dg, SubDgs\[]),
	     branch_on_known_type(SubDgs, OwDg, OwLab, Psi, 
		{Vreg,'*',Type,'*'}, BH, Dic1, CtlH'\CtlT, Ch, Name, SC, Done) ;

	Entry = {_,_,car(Type),Val}, Type = string, Val =\= '*',
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} : 
	ListDg = _, Psi = _ |
	     get_type_sub_dg({Type,Val}, Dg, SubDg),
	     get_cont(OwLab, BH, BH'),
	     ctl#ctl1(SubDg, BH', Dic, CtlH\[label(_)|CtlM], 
	                               Ch, Name, {L,M1}, {DL,DM}),
	     ctl#ctl1(OwDg, BH, Dic, CtlM\CtlT, Ch, Name, {M1,R}, {DM,DR}) ;

	Entry = {_,_,car(Type),_}, Type = nil,
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} : Psi = _, ListDg = _ |
	     get_type_sub_dg(Type, Dg, SubDg),
	     get_cont(OwLab, BH, BH'),
	     ctl#ctl1(SubDg, BH', Dic, CtlH\[label(_)|CtlM], 
	                               Ch, Name, {L,M}, {DL,DM}),
	     ctl#ctl1(OwDg, BH, Dic, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR}) ;

	Entry = {Areg,_,car(Type),_}, Type = real,
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_reg(Vreg,{Dic,Dic1}),Ch1),
	CtlH ! deref_car(Areg,Vreg),
	ListDg = _ |
	     get_type_sub_dgs(Type, Dg, SubDgs\[]),
	     branch_on_known_type(SubDgs, OwDg, OwLab, Psi, 
		{Vreg,'*',Type,'*'}, BH, Dic1, CtlH'\CtlT, Ch, Name, SC, Done) ;

	Entry = {Areg,_,car(Type),_}, Type = var,
	Ctl = CtlH\CtlT, Ch = {_,Ch1} :
	write_channel(get_reg(Vreg,{Dic,Dic1}),Ch1),
	write_channel(update(Psi,{Vreg,'*',Type,'*'},{Dic1,Dic2}),Ch1),
	CtlH ! deref_car(Areg,Vreg),
	CtlH' ! suspend_on(Vreg), Psi = _, ListDg = _, OwLab = _, Dg = _ |
	     ctl#ctl1(OwDg, BH, Dic2, CtlH''\CtlT, Ch, Name, SC, Done);

	Entry = {Areg,_,car(Type),_}, Type = number, 
	Ctl = CtlH\CtlT, Ch = {_,Ch1} :
        write_channel(get_2_regs(Reg1,Reg2,Dic,Dic2),Ch1),
	CtlH ! deref_car(Areg,Reg1,Reg2),
	ListDg = _ |
	     get_type_sub_dgs(integer, Dg, SubDgs\SubDgM),
	     get_type_sub_dgs(real, Dg, SubDgM\[]),
	     branches(SubDgs, OwDg, OwLab, Psi, known, {Reg1,Reg2,Type,'*'}, 
	            BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done) ;

	Entry = {Areg,_,car(Type),_}, Type = compound, 
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
        write_channel(get_2_regs(Reg1,Reg2,Dic,Dic2),Ch1),
	CtlH ! deref_car(Areg,Reg1,Reg2),
	write_channel(update(Psi,{Reg1,Reg2,Type,'*'},{Dic2,Dic2}),Ch1) |
	     get_type_sub_dgs(tuple, Dg, SubDgM\[]),
	     append(ListDg, SubDgM, SubDgs),
	     branches(SubDgs, OwDg, OwLab, Psi, known, {Reg1,Reg2,Type,'*'}, 
	             BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {Areg,_,car(Type),_}, Type = constant, 
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
        write_channel(get_2_regs(Reg1,Reg2,Dic,Dic2),Ch1),
	CtlH ! deref_car(Areg,Reg1,Reg2),
	ListDg = _ |
	     get_type_sub_dgs(integer, Dg, SubDgs\SubDg1),
	     get_type_sub_dgs(real, Dg, SubDg1\SubDg2),
	     get_type_sub_dgs(string, Dg, SubDg2\SubDg3),
	     get_type_sub_dgs(nil, Dg, SubDg3\[]),
	     branches(SubDgs, OwDg, OwLab, Psi, known, 
		{Reg1,Reg2,Type,'*'}, BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {Areg,_,car(Type),_}, Type = unknown,
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
        write_channel(get_2_regs(Reg1,Reg2,Dic,Dic2),Ch1),
	CtlH ! deref_car(Areg,Reg1,Reg2) |
             append(ListDg, Dg, SubDgs), 
	     branches(SubDgs, OwDg, OwLab, Psi, unknown, 
		{Reg1,Reg2,Type,'*'}, BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {Areg,_,car(Type),_}, Type = not_we,
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
        write_channel(get_2_regs(Reg1,Reg2,Dic,Dic2),Ch1),
	CtlH ! deref_car(Areg,Reg1,Reg2) |
             append(ListDg, Dg, SubDgs), 
	     branches(SubDgs, OwDg, OwLab, Psi, unknown, 
		{Reg1,Reg2,Type,'*'}, BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {Areg,_,car(Type),_}, Type = known, 
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
        write_channel(get_2_regs(Reg1,Reg2,Dic,Dic2),Ch1),
	CtlH ! deref_car(Areg,Reg1,Reg2) |
             append(ListDg, Dg, SubDgs), 
	     branches(SubDgs, OwDg, OwLab, Psi, known, 
		{Reg1,Reg2,Type,'*'}, BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {Areg,_,car(Type),_}, Type = ground, 
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
        write_channel(get_2_regs(Reg1,Reg2,Dic,Dic2),Ch1),
	CtlH ! deref_car(Areg,Reg1,Reg2) |
             append(ListDg, Dg, SubDgs), 
	     branches(SubDgs, OwDg, OwLab, Psi, known, 
		{Reg1,Reg2,Type,'*'}, BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {Areg,_,car(Type),Val}, Type = tuple, Val =\= '*', 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} : 
	write_channel(get_reg(Reg,{Dic,Dic1}),Ch1),
	CtlH ! deref_car(Areg,Reg),
	write_channel(update(Psi,{Reg,'*',Type,Val},{Dic1,Dic2}),Ch1),
	ListDg = _ |
	     get_type_sub_dg({Type,Val}, Dg, SubDg),
	     update_sub_args(Psi, Reg, Val, {Dic2,Dic3}, Ch1, {L,M}),
	     get_cont(OwLab, BH, BH'),
	     ctl#ctl1(SubDg, BH', Dic3, CtlH'\[label(_)|CtlM], 
	                                Ch, Name, {M,M1}, {DL,DM}),
	     ctl#ctl1(OwDg, BH, Dic2, CtlM\CtlT, Ch, Name, {M1,R}, {DM,DR});

	Entry = {Areg,_,car(Type),'*'}, Type = tuple, 
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
        write_channel(get_2_regs(Reg1,Reg2,Dic,Dic2),Ch1),
	CtlH ! deref_car(Areg,Reg1,Reg2),
	ListDg = _ |
	     get_type_sub_dgs(Type, Dg, SubDgs\[]),
	     branch_on_known_type(SubDgs, OwDg, OwLab, Psi, 
		{Reg1,Reg2,Type,'*'}, BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done);
		
	otherwise, ListDg = [list(SubDg)] |
        case_psi_list(Dg, SubDg, OwDg, OwLab, Psi, Entry, BH, Dic, Ctl, Ch, 
	                                                        Name, SC, Done);

	otherwise, ListDg = [] |
        case_psi_all(Dg, OwDg, OwLab, Psi, Entry, BH, Dic, Ctl, Ch, 
	                                                        Name, SC, Done).

case_psi_all(Dg, OwDg, OwLab, Psi, Entry, BH, Dic, Ctl, Ch, Name, SC, Done)
:-
	Entry = {Areg,_,ref,_}, 
	Ctl = CtlH\CtlT, Ch = {_,Ch1} :
	CtlH ! deref_value(Areg,Vreg),
	write_channel(get_reg(Vreg,{Dic,Dic1}),Ch1) |
             branches(Dg, OwDg, OwLab, Psi, unknown, {Areg,Vreg,deref,'*'}, 
			BH, Dic1, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {Areg,_,cdr,_}, 
	Ctl = CtlH\CtlT, Ch = {_,Ch1} :
	write_channel(get_2_regs(Vreg,Reg,Dic,Dic2),Ch1),
	CtlH ! deref(Areg,Reg,Vreg) |
             branches(Dg, OwDg, OwLab, Psi, unknown, {Reg,Vreg,deref,'*'}, 
			BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {Base,_,sub_arg,_},
	Ctl = CtlH\CtlT, Ch = {_,Ch1} :
	write_channel(get_2_regs(Areg,Vreg,Dic,Dic2),Ch1),
	CtlH ! deref(Base,Areg,Vreg) | 
             branches(Dg, OwDg, OwLab, Psi, unknown, {Areg,Vreg,deref,'*'}, 
			BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {_,_,list,_} : 
        Psi = _, OwLab = _, Dg = _ |
	     ctl#ctl1(OwDg, BH, Dic, Ctl, Ch, Name, SC, Done) ;

	Entry = {Areg,Vreg,Type,_}, Type = unknown, Areg = a(_), Vreg = '*',
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_reg(Reg1,{Dic,Dic1}),Ch1),
	CtlH ! deref_value(Areg,Reg1) |
	     branches(Dg, OwDg, OwLab, Psi, unknown,
		{Areg,Reg1,Type,'*'}, BH, Dic1, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {Areg,Vreg,Type,_}, Type = not_we, Areg = a(_), Vreg = '*',
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_reg(Reg1,{Dic,Dic1}),Ch1),
	CtlH ! deref_value(Areg,Reg1) |
	     branches(Dg, OwDg, OwLab, Psi, unknown,
		{Areg,Reg1,Type,'*'}, BH, Dic1, CtlH'\CtlT, Ch, Name, SC, Done);
	
	Entry = {Areg,Vreg,Type,_}, Type = known, Areg = a(_), Vreg = '*',
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_reg(Reg1,{Dic,Dic1}),Ch1),
	CtlH ! deref_value(Areg,Reg1) |
	     branches(Dg, OwDg, OwLab, Psi, known, 
		{Areg,Reg1,Type,'*'}, BH, Dic1, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {Areg,Vreg,Type,_}, Type = ground, Areg = a(_), Vreg = '*',
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_reg(Reg1,{Dic,Dic1}),Ch1),
	CtlH ! deref_value(Areg,Reg1) |
	     branches(Dg, OwDg, OwLab, Psi, known, 
		{Areg,Reg1,Type,'*'}, BH, Dic1, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {Areg,Vreg,Type,_}, Type = unknown, Areg = {a(_),_}, Vreg = '*',
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_2_regs(Reg1,Reg2,Dic,Dic2),Ch1),
	CtlH ! deref(Areg,Reg1,Reg2) |
	     branches(Dg, OwDg, OwLab, Psi, unknown,
		{Reg1,Reg2,Type,'*'}, BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {Areg,Vreg,Type,_}, Type = not_we, Areg = {a(_),_}, Vreg = '*',
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_2_regs(Reg1,Reg2,Dic,Dic2),Ch1),
	CtlH ! deref(Areg,Reg1,Reg2) |
	     branches(Dg, OwDg, OwLab, Psi, unknown,
		{Reg1,Reg2,Type,'*'}, BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {Areg,Vreg,Type,_}, Type = known,
	Areg = {a(_),_}, Vreg = '*',
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_2_regs(Reg1,Reg2,Dic,Dic2),Ch1),
	CtlH ! deref(Areg,Reg1,Reg2) |
	     branches(Dg, OwDg, OwLab, Psi, known, 
		{Reg1,Reg2,Type,'*'}, BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {Areg,Vreg,Type,_}, Type = ground, Areg = {a(_),_}, Vreg = '*',
	Ctl = CtlH\CtlT, Ch = {_,Ch1} : 
	write_channel(get_2_regs(Reg1,Reg2,Dic,Dic2),Ch1),
	CtlH ! deref(Areg,Reg1,Reg2) |
	     branches(Dg, OwDg, OwLab, Psi, known, 
		{Reg1,Reg2,Type,'*'}, BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done);

	Entry = {Areg,_,car,_},	Ctl = CtlH\CtlT, Ch = {_,Ch1} :
	write_channel(get_2_regs(Reg1,Reg2,Dic,Dic2),Ch1),
	CtlH ! deref_car(Areg,Reg1,Reg2) |
	     branches(Dg, OwDg, OwLab, Psi, unknown, {Reg1,Reg2,deref,'*'}, 
			BH, Dic2, CtlH'\CtlT, Ch, Name, SC, Done) ;

	Entry = {_,_,car(list),_} : Psi = _, OwLab = _, Dg = _ |
	     ctl#ctl1(OwDg, BH, Dic, Ctl, Ch, Name, SC, Done).

case_psi_list(Dg, SubDg, OwDg, OwLab, Psi, Entry, BH, Dic, Ctl, Ch, 
	                                                        Name, SC, Done)
:-
	Entry = {Areg,_,ref,_}, BH = {Br,Ha,Go},
	Ctl = CtlH\CtlT, Ch = {_,Ch1}, SC = {L,R}, Done = {DL,DR} :
	write_channel(get_reg(Vreg,{Dic,Dic1}),Ch1),
	CtlH ! deref_list(Areg,Vreg,label(L1)),
	write_channel(update(Psi,{{Areg,-1},'*',list,'*'},{Dic1,Dic2}),Ch1) |
             phase2_dic#add_child(Psi,1,{Vreg,'*',ref,'*'},{Dic2,Dic3}),
             phase2_dic#add_child(Psi,2,{Areg,'*',cdr,'*'},{Dic3,Dic4}),
	     replace_owlab(Areg, OwLab, Go, Go', CtlH''\[label(L1)|CtlM], 
				{L,M1}, {DL,DL1}),
	     ctl#ctl1(SubDg, {Br,Ha,Go'}, Dic4, CtlH'\CtlH'', 
				Ch, Name, {M1,M}, {DL1,DM}),
             branches(Dg, OwDg, OwLab, Psi, unknown, {Areg,Vreg,deref,'*'}, 
				BH, Dic, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR});

	Entry = {Base,_,cdr,_}, BH = {Br,Ha,Go},
	Ctl = CtlH\CtlT, Ch = {_,Ch1}, SC = {L,R}, Done = {DL,DR} :
        write_channel(get_2_regs(Areg,Vreg,Dic,Dic2),Ch1),
	CtlH ! deref_list(Base,Areg,Vreg,label(L1)), 
	write_channel(update(Psi,{{Areg,-1},'*',list,'*'},{Dic2,Dic3}),Ch1) |
             phase2_dic#add_child(Psi,1,{Vreg,'*',ref,'*'},{Dic3,Dic4}),
             phase2_dic#add_child(Psi,2,{Areg,'*',cdr,'*'},{Dic4,Dic5}),
	     replace_owlab(Areg, OwLab, Go, Go', CtlH''\[label(L1)|CtlM], 
				{L,M1}, {DL,DL1}),
	     ctl#ctl1(SubDg, {Br,Ha,Go'}, Dic5, CtlH'\CtlH'', 
				Ch, Name, {M1,M}, {DL1,DM}),
             branches(Dg, OwDg, OwLab, Psi, unknown, {Areg,Vreg,deref,'*'}, 
				BH, Dic2, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR});

	Entry = {Base,_,sub_arg,_}, BH = {Br,Ha,Go},
	Ctl = CtlH\CtlT, Ch = {_,Ch1}, SC = {L,R}, Done = {DL,DR} :
	CtlH ! deref_list(Base,Areg,Vreg,label(L1)), 
        write_channel(get_2_regs(Areg,Vreg,Dic,Dic2),Ch1),
	write_channel(update(Psi,{{Areg,-1},'*',list,'*'},{Dic2,Dic3}),Ch1) |
             phase2_dic#add_child(Psi,1,{Vreg,'*',ref,'*'},{Dic3,Dic4}),
             phase2_dic#add_child(Psi,2,{Areg,'*',cdr,'*'},{Dic4,Dic5}),
	     replace_owlab(Areg, OwLab, Go, Go', CtlH''\[label(L1)|CtlM], 
				{L,M1}, {DL,DL1}),
	     ctl#ctl1(SubDg, {Br,Ha,Go'}, Dic5, CtlH'\CtlH'', 
				Ch, Name, {M1,M}, {DL1,DM}),
             branches(Dg, OwDg, OwLab, Psi, unknown, {Areg,Vreg,deref,'*'}, 
				BH, Dic2, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR});

	Entry = {Areg,_,Type,_}, Type = list, 	Areg = a(_),
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
	CtlH ! deref(Areg,Areg), Dg = _ |
             phase2_dic#look_child(Psi,1,Dic,CarEnt,CarKey),
             phase2_dic#look_child(Psi,2,Dic,CdrEnt,CdrKey),
	     car(CarKey, CarEnt, Areg, {Dic,Dic3}, Ch1, {L,M1}),
	     update_sub_arg(CdrKey,CdrEnt, Areg(1), {Dic3,Dic4}, Ch1,{M1,M2}),
	     get_cont(OwLab, BH, BH'),
	     ctl#ctl1(SubDg, BH', Dic4, CtlH'\[label(_)|CtlM], 
                                        Ch, Name, {M2,M}, {DL,DM}),
	     ctl#ctl1(OwDg, BH, Dic, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR}) ;

	Entry = {Areg,_,Type,_}, Type = list, 	Areg = {a(_),_}, 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
	write_channel(get_reg(Reg,{Dic,Dic1}),Ch1),
	write_channel(update(Psi,{Reg,'*',Type,'*'},{Dic1,Dic2}),Ch1),
	CtlH ! deref(Areg,Reg),Dg = _ |
             phase2_dic#look_child(Psi,1,Dic,CarEnt,CarKey),
             phase2_dic#look_child(Psi,2,Dic,CdrEnt,CdrKey),
	     car(CarKey, CarEnt, Reg, {Dic2,Dic3}, Ch1, {L,M1}),
	     update_sub_arg(CdrKey,CdrEnt, Reg(1), {Dic3,Dic4}, Ch1,{M1,M2}),
	     get_cont(OwLab, BH, BH'),
	     ctl#ctl1(SubDg, BH', Dic4, CtlH'\[label(_)|CtlM], 
                                        Ch, Name, {M2,M}, {DL,DM}),
	     ctl#ctl1(OwDg, BH, Dic2, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR}) ;

	Entry = {Areg,_,Type,_}, Type = unknown, BH = {Br,Ha,Go},
	Ctl = CtlH\CtlT, Ch = {_,Ch1}, SC = {L,R}, Done = {DL,DR} :
	write_channel(get_reg(Vreg,{Dic,Dic1}),Ch1),
	CtlH ! deref_list(Areg,Vreg,label(L1)),
	write_channel(update(Psi,{{Areg,-1},'*',list,'*'},{Dic1,Dic2}),Ch1) |
             phase2_dic#add_child(Psi,1,{Vreg,'*',ref,'*'},{Dic2,Dic3}),
             phase2_dic#add_child(Psi,2,{Areg,'*',cdr,'*'},{Dic3,Dic4}),
	     replace_owlab(Areg, OwLab, Go, Go', CtlH''\[label(L1)|CtlM], 
				{L,M1}, {DL,DL1}),
	     ctl#ctl1(SubDg, {Br,Ha,Go'}, Dic4, CtlH'\CtlH'', 
				Ch, Name, {M1,M}, {DL1,DM}),
             branches(Dg, OwDg, OwLab, Psi, unknown, {Areg,Vreg,deref,'*'}, 
				BH, Dic, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR});

	Entry = {Areg,_,Type,_}, Type = not_we, BH = {Br,Ha,Go},
	Ctl = CtlH\CtlT, Ch = {_,Ch1}, SC = {L,R}, Done = {DL,DR} :
	write_channel(get_reg(Vreg,{Dic,Dic1}),Ch1),
	CtlH ! deref_list(Areg,Vreg,label(L1)),
	write_channel(update(Psi,{{Areg,-1},'*',list,'*'},{Dic1,Dic2}),Ch1) |
             phase2_dic#add_child(Psi,1,{Vreg,'*',ref,'*'},{Dic2,Dic3}),
             phase2_dic#add_child(Psi,2,{Areg,'*',cdr,'*'},{Dic3,Dic4}),
	     replace_owlab(Areg, OwLab, Go, Go', CtlH''\[label(L1)|CtlM], 
				{L,M1}, {DL,DL1}),
	     ctl#ctl1(SubDg, {Br,Ha,Go'}, Dic4, CtlH'\CtlH'', 
				Ch, Name, {M1,M}, {DL1,DM}),
             branches(Dg, OwDg, OwLab, Psi, unknown, {Areg,Vreg,deref,'*'}, 
				BH, Dic, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR});

	Entry = {Areg,_,Type,_}, Type = known, BH = {Br,Ha,Go},
	Ctl = CtlH\CtlT, Ch = {_,Ch1}, SC = {L,R}, Done = {DL,DR} :
	write_channel(get_reg(Vreg,{Dic,Dic1}),Ch1),
	CtlH ! deref_list(Areg,Vreg,label(L1)),
	write_channel(update(Psi,{{Areg,-1},'*',list,'*'},{Dic1,Dic2}),Ch1) |
             phase2_dic#add_child(Psi,1,{Vreg,'*',ref,'*'},{Dic2,Dic3}),
             phase2_dic#add_child(Psi,2,{Areg,'*',cdr,'*'},{Dic3,Dic4}),
	     replace_owlab(Areg, OwLab, Go, Go', CtlH''\[label(L1)|CtlM], 
				{L,M1}, {DL,DL1}),
	     ctl#ctl1(SubDg, {Br,Ha,Go'}, Dic4, CtlH'\CtlH'', 
				Ch, Name, {M1,M}, {DL1,DM}),
             branches(Dg, OwDg, OwLab, Psi, unknown, {Areg,Vreg,deref,'*'}, 
				BH, Dic, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR});

	Entry = {Areg,_,Type,_}, Type = ground, BH = {Br,Ha,Go},
	Ctl = CtlH\CtlT, Ch = {_,Ch1}, SC = {L,R}, Done = {DL,DR} :
	write_channel(get_reg(Vreg,{Dic,Dic1}),Ch1),
	CtlH ! deref_list(Areg,Vreg,label(L1)),
	write_channel(update(Psi,{{Areg,-1},'*',list,'*'},{Dic1,Dic2}),Ch1) |
             phase2_dic#add_child(Psi,1,{Vreg,'*',ground,'*'},{Dic2,Dic3}),
             phase2_dic#add_child(Psi,2,{Areg,'*',cdr,'*'},{Dic3,Dic4}),
	     replace_owlab(Areg, OwLab, Go, Go', CtlH''\[label(L1)|CtlM], 
				{L,M1}, {DL,DL1}),
	     ctl#ctl1(SubDg, {Br,Ha,Go'}, Dic4, CtlH'\CtlH'', 
				Ch, Name, {M1,M}, {DL1,DM}),
             branches(Dg, OwDg, OwLab, Psi, unknown, {Areg,Vreg,ground,'*'}, 
				BH, Dic, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR});

	Entry = {Areg,_,car,_}, BH = {Br,Ha,Go},
	Ctl = CtlH\CtlT, Ch = {_,Ch1}, SC = {L,R}, Done = {DL,DR} :
	CtlH ! deref_car_list(Areg,Reg1,Reg2,label(L1)),
        write_channel(get_2_regs(Reg1,Reg2,Dic,Dic2),Ch1),
	write_channel(update(Psi,{{Reg1,-1},'*',list,'*'},{Dic2,Dic3}),Ch1) |
             phase2_dic#add_child(Psi,1,{Reg2,'*',ref,'*'},{Dic3,Dic4}),
             phase2_dic#add_child(Psi,2,{Reg1,'*',cdr,'*'},{Dic4,Dic5}),
	     replace_owlab(Areg, OwLab, Go, Go', CtlH''\[label(L1)|CtlM], 
				{L,M1}, {DL,DL1}),
	     ctl#ctl1(SubDg, {Br,Ha,Go'}, Dic5, CtlH'\CtlH'', 
				Ch, Name, {M1,M}, {DL1,DM}),
             branches(Dg, OwDg, OwLab, Psi, unknown, {Reg1,Reg2,deref,'*'}, 
				BH, Dic2, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR});

	Entry = {Areg,_,car(Type),_}, Type = list, 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
	write_channel(get_reg(Reg,{Dic,Dic1}),Ch1),
	CtlH ! deref_car(Areg,Reg),
	write_channel(update(Psi,{Reg,'*',Type,'*'},{Dic1,Dic2}),Ch1),
	Dg = _ |
             phase2_dic#look_child(Psi,1,Dic,CarEnt,CarKey),
             phase2_dic#look_child(Psi,2,Dic,CdrEnt,CdrKey),
	     car(CarKey, CarEnt, Reg, {Dic2,Dic3}, Ch1, {L,M1}),
	     update_sub_arg(CdrKey,CdrEnt, Reg(1), {Dic3,Dic4}, Ch1,{M1,M2}),
	     get_cont(OwLab, BH, BH'),
	     ctl#ctl1(SubDg, BH', Dic4, CtlH'\[label(_)|CtlM], 
	                                Ch, Name, {M2,M}, {DL,DM}),
	     ctl#ctl1(OwDg, BH, Dic1, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR}).

replace_owlab(Areg, OwLab, Go, GoO, Ctl, SC, Done)
:-
	OwLab = [], Go = [], Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	CtlH = CtlT, L = R, DL = DR, GoO = Go, Areg = _ ;

	OwLab = [], Go =\= [], Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	CtlH = [label(L1),
		decrement_pointer(Areg),
		Go|CtlT], 
	GoO = goto(label(L1)), L = R, DL = DR, Go = _ ;

	OwLab =\= [], Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	CtlH = [label(L1),
		decrement_pointer(Areg),
		goto(OwLab)|CtlT], 
	GoO = goto(label(L1)), L = R, DL = DR, Go = _.

get_cont(OwLab, BH, BHO)
:-
	OwLab = [] : BHO = BH ;
	OwLab =\= [], BH = {B,H,_} : BHO = {B,H,goto(OwLab)}.

car(Psi, Entry, Reg, Dic, Ch, SC)
:-
	Entry = {_,_,new,_}, SC = {L,R} :
	write_channel(add(Psi,{Reg,'*',car,'*'},Dic),Ch), L = R ;

	Entry = {_,_,car,_}, SC = {L,R} :
	write_channel(update(Psi,{Reg,'*',car,'*'},Dic),Ch), L = R ;

	Entry = {_,_,Type,Vl}, Type = car(_), SC = {L,R} :
	write_channel(update(Psi,{Reg,'*',Type,Vl},Dic),Ch), L = R ;

	otherwise, Entry = {_,_,Type,Vl}, SC = {L,R} :
	write_channel(update(Psi,{Reg,'*',car(Type),Vl},Dic),Ch), L = R.

update_sub_args(Psi, Areg, I, Dic, Ch, SC)
:-
	I > 0, Dic = {DicIn,DicOut}, SC = {L,R} :
	Dic' = {DicM,DicOut}, SC' = {M,R} |
        phase2_dic#look_child(Psi, I, DicIn, Ent, Key), 
	I' := I - 1,
	update_sub_arg(Key, Ent, Areg(I), {DicIn, DicM}, Ch, {L,M}),
	update_sub_args ;

	I = 0, Dic = {DicIn,DicOut}, SC = {L,R} :
	DicOut = DicIn, L = R, Psi = _, Areg = _, Ch = _.

update_sub_arg(Psi, Ent, Addr, Dic, Ch, SC)
:-
	Ent = {_,_,new,_}, SC = {L,R} :
	write_channel(add(Psi,{Addr,'*',sub_arg,'*'},Dic), Ch), L = R ;

	Ent = {Areg,_,Type,Val}, Type =\= new,
	Areg =\= a(_), SC = {L,R} :
	write_channel(update(Psi,{Addr,'*',Type,Val},Dic),Ch), L = R ;

	Ent = {Areg,_,Type,_}, Type =\= new, Areg = a(_),
	Dic = {DicIn,DicOut}, SC = {L,R} :
	DicOut = DicIn, L = R, Psi = _, Addr = _, Ch = _.

get_type_sub_dgs(Type, Dg, SubDgs)
:-
	Dg ? E, E = {{Type,_},_}, SubDgs = H\T :
	H ! E, SubDgs' = H'\T | get_type_sub_dgs ;

	Dg ? E, E = {{Type1,_},_}, Type1 =\= Type |
				get_type_sub_dgs ;

	Dg ? E, E = nil(_), Type = nil, SubDgs = H\T :
	H ! E, SubDgs' = H'\T | get_type_sub_dgs ;

	Dg ? nil(_), Type =\= nil | get_type_sub_dgs ;

	Dg = [], SubDgs = H\T :	H = T, Type = _.

get_type_sub_dg(Type, Dg, SubDg)
:-
	Dg = [{Type,SubDg1}|_] : SubDg = SubDg1 ;
	Dg ? {Type1,_}, Type =\= Type1 | get_type_sub_dg ;
	Dg = [] : SubDg = [], Type = _.

branch_on_known_type(SubDgs, OwDg, OwLab, Psi, Entry, 
				BH, Dic, Ctl, Ch, Name, SC, Done)
:-
	SubDgs = [], Ch = {_,Ch1} : 
        write_channel(update(Psi,Entry,{Dic,Dic1}),Ch1), OwLab = _ |
	ctl#ctl1(OwDg, BH, Dic1, Ctl, Ch, Name, SC, Done) ;

	SubDgs =\= [], 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
        write_channel(update(Psi,Entry,{Dic,Dic1}),Ch1) |
	group_types(SubDgs, {_,DgTypes}),
	DgTypes? = [DgType],
	get_cont(OwLab, BH, BH'),
	a_type(DgType, label(L1), Psi, Entry, BH', Dic, 
			CtlH\[label(L1)|CtlM], Ch, Name, {L,M}, {DL,DM}),
	ctl#ctl1(OwDg, BH, Dic1, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR}).	

branches(Dg, OwDg, OwLab, Psi, Known, Entry, BH, Dic, Ctl, Ch, Name, SC, Done)
:-
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
        write_channel(update(Psi,Entry,{Dic,Dic1}),Ch1) |
	group_types(Dg,Dg1),
	get_cont(OwLab, BH, BH'),
	analyze_type(Dg1, label(Ow), Psi, Known, Entry, BH', Dic, 
	             CtlH\[label(Ow)|CtlM], Ch, Name, {L,M}, {DL,DM}),
	ctl#ctl1(OwDg, BH, Dic1, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR}).	

group_types(Dg,DgGroups)+(DgGroupsAcc=[],N=0)
:- 
	Dg ? list(SubDg) : DgGroupsAcc' = [list(SubDg)|DgGroupsAcc] |
	   N' := N + 1, group_types ;

	Dg ? nil(SubDg), DgGroupsAcc  = [list(SubDg1)|DgGroupsAcc1]  :
	DgGroupsAcc' = [list(SubDg1),nil(SubDg)|DgGroupsAcc1] |
	   N' := N + 1, group_types ;

	Dg ? nil(SubDg),
	DgGroupsAcc  =\= [list(_)|_]  :
	DgGroupsAcc' = [nil(SubDg)|DgGroupsAcc] |
           N' := N + 1, group_types ;

	Dg ? {tuple(Ar),SubDg} |
	   add_type(tuple, Ar, SubDg, DgGroupsAcc, N, DgGroupsAcc', N'),
	   group_types ;

	Dg ? {integer(I),SubDg} |
	   add_type(integer, I, SubDg, DgGroupsAcc, N, DgGroupsAcc', N'),
	   group_types ;

	Dg ? {real(R),SubDg} | 
	   add_type(real, R, SubDg, DgGroupsAcc, N, DgGroupsAcc', N'),
	   group_types ;

	Dg ? {string(S),SubDg} | 
	   add_type(string, S, SubDg, DgGroupsAcc, N, DgGroupsAcc', N'),
	   group_types ;

	Dg = [] : DgGroups = {N,DgGroupsAcc}.

add_type(Type, Value, SubDg, DgGroupsAcc, N, DgGroupsAcc1, N1)
:-
	DgGroupsAcc ? {Type,Nt,Values} :
	DgGroupsAcc1 = [{Type,Nt',Values1}|DgGroupsAcc'], N1 = N |
	    insert_value(Value, SubDg, Values, Values1),
	    Nt' := Nt + 1 ;

	DgGroupsAcc ? {Type1,SubDg1} : DgGroupsAcc1 ! {Type1,SubDg1} |
	    add_type ;

	DgGroupsAcc ? {Type1,Nt,Types}, Type1 =\= Type :
	DgGroupsAcc1 ! {Type1,Nt,Types} |
	    add_type ;

	DgGroupsAcc = [] : DgGroupsAcc1 = [{Type,1,[{Value,SubDg}]}] |
	    N1 := N + 1.

insert_value(Value, SubDg, ValuesIn, ValuesOut)
:-
	ValuesIn ? {Value1,SubDg1}, Value1 @< Value : 
	ValuesOut ! {Value1,SubDg1} | 
                 insert_value ;

	ValuesIn = [{V,_}|_], Value @< V : 
	ValuesOut = [{Value,SubDg}|ValuesIn] ;

	ValuesIn = [] : ValuesOut = [{Value,SubDg}].

analyze_type(DgGroups, OwLab,
	               Psi, Known, Regs, BH, Dic, Ctl, Ch, Name, SC, Done)
:-
	DgGroups = {N,DgTypes},	N > 4, Regs = {_A,Vreg}, 
	Ctl = CtlH\CtlT :
	CtlH ! switch_on_tag(Vreg, Tags) |
	many_types(DgTypes, OwLab, Psi, Known, 
			Regs, BH, Dic, Tags, CtlH'\CtlT, Ch, Name, SC, Done);

	DgGroups = {N,DgTypes}, N =< 4 |
	few_types(DgTypes, OwLab, 
	               Psi, Known, Regs, BH, Dic, Ctl, Ch, Name, SC, Done).

few_types(DgTypes, OwLab, 
	                 Psi, Known, Regs, BH, Dic, Ctl, Ch, Name, SC, Done)
:-
	DgTypes ? DgType,
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	Ctl' = CtlM\CtlT, SC' = {M,R}, Done' = {DM,DR} |
	one_type(DgType, label(NextLab), Psi, Regs, BH, Dic, 
			CtlH\[label(NextLab)|CtlM], Ch, Name, {L,M}, {DL,DM}),
	few_types ;

	DgTypes = [], Known = unknown, Regs = {Areg,Vreg,_,_},
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	CtlH = [if_tag(Vreg,'=\=',variable,OwLab),
		suspend_on(Areg)|CtlT], R = L, DR = DL, 
		Psi = _, BH = _, Dic = _, Ch = _, Name = _ ;
 
	DgTypes = [], Known = known, 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
        CtlH = CtlT, R = L, DR = DL, 
	Regs = _, Psi = _, BH = _, Dic = _, Ch = _, Name = _, OwLab = _.
 
many_types(DgTypes, OwLab, 
	            Psi, Known, Regs, BH, Dic, Tags, Ctl, Ch, Name, SC, Done)
:-
	DgTypes ? DgType,
	arg(1,DgType,Tag),
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	Tags ! {Tag,label(Lab)}, CtlH ! label(Lab),
	Ctl' = CtlM\CtlT, SC' = {M,R}, Done' = {DM,DR} |
	check_ground(Tag,{L,L1}),
	a_type(DgType, OwLab, Psi, Regs, BH, Dic, CtlH'\CtlM,
						Ch, Name, {L1,M}, {DL,DM}),
	many_types ;

	DgTypes = [], Known = unknown, 
	Regs = {Areg,_,_,_}, Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	CtlH = [label(VarL),suspend_on(Areg)|CtlT],
	Tags = [{variable,label(VarL)},OwLab], R = L, DR = DL,
	Psi = _, BH = _, Dic = _, Ch = _, Name = _ ;

	DgTypes = [], Known = known,
        Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} : 
	Tags = [OwLab], CtlH = CtlT, R = L, DR = DL, 
	Regs = _, Psi = _, BH = _, Dic = _, Ch = _, Name = _.

check_ground(Term,SC)
:-
	known(Term), SC = {L,R} : L = R.

a_type(DgType, OwLabel, Psi, Regs, BH, Dic, Ctl, Ch, Name, SC, Done)
:-
	DgType = nil(SubDg), Regs = {Areg,Vreg,_,_}, Ch = {_,Ch1} :
	write_channel(update(Psi,{Areg,Vreg,nil,nil},{Dic,Dic'}),Ch1),
	                                        OwLabel = _ |
	ctl#ctl1(SubDg, BH, Dic', Ctl, Ch, Name, SC, Done);

	DgType = list(SubDg), Regs = {Areg,_,_,_}, Ch = {_,Ch1} :
	write_channel(update(Psi,{Areg,'*',list,'*'},{Dic,Dic'}),Ch1),
	                                        OwLabel = _ |
	ctl#ctl1(SubDg, BH, Dic', Ctl, Ch, Name, SC, Done);

	DgType = {Type,N,SubDgs}, SubDgs ? {Low,_SubDg} |
	get_high(SubDgs', Low, High),
	branch_values(Type, N, SubDgs, {Low,High}, OwLabel, Psi, Regs,
					BH, Dic, Ctl, Ch, Name, SC, Done).

get_high(SubDgs, Low, High)
:-
	SubDgs = [{High1,_SubDg}|SubDg'] : 
	Low' = High1, Low = _ | 
	get_high(SubDg',Low',High) ;

	SubDgs = [] : High = Low.

branch_values(Type, N, SubDgs, LH, OwLabel, Psi, Regs,
					BH, Dic, Ctl, Ch, Name, SC, Done)
:-
	Type =\= string, Type =\= real, BH = {B,_H,_}, N < B : LH = _ | 
	conditional_jumps(Type, SubDgs, OwLabel, Psi, Regs, BH,	Dic, Ctl, Ch, 
							Name, SC, Done) ;

	BH = {B,_,_}, N >= B,
	LH = {Low,High}, Low < High, (N * 100)/(High - Low) > 40,
	Type =\= string, Type =\= real,	Regs = {_A,Vreg,_,_},
	Ctl = CtlH\CtlT, SC = {L,R} :
	CtlH ! {Inst,Vreg,Low,High,Labels} |
	utils#append_strings(['branch_',Type],Inst),
	check_ground(Inst,{L,M}),
	branch_table(Type, SubDgs, Low, OwLabel, Psi, Regs, BH,
			Dic, Labels, CtlH'\CtlT, Ch, Name, {M,R}, Done) ;

	BH = {B,_,_}, N >= B,
	Type = integer,
	LH = {Low,High}, Low < High, 
	(N * 100)/(High - Low) =< 40,
	Ctl = CtlH\CtlT,
	Regs = {_Areg,Vreg,_,_} :
	CtlH ! case_hash_integer(Vreg,Labels) |
	gen_hash(integer, N, SubDgs, SubDgs'),
	sort(SubDgs', SubDgs''),
	hash_table(integer, SubDgs'', OwLabel, N, Psi, Regs, BH, Dic, 
				CtlH'\CtlT, Labels, Ch, Name, SC, Done) ;

	BH = {B,_,_}, N >= B,
	Type = tuple,
	LH = {Low,High}, 
	Ctl = CtlH\CtlT,
	Regs = {_,Vreg,_,_} :
	CtlH ! branch_tuple(Vreg,Low,High,Labels) |
	branch_table(Type, SubDgs, Low, OwLabel, Psi, Regs, BH, Dic, 
				Labels, CtlH'\CtlT, Ch, Name, SC, Done) ;

	Type = string, BH = {_,H,_}, N > H,
	Ctl = CtlH\CtlT,
	Regs = {Areg,_Vreg,_,_} :
	CtlH = [case_hash_string(Areg,Labels)|CtlH'], LH = _ |
	gen_hash(string, N, SubDgs, SubDgs'),
	sort(SubDgs', SubDgs''),
	hash_table(string, SubDgs'', OwLabel, N, Psi, Regs, BH, Dic, 
				CtlH'\CtlT, Labels, Ch, Name, SC, Done) ;

	Type = real,
	BH = {_,H,_}, N > H,
	Ctl = CtlH\CtlT,
	Regs = {Areg,_Vreg,_,_} :
	CtlH = [case_hash_real(Areg,Labels)|CtlH'], LH = _ |
	gen_hash(real, N, SubDgs, SubDgs'),
	sort(SubDgs', SubDgs''),
	hash_table(real, SubDgs'', OwLabel, N, Psi, Regs, BH, Dic, 
				CtlH'\CtlT, Labels, Ch, Name, SC, Done) ;

	Type = string, BH = {_,H,_}, N =< H : LH = _ |
	conditional_jumps(string, SubDgs, OwLabel, Psi, Regs, BH, Dic, Ctl, Ch,
							Name, SC, Done) ;

	Type = real, BH = {_,H,_}, N =< H : LH = _ |
	conditional_jumps(real, SubDgs, OwLabel, Psi, Regs, BH, Dic, Ctl, Ch,
							Name, SC, Done) ;

        LH = {Low,Low}, N = 1 |
	conditional_jumps(Type, SubDgs, OwLabel, Psi, Regs, BH,	Dic, Ctl, Ch, 
							Name, SC, Done).


conditional_jumps(Type, SubDgs, OwLabel, Psi, Regs, BH, Dic, Ctl, Ch, 
						Name, SC, Done)
:-
	SubDgs ? {S,SubDg}, SubDgs' =\= [], Type = string,
	Regs = {Areg,_,_,_}, 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
	CtlH ! if_string(Areg,'=\=',S,label(L2)),
	write_channel(update(Psi,{Areg,'*',string,S},{Dic,Dic1}),Ch1),
	Ctl' = CtlM\CtlT, SC' = {M,R}, Done' = {DM,DR} |
	ctl#ctl1(SubDg, BH, Dic1, CtlH'\[label(L2)|CtlM], Ch, Name, 
						{L,M}, {DL,DM}),
	conditional_jumps ;

	SubDgs ? {Real,SubDg}, SubDgs' =\= [], Type = real,
	Regs = {Areg,_,_,_}, 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
	CtlH ! if_real(Areg,'=\=',Real,label(L2)),
	write_channel(update(Psi,{Areg,'*',real,Real},{Dic,Dic1}),Ch1),
	Ctl' = CtlM\CtlT, SC' = {M,R}, Done' = {DM,DR} |
	ctl#ctl1(SubDg, BH, Dic1, CtlH'\[label(L2)|CtlM], Ch, Name, 
						{L,M}, {DL,DM}),
	conditional_jumps ;

	SubDgs ? {I,SubDg}, SubDgs' =\= [], Type = integer,
	Regs = {Areg,Vreg,_,_}, 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR},Ch = {_,Ch1} :
	CtlH ! if_integer(Vreg,'=\=',I,label(L2)),
	write_channel(update(Psi,{Areg,Vreg,integer,I},{Dic,Dic1}),Ch1),
	Ctl' = CtlM\CtlT, SC' = {M,R}, Done' = {DM,DR} |
	ctl#ctl1(SubDg, BH, Dic1, CtlH'\[label(L2)|CtlM], Ch, Name, 
						{L,M}, {DL,DM}),
	conditional_jumps ;

	SubDgs ? {I,SubDg}, SubDgs' =\= [], Type = tuple,
	Regs = {Areg,Vreg,_,_}, 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
	CtlH ! if_tuple(Vreg,'=\=',I,label(L2)),
	write_channel(update(Psi,{Areg,Vreg,tuple,I},{Dic,Dic1}),Ch1),
	Ctl' = CtlM\CtlT, SC' = {M1,R}, Done' = {DM,DR} |
	update_sub_args(Psi, Areg, I, {Dic1,Dic2}, Ch1, {L,M}),
	ctl#ctl1(SubDg, BH, Dic2, CtlH'\[label(L2)|CtlM], Ch, Name, 
						{M,M1}, {DL,DM}),
	conditional_jumps ;

	SubDgs ? {S,SubDg}, SubDgs' = [], Type = string,
	Regs = {Areg,_,_,_}, Ctl = CtlH\CtlT, Ch = {_,Ch1} :
	CtlH ! if_string(Areg,'=\=',S,OwLabel),
	write_channel(update(Psi,{Areg,'*',string,S},{Dic,Dic1}),Ch1) |
	ctl#ctl1(SubDg, BH, Dic1, CtlH'\CtlT, Ch, Name, SC, Done) ;

	SubDgs ? {Real,SubDg}, SubDgs' = [], Type = real,
	Regs = {Areg,_,_,_}, Ctl = CtlH\CtlT, Ch = {_,Ch1} :
	CtlH ! if_real(Areg,'=\=',Real,OwLabel),
	write_channel(update(Psi,{Areg,'*',real,Real},{Dic,Dic1}),Ch1)|
	ctl#ctl1(SubDg, BH, Dic1, CtlH'\CtlT, Ch, Name, SC, Done) ;

	SubDgs ? {I,SubDg}, SubDgs' = [], Type = integer,
	Regs = {Areg,Vreg,_,_}, Ctl = CtlH\CtlT, Ch = {_,Ch1} :
	CtlH ! if_integer(Vreg,'=\=',I,OwLabel),
	write_channel(update(Psi,{Areg,Vreg,integer,I},{Dic,Dic1}),Ch1) |
	ctl#ctl1(SubDg, BH, Dic1, CtlH'\CtlT, Ch, Name, SC, Done) ;

	SubDgs ? {I,SubDg}, SubDgs' = [], Type = tuple,
	Regs = {Areg,Vreg,_,_}, Ctl = CtlH\CtlT, SC = {L,R}, Ch = {_,Ch1} :
	CtlH ! if_tuple(Vreg,'=\=',I,OwLabel),
	write_channel(update(Psi,{Areg,Vreg,tuple,I},{Dic,Dic1}),Ch1) |
	update_sub_args(Psi, Areg, I, {Dic1,Dic2}, Ch1, {L,M}),
	ctl#ctl1(SubDg, BH, Dic2, CtlH'\CtlT, Ch, Name, {M,R}, Done).

branch_table(Type, SubDgs, Low, OwLabel, Psi, Regs, BH, 
					Dic, Labels, Ctl, Ch, Name, SC, Done)
:-
	SubDgs ? {I,SubDg}, I = Low, Type = integer, Regs = {Areg,Vreg,_,_}, 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
	Labels ! label(ILab), CtlH ! label(ILab),
	Ctl' = CtlM\CtlT, SC' = {M,R}, Done' = {DM,DR},
	write_channel(update(Psi,{Areg,Vreg,integer,I},{Dic,Dic1}),Ch1)|
	Low' := Low + 1,
	ctl#ctl1(SubDg, BH, Dic1, CtlH'\CtlM, Ch, Name, {L,M}, {DL,DM}),
	branch_table ;

	SubDgs ? {I,SubDg}, I = Low, Type = tuple, Regs = {Areg,Vreg,_,_}, 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} :
	Labels ! label(ILab), CtlH ! label(ILab),
	Ctl' = CtlM\CtlT, SC' = {M1,R}, Done' = {DM,DR},
	write_channel(update(Psi,{Areg,Vreg,tuple,I},{Dic,Dic1}),Ch1)|
	Low' := Low + 1,
	update_sub_args(Psi, Areg, I, {Dic1,Dic2}, Ch1, {L,M}),
	ctl#ctl1(SubDg, BH, Dic2, CtlH'\CtlM, Ch, Name, {M,M1}, {DL,DM}),
	branch_table ;

	SubDgs = [{I,_SubDg}|_], I > Low : Labels ! OwLabel |
	Low' := Low + 1, branch_table ;

	SubDgs = [], Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	Labels = [OwLabel], CtlH = CtlT, L = R, DL = DR,
	Type = _, Low = _, Psi = _, Regs = _, BH = _, Dic = _, Ch = _, Name =_.

one_type(DgType, OwLabel, Psi, Regs, BH, Dic, Ctl, Ch, Name, SC, Done)
:- 
	DgType = list(SubDg), Regs = {Areg,Vreg,_,_}, 
	Ctl = CtlH\CtlT, Ch = {_,Ch1} :
	CtlH ! if_tag(Vreg,'=\=',list,OwLabel),
	write_channel(update(Psi,{Areg,'*',list,'*'},{Dic,Dic1}),Ch1) |
             phase2_dic#add_child(Psi,1,{Vreg,'*',car,'*'},{Dic1,Dic2}),
             phase2_dic#add_child(Psi,2,{Areg(1),'*',sub_arg,'*'},{Dic2,Dic3}),
	     ctl#ctl1(SubDg, BH, Dic3, CtlH'\CtlT, Ch, Name, SC, Done) ;

	DgType = nil(SubDg), Regs = {Areg,Vreg,_,_}, 
	Ctl = CtlH\CtlT, Ch = {_,Ch1} :
	CtlH ! if_tag(Vreg,'=\=',nil,OwLabel),
	write_channel(update(Psi,{Areg,Vreg,nil,nil},{Dic,Dic'}),Ch1)|
	ctl#ctl1(SubDg, BH, Dic', CtlH'\CtlT, Ch, Name, SC, Done) ;

	DgType = {Type,N,SubDgs}, SubDgs ? {Low,_SubDg} |
	get_high(SubDgs', Low, High),
	branch_values(Type, N, SubDgs, {Low,High}, OwLabel, Psi, Regs, BH, Dic,
						Ctl, Ch, Name, SC, Done).

gen_hash(Type, M, SubDgs, SubDgsOut)
:-
        SubDgs = [] : SubDgsOut = [], Type = _, M = _ ;

	Type = integer, SubDgs ? {Value,SubDg} : SubDgsOut ! {I,Value,SubDg} |
	mod(Value?, M?, I), gen_hash ;

	Type = real, SubDgs ? {Value,SubDg} : SubDgsOut ! {I,Value,SubDg} |
	mod(Value?, M?, I), gen_hash ;

	Type = string, SubDgs ? {Value,SubDg} : SubDgsOut ! {I,Value,SubDg} |
	I := string_hash(Value) \ M,
	gen_hash.

sort(SubDgsIn, SubDgsOut)+(Tail=[])
:-
	SubDgsIn ? X |
	partition(SubDgsIn', X, Smaller, Larger),
	sort(Smaller, SubDgsOut, [X|SubDgsOut1]),
	sort(Larger, SubDgsOut1, Tail) ;

        SubDgsIn = [] : SubDgsOut = Tail.

partition(SubDgsIn, Pivot, Smaller, Larger)
:-
	SubDgsIn ? X, X = {Hash,_Value,_SubSg}, 
	Pivot = {PivotHash,_PivotValue,_PivotSubDg}, PivotHash >= Hash :
	Smaller ! X | partition ;

	SubDgsIn ? X, X = {Hash,_Value,_SubSg},
	Pivot = {PivotHash,_PivotValue,_PivotSubDg}, PivotHash < Hash :
	Larger ! X | partition ;

	SubDgsIn = [] : Smaller = [], Larger = [], Pivot = _.

hash_table(Type, SubDgs, OwLabel, Entries, Psi, Regs, BH, Dic, Ctl,
					Labels, Ch, Name, SC, Done)+(N=0)
:-
	SubDgs = [], N < Entries : Labels ! OwLabel | N' := N + 1,
	hash_table ;

	SubDgs = [], N = Entries, Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	Labels = [OwLabel], CtlH = CtlT, L = R, DL = DR,
	Type = _, Psi = _, Regs = _, BH = _, Dic = _, Ch = _, Name = _ ;

	SubDgs = [{Hash,_Value,_SubDg}|_SubDgs'],	N < Hash : 
	Labels ! OwLabel | N' := N + 1,	hash_table ;

	SubDgs = [{Hash,_Value,_SubDg}|_SubDgs'],
	N = Hash, Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	Labels ! label(L1), CtlH ! label(L1),
	Ctl' = CtlM\CtlT, SC' = {M,R}, Done' = {DM,DR} |
	get_equals(N, SubDgs, SubDgsEq, SubDgs'),
	conditional_jumps(Type, SubDgsEq, OwLabel, Psi, Regs, BH, Dic, 
					CtlH'\CtlM, Ch, Name, {L,M}, {DL,DM}),
	N' := N + 1,
	hash_table.

get_equals(N, Ss, SsEq, SsRest)
:-
	Ss ? {N,Value,SubDg} : SsEq ! {Value,SubDg} | 
	get_equals ;

	Ss = [] : SsEq = [], SsRest = [], N = _ ;

	Ss = [{I,_,_}|_], I =\= N : SsRest = Ss, SsEq = [].

append(Xs,Ys,Zs)
:-
	Xs ? X : Zs ! X | append ;
        Xs = [] : Zs = Ys.

