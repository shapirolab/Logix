/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/unsafe_term_creation.cp,v 1.1.1.1 1999/07/09 07:03:02 bill Exp $ */
-language(compound).
-export([term_creation/6]).
-mode(trust).

procedure term_creation(Term, Dic, TermOut, Ctl, Ch, SC).

term_creation(Term, Dic, TermOut, Ctl, Ch, SC)+(Base=0)
:-
	Term = integer(I), 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	TermOut = I, DicO = DicI, CtlH = CtlT, L = R, Ch = _, Base = _ ;

	Term = string(S), 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	TermOut = S, DicO = DicI, CtlH = CtlT, L = R, Ch = _, Base = _ ;

	Term = nil([]), 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	TermOut = [], DicO = DicI, CtlH = CtlT, L = R, Ch = _, Base = _ ;

	Term = real(I), 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	TermOut = I, DicO = DicI, CtlH = CtlT, L = R, Ch = _, Base = _ ;

	Term = psi(Psi), Dic = {DicI,_} :
	write_channel(look(Psi,Entry,DicI), Ch) |
	variable(Psi, Entry, Base, Dic, TermOut, Ctl, Ch, SC);

	Term = variable('_'), Base = 0, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Areg,Dic),Ch),
	CtlH = [allocate_var(Areg)|CtlT], TermOut = Areg, L = R ;

	Term = variable('_'), Base =\= 0,
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	CtlH = CtlT, DicI = DicO, L = R, TermOut = {'_'}, Ch = _ ;

	Term = ro(psi(Ro)), Dic = {DicI,_} :	
        write_channel(look(ro(Ro),Entry,DicI),Ch), Base = _ |
	ro_variable(Ro, Entry, Dic, TermOut, Ctl, Ch, SC) ;

	Term = tuple(_) : Base = _ |
	ground_check#check_ground(Term, CtlTerm, {ground,Ground}),
	compound_term_creation(Ground, Term, CtlTerm, Dic, TermOut, Ctl, Ch, SC);

	Term = list(_) : Base = _ |
	ground_check#check_ground(Term, CtlTerm, {ground,Ground}),
	compound_term_creation(Ground, Term, CtlTerm, Dic, TermOut, Ctl, Ch, SC).

compound_term_creation(Ground, Term, CtlTerm, Dic, TermOut, Ctl, Ch, SC)
:-
	Ground = ground, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(TermOut,Dic),Ch),
	CtlH = [fetch(CtlTerm,TermOut)|CtlT], L = R, Term = _ ;

	Ground = not_ground, Term = list(_), 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT :
	write_channel(get_reg(TermOut,{DicI,DicM}),Ch), CtlTerm = _ |
	    not_grounded_list(Term, TermOut, {DicM,DicO}, Args,
			CtlH\[allocate_list(Args,TermOut)|CtlT], 
			Ch, SC) ;

        Ground = not_ground, Term = tuple(Tuple), 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT :
	write_channel(get_reg(TermOut,{DicI,DicM}),Ch), CtlTerm = _ |
            arity(Tuple,Length),
	    not_grounded_tuple(Tuple, Length, TermOut, {DicM,DicO}, Args,
			CtlH\[allocate_tuple(Args,TermOut)|CtlT],
			Ch, SC).

not_grounded_list(List, ListOut, Dic, Args, Ctl, Ch, SC)+(N=0)
:-
	List = list([Car|List']),
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	Args ! ArgN, 
	Ctl' = CtlM\CtlT, Dic' = {DicI',DicO}, SC' = {M,R} |
	N' := N + 1,
        car_creation(Car, {DicI, DicI'}, ArgN, CtlH\CtlM, Ch, {L,M}),
	not_grounded_list ;

	List =\= list(_) : Args = ArgN |
	     term_creation(List, Dic, ArgN, Ctl, Ch, SC, {ListOut,N}).

not_grounded_tuple(Term, Length, TermOut, Dic, Args, Ctl, Ch, SC) + (N=1)
:-
	N =< Length, 
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	Args ! ArgN, 
	Dic' = {DicI',DicO}, SC' = {M,R}, Ctl' = CtlM\CtlT |
        arg(N,Term,TermN), N' := N + 1,
        term_creation(TermN, {DicI,DicI'}, ArgN, CtlH\CtlM,Ch,{L,M},{TermOut,N}),
	not_grounded_tuple ;

	N > Length, Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	Args = [], DicO = DicI, CtlH = CtlT, L = R, Term = _, TermOut = _, Ch =_.

variable(Var, Entry, Base, Dic, VarOut, Ctl, Ch, SC)
:-
	Entry = {_,_,new,_},	Base =\= 0, 	Ctl = CtlH\CtlT, SC = {L,R} :
	VarOut = {'_'},	CtlH = CtlT, L = R,
	write_channel(add(Var,{Base,'*',sub_arg,'*'},Dic),Ch) ;

	Entry = {_,_,new,_},    Base = 0,
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	CtlH = [allocate_var(Areg)|CtlT],
	VarOut = Areg, L = R,
	write_channel(get_reg(Areg,{DicI,DicM}),Ch),
	write_channel(add(Var,{Areg,'*',ref,'*'},{DicM,DicO}),Ch),
	Base = _ ;

	Entry = {Areg,_,ref,_}, Dic = {DicI,DicO},  Ctl = CtlH\CtlT, SC = {L,R} :
	VarOut = Areg, CtlH = CtlT, L = R, DicO = DicI,	Var = _, Ch =_, Base = _;

	Entry = {Areg,_,car,_}, Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(VarOut,{DicI,DicM}),Ch),
	CtlH = [load_car(Areg,VarOut)|CtlT], L = R, Base = _,
	write_channel(update(Var,{VarOut,'*',ref,'*'},{DicM,DicO}),Ch) ;

	Entry = {Areg,_,car(Tp),Val}, 	Tp =\= var, 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(VarOut,{DicI,DicM}),Ch),
	CtlH = [load_car(Areg,VarOut)|CtlT],	L = R, Base = _, 
	write_channel(update(Var,{VarOut,'*',Tp,Val},{DicM,DicO}),Ch) ;

	Entry = {Areg,_,car(var),_}, 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(VarOut,{DicI,DicM}),Ch),
	CtlH = [load_car(Areg,VarOut)|CtlT],	L = R, Base = _, 
	write_channel(update(Var,{VarOut,'*',ref,'*'},{DicM,DicO}),Ch) ;

	Entry = {Areg,_,sub_arg,_}, Base =\= 0,
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	VarOut = {'&',Areg}, CtlH = CtlT, L = R, DicO = DicI, Var = _, Ch = _ ;
	
	Entry = {Areg,_,sub_arg,_}, Base = 0, 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
        write_channel(get_reg(VarOut,{DicI,DicM}),Ch),
	CtlH = [deref(Areg,VarOut)|CtlT],	L = R, 
	write_channel(update(Var,{VarOut,'*',ref,'*'},{DicM,DicO}),Ch) ;

	Entry = {Areg,_,cdr,_}, Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
        write_channel(get_reg(VarOut,{DicI,DicM}),Ch),
	CtlH = [deref(Areg,VarOut)|CtlT],	L = R,  Base = _,
	write_channel(update(Var,{VarOut,'*',ref,'*'},{DicM,DicO}),Ch) ;

	Entry = {Areg,_,deref,_}, Ctl = CtlH\CtlT, SC = {L,R} :
	VarOut = Areg, CtlH = CtlT, L = R, Base = _,
	write_channel(update_type(Var,ref,Dic),Ch) ;

	Entry = {Areg,_,list,_}, Areg = a(_), Ctl = CtlH\CtlT :
	VarOut = Areg, CtlH = CtlT, Base = _ |
	update_unsafe_variables(Var, 2, Dic, Ch, SC) ;

	Entry = {Areg,_,list,_}, Areg = {a(_),I}, I > 0, Base =\= 0, 
	Ctl = CtlH\CtlT :
	VarOut = {'&',Areg}, CtlH = CtlT |
	update_unsafe_variables(Var, 2, Dic, Ch, SC) ;

	Entry = {Areg,_,list,_}, Areg = {a(_),I}, I > 0, Base = 0,
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT :
        write_channel(get_reg(VarOut,{DicI,DicM}),Ch),
	CtlH = [deref(Areg,VarOut)|CtlT],	
	write_channel(update(Var,{VarOut,'*',list,'*'},{DicM,DicM1}),Ch) |
        update_unsafe_variables(Var,2, {DicM1,DicO}, Ch, SC) ;

	Entry = {Areg,_,list,_}, Areg = {Reg,-1}, Reg = a(_),
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT :
        write_channel(get_reg(VarOut,{DicI,DicM}),Ch),
	CtlH = [decrement_pointer(Reg,VarOut)|CtlT],	Base = _,
	write_channel(update(Var,{VarOut,'*',list,'*'},{DicM,DicM1}),Ch) |
        update_unsafe_variables(Var,2, {DicM1,DicO}, Ch, SC) ;

	Entry = {_,Areg,integer,_}, Areg = a(_),
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	VarOut = Areg, DicI = DicO, CtlH = CtlT, L = R, 
	Base = _, Var = _, Ch =_;

	Entry = {Areg,'*',integer,_}, Base =\= 0,
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	VarOut = Areg, DicI = DicO, CtlH = CtlT, L = R, Var = _, Ch =_;

	Entry = {Areg,'*',integer,_}, Areg = a(_),
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	VarOut = Areg, DicI = DicO, CtlH = CtlT, L = R, 
	Base = _, Var = _, Ch =_;

	Entry = {Areg,'*',integer,I}, Base = 0, Areg = {a(_),_},  
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	CtlH = [deref(Areg,Reg,VarOut)|CtlT],
	write_channel(get_2_regs(Reg,VarOut,DicI,DicM1),Ch),
	write_channel(update(Var,{Reg,VarOut,integer,I},{DicM1,DicO}),Ch),
	L = R ;

	Entry = {Areg,_,tuple,Ar}, Areg = a(_), Ar =\= '*', Ctl = CtlH\CtlT :
	VarOut = Areg, CtlH = CtlT, Base = _ |
	update_unsafe_variables(Var, Ar, Dic, Ch, SC) ;

	Entry = {Areg,_,tuple,Ar}, Areg = {a(_),_}, Base =\= 0, Ar =\= '*', 
	Ctl = CtlH\CtlT :
	VarOut = {'&',Areg}, CtlH = CtlT |
	update_unsafe_variables(Var, Ar, Dic, Ch, SC) ;

	Entry = {Areg,_,tuple,Ar}, Ar =\= '*', Areg = {a(_),_}, Base = 0,
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT :
        write_channel(get_reg(VarOut,{DicI,DicM}),Ch),
	CtlH = [deref(Areg,VarOut)|CtlT],
	write_channel(update(Var,{VarOut,'*',tuple,Ar},{DicM,DicM1}),Ch) |
	update_unsafe_variables(Var, Ar, {DicM1,DicO}, Ch, SC) ;

	Entry = {Areg,_,var,_}, Areg = a(_), Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(update_type(Var,ref,Dic),Ch),
	VarOut = Areg, CtlH = CtlT, L = R, Base = _ ;

	Entry = {Areg,_,var,_}, Areg = {a(_),_}, Base = 0,
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
        write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [deref(Areg,Reg)|CtlT],
	write_channel(update(Var,{Reg,'*',ref,'*'},{DicM,DicO}),Ch),
	VarOut = Reg, L = R ;

	Entry = {Areg,_,var,_}, Areg = {a(_),_}, Base =\= 0,
	Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(update_type(Var,sub_arg,Dic),Ch),
	VarOut = {'&',Areg}, CtlH = CtlT, L = R ;

	Entry = {Areg,_,ref(var),_}, Areg = a(_), Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(update_type(Var,ref,Dic),Ch),
	VarOut = Areg, CtlH = CtlT, L = R, Base = _ ;

	Entry = {Areg,_,ref(var),_}, Areg = {a(_),_}, Base = 0,
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
        write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [deref(Areg,Reg)|CtlT],
	write_channel(update(Var,{Reg,'*',ref,'*'},{DicM,DicO}),Ch),
	VarOut = Reg, L = R ;

	Entry = {Areg,_,ref(var),_}, Areg = {a(_),_}, Base =\= 0,
	Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(update_type(Var,sub_arg,Dic),Ch),
	VarOut = {'&',Areg}, CtlH = CtlT, L = R ;

	otherwise | variable_rest(Var, Entry, Base, Dic, VarOut, Ctl, Ch, SC).

variable_rest(Var, Entry, Base, Dic, VarOut, Ctl, Ch, SC)
:-
	Entry = {Areg,_,_,_}, Areg = a(_),
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	VarOut = Areg, DicI = DicO, CtlH = CtlT, L = R, Base = _, Var = _, Ch =_;

	Entry = {Areg,_,_,_}, Base =\= 0, Areg = {a(_),_},
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	VarOut = {'&',Areg}, DicI = DicO, CtlH = CtlT, L = R, Var = _, Ch =_;

	Entry = {Areg,_,Type,_}, Base = 0, Areg = {a(_),_},  
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
        write_channel(get_reg(VarOut,{DicI,DicM}),Ch),
	CtlH = [deref(Areg,VarOut)|CtlT],	L = R, 
	write_channel(update(Var,{VarOut,'*',Type,'*'},{DicM,DicO}),Ch).

ro_variable(Ro, Entry, Dic, RoOut, Ctl, Ch, SC)
:-
	Entry = {_,_,new,_}, Dic = {DicI,DicO} :
	RoOut = ro(VarOut),
	write_channel(add(ro(Ro),{ro(VarOut),'*',ref,'*'},{DicI,DicM}),Ch)|
	term_creation(psi(Ro), {DicM,DicO}, VarOut, Ctl, Ch, SC) ;

	Entry = {Areg,'*',ref,_}, Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R}:
	RoOut = Areg, DicO = DicI, CtlH = CtlT, L = R, Ro = _, Ch = _.

car_creation(Car, Dic, Arg, Ctl, Ch, SC)
:-
	Car = psi(Psi), Dic = {DicI,_} :
	write_channel(look(Psi,Entry,DicI), Ch) |
	car_variable(Psi, Entry, Dic, Arg, Ctl, Ch, SC);

	Car =\= psi(_) |
        term_creation(Car, Dic, Arg, Ctl, Ch, SC, 1).

car_variable(Var, Entry, Dic, VarOut, Ctl, Ch, SC)
:-
	Entry = {Areg,_,car,_},	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	VarOut = {Areg,0}, CtlH = CtlT, L = R, DicO = DicI, Var = _, Ch = _ ;

	Entry = {Areg,_,car(var),_}, Ctl = CtlH\CtlT, SC = {L,R} :
	VarOut = {Areg,0}, CtlH = CtlT, L = R, 
	write_channel(update_type(Var,car,Dic),Ch) ; 

	Entry = {Areg,_,car(list),_}, Ctl = CtlH\CtlT :
	VarOut = {Areg,0}, CtlH = CtlT |
	update_unsafe_variables(Var, 2, Dic, Ch, SC) ;

	Entry = {Areg,_,car(tuple),Ar}, Ar =\= '*', Ctl = CtlH\CtlT :
	VarOut = {Areg,0}, CtlH = CtlT |
	update_unsafe_variables(Var, Ar, Dic, Ch, SC) ;

	Entry = {Areg,_,car(tuple),'*'}, 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	VarOut = {Areg,0}, CtlH = CtlT, L = R, DicO = DicI, Var = _, Ch = _ ;

	otherwise |
        variable(Var, Entry, 0, Dic, VarOut, Ctl, Ch, SC).

update_unsafe_variables(Psi, Ar, Dic, Ch, SC)
:-
	Ar > 0, Dic = {DicI,DicO}, SC = {L,R} :
        Dic'  = {DicM,DicO}, SC' = {M,R} |
        phase2_dic#look_child(Psi, Ar, DicI, Entry, Key),
        update_unsafe_var(Key, Entry, {DicI,DicM}, Ch, {L,M}),
	Ar' := Ar - 1,
	update_unsafe_variables ;

	Ar = 0, Dic = {DicI,DicO}, SC = {L,R} :
        DicO = DicI, R = L, Psi = _, Ch = _.

update_unsafe_var(Psi, Entry, Dic, Ch, SC)
:-
	Entry = {a(_),_,var,_}, SC = {L,R} :
        write_channel(update_type(Psi,ref,Dic),Ch), R = L ;

	Entry = {{a(_),_},_,var,_}, SC = {L,R} :
        write_channel(update_type(Psi,sub_arg,Dic),Ch), R = L ;

	Entry = {a(_),_,ref(var),_}, SC = {L,R} :
        write_channel(update_type(Psi,ref,Dic),Ch), R = L ;

	Entry = {{a(_),_},_,ref(var),_}, SC = {L,R} :
        write_channel(update_type(Psi,sub_arg,Dic),Ch), R = L ;

	Entry = {_,_,tuple,Ar}, Ar =\= '*' |
        update_unsafe_variables(Psi, Ar, Dic, Ch, SC) ;

	Entry = {_,_,list,_} |
        update_unsafe_variables(Psi, 2, Dic, Ch, SC) ;

	otherwise, Dic = {DicI,DicO}, SC = {L,R} :
        DicO = DicI, R = L, Psi = _, Entry = _, Ch = _.

