/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/new_term_creation.cp,v 1.1.1.1 1999/07/09 07:03:02 bill Exp $ */
-language(compound).
-export([new_term/6]).
-mode(trust).

procedure new_term(Psi, Term, Ctl, Dic, Ch, SC).

new_term(Psi, Term, Ctl, Dic, Ch, SC)
:-	
	Term = integer(I), Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [multiple_copy([I],[Reg])|CtlT],
	write_channel(add(Psi,{Reg,Reg,integer,I},{DicM,DicO}),Ch),
	L = R ;

	Term = string(S), Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [allocate_var(Reg),multiple_assign([S],[Reg])|CtlT],
	write_channel(add(Psi,{Reg,'*',string,S},{DicM,DicO}),Ch),
	L = R ;

	Term = real(I), Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [allocate_var(Reg),multiple_assign([I],[Reg])|CtlT],
	write_channel(add(Psi,{Reg,'*',real,I},{DicM,DicO}),Ch),
	L = R ;

	Term = nil([]), Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [allocate_var(Reg),multiple_assign([[]],[Reg])|CtlT],
	write_channel(add(Psi,{Reg,'*',nil,'*'},{DicM,DicO}),Ch),
	L = R ;

	Term = psi(P), Dic = {DicI,_} :
	write_channel(look(P,Entry,DicI),Ch) |
	update_dic(Psi, P, Entry, Ctl, Dic, Ch, SC) ;

	Term = ro(psi(Ro)), Dic = {DicI,DicO} :	
        write_channel(look(ro(Ro),EntryR,DicI),Ch),
	write_channel(add(Psi,{Reg,'*',ref,'*'},{DicM,DicM1}),Ch),
	write_channel(add(ro(Psi),{Reg,'*',ref,'*'},{DicM1,DicO}),Ch) |
	ro_variable(Ro, EntryR, {DicI,DicM}, Reg, Ctl, Ch, SC);

	Term = tuple(Tup), arity(Tup,Ar), Dic = {DicI,DicO} : 
	write_channel(add(Psi,{Reg,'*',tuple,Ar},{DicM,DicO}),Ch) |
	ground_check#check_ground(Term, CtlTerm, {ground,Ground}),
	tell_term(Ground, Term, CtlTerm, {DicI,DicM}, Reg, Ctl,	Ch, SC);

	Term = list(_), Dic = {DicI,DicO} : 
	write_channel(add(Psi,{Reg,'*',list,'*'},{DicM,DicO}),Ch) |
	ground_check#check_ground(Term, CtlTerm, {ground,Ground}),
	tell_term(Ground, Term, CtlTerm, {DicI,DicM}, Reg, Ctl,	Ch, SC).

update_dic(Psi, P, Entry, Ctl, Dic, Ch, SC)
:-
	Entry = {_,_,new,_}, Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [allocate_var(Reg)|CtlT],
	write_channel(add(Psi,{Reg,'*',ref,'*'},{DicM,DicM1}),Ch),
	write_channel(add(P,{Reg,'*',ref,'*'},{DicM1,DicO}),Ch),
	L = R ;

	Entry =\= {_,_,new,_}, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(add(Psi,Entry,Dic),Ch), P = _,
	CtlH = CtlT, DicO = DicI, L = R.

tell_term(Ground, Term, CtlTerm, Dic, Reg, Ctl, Ch, SC)
:-
	Ground = ground, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Reg,Dic),Ch),
	CtlH = [fetch(CtlTerm,Reg)|CtlT], L = R, Term = _ ;

	Ground = not_ground, Term = list(_), 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch), CtlTerm = _ |
	    not_grounded_list(Term, Reg, {DicM,DicO}, Args,
			CtlH\[allocate_list(Args,Reg)|CtlT],
			Ch, SC) ;

        Ground = not_ground, Term = tuple(Tuple), 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch), CtlTerm = _ |
            arity(Tuple,Length),
	    not_grounded_tuple(Tuple, Length, Reg, {DicM,DicO}, Args,
			CtlH\[allocate_tuple(Args,Reg)|CtlT],
			Ch, SC).

not_grounded_list(List, ListOut, Dic, Args, Ctl, Ch, SC)+(N=0)
:-
	List = list([Car|List']), Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R}:
	Args ! ArgN, Ctl' = CtlM\CtlT, Dic' = {DicI',DicO}, SC' = {M,R} |
	N' := N + 1,
        car_creation(Car, {DicI, DicI'}, ArgN, CtlH\CtlM, Ch, {L,M}),
	not_grounded_list ;

	List =\= list(_) : Args = ArgN |
        term_creation(List, Dic, ArgN, Ctl,Ch, SC, {ListOut,N}). 

not_grounded_tuple(Term, Length, TermOut, Dic, Args, Ctl, Ch, SC) + (N=1)
:-
	N =< Length, 	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	Args ! ArgN, 	Dic' = {DicI',DicO}, SC' = {M,R}, Ctl' = CtlM\CtlT |
	arg(N,Term,TermN), N' := N + 1,
	term_creation(TermN, {DicI,DicI'}, ArgN, CtlH\CtlM,Ch,{L,M},{TermOut,N}),
	not_grounded_tuple ;

	N > Length, Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	Args = [] , DicO = DicI, CtlH = CtlT, L = R, Term = _, TermOut = _, Ch=_.

term_creation(Term, Dic, Reg, Ctl, Ch, SC, Base)
:-
	Term = integer(I), Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
        Reg = I,  DicO = DicI, CtlH = CtlT, L = R,	Ch = _, Base = _ ;

	Term = string(I), Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = I, DicO = DicI, CtlH = CtlT, L = R,	Ch = _, Base = _ ;

	Term = nil([]), 	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = [], DicO = DicI, CtlH = CtlT, L = R,	Ch = _, Base = _ ;

	Term = real(I), 	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = I, DicO = DicI, CtlH = CtlT,L = R, Ch = _, Base = _ ;

	Term = psi(P), Dic = {DicI,_} :
	write_channel(look(P,Entry,DicI),Ch) |
	variable(P, Entry, Base, Dic, Reg, Ctl, Ch, SC);

        Term = variable('_'), Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
        CtlH = CtlT, DicI = DicO, L = R, Reg = {'_'}, Base = _, Ch = _ ;

	Term = ro(psi(Ro)), Dic = {DicI,_} :	
        write_channel(look(ro(Ro),EntryR,DicI),Ch),	Base = _ |
	ro_variable(Ro, EntryR, Dic, Reg, Ctl, Ch, SC);

	Term = tuple(_) : Base = _ |
	ground_check#check_ground(Term, CtlTerm, {ground,Ground}),
	tell_term(Ground, Term, CtlTerm, Dic, Reg, Ctl, Ch, SC) ;

	Term = list(_) : Base = _ |
	ground_check#check_ground(Term, CtlTerm, {ground,Ground}),
	tell_term(Ground, Term, CtlTerm, Dic, Reg, Ctl, Ch, SC).

car_creation(Car, Dic, Arg, Ctl, Ch, SC)
:-
	Car = psi(Psi), Dic = {DicI,_} :
	write_channel(look(Psi,Entry,DicI), Ch) |
	car_variable(Psi, Entry, Dic, Arg, Ctl, Ch, SC);

	Car =\= psi(_) |
        term_creation(Car, Dic, Arg, Ctl, Ch, SC, 0).

car_variable(Var, Entry, Dic, VarOut, Ctl, Ch, SC)
:-
	Entry = {Areg,_,car,_},	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	VarOut = {Areg,0}, CtlH = CtlT, L = R, DicO = DicI, Var = _, Ch = _ ;

	Entry = {Areg,_,car(_),_}, 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	VarOut = {Areg,0}, CtlH = CtlT, L = R, DicO = DicI, Var = _, Ch = _ ;

	Entry = {_,_,new,_}, 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(VarOut,{DicI,DicM}),Ch),
	CtlH = [allocate_var(VarOut)|CtlT],
	write_channel(add(Var,{VarOut,'*',var,'*'},{DicM,DicO}),Ch),
	L = R ;

	otherwise |
        variable(Var, Entry, error, Dic, VarOut, Ctl, Ch, SC).

variable(Term, Entry, Base, Dic, Reg, Ctl, Ch, SC)
:-
	Entry = {_,_,new,_},  	Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = {'_'},  
	write_channel(add(Term,{Base,'*',var,'*'},Dic),Ch),
	CtlH = CtlT, L = R ;

	Entry = {Areg,_,ref,_}, Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = Areg, CtlH = CtlT, DicI = DicO, L = R, Term = _, Base = _, Ch = _ ;

	Entry = {Areg,_,car,_}, Dic = {DicI,DicO},
	Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [load_car(Areg,Reg)|CtlT],
	L = R, Base = _, 
	write_channel(update(Term,{Reg,'*',ref,'*'},{DicM,DicO}),Ch) ;

	Entry = {Areg,_,car(Type),Val}, Dic = {DicI,DicO},
	Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [deref_car(Areg,Reg)|CtlT],
	L = R, Base = _,
	write_channel(update(Term,{Reg,'*',Type,Val},{DicM,DicO}),Ch) ;

	Entry = {Areg,_,sub_arg,_}, Dic = {DicI,DicO}, Ctl = CtlH\CtlT,SC ={L,R}:
	Reg = {'&',Areg}, CtlH = CtlT, L = R, DicI = DicO,
	Base = _, Term = _, Ch = _ ;

	Entry = {Areg,_,deref,_}, Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R}:
	Reg = Areg, CtlH = CtlT, L = R, DicO = DicI, Base = _, Term = _, Ch = _ ;

	Entry = {Areg,_,list,_}, Areg = a(_), 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = Areg, DicI = DicO, CtlH = CtlT, L = R, Term = _, Base = _, Ch = _ ;

	Entry = {Areg,_,list,_}, Areg = {a(_),I}, I > 0, 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = {'&',Areg}, DicI = DicO, CtlH = CtlT, L = R,
	Term = _, Base = _, Ch = _ ;

	Entry = {Areg,_,list,_}, Areg = {A,-1}, A = a(_),
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [decrement_pointer(A,Reg)|CtlT],
	write_channel(update(Term,{Reg,'*',list,'*'},{DicM,DicO}),Ch),
	L = R, Base = _ ;

	Entry = {_,Areg,integer,_}, Areg = a(_), 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = Areg, DicI = DicO, CtlH = CtlT, L = R, Term = _, Base = _, Ch = _ ;

	Entry = {Areg,'*',integer,_},
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = Areg, DicI = DicO, CtlH = CtlT, L = R, Term = _, Base = _, Ch = _ ;

	otherwise | varibale_rest(Term, Entry, Base, Dic, Reg, Ctl, Ch, SC).

varibale_rest(Term, Entry, Base, Dic, Reg, Ctl, Ch, SC)
:-
	Entry = {Areg,_,_,_}, Areg = a(_), 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = Areg, DicO = DicI, CtlH = CtlT, L = R, 
	Term = _, Base = _, Ch = _ ;

	Entry = {Areg,_,_,_}, Areg = {a(_),_},
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = {'&',Areg}, DicO = DicI, CtlH = CtlT, L = R, 
	Term = _, Base = _, Ch = _.

ro_variable(Ro, Entry, Dic, RoOut, Ctl, Ch, SC)
:-
	Entry = {_,_,new,_}, Dic = {DicI,DicO} :
	RoOut = ro(VarOut),
	write_channel(add(ro(Ro),{ro(VarOut),'*',ref,'*'},{DicI,DicM}),Ch) |
	unsafe_term_creation#term_creation(psi(Ro), {DicM,DicO}, VarOut, Ctl, 
	                                                          Ch, SC) ;

	Entry = {Areg,'*',ref,_}, Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R}:
	RoOut = Areg, DicO = DicI, CtlH = CtlT, L = R, 	Ro = _, Ch = _.

