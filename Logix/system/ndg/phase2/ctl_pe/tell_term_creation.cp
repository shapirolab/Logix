/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/tell_term_creation.cp,v 1.1 1999/07/09 07:03:01 bill Exp $ */
-language(compound).
-export([term_creation/8]).
-mode(trust).

procedure term_creation(Term, Dic, Reg, Ctl, UpDics, Ent, Ch, SC).

term_creation(Term, Dic, Reg, Ctl, UpDics, Ent, Ch, SC)+(Base=0)
:-
	Term = integer(I), 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, UpDics = UpH\UpT, SC = {L,R} :
	Reg = I, Ent = Term, DicO = DicI, CtlH = CtlT, UpH = UpT, L = R, 
	Ch = _, Base = _ ;

	Term = string(I), 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, UpDics = UpH\UpT, SC = {L,R} :
	Reg = I, Ent = Term, DicO = DicI, CtlH = CtlT, UpH = UpT, L = R, 
	Ch = _, Base = _ ;

	Term = nil([]), 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, UpDics = UpH\UpT, SC = {L,R} :
	Reg = [], Ent = Term, DicO = DicI, CtlH = CtlT, UpH = UpT, L = R, 
	Ch = _, Base = _;

	Term = real(I), 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, UpDics = UpH\UpT, SC = {L,R} :
	Reg = I, Ent = Term, DicO = DicI, CtlH = CtlT, UpH = UpT, L = R, 
	Ch = _, Base = _ ;

	Term = variable('_'), 
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R}, UpDics = UpH\UpT :
	CtlH = CtlT, DicI = DicO, L = R, Reg = {'_'}, UpH = UpT,
	Ent = error, Ch = _, Base = _ ;

	Term = psi(P), Dic = {DicI,_} :
	write_channel(look(P,Entry,DicI),Ch) |
	variable(P, Entry, Base, Dic, Reg, Ent, Ctl, UpDics, Ch, SC);

	Term = ro(psi(Ro)), Dic = {DicI,_} :	
        write_channel(look(ro(Ro),EntryR,DicI),Ch), Ent = ref('*'), Base = _ |
	ro_variable(Ro, EntryR, Dic, Reg, Ctl, UpDics, Ch, SC);

	Term = tuple(Tup), arity(Tup,Ar) : Ent = {Reg,tuple,Ar}, Base = _ |
	ground_check#check_ground(Term, CtlTerm, {ground,Ground}),
	tell_term(Ground, Term, CtlTerm, Dic, Reg, Ctl, UpDics, Ch, SC) ;

	Term = list(_) : Ent = {Reg,list,'*'}, Base = _ |
	ground_check#check_ground(Term, CtlTerm, {ground,Ground}),
	tell_term(Ground, Term, CtlTerm, Dic, Reg, Ctl, UpDics, Ch, SC).

tell_term(Ground, Term, CtlTerm, Dic, Reg, Ctl, UpDics, Ch, SC)
:-
	Ground = ground, Ctl = CtlH\CtlT, UpDics = UpH\UpT,	SC = {L,R} :
	write_channel(get_reg(Reg,Dic),Ch),
	CtlH = [fetch(CtlTerm,Reg)|CtlT], UpH = UpT, L = R, Term = _ ;

	Ground = not_ground, Term = list(_), Dic = {DicI,DicO}, Ctl = CtlH\CtlT :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch), CtlTerm = _ |
	not_grounded_list(Term, Reg, {DicM,DicO}, Args,
			CtlH\[allocate_list(Args,Reg)|CtlT], UpDics,
			Ch, SC) ;

        Ground = not_ground, Term = tuple(Tuple), 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch), CtlTerm = _ |
            arity(Tuple,Length),
	    not_grounded_tuple(Tuple, Length, Reg, {DicM,DicO}, Args,
			CtlH\[allocate_tuple(Args,Reg)|CtlT], UpDics,
			Ch, SC).

not_grounded_list(List, ListOut, Dic, Args, Ctl, UpDics, Ch, SC)+(N=0)
:-
	List = list([Car|List']), 
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, UpDics = UpH\UpT,	SC = {L,R} :
	Args ! ArgN, 
	Ctl' = CtlM\CtlT, Dic' = {DicI',DicO}, UpDics' = UpM\UpT, SC' = {M,R} |
	N' := N + 1,
	car_creation(Car, {DicI, DicI'}, ArgN, CtlH\CtlM, UpH\UpM, Ch, {L,M}),
	not_grounded_list ;

	List =\= list(_) : Args = ArgN |
	term_creation(List, Dic, ArgN, Ctl, UpDics, _, Ch, SC, {ListOut,N}). 

not_grounded_tuple(Term, Length, TermOut, Dic, Args, Ctl, UpDics, Ch, SC) + (N=1)
:-
	N =< Length, 
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, UpDics = UpH\UpT,	SC = {L,R} :
	Args ! ArgN, 
	Dic' = {DicI',DicO}, UpDics' = UpM\UpT,	SC' = {M,R}, Ctl' = CtlM\CtlT |
	arg(N,Term,TermN), N' := N + 1,
	term_creation(TermN, {DicI,DicI'}, ArgN, CtlH\CtlM, UpH\UpM, _, Ch, 
	                                                    {L,M}, {TermOut,N}),
	not_grounded_tuple ;

	N > Length, 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, UpDics = UpH\UpT, SC = {L,R} :
	Args = [] , DicO = DicI, CtlH = CtlT, UpH = UpT, L = R,
	Term = _, TermOut = _, Ch = _.

variable(Term, Entry, Base, Dic, Reg, Ent, Ctl, UpDics, Ch, SC)
:-
	Entry = {_,_,new,_},  Base =\= 0,
	UpDics = UpH\UpT, Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = {'_'}, Ent = error,
	write_channel(add(Term,{Base,'*',var,'*'},Dic),Ch),
	CtlH = CtlT, L = R, UpH = [{Term,Base,sub_arg}|UpT] ;

	Entry = {_,_,new,_},  Base = 0, Dic = {DicI,DicO},
	UpDics = UpH\UpT, Ctl = CtlH\CtlT, SC = {L,R} :
	Ent = error,
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [allocate_var(Reg)|CtlT],
	write_channel(add(Term,{Reg,'*',var,'*'},{DicM,DicO}),Ch),
	L = R, UpH = [{Term,Reg,ref}|UpT] ;

	Entry = {Areg,_,ref,_},
	Dic = {DicI,DicO}, UpDics = UpH\UpT, Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = Areg, Ent = ref('*'),
	CtlH = CtlT, DicI = DicO, UpH = UpT, L = R, 
	Term = _, Base = _, Ch = _ ;

	Entry = {Areg,_,car,_}, Dic = {DicI,DicO},
	UpDics = UpH\UpT, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [load_car(Areg,Reg)|CtlT],
	UpH = UpT, L = R, Base = _, Ent = ref('*'),	
	write_channel(update(Term,{Reg,'*',ref,'*'},{DicM,DicO}),Ch) ;

	Entry = {Areg,_,car(Type),Val}, Type =\= var, Dic = {DicI,DicO},
	UpDics = UpH\UpT, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [load_car(Areg,Reg)|CtlT],
	UpH = UpT, L = R, Base = _,
	Ent = {Reg,Type,Val},	
	write_channel(update(Term,{Reg,'*',Type,Val},{DicM,DicO}),Ch) ;

	Entry = {Areg,_,car(Type),Val}, Type = var, Dic = {DicI,DicO},
	UpDics = UpH\UpT, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [deref_car(Areg,Reg)|CtlT],
	UpH = [{Term,ref}|UpT], L = R, Base = _,
	Ent = {Reg,ref,'*'},	
	write_channel(update(Term,{Reg,'*',Type,Val},{DicM,DicO}),Ch) ;

	Entry = {Areg,_,sub_arg,_},
	Dic = {DicI,DicO}, UpDics = UpH\UpT, Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = {'&',Areg}, Ent = ref('*'),
	CtlH = CtlT, UpH = UpT,	L = R, DicI = DicO,
	Base = _, Term = _, Ch = _ ;

	Entry = {Areg,_,deref,_},
	Dic = {DicI,DicO}, UpDics = UpH\UpT, 	Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = Areg, Ent = ref('*'), 
	CtlH = CtlT, L = R, DicO = DicI, UpH = [{Term,ref}|UpT], 
	Base = _, Ch = _ ;

	Entry = {Areg,_,list,_}, Areg = a(_), 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT :
	Reg = Areg, Ent = {Areg,list,'*'}, DicI = DicO, CtlH = CtlT, 
	Ch = _, Base = _ |
        get_unsafe(Term, 2, DicI, UpDics, SC) ;

	Entry = {Areg,_,list,_}, Areg = {a(_),I}, I > 0, 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT :
	Reg = {'&',Areg}, Ent = {list,'*'}, DicI = DicO,  CtlH = CtlT, 
	Ch = _, Base = _ |
        get_unsafe(Term, 2, DicI, UpDics, SC) ;

	Entry = {Areg,_,list,_}, Areg = {A,-1}, A = a(_),
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [decrement_pointer(A,Reg)|CtlT],
	write_channel(update(Term,{Reg,'*',list,'*'},{DicM,DicO}),Ch),
	Ent = {Reg,list,'*'}, Base = _ |
        get_unsafe(Term, 2, DicI, UpDics, SC) ;

	Entry = {Areg,_,tuple,Ar}, Ar =\= '*', Areg = a(_),
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT :
	Reg = Areg, Ent = {tuple,Ar}, DicI = DicO, CtlH = CtlT,  
	Base = _, Ch = _ |
        get_unsafe(Term, Ar, DicI, UpDics, SC) ;

	Entry = {Areg,_,tuple,Ar}, Ar =\= '*', Areg = {a(_),_},
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT :
	Reg = {'&',Areg}, Ent = {tuple,Ar}, DicI = DicO, CtlH = CtlT,  
	Ch = _, Base = _ |
        get_unsafe(Term, Ar, DicI, UpDics, SC) ;

	Entry = {Areg,'*',integer,I}, 
	Dic = {DicI,DicO}, UpDics = UpH\UpT,	Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = Areg, Ent = {Areg,integer,I},
	DicI = DicO, UpH = UpT, CtlH = CtlT, L = R,
	Term = _, Base = _, Ch = _ ;

	Entry = {_,Areg,integer,I}, Areg = a(_), 
	Dic = {DicI,DicO}, UpDics = UpH\UpT,	Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = Areg, Ent = {Areg,integer,I},
	DicI = DicO, UpH = UpT, CtlH = CtlT, L = R,
	Term = _, Base = _, Ch = _ ;

	Entry = {Areg,_,cdr,_}, Areg = a(_),
	Dic = {DicI,DicO}, UpDics = UpH\UpT, Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = Areg, Ent = ref('*'),
	CtlH = CtlT, UpH = UpT,	L = R, DicI = DicO,	
	Term = _, Base = _, Ch = _ ;

	Entry = {Areg,_,var,_}, Areg = a(_),
	UpDics = UpH\UpT, Dic = {DicI,DicO},	Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = Areg, Ent = ref('*'),
	DicO = DicI, UpH = [{Term,ref}|UpT], CtlH = CtlT, L = R, 
	Ch = _, Base = _ ;

	Entry = {Areg,_,var,_}, Areg = {a(_),_},
	UpDics = UpH\UpT, Dic = {DicI,DicO},	Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = {'&',Areg}, Ent = ref('*'),
	DicO = DicI, UpH = [{Term,ref}|UpT], CtlH = CtlT, L = R, 
	Ch = _, Base = _;

	Entry = {Areg,_,ref(var),_}, Areg = a(_),
	UpDics = UpH\UpT, Dic = {DicI,DicO},	Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = Areg, Ent = ref('*'),
	DicO = DicI, UpH = [{Term,ref}|UpT], CtlH = CtlT, L = R, 
	Ch = _, Base = _ ;

	Entry = {Areg,_,ref(var),_}, Areg = {a(_),_},
	UpDics = UpH\UpT, Dic = {DicI,DicO},	Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = {'&',Areg}, Ent = ref('*'),
	DicO = DicI, UpH = [{Term,ref}|UpT], CtlH = CtlT, L = R, 
	Ch = _, Base = _;

	otherwise, Entry = {Areg,_,Tp,Vl}, Areg = a(_),
	Dic = {DicI,DicO}, UpDics = UpH\UpT,	Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = Areg, Ent = {Tp,Vl},
	DicI = DicO, UpH = UpT, CtlH = CtlT, L = R,
	Term = _, Base = _, Ch = _ ;

	otherwise, Entry = {Areg,_,Tp,Vl}, Areg = {a(_),_},
	Dic = {DicI,DicO}, UpDics = UpH\UpT,	Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = {'&',Areg}, Ent = {Tp,Vl},
	DicI = DicO, UpH = UpT, CtlH = CtlT, L = R,
	Term = _, Base = _, Ch = _ .

ro_variable(Ro, Entry, Dic, RoOut, Ctl, UpDics, Ch, SC)
:-
	Entry = {_,_,new,_}, Dic = {DicI,DicO}, UpDics = UpH\UpT :
	RoOut = ro(VarOut), UpH = UpT,
	write_channel(add(ro(Ro),{ro(VarOut),'*',ref,'*'},{DicI,DicM}),Ch) |
	unsafe_term_creation#term_creation(psi(Ro), {DicM,DicO}, VarOut, Ctl, 
	                                                           Ch, SC) ;

	Entry = {Areg,'*',ref,_},
	Dic = {DicI,DicO}, UpDics = UpH\UpT,	Ctl = CtlH\CtlT, SC = {L,R} :
	RoOut = Areg, DicO = DicI, UpH = UpT,
	CtlH = CtlT, L = R, 	Ro = _, Ch = _.

car_creation(Car, Dic, Arg, Ctl, UpDics, Ch, SC)
:-
	Car = psi(Psi), Dic = {DicI,_} :
	write_channel(look(Psi,Entry,DicI), Ch) |
	car_variable(Psi, Entry, Dic, Arg, Ctl, UpDics, Ch, SC);

	Car =\= psi(_) |
        term_creation(Car, Dic, Arg, Ctl, UpDics, _, Ch, SC, 0).

car_variable(Var, Entry, Dic, VarOut, Ctl, UpDics, Ch, SC)
:-
	Entry = {Areg,_,car,_},	
	Dic = {DicI,DicO}, UpDics = UpH\UpT, Ctl = CtlH\CtlT, SC = {L,R} :
	VarOut = {Areg,0}, 
	CtlH = CtlT, UpH = UpT, L = R, DicO = DicI, Var = _, Ch = _ ;

	Entry = {Areg,_,car(var),_}, 
	Dic = {DicI,DicO}, UpDics = UpH\UpT, Ctl = CtlH\CtlT, SC = {L,R} :
	VarOut = {Areg,0}, CtlH = CtlT, L = R, DicO = DicI, 
	UpH = [{Var,car}|UpT], Ch = _ ;

	Entry = {Areg,_,car(list),_}, Dic = {DicI,DicO}, Ctl = CtlH\CtlT :
	VarOut = {Areg,0}, CtlH = CtlT, DicO = DicI, Ch = _ |
	get_unsafe(Var, 2, DicI, UpDics, SC) ;

	Entry = {Areg,_,car(tuple),Ar}, Ar =\= '*', 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT :
	VarOut = {Areg,0}, DicO = DicI, CtlH = CtlT, Ch = _ |
	get_unsafe(Var, Ar, DicI, UpDics, SC) ;

	Entry = {Areg,_,car(tuple),'*'}, 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, UpDics = UpH\UpT, SC = {L,R} :
	VarOut = {Areg,0}, CtlH = CtlT, L = R, DicO = DicI, UpH = UpT,
	Var = _, Ch = _ ;

	otherwise |
        variable(Var, Entry, 0, Dic, VarOut, _, Ctl, UpDics, Ch, SC).

get_unsafe(Psi, Ar, Dic, UpDics, SC)
:-
	Ar > 0, SC = {L,R}, UpDics = UpH\UpT :
        SC' = {M,R}, UpDics' = UpM\UpT |
        phase2_dic#look_child(Psi,Ar,Dic,Entry,Key),
        update_unsafe_var(Key, Entry, Dic, UpH\UpM, {L,M}),
	Ar' := Ar - 1,
	get_unsafe ;

	Ar = 0, UpDics = UpH\UpT, SC = {L,R} :
        R = L, UpH = UpT, Psi = _, Dic = _.

update_unsafe_var(Psi, Entry, Dic, UpDics, SC)
:-
	Entry = {a(_),_,var,_}, UpDics = UpH\UpT, SC = {L,R} :
        UpH = [{Psi,ref}|UpT], R = L, Dic = _ ;

	Entry = {{a(_),_},_,var,_}, UpDics = UpH\UpT, SC = {L,R} :
        UpH = [{Psi,sub_arg}|UpT], R = L, Dic = _ ;

	Entry = {a(_),_,ref(var),_}, UpDics = UpH\UpT, SC = {L,R} :
        UpH = [{Psi,ref}|UpT], R = L, Dic = _ ;

	Entry = {{a(_),_},_,ref(var),_}, UpDics = UpH\UpT, SC = {L,R} :
        UpH = [{Psi,sub_arg}|UpT], R = L, Dic = _ ;

	Entry = {_,_,tuple,Ar}, Ar =\= '*' |
        get_unsafe(Psi, Ar, Dic, UpDics, SC) ;

	Entry = {_,_,list,_} |
        get_unsafe(Psi, 2, Dic, UpDics, SC) ;

	otherwise, UpDics = UpH\UpT, SC = {L,R} :
        UpH = UpT, R = L, Psi = _, Entry = _, Dic = _.

	
