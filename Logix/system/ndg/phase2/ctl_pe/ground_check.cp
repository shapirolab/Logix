/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/ground_check.cp,v 1.1 1999/07/09 07:03:04 bill Exp $ */
-language(compound).
-export([check_ground/3,
	get_vars/2,
	check_arg/2,
	check_arg/3,
	get_multiple_vars/2,
	allocate_vars/5,
	allocate_multiple_vars/5]).
-mode(trust).

%%%%%%	check_ground

check_ground(Term, CtlTerm, Ground)
:-
        Term = integer(I), Ground = {L,R} : CtlTerm = I, L = R ;

        Term = string(S), Ground = {L,R} : CtlTerm = S, L = R ;

        Term = nil(_), Ground = {L,R} : CtlTerm = [], L = R ;

        Term = real(I), Ground = {L,R} : CtlTerm = I, L = R ;

        Term = tuple(Tuple), 
	N := arity(Tuple), make_tuple(N, CtlTuple) :
	   CtlTerm = CtlTuple |
	     tuple_ground(Tuple, N, CtlTuple, Ground) ;

        Term = list([Car|Cdr]), Ground = {L,R} : CtlTerm = [CtlCar|CtlCdr] |
             check_ground(Car, CtlCar, {L,M}),
	     check_ground(Cdr, CtlCdr, {M,R}) ;

        Term = psi(_), Ground = {_L,R} : R = not_ground, CtlTerm = _ ;

        Term = ro(_), Ground = {_L,R} : R = not_ground, CtlTerm = _ ;

        Term = variable('_'), Ground = {_L,R} : R = not_ground, CtlTerm = _.

tuple_ground(Term, N, CtlTerm, Ground)
:-
	N-- > 0, Ground = {L,R},
	arg(N,Term,TermN), arg(N,CtlTerm,CtlTermN) :
	Ground' = {M,R} |
	  check_ground(TermN,CtlTermN,{L,M}),
	  self ;
        N = 0, Ground = {L,R} : L = R, Term = _, CtlTerm = _.


%%%%%%	check_arg
check_arg(ArgN, Vars)+(VarsT=[])
:-
        ArgN = psi(_) : Vars ! ArgN, VarsT = Vars' ;
%        ArgN = psi(X), X =\= variable(_) : Vars = VarsT ;
        ArgN = integer(_) : Vars = VarsT ;
        ArgN = string(_) : Vars = VarsT ;
        ArgN = real(_) : Vars = VarsT ;
        ArgN = nil(_) : Vars = VarsT ;
        ArgN = variable(_) : Vars = VarsT ;
        ArgN = ro(Ro) | check_arg(Ro, Vars, VarsT) ;
        ArgN = tuple(Tuple),
        Ar := arity(Tuple) | get_tuple_vars(Tuple, Ar, Vars, VarsT) ;
        ArgN = list([Car|Cdr]) | 
	     check_arg(Car, Vars, VarsT'),
	     check_arg(Cdr, VarsT', VarsT).

get_tuple_vars(Term, Ar, Vars, VarsT) + (N=1)
:-
	N++ =< Ar, 
        arg(N,Term,ArgN) |
	  check_arg(ArgN, Vars, Vars'),
	  self ;
	N > Ar : Vars = VarsT, Term = _.

%%%%%%	get_multiple_vars

get_multiple_vars(Vars, VarsMul)
:-
	Vars ? V |
	     check_V(V, Vars', Vars'', VarsMul, VarsMul'),
	     self ;
	Vars = [] : VarsMul = [].

check_V(V, VarsIn, VarsOut, VarsMul, VarsMulT)
:-
	VarsIn ? V : VarsMul ! V, VarsMulT = VarsMul' |
	       delete_V(V, VarsIn', VarsOut) ;

	VarsIn ? V1, V1 =\= V : VarsOut ! V1 | check_V ;

	VarsIn = [] : VarsOut = [], VarsMul = VarsMulT, V = _.

delete_V(V, VarsIn, VarsOut)
:-
	VarsIn ? V | delete_V ;
        VarsIn ? V1, V =\= V1 : VarsOut ! V1 | delete_V ;
        VarsIn = [] : VarsOut = [], V = _.

%%%%%%%%%  allocate_multiple_vars

allocate_multiple_vars(Tells, Ctl, Dic, Ch, SC)
:-
	get_vars(Tells, Vars\[]),
	get_multiple_vars(Vars, Aliases),
	allocate_vars(Aliases, Dic, Ctl, Ch, SC).

get_vars(Term, Vars)
:-
	Term ? Tg, arity(Tg,Ar), Vars = VH\VT :
                    Vars' = VH'\VT |
                    get_vars_term(Tg, Ar, VH\VH'),
	            self ;
	Term ? Tg, string(Tg) |	get_vars ;
	Term = list([Car|Cdr]), Vars = VH\VT |
                    check_arg(Car, VH, VH'),
		    check_arg(Cdr, VH', VT) ;
	Term = tuple(Tup), arity(Tup,Ar) |
                    get_vars_term(Tup, Ar, Vars) ;
	Term = psi(_), Vars = H\T : H = [Term|T] ;
	Term = integer(_), Vars = H\T : H = T ;
	Term = string(_), Vars = H\T : H = T ;
	Term = real(_), Vars = H\T : H = T ;
	Term = nil(_), Vars = H\T : H = T ;
	Term = variable(_), Vars = H\T : H = T ;
	Term = ro(Ro), Vars = H\T | check_arg(Ro, H, T);
	Term = [], Vars = VH\VT : VT = VH;
	otherwise, tuple(Term), arity(Term,Ar) |
                    get_vars_term(Term, Ar, Vars).

get_vars_term(Tg, Ar, Vars)
:-
	Ar-- > 1, arg(Ar,Tg,TgAr), Vars = VH\VT :
        Vars' = VH'\VT |
        check_arg(TgAr, VH, VH'),
	self ;
	
	Ar = 1, Vars = VH\VT : VT = VH, Tg = _.

allocate_vars(Vars, Dic, Ctl, Ch, SC)
:-
	Vars ? psi(V), 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	Dic' = {DicM,DicO}, Ctl' = CtlM\CtlT, SC' = {M,R},
	write_channel(look(V,Entry,DicI),Ch) |
	     check_var(V, Entry, {DicI,DicM}, CtlH\CtlM, Ch, {L,M}),
	     self ;

        Vars = [], 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	DicO = DicI, CtlH = CtlT, L = R, Ch = _.

check_var(V, Entry, Dic, Ctl, Ch, SC)
:-
	Entry = {_,_,new,_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [allocate_var(Reg)|CtlT],
	write_channel(add(V,{Reg,'*',ref,'*'},{DicM,DicO}),Ch),
	L = R ;

	Entry = {Reg,_,var,_}, Reg = a(_), Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(update(V,{Reg,'*',ref,'*'},Dic),Ch),
	CtlH = CtlT, L = R ;
	
	Entry = {Reg,_,var,_}, Reg = {a(_),_}, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(update(V,{Reg,'*',sub_arg,'*'},Dic),Ch),
	CtlH = CtlT, L = R ;
	
	Entry = {Reg,_,car(var),_}, Ctl = CtlH\CtlT, SC = {L,R} :
	write_channel(update(V,{Reg,'*',car,'*'},Dic),Ch),
	CtlH = CtlT, L = R ;
	
	otherwise,
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	CtlH = CtlT, DicO = DicI, L = R, Entry = _, V = _, Ch = _.
