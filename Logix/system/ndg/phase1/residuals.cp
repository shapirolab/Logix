/* $Header: /home/qiana/Repository/Logix/system/ndg/phase1/residuals.cp,v 1.1 1999/07/09 07:03:07 bill Exp $ */
-export([residuals/4]).
-language(compound).
-mode(trust).

procedure residuals(Psi, Us, SubTrsL, Cs).

residuals(Psi, Us, SubTrsL, Cs)
:-
	Us ? {Type, Clss}, SubTrsL = STsH\STsT :
        STsH ! {Type1,ST'}, Cs ! {Residuals,ST'}, SubTrsL' = STsH'\STsT |
	   change_type(Type, Type1),
           clss_residuals(Psi, Type, Clss, Residuals),
	   self ;

	Us = [], SubTrsL = STsH\STsT : STsH = STsT, Cs = [], Psi = _.

change_type(Type, Type1)
:-
	Type = tuple/Ar : Type1 = tuple(Ar);
	Type =\= tuple/_ : Type1 = Type.

clss_residuals(Psi, Type, Clss, Residuals)
:-
	Clss ? Cls : Residuals ! Residual |
             cls_residual(Psi, Type, Cls, Residual),
	     self ;

	Clss = [] : Residuals = [], Psi = _, Type = _.

cls_residual(Psi, Type, Cls, Residual)
:-
	Cls = {ClsId,{Ask,Tell,Body}} : Residual = {ClsId,Res1} |
            get_ask(Psi, Type, Ask, Ask1, U),
            replace_unification(Psi, U, Ask11\Ask1, Subs),
	    variable_unification(Subs, Ask12\Ask11, Subs'),
	    delete_type_check(Psi, Type, Ask12, Ask13),
	    substitute#substitute(Subs', {Ask13,Tell,Body}, Res1).

delete_type_check(Psi, Type, AskI, AskO)
:-
	AskI ? number(Psi), Type = integer(_) | self ;
	
	AskI ? number(Psi), Type = real(_) | self ;

	AskI ? T, T = number(Psi), Type =\= integer(_), Type =\= real(_) :
		AskO ! 	T | self ;

	AskI ? T, T =\= number(Psi) : AskO ! T | self ;

	AskI = [] : AskO = [], Psi = _, Type = _ .

get_ask(Psi, Type, Ask, Ask1, U) 
:-
	Ask ? Psi = T, T =\= psi(_), T = Type : Ask1 = Ask', U = T ;

	Ask ? T = Psi, T =\= psi(_), T = Type : Ask1 = Ask', U = T ;

	Ask ? Psi = nil(_), Type = nil : Ask1 = Ask', U = nil([]) ;

	Ask ? nil(_) = Psi, Type = nil : Ask1 = Ask', U = nil([]) ;

	Ask ? Psi = T, T = tuple(Tup), arity(Tup,Ar), Type = tuple/Ar : 
        Ask1 = Ask', U = T ;

	Ask ? T = Psi, T = tuple(Tup), arity(Tup,Ar), Type = tuple/Ar : 
        Ask1 = Ask', U = T ;
	
	Ask ? Psi = T, T = list(_), Type = list : Ask1 = Ask', U = T ;

	Ask ? T = Psi, T = list(_), Type = list : Ask1 = Ask', U = T ;
	
	otherwise, Ask ? A : Ask1 ! A | self.

replace_unification(Psi, U, AskL, Subs)
:-
	U = integer(_), AskL = AskH\AskT : Subs = [], AskH = AskT, Psi = _ ;

	U = string(_), AskL = AskH\AskT : Subs = [], AskH = AskT, Psi = _ ;

	U = nil(_), AskL = AskH\AskT : Subs = [], AskH = AskT, Psi = _ ;

	U = real(_), AskL = AskH\AskT : Subs = [], AskH = AskT, Psi = _ ;

        U = list([CarU|CdrU]), AskL = AskH\AskT, Psi = psi(List) |
/*
            replace_unification_arg(CarU, psi([1|List]), 
						AskH\AskM, Subs, Subs'),
            replace_unification_arg(CdrU, psi([2|List]), 
						AskM\AskT, Subs', []) ;
*/
            append(List,[1],CarPsi),
	    append(List,[2],CdrPsi),
            replace_unification_arg(CarU, psi(CarPsi), AskH\AskM, Subs, Subs'),
            replace_unification_arg(CdrU, psi(CdrPsi), AskM\AskT, Subs', []) ;

        U = tuple(Tup) |
            arity(Tup,Ar),
            replace_tup_unification(Psi, Tup, Ar, AskL, Subs, []).
	
replace_tup_unification(Psi, Tup, Ar, AskL, Subs, SubsT)+(N=1) 
:-
	N++ =< Ar, AskL = AskH\AskT, Psi = psi(List),
	arg(N,Tup,TupN) :
	AskL' = AskM\AskT |
	    append(List,[N],PsiN), 
	    replace_unification_arg(TupN, psi(PsiN), AskH\AskM, Subs, Subs'),
/*	    
	    replace_unification_arg(TupN, psi([N|List]), 
						AskH\AskM, Subs, Subs'),
*/
	    self ;

	N > Ar, AskL = H\T : H = T, Subs = SubsT, Psi = _, Tup = _, N = _ .

replace_unification_arg(Term, Psi, AskL, Subs, SubsT)
:-
	Term = variable(X), X =\= '_', AskL = AskH\AskT :
	AskH = AskT, Subs = [{Term,Psi}|SubsT] ;

	Term = variable('_'), AskL = AskH\AskT :
	AskH = AskT, Subs = SubsT, Psi = _ ;

	Term =\= variable(_), AskL = AskH\AskT :
	AskH = [Psi = Term |AskT], Subs = SubsT.

variable_unification(Subs, AskL, SubsO)
:-
	Subs ? {Name,Psi}, AskL = AskH\AskT : AskL' = AskM\AskT |
	     member_subs(Name, Psi, Subs', AskH\AskM, SubsO, SubsO'),
	     self ;

	Subs = [], AskL = AskH\AskT : SubsO = [], AskH = AskT.

member_subs(Name, Psi, Subs, Ask, SubsO, SubsO1)
:-
	Subs = [{Name,Psi1}|_], Ask = AskH\AskT : 
        AskH = [Psi = Psi1 | AskT], SubsO = SubsO1 ;
        
        Subs  ? {Name1,_}, Name1 =\= Name | self ;

	Subs = [], Ask = AskH\AskT : AskH = AskT, SubsO = [{Name,Psi}|SubsO1].

append(Xs, Ys, Zs) 
:-
	Xs ? X : Zs ! X | self ;
	Xs = [] : Zs = Ys.

