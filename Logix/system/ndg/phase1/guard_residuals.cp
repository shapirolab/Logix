/* $Header: /home/qiana/Repository/Logix/system/ndg/phase1/guard_residuals.cp,v 1.1.1.1 1999/07/09 07:03:07 bill Exp $ */
-export([residuals/8]).
-language(compound).
-mode(trust).

procedure residuals(Res, Guard, Conn, ResT, ResF, ResOw, Ch, SC).

residuals(Res, Guard, Conn, ResT, ResF, ResOw, Ch, SC)
:-
	Res ? Cls, Cls = {Id,{Ask,Tell,Body}}, SC = {L,R},
	ResT = TrH\TrT, ResF = FlH\FlT, ResOw = OwH\OwT :
	ResT' = TrM\TrT, ResF' = FlM\FlT, ResOw' = OwM\OwT, SC' = {M,R} |
            check_ask(Guard, Conn, Ask, Ask', Subs, Ch, Ind),
	    substitute#substitute(Subs, {Ask',Tell,Body}, Cls1),
	    add_cls(Ind, {Id,Cls1}, TrH\TrM, FlH\FlM, OwH\OwM, {L,M}),
	    self ;
	
	Res = [], ResT = TrH\TrT, ResF = FlH\FlT, ResOw = OwH\OwT, SC = {L,R} :
	TrH = TrT, FlH = FlT, OwH = OwT, R = L, Ch = _, Conn = _, Guard = _.

add_cls(Ind, Cls, ResT, ResF, ResOw, SC)
:-
	Ind = fail,
	ResT = TrH\TrT, ResF = FlH\FlT, ResOw = OwH\OwT, SC = {L,R} :
	TrH = TrT, FlH = FlT, OwH = OwT, R = L |
        fail(Cls, ' contains inconsistent tests') ;

	Ind = 0, 
	ResT = TrH\TrT, ResF = FlH\FlT, ResOw = OwH\OwT, SC = {L,R} :
	TrH = TrT, FlH = [Cls|FlT], OwH = OwT, R = L ;
        
	Ind = 2,
	ResT = TrH\TrT, ResF = FlH\FlT, ResOw = OwH\OwT, SC = {L,R} :
	TrH = [Cls|TrT], FlH = FlT, OwH = OwT, R = L ;
        
	Ind = [],
	ResT = TrH\TrT, ResF = FlH\FlT, ResOw = OwH\OwT, SC = {L,R} :
	TrH = TrT, FlH = FlT, OwH = [Cls|OwT], R = L.
        
check_ask(G, Conn, Ask, AskO, Subs, Ch, Ind)+(IndI=[],SubsT=[],Count=0)
:-
	Ask ? A,  IndI =\= fail :
        write_channel(relation(G,A,St),Ch) |
	    get_ask(St, G, Conn, A, 
	            AskO, AskO', Subs, Subs', IndI, IndI', Count, Count'),
	    self ;

	IndI = fail : Ind = fail, Ask = AskO, Subs = SubsT,
	G = _, Conn = _, Ch = _, Count = _ ;

	Ask = [], IndI =\= fail : 
        AskO = [], Ind = IndI, Subs= SubsT, G = _, Ch = _, Conn = _, Count = _.

get_ask(St, G, Conn, A, AskO, AskOT, Subs, SubsT, IndI, IndIO, C, CO)
:-
	St = ow : 
        AskO = [A|AskOT], Subs = SubsT, IndIO = IndI, CO = C, G = _, Conn = _ ;

	St =\= ow, 
	Bit2 := (St /\ 2),
	Bit1 := (St /\ 1),
	Bit2 = IndI,
	Bit1 = 0 :
        IndIO = IndI,
        AskO = [A|AskOT],
	Subs = SubsT, C = CO, G = _, Conn = _;

	St =\= ow, A =\= {'=?=',_,_},
	Bit2 := (St /\ 2),
	Bit1 := (St /\ 1),
	Bit2 = IndI,
	Bit1 = 1,
	Conn = [] :
        IndIO = IndI, C = CO |
            get_residual(G, Conn, A, AskO, AskOT, Subs, SubsT) ;

	St =\= ow, A =\= {'=?=',_,_},
	Bit2 := (St /\ 2),
	Bit1 := (St /\ 1),
	Bit2 = IndI,
	Bit1 = 1,
	Conn =\= [], C = 0 :
        IndIO = IndI, CO = 1 |
            get_residual(G, Conn, A, AskO, AskOT, Subs, SubsT) ;

	St =\= ow, A =\= {'=?=',_,_},
	Bit2 := (St /\ 2),
	Bit1 := (St /\ 1),
	Bit2 = IndI,
	Bit1 = 1,
	Conn =\= [], C = 1 :
        IndIO = IndI,
        AskO = [A|AskOT],
	Subs = SubsT, C = CO, G = _, Conn = _;

	St =\= ow, IndI =\= [],
	Bit2 := (St /\ 2),
	Bit2 =\= IndI : 
        IndIO = fail, C = CO,
	G = _, Conn = _, A = _, AskO = _, AskOT = _, Subs = _, SubsT = _;

	St =\= ow, IndI = [],
	Bit2 := (St /\ 2),
	Bit1 := (St /\ 1),
	Bit1 = 0 :
        IndIO = Bit2,
        AskO = [A|AskOT],
	Subs = SubsT, C = CO, G = _, Conn = _;

	St =\= ow, IndI = [], A =\= {'=?=',_,_},
	Bit2 := (St /\ 2),
	Bit1 := (St /\ 1),
	Bit1 = 1, Conn = [] :
        IndIO = Bit2, CO = C |
            get_residual(G, Conn, A, AskO, AskOT, Subs, SubsT) ;

	St =\= ow, IndI = [], A =\= {'=?=',_,_},
	Bit2 := (St /\ 2),
	Bit1 := (St /\ 1),
	Bit1 = 1, Conn =\= [], C = 0  :
        IndIO = Bit2, CO = 1 |
            get_residual(G, Conn, A, AskO, AskOT, Subs, SubsT) ;

	St =\= ow, IndI = [], A =\= {'=?=',_,_},
	Bit2 := (St /\ 2),
	Bit1 := (St /\ 1),
	Bit1 = 1, Conn =\= [], C = 1  :
        IndIO = Bit2,
        AskO = [A|AskOT],
	Subs = SubsT, C = CO, G = _, Conn = _;

	St =\= ow, A = {'=?=',_,Term}, G = {'=?=',_,Term} :
        AskO = AskOT, Subs = SubsT, IndIO = 2, CO = C, IndI = _, Conn = _ ;

	St =\= ow, A = {'=?=',_,TermA}, G = {'=?=',_,TermG}, TermA =\= TermG :
        C = CO, Conn = _ |
            compare_term(TermG, TermA, St'),
	    ask_unify(St', A, TermG, TermA, AskO, AskOT, Subs, SubsT, IndI, IndIO).

get_residual(G, Conn, A, AskO, AskOT, Subs, SubsT)
:-
	Conn ? Ar, 
	arg(Ar,A,V), arg(Ar,G,V1),
	V = variable(X), X =\= '_' : 
	Subs ! {V,V1} |
	     self ;

	Conn ? Ar, arg(Ar,A,V), V = variable('_') | 
             self ;
	
	Conn ? Ar, arg(Ar,A,V), V =\= variable(_),
             arg(Ar,G,V1) :
             AskO ! V1 =?= V |
	     self ;

	Conn = [] : Subs = SubsT, AskO = AskOT, A = _, G = _.

compare_term(TermG, TermA, St)+(Ind=1)
:-
	TermA = TermG : St = Ind ;

	TermA =\= TermG, TermG = psi(_), TermA = variable(_) : St = Ind ;

	TermA =\= TermG, 
	TermG = tuple(TupG), TermA = tuple(TupA), arity(TupG,A), arity(TupA,A) |
            compare_tuple(A, TupG, TupA, Ind, St) ;

	TermA =\= TermG, 
	TermG = list([CarG|CdrG]), TermA = list([CarA|CdrA]) |
        compare_term(CarG, CarA, St, Ind'),
	compare_term(CdrG, CdrA, Ind', Ind) ;

	otherwise : St = ow, TermG = _, TermA = _, Ind = _.

compare_tuple(Ar, TupG, TupA, Ind, IndO)
:-
	Ar-- > 0, arg(Ar, TupG, ArG), arg(Ar, TupA, ArA) |
            compare_term(ArG, ArA, Ind', Ind),
	    self ;

	Ar = 0 : Ind = IndO, TupA = _, TupG = _.

ask_unify(St, A, TermG, TermA, AskO, AskOT, Subs, SubsT, IndI, IndIO)
:-
	St = ow :
        AskO = [A|AskOT], Subs = SubsT, IndIO = IndI, TermG = _, TermA = _ ;

	St = 1 :        AskO = AskOT, IndIO = 2, A = _, IndI = _ |
            get_subs(TermG, TermA, Subs, SubsT).

get_subs(Arg, Arg1, Subs, SubsT)
:-
	Arg = psi(_), Arg1 = variable(V), V =\= '_' : Subs = [{Arg1,Arg}|SubsT];
	Arg = psi(_), Arg1 = variable('_') : Subs = SubsT;

	Arg = tuple(Tuple), Arg1 = tuple(Tuple1), arity(Tuple,Ar), Arg =\=Arg1 |
              get_subs_tup(Ar, Tuple, Tuple1, Subs, SubsT) ;

	Arg = list([ArgCar|ArgCdr]), Arg1 = list([ArgCar1|ArgCdr1]), 
	Arg =\= Arg1 |
	      get_subs(ArgCar, ArgCar1, Subs, Subs'),
	      get_subs(ArgCdr, ArgCdr1, Subs', SubsT) ;

	Arg = Arg1 : Subs = SubsT.

get_subs_tup(Ar, Tuple, Tuple1, Subs, SubsT)
:-
	Ar > 0, arg(Ar,Tuple,ArT), arg(Ar,Tuple1,ArT1) |
              get_subs(ArT, ArT1, Subs, Subs'),
	      Ar' := Ar - 1,
	      self ;

	Ar = 0 : Subs = SubsT, Tuple = _, Tuple1 = _.
