/* $Header: /home/qiana/Repository/Logix/system/ndg/phase1/indexing_function.cp,v 1.1 1999/07/09 07:03:07 bill Exp $ */
-export([indexing_function/4]).
-language(compound).
-mode(trust).

procedure indexing_function(Cp, IxAlg, Ix, Ch).

indexing_function(Cp, IxAlg, Ix, Ch) 
:-
	Cp =\= [], IxAlg = plain |
             get_guards(Cp, Gs, Ch),
	     get_index(Gs, Ix) ;

	Cp =\= [], IxAlg = maxcares |
             get_guards(Cp, Gs, Ch),
	     sort(Gs, Gs1, maxcare),
	     get_index(Gs1, Ix) ;

	Cp =\= [], IxAlg = minvariability |
	     get_guards(Cp, Gs, Ch),
%	     screen#display({Cp,Gs1},type(ground)),
	     sort(Gs, Gs1, minvariability),
	     get_index(Gs1, Ix).

get_guards(Clss, Gs, Ch)+(GsAcc=[])
:-
	Clss ? Cls, Cls = {ClsId,{Ask,_,_}} |
	     get_cls_guards(Ask, ClsId, Cls, GsAcc, GsAcc', Ch),
	     self ;
	
	Clss = [] : Gs = GsAcc, Ch = _.

get_cls_guards(Ask, ClsId, Cls, GsAcc, GsAccOut, Ch)
:-
	Ask ? psi(Psi) = T, T =\= psi(_) | 
	    add_psi(psi(Psi), T, ClsId, Cls, GsAcc, GsAcc'),
	    get_cls_guards ;

	Ask ? T = psi(Psi), T =\= psi(_) | 
	    add_psi(psi(Psi), T, ClsId, Cls, GsAcc, GsAcc'),
	    get_cls_guards ;

	Ask = [] : GsAccOut = GsAcc, ClsId = _, Cls = _, Ch = _ ;

	otherwise, Ask ? G :
	write_channel(connect(G,Check,Conn),Ch) |
	    guard_connected(Check, G, ok, St),
	    add_guard(St, ClsId, Conn, Check, G, GsAcc, GsAcc', Ch),
	    self.

add_psi(Psi, T, ClsId, Cls, GsAcc, GsAccOut)
:-
	GsAcc ? G, G =\= {{value(Psi),_},_,_,_} : GsAccOut ! G | add_psi ;

	GsAcc ? {{value(Psi),Us},Vr,CsN,Cs} :
	GsAccOut = [{{value(Psi),Us1},Vr1,CsN1,Cs1}|GsAcc'] |
	      get_type(T,T1),
	      add_type(T1, Cls, Us, Vr, Us1, Vr1),
	      add_mem(ClsId, Cs, CsN, Cs1, CsN1);

	GsAcc = [] : 
        GsAccOut = [{{value(Psi),[{T1,[Cls]}]},3,1,[ClsId]}] |
	      get_type(T,T1).

get_type(T,T1)
:-
	T = integer(_) : T1 = T ;
	T = string(_) : T1 = T ;
	T = nil(_) : T1 = nil ;
	T = real(_) : T1 = T ;
	T = list(_) : T1 = list ;
	T = tuple(Tu), arity(Tu,Ar) : T1 = tuple/Ar.

add_type(T, Cls, Us, Vr, Us1, Vr1)
:-
	Us ? {T,Cs} : Us1 = [{T,[Cls|Cs]}|Us'], Vr1 = Vr ;
	Us ? U, U = {T1,_}, T1 =\= T : Us1 ! U | add_type ;
	Us = [] : Us1 = [{T,[Cls]}] | Vr1 := Vr + 1.

add_mem(T, Cs, CsN, Cs1, CsN1) 
:-
	Cs ? T : Cs1 = [T|Cs'], CsN1 = CsN ;
	Cs ? T1, T =\= T1 : Cs1 ! T1 | add_mem ;
	Cs = [] : Cs1 = [T] | CsN1 := CsN + 1.

guard_connected(Check, G, St, StO)
:-
	Check ? Ar,
	arg(Ar,G,Arg) |
	   connected_arg(Arg, St, St'),
	   self ;

	Check = [] : StO = St, G = _.

connected_arg(Arg, St, StOut)
:-
	Arg = integer(_) : StOut = St ;
	Arg = string(_) : StOut = St ;
	Arg = nil(_) : StOut = St ;
	Arg = real(_) : StOut = St ;
	Arg = variable('_') : StOut = St ;
	Arg = psi(_) : StOut = St ;
	Arg = tuple(Tuple) |
              arity(Tuple,Ar), 
	      connected_tuple(Tuple, Ar, St, StOut) ;
	Arg = list([ArgCar|ArgCdr]) |
	      connected_arg(ArgCar, St, Sta),
	      connected_arg(ArgCdr, Sta, StOut) ;
        Arg = variable(V), V =\= '_' : StOut = no, St = _.

connected_tuple(Tuple, Ar, St, StOut)
:-
	Ar-- > 0,
	arg(Ar,Tuple,Arg) |
	     connected_arg(Arg, St, St'),
	     self ;

        Ar = 0 : St = StOut, Tuple = _.

add_guard(St, ClsId, Conn, Check, G, GsAcc, GsAccOut, Ch)
:-
	St = ok | 
	     add_guard1(0, ClsId, Conn, Check, G, GsAcc, GsAccOut, Ch) ;

	St = no : GsAcc = GsAccOut, 
		  ClsId = _, Conn = _, Check = _, G = _, Ch = _.

add_guard1(St, ClsId, Conn, Check, G, GsAcc, GsAccOut, Ch)
:-
	St = 0, GsAcc ? G1, G1 = {{value(_),_},_,_,_} :
	GsAccOut ! G1 | 
	     self ;

	St = 0, GsAcc ? {G1,2,CsN,Cs}, G1 = {G2,Cn,Ck} :
	write_channel(dominate(G2,G,St'),Ch),
	GsAccOut ! {{GO,Cn,Ck},2,CsN1,Cs1} |
	     add_cls(St', G2, GO, ClsId, Cs, CsN, Cs1, CsN1),
	     self ; 

	St =\= 0 : GsAcc = GsAccOut,
	           Check = _, ClsId = _, Conn = _, G = _, Ch = _ ;

        GsAcc = [], St = 0 : 
        GsAccOut = [{{G,Conn,Check},2,1,[ClsId]}], Ch = _ .

add_cls(St, GOld, G, ClsId, Cs, CsN, Cs1, CsN1)
:-
	St =\= 0 : G = St, GOld = _ | add_mem(ClsId, Cs, CsN, Cs1, CsN1) ;
        St = 0 : G = GOld, Cs1 = Cs, CsN1 = CsN, ClsId = _.

sort(Gs, GsOut, Order)
:-
	pair(Gs, In, Order),
	msort1(In, GsOut, Order).

msort1(Gs, GsOut, Order)
:-
	Gs = [] : GsOut = [], Order = _ ;
	Gs = [X] : GsOut = X, Order = _ ;
	otherwise | msort(Gs, Gs', Order), msort1.

pair(Gs, GsOut, Order)
:-
	Order = maxcare, Gs ? {P,V,N,Cs}, Gs' ? {P1,V1,N1,Cs1}, N < N1 :
	GsOut ! [{P1,V1,N1,Cs1},{P,V,N,Cs}] | pair ;

	Order = maxcare, Gs ? {P,V,N,Cs}, Gs' ? {P1,V1,N1,Cs1}, N >= N1 :
	GsOut ! [{P,V,N,Cs},{P1,V1,N1,Cs1}] | pair ;

	Order = minvariability, Gs ? {P,V,N,Cs}, Gs' ? {P1,V1,N1,Cs1}, N < N1 :
	GsOut ! [{P1,V1,N1,Cs1},{P,V,N,Cs}] | pair ;

	Order = minvariability, Gs ? {P,V,N,Cs}, Gs' ? {P1,V1,N1,Cs1}, N > N1 :
	GsOut ! [{P,V,N,Cs},{P1,V1,N1,Cs1}] | pair ;

	Order = minvariability, Gs ? {P,V,N,Cs}, Gs' ? {P1,V1,N,Cs1},V =< V1 :
	GsOut ! [{P,V,N,Cs},{P1,V1,N,Cs1}] | pair ;

	Order = minvariability, Gs ? {P,V,N,Cs}, Gs' ? {P1,V1,N,Cs1}, V > V1 :
	GsOut ! [{P1,V1,N,Cs1},{P,V,N,Cs}] | pair ;

	Gs = [X] : GsOut = [[X]], Order = _ ;

	Gs = [] : GsOut = [], Order = _.

msort(Gs, GsOut, Order)
:-
	Gs ? Xs1, Gs' ? Xs2 : GsOut = [Ys|GsOut'] |
           merge(Xs1,Xs2,Ys,Order), msort ;
	
	Gs = [Xs] : GsOut = [Xs], Order = _ ;

	Gs = [] : GsOut = [], Order = _ .

merge(Xs, Ys, Zs, Order)
:-
	Order = maxcare, Xs ? {P,V,N,Cs}, Ys = [{_,_,N1,_}|_], N >= N1 : 
	Zs ! {P,V,N,Cs} |
	      merge ;

	Order = maxcare, Xs = [{_,_,N,_}|_], Ys ? {P1,V1,N1,Cs1}, N < N1 : 
	Zs ! {P1,V1,N1,Cs1} |
	      merge ;

	Order = minvariability, Xs ? {P,V,N,Cs}, Ys = [{_,_,N1,_}|_], 
	N > N1 : 
	Zs ! {P,V,N,Cs} |
	      merge ;

	Order = minvariability, Xs = [{_,_,N,_}|_], Ys ? {P1,V1,N1,Cs1}, 
	N < N1 : 
	Zs ! {P1,V1,N1,Cs1} |
	      merge ;

	Order = minvariability, Xs ? {P,V,N,Cs}, Ys = [{_,V1,N,_}|_],
	V =< V1 :
	Zs ! {P,V,N,Cs} |
	      merge ;

	Order = minvariability, Xs = [{_,V,N,_}|_], Ys ? {P1,V1,N,Cs1},
	V > V1 :
	Zs ! {P1,V1,N,Cs1} |
	      merge ;

        Xs = [] : Zs = Ys, Order = _ ;

	Ys = [] : Zs = Xs, Order = _ .

get_index(Gs, Ix)
:-
	Gs = [{{value(Psi),Us},_,_,ClsIds}|_] |
	consistent(Us, St, Cls),
	gen_index(St, Psi, Us, ClsIds, Cls, Ix) ;

	Gs = [{GuardCC,_,_,_}|_], GuardCC =\= {value(_),_} : Ix = GuardCC ;

	Gs = [] : Ix = [].

gen_index(St, Psi, Us, ClsIds, Cls, Ix)
:-
	St = consistent :
        Ix = {value(Psi),Us,ClsIds}, Cls = _;
	
	St = inconsistent : ClsIds = _, Ix = _, Psi = _, Us = _ |
		fail(Cls,' contains inconsistent tests').

consistent(Us, St, Cls)+(ClsIds=[]) 
:-
	Us ? {T,Clss} |
	insert_clss(T, Clss, ClsIds, ClsIds'), 
	consistent;

	Us = [] : Cls = _ | check_consistent(ClsIds, St, Cls).

insert_clss(T, Clss, ClsIds, ClsIdsO)
:-
	Clss ? {Id,Cls} |
	insert_cls(T, Id, Cls, ClsIds, ClsIds'),
	insert_clss ;
	
	Clss = [] : ClsIdsO = ClsIds, T = _.

insert_cls(T, Id, Cls, ClsIds, ClsIdsO)
:-
	ClsIds ? C, C = {_,Id1,_}, Id > Id1 : ClsIdsO ! C | insert_cls;

	ClsIds ? C, C = {_,Id1,_}, Id =< Id1 : 
	ClsIdsO = [{T,Id,Cls},C|ClsIds'] ;

	ClsIds = [] : ClsIdsO = [{T,Id,Cls}].

check_consistent(ClsIds, St, Cls)
:-
	ClsIds = [{T,Id,Cls1},{T1,Id,_}|_], T =\= T1 : 
	St = inconsistent, Cls = {Id,Cls1} ;

	ClsIds ? {T,Id,_}, ClsIds' = [{T,Id,_}|_] | check_consistent ;
	
	ClsIds ? {_,Id,_}, ClsIds' = [{_,Id1,_}|_], Id =\= Id1 | 
	check_consistent ;

	ClsIds = [_] : St = consistent, Cls = _ ;
	
	ClsIds = [] : St = consistent, Cls = _.
