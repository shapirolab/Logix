/* $Header: /home/qiana/Repository/Logix/system/ndg/phase1/compile_node.cp,v 1.1 1999/07/09 07:03:07 bill Exp $ */
-export([compile_node/10]).
-language(compound).
-mode(trust).

procedure compile_node(ProcName, Res, Ix, N, Dg, Cs, CsOw, NO, Ch, SC).

compile_node(ProcName, Res, Ix, N, Dg, Cs, CsOw, NO, Ch, SC)
:- 
	Ix = [], SC = {L,R} :
	CsOw = {[],Dg}, Cs = [], L = R, NO = N, Res = _, Ch = _ |
	     fail(ProcName,' Uncomputable ask part') ;
 
	Ix = {value(Psi),Us,ClsIds}, SC = {L,R} :
        Dg = case(value(Psi),SubTrs), CsOw = {ResOw,Ow}, L = R, NO = N,
	ProcName = _ , Ch = _ |
	     get_res_ow(Res, ClsIds, ResOw),
	     residuals#residuals(Psi, Us, SubTrs\[{ow,Ow}], Cs) ;

        Ix = {Guard,Conn,_}, Guard =\= value(_), Guard =\= {'=?=',_,_},
        arity(Guard,GAr), make_tuple(GAr, Guard1) :
        write_channel(connect(Guard,_,Conn),Ch),
        Dg = case(Guard1,[{true,True},{false,False},{ow,Ow}]), 
	Cs = [{ResT,True},{ResF,False}],
	CsOw = {ResOw,Ow}, ProcName = _ |
             replace_vars_in_guard(GAr, Conn, Guard, Guard1, N, NO),
	     guard_residuals#residuals(Res, Guard1, Conn, 
	                             ResT\[], ResF\[], ResOw\[],
				     Ch, SC) ;

        Ix = {{'=?=',Psi,Term},_,_} :
        Guard1 = {'=?=',Psi,Term1},
        Dg = case(Guard1,[{true,True},{false,False},{ow,Ow}]), 
	Cs = [{ResT,True},{ResF,False}],
	CsOw = {ResOw,Ow}, ProcName = _ |
             replace_arg(Term, Term1, N, NO),
	     guard_residuals#residuals(Res, Guard1, [3], 
	                             ResT\[], ResF\[], ResOw\[],
				     Ch, SC).


get_res_ow(Clss, ClsIds, ResOw)
:-
	Clss ? Cls, Cls = {ClsId,_} |
             ow_cls(Cls, ClsId, ClsIds, ResOw, ResOw', ClsIds'),
	     self ;

	Clss = [] : ResOw = [], ClsIds = _.

ow_cls(Cls, ClsId, ClsIds, ResOw, ResOwO, ClsIdsO)
:-
	ClsIds ? ClsId : ResOw = ResOwO, ClsIdsO = ClsIds',
	                 Cls = _ ;

	ClsIds ? Id, Id =\= ClsId : ClsIdsO ! Id |
               self ;

        ClsIds = [] : ResOw ! Cls, ResOwO = ResOw',
	              ClsId = _, ClsIdsO = [].

replace_vars_in_guard(GAr, Conn, Guard, Guard1, N, NO) 
:-
	GAr-- > 0 |
	     replace_var(GAr, Conn, Guard, Guard1, N, N'),
	     self ;

        GAr = 0 : NO = N, Conn = _, Guard = _, Guard1 = _.

replace_var(GAr, Conn, G, GO, N, NO)
:-
	Conn ? GAr,
        arg(GAr,GO,ArG1) : 
        ArG1 = psi([variable(N)]), G = _, Conn' = _ |
            NO := N + 1 ;
	
	Conn ? I, I =\= GAr | replace_var ;

	Conn = [],
	arg(GAr,G,ArG), arg(GAr,GO,ArG^) : NO = N.

replace_arg(Arg, Arg1, N, NO)
:-
	Arg = integer(_) : Arg1 = Arg, NO = N ;
	Arg = string(_) : Arg1 = Arg, NO = N ;
	Arg = nil(_) : Arg1 = Arg, NO = N ;
	Arg = real(_) : Arg1 = Arg, NO = N ;
	Arg = variable('_') : Arg1 = Arg, NO = N ;
	Arg = variable(V), V =\= '_' : Arg1 = psi([variable(N)]) | NO := N + 1;
	Arg = psi(_) : Arg1 = Arg, NO = N ;
	Arg = tuple(Tuple),
        arity(Tuple,Ar), 
	make_tuple(Ar,Tuple1) : Arg1 = tuple(Tuple1) |
              replace_tuple(Ar, Tuple, Tuple1, N, NO) ;

	Arg = list([ArgCar|ArgCdr]) : Arg1 = list([ArgCar1|ArgCdr1]) |
	      replace_arg(ArgCar, ArgCar1, N, N'),
	      replace_arg(ArgCdr, ArgCdr1, N', NO).

replace_tuple(Ar, Tuple, Tuple1, N, NO)
:-
	Ar-- > 0, arg(Ar,Tuple,ArT), arg(Ar,Tuple1,ArT1) |
              replace_arg(ArT, ArT1, N, N'),
	      self ;

	Ar = 0 : N = NO, Tuple = _, Tuple1 = _.
