/* $Header: /home/qiana/Repository/Logix/system/ndg/phase1/decision_graph.cp,v 1.1.1.1 1999/07/09 07:03:06 bill Exp $ */
-export([decision_graph/7]).
-language(compound).
-mode(trust).

procedure decision_graph(ProcName, Clss, Instruction, IxAlg, Dg, Ch, SC).

decision_graph(ProcName, Clss, Instruction, IxAlg, Dg, Ch, SC) 
:-
	init_proc(Clss, Cp),
	check_Cp(Cp, Status),
	decision_graph1(ProcName, Cp, Instruction, IxAlg, Status, Dg, Ch, SC).

init_proc(Clss, Cp)+(ClsId=1)
:-
	Clss ? Cls : Cp ! {ClsId,Cls} | ClsId' := ClsId + 1, init_proc ;

	Clss = [] : Cp = [], ClsId = _.

decision_graph1(ProcName, Cp, Instruction, IxAlg, Status, Dg, Ch, SC)+(N=1)
:-
	Status = {nil,_} : 
        Dg = [SusInst], Cp = _, IxAlg = _, ProcName = _, Ch = _, N = _ | 
	       gen_susinst(Instruction, SusInst, SC) ;

	Status = {body,Body} : Dg = [{[],DgBodies}],
	       Cp = _, IxAlg = _, ProcName = _, Instruction = _, Ch = _, N= _ | 
	       body#body(Body, DgBodies, SC) ;

        Status = {tell,_}, SC = {L,R} :
                    IxAlg = _, ProcName = _, Ch = _, N = _ | 
		sort_clss(Cp, Cp1),	
	       tell#tell(Cp1, Dg\ [SusInst], {L,M}),
	       gen_susinst(Instruction, SusInst, {M,R}) ;

	Status = {ask,_}, SC = {L,R} |
%       screen#display(dg(Cp,N),type(ground)),
	       indexing_function#indexing_function(Cp, IxAlg, Ix, Ch),
	       compile_node#compile_node(ProcName, Cp, Ix, N, Dg, Cs, CsOw, N', 
	                                                            Ch, {L,M}),
	       compile_ow_residual(ProcName, CsOw, Instruction, IxAlg, N', 
	                                       NewInst, Ch, {M,R'}),
	       compile_residuals(ProcName, Cs, NewInst, IxAlg, N', Ch, {R',R}).

gen_susinst(Instruction, SusInst, SC)
:-
	Instruction = suspend(Failure,Suspend),
	Failure = fail, Suspend = suspend, SC = {L,R} :
	SusInst = suspend, L = R ;

	Instruction = suspend(Failure,Suspend),
	Failure =\= fail , Suspend = suspend, SC = {L,R} :
	SusInst = suspend(Failure) , L = R ;

	Instruction = suspend(Failure,Suspend),
	Failure =\= fail, Suspend =\= suspend, SC = {L,R} :
	SusInst = suspend(Failure,Suspend) , L = R ;

 	Instruction = goto(Label), SC = {L,R} :
	SusInst = goto(Label) , L = R.

compile_ow_residual(ProcName, CsOw, Instruction, IxAlg, N, NewInst, Ch, SC)
:-
	CsOw = {[],Dg} : NewInst = Instruction, Dg = [SusInst], 
	                     IxAlg = _, ProcName = _, Ch = _, N = _ | 
	     gen_susinst(Instruction, SusInst, SC) ;

	CsOw = {Cp,Dg},
	Cp =\= [] :
	Dg = {label(L),Dg1},
	NewInst = goto(label(L)) |
	     check_Cp1(Cp,Status),
	     decision_graph1(ProcName, Cp, Instruction, IxAlg, Status, Dg1, 
                             Ch, SC, N).

compile_residuals(ProcName, Cs, Instruction, IxAlg, N, Ch, SC)
:-
	Cs ? {Cp,Dg}, SC = {L,R} : SC' = {M,R} |
           check_Cp(Cp, Status),
	   decision_graph1(ProcName, Cp, Instruction, IxAlg, Status, Dg, 
								Ch, {L,M}, N),
	   compile_residuals ;

	Cs = [], SC = {L,R} : 
        L = R, Instruction = _, IxAlg = _, ProcName = _, Ch = _, N = _.

check_Cp(Cp, Status)
:-
	Cp =\= [] | check_Cp1(Cp, Status) ;
	
	Cp = [] : Status = {nil,_}.

check_Cp1(Clss, Status)+(N=0)
:-
	Clss ? {Id,{[],[],Body1}} :
        Status = {body,Body}, N = _ |
	get_body(Id, Body1, Clss', Body) ;

	Clss ? {_,{[],Tell,_}}, Tell =\= [] | check_Cp1 ;

	Clss ? {_,{Ask,_,_}}, Ask =\= [] | N' := N + 1, check_Cp1 ;
	
	Clss = [], N = 0 : Status = {tell, _} ;
	
	Clss = [], N > 0 : Status = {ask,N}.

get_body(Id, Body, Clss, BodyO)
:-
	Clss ? {Id1,{[],[],Body1}}, Id1 > Id :
	Body' = Body1, Id' = Id1, Body = _ |
	get_body ;

	Clss ? {Id1,{[],[],_}}, Id1 =< Id | get_body ;

	Clss = [] : BodyO = Body, Id = _ ;

	otherwise, Clss ? _ | get_body.

sort_clss(ClssI,ClssO)
:-
	qsort1(ClssI, ClssO-[]).

qsort1(Clss, ClssO) :-
	Clss ? X, ClssO = Sorted-Tail |
	partition(X, Clss', Small, Large),
	qsort1(Small, SSorted-Tail),
	qsort1(Large, Sorted-[X|SSorted]) ;

	Clss = [], ClssO = X-Y : X = Y.

partition(X, List, Small, Large) :-
	List ? Y,  X = {Id,_}, Y = {Id1,_}, Id > Id1 : Small ! Y | partition ;
	List ? Y,  X = {Id,_}, Y = {Id1,_}, Id =< Id1 : Large ! Y | partition ;
	List = [] : Small = [], Large = [], X = _.
	
