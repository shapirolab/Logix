/* $Header: /home/qiana/Repository/Logix/system/ndg/phase1/tell.cp,v 1.1 1999/07/09 07:03:07 bill Exp $ */
-export([tell/3]).
-language(compound).
-mode(trust).

procedure tell(Clss, DgL, SC).

tell(Clss, DgL, SC)
:-
	Clss ? {_,{_,Tell,Body}}, DgL = DgH\DgT, SC = {L,R} :
	DgH ! {DgTells,DgBodies}, DgL' = DgH'\DgT, SC' = {L',R} |
             tell2(Tell, DgTells, {L,M}),
	     body#body(Body, DgBodies, {M,L'}),
	     self ;

	Clss = [], DgL = DgH\DgT, SC = {L,R} : 
        DgH = DgT, R = L.

tell2(Tell, DgTells, SC)
:-
	Tell ? TellGuard, SC = {L,R}, tuple(TellGuard),
	N := arity(TellGuard),
	make_tuple(N,DgTellTest),
	arg(1,TellGuard,Pred),
	arg(1,DgTellTest,TellPred) :
	TellPred = Pred,
	DgTells ! DgTellTest, 
	SC'={L',R} |
	     body#copytuple(TellGuard, 1, N, DgTellTest, {L,L'}),
	     self ;
	     
	Tell ? TellGuard, string(TellGuard) :
	DgTells ! TellGuard |
	     self ;

        Tell = [], SC = {L,R} : DgTells = [], R = L.
