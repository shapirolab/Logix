/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctlopt/ctlopt.cp,v 1.1 1999/07/09 07:03:05 bill Exp $ */
-language(compound).
-export([ctlopt/3]).
-mode(trust).

procedure ctlopt(Ctl, CtlOpt, SC).

ctlopt(CTLs, CTLsOPT, SC) :-

	CTLs ? {ProcId, Instructions} ,
	SC = {L, R} :
	CTLsOPT ! {ProcId, Instructions'}, SC' = {M1, R} | 
		ctlopt1#instructions(Instructions, Instructions1, {L,M}),
		peephole#peephole(Instructions1,Instructions',{M,M1}),
		ctlopt;

	CTLs = [] ,
	SC = {L,R} :
	CTLsOPT = [],
	L = R  |
		true.
