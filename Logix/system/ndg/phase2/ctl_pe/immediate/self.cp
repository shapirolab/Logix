/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/immediate/self.cp,v 1.1 1999/07/09 07:03:04 bill Exp $ */
-language([compound,typed]).
-mode(trust).
-export([immediate_activation/8, evaluate/6]).

procedure immediate_activation(Imm, Bodies, Ctl, Dic, BodiesO, Ch, SC, Done).

immediate_activation(Imm, Bodies, Ctl, Dic, BodiesO, Ch, SC, Done)
:-
	immediate_activation#immediate_activation(
			Imm, Bodies, Ctl, Dic, BodiesO, Ch, SC, Done).

procedure evaluate(Dg, Dic, Imm, StL, Ch, Done).

evaluate(Dg, Dic, Imm, StL, Ch, Done)
:-
	evaluate#eval(Dg, Dic, Imm, StL, Ch, Done).

