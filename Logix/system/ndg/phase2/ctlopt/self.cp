/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctlopt/self.cp,v 1.1.1.1 1999/07/09 07:03:05 bill Exp $ */
-language(compound).
-export([ctlopts/3]).
-mode(trust).

procedure ctlopts(Ctl, CtlOpt, SC).

ctlopts(Ctl, CtlOpt, SC) :-
	ctlopt#ctlopt(Ctl, CtlOpt, SC).
