/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/self.cp,v 1.1 1999/07/09 07:03:03 bill Exp $ */
-language([compound,typed]).
-export([ctls/5]).
-mode(trust).

Entry ::= {Areg,Vreg,Type,Value}.

Type ::= new ; load ; car ; addr ; sub_arg ; deref ;
	list ; nil ; string ; number ; integer ; real ; constant ; 
	tuple ; compound ; known ; unknown ; vector ; module ; var.

TypeCheck ::= list ; string ; number ; integer ; real ; constant ; tuple ;
		compound ; known ; unknown ; vector ; module ; var.

procedure ctls(Dgs, Ctls, Iterate, Pr, SC).

ctls(Dgs, Ctls, Iterate, Pr, SC)
:-
	ctls#ctls(Dgs, Ctls, Iterate, Pr, SC).
