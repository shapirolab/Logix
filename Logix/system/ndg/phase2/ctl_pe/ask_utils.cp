/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/ask_utils.cp,v 1.1 1999/07/09 07:03:00 bill Exp $ */
-language(compound).
-export([compile_guard/12,
	select_sdgs/9]).
-mode(trust).

compile_guard(Guard, Args, Type, Labels, BH, DicT, DicO, Ctl, 
							Ch, Name, SC, Done)
:-
	Type = both,
	Labels = [true(True),false(False),ow(Ow)],
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	CtlH ! call(Guard,Args,[label(Fl),label(Sus)]) |
%	screen#display({Guard,Args,DicO,False},type(ground)),
           get_cont(Ow, BH, BH'),
	   ctl#ctl1(True, BH', DicT, CtlH'\[label(Fl)|CtlM], 
						Ch, Name, {L,M}, {DL,DM}),
	   ctl#ctl1(False, BH', DicO, CtlM\[label(Sus)|CtlM2],
						Ch, Name, {M,M1}, {DM,DM1}),
	   ctl#ctl1(Ow, BH, DicO, CtlM2\CtlT, Ch, Name, {M1,R}, {DM1,DR}) ;

	Type = fail,
	Labels = [true(True),false(False),ow(Ow)], Ow = {label(_),_},
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	CtlH ! call(Guard,Args,[label(Fl)]) |
           get_cont(Ow, BH, BH'),
	   ctl#ctl1(True, BH', DicT, CtlH'\[label(Fl)|CtlM], 
						Ch, Name, {L,M}, {DL,DM}),
	   ctl#ctl1(False, BH', DicO, CtlM\CtlM2, Ch, Name, {M,M1}, {DM,DM1}),
	   ctl#ctl1(Ow, BH, DicO, CtlM2\CtlT, Ch, Name, {M1,R}, {DM1,DR}) ;

	Type = fail,
	Labels = [true(True),false(False),ow(Ow)], Ow =\= {label(_),_},
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	CtlH ! call(Guard,Args,[label(Fl)]) |
	   ctl#ctl1(True, BH, DicT, CtlH'\[label(Fl)|CtlM], 
						Ch, Name, {L,M}, {DL,DM}),
	   ctl#ctl1(False, BH, DicO, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR}).

select_sdgs(St, Labels, BH, Dic, Ctl, Ch, Name, SC, Done)
:-
	St = true, Labels = [true(True),_,ow(Ow)], Ow = {label(_),_},
	tuple(Dic),
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} |
           get_cont(Ow, BH, BH'),
	   ctl#ctl1(True, BH', Dic, CtlH\CtlM, Ch, Name, {L,M}, {DL,DM}),
	   ctl#ctl1(Ow, BH, Dic, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR}) ;

	St = true, Labels = [true(True),_,ow(Ow)], Ow = {label(_),_},
	Dic = [DicT,DicO],
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} |
           get_cont(Ow, BH, BH'),
	   ctl#ctl1(True, BH', DicT, CtlH\CtlM, Ch, Name, {L,M}, {DL,DM}),
	   ctl#ctl1(Ow, BH, DicO, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR}) ;

	St = false, Labels = [_,false(False),ow(Ow)], Ow = {label(_),_},
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} |
           get_cont(Ow, BH, BH'),
	   ctl#ctl1(False, BH', Dic, CtlH\CtlM, Ch, Name, {L,M}, {DL,DM}),
	   ctl#ctl1(Ow, BH, Dic, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR}) ;

	St = true, Labels = [true(True),_,ow(Ow)], Ow =\= {label(_),_},
	tuple(Dic) |
	   ctl#ctl1(True, BH, Dic, Ctl, Ch, Name, SC, Done) ;

	St = true, Labels = [true(True),_,ow(Ow)], Ow =\= {label(_),_},
	Dic = [DicT,_] |
	   ctl#ctl1(True, BH, DicT, Ctl, Ch, Name, SC, Done) ;

	St = false, Labels = [_,false(False),ow(Ow)], Ow =\= {label(_),_} |
	   ctl#ctl1(False, BH, Dic, Ctl, Ch, Name, SC, Done) ;

	St = ow, Labels = [_,_,ow(Ow)] |
	   ctl#ctl1(Ow, BH, Dic, Ctl, Ch, Name, SC, Done).

get_cont(Ow, BH, BHO)
:-
	Ow = {Lab,_}, Lab = label(_), BH = {B,H,_} : BHO = {B,H,goto(Lab)} ;
	Ow =\= {label(_),_} : BHO = BH.

