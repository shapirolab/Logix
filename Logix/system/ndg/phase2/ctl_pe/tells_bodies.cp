/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/tells_bodies.cp,v 1.1.1.1 1999/07/09 07:03:02 bill Exp $ */
-language(compound).
-export([tells_bodies/8]).
-mode(trust).

procedure tells_bodies(Dg, BH, Dic, Ctl, Ch, Name, SC, Done).

tells_bodies(Dg, BH, Dic, Ctl, Ch, Name, SC, Done)
:-
	Dg = [{[],DgBodies}] : BH = _ |
	bodies#bodies(DgBodies, Ctl, Dic, Ch, Name, SC, Done) ;

	Dg ? Dg1, Dg1 = {DgTells,DgBodies},
	Dg1 =\= goto(_), Dg1 =\= suspend(_),
	DgTells =\= [], 
	Dg' =\= [suspend], Dg' =\= [suspend(_)], Dg' =\= [suspend(_,_)],
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	CtlH ! set_HBT |
	tell_body#tell_body(DgTells, DgBodies, CtlH'\[label(L1)|CtlM], 
				label(L1), Dic, Ch, Name, {L,M}, {DL,DM}),
	tells_bodies_nexts(Dg', BH, Dic, CtlM\CtlT, Ch, Name, {M,R}, {DM,DR}) ;

	Dg ? Dg1, Dg1 = {DgTells,DgBodies},
	Dg1 =\= goto(_), Dg1 =\= suspend(_),
	DgTells =\= [], 
	Dg' = [suspend],
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
        Ctl' = CtlM\CtlT, SC' = {M,R}, Done' = {DM,DR} |
	tell_body#tell_body(DgTells, DgBodies, CtlH\[label(L1)|CtlM], 
				label(L1), Dic, Ch, Name, {L,M}, {DL,DM}),
	tells_bodies ;

	Dg ? Dg1, Dg1 = {DgTells,DgBodies},
	Dg1 =\= goto(_), Dg1 =\= suspend(_),
	DgTells =\= [], 
	Dg' = [suspend(_)],
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
        Ctl' = CtlM\CtlT, SC' = {M,R}, Done' = {DM,DR} |
	tell_body#tell_body(DgTells, DgBodies, CtlH\[label(L1)|CtlM], 
				label(L1), Dic, Ch, Name, {L,M}, {DL,DM}),
	tells_bodies ;

	Dg ? Dg1, Dg1 = {DgTells,DgBodies},
	Dg1 =\= goto(_), Dg1 =\= suspend(_),
	DgTells =\= [], 
	Dg' = [suspend(_,_)],
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
        Ctl' = CtlM\CtlT, SC' = {M,R}, Done' = {DM,DR} |
	tell_body#tell_body(DgTells, DgBodies, CtlH\[label(L1)|CtlM], 
				label(L1), Dic, Ch, Name, {L,M}, {DL,DM}),
	tells_bodies ;

        Dg = [suspend], Ctl = CtlH\CtlT, Ch = {_,Ch1} : BH = _|
        store_regs(Dic, CtlH\[suspend|CtlT], Ch1, Name, SC, Done) ;

        Dg = [suspend(F)], Ctl = CtlH\CtlT, Ch = {Ch2,Ch1}, SC = {L,R},
	Name = {_,_,_,Dic1,_} : BH = _,
   	write_channel(spawn({F,Dic1,Addr,{L,M}}),Ch2) |
%	build_dic(Dic1, Dic2),
        store_regs(Dic, CtlH\[suspend(Addr)|CtlT], Ch1, Name, {M,R}, Done) ;

        Dg = [suspend(F,S)], Ctl = CtlH\CtlT, Ch = {Ch2,Ch1}, SC = {L,R},
	Name = {_,_,_,Dic1,_} : BH = _,
        write_channel(spawn({F,Dic1,AddrF,{L,M}}),Ch2),
        write_channel(spawn({S,Dic1,AddrS,{M,M1}}),Ch2) |
%	build_dic(Dic1, Dic2),
        store_regs(Dic, CtlH\[suspend(AddrF,AddrS)|CtlT], Ch1, Name, 
							{M1,R}, Done) ;

	Dg = [goto(Lab)], BH = {_,_,[]}, 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} : 
        CtlH = [goto(Lab)|CtlT], L = R, DL = DR, Dic = _, Ch = _, Name = _ ;

	Dg = [goto(_)], BH = {_,_,Go}, Go =\= [], 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} : 
        CtlH = [Go|CtlT], L = R, DL = DR, Dic = _, Ch = _, Name = _.

tells_bodies_nexts(Dg, BH, Dic, Ctl, Ch, Name, SC, Done)
:-
	Dg ? Dg1, Dg1 = {DgTells,DgBodies}, 
	Dg1 =\= goto(_), Dg1 =\= suspend(_),
	DgTells =\= [], 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	CtlH ! undo,
	Ctl' = CtlM\CtlT, SC' = {M,R}, Done' = {DM,DR} |
	tell_body#tell_body(DgTells, DgBodies, CtlH'\[label(L1)|CtlM],
				label(L1), Dic, Ch, Name, {L,M}, {DL,DM}),
	tells_bodies_nexts ;

        Dg = [suspend], Ctl = CtlH\CtlT, Ch = {_,Ch1} : BH = _ |
        store_regs(Dic, CtlH\[suspend|CtlT], Ch1, Name, SC, Done) ;

        Dg = [suspend(F)], Ctl = CtlH\CtlT, Ch = {Ch2,Ch1}, SC = {L,R},
	Name = {_,_,_,Dic1,_} :
        write_channel(spawn({F,Dic1,Addr,{L,M}}),Ch2), BH = _ |
%	build_dic(Dic1, Dic2),
        store_regs(Dic, CtlH\[suspend(Addr)|CtlT], Ch1, Name, {M,R}, Done) ;

        Dg = [suspend(F,S)], Ctl = CtlH\CtlT, Ch = {Ch2,Ch1}, SC = {L,R},
	Name = {_,_,_,Dic1,_} :
        write_channel(spawn({F,Dic1,AddrF,{L,M}}),Ch2),
        write_channel(spawn({S,Dic1,AddrS,{M,M1}}),Ch2), BH = _ |
%	build_dic(Dic1, Dic2),
        store_regs(Dic, CtlH\[suspend(AddrF,AddrS)|CtlT], Ch1, Name, 
							{M1,R}, Done) ;
/*
 	Dg = [goto(Lab)], BH = {_,_,[]}, 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} : 
        CtlH = [undo,goto(Lab)|CtlT], 
	L = R, DL = DR, Dic = _, Ch = _, Name = _ ;
*/

 	Dg = [goto(_)], BH = {_,_,Go}, Go =\= [],
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} : 
        CtlH = [undo,Go|CtlT], 
	L = R, DL = DR, Dic = _, Ch = _, Name = _.

store_regs(Dic, Ctl, Ch, Name, SC, Done)
:-
	Name = {{_,Ar,_,_},_,_,_,_}, Ctl = CtlH\CtlT, SC = {L,R} |
	assignment(Src, Dest, CtlM\CtlT, {L,M}),
	prepare_assignment(Ar, Src, Dest, Dic, CtlH\CtlM, Ch, {M,R}, Done).

assignment(Src, Dest, Ctl, SC)
:-
	Src =\= [], Dest =\= [], Ctl = CtlH\CtlT, SC = {L,R} :
                 CtlH = [multiple_copy(Src,Dest)|CtlT], L = R ;

	Src = [], Dest = [], Ctl = CtlH\CtlT, SC = {L,R} : CtlH = CtlT, L = R.

prepare_assignment(Ar, Src, Dest, Dic, Ctl, Ch, SC, Done)
:-
	Ar > 0, Ctl = CtlH\CtlT  : Ctl' = CtlM\CtlT,
	write_channel(look([Ar],{AddrReg,_,_,_},Dic),Ch) |
        assign(a(Ar), AddrReg, CtlH\CtlM, Dest, Dest', Src, Src'),
	Ar' := Ar - 1, 
	prepare_assignment ;

	Ar = 0, Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} : 
        Dest = [], Src = [], CtlH = CtlT, L = R, DL = DR, Dic = _, Ch = _.

assign(Reg, AddrReg, Ctl, Dest, DestT, Src, SrcT)
:-
	AddrReg = {Areg,-1}, Areg =\= Reg, Ctl = CtlH\CtlT :
	CtlH = [decrement_pointer(Areg,Reg)|CtlT], Dest = DestT, Src = SrcT;

	AddrReg = {Reg,-1}, Ctl = CtlH\CtlT :
	CtlH = [decrement_pointer(Reg)|CtlT], Dest = DestT, Src = SrcT;

	AddrReg = Reg, Ctl = CtlH\CtlT : Dest = DestT, Src = SrcT, CtlH = CtlT;

	AddrReg = a(_), AddrReg =\= Reg, Ctl = CtlH\CtlT : 
	Dest = [Reg|DestT], Src = [AddrReg|SrcT], CtlH = CtlT.

	
