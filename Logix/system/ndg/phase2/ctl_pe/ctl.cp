/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/ctl.cp,v 1.1.1.1 1999/07/09 07:03:02 bill Exp $ */
-language(compound).
-export([ctl/7,ctl1/8]).
-mode(trust).

procedure ctl(Phase1Out, Ctl, Imm, Iterate, Ch, SC, Done).

ctl(Phase1Out, Ctl, Imm, Iterate, Ch, SC, Done) 
:-	
	Phase1Out = {Proc,Dic,Dg}, Proc = {Name,A,Id,Num}, A > 0,
	known(Dg), SC = {L,R} :
	Ctl = {{Name,A,Id,Num,HighReg},[multiple_copy(PRs,Regs),
		     IterLabel|CtlCode]} |
	make_channel(Ch1, DicReq),
	close_ch(M, Ch1),
	prepare_dic(Dic,A,Dic1, Ch1),
	multiple_assignment(A, Regs, PRs, {L,L1}),
	freeze(Dg, [], DgF, _), 
	melt(DgF, DgM, _),
	ctl1(DgM, {4,8,[]}, Dic1, CtlCode\[], {Ch,Ch1}, 
		              {Proc,Iterate,Imm,Dic1,HighReg}, {L1,M}, Done),
%	screen#display({Proc,Dic1},type(ground)),
	phase2_dic#dic(DicReq, {A,HighReg}, IterLabel, {M,R}) ;

	Phase1Out = {Proc,Dic,Dg}, Proc = {Name,A,Id,Num}, A = 0,
	known(Dg), SC = {L,R} :
	Ctl = {{Name,A,Id,Num,HighReg},[IterLabel|CtlCode]} |
	make_channel(Ch1, DicReq),
	close_ch(M, Ch1),
	freeze(Dg, [], DgF, _), 
	melt(DgF, DgM, _),
	ctl1(DgM, {4,8,[]}, {{[],[]},A}, CtlCode\[], {Ch,Ch1}, 
		              {Proc,Iterate,Imm,Dic,HighReg}, {L,M}, Done),
% 	screen#display({Proc,Dic},type(ground)),
	phase2_dic#dic(DicReq, {A,HighReg}, IterLabel, {M,R}).

close_ch(Done, Ch) :- Done = done : close_channel(Ch) | true.

prepare_dic(Dic, Ar, DicOut, Ch)
:-
	Dic = [] |
	create_dic(Ar, {{[],[]},Ar}, DicOut, Ch) ;
	
	Dic = {[],[]} |
	create_dic(Ar, {Dic,Ar}, DicOut, Ch) ;
	
	Dic = {D,[]}, D =\= [] : DicOut = {Dic,Ar}, Ch = _ ;
 
	Dic = {_,A}, integer(A) : DicOut = Dic, Ar = _, Ch = _.

create_dic(Ar, DicIn, DicOut, Ch)+(N=1)
:-
	N =< Ar : 
        write_channel(add([N],{a(N),'*',ref,'*'}, {DicIn, DicIn'}),Ch) |
		N' := N + 1,
		create_dic ;

	N > Ar : DicOut = DicIn, Ch = _.

multiple_assignment(Ar, Regs, PRs, SC)
:-
	Ar > 0 : Regs ! a(Ar), PRs ! {'PR',Ar'} |
		Ar' := Ar - 1,
		multiple_assignment ;

	Ar = 0, SC = {L,R} : Regs = [], PRs = [], R = L.

ctl1(Dg, BH, Dic, Ctl, Ch, Name, SC, Done)
:-
	Dg = case(value(psi(Arg)),Dg'), Ch = {_,Ch1} :
	write_channel(look(Arg,Entry,Dic),Ch1) |
	case_psi#case_psi(Dg', Arg, Entry, BH, Dic, Ctl, Ch, Name, SC, Done) ;

	Dg = case(Test,Labels), tuple(Test), 
	Test =\= value(_) |
	   ask_guard#guard(Test, Labels, BH, Dic, Ctl, Ch, Name, SC, Done) ;
/*
	Dg = case({'=',psi(A),Term},Labels), Term =\= psi(_), Ch = {_,Ch1} :
	write_channel(look(A,Entry,Dic),Ch1) |
	   input_unify#input_unify(A, Entry, Term, Labels, BH, Dic, Ctl, Ch, 
							Name, SC, Done) ;

	Dg = case({'=',psi(A),psi(Term)},Labels), Ch = {_,Ch1} :
	write_channel(look(A,Entry,Dic),Ch1),
	write_channel(look(Term,Entry1,Dic),Ch1) |
	   ask_psi_psi#ask_psi_psi(A, Entry, Term, Entry1, 
				Labels, BH, Dic, Ctl, Ch, Name, SC, Done) ;
*/
	Dg = case(Test,[true(True),false(_),ow(False)]),
	string(Test), Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	CtlH = [call(Test,[],[label(Fl)])|CtlM] |
           get_cont(False, BH, BH'),
	   ctl1(True, BH', Dic, CtlM\[label(Fl)|CtlM1], Ch, Name, 
							{L,M}, {DL,DM}),
	   ctl1(False, BH, Dic, CtlM1\CtlT, Ch, Name, {M,R}, {DM,DR}) ;

	list(Dg) | 
           tells_bodies#tells_bodies(Dg, BH, Dic, Ctl, Ch, Name, SC, Done) ;

	Dg = {Lab,Dg'}, Lab = label(_), Ctl = CtlH\CtlT :
	CtlH ! Lab, Ctl'  = CtlH'\CtlT |	
	   ctl1 ;

/*
	Dg = goto(_), BH = {_,_,[]}, 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	CtlH = [Dg|CtlT], L = R, DL = DR,
	Dic = _, Ch = _, Name = _ ;
*/
	
	Dg = goto(_), BH = {_,_,Go}, Go =\= [], 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	CtlH = [Go|CtlT], L = R, DL = DR,
	Dic = _, Ch = _, Name = _ ;

	Dg = [], Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} :
	CtlH = CtlT, L = R, DL = DR, 
	BH = _, Dic = _, Ch = _, Name = _ .

get_cont(Ow, BH, BHO)
:-
	Ow = {Lab,_}, Lab = label(_), BH = {B,H,_} : BHO = {B,H,goto(Lab)} ;
	Ow =\= {label(_),_} : BHO = BH.
