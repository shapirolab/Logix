/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/ctls.cp,v 1.1.1.1 1999/07/09 07:03:03 bill Exp $ */
-language(compound).
-export([ctls/5]).
-mode(trust).

procedure ctls(Dgs, Ctls, Iterate, Partial, SC).

ctls(Dgs, Ctls, Iterate, Partial, SC) :-
	Dgs = [Dg|_], Partial = ms(Prec,Imm),
	Dg = {{P,A,I},{Dic,_,_,_,_},Dg'},
	SC = {L,R} :
	Ctls ! Ctl |
%	computation#display(term,Dgs,type(ground)),
	   make_channel(Ch, Req),
	   ctl#ctl({{P,A,I,0},Dic,Dg'}, Ctl, Imm, Iterate, Ch, 
							{L,R1}, {done,Do}),
	   ctls1(Do, Req, Dgs, Ctls', Iterate, Prec, Imm,  
				[{{P,A,I},Dic,{P,A,I,0}}],Ch,{R1,R}),
	   close_ch(R1,Ch).

close_ch(R,Ch) 
:- 
	R = done : close_channel(Ch) ;
	R = {_,done} : close_channel(Ch).

ctls1(Do, Request, Dgs, Ctls, Iterate, Prec, Imm, CompiledProcs, Ch, SC) 
:-
	Request ? opt(Opt) : Opt = Prec | 
%       computation#display(term,opt(Opt,Prec)),
	self ;

	Request ? entries(Proc,DicEnts) : DicEnts = {Case,Ask,Tell,Body} |
        get_dg(Proc, Dgs, _, _, Case, Ask, Tell, Body),
	self ;

	Request ? Req,
	Req = spawn({Proc,Dic,Addr,S}) : Do = _ |
		get_dg(Proc, Dgs, Dg, _DgDic, _Case, _Ask, _Tell, _Body),
%		get_dic(Prec, Dic, DgDic, Case, Ask, Tell, Body, Dic1),
                compile({Proc,Dic,Addr,S}, Dg, Ctls, Iterate, CompiledProcs, 
                     Imm, Ctls', CompiledProcs', Ch, Do'),
%	computation#display(term,{Req,CompiledProcs},type(ground)),
        self;

/*
	Do = done, Request ? Req,
	Req = activate(Proc,Imm,Dic,StL,S) |
		get_dg(Proc, Dgs, Dg, _, _, _, _, _),
		immediate#evaluate(Dg, Dic, Imm, StL, Ch, S),
	self;
*/

	Request = [], SC = {L,R} : Ctls = [], L = R, 
        Dgs = _, Iterate = _, Prec = _, Imm = _, 
	CompiledProcs = _, Ch = _, Do = _.

compile(Req, Dg, Ctls, Iterate, CompiledProcs, Imm, 
				CtlsT, CompiledProcsO, Ch, Do)+(Count=0)
:-
	Req = {Proc,Dic,Addr,{L,R}},
	CompiledProcs ? Ent, Ent = {Proc,Dic,Addr1} : 
        Addr = Addr1,
	CompiledProcsO = [Ent|CompiledProcs'],
	Ctls = CtlsT,
	L =R, Do = done, Ch = _, Count = _, Iterate = _, Dg = _, Imm =_ ;

	Req = {Proc,_,_,_},
	CompiledProcs ? {Proc1,Dic1,Addr1},
	Proc1 =\= Proc :
        CompiledProcsO ! {Proc1,Dic1,Addr1} |
             self;

	Req = {Proc,Dic,_,_}, 
	CompiledProcs ? {Proc,Dic1,Addr1},
	Dic1 =\= Dic,
	Count++ :
        CompiledProcsO ! {Proc,Dic1,Addr1} |
%	computation#display(term,{Proc,Dic,Dic1},[length(2000),depth(2000)]),
             self;

	CompiledProcs = [],
	Req = {{P,A,I},Dic,Addr,SC} :
        Ctls = [Ctl|CtlsT],
	CompiledProcsO = [{{P,A,I},Dic,{P,A,I,Count}}],
	Addr = {P,A,I,Count} |
     	ctl#ctl({{P,A,I,Count},Dic,Dg}, Ctl, Imm, Iterate, Ch, SC, {done,Do}).

get_dg(Proc, Dgs, Dg, DgDic, Case, Ask, Tell, Body)
:-
	Dgs = [{Proc,{Dic,C,A,T,B},Dg1}|_] : 
	Dg = Dg1, DgDic = Dic, Case = C, Ask = A, Tell = T, Body = B;

	Dgs ? {Proc1,_,_}, Proc1 =\= Proc | self.

