/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/bodies.cp,v 1.1 1999/07/09 07:03:01 bill Exp $ */
-language(compound).
-export([bodies/7,reorder/4]).
-mode(trust).

procedure bodies(Bodies, Ctl, Dic, Ch, Name, SC, Done).

bodies(Bodies, Ctl, Dic, Ch, Name, SC, Done)
:-
	Name = {_,_,no,_,_} |
        reorder(Bodies, Name, BodiesO, LastBody),
	get_vars(LastBody, VarsB),
	ground_check#get_multiple_vars(VarsB, Vars),
	bodies1(BodiesO, Vars, LastBody, Ctl, Dic, Ch, Name, SC, Done) ;

	Ctl = CtlH\CtlT, SC = {L,R}, Name = {_,_,Imm,_,_},
	Imm =\= no, Ch = {_,Ch1} |
        reorder(Bodies, Name, BodiesO, LastBody),
	get_vars(LastBody, LastBodyVars),
	ground_check#get_multiple_vars(LastBodyVars, Vars),
	immediate#immediate_activation(Imm, BodiesO, LastBodyVars,
			CtlH\CtlM, {Dic,Dic1}, BodiesO1, Ch1, {L,M}),
%	screen#display(BodiesO,type(ground)),
	bodies1(BodiesO1, Vars, LastBody, CtlM\CtlT, Dic1, Ch, Name, {M,R},Done).

get_vars(LastBody, Vars)
:-
	LastBody = [] : Vars = [] ;
	LastBody =\= [] | ground_check#get_vars(LastBody, Vars\[]).

bodies1(Bodies, Vars, LastBody, Ctl, Dic, Ch, Name, SC, Done)
:-
	Bodies =\= [], 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} |
	mid_bodies(Bodies, CtlH\CtlM, {Dic,Dic1}, Ch, {L,M}, {DL,DM}),
%	screen#display({LastBody,Name},type(ground)),
        ground_check#allocate_vars(Vars, {Dic1,Dic2}, CtlM\CtlM1, Ch1, {M,M1}),
	update_unsafe(Vars, {Dic2,Dic3}, Ch1, {M1,M2}),
	last_body(LastBody, Name, CtlM1\CtlT, Dic3, Ch, {M2,R}, {DM,DR}) ;

	Bodies = [], LastBody =\= [], 
	Ctl = CtlH\CtlT, SC = {L,R}, Ch = {_,Ch1} |
        ground_check#allocate_vars(Vars, {Dic,Dic2}, CtlH\CtlM1, Ch1, {L,M1}),
	update_unsafe(Vars, {Dic2,Dic3}, Ch1, {M1,M2}),
	last_body(LastBody, Name, CtlM1\CtlT, Dic3, Ch, {M2,R}, Done) ;

	Bodies = [], LastBody = [], 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR} : 
        CtlH = [halt|CtlT], L = R, DL = DR, Vars = _, Dic = _, Ch = _, Name = _.

update_unsafe(Vars, Dic, Ch, SC)
:-
	Vars ? psi(V), Dic = {DicI,DicO}, SC = {L,R} :
        Dic'  = {DicM,DicO}, SC' = {M,R},
        write_channel(look(V,Entry,DicI),Ch) |
        update_unsafe_var(V, Entry, {DicI,DicM}, Ch, {L,M}),
	update_unsafe ;

	Vars = [], Dic = {DicI,DicO}, SC = {L,R} :
        DicO = DicI, R = L, Ch = _.

update_unsafe_var(Psi, Entry, Dic, Ch, SC)
:-
	Entry = {a(_),_,var,_}, SC = {L,R} :
        write_channel(update_type(Psi,ref,Dic),Ch), R = L ;

	Entry = {{a(_),_},_,var,_}, SC = {L,R} :
        write_channel(update_type(Psi,sub_arg,Dic),Ch), R = L ;

	Entry = {a(_),_,ref(var),_}, SC = {L,R} :
        write_channel(update_type(Psi,ref,Dic),Ch), R = L ;

	Entry = {{a(_),_},_,ref(var),_}, SC = {L,R} :
        write_channel(update_type(Psi,sub_arg,Dic),Ch), R = L ;

	Entry = {_,_,tuple,Ar}, Ar =\= '*' |
        update_unsafe_variables(Psi, Ar, Dic, Ch, SC) ;

	Entry = {_,_,list,_} |
        update_unsafe_variables(Psi, 2, Dic, Ch, SC) ;

	otherwise, Dic = {DicI,DicO}, SC = {L,R} :
        DicO = DicI, R = L, Psi = _, Entry = _, Ch = _.

update_unsafe_variables(Psi, Ar, Dic, Ch, SC)
:-
	Ar > 0, Dic = {DicI,DicO}, SC = {L,R} :
        Dic'  = {DicM,DicO}, SC' = {M,R} |
        phase2_dic#look_child(Psi,Ar,DicI,Entry,Key),
        update_unsafe_var(Key, Entry, {DicI,DicM}, Ch, {L,M}),
	Ar' := Ar - 1,
	update_unsafe_variables ;

	Ar = 0, Dic = {DicI,DicO}, SC = {L,R} :
        DicO = DicI, R = L, Psi = _, Ch = _.

reorder(Bodies, Name, BodiesOut, LastBody)
:-
	Name = {ProcName,Order,_,_,_}, Order = same_last |
	reorder_same_last(Bodies, ProcName, BodiesOut, LastBody) ;

        Name = {_,Order,_,_,_}, Order = first, Bodies ? spawn(Body), 
	Body =\= {'=',_,_}, Body =\= {plus,_,_,_}, Body =\= {diff,_,_,_} :
        LastBody = Body, BodiesOut = Bodies' ;

        Name = {_,Order,_,_,_}, Order = first, Bodies ? B, B = spawn(Body), 
	Bodies' =\= [], Body = {'=',_,_} : 
        BodiesOut ! B | 
        reorder ;

        Name = {_,Order,_,_,_}, Order = first, Bodies ? B, B = spawn(Body), 
	Bodies' =\= [], Body = {plus,_,_,_} :
        BodiesOut ! B | 
        reorder ;

        Name = {_,Order,_,_,_}, Order = first, Bodies ? B, B = spawn(Body), 
	Bodies' =\= [], Body = {diff,_,_,_} :
        BodiesOut ! B | 
        reorder ;

	Bodies ? spawn(B), Bodies' = [] :
        BodiesOut = [], LastBody = B, Name = _ ;

	Bodies  = [] : BodiesOut = [], LastBody = [], Name = _.

reorder_same_last(Bodies, Name, BodiesOut, LastBody)
:-
	Bodies ? spawn(Body) |
	arity(Body,N), arg(1,Body,P), ArB := N - 1, 
	reorder_same_last1(Body, Name, {P,ArB,0}, Bodies', BodiesOut, 
						LastBody).

reorder_same_last1(Body, Name, NameB, BodiesT, BodiesOut, LastBody)
:-
	Name = NameB : LastBody = Body, BodiesOut = BodiesT ; 

	BodiesT = [], Name =\= NameB : LastBody = Body, BodiesOut = [] ;

	BodiesT =\= [], Name =\= NameB : BodiesOut = [spawn(Body)|BodiesOut'] |
	reorder_same_last(BodiesT, Name, BodiesOut', LastBody).

mid_bodies(Bodies, Ctl, Dic, Ch, SC, Done)
:-
	Bodies ? spawn(Body),
	Ctl = CtlH\CtlT, Dic = {DicIn, DicOut},	
	SC = {L,R}, Ch = {Ch1,Ch2} :
	write_channel(get_reg(Reg,{DicIn,Dic1}),Ch2),
        write_channel(spawn({{Name,N1,0},CallDic,Addr,{L,M}}),Ch1) |
	arity(Body, N), N1 := N - 1, arg(1, Body, Name),
	mid_body(Body, N, CtlH\[allocate_pr(Args,Reg),
				enqueue(Addr,Reg)|CtlM], 
				Args, {Dic1,DicM}, Ch2, {M,M1}),
	build_dic#build_call_dic(Body, N, spawn, DicM, CallDic, Ch, {M1,M2}),
	mid_bodies_reg(Bodies', Reg, CtlM\CtlT, {DicM,DicOut}, 
	                                                Ch, {M2,R}, Done);

	Bodies = [], 
	Ctl = CtlH\CtlT, Dic = {In,Out}, SC = {L,R}, Done = {DL,DR} :
	CtlH = CtlT, Out = In, L = R, DL = DR, Ch = _.

mid_bodies_reg(Bodies, Reg, Ctl, Dic, Ch, SC, Done)
:-
	Bodies ? spawn(Body),
	Ctl = CtlH\CtlT, Dic = {DicIn, DicOut},	
	SC = {L,R}, Ch = {Ch1,Ch2} :
        write_channel(spawn({{Name,N1,0},CallDic,Addr,{M1,M2}}),Ch1),
	Ctl' = CtlM\CtlT, Dic' = {DicM, DicOut}, SC' = {M2,R} |
	arity(Body, N), N1 := N - 1, arg(1, Body, Name),
	mid_body(Body, N, CtlH\[allocate_pr(Args,Reg),
				enqueue(Addr,Reg)|CtlM], 
				Args, {DicIn,DicM}, Ch2, {L,M}),
	build_dic#build_call_dic(Body, N, spawn, DicM, CallDic, Ch, {M,M1}),
	mid_bodies_reg ;

	Bodies = [], 
	Ctl = CtlH\CtlT, Dic = {In,Out}, SC = {L,R}, Done = {DL,DR} :
	CtlH = CtlT, Out = In, L = R, DL = DR, Reg = _, Ch = _.

mid_body(Body, Ar, Ctl, Args, Dic, Ch, SC)+(N=2)
:-
	N =< Ar, Ctl = CtlH\CtlT, Dic = {In,Out}, SC = {L,R} :
	Ctl' = CtlM\CtlT, Dic' = {In',Out}, SC' = {M,R}, Args ! ArgsN |
	arg(N, Body, ArgN), N' := N + 1,
	body_term_creation#term_creation(ArgN, {In,In'}, ArgsN, 
					                 CtlH\CtlM, Ch, {L,M}),
	mid_body ;

	N > Ar, Ctl = CtlH\CtlT, Dic = {In,Out}, SC = {L,R} :
	Args = [], CtlH = CtlT, In = Out, L = R, Body = _, Ch = _.

last_body(Body, Name, Ctl, Dic, Ch, SC, Done)
:-
	arg(1,Body,P), arity(Body,Ar), N := Ar -1, Name = {Addr,_,_,_,_},
	Addr = {_,N,_,_},
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {Ch2,Ch1}: 
	write_channel(spawn({{P,N,0},CallDic1,Addr1,{L,M}}),Ch2),
	write_channel(spawn({{P,N,0},CallDic2,Addr2,{M,M1}}),Ch2),
	DR = DL |
%	screen#display({Dic,CallDic1,CallDic2},type(ground)),
        assignment(Regs, Args, Ar, CtlM\[Inst|CtlM1], Ch1, {M1,M2}),
	assignment(PRs, RegsPR, Ar, CtlM1\[deschedule|CtlT], Ch1, {M2,M3}),
	build_dic#build_call_dic(Body, Ar,iterate,Dic1, 
						CallDic1, Ch, {M3,M4}),
	build_dic#build_call_dic(Body, Ar, spawn, Dic1, 
						CallDic2, Ch, {M4,M5}),
	last_body1(Addr, Addr1, Addr2, Inst, Ch1, {M5,M6}),
	iterate(Body, Ar, CtlH\CtlM, {Regs, Args, PRs, RegsPR}, Dic, Dic1, Ch1,
	                                                     {M6,R}) ;

	arg(1,Body,P), arity(Body,Ar), N := Ar -1, Name = {Addr,_,_,_,_},
	Addr = {_,N1,_,_}, N =\= N1,
	Ctl = CtlH\CtlT, SC = {L,R}, Ch = {Ch2,Ch1}, Done = {DL,DR} : 
	CtlH ! set_cp_arity(N), DR = DL,
	write_channel(spawn({{P,N,0},CallDic1,Addr1,{L,M}}),Ch2),
	write_channel(spawn({{P,N,0},CallDic2,Addr2,{M,M1}}),Ch2) |
        assignment(Regs, Args, Ar, CtlM\[Inst|CtlM1], Ch1, {M1,M2}),
	assignment(PRs, RegsPR, Ar, CtlM1\[deschedule|CtlT], Ch1, {M2,M3}),
	build_dic#build_call_dic(Body, Ar,iterate, Dic1, CallDic1, Ch, {M3,M4}),
	build_dic#build_call_dic(Body, Ar, spawn, Dic1, CallDic2, Ch, {M4,M5}),
	last_body1(Addr, Addr1, Addr2, Inst, Ch1, {M5,M6}),
	iterate(Body, Ar, CtlH'\CtlM, {Regs, Args, PRs, RegsPR}, Dic, Dic1, 
							Ch1, {M6,R}).

last_body1(Addr, Addr1, Addr2, CtlInst, Ch, SC)
:-
	Addr = Addr1, SC = {L,R} :
        write_channel(iterative_label,Ch),
	CtlInst = iterate(Addr), L = R, Addr2 = _ ;

	Addr =\= Addr1, Addr1 =\= Addr2, SC = {L,R} : 
	CtlInst = execute(Addr1,Addr2), L = R, Ch = _ ;

	Addr =\= Addr1, Addr1 = Addr2, SC = {L,R} : 
        CtlInst = execute(Addr1), Ch = _, L = R .


assignment(Regs, Args, Ar, Ctl, Ch, SC)
:-
	Regs =\= [], Args =\= [], Ctl = CtlH\CtlT :
	CtlH = [multiple_copy(ArgsOut,RegsOut)|CtlT] |
	Ar1 := Ar - 1,
	used_regs(Ar1, Args, Used),
	check_copy(Args, Regs, ArgsOut, RegsOut, Used, Ch, SC) ;

	Regs = [], Args = [], Ctl = CtlH\CtlT, SC = {L,R} :
        CtlH = CtlT, L = R, Ar = _, Ch = _.

used_regs(Ar, Args, Used)
:-
       Args ? a(I), I > Ar | Ar' := I, used_regs ;
       Args ? a(I), I =< Ar | used_regs ;
       Args ? {a(I),_}, I > Ar | Ar' := I, used_regs ;
       Args ? {a(I),_}, I =< Ar | used_regs ;
       Args ? {'&',{a(I),_}}, I > Ar | Ar' := I, used_regs ;
       Args ? {'&',{a(I),_}}, I =< Ar | used_regs ;
       Args ? car(a(I)), I > Ar | Ar' := I, used_regs ;
       Args ? car(a(I)), I =< Ar | used_regs ;
       Args ? ro({'&',{a(I),_}}), I > Ar | Ar' := I, used_regs ;
       Args ? ro({'&',{a(I),_}}), I =< Ar | used_regs ;
       Args ? ro({a(I),_}), I > Ar | Ar' := I, used_regs ;
       Args ? ro({a(I),_}), I =< Ar | used_regs ;
       Args ? ro(a(I)), I > Ar | Ar' := I, used_regs ;
       Args ? ro(a(I)), I =< Ar | used_regs ;
       Args ? {'PR',_} | used_regs ;
       Args ? Imm, constant(Imm) | used_regs ;
       Args = [] | Used = Ar.

check_copy(Args, Regs, ArgsOut, RegsOut, Used, Ch, SC)+
					(Omit=0,LeftArgs=[],LeftRegs=[]) 
:-
	Regs ? Reg, Args ? _ |
	check_reg(Reg, Args', unused, St'),
	check_reg(Reg, LeftArgs, St', St),
	check_copy1(St, Args, Reg, Regs', ArgsOut, RegsOut, Omit, 
					LeftArgs, LeftRegs, Used, Ch, SC) ;

        Regs = [], Args = [], LeftArgs = [], LeftRegs = [], SC = {L,R} : 
	write_channel(update_max_reg(Used),Ch),
        ArgsOut = [], RegsOut = [], L = R, Omit = _  | true ;

	Args = [], Regs = [], LeftArgs ? Arg, LeftRegs ? Reg, Omit = 0 :
	ArgsOut ! Reg, RegsOut ! a(Used'), ArgsOut' ! Arg, RegsOut' ! Reg |
	replace(Reg, a(Used'), LeftArgs', Args'),
	Used' := Used + 1,
	check_copy(Args', LeftRegs', ArgsOut'', RegsOut'', Used', Ch, SC,
			0, [], []) ;

	Args = [], Regs = [], LeftArgs =\= [], LeftRegs =\= [], Omit > 0 |
	check_copy(LeftArgs, LeftRegs, ArgsOut, RegsOut, Used, Ch, SC, 
			0, [], []).

replace(Reg, NewReg, Args, ArgsOut)
:-
        Args ? Reg : ArgsOut ! NewReg | replace ;
        Args ? {'&',{Reg,O}} : ArgsOut ! {'&',{NewReg,O}} | replace ;
        Args ? car(Reg) : ArgsOut ! car(NewReg) | replace ;
        Args ? ro(Reg) : ArgsOut ! ro(NewReg) | replace ;
        Args ? ro({'&',{Reg,O}}) : ArgsOut ! ro({'&',{NewReg,O}}) | replace ;
        Args = [] : ArgsOut = [], Reg = _, NewReg = _ ;
        otherwise, Args ? Arg : ArgsOut ! Arg | replace.

check_reg(Reg, Args, St, StOut)
:-
	Args = [Reg|_] : StOut = used, St = _ ;
	Args = [{'&',{Reg,_}}|_] : StOut = used, St = _ ;
	Args = [car(Reg)|_] : StOut = used, St = _ ;
	Args = [ro(Reg)|_] : StOut = used, St = _ ;
	Args = [ro({'&',{Reg,_}})|_] : StOut = used, St = _ ;
	Args = [] : StOut = St, Reg = _ ;
	otherwise, Args ? _ | check_reg.

check_copy1(St, Args, Reg, Regs, ArgsOut, RegsOut, Omit, LeftArgs,
						LeftRegs, Used, Ch, SC)
:-
        St = unused, Args ? Arg : ArgsOut ! Arg, RegsOut ! Reg |
	replace(Arg, Reg, LeftArgs, LeftArgs'),
	replace(Arg, Reg, Args', Args''),
	Omit' := Omit + 1,
	check_copy(Args'', Regs, ArgsOut', RegsOut', Used, Ch, SC,
						Omit', LeftArgs', LeftRegs) ;

        St = used, Args ? Arg |
	check_copy(Args', Regs, ArgsOut, RegsOut, Used, Ch, SC,
					Omit, [Arg|LeftArgs], [Reg|LeftRegs]).

iterate(Body, Ar, Ctl, AssnArgs, Dic, DicO, Ch, SC)
:-
	Ar > 1 |
	arg(Ar, Body, ArgAr),
	iterate1(ArgAr, Body, Ar, Ctl, AssnArgs, Dic, DicO, Ch, SC) ;

	Ar = 1, AssnArgs={Regs,Args,PRs,RegsPR}, Ctl = CtlH\CtlT, SC = {L,R} :
	CtlH = CtlT, Regs = [],	Args = [], PRs = [], RegsPR = [], 
	L = R, DicO = Dic, Body = _, Ch = _.

iterate1(ArgAr, Body, Ar, Ctl, AssnArgs, Dic, DicO, Ch, SC)
:-
	Ar' := Ar - 1, 	Ctl = CtlH\CtlT, SC = {L,R},
	AssnArgs = {Regs, Args, PRs, RegsPR} :
	PRs ! {'PR',Ar2}, RegsPR ! a(Ar'),
	AssnArgs' = {Regs',Args',PRs',RegsPR'} |
	iterate_term_creation#term_creation(ArgAr, {Dic,Dic'}, TermOut, 
					CtlH\CtlM, Ch, {L,M}),
	Ar2 := Ar' - 1, 
	check_termout(TermOut, a(Ar'), Args, Args', Regs, Regs', {M,M1}),
	iterate(Body, Ar', CtlM\CtlT, AssnArgs', Dic', DicO, Ch, {M1,R}).

check_termout(TermOut, Reg, Args, ArgsOut, Regs, RegsOut, SC)
:-
	TermOut = Reg, SC = {L,R} : ArgsOut = Args, RegsOut = Regs, L = R ;
        
        TermOut =\= Reg, SC = {L,R} : 
	Args ! TermOut, Regs ! Reg, ArgsOut = Args', RegsOut = Regs', L = R.

