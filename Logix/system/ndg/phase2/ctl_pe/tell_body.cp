/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/tell_body.cp,v 1.1 1999/07/09 07:03:01 bill Exp $ */
-language(compound).
-export([tell_body/9]).
-mode(trust).

procedure tell_body(DgTells, DgBodies, Ctl, Fail, Dic, Ch, Name, SC, Done).

tell_body(DgTells, DgBodies, Ctl, Fail, Dic, Ch, Name, SC, Done)
:-
	Ch = {_,Ch1}, Ctl = CtlH\CtlT, SC = {L,R} |
        ground_check#allocate_multiple_vars(DgTells, CtlH\CtlM,
	                                             {Dic,Dic1}, Ch1, {L,M}),
	bodies#reorder(DgBodies, Name, _, LastBody),
	get_vars(LastBody, LastBodyVars), 
	sort_tell(DgTells, Tgs, Tgs', Uns, News, Dic1, Ch1),
	filter_unsafe(Uns, LastBodyVars, UnsO, Tgs', Tgs''),
	filter_unsafe(News, LastBodyVars, NewsO, Tgs'', Tgs'''),
	count_uns(UnsO, Uns1, Tgs'''), 
	tell_guards(Tgs, CtlM\CtlM1, Commit, Fail, 
					{Dic1,Dic2}, Ch1, {M,M1}),
	new_terms(NewsO, CtlM1\CtlM2, {Dic2,Dic3}, Ch1, {M1,M2}),
	tell_unifications_body(Uns1, Commit, DgBodies, CtlM2\CtlT, Fail,
	                                Dic3, Ch, Name, {M2,R}, Done).

get_vars(LastBody, LastBodyVars)
:-
	LastBody = [] : LastBodyVars = [] ;
	LastBody =\= [] | ground_check#get_vars(LastBody, LastBodyVars\[]).

filter_unsafe(Uns, LastBody, UnsO, Tgs, TgsT)
:-
	Uns ? Un, Un = {Var,_} |
        member_var(Var, Un, LastBody, UnsO, UnsO', Tgs, Tgs'),
	filter_unsafe ;

	Uns = [] : UnsO = [], Tgs = TgsT, LastBody = _.

member_var(Var, Un, LastBody, Uns, UnsT, Tgs, TgsT)
:-
	LastBody = [Var|_], Un = {X,Y} : Uns = UnsT, Tgs = [{'=',X,Y}|TgsT] ;
	LastBody ? V, V =\= Var | member_var ;
	LastBody = [] : Uns = [Un|UnsT], Tgs = TgsT, Var = _.

count_uns(Uns, UnsO, Tgs)+(N=0)
:-
	Uns ? U, N < 10 : UnsO ! U | N' := N + 1, count_uns ;
	Uns ? {X,Y}, N >= 10 : Tgs ! {'=',X,Y} | count_uns;
	Uns = [] : UnsO = [], Tgs = [], N = _.

sort_tell(Tells, Tgs, TgT, Uns, News, Dic, Ch)
:-
	Tells ? U, U = {'=',P1,P2}, P1 = psi(P), P2 =\= psi(_) :
	write_channel(look(P,Entry,Dic),Ch) |
	part_tell(P1, Entry, P2, term, Uns, Uns', Tgs, Tgs', News, News'),
	sort_tell ;
	 
	Tells ? U, U = {'=',P1,P2}, P1 =\= psi(_), P2 = psi(P) :
	write_channel(look(P,Entry,Dic),Ch) |
	part_tell(P2, Entry, P1, term, Uns, Uns', Tgs, Tgs', News, News'),
	sort_tell ;
	 
	Tells ? U, U = {'=',P1,P2}, P1 = psi(Ps1), P2 = psi(Ps2) :
	write_channel(look(Ps1,Entry1,Dic),Ch),
	write_channel(look(Ps2,Entry2,Dic),Ch) |
	part_tell(P1, Entry1, P2, Entry2, Uns, Uns', Tgs, Tgs', News, News'),
	sort_tell ;

	Tells ? U, U = {'=',P1,P2}, P1 =\= psi(_), P2 =\= psi(_) : 
	Tgs ! U | sort_tell ;

	Tells ? U, U =\= {'=',_,_} : Tgs ! U | sort_tell ;

	Tells = [] : Tgs = TgT, News = [], Uns = [], Ch = _, Dic = _.

part_tell(Psi1, Entry1, Psi2, Entry2, Uns, UnsO, Tgs, TgsO, News, NewsO)
:-
	Entry1 = {_,_,new,_} : Entry2 = _,
	Uns = UnsO, Tgs = TgsO, News = [{Psi1,Psi2}|NewsO] ;

	Entry2 = {_,_,new,_} : Entry1 = _, 
	Uns = UnsO, Tgs = TgsO, News = [{Psi2,Psi1}|NewsO] ;

	otherwise : NewsO = News | 
        get_ent_st(Entry1, St1),
        get_ent_st(Entry2, St2),
	part_tell_no_new(Psi1, St1, Psi2, St2, Uns, UnsO, Tgs, TgsO).

get_ent_st(Entry,St)
:-
	Entry = {_,_,car,_} : St = 0  ;
	Entry = {_,_,cdr,_} : St = 0  ;
	Entry = {_,_,sub_arg,_} : St = 0  ;
	Entry = {_,_,ref,_} : St = 0  ;
	Entry = {_,_,deref,_} : St = 0 ;
	Entry = {_,_,var,_} : St = 0 ;
	Entry = {_,_,ref(var),_} : St = 0 ;
	Entry = {_,_,car(var),_} : St = 0 ;
	otherwise : St = 1, Entry = _.

part_tell_no_new(Psi1, St1, Psi2, St2, Uns, UnsO, Tgs, TgsO)
:-
	St1 = 0, St2 = 0 :	Uns = UnsO, Tgs = [{'=',Psi1,Psi2}|TgsO] ;
	St1 = 0, St2 = 1 :	Uns = [{Psi1,Psi2}|UnsO], Tgs = TgsO ;
	St1 = 1, St2 = 0 :      Uns = [{Psi2,Psi1}|UnsO], Tgs = TgsO ;
	St1 = 1, St2 = 1 :	Uns = UnsO, Tgs = [{'=',Psi1,Psi2}|TgsO].

tell_guards(Tgs, Ctl, Commit, Fail, Dic, Ch, SC)
:-
	Tgs ? Tell,
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	Ctl' = CtlM\CtlT, Dic' = {DicI',DicO}, 
	Commit ! commit, SC' = {M,R} |
	tell_guard(Tell, CtlH\CtlM, Fail, {DicI,DicI'}, Ch, {L,M}),
	tell_guards ;

	Tgs = [], 
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	CtlH = CtlT, Commit = [],
	DicO = DicI, L = R, Fail = _, Ch = _ .

tell_guard(Tell, Ctl, Fail, Dic, Ch, SC)
:-
	Tell = {'=',P1,P2}, P1 =\= ro(_),
	Ctl = CtlH\CtlT, Dic = {DicI,DicO},	SC = {L,R} |
	unsafe_term_creation#term_creation(P1, {DicI, Dic1}, Reg1,
					               CtlH\CtlM, Ch, {L,M}),	
	unsafe_term_creation#term_creation(P2, {Dic1, DicO}, Reg2,
               		CtlM\[unify([Reg1],[Reg2],Fail)|CtlT], Ch, {M,R}) ;

	Tell = {'=',P1,P2}, P2 =\= ro(_),
	Ctl = CtlH\CtlT, Dic = {DicI,DicO},	SC = {L,R} |
	unsafe_term_creation#term_creation(P2, {DicI, Dic1}, Reg1,
					               CtlH\CtlM, Ch, {L,M}),	
	unsafe_term_creation#term_creation(P1, {Dic1, DicO}, Reg2,
               		CtlM\[unify([Reg1],[Reg2],Fail)|CtlT], Ch, {M,R}) ;

	Tell = {'=',P1,P2}, P1 = ro(_), P2 = ro(_), Ctl = CtlH\CtlT |
	prepare_arguments(Tell, 3, CtlH\[call('=',TellArgs,Fail)|CtlT],
	                                                TellArgs, Dic, Ch, SC) ;

	tuple(Tell), Tell =\= {'=',_,_}, Ctl = CtlH\CtlT |
	arity(Tell,N), arg(1,Tell,Guard),
	prepare_arguments(Tell, N,CtlH\[call(Guard,TellArgs,Fail)|CtlT],
	                                                TellArgs, Dic, Ch, SC) ;

	Ctl = CtlH\CtlT, string(Tell), Dic = {DicI,DicO}, SC = {L,R} :
	CtlH = [call(Tell,[],Fail)|CtlT],
	DicO = DicI, L = R, Ch = _.

prepare_arguments(Tell, Ar, Ctl, TellArgs, Dic, Ch, SC)+(N=2)
:-
	N =< Ar,
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	Dic' = {Dic1,DicO}, Ctl' = CtlM\CtlT,	SC' = {M,R},
	TellArgs ! Reg1 |
	arg(N,Tell,ArgN), N' := N + 1,
	unsafe_term_creation#term_creation(ArgN, {DicI,Dic1}, Reg1, CtlH\CtlM, 
	                                                     Ch, {L,M}),
	prepare_arguments ;

	N > Ar, 
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	TellArgs = [], DicI = DicO, CtlH = CtlT, L = R,
	Tell = _, Dic = _, Ch = _.

new_terms(News, Ctl, Dic, Ch, SC)
:-
	News = [], 
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	CtlT = CtlH, DicO = DicI, R = L, Ch = _ ;

	News ? E, E = {psi(Psi),Term}, 
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	Ctl' = CtlM\CtlT, Dic' = {DicM,DicO}, SC' = {M,R} |
	new_term_creation#new_term(Psi, Term, CtlH\CtlM, {DicI,DicM}, Ch, {L,M}),
	new_terms.

tell_unifications_body(Uns, Commit, DgBodies, Ctl, Fail, Dic, Ch, Name, SC, Done)
:-
	Uns = [], Commit = [commit|_], Ctl = CtlH\CtlT :
	CtlH ! commit, Fail = _ |
	bodies#bodies(DgBodies, CtlH'\CtlT, Dic, Ch, Name, SC, Done) ;

	Uns = [], Commit = [] : Fail = _ |
	bodies#bodies(DgBodies, Ctl, Dic, Ch, Name, SC, Done) ;

	Uns =\= [], Ctl = CtlH\CtlT, SC = {L,R}, Ch = {_,Ch1} |
 	allocate_terms(Uns, CtlH\CtlM1, Allocated, {Dic,Dic2}, UpDics\[], UnsO, 
	                                               Ch1, {L,M1}),
	output_args(UnsO, Derefs, Asgns, CtlM1\CtlM2, {Dic2,Dic3}, Ch1, {M1,M2}),
	unifications_body(DgBodies, Derefs, Asgns, Allocated, CtlM2\CtlT, 
	            Commit, Fail, Dic3, UpDics, Ch, Name, {M2,R}, Done).

allocate_terms(Uns, Ctl, Allocated, Dic, UpDics, UnsO, Ch, SC)
:-
	Uns = [], Ctl = CtlH\CtlT, Dic = {DicI,DicO},
	UpDics = UpH\UpT, SC = {L,R} :
	CtlH = CtlT, Allocated = [], DicO = DicI, UpH = UpT, UnsO = [],  
	L = R, Ch = _ ;

	Uns ? {Psi,Term}, 
	Ctl = CtlH\CtlT, Dic = {DicI,DicO},	UpDics = UpH\UpT, SC = {L,R} :
	Allocated ! Reg, UnsO ! {Psi,Ent}, 
	Ctl' = CtlM\CtlT, Dic' = {DicM,DicO}, UpDics' = UpM\UpT, SC' = {M,R} |
	tell_term_creation#term_creation(Term, {DicI,DicM}, Reg, 
	                                   CtlH\CtlM, UpH\UpM, Ent, Ch, {L,M}),
%	screen#display({Term,Reg},type(ground)),
	allocate_terms.

output_args(Uns, Derefs, Asgns, Ctl, Dic, Ch, SC)
:-
	Uns = [], Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	Derefs = [], Asgns = [], CtlH = CtlT, DicO = DicI, L = R, Ch = _ ;

	Uns ? {psi(Psi),TV}, Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	write_channel(look(Psi,Entry,DicI),Ch),
	Asgns ! {Psi,TV,Arg}, 
	Ctl' = CtlM\CtlT, Dic' = {DicM,DicO}, SC' = {M,R} |
	output_arg(Psi, Entry, Derefs, Derefs', Arg, 
	                                     CtlH\CtlM, {DicI,DicM}, Ch, {L,M}), 
	output_args.


output_arg(Psi, Entry, Derefs, DerefsOut, Reg, Ctl, Dic, Ch, SC)
:-
	Entry = {Areg,_,car,_},	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [deref_car(Areg,Reg)|CtlT],
	write_channel(update(Psi,{Reg,'*',ref,'*'},{DicM,DicO}),Ch),
	Derefs = [Reg|DerefsOut], L = R ;

	Entry = {Areg,_,car(var),_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
	write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [deref_car(Areg,Reg)|CtlT],
	write_channel(update(Psi,{Reg,'*',ref,'*'},{DicM,DicO}),Ch),
	Derefs = [Reg|DerefsOut], L = R ;

	Entry = {Areg,_,cdr,_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
        write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [deref(Areg,Reg)|CtlT],
	write_channel(update(Psi,{Reg,'*',ref,'*'},{DicM,DicO}),Ch),
	Derefs = [Reg|DerefsOut], L = R ;

	Entry = {Areg,_,sub_arg,_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
        write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [deref(Areg,Reg)|CtlT],
	write_channel(update(Psi,{Reg,'*',ref,'*'},{DicM,DicO}),Ch),
	Derefs = [Reg|DerefsOut], L = R ;

	Entry = {Areg,_,ref,_},	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = Areg, CtlH = CtlT, DicO = DicI,  
	Derefs = [Reg|DerefsOut], L = R, Psi = _, Ch = _ ;

	Entry = {Areg,_,deref,_}, Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R}:
	Reg = Areg, CtlH = CtlT, DicO = DicI,  
	Derefs = [Reg|DerefsOut], L = R, Psi = _, Ch = _ ;

	Entry = {Areg,_,var,_}, Areg = {a(_),_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
        write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [deref(Areg,Reg)|CtlT],
	write_channel(update(Psi,{Reg,'*',var,'*'},{DicM,DicO}),Ch),
	Derefs = DerefsOut, L = R ;

	Entry = {Areg,_,var,_}, Areg = a(_),
	Dic = {DicI,DicO}, Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = Areg, CtlH = CtlT, DicO = DicI, Ch = _,
	Derefs = DerefsOut, L = R, Psi = _ ;
	
	Entry = {Areg,_,ref(var),_}, Areg = {a(_),_},
	Ctl = CtlH\CtlT, Dic = {DicI,DicO}, SC = {L,R} :
        write_channel(get_reg(Reg,{DicI,DicM}),Ch),
	CtlH = [deref(Areg,Reg)|CtlT],
	write_channel(update(Psi,{Reg,'*',var,'*'},{DicM,DicO}),Ch),
	Derefs = DerefsOut, L = R ;

	Entry = {Areg,_,ref(var),_}, Areg = a(_), Ctl = CtlH\CtlT, SC = {L,R} :
	Reg = Areg, Derefs = DerefsOut, L = R ,
	CtlH = [deref(Areg,Areg)|CtlT], 
	write_channel(update(Psi,{Areg,'*',var,'*'},Dic),Ch).
	
unifications_body(DgBodies, Derefs, Asgns, Allocated, Ctl, Commit, Fail,
				Dic, UpDics, Ch, Name, SC, Done)
:-
	Derefs = [], Allocated =\= [], Asgns =\= [], Ctl = CtlH\CtlT, 
	SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1},
	Commit = []  : 
	CtlH ! multiple_assign(Allocated,Asgns1), 
	UpDics = _, Fail = _ |
	get_asgns_and_dic(Asgns, {Dic,Dic1}, Asgns1, Ch1, {L,M}, {DL,DM}),
	bodies#bodies(DgBodies, CtlH'\CtlT, Dic1, Ch, Name, {M,R}, {DM,DR}) ;

	Derefs = [], Allocated =\= [], Asgns =\= [], Ctl = CtlH\CtlT, 
	SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1},
	Commit = [commit|_]  : 
	CtlH' ! multiple_assign(Allocated,Asgns1), 
	CtlH ! commit, 
	UpDics = _, Fail = _ |
	get_asgns_and_dic(Asgns, {Dic,Dic1}, Asgns1, Ch1, {L,M}, {DL,DM}),
	bodies#bodies(DgBodies, CtlH''\CtlT, Dic1, Ch, Name, {M,R}, {DM,DR});

	Derefs =\= [],
	SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} |
	get_asgns_and_dic(Asgns, {Dic,Dic1}, Asgns1, Ch1, {L,M}, {DL,DM}),
	bodies#bodies(DgBodies, Ctl1\[], Dic1, Ch, Name, {M,M1}, {DM,DM1}),
	update_dic(UpDics, Dic1, Dic2, Ch1, {M1,M2}, {DM1,DM2}),	
	bodies#bodies(DgBodies, Ctl2\[], Dic2, Ch, Name, {M2,R1}, {DM2,DR}),
	compare_bodies(Ctl1, Ctl2, Ctl, Derefs, Allocated, Asgns1, 
							Commit, Fail, {R1,R}).

get_asgns_and_dic(Asgns, Dic, AsgnsO, Ch, SC, Done)
:-
	Asgns ? {Psi,TV,Arg}, Dic = {DicI,DicO}, SC = {L,R}, Done = {DL,DR} :
	AsgnsO ! Arg, Dic' = {DicM,DicO}, SC' = {M,R}, Done' = {DM,DR} |
	update_psi(Psi, TV, Arg, {DicI,DicM}, Ch, {L,M}, {DL,DM}),
	get_asgns_and_dic ;

	Asgns = [], Dic = {DicI,DicO}, SC = {L,R}, Done = {DL,DR} :
	AsgnsO = [], DicO = DicI, R = L, DR = DL, Ch = _.

update_psi(Psi, TV, Arg, Dic, Ch,  SC, Done)
:-
	TV = {Tp,Vl}, SC = {L,R}, Done = {DL,DR} :
	write_channel(update(Psi,{Arg,'*',Tp,Vl},Dic),Ch),
	R = L, DR = DL ;

	TV = {Reg,Tp,Vl}, SC = {L,R}, Done = {DL,DR} :
	write_channel(update(Psi,{Reg,'*',Tp,Vl},Dic),Ch),
	R = L, DR = DL, Arg = _.

compare_bodies(BodyCtl1, BodyCtl2, Ctl, Derefs, Allocated, Asgns, Commit, 
								Fail, SC) 
:-
	BodyCtl1 =\= BodyCtl2, Ctl = CtlH\CtlT, SC = {L,R}, 
	Commit = [commit|_] :
	CtlH = [deref_vars(Derefs,label(L1)),
		multiple_assign_and_commit_trail(Allocated,Asgns)|CtlM] |
	copy_body(BodyCtl1, CtlM, [label(L1),
				   unify(Asgns,Allocated,Fail),
				   commit|CtlM1], 
			{L,M}),
	copy_body(BodyCtl2, CtlM1, CtlT, {M,R}) ;

	BodyCtl1 =\= BodyCtl2, Commit = [],
	Ctl = CtlH\CtlT, SC = {L,R} :
	CtlH = [deref_vars(Derefs,label(L1)),
		multiple_assign_and_commit(Allocated,Asgns)|CtlM] |
	copy_body(BodyCtl1, CtlM, [label(L1),
				   unify(Asgns,Allocated,Fail),
				   commit|CtlM1], 
			{L,M}),
	copy_body(BodyCtl2, CtlM1, CtlT, {M,R}) ;

	BodyCtl1 = BodyCtl2, Ctl = CtlH\CtlT :
	CtlH = [unify(Asgns,Allocated,Fail),
		commit|CtlM], Derefs = _, Commit = _ |
	copy_body(BodyCtl2, CtlM, CtlT, SC).

copy_body(BodyCtl, Ctl, CtlT, SC)
:-
	BodyCtl ? I : Ctl ! I | copy_body ;
	BodyCtl = [], SC = {L,R} : Ctl = CtlT, L = R.

update_dic(UpDics, Dic, DicO, Ch, SC, Done)
:-
	UpDics ? {Psi,Type} :
	write_channel(update_type(Psi,Type,{Dic,Dic'}),Ch) |
	update_dic ;

	UpDics ? {Psi,Addr,Type} :
	write_channel(update(Psi,{Addr,'*',Type,'*'},{Dic,Dic'}),Ch) |
	update_dic ;

	UpDics = [], SC = {L,R}, Done = {DL,DR} :
	DicO = Dic, L = R, DL = DR, Ch = _ .

