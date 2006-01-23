/* $Header: /home/qiana/Repository/Logix/system/ndg/phase0/preproc.cp,v 1.2 2006/01/23 11:54:14 bill Exp $ */
-export([preproc/2]).
-language(compound).
-mode(trust).

%%%%%%%%	called by :	fcpcolon

AskOut ::= Ask.

procedure preproc(Procedures, ProcsOut).

preproc(Procedures, ProcsOut) :- 
	preproc1(Procedures, ProcsOut\[]).

procedure preproc1(Procs, ProcsOutL).

preproc1(Procs, ProcsOutL) :-

    Procs ? procedure({/,P,A},Clss),
    ProcsOutL = ProcsH \ ProcsT :
      ProcsH ! procedure({P,A,0},Failure,Suspend,ClssOut),
      ProcsOutL' = ProcsT' \ ProcsT |
	change_representation(Clss,Clss1),
	eliminate_equal(Clss1,Clss11),
	heads_unfold(Clss11,Clss2),
	eliminate_post_unification(Clss2,Clss3),
	rename_apart(Clss3,Clss4),
	preproc2({P,A,0}, Clss4, ClssOut, Failure, Suspend, ProcsH'\ProcsT'),
	self;

    Procs = [], ProcsOutL = H\T :
      H = T .

procedure change_representation(Clss,ClssOut).

change_representation(Clss,ClssOut) :-

    Clss ? {H,{A,T},B} :
      ClssOut ! {H1,{A1,T1},B1} |
	substitute#replace_reals(A, T, B, A0, T0, B0),
	change_goal(H,H1),
	change_list(A0,A1),
	change_list(T0,T1),
	change_list(B0,B1),
%	screen#display(preproc(A1,A11,A2,Replaces),type(ground)),
	self;

    Clss = [] :
      ClssOut = [].

procedure change_goal(Goal, GoalOut).

change_goal(Goal, GoalOut) :-

    tuple(Goal),
    ArG := arity(Goal),
    make_tuple(ArG,Tuple) :
      GoalOut = Tuple |
	change_goal1(ArG,Goal,GoalOut);

    string(Goal) :
      GoalOut = Goal .

procedure change_goal1(Ar, Goal, GoalOut).

change_goal1(Ar, Goal, GoalOut) :-

    Ar-- > 1,
    arg(Ar,Goal,GoalAr),
    arg(Ar,GoalOut,GoalOutAr) |
	change_term(GoalAr,GoalOutAr),
	self;

    Ar =< 1,
    arg(1,Goal,Functor),
    arg(1,GoalOut,FunctorOut) :
      FunctorOut = Functor .

procedure change_list(List, ListOut).

change_list(List, ListOut) :-

    List ? L :
    ListOut ! L1 |
	change_goal(L,L1),
	self;

    List = [] :
      ListOut = [].

procedure change_term(Term, TermOut).

change_term(Term, TermOut) :-

    integer(Term) :
      TermOut = integer(Term) ;

    string(Term) :
      TermOut = string(Term) ;

    Term = [] :
      TermOut = nil(Term) ;

    real(Term) :
      TermOut = real(Term) ;

    Term = [Car|Cdr] : 
      TermOut = list([CarOut|CdrOut]) |
	change_term(Car, CarOut),
	change_term(Cdr, CdrOut);

    tuple(Term),
    Ar := arity(Term) |
	change_tuple(Ar,Term,TermOut).


procedure change_tuple(Ar, Tuple, TupleOut).
procedure change_tuple(Ar, Tuple, TupleOut, Quote, QMark).

change_tuple(Ar, Tuple, TupleOut)+(Quote='`',QMark='?') :-

    Ar =\= 2,
    make_tuple(Ar,TupleOut1) : Quote = _, QMark = _,
      TupleOut = tuple(TupleOut1) |
	change_tuple_args(Ar, Tuple, TupleOut1);

    Ar = 2,
    Tuple = {Arg1,Arg2},
    Arg1 = '_var' : Quote = _, QMark = _,
      TupleOut = variable(Arg2) ;

    Ar = 2,
    Tuple = {Arg1,Arg2},
    Arg1 = '_ro' : Quote = _, QMark = _,
      TupleOut = ro(variable(Arg2)) ;

    Ar = 2,
    Tuple = {QMark,Arg2} : Quote = _,
      TupleOut = tuple({string('_ro'),Arg2Out}) |
	change_term(Arg2,Arg2Out) ;

    Ar = 2,
    Tuple = {Quote,Arg2},
    Arg2 = {QMark,Arg22} :
      TupleOut = tuple({string('?'),Arg22Out}) |
	change_term(Arg22,Arg22Out);

    Ar = 2,
    Tuple = {Quote,Arg2},
    Arg2 =\= {Quote,_},
    Arg2 =\= {QMark,_} :
      TupleOut = tuple({string('_var'),Arg2Out}) |
	change_term(Arg2,Arg2Out);

    Ar = 2,
    Tuple = {Quote,Arg2},
    Arg2 = {Quote,_Arg22} :
      TupleOut = tuple({string('`'),Arg2Out}) |
	strip_quote(Arg2,Arg2Out, Quote, QMark);

    Ar = 2,
    Tuple = {Arg1,Arg2},
    Arg1 =\= '_var',
    Arg1 =\= '_ro',
    Arg1 =\= '?',
    Arg1 =\= '`' : Quote = _, QMark = _,
      TupleOut = tuple({Arg1Out,Arg2Out}) |
	change_term(Arg1,Arg1Out),
	change_term(Arg2,Arg2Out).
	
procedure strip_quote(Arg, ArgOut, Quote, QMark).

strip_quote(Arg, ArgOut, Quote, QMark) :-

    Arg = {Quote,Arg'},
    Arg' = {Quote,_} :
      ArgOut = tuple({string('`'),ArgOut'}) |
	self;

    Arg = {Quote,Arg2},
    Arg2 = {QMark,Arg22} :
      ArgOut = tuple({QMark,Arg22Out}) |
	change_term(Arg22,Arg22Out);

    Arg = {Quote,Arg2},
    Arg2 =\= {Quote,_},
    Arg2 =\= {QMark,_} |
	change_term(Arg,ArgOut).

procedure change_tuple_args(Ar, Tuple, TupleOut).

change_tuple_args(Ar, Tuple, TupleOut) :-

    Ar-- > 0,
    arg(Ar,Tuple,TupleAr),
    arg(Ar,TupleOut,TupleOutAr) |
	change_term(TupleAr,TupleOutAr),
	self;

    Ar = 0 : Tuple = _, TupleOut = _ .

procedure eliminate_equal(Clss,ClssO).

eliminate_equal(Clss,ClssO) :-

    Clss ? {H,{A,T},B} :
      ClssO ! {H,{A1,T},B} | 
        equal_ask(A,A1),eliminate_equal;

    Clss = [] :
      ClssO = [] .

procedure equal_ask(Ask,AskO).

equal_ask(Ask,AskO) :-

    Ask ? {'=?=',X,Y} :
      AskO ! {'=',X,Y} |
	self;

    Ask ? A, A =\= {'=?=',_,_} :
      AskO ! A |
	self;

    Ask = [] :
      AskO = [] .

procedure eliminate_post_unification(Clss,ClssOut).

eliminate_post_unification(Clss,ClssOut) :-

    Clss ? {A,T,B} :
      ClssOut ! {AO,TO,BO} |
        get_post_unification(A, T, B, AO, TO, BO),
	self;

    Clss = [] :
      ClssOut = [].

procedure get_post_unification(Ask, Tell, Body, AskO, TellO, BodyO).

get_post_unification(Ask, Tell, Body, AskO, TellO, BodyO)
:-
	get_post(Ask, Sub, Ask'),
	get_post_un(Sub, Ask', Tell, Body, AskO, TellO, BodyO).

procedure get_post(Ask, Sub, AskO).

get_post(Ask, Sub, AskO)
:-
	Ask ? {'=',Var,Var1}, Var = variable(_), Var1 = variable(_) :
        Sub = [{Var1,Var}], AskO = Ask' ;

	Ask ? {'=',Var,Var1}, Var = psi(_), Var1 = variable(_) :
        Sub = [{Var1,Var}], AskO = Ask' ;

	Ask ? {'=',Var,Var1}, Var1 = psi(_), Var = variable(_) :
        Sub = [{Var,Var1}], AskO = Ask' ;

	Ask ? A, 
	A =\= {'=',variable(_),variable(_)},
	A =\= {'=',variable(_),psi(_)},
	A =\= {'=',psi(_),variable(_)} :
        AskO ! A |
        self ;

	Ask = [] : Sub = [], AskO = [].

procedure get_post_un(Sub, Ask, Tell, Body, AskO, TellO, BodyO).

get_post_un(Sub, Ask, Tell, Body, AskO, TellO, BodyO)
:-
	Sub = [] : AskO = Ask, TellO = Tell, BodyO = Body ;

	Sub =\= [] |
	substitute#substitute(Sub, {Ask,Tell,Body}, {Ask',Tell',Body'}),
	get_post(Ask', Sub', Ask''),
	self.

procedure heads_unfold(Clss,ClssOut).

heads_unfold(Clss,ClssOut) :- 

    Clss ? {H1,{A1,T1},B1},
    ArH := arity(H1) - 1 :
      ClssOut ! {A2,T2,B2} |
	replace_head_args(H1,ArH,A11\A1,Subs),
	head_unification(Subs,A12\A11,Subs'),
	substitute#substitute(Subs',{A12,T1,B1},{A2,T2,B2}),
	self;

    Clss = [] :
      ClssOut = [] .

procedure replace_head_args(H1,Ar,Ask,Subs).
procedure replace_head_args(H1,Ar,Ask,Subs,N,SubsT).

replace_head_args(H1,Ar,Ask,Subs) + (N=1,SubsT=[]) :-
    N++ =< Ar, Ask = AskH\AskT,
    arg(N',H1,H1N) :
      Ask' = AskM\AskT |
	replace_head_arg(H1N, N, AskH\AskM, Subs, Subs'),
	self;

    N > Ar, Ask = H\T : H1 = _,
      H = T,
      Subs = SubsT .

procedure replace_head_arg(H1N, N, Ask, Subs, SubsT).

replace_head_arg(H1N, N, Ask, Subs, SubsT) :-

    H1N = variable(X), X =\= '_', Ask = AskH\AskT :
      AskH = AskT,
      Subs = [{H1N,psi([N])}|SubsT] ;

    H1N = variable('_'), Ask = AskH\AskT : N = _,
      AskH = AskT,
      Subs = SubsT ;

    H1N =\= variable(_), Ask = AskH\AskT :
      AskH = [psi([N]) = H1N |AskT],
      Subs = SubsT .

procedure head_unification(Subs, Ask, SubsO).

head_unification(Subs, Ask, SubsO) :-

    Subs ? {Name,Psi}, Ask = AskH\AskT :
      Ask' = AskM\AskT |
	member_subs(Name, Psi, Subs', AskH\AskM, SubsO, SubsO'),
	self;

    Subs = [], Ask = AskH\AskT :
      SubsO = [],
      AskH = AskT .

procedure member_subs(Name, Psi, Subs, Ask, SubsO, SubsO1).

member_subs(Name, Psi, Subs, Ask, SubsO, SubsO1) :-
    Subs = [{Name,Psi1}|_], Ask = AskH\AskT :
      AskH = [Psi = Psi1 | AskT],
      SubsO = SubsO1 ;

    Subs  ? {Name1,_}, Name1 =\= Name |
	self;

    Subs = [], Ask = AskH\AskT :
      AskH = AskT,
      SubsO = [{Name,Psi}|SubsO1] .

procedure rename_apart(Clss1,Clss2).
procedure rename_apart(Clss1,Clss2,ClsN).

rename_apart(Clss1,Clss2) + (ClsN=1) :-

    Clss1 ? Cls1,
    ClsN++ :
      Clss2 ! Cls2 |
	rename_cls(ClsN,Cls1,Cls2),
	self;

    Clss1 = [] : ClsN = _,
      Clss2 = [] .

procedure rename_cls(ClsN,Cls1,Cls2).
procedure rename_cls(ClsN,Cls1,Cls2,VarN,Dic).

rename_cls(ClsN,Cls1,Cls2) + (VarN=1,Dic=[]) :-
    Cls1 = {A1,T1,B1} :
      Cls2 = {A2,T2,B2} |
	rename_list(A1,A2,ClsN,VarN,Dic,VarN2,Dic2),
	rename_list(T1,T2,ClsN,VarN2,Dic2,VarN3,Dic3),
	rename_list(B1,B2,ClsN,VarN3,Dic3,_,_).

procedure rename_list(L1,L2,ClsN,VarN,Dic,VarN1,Dic1).

rename_list(L1,L2,ClsN,VarN,Dic,VarN1,Dic1) :-

    L1 ? G1 :
      L2 ! G2 |
	rename_goal(G1,G2,ClsN,VarN,Dic,VarN',Dic'),
	self;

    L1 = [] : ClsN = _,
      L2 = [],
      VarN1 = VarN,
      Dic1 = Dic.

procedure rename_goal(G1,G2,ClsN,VarN,Dic,VarN1,Dic1).

rename_goal(G1,G2,ClsN,VarN,Dic,VarN1,Dic1) :-

    tuple(G1),
    ArG := arity(G1),
    make_tuple(ArG,Tuple) :
      G2 = Tuple |
	rename_goal1(ArG,G1,G2,ClsN,VarN,Dic,VarN1,Dic1);

    string(G1) : ClsN = _,
      G2 = G1,
      VarN1 = VarN,
      Dic1 = Dic .

procedure rename_goal1(Ar,G1,G2,ClsN,VarN,Dic,VarN1,Dic1).

rename_goal1(Ar,G1,G2,ClsN,VarN,Dic,VarN1,Dic1) :-

    Ar-- > 1,
    arg(Ar,G1,G1Ar),
    arg(Ar,G2,G2Ar) |
	rename_term(G1Ar,G2Ar,ClsN,VarN,Dic,VarN',Dic'),
	self;

    Ar = 1,
    arg(1,G1,Fun1),
    arg(1,G2,Fun2) : ClsN = _,
      Fun2 = Fun1,
      VarN1 = VarN,
      Dic1 = Dic .

procedure rename_term(Term1,Term2,ClsN,VarN,Dic,VarN1,Dic1).

rename_term(Term1,Term2,ClsN,VarN,Dic,VarN1,Dic1) :-

    Term1 = psi(X) : ClsN = _,
      Term2 = psi(X),
      VarN1 = VarN, Dic1 = Dic ;

    Term1 = integer(X) : ClsN = _,
      Term2 = integer(X),
      VarN1 = VarN,
      Dic1 = Dic ;

    Term1 = string(X) : ClsN = _,
      Term2 = string(X),
      VarN1 = VarN,
      Dic1 = Dic ;

    Term1 = real(X) : ClsN = _,
      Term2 = real(X),
      VarN1 = VarN,
      Dic1 = Dic ;

    Term1 = nil(X) : ClsN = _,
      Term2 = nil(X),
      VarN1 = VarN,
      Dic1 = Dic ;

    Term1 = list([Car|Cdr]) :
      Term2 = list([CarOut|CdrOut]) |
	rename_term(Car,CarOut,ClsN,VarN,Dic,VarN',Dic'),
	rename_term(Cdr,CdrOut,ClsN,VarN',Dic',VarN1,Dic1);

    Term1 = tuple(Tuple),
    Ar := arity(Tuple),
    make_tuple(Ar,TupleOut) :
      Term2 = tuple(TupleOut) |
	rename_tuple(Ar,Tuple,TupleOut,ClsN,VarN,Dic,VarN1,Dic1);

    Term1 = variable('_') : ClsN = _,
      Term2 = variable('_'),
      VarN1 = VarN,
      Dic1 = Dic ;

    Term1 = ro(X), X = psi(_) : ClsN = _,
      Term2 = ro(X),
      VarN1 = VarN,
      Dic1 = Dic;

    Term1 = ro(X), X =\= psi(_) :
      Term2 = ro(X1) |
	member(X,Dic,Name),
	rename_var(Name,X,X1,ClsN,VarN,Dic,VarN1,Dic1);

    Term1 = variable(X), X =\= '_' |
	member(Term1,Dic,Name),
	rename_var(Name,Term1,Term2,ClsN,VarN,Dic,VarN1,Dic1).

procedure rename_tuple(Ar,Tuple,TupleOut,ClsN,VarN,Dic,VarN1,Dic1).

rename_tuple(Ar,Tuple,TupleOut,ClsN,VarN,Dic,VarN1,Dic1) :-

    Ar-- > 0,
    arg(Ar,Tuple,TupleAr),
    arg(Ar,TupleOut,TupleOutAr) |
	rename_term(TupleAr,TupleOutAr,ClsN,VarN,Dic,VarN',Dic'),
	self;

    Ar = 0 : Tuple = _, TupleOut = _, ClsN = _,
      VarN1 = VarN,
      Dic1 = Dic .

procedure rename_var(Name,X,X1,ClsN,VarN,Dic,VarN1,Dic1).

rename_var(Name,X,X1,ClsN,VarN,Dic,VarN1,Dic1) :-

    Name = no,
    VarN++ :
      X1 = variable({'Z',ClsN,VarN}),
      Dic1 = [{X,X1}|Dic],
      VarN1 = VarN' ;

    Name =\= no : X = _, ClsN = _,
      X1 = Name,
      VarN1 = VarN,
      Dic1 = Dic .

procedure member(X,Dic,Name).

member(X,Dic,Name) :-

    Dic = [{X,X1}|_] :
      Name = X1 ;

    Dic ? {X1,_}, X =\= X1 |
	self;

    Dic = [] : X = _,
      Name = no .


procedure preproc2(ProcOut, Clss, ClssOut, Failure, Suspend, ProcsOutL).

preproc2(ProcOut, Clss, ClssOut, Failure, Suspend, ProcsOutL) :-

    Clss = [{A,_,_}|_] |
	search_ow(A,Ow,St),
	preproc3(St, Ow, ProcOut, Clss, ClssOut, Failure, Suspend, ProcsOutL);

    Clss = [], ProcsOutL = PH\PT : ProcOut = _,
      ClssOut = [],
      Failure = fail,
      Suspend = suspend,
      PH = PT .

procedure search_ow(Ask, Ow, St).

search_ow(Ask, Ow, St) :-

    Ask ? otherwise :
      Ow = Ask',
      St = yes ;

    Ask ? A, A =\= otherwise :
      Ow ! A |
	self;

    Ask = [] :
      Ow =[],
      St =no .

procedure preproc3(St,Ow, ProcOut, Clss, ClssOut, Failure, Suspend, ProcsOutL).

preproc3(St,Ow, ProcOut, Clss, ClssOut, Failure, Suspend, ProcsOutL) :-

    St = no,
    Clss ? C : Ow = _,
      ClssOut ! C |
	preproc2(ProcOut, Clss', ClssOut', Failure, Suspend, ProcsOutL);

    St = yes,
    Clss ? {_,T,B}, 
    ProcOut = {P,A,I},
    ProcsOutL = PH \ PT :
      ClssOut = [],
      Failure = {P,A,I1},
      PH ! procedure(Failure,Failure1,Suspend,[{Ow,T,B}]) |
	I1 := I + 1,
	preproc4(Failure, Clss', Failure1, Suspend, PH'\PT).

procedure preproc4(ProcOut, Clss, Failure, Suspend, ProcsOutL).

preproc4(ProcOut, Clss, Failure, Suspend, ProcsOutL) :-

    Clss = [],
    ProcsOutL = PH \ PT : ProcOut = _,
      Failure = fail,
      Suspend = suspend,
      PH = PT ;

    Clss = [{Ask,_,_}|_],
    ProcOut = {P,A,I} :
      Failure = {P,A,I1} |
	I1 := I + 1,
	search_ow(Ask,Ow,St),
	preproc5(St, Ow, Failure, Clss, Suspend, ProcsOutL).

procedure preproc5(St, Ow, ProcOut, Clss, Suspend, ProcsOutL).

preproc5(St, Ow, ProcOut, Clss, Suspend, ProcsOutL) :-

    St = no,
    Clss ? C,
    ProcsOutL = PH\PT : Ow = _,
      Suspend = ProcOut,
      ClssOut ! C,
      PH ! procedure(ProcOut,Failure,Suspend1,ClssOut) |
	preproc2(ProcOut, Clss'?, ClssOut', Failure, Suspend1, PH'\PT);

    St = yes,
    Clss ? {_,T,B}, 
    ProcsOutL = PH \ PT :
      PH ! procedure(ProcOut,Failure,Suspend,[{Ow,T,B}]) |
	preproc4(ProcOut, Clss', Failure, Suspend, PH'\PT).
