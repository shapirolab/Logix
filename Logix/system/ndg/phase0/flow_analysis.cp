/* $Header: /home/qiana/Repository/Logix/system/ndg/phase0/flow_analysis.cp,v 1.1.1.1 1999/07/09 07:03:06 bill Exp $ */
-export([flow_analysis/2]).
-language(compound).
-mode(trust).

procedure flow_analysis(Procs, ProcsOut).

flow_analysis(Procs, ProcsOut)
:-
	Procs ? procedure(Name,Failure,Suspend,ClssOut) :
        ProcsOut ! 
	  procedure(Name,{[],CaseSet, AskSet,TellSet,ImmSet},
				Failure,Suspend,ClssOut)|
	      get_sets(ClssOut, CaseSet, AskSet, TellSet, ImmSet),
              self ;
	
	Procs = [] : ProcsOut = [].

get_sets(ClssOut, CaseSet, AskSet, TellSet, ImmSet)+
				(N=0, Case = [], Ask=[], Tell=[], Imm=[])
:-
    ClssOut ? {A,T,B},
    N++ |
	sort_ask(A, A'),
	get_case(A', [], CasePsis, VarPos, Rest),
	add_psis(CasePsis, Case, Case'),
	get_psis_set(Rest, VarPos, Ask, Ask'),
	get_psis_set(T, VarPos, Tell, Tell'),
	get_psis_set(B, VarPos, Imm, Imm'),
	self ;

    ClssOut = [] |
	compute_set(Case, CaseSet, N),
	compute_set(Ask, AskSet, N),
	compute_set(Tell, TellSet, N),
	compute_set(Imm, ImmSet, N).

sort_ask(Ask, AskOut)+(AskRest=[])
:-
	Ask ? U, U = {'=',Psi,T}, Psi = psi(_), T =\= psi(_) : 
	AskOut ! U | self ;

	Ask ? U, U = {'=',T,Psi}, Psi = psi(_), T =\= psi(_) : 
	AskOut ! {'=',Psi,T} | self ;

	Ask = [] : AskOut = AskRest ;
	
	otherwise, Ask ? A : AskRest' = [A|AskRest] | self.
 
get_case(A, Case, CaseO, VarPos, Rest)+(VarPosI=[])
:-
	A ? U, U = {'=',Psi,T}, Psi = psi(L), T =\= psi(_) |
	get_var_pos(Psi, L, T, VarPosI, VarPosI', A', A'', Case, Case'),
	sort_ask(A'', A'''),
	self ;

	A = [] : CaseO = Case, VarPos = VarPosI, Rest = [] ;

	otherwise : Rest = A, CaseO = Case, VarPos = VarPosI.

get_var_pos(Psi, Pos, Term, VarPosI, VarPosO, AskI, AskO, CasePosI, CasePosO)
:-
	Term = integer(_) :
	VarPosO = VarPosI, AskO = AskI, CasePosO = [Psi|CasePosI], Pos = _ ;

	Term = real(_) :
	VarPosO = VarPosI, AskO = AskI, CasePosO = [Psi|CasePosI], Pos = _ ;

	Term = string(_) :
	VarPosO = VarPosI, AskO = AskI, CasePosO = [Psi|CasePosI], Pos = _ ;

	Term = nil(_) :
	VarPosO = VarPosI, AskO = AskI, CasePosO = [Psi|CasePosI], Pos = _ ;

	Term = psi(_) :
	VarPosO = VarPosI, AskO = AskI, CasePosO = [Psi,Term|CasePosI], Pos =_;

	Term = list([Car|Cdr]) |
	get_var_pos(psi([1|Pos]), [1|Pos], Car, 
		VarPosI, VarPosI', AskI, AskI', [Psi|CasePosI], CasePosI'),
	get_var_pos(psi([2|Pos]), [2|Pos], Cdr, 
			VarPosI', VarPosO, AskI', AskO, CasePosI', CasePosO) ;

	Term = tuple(Tup), arity(Tup,Ar) |
	get_tuple_pos(Tup, Ar, Pos, 
		VarPosI, VarPosO, AskI, AskO, [Psi|CasePosI], CasePosO) ;

	Term = variable(_) :
	VarPosO = [{Term,Psi}|VarPosI], CasePosO = CasePosI |
	replace_var_pos(Term, psi(Pos), AskI, AskO).

get_tuple_pos(Tup, Ar, Pos, VarPosI, VarPosO, AskI, AskO, CasePosI, CasePosO)
:-
    Ar-- > 0, arg(Ar,Tup, TupAr) |
	get_var_pos(psi([Ar|Pos]), [Ar|Pos], TupAr, 
			VarPosI, VarPosI', AskI, AskI', CasePosI, CasePosI'),
	self ;

    Ar = 0 : 
	VarPosO = VarPosI, AskO = AskI, CasePosO = CasePosI,
	Tup = _, Pos = _.

replace_var_pos(Var, Psi, AskI, AskO)
:-
	AskI ? Var = Term, Term =\= psi(_), Term =\= variable(_) :
	AskO ! Psi = Term |
	self ;
	
	AskI ? Term = Var, Term =\= psi(_), Term =\= variable(_) :
	AskO ! Psi = Term |
	self ;

	AskI = [] : AskO = [], Var = _, Psi = _ ;

	otherwise, AskI ? A : AskO ! A | self.

compute_set(Psis, Set, N)
:-
	Psis ? {Psi,N1}, N2 := (N1*100)/N : Set ! {Psi,N2} | 
				self ;
	Psis = [] : Set = [], N = _.

get_psis_set(A, VarPos, Set, SetO)
:-
	get_psis(A, VarPos, Psis),
	add_psis(Psis, Set, SetO).

get_psis(As, VarPos, Psis)+(PsisA=[])
:-
	As ? Psi1 = Psi2 |
	get_psis1([Psi1,Psi2], VarPos, PsisA, PsisA'),
	self ;

	As ? {_,Psi} |
	get_psis1([Psi], VarPos, PsisA, PsisA'),
	self ;

	As ? Psi1 < Psi2 |
	get_psis1([Psi1,Psi2], VarPos, PsisA, PsisA'),
	self ;

	As ? Psi1 =< Psi2 |
	get_psis1([Psi1,Psi2], VarPos, PsisA, PsisA'),
	self ;

	As ? Psi1 >= Psi2 |
	get_psis1([Psi1,Psi2], VarPos, PsisA, PsisA'),
	self ;

	As ? Psi1 > Psi2 |
	get_psis1([Psi1,Psi2], VarPos, PsisA, PsisA'),
	self ;

	As ? Psi1 =:= Psi2 |
	get_psis1([Psi1,Psi2], VarPos, PsisA, PsisA'),
	self ;

	As ? plus(Psi1,Psi2,Psi3) |
	get_psis1([Psi1,Psi2,Psi3], VarPos, PsisA, PsisA'),
	self ;

	As ? diff(Psi1,Psi2,Psi3) |
	get_psis1([Psi1,Psi2,Psi3], VarPos, PsisA, PsisA'),
	self ;

	As ? spawn(plus(Psi1,Psi2,Psi3)) |
	get_psis1([Psi1,Psi2,Psi3], VarPos, PsisA, PsisA'),
	self ;

	As ? spawn(diff(Psi1,Psi2,Psi3)) |
	get_psis1([Psi1,Psi2,Psi3], VarPos, PsisA, PsisA'),
	self ;

	As ? spawn({'=',Psi1,Psi2}) |
	get_psis1([Psi1,Psi2], VarPos, PsisA, PsisA'),
	self ;

	As = [] : Psis = PsisA, VarPos = _ ;

	otherwise, As ? _ | self.

get_psis1(Psis, VarPos, PsisA, PsisO)
:-
	Psis ? Psi, Psi = psi(_) | 
	member(Psi, PsisA, PsisA'),
	self;

	Psis ? list([Car|Cdr]) |
	get_psis1(Car, VarPos, PsisA, PsisA'),
	get_psis1(Cdr, VarPos, PsisA', PsisA''),
	self;

	Psis ? tuple(Tup) |
	arity(Tup, N),
	get_tup_psis(Tup, N, VarPos, PsisA, PsisA'),
	self;

	Psis ? V, V = variable(_) |
	get_psi(V, VarPos, Psi),
	member(Psi, PsisA, PsisA'),
	self;

	Psis = variable(_) |
	get_psi(Psis, VarPos, Psi),
	member(Psi, PsisA, PsisO) ;

	Psis = psi(_) : VarPos = _ | member(Psis, PsisA, PsisO) ;

	Psis = [] : PsisO = PsisA, VarPos = _ ;

	otherwise, Psis ? _ | self;

	otherwise : PsisO = PsisA, Psis = _, VarPos = _.

get_psi(Var, VarPos, Psi)
:-
	VarPos = [{Var,P}|_] : Psi = P ;
	VarPos ? {V,_}, V =\= Var | self ;
	VarPos = [] : Psi = no, Var = _.

get_tup_psis(Tup, N, VarPos, PsisA, PsisO)
:-
    N-- > 0,
    arg(N, Tup, ArgN) |
	get_psis1(ArgN, VarPos, PsisA, PsisA'),
	self ;

    N = 0 : PsisO = PsisA, Tup = _, VarPos = _.

add_psis(Psis, Ask, AskO)
:-
	Psis ? Psi |
	member_ask(Psi, Ask, Ask'),
	self ;
	
	Psis = [] : AskO = Ask.

member_ask(Psi, Ask, AskO)
:-
	Ask ? {Psi,N}, N1 := N + 1 : AskO = [{Psi,N1}|Ask'] ;
	Ask ? A, A =\= {Psi,_} : AskO ! A | member_ask ;
	Ask = [] : AskO = [{Psi,1}].

member(Psi, Ls, LsO)
:-
	Psi = no : LsO = Ls ;
	Psi =\= no | member1(Psi, Ls, LsO).

member1(Psi, Ls, LsO)
:-
	Ls = [Psi|_] : LsO = Ls ;
	Ls ? L, L =\= Psi : LsO ! L | self ;
	Ls = [] : LsO = [Psi].
