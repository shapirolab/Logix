/* $Header: /home/qiana/Repository/Logix/system/ndg/phase1/self.cp,v 1.1 1999/07/09 07:03:07 bill Exp $ */
-language(compound).
-scope(ndg).
-export([dgs/4,dts/4]).
-mode(trust).

Cp ::= {Dic,[{ClsId,Cls}]}.

ClsId ::= Integer.

Cls ::= {Ask,Tell,Body}.

Ix ::= {value(Psi),[{Unification,[{ClsId,Cls}]}],ClsIds} ;
       {Guard}.

ClsIds ::= [ClsId]. 

Unification ::= integer(Integer) ; string(String) ; real(Real) ; nil([]); 
	        list ; tuple/Integer.

Guard ::= {Any,Conns,Checks}.

Conns ::= [Integer].

Checks ::= [Integer].

Psi ::= psi([variable(Any)]) ; psi([Integer]).


procedure dgs(Procs, IxAlg, Dgs, SC).

dgs(Procs, IxAlg, Dgs, SC) 
:-	
	SC = {L,R} |
	make_channel(Ch, Req),
	dgs1(Procs, IxAlg, Dgs, {L,R}, Ch),
	guards#server(Req),
	close_ch(R, Ch).

close_ch(R,Ch) 
:- 
	R = done : close_channel(Ch) ;
	R = {_,done} : close_channel(Ch).

dgs1(Procs, IxAlg, Dgs, SC, Ch)
:-
	Procs ? procedure({P,A,I},Dic,F,S,Clss),
	SC = {L,R} :
	Dgs ! {{P,A,I},Dic,Dg},
	SC' = {M,R} |
	decision_graph#decision_graph({P,A}, Clss, suspend(F,S), IxAlg, Dg,
	                              Ch, {L,M}),
	dgs1 ;

	Procs = [], SC = {L,R} : Dgs = [], L = R, IxAlg = _, Ch = _.

procedure dts(Procs, IxAlg, Dgs, SC).

dts(Procs, IxAlg, Dgs, SC) 
:-
	Procs ? procedure({P,A,I},Dic,F,S,Clss),
	SC = {L,R} :
	Dgs ! {{P,A,I},Dic,Dg},
	SC' = {M,R} |
	decision_tree#decision_tree({P,A}, Clss, suspend(F,S), IxAlg, Dg,
	                              {L,M}),
	dts ;

	Procs = [], SC = {L,R} : Dgs = [], L = R, IxAlg = _.




