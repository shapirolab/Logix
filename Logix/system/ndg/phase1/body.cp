/* $Header: /home/qiana/Repository/Logix/system/ndg/phase1/body.cp,v 1.1.1.1 1999/07/09 07:03:07 bill Exp $ */
-export([body/3,copytuple/5]).
-language(compound).
-mode(trust).

procedure body(Body, DgBodies, SC).

body(Body, DgBodies, SC)
:-
	Body ? BodyGoal,
	N := arity(BodyGoal),
	make_tuple(N,Goal),
	arg(1,BodyGoal,Pred),
	arg(1,Goal,GPred),
	SC = {Left,Right} :
	GPred = Pred,
	SC' = {Left',Right},
	DgBodies ! spawn(Goal) |
	     copytuple(BodyGoal, 1, N, Goal, {Left,Left'}),
	     self ;

	Body = [], SC = {L,R} : DgBodies = [], R = L.

copytuple(Tuple, Low, High, To, SC)
:-
	High-- > Low, SC = {L,R},
	arg(High,Tuple,Arg), arg(High,To,NewArg) : SC' = {L',R} |
	     copytuple1(Arg, NewArg, {L,L'}),
	     self ;

	High = Low, SC = {L,R} : L = R, Tuple = _, To = _.

copytuple1(A, NewA, SC)
:-
	A = integer(_), SC = {L,R} : NewA = A , L = R ;

	A = string(_), SC = {L,R} : NewA = A , L = R ;

	A = nil(_), SC = {L,R} : NewA = A , L = R ;

	A = real(_), SC = {L,R} : NewA = A , L = R ;

	A = tuple(Tuple),
        arity(Tuple,Ar),
	make_tuple(Ar,NewTuple) :
	NewA = tuple(NewTuple) |
	    copytuple(Tuple, 0, Ar, NewTuple, SC) ;

	A = list([Car|Cdr]), SC = {L,R} : NewA = list([CarArg|CdrArg]) |
	    copytuple1(Car, CarArg, {L,L'}),	
	    copytuple1(Cdr, CdrArg, {L',R}) ;

	A = variable(V), V =\= '_', SC = {L,R} : NewA = psi([A]), L = R ;

	A = ro(V), V = variable(_), SC = {L,R} : NewA = ro(psi([V])), L = R ;

	A = ro(V), V = psi(_), SC = {L,R} : NewA = A, L = R ;

	A = variable('_'), SC = {L,R} : NewA = A, L = R ;

	A = psi(_), SC = {L,R} : NewA = A, L = R.
