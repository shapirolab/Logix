/*
Break-point management of algorithmic debugger.

Yossi Lichtenstein, Peter Gerstenhaber

Last update by          $Author: bill $
			$Date: 1999/07/09 07:03:22 $
Currently locked by     $Locker:  $
			$Revision: 1.1.1.1 $
			$Source: /home/qiana/Repository/Logix/system/debug/breaks.cp,v $

Copyright (C) 1988, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([add/5,add/4,remove/5,derive/7,check_break/4,
	 check_remove/4,check_and_remove/5]).
-language(compound).
-mode(trust).

Goal	::= true ; Tuple ; String.
Body	::= Goal ; (Goal,Body) ; '?'.
Location::= Any.
Mode	::= print ; query.
Predicate ::= String ; Constant.
Pattern ::= Any. %frozen goal
Arity	::= Integer ; all.
PredArity::= Predicate; Predicate/Arity; Pattern.
Break	::= break(PredArity,Mode,Location).
NewBreak::= Break.
UserBreak::= break; break(Any); break(Any,Any); break(Any,Any,Any).
BreakItm::= {Location,Predicate/Arity,Mode}; {Location,Pattern,Mode}.
Breaks	::= [BreakItm].
NonEmpty_Breaks ::= [BreakItm | Breaks].
Circuit	::= Any - Any.
Answer	::= Mode ; dont_ask.
Error	::= display(Message, How); [].
Message ::= Any.
How	::= Any.
Ok	::= true; false.
Remove	::= remove(PredArity,Location).
NewRemove::= Remove.
UserRemove ::= remove ; remove(Any); remove(Any,Any).

/***********************************************************************/

/***********************************************************************/
procedure add(UserBreak, Breaks, Breaks, Circuit, Error).
add(Break,Breaks,NewBreaks,Circuit,Error) :-
	check_break(Ok,Break, Break', Error),
	call_if_ok(Ok, add(Break',Breaks,NewBreaks,Circuit)).

/***********************************************************************/
procedure check_break(Ok, UserBreak, NewBreak, Error).
check_break(Ok, Break, NewBreak, Error) :-
	Break = break |
		check_break(Ok, break(all/all,query,post), NewBreak, Error);

	Break = break(PredArity) |
		check_break(Ok, break(PredArity,query,post), NewBreak, Error);

	Break = break(PredArity, Mode) |
		check_break(Ok, break(PredArity,Mode, post), NewBreak, Error);

	Break = break(PredArity, Mode, Loc),
	constant(PredArity) |
		check_break(Ok, break(PredArity/all,Mode,Loc), NewBreak,Error);

	Break = break(Pred, Mode, Loc),
	tuple(Pred), Pred =\= _/_,
	    freeze(Pred, [], FrozenPred,_),
	    NewBreak = break(pattern(FrozenPred), Mode, Loc) |
		check_break_parm(Ok, Break, Error);

	Break = break(_Pred/_Arity, _Mode, _Loc) :
	    NewBreak = Break |
		check_break_parm(Ok, Break, Error).

/***********************************************************************/
procedure check_break_parm(Ok, Break, Error).
check_break_parm(Ok, Break, Error) :-
	Break = break(_PredArity, Mode, _Location),
	Mode =\= query, Mode =\= print :
	    Ok = false,
	    Error = display(['invalid break type, expected query or print' |
			Q]\Q, [list]);

	Break = break(_PredArity, _Mode, Location),
	Location =\= pre, Location =\= post :
	    Ok = false,
	    Error =display(['invalid location, expected pre or post' |
			Q] \ Q, [list]);

	otherwise :
	    Break = _,
	    Ok = true,
	    Error = [].

/***********************************************************************/
procedure call_if_ok(Ok, Goal).
call_if_ok(Ok, Call)  :-
	Ok   = true,
        Call = add(BP, Breaks, NewBreaks, Circuit) | 
		add(BP, Breaks, NewBreaks, Circuit);

	Ok   = true,
        Call = remove(BP, Breaks, NewBreaks, Circuit, Error) | 
		remove(BP, Breaks, NewBreaks, Circuit, Error);

	Ok =\= true,
	arg(3,Call,Breaks), arg(4,Call,Breaks^), arg(5,Call,Circuit) :
	  Circuit = _L - _R.


/***********************************************************************/
procedure add(Break,Breaks,NonEmpty_Breaks,Circuit).
add(Break,Breaks,NewBreaks,Circuit) :-
	Circuit = L - R,
	Break   = break(Predicate,_Mode,Location),
	Breaks  = [{Loc,Pred,_Mod} | _Breaks1],
	Predicate = Pred, Location = Loc :
	    NewBreaks = Breaks,
	    L = R ;

	Circuit = L - R,
	Break   = break(Predicate,Mode,Location),
	Breaks  = [{_Loc,Pred,_Mod} | _Breaks1],
	Predicate = all/all, Predicate =\= Pred :
	    NewBreaks = [{Location,all/all,Mode} | Breaks],
	    L = R ;

	Break   = break(Predicate,_Mode,_Location),
	Breaks  = [BreakPoint | Breaks'],
	BreakPoint = {_Loc,Pred,_M},
	Predicate  =\= Pred :
	    NewBreaks = [BreakPoint | NewBreaks'] |
		add;

	Break   = break(Predicate,_Mode,Location),
	Breaks  = [BreakPoint | Breaks'],
	BreakPoint = {Loc,Pred,_M},
	Predicate  = Pred, Location =\= Loc :
	    NewBreaks = [BreakPoint | NewBreaks'] |
		add;

	Circuit = L - R,
	Break   = break(Predicate,Mode,Location),
	Breaks  = [] :
	    NewBreaks = [ {Location,Predicate,Mode} ],
	    R = L.

/***********************************************************************/
procedure check_remove(Ok, UserRemove, NewRemove, Error).
check_remove(Ok, Remove, NewRemove, Error) :-
	Remove = remove |
		check_remove(Ok, remove(all/all,_), NewRemove, Error);

	Remove = remove(PredArity) |
		check_remove(Ok, remove(PredArity,_), NewRemove, Error);

	Remove = remove(PredArity, Loc),
	constant(PredArity) |
		check_remove(Ok, remove(PredArity/_,Loc), NewRemove,Error);

	Remove = remove(Pred, Loc),
	tuple(Pred), Pred =\= _/_,
	freeze(Pred, [], FrozenPred,_) :
	    NewRemove = remove(FrozenPred, Loc) |
		check_remove_parm(Ok, Remove, Error);

	Remove = remove(_Pred/_Arity, _Loc) :
	    NewRemove = Remove |
		check_remove_parm(Ok, Remove, Error);

	otherwise :
	    Ok = false,
	    Remove = NewRemove,
	    Error = display([invalid_format(Remove), " use 'help(remove)' ",
			     "for further information." | Q]\Q, [list]).

/***********************************************************************/
procedure check_remove_parm(Ok, Remove, Error).
check_remove_parm(Ok, Remove, Error) :-
	Remove = remove(_PredArity, Location),
	Location =\= pre, Location =\= post :
	    Ok = false,
	    Error =display(['invalid location, expected pre or post' |
			Q] \ Q, [list]) ;

	Remove = remove(_PredArity, Location),
	Location = pre :
	    Error = _,
	    Ok = true;

	Remove = remove(_PredArity, Location),
	Location = post :
	    Error = _,
	    Ok = true;

	Remove = remove(_PredArity, Location),
	var(Location) :
	    Error = _,
	    Ok = true.


/***********************************************************************/
procedure remove(Remove,Breaks,Breaks,Circuit,Error).

remove(Remove,Breaks,NewBreaks,Circuit,Error) :-
	Circuit = L - R,
	Breaks  = [{Loc,Pred,_Mode} | Breaks1],
	Remove  = remove(Predicate,Location) :
	    Pred = Predicate,
	    Location = Loc,
	    R = L,
	    Error = [] ,
	    NewBreaks = Breaks1;

	Circuit = L - R,
	Breaks  = [] :
	    NewBreaks = [],
	    R = L,
	    Error = display([break_point_not_found(Remove)|Q]\Q,[list]);

	otherwise,
	Breaks = [Break | Breaks'] :
	    NewBreaks = [Break | NewBreaks'] |
		remove.

/**********************************************************************/
procedure check_and_remove(Remove,Breaks,Breaks,Circuit,Error).
check_and_remove(Remove,Breaks,NewBreaks,Circuit,Error) :-
	check_remove(Ok, Remove, Remove', Error),
	call_if_ok(Ok,remove(Remove', Breaks, NewBreaks, Circuit, Error)).

/**********************************************************************/
procedure derive(Body,Location,Mode, Breaks, Breaks, Circuit,Error).
derive(Body, Loc, Mode, Breaks, BreaksOut, Circuit, Error) :-
	Circuit = L - R,
	Body    = '?' :
	    Loc = _, Mode = _,
	    BreaksOut = Breaks,
	    L = R,
	    Error = display(["Derive is only valid at a post reduction breakpoint."|Q]\Q, [list]);

	Circuit = L - R,
	Body = (First, Body'),
	arg(1,First,Pred), arity(First,Arity),RealArity := Arity-1 |
		add(break(Pred/RealArity,Mode,Loc), Breaks, Breaks',L-L'),
		derive(Body', Loc, Mode, Breaks',BreaksOut, L'-R, Error);

	Circuit = L - R,
	Body = (First, Body'),
	constant(First) |
		add(break(First/0,Mode,Loc), Breaks, Breaks',L-L'),
		derive(Body', Loc, Mode, Breaks',BreaksOut, L'-R, Error);

	Body =\= (_,_), Body =\= true,
	arg(1,Body,Pred), arity(Body,Arity), RealArity := Arity-1,
	Error = [] |
		add(break(Pred/RealArity,Mode,Loc), Breaks, BreaksOut,Circuit);

	constant(Body),Body =\= '?',
	Error = [] |
		add(break(Body/0,Mode,Loc), Breaks, BreaksOut,Circuit).


