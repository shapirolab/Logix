/*
Precompiler for Pi Calculus procedures - Stochastic Pi Calculus Phase.

Bill Silverman, February 1999.

Last update by		$Author: bill $
		       	$Date: 2000/03/07 11:52:28 $
Currently locked by 	$Locker:  $
			$Revision: 1.2 $
			$Source: /home/qiana/Repository/PiFcp/pifcp/spc.cp,v $

Copyright (C) 2000, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export(output/3).
-mode(interpret).
-language(compound).

/*
** output/3
**
** Input:
**
**   In is a stream of  Mode(Atom, RHSS, Procedure).
**
**      Mode is one of {export,
**			no_guard, none?,
**			compare, logix,
**			receive, send, mixed}.
**
**      Atom is an Fcp atom of a compound procedure:
**
**        ProcedureName(<arguments>)
**  or
**        ProcedureName
**
**      RHSS is the right-hand-side of the compound procedure.
**
**      Procedure is [] or the compound procedure's communication part.
**
**        ProcedureName.Mode<arguments'>)
**
**      where Mode is in {send, mixed}.
**
**   Delay is one of {none, stochastic}.
**
** Output:
**
**   Terms is a stream of compound procedures.
*/

output(In, Delay, Terms) :-

    Delay =?= none,
    In ? Mode(Atom, RHSS, []),
    Mode =\= conflict :
      Terms ! (Atom :- RHSS) |
	self;

    Delay =?= none,
    In ? Mode(Atom, RHSS, Procedure), Procedure =\= [],
    Mode =\= conflict :
      Terms ! (Atom :- RHSS),
      Terms' ! Procedure |
	self;

    /* Discard conflicted code. */
    In ? conflict(_Atom, _RHSS, _Procedure) |
	self;

    Delay =?= stochastic,
    In ? export(Atom, RHSS, _Procedure) :
      Terms ! (Atom :- RHSS) |
	self;

    Delay =?= stochastic,
    In ? Mode(Atom, RHSS, Procedure),
    Mode =\= export, Mode =\= conflict |
	atom_to_lhs,
	stochastic;

    In = [] :
      Delay = _,
      Terms = [].


atom_to_lhs(Atom, LHS) :-

    string(Atom) :
      LHS = Atom(`spifcp(schedule));

    tuple(Atom) |
	utils#tuple_to_dlist(Atom, AL, [`spifcp(schedule)]),
	utils#list_to_tuple(AL, LHS).


stochastic(In, Delay, Terms, Mode, LHS, RHSS, Procedure) :-

    Mode =?= receive,
    Procedure =?= [] |
	receive_scheduler(LHS, RHSS, RHSS', Procedure'),
	communication;

    Mode =\= receive,
    Procedure =?= [] :
      Terms ! (Atom? :- RHSS) |
	piutils#tuple_to_atom(LHS, Atom),
	output;

    Mode =\= receive,
    Procedure =?= (Atom :- Communicate) :
      Procedure' = (Atom' :- Communicate) |
	atom_to_lhs(Atom, Atom'),
	communication.

  communication(In, Delay, Terms, Mode, LHS, RHSS, Procedure) :-

    Procedure =?= (Atom1 :- Communicate1) :
      Mode = _,
      Terms ! (LHS :- RHS?),
      Terms' ! (Atom2? :- Communicate2?) |
	reform_rhs(RHSS, write_channel(schedule(WaitList?), `spifcp(schedule)),
			RHS),
	piutils#untuple_predicate_list(';', Communicate1, CL1),
	get_sends(RHSS, Sends),
	rewrite_rhss(CL1?, Sends?, WaitList, WaitArguments, CL2),
	piutils#make_predicate_list(';', CL2?, Communicate2),
	utils#tuple_to_dlist(Atom1, AL1, WaitArguments?),
	utils#list_to_tuple(AL1?, Atom2),
	output.

  reform_rhs(RHSS, Schedule, RHS) :-

    RHSS =?= (Ask : Tell | Body) :
      RHS = (Ask : Schedule, Tell | Body);

    RHSS  =\= (_ | _) :
      RHS = (true : Schedule | RHSS).


receive_scheduler(LHS, RHS1, RHS2, Procedure) :-

    arg(1, LHS, ProcedureName),
    string_to_dlist(ProcedureName, PL, Receive),
    string_to_dlist(".receive", RL, []) :
      Receive = RL,
      Procedure = (ReceiveLHS? :- RHS1) |
	list_to_string(PL, RHS2),
	utils#tuple_to_dlist(LHS, [_ProcedureName | Arguments], []),
	utils#list_to_tuple([RHS2 | Arguments?], ReceiveLHS).


get_sends(RHS, Sends) :-

    RHS =\= (_ | _) :
      Sends = {[], []};

    RHS =?= (SendChannels : SendWrites | _Body) :
      Sends = {Channels, Writes} |
	piutils#untuple_predicate_list(',', SendChannels, Channels),
	piutils#untuple_predicate_list(',', SendWrites, Writes).


rewrite_rhss(CL1, Sends, WaitList, WaitVariables, CL2) :-

    CL1 ? Receive, Receive =?= (Ask : Tell | Body), Body =\= self,
    Ask = (Channel = _Tuple, _We),
    Channel = `ChannelName, string(ChannelName) :
      WaitVariable = `spwfcp(ChannelName),
      CL2 ! (known(WaitVariable), Ask : Tell | Body),
      WaitList ! receive(Channel, WaitVariable),
      WaitVariables ! WaitVariable |
	self;

    CL1 ? Send, Send =?= (Ask : Tell | Body),
    Ask = (`pifcp(chosen) = Index) :
      WaitVariable = `spwfcp(Index),
      CL2 ! (known(WaitVariable), Ask : Tell | Body) |
	self;

    CL1 ? Other,
    otherwise :
      CL2 ! Other |
	self;

    CL1 =?= [],
    Sends = {Channels, Writes} :
      CL2 = [] |
	complete_waitlist.

  complete_waitlist(Channels, Writes, WaitList, WaitVariables) :-
	
    Channels ? (Channel = _Tuple), Channel =?= `_ChannelName,
    Writes ? write_channel(PiMessage, _FcpChannel),
    PiMessage =?= {_Sender, _ChannelList, SendIndex, _ChoiceVariable} :
      WaitVariable = `spwfcp(SendIndex),
      WaitList ! send(Channel, WaitVariable),
      WaitVariables ! WaitVariable |
	self;

    Channels =?= [],
    Writes =?= [] :
      WaitList = [],
      WaitVariables = [].
