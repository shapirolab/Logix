/*
Precompiler for Pi Calculus procedures - Stochastic Pi Calculus Phase.

Bill Silverman, February 1999.

Last update by		$Author: bill $
		       	$Date: 2000/03/14 13:44:46 $
Currently locked by 	$Locker:  $
			$Revision: 1.3 $
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
**			none, no_guard,
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
**        ProcedureName.Mode(<arguments'>) :- <compound rhs>
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
	add_schedule_channel,
	stochastic;

    In = [] :
      Delay = _,
      Terms = [].


add_schedule_channel(Atom, LHS) :-

    string(Atom) :
      LHS = Atom(`pifcp(schedule));

    tuple(Atom) |
	utils#tuple_to_dlist(Atom, AL, [`pifcp(schedule)]),
	utils#list_to_tuple(AL, LHS).


stochastic(In, Delay, Terms, Mode, LHS, RHSS, Procedure) :-

    Mode =?= receive,
    Procedure =?= [] |
	create_receive_scheduler(LHS, RHSS, RHSS', Procedure'),
	communication;

    Mode =\= receive,
    Procedure =?= [] :
      Terms ! (Atom? :- RHSS) |
	piutils#tuple_to_atom(LHS, Atom),
	output;

    Mode =\= receive,
    Procedure =?= (Atom :- Communicate) :
      Procedure' = (Atom'? :- Communicate) |
	add_schedule_channel(Atom, Atom'),
	communication.

  communication(In, Delay, Terms, Mode, LHS, RHSS, Procedure) :-

    Procedure =?= (Atom1 :- Communicate1),
    arg(1, Atom1, ProcName),
    string_to_dlist(ProcName, PNL, []) : Mode = _,
      Terms ! (LHS :- RHS?),
      Terms' ! (Atom2? :- Communicate2?) |
	remake_rhs(RHSS, Sends, RHSS'),
	reform_rhs(RHSS',
		   write_channel(schedule(WaitList?), `pifcp(schedule)), RHS),
	piutils#untuple_predicate_list(';', Communicate1, CL1),
	make_end_waitlist,
	rewrite_rhss(CL1?, Sends?,  EndList, WaitList, WaitArguments, CL2),
	piutils#make_predicate_list(';', CL2?, Communicate2),
	utils#tuple_to_dlist(Atom1, AL1, [`pifcp(count) | WaitArguments?]),
	utils#list_to_tuple(AL1?, Atom2),
	output.

  reform_rhs(RHSS, Schedule, RHS) :-

    RHSS =?= (Ask : Tell | Body) :
      RHS = (Ask : Schedule, Tell | Body);

    RHSS =?= (Ask | Body), Ask =\= (_ : _) :
      RHS = (Ask : Schedule | Body);

    RHSS  =\= (_ | _) :
      RHS = (true : Schedule | RHSS).

  make_end_waitlist(PNL, EndList) :-

    PNL ? Dot :
    ascii('.', Dot) |
	self;

    otherwise :
      EndList = ProcName?(`pifcp(count)) |
	list_to_string(PNL, ProcName).


create_receive_scheduler(LHS, RHS1, RHS2, Procedure) :-

    arg(1, LHS, ProcedureName),
    string_to_dlist(ProcedureName, PL, Receive),
    string_to_dlist(".receive", RL, []) :
      Receive = RL,
      Procedure = (ReceiveLHS? :- RHS1) |
	list_to_string(PL, RHS2),
	utils#tuple_to_dlist(LHS, [_ProcedureName | Arguments],	[]),
	utils#list_to_tuple([RHS2 | Arguments?], ReceiveLHS).


remake_rhs(RHS1, Sends, RHS2) :-

    RHS1 =\= (_ | _) :
      Sends = {[], []},
      RHS2 = RHS1;

    RHS1 =?= (SendChannels : SendWrites | Body1) :
      RHS2 = Body2?,
      Sends = {Channels, Writes} |
	piutils#untuple_predicate_list(',', SendChannels, Channels),
	piutils#untuple_predicate_list(',', SendWrites, Writes),
	make_waits(Channels?, Writes?, Body2, Body1).

  make_waits(Channels, Writes, NewBody, OldBody) :-

    Channels ? (Channel = _PiChannel),
    Writes ? write_channel(PiMessage, _FcpChannel),
    PiMessage =?= _Sender(_Message, ChoiceTag, _Choice) :
      NewBody = (pi_wait_to_send(`spsfcp(ChoiceTag), PiMessage, Channel,
				 `sptfcp(ChoiceTag)),
		 NewBody'?) |
	self;

    Channels =?= [],
    Writes =?= [] :
      NewBody = OldBody.


rewrite_rhss(CL1, Sends, EndList, WaitList, WaitVariables, CL2) :-

    CL1 ? Receive, Receive =?= (Ask : Tell | Body), Body =\= self,
    Ask = (Channel = _Tuple, _We),
    Channel = `ChannelName, string(ChannelName) :
      WaitVariable = `sprfcp(ChannelName),
      CL2 ! (Ask'? : SetCount?, Tell | Body),
      WaitList ! receive(Channel, WaitVariable),
      WaitVariables ! WaitVariable |
	communication_guard_count((known(WaitVariable), Ask), Ask', SetCount),
	self;

    CL1 ? Send, Send =?= (Ask | Body),
    Ask = (`pifcp(chosen) = Index) :
      WaitVariable = `sptfcp(Index),
      CL2 ! (Ask'? : SetCount? | Body) |
	communication_guard_count((known(WaitVariable), Ask), Ask', SetCount),
	self;

    CL1 ? Send, Send =?= (Ask : Tell | Body),
    Ask = (`pifcp(chosen) = Index) :
      WaitVariable = `sptfcp(Index),
      CL2 ! (Ask'? : SetCount?, Tell | Body) |
	communication_guard_count((known(WaitVariable), Ask), Ask', SetCount),
	self;

    CL1 ? Other,
    otherwise :
      CL2 ! Other |
	self;

    CL1 =?= [],
    Sends = {Channels, Writes} :
      CL2 = [] |
	complete_waitlist.

  communication_guard_count(Ask1, Ask2, SetCount) :-

    Ask1 =?= (Predicate, Ask1') :
      Ask2 = (Predicate, Ask2'?) |
	self;

    Ask1 =\= (_,_) :
      Ask2 = (Ask1, info(4, `pifcp(creations))),
      SetCount = (`pifcp(count) = `pifcp(creations)).

  complete_waitlist(EndList, Channels, Writes, WaitList, WaitVariables) :-
	
    Channels ? (Channel = PiChannel), Channel =?= `_ChannelName,
    PiChannel = _ChannelId(FcpChannel, _Stream, _Receive, _Send),
    Writes ? write_channel(PiMessage, FcpChannel),
    PiMessage =?= {_Sender, _ChannelList, SendIndex, _ChoiceVariable} :
      WaitList ! send(Channel, `spsfcp(SendIndex)),
      WaitVariables ! `sptfcp(SendIndex) |
	self;

    Channels =?= [],
    Writes =?= [] :
      WaitList = EndList,
      WaitVariables = [] .
