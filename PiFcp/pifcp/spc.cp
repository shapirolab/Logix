/*
Precompiler for Pi Calculus procedures - Stochastic Pi Calculus Phase.

Bill Silverman, February 1999.

Last update by		$Author: bill $
		       	$Date: 2000/05/07 09:05:50 $
Currently locked by 	$Locker:  $
			$Revision: 2.1 $
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

    Procedure =?= [] :
      Mode = _,
      Terms ! (Atom? :- RHSS) |
	piutils#tuple_to_atom(LHS, Atom),
	output;

    Procedure =?= (Atom :- Communicate) :
      Procedure' = (Atom'? :- Communicate) |
	add_schedule_channel(Atom, Atom'),
	communication.

  communication(In, Delay, Terms, Mode, LHS, RHSS, Procedure) :-

    Procedure =?= (Atom :- Communicate1),
    arg(1, Atom, ProcName),
    string_to_dlist(ProcName, PNL, []) : Mode = _,
      Terms ! (LHS :- RHS?),
      Terms' ! (Atom'? :- Communicate2?) |
	utils#tuple_to_dlist(Atom, ADL, [`pifcp(count)]),
	utils#list_to_tuple(ADL?, Atom'),
	remake_rhs(RHSS, Prepares, RHSS'),
	reform_rhs(RHSS',
		write_channel(schedule(WaitList?), `pifcp(schedule)), RHS),
	piutils#untuple_predicate_list(';', Communicate1, CL1),
	make_reference_name,
	rewrite_rhss,
	piutils#make_predicate_list(';', CL2?, Communicate2),
	output.

  reform_rhs(RHSS, Schedule, RHS) :-

    RHSS =?= (Ask : Tell | Body) :
      RHS = (Ask : Schedule, Tell | Body);

    RHSS =?= (Ask | Body), Ask =\= (_ : _) :
      RHS = (Ask : Schedule | Body);

    RHSS  =\= (_ | _) :
      RHS = (true : Schedule | RHSS).


make_reference_name(PNL, RefName) :-

    PNL ? Dot :
    ascii('.', Dot) |
	self;

    otherwise,
    list_to_string(PNL, RefName^) |
	true.


remake_rhs(RHS1, Prepares, RHS2) :-

    RHS1 =\= (_ | _) :
      Prepares = {[], []},
      RHS2 = RHS1;

    RHS1 =?= (Asks : Tells | Body) :
      Prepares = {Asks'?, Tells'?} |
	piutils#untuple_predicate_list(',', Asks, Asks'),
	piutils#untuple_predicate_list(',', Tells, Tells'),
	extract_reads_and_writes,
	make_waits(IdWrites?, Body, Body'),
	complete_preparation.

  make_waits(IdWrites, OldBody, NewBody) :-

    IdWrites ? {_Identify, write_channel(PiMessage, _FcpChannel)},
    PiMessage =?= _Sender(_Message, ChoiceTag, _Choice) :
      NewBody = (pi_wait_to_send(`spsfcp(ChoiceTag), PiMessage),  NewBody'?) |
	self;

    IdWrites =?= [] :
      NewBody = OldBody.

  /* This procedure should be replaced if the monitor is enhanced to
     prepare the Receives.*/
  complete_preparation(Reads, Body, RHS2) :-

    Reads =?= [] :
      RHS2 = Body;

    otherwise :
      RHS2 = (PrepareReceives?, Body) |
	piutils#make_predicate_list(',', Reads, PrepareReceives).


extract_reads_and_writes(Asks, Tells, IdWrites, Reads) :-

    Asks ? Identify,
    Tells ? Write, Write = write_channel(_PiMessage, _FcpChannel) :
      IdWrites ! {Identify, Write} |
	self;

    Asks ? Identify,
    Tells ? true,
    Identify = (`ChannelName = _Tuple),
    Asks' ? Read,
    Read =?= read_vector(2, _FcpVector, Stream) :
      Reads ! pi_wait_to_receive(`sprfcp(ChannelName),`pifcp(chosen), Stream) |
	self;      

    Asks ? Identify,
    Tells =?= [],
    Identify = (`ChannelName = _Tuple),
    Asks' ? Read,
    Read =?= read_vector(2, _FcpVector, Stream) :
      Reads ! pi_wait_to_receive(`sprfcp(ChannelName),`pifcp(chosen), Stream) |
	self;      

    Asks =?= [] :
      Tells = _,
      IdWrites = [],
      Reads = [].


rewrite_rhss(ProcName, RefName, CL1, Prepares, WaitList, CL2) :-

    CL1 ? Receive, Receive =?= (Ask : Tell | Body), Body =\= self,
    Ask = (_Stream ? _Message, Identify, _We),
    Identify = (`ChannelName = _Creator(_FcpVector, _Arguments)) :
      WaitList ! receive(`ChannelName, `sprfcp(ChannelName)),
      CL2 ! (Ask : write_channel(terminate(RefName-ChannelName, `pifcp(count)),
					`pifcp(schedule)), Tell |
			Body) |
	self;

    CL1 ? Send, Send =?= (Ask | Body),
    Ask = (`pifcp(chosen) = _Index) :
      CL2 ! (Ask : write_channel(terminate(RefName, `pifcp(count)),
					`pifcp(schedule)) |
			Body) |
	self;

    CL1 ? Send, Send =?= (Ask : Tell | Body),
    Ask = (`pifcp(chosen) = _Index) :
      CL2 ! (Ask : write_channel(terminate(RefName, `pifcp(count)),
					`pifcp(schedule)), Tell |
			Body) |
	self;

    CL1 ? Other,
    otherwise :
      CL2 ! Other |
	self;

    CL1 =?= [],
    Prepares = {Asks, Tells} :
      ProcName = _,
      CL2 = [] |
	extract_reads_and_writes + (Reads = _),
	complete_waitlist + (EndList = RefName(`pifcp(count))).

  complete_waitlist(EndList, IdWrites, WaitList) :-
	
    IdWrites ? {(Channel = _PiChannel),
		write_channel(PiMessage, _FcpChannel)},
    PiMessage =?= {_Sender, _ChannelList, SendIndex, _ChoiceVariable} :
      WaitList ! send(Channel, `spsfcp(SendIndex)) |
	self;

    IdWrites =?= [] :
      WaitList = EndList.
