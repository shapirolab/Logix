/*
Meta-Interpreter of algorithmic debugger.

Yossi Lichtenstein, Peter Gerstenhaber

Last update by          $Author: bill $
			$Date: 1999/07/09 07:03:21 $
Currently locked by     $Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/system/debug/reduce.cp,v $

Copyright (C) 1988, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([debug/6]).
-language(compound).
-mode(trust).

Goal	::= Tuple ; String ; List.
RPCGoal ::= {`'#', Module, Goal}.
Goals	::= Goal - Goal.
NewGoal	::= Goal.
Body    ::= Goal.
Id	::= Number.
Id_Abort::= Id ; 'Abort'.
Time	::= Integer.
Trace	::= (Trace,Trace) ; true ; execute ;
	    trace(Id,Time,Any,Children,Parent).
Children::= Trace.
Parent	::= Trace.
Depth	::= Integer.
NewDepth::= Depth.
Increment ::= Depth.
Breaks	::= [Any].
Context ::= [] ; Vector.
Open_Context ::= {String, Context-Context,String-String}.
Element	::= disabled([Goal]) ; enabled([Goal]) ; resolvent([Goal]).
Empty_Element::= disabled([]) ; enabled([]) ; resolvent([]).
Elements     ::= [Element].
MoreElements ::= Signal; Element.
Commands::= [MoreElements].
Non_Empty_Commands ::= [MoreElements | Commands].
Choke_Msg_On_Right ::= Elements - [Empty_Element | Elements].
Circuit	::= [Element] - [Element].
Signals	::= [Signal].
Signal	::= suspend ; resume ; Depth ; ValidCommand.
ValidCommand ::= clear ; Break ; Remove.
NewCommand   ::= ValidCommand.
Break	::= break(Any,QueryPrint,PrePost).
Remove	::= remove(Any,PrePost).
QueryPrint ::= query ; print.
PrePost	::= pre ; post.
Goal_Info ::= {Goal, Open_Context}.
Debug_Info ::= {Breaks, Depth, Execute_or_Interpret, Channels}.
NewDebug_Info ::= Debug_Info.
Channel	::= Vector.
Channels::= {IO, User}.
IO	::= Channel.
User	::= Channel.
Execute_or_Interpret ::= execute; interpret.
Trace_Info ::= {Trace, Trace}.
State	::= started; terminated; running; suspended.
Events  ::= [ String ].
Event   ::= State; failed(Any,Any).
SubComputation_Events ::= [ Event ].
Ok	::= true ; false.
Command	::= clear; AnyBreak; AnyRemove.
AnyBreak ::= break ; break(Any) ; break(Any,Any) ; break(Any,Any,Any).
AnyRemove::= remove ; remove(Any) ; remove(Any,Any).
Where	::= reduce	 ; perform(Done,Id) ; wait(Return_Info, Done).
Error   ::= display(Any,List); [].
Done	::= done.
Return_Info ::= {Body, Id_Abort, Time}.
Clause	  ::= clause(Goal, Goal, Id, Time).
SubComputation ::= {Goal, IO, List}.
SubInput::= List.
Module	::= String.
Message	::= display(Goal,[Any]); ask([Any],[Any]\[Any],[Any]).

/*************************************************************************/
procedure debug(RPCGoal, Breaks, Commands, Trace, IO, Events).
% {debug / 6, [no([], debug(_, _, _, _, _, _)), redundant(1, User_Input)]}
debug(RPCGoal, Breaks, Commands, Trace, IO, Events) :-
	Circuit = _L - _R,
	Channels = {IO, User},
	make_channel(User,User_Input),
	user#monitor(User_Input),

	report_state(Commands, Circuit, Channels, Events),
	monitor(Commands,Signals,Circuit,Channels),
	make_channel(Context, Stream),
	computation # (self#Stream?),
	close(Self, Signals, Context),
	reduce(Signals, Circuit
		,{RPCGoal,{self,Context-_,self-Self}}
		,{Breaks, -1, interpret, Channels}
		,{Trace,true}
	).

/*************************************************************************/
procedure report_state(Commands,Circuit,Channels,Events).
procedure report_state(Commands,Circuit,Channels,Events,State).
report_state(Commands,Circuit,Channels,Events)+(State=started) :-
	Commands = [] :
	    Circuit = _, State=_,
	    Events = ["Debug Reduction Aborted"] |
		close_channels(Channels);

	Circuit = L-R, L =?= R :
	    Commands = _, State = _,
	    Events = ["Debug Reduction Terminated"] |
		close_channels(Channels);

	State = started :
	    Events!"Debug Reduction Started",
	    State' = running |
		report_state;

	Circuit = L-R,
	L?_Junk :
	    Circuit' = L'-R |
		report_state;

	Circuit = L-R,
	R?_Junk :
	    Circuit' = L-R' |
		report_state;

	Commands?suspend,
	State = running : 
	    Events!"Debug Reduction Suspended",
	    State' = suspended |
		report_state;

	Commands?resolvent(_Resolvent_List),
	State = running : 
	    Events!"Debug Reduction Suspended",
	    State' = suspended |
		report_state;

	Commands?enabled(_Enabled_List),
	State = running : 
	    Events!"Debug Reduction Suspended",
	    State' = suspended |
		report_state;

	Commands?disabled(_Disabled_List),
	State = running :
	    Events!"Debug Reduction Suspended",
	    State' = suspended |
		report_state;

	Commands?resume,
	State = suspended :
	    Events!"Debug Reduction Resumed",
	    State' = running |
		report_state;

	Commands?suspend,
	Channels = {IO,User},
	State = suspended :
	    Channels' = {IO', User} | 
		write_channel(display(['Debug Reduction already Suspended.'|Q]
							\Q,
			[list]),IO, IO'),
		report_state;

	Commands?resume,
	Channels = {IO,User},
	State = running :
	    Channels' = {IO', User} | 
		write_channel(display(['Debug Reduction already Running.'|Q]\Q,
			[list]),IO,IO'),
		report_state;

	Commands?Command,
	Command =\= suspend, Command =\= resume |
		report_state.

/*************************************************************************/
procedure close_channels(Channels).
close_channels(Channels) :-
	Channels = {IO, User} |
		close_channel(IO),
		close_channel(User).

/*************************************************************************/
procedure monitor(Commands,Signals,Circuit,Channels).

monitor(Commands,Signals,Circuit,Channels) :-
	Commands = [] : 
	    Channels = _,
	    Circuit = M-M,
	    Signals = [];

	Commands?suspend :
	    Signals!suspend |
		monitor;

	Commands?resume :
	    Signals!resume |
		monitor;

	Commands?Depth, integer(Depth) :
	    Signals!Depth |
		monitor;

	Circuit = L-R,
	Commands?resolvent(X) :
	    Signals!suspend,
	    L = [resolvent(X) | L'] |
		close_tail(Commands', Signals', L'-R, Channels);

	Circuit = L-R,
	Commands?enabled(X) :
	    Signals!suspend,
	    L = [enabled(X) | L'] |
		close_tail(Commands', Signals', L'-R, Channels);

	Circuit = L-R,
	Commands?disabled(X) :
	    Signals!suspend,
	    L = [disabled(X) | L'] |
		close_tail(Commands', Signals', L'-R, Channels);

	otherwise,
	Commands = [Command | MoreCommands] :
	    Commands' = [Command' | MoreCommands] |
		check_command(Ok, Command, Command', Error),
		send_signal(Ok, Commands', Signals, Circuit,Channels, Error).


/*************************************************************************/
procedure close_tail(Commands,Signals,Choke_Msg_On_Right,Channels).
close_tail(Commands,Signals,Choke_Msg_On_Right,Channels) :-
	Choke_Msg_On_Right = L-R,
	R?{_,Goals} :
	    Goals = [] |
		monitor(Commands, Signals, L-R', Channels).

/*************************************************************************/
procedure check_command(Ok, Command, NewCommand, Error).
check_command(Ok, Command, NewCommand, Error) :-
	Command = clear :
	    Ok    = true,
	    Error = [],
	    NewCommand = Command;

	Command = break |
		breaks#check_break(Ok, Command, NewCommand, Error);

	arg(1,Command,break) |
		breaks#check_break(Ok, Command, NewCommand, Error);

	Command = remove |
		breaks#check_remove(Ok, Command, NewCommand, Error);

	arg(1,Command,remove) |
		breaks#check_remove(Ok, Command, NewCommand, Error);

	otherwise :
	    Ok = false,
	    Error = display([unknown_command(Command) | CR]\CR,[]),
	    NewCommand = Command |
		true.

/*************************************************************************/
procedure send_signal(Ok,Non_Empty_Commands,Signals,Circuit,Channels,Error).
send_signal(Ok,Non_Empty_Commands,Signals,Circuit,Channels,Error) :-
	Ok =\= true,
	Channels = {IO, _User},
	Non_Empty_Commands?_Command |
		write_channel(Error, IO),
%computation # display(term, debug#reduce(unknown_request(Command))),
		monitor(Non_Empty_Commands', Signals, Circuit, Channels);

	Ok = true,
	Non_Empty_Commands?Command :
	    Error = _,
	    Signals = [Command | Signals'] |
		monitor(Non_Empty_Commands', Signals', Circuit, Channels).


/*************************************************************************/
procedure reduce(Signals, Circuit, Goal_Info, Debug_Info, Trace_Info). 
reduce(Signals, Circuit, Goal_Info, Debug_Info, Trace_Info) :-
	Signals = [],
	Goal_Info = {_Goal,{_Module, _Context, Lc-Rc}} :
	    Debug_Info = _, Trace_Info = _ |
		Lc = Rc,
		Circuit = L - L;

	Signals?suspend |
		suspend(Signals', Circuit, Goal_Info, Debug_Info,
			Trace_Info, reduce);

	Signals?Signal,
	Signal =\= suspend |
		nonurgent_signal(Signal,Debug_Info,Debug_Info'),
		reduce;

/*************************************************************************/

	unknown(Signals),
	Circuit = L-R,
	Goal_Info = {true, {_Module, _Context,Lc-Rc}} :
	    Debug_Info = _,
	    Trace_Info = {true,_},
	    L = R, Lc = Rc ;

	unknown(Signals),
	Goal_Info = {Goal, _Context_Info},
	Debug_Info = {_Breaks, Depth, _Execute_or_Interpret, Channels},
	Channels   = {_IO, User},
	Depth =\= 0,
	Goal =\= _#_, Goal =\= (_,_), Goal =\= true,
	Goal =\= [],  Goal =\= [_|_] |
		write_channel(pre(Signals, Goal_Info, Debug_Info, Debug_Info',
				  Id, Done1), User),
		perform(Done1, Signals, Circuit, Goal_Info, Debug_Info',
			Trace_Info, Id);

	unknown(Signals),
	Circuit = L-R,
	Goal_Info = {Goal, {Module, Context, Lc-Rc}},
	Trace_Info = {Tree,Parent},
	Goal = (GoalA,GoalB) :
	    Tree = (TreeA,TreeB),
	    Goal_I1 = {GoalA, {Module, Context, Lc-Mc}},
	    Goal_I2 = {GoalB, {Module, Context, Mc-Rc}} |
		reduce(Signals, L-M,Goal_I1,Debug_Info,{TreeA,Parent}), 
		reduce(Signals, M-R,Goal_I2,Debug_Info,{TreeB,Parent});

	unknown(Signals),
	Circuit = L-R,
	Goal_Info = {[Goal|Goals], {Module, Context-Context'', Lc-Rc}},
	Trace_Info = {Tree,Parent} :
	    Tree = (TreeA,TreeB),
	    Goal_I1 = {Goal,  {Module, Context-Context', Lc-Mc}},
	    Goal_I2 = {Goals, {Module, Context'-Context'', Mc-Rc}} |
		reduce(Signals, L-M,Goal_I1,Debug_Info,{TreeA,Parent}), 
		reduce(Signals, M-R,Goal_I2,Debug_Info,{TreeB,Parent});

	unknown(Signals),
	Circuit = L-R,
	Goal_Info = {[], {_Module, Context-Context^, Lc-Rc}},
	Trace_Info = {true^,_Parent}:
	    Debug_Info = _,
	    L  = R,
	    Lc = Rc;

	unknown(Signals),
	Circuit = L-R,
	Goal_Info = {computation#Goal, {_Module, Context-Context', Lc-Rc}},
	Goal =\= _#_ :
	    Debug_Info = _,
	    Trace_Info = {true,_},
	    L = R, Lc = Rc, Context' = Context |
		computation # Goal;

	unknown(Signals),
	Goal_Info = {Goal, {_Module, Context-Context2, Lc-Rc}},
	Goal = M#_G, M =\= computation,
	Debug_Info = {_Breaks, _Depth, _Execute_or_Interpret, Channels} :
	    Context_Info' = {Module',Context'-_,Module'-NewRc},
	    Goal_Info' = {Goal', Context_Info'} |
%computation # display(term, error(_CLR), type(ground)),
		write_channel(service_id(SId), Context),
		service_id(SId, {Context,Lc}, {Context2,Rc}),
		computation_utils # call_context_goal(SId # Goal,
						Context', NewGoal, Ok),
		check_context(Ok, NewGoal, Goal - Goal', Signals, Channels),
		module_name(Goal,Module'),
		close(NewRc, Signals, Context'),
		reduce;

	unknown(Signals),
	Goal_Info = {Goal, {_Module, Context-Context2, Lc-Rc}},
	Goal = computation#(_M#_G),
	Debug_Info = {_Breaks, _Depth, _Execute_or_Interpret, Channels} :
	    Context_Info' = {Module',Context'-_,Module'-NewRc},
	    Goal_Info' = {Goal', Context_Info'} |
%computation # display(term, error(_CLR),type(ground)),
		write_channel(service_id(SId), Context),
		service_id(SId,{Context,Lc},{Context2,Rc}),
		computation_utils # call_context_goal(SId # Goal,
						Context', NewGoal, Ok),
		check_context(Ok, NewGoal, Goal - Goal', Signals, Channels),
		module_name(Goal,Module'),
		close(NewRc, Signals, Context'),
		reduce.

/*************************************************************************/
procedure suspend(Signals, Circuit, Goal_Info, Debug_Info, Trace_Info,
		  Where).
suspend(Signals, Circuit, Goal_Info, Debug_Info, Trace_Info, Where) :-
	Signals = [] :
	    Goal_Info = _, Debug_Info = _, Trace_Info = _, Where = _ |
		Circuit = M-M;

	Signals?resume,
	Where = reduce |
		reduce(Signals',Circuit,Goal_Info,Debug_Info,Trace_Info);

	Signals?resume,
	Where = perform(Done, Id) |
		perform(Done, Signals', Circuit, Goal_Info,
				Debug_Info, Trace_Info, Id);

	Signals?resume,
	Where = wait(Return_Info, Done) |
		wait(Signals', Circuit, Goal_Info, Debug_Info,
				Trace_Info, Return_Info, Done);

	Signals?Signal,
	Signal =\= resume |
		nonurgent_signal(Signal, Debug_Info, Debug_Info'),
		suspend;

	unknown(Signals),
	Circuit = L - R,
	L?resolvent(Goals),
	Goal_Info = {Goal, _Context} :
	    Goals!Goal,
	    R!resolvent(Goals'),
	    Circuit' = L' - R' |
		suspend;

	unknown(Signals),
	Circuit = L - R,
	L?Element, Element = Request(_Goals),
	Request =\= resolvent,
	unknown(Debug_Info) :
	    R!Element,
	    Circuit' = L' - R' |
		suspend;

	unknown(Signals),
	Circuit = L - R,
	L?enabled(Goals),
	Goal_Info = {Goal, _Context},
	Debug_Info = {_Breaks, Depth, _Mode, _Channels},
	Depth =\= 0 :
	    Goals!Goal,
	    R!enabled(Goals'),
	    Circuit' = L' - R' |
		suspend;

	unknown(Signals),
	Circuit = L - R,
	L?Element, Element = enabled(_Goals),
	Debug_Info = {_Breaks, 0, _Mode, _Channels} :
	    R!Element,
	    Circuit' = L' - R' |
		suspend;

	unknown(Signals),
	Circuit = L - R,
	L?disabled(Goals),
	Goal_Info = {Goal, _Context},
	Debug_Info = {_Breaks, 0, _Mode, _Channels} :
	    Goals!Goal,
	    R!disabled(Goals'),
	    Circuit' = L' - R' |
		suspend;

	unknown(Signals),
	Circuit = L - R,
	L?Element, Element = disabled(_Goals),
	Debug_Info = {_Breaks, Depth, _Mode, _Channels},
	Depth =\= 0 :
	    R!Element,
	    Circuit' = L' - R' |
		suspend.

/*************************************************************************/
procedure nonurgent_signal(Signal, Debug_Info, NewDebug_Info).
nonurgent_signal(Signal, Debug_Info, NewDebug_Info) :-
	Signal = suspend :
	    NewDebug_Info = Debug_Info;

	Signal = resume :
	    NewDebug_Info = Debug_Info;


	integer(Signal),
	Debug_Info  = {Breaks, Depth, Execute_or_Interpret, Channels} :
	    NewDebug_Info =  {Breaks,NewDepth,Execute_or_Interpret,Channels} |
		increment_depth(Signal, Depth, NewDepth);

	arg(1,Signal,break),
	Debug_Info  = {Breaks, Depth, Execute_or_Interpret, Channels} :
	    NewDebug_Info = {Breaks', Depth, Execute_or_Interpret, Channels} |
		breaks#add(Signal, Breaks, Breaks', done - _Done);

	arg(1,Signal,remove),
	Debug_Info  = {Breaks, Depth, Execute_or_Interpret, Channels} :
	    NewDebug_Info = {Breaks', Depth, Execute_or_Interpret, Channels} |
		breaks#remove(Signal, Breaks, Breaks', done - _Done, _Error);

	Signal = clear,
	Debug_Info  = {_Breaks, Depth, Execute_or_Interpret, Channels} :
	    NewDebug_Info = {[], Depth, Execute_or_Interpret, Channels}.

/*************************************************************************/
procedure increment_depth(Increment, Depth, NewDepth).
increment_depth(Increment, Depth, NewDepth) :-
	Increment >= 0,
	NewDepth^ := Increment + Depth |
		true;

	Increment < 0 :
	    Depth = _,
	    NewDepth = Increment.

/*************************************************************************/
procedure perform(Done, Signals, Circuit, Goal_Info, Debug_Info,
		  Trace_Info, Id). 
perform(Done, Signals, Circuit, Goal_Info, Debug_Info, Trace_Info, Id) :-
	Signals = [], Circuit = L-R :
	    Done = _, Goal_Info = _, Debug_Info = _, Trace_Info = _, Id = _,
	    L=R;

	Signals?suspend |
		suspend(Signals', Circuit, Goal_Info, Debug_Info,
			Trace_Info, perform(Done, Id));

	Signals?Signal,
	Signal =\= suspend |
		nonurgent_signal(Signal, Debug_Info, Debug_Info'),
		perform;

	known(Done),
	unknown(Signals),
	Debug_Info = {_Breaks, _Depth, interpret, Channels},
	Channels   = {_IO, User},
	Goal_Info  = {Goal, {_,Context-Context',_}} :
	    Return_Info = {Body, Id, Time} |
		execute(Context, Context', clause(Goal,Body,Id,Time)),
%		send(Goal_Info, Return_Info),
		write_channel(post(Signals, Return_Info, Goal_Info, Debug_Info,
					Debug_Info', Done1), User),
		wait(Signals, Circuit, Goal_Info, Debug_Info',
			Trace_Info, Return_Info, Done1);

	known(Done),
	unknown(Signals),
	Circuit    = L - R,
	Debug_Info = {_Breaks, _Depth, execute, {IO, _User}},
	Goal_Info  = {Goal, {_Module, Context-Context^, Lc-Rc}},
	Trace_Info = {Trace,_} :
	    Id = _,
	    L      = R,
	    Trace  = execute |
		write_channel(service_id(SId), Context),
		service_id(SId,Lc,Rc),
		computation # call([identifier(debug),
				    widgets # vanilla #
						trace(SId, Goal, _) | Is],
				   Events),
		monitor_subcomputation(Signals, {Goal, IO, Is}, Events).
		
/*************************************************************************/
procedure execute(Context, Context, Clause).
execute(Context, NewContext, Clause) :-

	Clause	 = clause(Clause2,Body,Id,Time),
	Clause2	 = clause(UserGoal,UserBody) :
	    Id	 = 0,
	    Body = true,
	    write_channel(clause(UserGoal,UserBody,_ID,Time),
			  Context, NewContext) ;

	Clause	 = clause(Clause4,Body,Id,Time),
	Clause4	 = clause(_UserGoal,_UserBody,_UserId,Time4) :
	    Id	 = 0,
	    Time = Time4,
	    Body = true,
	    write_channel(Clause4, Context, NewContext) ;

	Clause	 = clause(Turtle_Prog,Body,Id,Time),
	Turtle_Prog = Goal @ _Processor,
	info(5,Time^) :
	    Context = NewContext,
	    Id	 = 0,
	    Body = Goal ;

	otherwise :
	   write_channel(Clause, Context, NewContext) .


/*************************************************************************/
%procedure continue(Time, Goal).
%continue :-	
%	known(Time) | Goal = true.


/*************************************************************************/
procedure wait(Signals, Circuit, Goal_Info, Debug_Info, Trace_Info,
		Return_Info, Done).
wait(Signals, Circuit, Goal_Info, Debug_Info, Trace_Info, Return_Info, Done) :-
	Signals = [], Circuit = L - R :
	    Goal_Info = _, Debug_Info = _, Trace_Info = _, Done = _,
	    L = R,
	    Return_Info = {_Body, Id, _Time} |
		try_to_abort(Id);

	Signals = [suspend | Signals'],
	Return_Info = {_Body,  Id, _Time},
	unknown(Id) :
	    Done = _,
	    Id = 'Abort' |
		suspend(Signals', Circuit, Goal_Info, Debug_Info,
			Trace_Info, perform(done, _Id));
% This is the case when the user specifies a  clause number via the
% "data" statement, and then the clause suspends.  This gives us a
% "live" resolvent, since this process will  NOT be suspended.

	Signals?suspend,
	Return_Info = {_Body,  Id, Time},
	number(Id),
	unknown(Time) |
		suspend(Signals', Circuit, Goal_Info, Debug_Info,
			Trace_Info, wait(Return_Info, Done));

	Signals?suspend,
	Return_Info = {_Body, _Id, Time},
	number(Time), unknown(Done) |
		suspend(Signals', Circuit, Goal_Info, Debug_Info,
			Trace_Info, wait(Return_Info, Done));

	Signals?Signal,
	Signal =\= suspend |
		nonurgent_signal(Signal, Debug_Info, Debug_Info'),
		wait;

	unknown(Signals),
	Return_Info = {Body, Id, Time},
	known(Time), known(Done),
	Trace_Info  = {Trace, Parent},
	Goal_Info   = {_Goal, Context},
	Debug_Info  = {Breaks, Depth, Mode, Channels} :
	    Debug_Info'  = {Breaks, NewDepth, Mode, Channels},
	    Trace	 = trace(Id,Time,_Process,Children,Parent),
	    Trace_Info'  = {Children,Trace},
	    Goal_Info'   = {Body, Context} |
		newdepth(Depth, NewDepth),
		reduce(Signals, Circuit, Goal_Info', Debug_Info',
			Trace_Info').

/*************************************************************************/
procedure try_to_abort(Id_Abort).
try_to_abort(Id_Abort) :-
	known(Id_Abort) | true;

	true : Id_Abort = 'Abort' | true.

/*************************************************************************/
procedure newdepth(Depth, NewDepth).
newdepth(Depth, NewDepth) :-
	Depth = -1 | NewDepth = Depth;

	otherwise  | NewDepth := Depth-1.

/*************************************************************************/
procedure check_context(Ok, NewGoal, Goals, Signals, Channels).
check_context(Ok, NewGoal, Goals, Signals, Channels) :-
	Ok = true : 
	    Signals = _, Channels = _,
	    Goals = _Old - NewGoal ;

	Ok =\= true,
	Goals = OldG - NewG,
	Channels = {_IO, User} :
	    NewGoal = _ |
		unify_when_done(Done,true-NewG),
		write_channel(pre(Signals, {no_service(OldG),invalid_module},
				{[],0,interpret,Channels}, _, _, Done)
				,User).
		

/*************************************************************************/
procedure unify_when_done(Done,Goals).
unify_when_done(Done,Goals) :-
	known(Done),
	Goals = LeftGoal - RightGoal :
	    LeftGoal = RightGoal.

/*************************************************************************/
procedure module_name(RPCGoal,Module).
module_name(RPCGoal,Module) :-
	RPCGoal  = M#G,
	G =\= _#_ :
	    Module = M ;

	RPCGoal  = _M#RPCGoal',
	RPCGoal' = _#_ |
		module_name.

/*************************************************************************/
procedure close(Done, Signals, Context).
close(Done, Signals, Context):-
	known(Done) :
	    Signals = _ |
%computation#display(term, Done, [prefix("Closing Context: ")]),
		close_cntx(Context);

	Signals = [] :
	    Done = _ |
%computation#display(term, received_signal,[prefix("Closing Context: ")]),
		close_cntx(Context);

	Signals?_Signal |
		close(Done, Signals', Context).

/*************************************************************************/
procedure close_cntx(Context).
close_cntx(Context) :-
	Context = [] | 
		true;

	Context =\= [] | 
		close_channel(Context).


/*************************************************************************/
procedure monitor_subcomputation(Signals,SubComputation,SubComputation_Events).
monitor_subcomputation(Signals,SubComputation,SubComputation_Events) :-
	Signals = [],
	SubComputation    = {Goal, IO, In_Stream} :
	    SubComputation_Events = _,
	    In_Stream ! abort, In_Stream' = _ |
		try_to_write(display([subcomputation(Goal),
					' - aborted.' | Q]\Q, [list]), IO),
		true;

	Signals?_Signal |
		monitor_subcomputation;

	unknown(Signals),
	SubComputation_Events = [] :
	    SubComputation = _ |
		true;

	unknown(Signals),
	SubComputation_Events = [Event | _SubComputation_Events],
	Event = failed(call(debug),failed(_,_)) :
	    SubComputation = _ |
		true;

	unknown(Signals),
	SubComputation_Events?Event,
	Event =\= failed(call(debug),failed(_,_)),
	SubComputation = {Goal, IO, Input} |
		try_to_write(display([subcomputation(Goal), ' - ',
					Event, '.' | Q] \ Q, [list]), IO),
		choke_subcomputation(Event, Input),
		monitor_subcomputation.

/*************************************************************************/
procedure choke_subcomputation(Event,SubInput).
choke_subcomputation(Event,SubInput) :-
	Event = terminated :
	    SubInput = [] ;

	Event =\= terminated :
	    SubInput = _ .

/*************************************************************************/
procedure try_to_write(Message, Channel).
try_to_write(Message, Channel) :-
	true :
	    write_channel(Message, Channel);

	otherwise,
	Message = display(What\CR,How) :
	    Channel = _,
	    CR  = ' 
 ' 		|
		computation # display(term, What, How).


service_id(ServiceId, Left, Right) :-
    known(ServiceId) :
      Left = Right .
