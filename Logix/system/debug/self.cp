/*
Main control of algorithmic debugger.

Yossi Lichtenstein, Peter Gerstenhaber

Last update by          $Author: bill $
			$Date: 2003/04/30 06:08:29 $
Currently locked by     $Locker:  $
			$Revision: 1.2 $
			$Source: /home/qiana/Repository/Logix/system/debug/self.cp,v $

Copyright (C) 1988, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([start/1,start/2,interpret/2,adb/3,read/6,print_response/6]).
-language(compound).
-mode(trust).

Module	::= Tuple ; String.
Call	::= Tuple.
Goal	::= Tuple ; String ; RPCGoal.
RPCGoal	::= {`'#',Module,Goal}.
Id	::= String.
Interpret::= RPCGoal; interpret(Id,RPCGoal).
PrePost	::= pre; post.
Query_Print ::= query, print.
Arity   ::= Integer ; all.
Frozen_Goal ::= String.
BreakPoint  ::= String/Arity.		% ; Frozen_Goal.
Breaks  ::= [pre(BreakPoint,query)].
L	::= [Any].
Pred	::= String/Integer.
Predicate ::= Pred ; Any.
Mode	::= print; query.
Loc	::= pre; post.
Break   ::= break ; break(Predicate) ; break(Predicate,Mode) ;
                break(Predicate,Mode,Loc).
Remove  ::= remove ; remove(Predicate) ; remove(Predicate,Loc).
Cmd     ::= disabled(L) ; enabled(L) ; resolvent(L) ; help ; help(Any) ;
            suspend     ; resume     ; Break ; Remove ; clear ; [].
Cmds    ::= [Cmd].
Command ::= Cmd ; query.
Command1::= Command ; Integer.
Ok	::= true; false.
Ok2	::= Any.
Request	::= goal(Goal) ; behavior(Integer,Goal,L) ; states(L);
		child(Integer,Ok) ; parent(Ok) ; trace(Trace).
I	::= depth; depth(integer); Command; Request; abort; Any.
In	::= [I].
Commands::= [Command1].
Requests::= [Request].
System	::= [Any].

Display ::= display(Any, List) ; display_stream(List, List).
Display_Continue	::= Display ; continue.
Ask     ::= ask(List, List, List, [Display_Continue]).
CloseAsk::= ask(List, [], List, [Display_Continue]).
TTY_IO  ::= Display ; Ask.
TTY_IOs ::= [TTY_IO].
IO      ::= TTY_IOs.
NonEmpty_IO ::= [ TTY_IO | TTY_IOs].
CloseIO ::= Display ; CloseAsk.
CloseIOs::= [CloseIO].
Done	::= done.

Options ::= List.
NewOptions      ::= [prefix(String) | Options].
 
Trace   ::= (Trace,Trace) ; true ; execute ;
            trace(Integer,Integer,Any,Trace,Trace).
 
Event           ::= String.
Events          ::= [Event].
NonEmpty_Events	::= [Event | Events].
Non_Help        ::= Any.
Log             ::= log ; nolog.
Status          ::= Any.
OutStreams      ::= {Commands, Requests, System}.
ChokeStreams    ::= {[], [], System}.
NewOutStreams   ::= OutStreams.

C	    ::= I.
CommandsOut ::= Commands.
RequestsOut ::= Requests.
SystemOut   ::= System.

O	::= I.
Commands' ::= Commands.
Requests' ::= Requests.
System'	::= System.

Answer	::= Any.

/*************************************************************************/
procedure start(RPCGoal).
procedure start(RPCGoal,Trace).
start(RPCGoal) + (Trace = _) :- 
	RPCGoal = _#_ | 
		adb('<?>',RPCGoal,Trace).

/*************************************************************************/
procedure interpret(Id, RPCGoal).
interpret(Id, RPCGoal) :- 
	computation # events(Events),
	do_debug(Events, Id, RPCGoal).

/*************************************************************************/
procedure do_debug(NonEmpty_Events, Id, RPCGoal).
do_debug(NonEmpty_Events, Id, RPCGoal) :-
	NonEmpty_Events?resumed :
	    NonEmpty_Events' = _ |
		adb(Id, RPCGoal,_);

	NonEmpty_Events?aborted :
	    NonEmpty_Events' = _, Id = _, RPCGoal = _ ;

	NonEmpty_Events?Event,
	Event =\= resumed, Event =\= aborted |
		do_debug.

/*************************************************************************/
procedure adb(Id,RPCGoal,Trace).
adb(Id,RPCGoal,Trace) :-
	RPCGoal  = _Module#Goal,
	string_to_dlist(Id, L,E),
	string_to_dlist(' ',E,[]),
	freeze(RPCGoal,[],FG,_) :
	    melt(FG,_MGoal,_)
	     |
		list_to_string(L,Id')
		,log(nolog)
		,breaklist(Goal, Breaks)
		,make_channel(IO_Channel,IO)

		,computation # shell(prompt,'@debug ->')
		,computation # shell(filter_system_macros,Input,NonCommands)
		,filter(Id', Input, IO?, Events, {Commands1,_Commands2,NonCommands})

		,reduce#debug(RPCGoal,Breaks,Commands1?,Trace,IO_Channel,Events)
%	    ,schedule#schedule(MGoal,Trace,Reductions,Channel,ToSchedule)
%	    ,process#process(MGoal,Trace,Reductions,Channel)
%	    ,print#print(Commands2?,Trace?,ToSchedule)
	    .

/********************************************************************/
procedure log(Log).
log(Log) :-
	Log = log |
		processor # interface(user_name, User),
		processor # interface(date_time,UD,D,T),
		file # put_file('.debug_log',['Debug ',User,' ',
				UD,' ',D,' ',T,'
'], [append], Ok),
		check(Ok);

	Log =\= log |
		true.

/********************************************************************/
procedure check(Status).
check(Status) :-
	Status = true | true;

	Status =\= true |
		computation # display(term,'Could not log user information.').

/********************************************************************/
procedure filter(Id, In, IO, Events, OutStreams).
filter(Id, In, IO, Events, OutStreams) :-
	Events?Event |
		print_event(Id, Event),
		filter;

	IO =[] |
		shutdown(Id, In, IO, Events, OutStreams);

	In ? shutdown(debug),
	OutStreams = {_Commands, _Requests, System} :
	    Id = _, IO = _, Events = _,
	    System = In' |
		computation # shell(pop_prompt);

	In?abort |
		shutdown(Id, In', IO, Events, OutStreams);

	IO=[_I|_IO'],
	unknown(Events) |
		process_io(Id, In, IO, Events, OutStreams);

	In?C, C =\= abort, C =\= shutdown(debug),
	unknown(Events) |
		send_a_command(C, Id, OutStreams, OutStreams'),
		filter;

	In = [], unknown(IO), Events = [],
	OutStreams = {Commands, Requests, _System} :
	    Id = _,
	    Commands=[], Requests=[]  | true.



/*************************************************************************/
procedure shutdown(Id, In, CloseIOs, Events, ChokeStreams).
shutdown(Id, In, CloseIOs, Events, ChokeStreams) :-
	ChokeStreams = {Commands, Requests, System} :
	    Commands=[], Requests=[], System = In |
		computation # shell(pop_prompt),
		complete_io(Id, CloseIOs),
		print_all_events(Id, Events).

/*************************************************************************/
procedure send_a_command(C, Id, OutStreams, NewOutStreams).
send_a_command(C, Id, OutStreams, NewOutStreams) :-
%	C |
	OutStreams = {Commands, Requests, System} :
	    NewOutStreams = {NewCommands, NewRequests, NewSystem} |
		help(C,H),
		command(H, R, Id, Commands, NewCommands),
		request(R, T, Requests, NewRequests),
		trap_invalid(T,S),
		system(S, System, NewSystem).


/*************************************************************************/
procedure help(Command, Non_Help).
help(Command, Non_Help) :-
	Command = help |
		computation # display(term, "
This is the command mode of the debugging system.  It is entered whenever
the system is idle, or whenever the user hits a character.  Any command
which is not understood by the debugger is sent to the logix command
interpreter.  
The following is a list of the valid commands understood by the ADB
system:

abort		Abort the current computation, and terminate debug session.
suspend		suspend the current computation.
resume		resume the current computation.
depth(N)	increase the computation depth bound by N.
resolvent	Obtain the current set of processes in the computation.
enabled		Obtain the set of the current processes which are being executed. 
disabled	Obtain the set of the current processes which are impeded on the
		depth bound.
break		Send the break  command to all active processes.
remove		Send the remove command to all active processes.
clear		Send the clear  command to all active processes.
query		Return from temporary command mode to directive mode.

To obtain help on a specific command, enter 'help(Command)'.
To obtain a list of new features enter  'help(news)'.
",				[close([], Non_Help)]);

	Command = help(news) |
		computation # display(term, "
Version A.01.08 changes:
	Default computation depth bound is unlimited.  The directive
	'depth' has been added to change the depth bound for a process.

	The directive 'trace' has been added to set up a breakpoint
	list of break(all/all,print,Pre_Post).  More info available
	via the directive 'help(trace)'.

	The enabled list or disabled list will be incomplete if requested
	from temporary command mode (i.e. prompt of '@debug? ->'), since
	the user may change the depth bound for any process at a query
	breakpoint.  The resolvent will always report information on all
	active processes.
",				[close([], Non_Help)]);

	Command = help(abort) |
		computation # display(term, "
abort will abort the current computation, and terminate the debug
session.
",				[close([], Non_Help)]);

	Command = help(suspend) |
		computation # display(term, "
suspend will suspend the current computation, and leave it in a stable
state.
",				[close([], Non_Help)]);

	Command = help(resume) |
		computation # display(term, "
resume will reschedule the current computation for execution.
",				[close([], Non_Help)]);

	Command = help(resolvent) |
		computation # display(term, "
resolvent will suspend the current computation, and return a list of 
goals in the current computation.
",				[close([], Non_Help)]);

	Command = help(enabled) |
		computation # display(term, "
enabled(List) will suspend the current computation, and return the list of
goals in the current computation which are scheduled to execute (i.e. they
have not exceeded their depth bound.
",				[close([], Non_Help)]);

	Command = help(disabled) |
		computation # display(term, "
disabled(List) will suspend the current computation, and return the list of
goals in the current computation which are not scheduled to execute since
they have exceeded their depth bound.
",				[close([], Non_Help)]);

	Command = help(depth) |
		computation # display(term, "
depth(N) will increase the depth bound for all processes by N.  If N is -1,
it will make the depth bound unlimited. depth will default to depth(-1).
",				[close([], Non_Help)]);

	Command = help(break) |
		computation # display(term, "
break will set a break point in all active processes (and by inheritance,
their descendents).  The break command has the same format as the break
directive.
",				[close([], Non_Help)]);

	Command = help(remove) |
		computation # display(term, "
remove will remove a break point from all active processes (and by inheritance,
their descendents).  The remove command has the following formats:

remove			remove an all/all breakpoint for all processes.
remove(Predicate)	remove the first breakpoint for this predicate in all
			processes.
remove(Predicate,Pre_or_Post)

Predicate may either be of the form Predicate, Predicate/Arity, or Pattern.
If Predicate is specified, a breakpoint of the form Predicate/_ is searched
for.
",				[close([], Non_Help)]);

	Command = help(clear) |
		computation # display(term, "
clear will clear the break point list of all active processes (and by
inheritance, their descendents).
",				[close([], Non_Help)]);

	Command = help(query) |
		computation # display(term, "
query will return to the directive mode (if command mode was entered from
directive mode).
",				[close([], Non_Help)]);

	Command = help(help) |
		computation # display(term, "
The help command prints out help information for different commands.
",				[close([], Non_Help)]);

	otherwise |
	    Command = Non_Help.

/*************************************************************************/
procedure command(I, O, Id, Commands, Commands').
command(I, O, Id, Commands, Commands') :-
	I = resolvent,
	string_to_dlist(Id, L,E),
	string_to_dlist(goal,E,[]) :
	    Commands!resolvent(R), O = [] | 
		list_to_string(L,Prefix),
		computation # display(stream,R,[known(prefix),prefix(Prefix)]);

	I = resolvent(R)	: 
	    Id = _,
	    Commands ! resolvent(R'), O = [] | 
		unify_wo_failure(R',R);

	I = enabled,
	string_to_dlist(Id, L,E),
	string_to_dlist(enabled,E,[]) :
	    Commands!enabled(R),   O = [] |
		list_to_string(L,Prefix),
		computation # display(stream,R,[known(prefix),prefix(Prefix)]);

	I = disabled,
	string_to_dlist(Id, L,E),
	string_to_dlist(disabled,E,[]) :
	    Commands!disabled(R),  O = [] |
	    list_to_string(L,Prefix),
		computation # display(stream,R,[known(prefix),prefix(Prefix)]);

	otherwise  :
	    Id = _ |
		command2(I,O,Commands,Commands').

/*************************************************************************/
procedure unify_wo_failure(List,Any).
unify_wo_failure(L,R) :-
	known(L) :
	    L = R;

	otherwise | 
	    L = _, R = _.

/*************************************************************************/
procedure command2(I,O,Commands,Commands').
command2(I,O,Commands,Commands') :-
	I = suspend		: Commands!I, O = [] ;

	I = resume		: Commands!I, O = [] ;

	I = depth 		: Commands!(-1), O = [] ;

	I = depth(X), integer(X): Commands!X, O = [];

	I = resolvent(_L)	: Commands!I, O = [];

	I = enabled(_L)		: Commands!I, O = [];

	I = disabled(_L)	: Commands!I, O = [];

/*** Following commands manipulate break lists of ALL active processes	  ***/

	 arg(1,I,break)		: Commands!I, O = [];

	arg(1,I,remove)		: Commands!I, O = [];

	I = clear		: Commands!I, O = [];

	otherwise : I = O, Commands = Commands'.

/*************************************************************************/
procedure request(I, O, Requests, Requests').
request(I, O, Requests, Requests') :-
	I = [] : O = I, Requests = Requests';

	I = goal(_G)	: Requests!I, O = [];

	I = behavior(M,_G,_MB), integer(M) :
		Requests!I, O = [];

	I = states(_MB) : Requests!I, O = [];

	I = child(M,_Ok), integer(M) :
	    Requests!I, O = [];

	I = parent(_Ok) : Requests!I, O = [];

	I = trace(_Tree) : Requests!I, O = [];

	I = _child(U, _), unknown(U) : I = O, Requests = Requests';

	I = _behavior(U, _, _), unknown(U) : I = O, Requests = Requests';

	otherwise : I = O, Requests = Requests'.

/*************************************************************************/
procedure trap_invalid(Command, Cmd).
trap_invalid(Command, Cmd) :-
	Command = query |
		computation # display(term,
			"No breakpoints directives are enqueued.",
					close([], Cmd));

	otherwise :
	    Command = Cmd |
		true.


/*************************************************************************/
procedure system(I, System, System').
system(I, System, System') :-
	I = [] :
	    System = System';

	I = 'Logix'#Command :
	    System!Command  |
		computation # display(term, I, prefix('System Command:  '));

	otherwise :
	    System ! I .


/*************************************************************************/
procedure complete_io(Id, CloseIOs).
complete_io(Id, CloseIOs) :-
	CloseIOs = [] :
	    Id = _;

	CloseIOs ? display(List, How), List = Goal\End :
	    End = ['
']	    | %put a carriage return at the end.
		computation # display(term, [Id | Goal], How),
		complete_io;

	CloseIOs ? display_stream(List, How) |
		computation # display(stream, List, [prefix(Id) | How]),
		complete_io;

	CloseIOs ? ask(_Prompt,Resp,_How,_More) :
	    Id = _ ,
	    Resp = [] | 
		complete_io.

/*************************************************************************/
procedure process_io(Id, In, NonEmpty_IO, Events, OutStreams).
process_io(Id, In, NonEmpty_IO, Events, OutStreams) :-
	NonEmpty_IO?display(List, How), List = Goal\End :
	  End = ['
']	    |    % put a carriage return at the end
		computation # display(term, [Id, ' ' | Goal], How),
		filter(Id, In, NonEmpty_IO', Events, OutStreams);

	NonEmpty_IO?display_stream(What, How) |
		prefix_id(Id, How, NewHow),
		computation # display(stream, What, NewHow),
		filter(Id, In, NonEmpty_IO', Events, OutStreams);

	NonEmpty_IO?Ask,
	Ask = ask(_Prompt,_Response,_How,_Print_Requests) |
		read(Id, In, NonEmpty_IO', Events, OutStreams, Ask).

/*************************************************************************/
procedure prefix_id(Id, Options, NewOptions).
prefix_id(Id, Options, NewOptions) :-
	Options = [] :
	    NewOptions = [prefix(Id)];

	Options?Option,
	Option =\= prefix(_) :
	    NewOptions!Option |
		prefix_id;

	Options?prefix(Prefix),
	string_to_dlist(Id, L,E),
	string_to_dlist(Prefix,E,[]) :
	    NewOptions = [known(NewPrefix),prefix(NewPrefix) | Options'] |
		list_to_string(L,NewPrefix).

/*************************************************************************/
procedure read(Id, In, IO, Events, OutStreams, Ask).
read(Id, In, IO, Events, OutStreams, Ask) :-
	Ask = ask(Prompt, _Response, How, _Print_Requests) |
		computation # display(ask, [Id | Prompt], H1\[], How),
		parse # characters(H1?,Terms,Ok),
		goal_compiler # term(Terms, Answer),
%computation # display(term, response(Answer,Ok),[known(Answer), known(Ok)]),
		check_parse(Ok, Id, In, IO, Events, OutStreams, Ask, Answer).

/*************************************************************************/
/*************************************************************************/
procedure check_parse(Ok2, Id, In, IO, Events, OutStreams, Ask, Answer).
check_parse(Ok2, Id, In, IO, Events, OutStreams, Ask, Answer) :-
	Ok2 = [] |
%computation # display(term, check_parse(ok,Answer)),
		wait(Id, In, IO, Events, OutStreams, Ask, Answer);

	Ok2 =\= [] :
	    Answer = _ |
		computation # display(stream, Ok2,
					[type(ground),close(done,Done)]),
		call(Done,read(Id, In, IO, Events, OutStreams, Ask)).

/*************************************************************************/
procedure call(Done, Call).
call(Done, Call) :-
	known(Done),
	Call =  read(Id, In, IO, Events, OutStreams, Ask) |
		read(Id, In, IO, Events, OutStreams, Ask);

	known(Done),
	Call =  print_response(Id, In, IO, Events, OutStreams, Ask) |
		print_response(Id, In, IO, Events, OutStreams, Ask).

/*************************************************************************/
procedure wait(Id, In, IO, Events, OutStreams, Ask, Answer).
wait(Id, In, IO, Events, OutStreams, Ask, Answer) :-
	Answer = [debug] |
		computation # shell(prompt,'@debug? ->'),
		temporary_command_mode(Id, In, IO, Events, OutStreams, Ask);

	Answer = [debug#abort] :
	    In' = [abort| In] |
		temporary_command_mode(Id, In', IO, Events, OutStreams, Ask);

	Answer = [debug#Command], Command =\= abort:	%,
%	Ask = ask(_Prompt, H\_T,_How,_More) :
%	    H   = [],
	    In' = [Command | In] |
		computation # shell(prompt, '@debug? ->'),
		temporary_command_mode(Id, In', IO, Events, OutStreams, Ask);

	Answer	   ? Display,
	Display    = display_variable(_,_),
	OutStreams = {Commands, Requests, System} :
	    System	= [Display | NewSystem],
	    OutStreams'	= {Commands, Requests, NewSystem} |
		wait;

	otherwise, Ask = ask(Prompt,Ans,How,More) :
	    Ans! Answer,
	    Ask' = ask(Prompt, Ans', How, More) |
%computation # display(term, Answer),
		print_response(Id, In, IO, Events, OutStreams, Ask').

/*************************************************************************/
procedure print_response(Id, In, IO, Events, OutStreams, Ask).
print_response(Id, In, IO, Events, OutStreams, Ask) :-
	Ask = ask(_Prompt, _Answer, _How, Print_Requests),
	Print_Requests = [] |
		filter(Id, In, IO, Events, OutStreams);

	Ask = ask(Prompt, Answer, Format, Print_Requests),
	Print_Requests?continue :
	    Ask' = ask(Prompt, Answer, Format, Print_Requests') |
		read(Id, In, IO, Events, OutStreams, Ask');

	Ask = ask(Prompt, Answer, Format, Print_Requests),
	Print_Requests?display_stream(What, How) :
	    Ask' = ask(Prompt, Answer, Format, Print_Requests') |
		prefix_id(Id, [ close(done,Done) | How ], NewHow),
		computation # display(stream, What, NewHow),
		call(Done,print_response(Id,In,IO,Events,OutStreams,Ask'));

	Ask = ask(Prompt, Answer, Format, Print_Requests),
	Print_Requests?display(What\CR, How) :
	    CR = '
',
	    Ask' = ask(Prompt, Answer, Format, Print_Requests') |
		computation # display(term, [Id | What], How),
		print_response.
%		read(Id, In, IO, Events, OutStreams, Ask').
		
/*************************************************************************/
procedure temporary_command_mode(Id, In, IO, Events, OutStreams, Ask).
temporary_command_mode(Id, In, IO, Events, OutStreams, Ask) :-
	Events?Event |
		print_event(Id, Event),
		temporary_command_mode;

	In?query,
	known(OutStreams),
	unknown(Events) |
		computation # shell(pop_prompt),
		read(Id, In', IO, Events, OutStreams, Ask);

	In?query#Response,
	unknown(Events) |
		computation # shell(pop_prompt),
		wait(Id, In', IO, Events, OutStreams, Ask, [Response]);

	In=[abort|_In'],
	unknown(Events) :
	    Ask = _ |
		computation # shell(pop_prompt),
		filter(Id, In, IO, Events, OutStreams);

	In=[abort|_In'],
	unknown(Events) :
	    Ask = _ |
		computation # shell(pop_prompt),
		filter(Id, In, IO, Events, OutStreams);

	In?I,
	I =\= query, I =\= query#_, I =\= abort,
	unknown(Events) |
%computation # display(term, I, [prefix('TCM:  ')]),
		send_a_command(I, Id, OutStreams, OutStreams'),
		temporary_command_mode.

/*************************************************************************/
procedure breaklist(Goal,Breaks).
breaklist(Goal,Breaks) :-
	Goal = _Module#Goal' | breaklist;

	Goal =\= _#_,
	arg(1,Goal,Predicate), 
	RealArity:= arity(Goal) - 1 :
	    Breaks =[pre(Predicate/RealArity,query)];

	constant(Goal) :
	    Breaks = [pre(Goal/0,query)].

/*************************************************************************/
procedure print_all_events(Id, Events).
print_all_events(Id, Events) :-
	Events = [] :
	    Id = _ ;

	Events?Event |
		print_event(Id, Event),
		print_all_events.

/*************************************************************************/
procedure print_event(Id, Event).
print_event(Id, Event) :-
		computation # display(term, Event, prefix(Id)).

