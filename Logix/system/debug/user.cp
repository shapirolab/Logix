/*
User interface of algoritmic debugger.

Yossi Lichtenstein, Peter Gerstenhaber

Last update by          $Author: bill $
			$Date: 2003/04/30 06:09:44 $
Currently locked by     $Locker:  $
			$Revision: 1.2 $
			$Source: /home/qiana/Repository/Logix/system/debug/user.cp,v $

Copyright (C) 1988, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([monitor/1,pre/6,post/6]).
-language(compound).
-mode(trust).

Command ::= pre(Signals,Goal_In,Debug_Info,NewDebug_Info,Id,Done) ;
	    post(Signals,Execute_Info,Goal_In,Debug_Info,NewDebug_Info,Done).
Commands::= [Command].
Time	::= Any.
Goal	::= Tuple ; String.
Channel_or_Error::= Channel ; invalid_module.
Goal_In	::= {Goal, Channel_or_Error}.
Goal_Rec::= {Goal, Open_Context, Execute_Info}.
Goal_Info::= {Goal, Location, Execute_Info}.
Open_Context	::= {Module, Context, Circuit}.
Module	::= String ; {`"#", String, Module}.
Circuit	::= Any - Any.
Depth	::= Integer.
Termination_Info::= {Debug_Info, Debug_Info, Done}.
Debug_Info	::= {Breaks, Depth, Execute_or_Interpret, Channels}.
Execute_or_Interpret ::= execute; interpret.
Channel	::= Vector.
Channels::= {IO, User}.
IO	::= Channel.
User	::= Channel.
NewDebug_Info	::= Debug_Info.
Execute_Info	::= {Body, Id, Time}.
Body	::= true ; Goal ; (Goal,Body) ; failed(Goal,Any).
Id	::= Integer ; 'Abort'.
Done	::= done.
Loc	::= pre ; post.
Location::= Loc.
Mode	::= print ; query.
Arity	::= Integer ; all.
Predicate	::= String.
PredicateArity	::= String/Arity ; pattern(String).
Break	::= break(PredicateArity,Mode,Location).
Breaks	::= [{Location,PredicateArity,Mode}].
NewBreaks	  ::= Breaks.
Printable_Pattern ::= String; Tuple; Predicate/Arity.
Printable_Breaks  ::= [{Location, Printable_Pattern,Mode}].
Answer	::= Mode ; dont_ask.
Directive ::= [Any | List].
Error_Msg ::= display(What,How) ; [].
What	::= Any.
How	::= List.
Format	::= [list | Nil].
Subject	::= Any.
Signal	::= suspend ; Any.
Signals	::= [Signal].
Help_Msg	::= display(Any,Format).
Print_Req	::= [Help_Msg].
Print_Request	::= display(Any,List) ; display_stream(List,List).
Print_Request1	::= Print_Request ; continue.
Print_Requests	::= [Print_Request1].
Msg	::= [] ; Print_Request.
Message	::= Print_Request.
Head	::= [Any | List].
Tail	::= List.
Release	::= Any.
Index	::= Integer.
Current	::= Index.

%Directive ::= y ; yes ; wait(Any) ; data(Any) ;
%              break(Predicate/Arity,Mode,Location) ;
%              remove(Predicate/Arity,Mode,Location).
%TTY_IO	::= display(Goal,[Any]); ask([Any],[Any]\[Any],[Any]).

/***********************************************************************/
procedure monitor(Commands).
monitor(Commands) :-
	Commands = [] |
		true;

	Commands?Command,
	Command = pre(Signals,Goal_In,Debug_Info, NewDebug_Info, Id, Done) |
		pre(Signals, Goal_In, Debug_Info, NewDebug_Info, Id, Done),
		monitor;

	Commands?Command,
	Command = post(Signals, Execute_Info, Goal_In, Debug_Info,
			NewDebug_Info, Done) |
		post(Signals, Execute_Info, Goal_In, Debug_Info,
			NewDebug_Info, Done),
		monitor.

/***********************************************************************/
procedure post(Signals, Execute_Info, Goal_In, Debug_Info, NewDebug_Info,
		Done). 

post(Signals, Execute_Info, Goal_In, Debug_Info, NewDebug_Info, Done) :-
	Execute_Info = {_Body, 'Abort', _Time} :
	    Signals = _, Goal_In = _,
	    Done = done,
	    Debug_Info = NewDebug_Info |
		true;

	Execute_Info = {_Body, _Id, Time},
	number(Time),
	Goal_In    = {Goal,_OpenContext},
	Debug_Info = {Breaks, _Depth, _Execute_or_Interpret, _Channels} |
		break(post,Goal,Breaks,Answer,_Index),
		ask(Signals, Answer, post, Goal, Debug_Info, NewDebug_Info,
			Execute_Info, Done);


	Execute_Info = {Body, _Id, Time},
	Time = failed(Clause,RealTime), integer(RealTime) :
	    Goal_In = _,
	    Body = true |
		ask(Signals, query, post, failed(Clause), Debug_Info,
			NewDebug_Info, Execute_Info, Done). 

/***********************************************************************/
procedure pre(Signals, Goal_In, Debug_Info, NewDebug_Info, Id, Done).

pre(Signals, Goal_In, Debug_Info, NewDebug_Info, Id, Done) :-
	Goal_In = {Goal, invalid_module} :
	    Execute_Info = {'?', Id, _Time} |
		ask(Signals, query, pre, Goal, Debug_Info, NewDebug_Info,
			 Execute_Info, Done);

	otherwise,
	Goal_In = {Goal,_OpenContext},
	Debug_Info = {Breaks, _Depth, _Execute_or_Interpret, _Channels} :
	    Execute_Info = {'?', Id, _Time} |
		break(pre,Goal,Breaks,Answer,_Index),
		ask(Signals, Answer, pre, Goal, Debug_Info, NewDebug_Info,
			 Execute_Info, Done).
		

/***********************************************************************/
procedure ask(Signals, Answer, Location, Goal, Debug_Info, NewDebug_Info,
		Execute_Info, Done).
ask(Signals, Answer, Location, Goal, Debug_Info, NewDebug_Info,
    Execute_Info, Done):-
	Answer = dont_ask :
	    Signals = _, Location = _, Goal = _, Execute_Info = _,
	    Done = done, 
	    NewDebug_Info = Debug_Info | true;

	Debug_Info = {_Breaks, _Depth, execute, _Channels} :
	    Answer  = _,
	    Answer' = dont_ask |
		ask;

	Answer     = print,
	Location   = pre,
	Debug_Info = {_Breaks, _Depth, interpret, Channels},
	Channels   = {IO,_User} :
	    Signals = _, Execute_Info = _,
	    NewDebug_Info = Debug_Info |
		write(display([Goal, ' :- ?.' | Q] \ Q,
			[list,close(done,Done)]), IO);

	Answer     = print,
	Location   = post,
	Debug_Info = {_Breaks, _Depth, interpret, Channels},
	Channels   = {IO,_User},
	Execute_Info = {true, _, _} :
	    Signals = _,
	    NewDebug_Info = Debug_Info |
		write(display([Goal, '.' | Q] \ Q,
			[list,close(done,Done)]), IO);

	Answer     = print,
	Location   = post,
	Debug_Info = {_Breaks, _Depth, interpret, Channels},
	Channels   = {IO,_User},
	Execute_Info = {Body, _, _},
	Body	   =\= true :
	    Signals = _,
	    NewDebug_Info = Debug_Info |
		write(display([Goal, ' :- 
	',	Body, '.' | Q] \ Q, [list,close(done,Done)]), IO);

	Answer     = query,
	Location   = pre,
	Debug_Info = {_Breaks, _Depth, interpret, Channels},
	Channels   = {IO,_User} |
		write(ask([Goal,' :- ?
query ->'],Ans,[list,read(chars)], Print_Requests),IO),
		read(Signals, Ans, Print_Requests,{Goal,Location,Execute_Info},
			{Debug_Info, NewDebug_Info, Done});

	Answer     = query,
	Location   = post,
	Debug_Info = {_Breaks, _Depth, interpret, Channels},
	Channels   = {IO,_User},
	Execute_Info = {true, _, _}  |
		write(ask([Goal,'.
query ->'],Ans,[list,read(chars)], Print_Requests),IO),
		read(Signals, Ans, Print_Requests,{Goal,Location,Execute_Info},
			{Debug_Info, NewDebug_Info, Done});

	Answer     = query,
	Location   = post,
	Debug_Info = {_Breaks, _Depth, interpret, Channels},
	Channels   = {IO,_User},
	Execute_Info = {Body, _, _},
	Body       =\= true |
		write(ask([Goal,' :- 
	', Body, '.
query ->'],Ans,[list,read(chars)], Print_Requests),IO),
		read(Signals, Ans, Print_Requests,{Goal,Location,Execute_Info},
			{Debug_Info, NewDebug_Info, Done}).

/***********************************************************************/
procedure read(Signals,Directive,Print_Requests,Goal_Info,Termination_Info).

read(Signals, Directive, Print_Requests, Goal_Info, Termination_Info) :-
	Signals = [],
	Termination_Info = {Debug_Info, NewDebug_Info, Done} :
	    Directive = _, Goal_Info = _,
	    Done = done,
	    Print_Requests = [],
	    NewDebug_Info = Debug_Info | true;

	Signals?_Signal |
		read;

	unknown(Signals),
	known(Directive) |
		answer(Signals, Directive, Print_Requests, Goal_Info,
			Termination_Info).


/***********************************************************************/
procedure answer(Signals,Directive,Print_Requests,Goal_Info,Termination_Info).
answer(Signals,Directive,Print_Requests,Goal_Info,Termination_Info) :-
	Directive = [ [] | _Directive'],
	Termination_Info = {Debug_Info, NewDebug_Info, Done} :
	    Signals = _, Goal_Info = _,
	    Done = done,
	    Print_Requests = [],
	    NewDebug_Info = Debug_Info | true;

	Directive?[y]   : Directive' = [[] | _] | answer;

	Directive?[yes] : Directive' = [[] | _] | answer;

	Directive = [ [wait(Release)] | _Directive'],
	Termination_Info = {Debug_Info, NewDebug_Info, Done} :
	    Signals = _, Goal_Info = _,
	    Done = Release, 
	    Print_Requests = [],
	    NewDebug_Info = Debug_Info | true;

	Directive?[Break], arg(1,Break,break),
	Termination_Info = {Debug_Info, NewDebug_Info, Done},
	Debug_Info = {Breaks, Depth, Execute_or_Interpret, Channels} :
	    Debug_Info' = {Breaks', Depth, Execute_or_Interpret, Channels},
	    Termination_Info' = {Debug_Info', NewDebug_Info, Done} |
		breaks#add(Break,Breaks,Breaks',query-_Query,Error_Msg),
		send_message(Error_Msg, Print_Requests, Print_Requests'),
		read(Signals, Directive', Print_Requests', Goal_Info,
			Termination_Info');

	Directive?[break],
	Termination_Info = {Debug_Info, NewDebug_Info, Done},
	Debug_Info = {Breaks, Depth, Execute_or_Interpret, Channels} :
	    Debug_Info' = {Breaks', Depth, Execute_or_Interpret, Channels},
	    Termination_Info' = {Debug_Info', NewDebug_Info, Done} |
		breaks#add(break,Breaks,Breaks',query-_Query,Error_Msg),
		send_message(Error_Msg, Print_Requests, Print_Requests'),
		read(Signals, Directive', Print_Requests', Goal_Info,
			Termination_Info');

	Directive?[remove],
	Goal_Info = {Goal, Location, _Execute_Info},
	Termination_Info = {Debug_Info, NewDebug_Info, Done},
	Debug_Info = {Breaks, Depth, Execute_or_Interpret, Channels} :
	    Debug_Info' = {Breaks', Depth, Execute_or_Interpret, Channels},
	    Termination_Info' = {Debug_Info', NewDebug_Info, Done} |
		break(Location,Goal,Breaks,_Answer,Index),
		remove(Index, Breaks,Breaks',Error_Msg),
		complete(Signals, Directive', Print_Requests, Goal_Info,
			Termination_Info', Error_Msg);

	Directive?[Remove], arg(1,Remove,remove),
	Termination_Info = {Debug_Info, NewDebug_Info, Done},
	Debug_Info = {Breaks, Depth, Execute_or_Interpret, Channels} :
	    Debug_Info' = {Breaks', Depth, Execute_or_Interpret, Channels},
	    Termination_Info' = {Debug_Info', NewDebug_Info, Done} |
		breaks#check_and_remove(Remove,Breaks,Breaks',q-_Q,Error_Msg),
		send_message(Error_Msg, Print_Requests, Print_Requests'),
		read(Signals, Directive', Print_Requests', Goal_Info,
			Termination_Info');

	Directive?[derive(M,L)],
	Goal_Info = {_Goal, _Location, Execute_Info},
	Execute_Info = {Body, _ID, _Time},
	Termination_Info = {Debug_Info, NewDebug_Info, Done},
	Debug_Info = {Breaks, Depth, Execute_or_Interpret, Channels} :
	    Debug_Info' = {Breaks', Depth, Execute_or_Interpret, Channels},
	    Termination_Info' = {Debug_Info', NewDebug_Info, Done} |
		breaks#derive(Body,L,M,Breaks,Breaks',query-_Query, Error_Msg),
		send_message(Error_Msg, Print_Requests, Print_Requests'),
		read(Signals, Directive', Print_Requests', Goal_Info,
			Termination_Info');

	Directive?[list_breaks],
	Termination_Info = {Debug_Info, _NewDebug_Info, _Done},
	Debug_Info = {Breaks, _Depth, _Execute_or_Interpret, _Channels} :
	    Print_Requests = [display_stream(Printable_Breaks,
	  			[prefix(breakpoint)]),
				continue | Print_Requests'] |
		format(Breaks,Printable_Breaks),
		read(Signals, Directive', Print_Requests', Goal_Info,
			Termination_Info);

	Directive?[clear],
	Termination_Info = {Debug_Info, NewDebug_Info, Done},
	Debug_Info = {_Breaks, Depth, Execute_or_Interpret, Channels} :
	    Debug_Info' = {[], Depth, Execute_or_Interpret, Channels},
	    Termination_Info' = {Debug_Info', NewDebug_Info, Done} |
		send_message([], Print_Requests, Print_Requests'),
		read(Signals, Directive', Print_Requests', Goal_Info,
			Termination_Info');

	Directive = [ [trace] | _Directive'] :
	    Signals = _, Goal_Info = _ |
		setup_trace(post, Print_Requests, Termination_Info);

	Directive = [ [trace(Loc)] | _Directive'],
	known(Loc) :
	    Signals = _, Goal_Info = _ |
		setup_trace(Loc, Print_Requests, Termination_Info);

	Directive?[depth(NewDepth)],
	Termination_Info = {Debug_Info, NewDebug_Info, Done},
	Debug_Info = {Breaks, _Depth, Execute_or_Interpret, Channels} :
	    Debug_Info' = {Breaks, NewDepth, Execute_or_Interpret, Channels},
	    Termination_Info' = {Debug_Info', NewDebug_Info, Done} |
		send_message([], Print_Requests, Print_Requests'),
		read(Signals, Directive', Print_Requests', Goal_Info,
			Termination_Info');

	Directive?[data(Data)],
	Goal_Info = {Goal, _Location, Execute_Info},
	Execute_Info = {_Body, Id, _Time} :
	  Data = [goal(Goal),id(Id)] |
		send_message([], Print_Requests, Print_Requests'),
		read(Signals, Directive', Print_Requests', Goal_Info,
			Termination_Info);

	Directive = [ [execute] | _Directive'],
	Termination_Info = {Debug_Info, NewDebug_Info, Done},
	Debug_Info = {_Breaks, _Depth, _Execute_or_Interpret, Channels} :
	    Signals = _, Goal_Info = _,
	    Done = done,
            Print_Requests = [],
	    NewDebug_Info = {[], -1, execute, Channels} |
		true;

	Directive?[help] |
		help(main,Message, [list]),	
		send_message(Message, Print_Requests, Print_Requests'),
		read(Signals, Directive', Print_Requests', Goal_Info,
			Termination_Info);

	Directive?[help(Subject)] |
		help(Subject, Message, [list]),	
		send_message(Message, Print_Requests, Print_Requests'),
		read(Signals, Directive', Print_Requests', Goal_Info,
			Termination_Info);

	otherwise,
	Directive?[User_Input] :
	    Error = display([wrong_directive(User_Input)|Q]\Q,[list]) |
		send_message(Error, Print_Requests, Print_Requests'),
		read(Signals, Directive', Print_Requests', Goal_Info,
			Termination_Info).

/***********************************************************************/
procedure setup_trace(Loc, Print_Req, Termination_Info).
setup_trace(Loc, Print_Req, Termination_Info) :-
	Loc =\= pre, Loc =\= post :
	    Loc' = post,
	    Print_Req!display([invalid_location(Loc),
					", 'post' assumed."|Q]\Q, [list]) |
		setup_trace;

	otherwise,
	Termination_Info = {Debug_Info, NewDebug_Info, Done},
	Debug_Info	 = {_Breaks, Depth, Execute_or_Interpret, Channels} :
	    Done	 = done,
	    Print_Req	 = [],
	    Breaks'	 = [{Loc, all/all, print}],
	    NewDebug_Info= {Breaks', Depth, Execute_or_Interpret, Channels} |
		true.

/***********************************************************************/
procedure complete(Signals, Directive, Print_Requests, Goal_Info,
			Termination_Info, Error_Msg).
complete(Signals, Directive, Print_Requests, Goal_Info,
	 Termination_Info, Error_Msg):-
	Error_Msg = [] :
	    Directive' = [[] | Directive] |
		answer(Signals, Directive', Print_Requests, Goal_Info,
			Termination_Info);

	Error_Msg =\= [] |
		send_message(Error_Msg, Print_Requests, Print_Requests'),
		read(Signals, Directive, Print_Requests', Goal_Info,
			Termination_Info).

/***********************************************************************/
procedure send_message(Msg, Head, Tail).
send_message(Msg, Head, Tail) :-
	Msg = [] :
		Head = [continue | Tail] | true;

	Msg =\= [] :
		Head = [Msg, continue | Tail] | true.

/***********************************************************************/
procedure format(Breaks, Printable_Breaks).
% type_check {format / 2, [no([format / 2, 1, List, 1, post / 2, 1], _ / _)]}
format(Breaks, Printable_Breaks) :-
	Breaks = [] :
	    Printable_Breaks = [] | true;

	Breaks?Break,
	Break = {_Location,_Predicate/_Arity,_Mode} :
	    Printable_Breaks!Break |
		format;

	Breaks?{Location,pattern(FG),Mode} :
	    melt(FG,Melted_Goal,_),
	    Printable_Breaks!{Location,Melted_Goal,Mode} |
		format.

/***********************************************************************/


/***********************************************************************/

procedure break(Location,Goal,Breaks,Answer,Index).

break(Location,Goal,Breaks,Answer,Index) :-
	arg(1,Goal,Predicate),
	Arity := arity(Goal) - 1 |
		break1(Location,Goal,Predicate/Arity,Breaks,Answer,Index,0);

	constant(Goal) |
		break1(Location,Goal,Goal/0,Breaks,Answer,Index,0);

	otherwise |
		break1(Location,Goal,[]/0,Breaks,Answer,Index,0).

/***********************************************************************/
procedure break1(Location,Goal,PredicateArity,Breaks,Answer,Index,Current).
% type_check {break1 / 7, [no([break1 / 7, 4, List, 1, post / 2, 1], _ / _)]}
break1(Location,Goal,PredicateArity,Breaks,Answer,Index,Current) :-
	Breaks?{Loc,_PredArity,_Mode},
	Loc =\= Location,
	Current' := Current + 1 |
		break1;

	Breaks?{Location,Pred/_A,_Mode},
	PredicateArity = Predicate/_Arity,
	Pred =\= Predicate, Pred =\= all,
	Current' := Current + 1 |
		break1;

	Breaks?{Location,Predicate/A,_Mode},
	PredicateArity = Predicate/Arity,
	A =\= Arity, A =\= all,
	Current' := Current + 1 |
		break1;

	Breaks = [{Location,PredicateArity,Mode} | _Breaks'] :
	    Goal = _,
	    Answer = Mode,
	    Index  = Current ;

	Breaks = [{Location,all/all,Mode} | _Breaks'] :
	    Goal = _, PredicateArity = _,
	    Answer = Mode,
	    Index  = Current ;

	PredicateArity = _Predicate/Arity,
	Breaks = [{Location,all/Arity,Mode} | _Breaks'] :
	    Goal = _,
	    Answer = Mode,
	    Index  = Current ;

	PredicateArity = Predicate/_Arity,
	Breaks = [{Location,Predicate/all,Mode} | _Breaks'] :
	    Goal = _,
	    Answer = Mode,
	    Index  = Current ;

	Breaks = [{Location,pattern(FG),Mode} | _Breaks'] :
	    melt(FG,Goal,_),
	    PredicateArity = _,
	    Answer = Mode,
	    Index  = Current | true;

	Breaks?{Location,pattern(_FG),_Mode},
	otherwise,
	Current' := Current + 1 |
%	    melt(FG,Melted_Goal,_) |
%		screen#display(break1(Melted_Goal, Goal)),
		break1;

	Breaks = [] :
	    Location = _, Goal = _, PredicateArity = _, Current = _,
	    Index  = -1,
	    Answer = dont_ask |
		true.

/***********************************************************************/
procedure write(What, Channel).
write(What, Channel) :-
	true :
	    write_channel(What, Channel) |
		true;

	otherwise :
	    Channel = _ |
		abortio(What).

/***********************************************************************/
procedure abortio(What).
abortio(What) :-
	arg(1,What,display) |
		true;

	arg(1,What,display_stream) |
		true;

	What = ask(_Prompt, Answer, _How) :
	    Answer = [] |
		true.

/***********************************************************************/
procedure remove(Index, Breaks, NewBreaks, Error_Msg).
remove(Index, Breaks, NewBreaks, Error_Msg) :-
	Index < 0 :
	    Breaks    = NewBreaks,
	    Error_Msg = display(["No Current BreakPoint." | Q ] \Q, [list]) |
		true;

	Index = 0,
	Breaks?_Current_BreakPoint :
	    NewBreaks = Breaks',
	    Error_Msg = [] |
		true;

	Index > 0,
	Breaks?BreakPoint,
	Index' := Index - 1 :
	    NewBreaks!BreakPoint |
		remove;

	Breaks = [] :
	    NewBreaks = [],
	    Error_Msg = display(["Please report Internal Error:  ",
				user#remove(Index, []) | Q] \ Q, [list]) |
		true.

/***********************************************************************/
procedure help(Subject, Help_Msg, Format).
help(Subject, Help_Msg, Format) :-
	Subject = main :
	    Help_Msg = display(["

		Help Main Menu
The debugger is currently in the 'Directive' mode, Channel it is waiting
for a response at a break point.  The following are valid responses:
<CR>, y, yes	If this is a valid goal.
wait(Variable)	Suspend this goal until Variable is instantiated.
break		set a break point for this process and it's descendents.
remove		remove a break point for this process and it's descendents.
clear		remove all beak points for this process and it's descendents.
trace		Setup break(all/all,print,post) for this process.
derive		enter all direct descendents into the break point list.
list_breaks	list all active break points for this process.
depth(NewDepth)	Change depth for this goal to NewDepth.
data(Data)	further instantiate the head of a clause.
execute		let this goal continue out of the debugger as a subcomputation.
debug		change to temporary command mode.
help		print this menu.
help(Subject)	print out help for Subject.
			"|Q]\Q,Format);


	Subject = yes |
		Help_Msg = display(["
yes (<carriage-return> or y) will allow the goal and all derived goals
to continue being interpreted via the debugging system.  This implies that
the process is valid.
			"|Q]\Q,Format);


	Subject = wait |
		Help_Msg = display(["
wait(Variable) suspend the reduction of this goal (or all it's descendents
if this is a post reduction break), until Variable is 'known' (i.e. the
guard predicate known(Variable) will succeed).
			"|Q]\Q,Format);

	Subject = break|
		Help_Msg = display(["
break sets a break point for this process and all it's descendents.  There
are two types of break points.  A print break point will print the goal 
when this break point is satisfied, and continue.  A query break point will
read_again for a valid directive before continuing with the reduction of this goal.
A break point may be at one of two locations.  A pre-reduction break point
occurs immediately preceeding the reduction of the goal.  A post-reduction
break point occurs immediately following the reduction of the goal.

break has the following formats:
						meaning
break					break(all/all,query,post)
break(Predicate)			break(Predicate,query,post)
break(Predicate,Query_Print)    	break(Predicate,Query_Print,post)
break(Predicate,Query_Print,Pre_Post)
break(Predicate/Arity,query_Print,Pre_Post).

If Predicate is a constant, then it defaults to Predicate/all.  If predicate
is a tuple, it is assumed to be a pattern, which will be matched against
goals.  If a goal matches the pattern, then the breakpoint is taken.
Predicate may also be of the form Predicate/Arity.  Predicate/all is satisfied 
whenever the predicate is requested.  Predicate/Arity is satisfied whenever
the predicate is requested, with a matching arity.  Note all is a reserved
keyword, which implies that anything matches the condition.  Therefore all/3
matches all predicates of arity 3, while p/all matches the predicate p,
(ir)regardless of it's arity.
			"|Q]\Q,Format);

	Subject = remove |
		Help_Msg = display(["
remove will remove a break point for this process and all it's descendents.

The formats of the remove are:
remove			remove the breakpoint that stopped this process.
remove(Predicate)	remove the first breakpoint for this Predicate.
remove(Predicate,Pre_Post) remove the first breakpoint at this location.

Predicate may either be of the form Predicate/Arity or Pattern.  If the 
Predicate is a constant, then it matches the first breakpoint with the
same name.
			"|Q]\Q,Format);

	Subject = clear |
		Help_Msg = display(["
clear will clear the entire break point list for this process and all it's
descendents.
			"|Q]\Q,Format);

	Subject = trace |
		Help_Msg = display(["
trace		set break(all/all,print,post)
trace(Pre_Post)	set break(all/all,print,Pre_Post)
trace will set the break point list for this process and all it's descendents
to print out a trace of the execution from this point onwards.  To cancel
this, a command must be given (i.e. clear, remove(all/all,Pre_Post)).
			"|Q]\Q,Format);

	Subject = list_breaks|
		Help_Msg = display(["
list_breaks will list all break points for the current process.
			"|Q]\Q,Format);

	Subject = depth |
		Help_Msg = display(["
depth(NewDepth) will change the depth bound for the current process to
NewDepth.  At each reduction the depth is decremented until zero, at
which point it suspends until changed manually via the depth command.
A depth of -1 is treated to be of infinite depth.
			"|Q]\Q,Format);

	Subject = data |
		Help_Msg = display(["
data(goal(Goal), id(Id)) is a method by which the user may further instantiate
the head of a goal, or specify the clause-id which should be used in reduction.
Note that the clause-id should only be used if it will not cause the clause to
suspend, since the method of aborting a computation is to instantiate the 
clause-id to a string constant.
			"|Q]\Q,Format);

	Subject = execute |
		Help_Msg = display(["
execute will release the goal from the control of the debugger, and create a
subcomputation for the goal to execute.
			"|Q]\Q,Format);

	Subject = debug |
		Help_Msg = display(["
debug will switch the debugger from directive mode to temporary command mode.
In this mode all valid commands may be executed.  To return to the directive
the command 'query' should be entered.  No further directives will be requested
until the current directive is handled (i.e. the user shouldn't stay in
temporary command mode for extended periods of time, since the computation
might suspend waiting for results of predicates which are waiting for a
directive).  The following formats are available:
			meaning:
debug		switch to temporary command mode until the user requests
		to continue with directive requests.
debug#command	send the command to the command manager, and remain in
		command mode.

		The query command will return to the current directive request.
		query#response will reply to the current directive request with
		response, and continue processing directives and commands.
			"|Q]\Q,Format);

	Subject = help |
		Help_Msg = display(["
The help subsystem is available in both command and directive modes, although
the valid responses are different, or have slightly different meaning.  To
see a list of commands and a short description, enter help.  To receive
further information about a specific directive, enter 'help(Directive)'.
			"|Q]\Q,Format);

	otherwise :
	    Subject = _ |
		Help_Msg = display(["
Invalid subject.  Valid directives may be reviewed via 'help', and further
information may be received via 'help(Directive)'
			"|Q]\Q,Format).
