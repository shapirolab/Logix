/*

Shell server
Ehud Shapiro, 02-22-85
Bill Silverman 86-89

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:33 $
Currently locked by 	$Locker:  $
			$Revision: 1.1.1.1 $
			$Source: /home/qiana/Repository/Logix/system/shell_server/self.cp,v $

Copyright (C) 1989, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([serve/4, compile/2,
	 root/7, user/2, user/3,
	 delegates/2, delegates/3, shell/3]
).
-mode(trust).
-language(compound).

root(Bytes, Faults, Events, Signals, Out, Domain, TC) :-
	control # root(Bytes, Faults, Events, Signals, Out, Domain, TC).

user(OldWorkingDirectory, Input) + (BinMode = query) :-
	control # user_shell_context(OldWorkingDirectory, BinMode, Input).

shell(Requests, Events, Intercept) :-
	sub_computation # start(Requests, Events, Redelegates, Delegates),
	delegates.

delegates(Delegates, Redelegates) + (Intercept = [display]) :-
	delegates # start(Delegates, Redelegates, Intercept).

/*
** Requests? = Input stream of shell goals (from computation)
** GroundGoals? = Stream of shell goals (from keyboard)
** Forward^ = Output stream of remaining commands (after all processing)
** Controls^ = Control functions for shell_computation + Termination Signal
*/

procedure serve(Requests, GroundGoals, Forward, Controls).

serve(Requests, GroundGoals, Forward, Controls) :-
	computation # (self # service_id(SuperId)),
	edit(GroundGoals, GroundGoals'),
	compile(GroundGoals', CompiledGoals),
	goals_merger(CompiledGoals, Requests, Commands),
	user_macros([change_context(SuperId) | Commands], [], Commands1),
	system_macros(Commands1, Commands2),
	status(Commands2, error, 0, Commands3),
	initialize_computations(State),
	computations(Commands3, State, Commands4),
	output(	[prompt('@') | Commands4], [], _Break,
			Controls(SuperId, Forward)
	).

% shell is a pipe: edit || compile || goal
%			|| user_macros || system_macros || status
%			|| computations|| output || to_context

% ----------------------------- edit/2 ---------------------------------------

/*
** edit shell commands, separating commands by semi-colons.
*/


edit(In, Out) :-
  In ? (A; B) |
	edit([A ,B | In'], Out);

  otherwise,
  In ? X :
     Out ! X |
	edit;

  In = [] :
     Out = [].

%--------------------------- compile ------------------------------------------

/*
** Compile shell commands, applying conventions for suffix ^
*/


procedure compile(Terms,Goals).
compile(Terms,Goals) :-
  Terms ? Term |
	computation # Requests?,
	compile_goal(Term, Goals, Goals', Requests),
	compile;

  Terms = [] :
     Goals = [].

compile_goal(Term, Head, Tail, Requests) :-
  Term = {Hat,_X},
  Hat  = '^' :
     Head = [Term | Tail?],
     Requests = [];

  Term = ~`Name,
  constant(Name) :
     Head = Tail,
     Requests = dictionary(unbind, Name);

  Term = unbind(`Name),
  constant(Name) :
     Head = Tail,
     Requests = dictionary(unbind, Name);
	
  otherwise |
	goal_compiler # display(Term, Goal, Dictionary, Done),
	variable_dictionary(	Dictionary,
				_,Display,
				Requests, Requests1, Requests1
	),
	release_goal(Display,Done,Head,[Goal|Tail]).


release_goal(Display, Done, In, Out) :-
  Display ? D :
     In ! D |
	release_goal;

  Display = [] :
     Done = done,
     In   = Out.

variable_dictionary(In, DisplayIn, DisplayOut, Requests, Requests1, Add) :-
  In ? add(X,Y) :
     Add ! dictionary(add, X, Y, Reply) |
	when(Reply,DisplayOut',DisplayOut),
	variable_dictionary;

  In ? display(S,Y,Y'),
  string(S), S =\= '_' :
     Y = Y',
     Requests ! dictionary(unbind, S),
     DisplayIn' = DIn? |
	sublist([display_variable(S,Y)|_], DisplayIn, DisplayIn, DIn),
	variable_dictionary;

  In ? display('_',Y,Y') :
     Y = Y',
     DisplayIn' = [display_variable('_',Y) | DisplayIn] |
	variable_dictionary;

  otherwise,
  In ? display('_',Y,{Hat,Y'}),
  Hat = "^" :
     Y = Y' |
	variable_dictionary;

  In = [] :
     Requests = Requests1,
     Add = [] |
	sublist([], DisplayIn, DisplayIn, DisplayOut). 

sublist(D,D^,D1,D1^).
sublist(D,[_|Ds],D1,D2) :-	% sublist(D,_Ds,D1,D2) :-
  otherwise |			% D =\= _Ds, _Ds = [_|Ds] |
	sublist(D,Ds,D1,D2).

when(Reply,DisplayIn,DisplayOut) :-
  known(Reply) :
     DisplayIn = DisplayOut.

% ----------------------------- goal/3 ---------------------------------------

/*
** Merge command stream goals, input requests and deferred goals.
*/

goals_merger(Xs, Rs, Ys) :-

    true : Halt = _,
      make_channel(DC, Ds) |
	goals.

goals(Xs, Rs, Ds, Halt, Ys, DC) :-

    Xs ? G |
	goal;

    Rs ? G |
	goal;

    Ds ? G |
	goal;

    Xs = [] : Rs = _, Ds = _,
      Halt = halt,
      Ys = [],
      close_channel(DC) ;

    Rs = [] : Xs = _, Ds = _,
      Halt = halt,
      Ys = [],
      close_channel(DC) .

/*
** Parse compound input goals ([F | Gs], (F ; Gs)) ;
** defer insufficiently instantiated goals ;
** defer/serve conditional goals.
*/

goal(G, Xs, Rs, Ds, Halt, Ys, DC) :-

    unknown(G) :
      V = G? |
	defer,
	goals;

    tuple(G), arg(1, G, V), unknown(V) |
	defer,
	goals;

    G ? G'' :
      Xs' = [G' | Xs] |
	self;

    G = (G' ; X) :
      Xs' = [X | Xs] |
	self;

    G = defer(V, G') |
	defer,
	goals;

    G = if(V, T) :
      F = [] |
	conditional;

    G = if(V, T, F) |
	conditional;

    G = [] |
	goals;

    otherwise :
      Ys ! G |
	goals.

conditional(G, Xs, Rs, Ds, Halt, Ys, DC, V, T, F) :-

    unknown(V) : T = _, F = _ |
	defer,
	goals;

    V = true : G = _, F = _,
      G' = T |
	goal;

    V =\= true : G = _, T = _,
      G' = F |
	goal.

defer(V, G, Halt, DC) :-

    Halt = halt : V = _, G = _, DC = _ ;

    known(V) : Halt = _ |
	write_channel(G, DC).

% ------------------------- user_macros/3 -------------------------------------

/*
** serve front-end commands
** forward others via user_macros service
*/

user_macros(Xs, UMs, Ys) :-
  Xs ? X,
  X =\= filter_user_macros(_,_),
  X =\= change_context(_) :
     UMs ! expand(X, Ys\Ys') |
	user_macros;

  Xs ? change_context(NewId) :
     UMs = [],
     Ys ! '_set_scope'(NewId, OldId, Reply) |
	user_macros_stream(Reply, OldId, NewId, ShellId),
	computation #
		call([[user_macros | ShellId] # service_id(UMId)], Events),
	user_macros_id(UMId, Events, UMs'),
	user_macros;

  Xs ? filter_user_macros(Xs1', Xs'') :
     Xs1' = Xs'|
	user_macros;

  Xs = [] :
     UMs = [],
     Ys  = [];

  Xs =\= [_|_], Xs =\= [] :
     UMs = [] |
	broken_stream(filter_user_macros, Xs, Ys).


user_macros_stream(Ok, Old, New, Shell) :-
  Ok = true :
     Old   = _,
     Shell = New;

  Ok = false(_) :
     New   = _,
     Shell = Old.

user_macros_id(Id, Events, UMs) :-
  known(Id) :
     Events = _ |
	Id # UMs;

  Events = [Event | _Events'],
  Event =\= suspended, Event =\= resumed, Event =\= comment(_) :
     Id = _ |
	user_macros # UMs;

  otherwise,
  Events ? _Event |
	user_macros_id.


% ----------------------- system_macros/3 -------------------------------------

/*
** serve filter_system_macros command
** forward others via system_macros service
*/

system_macros(Xs, Ys) :-
  Xs ? filter_system_macros(Xs1', Xs'') :
     Xs1' = Xs' |
	system_macros;

  otherwise,
  Xs ? X |
	system_macros # expand(X, Ys, Ys'),
	system_macros;

  Xs = [] :
     Ys = [];

  Xs =\= [_|_], Xs =\= [] |
	broken_stream('user_macros/filter_system_macros', Xs, Ys).

% ---------------------------- status/4 ---------------------------------------

/*
** serve local value commands
** forward others
*/

status(In, CurrentService, CurrentComputation, Out) :-
  In ? computation(Computation, OldComputation) :
     CurrentComputation' = NewComputation? |
	computation(Computation, CurrentComputation, NewComputation),
	unify_without_failure(CurrentComputation,OldComputation),
	status;

  In ? service(Service, OldService) :
     CurrentService' = NewService? |
	service(Service, CurrentService, NewService),
	unify_without_failure(CurrentService, OldService),
	status;

  otherwise,
  In ? X :
     Out ! X |
	status;

  In = [] :
     CurrentService = _, CurrentComputation = _,
     Out = [].


service(Service,Service^,Service^).
service(Service,_,Service^) :-
	string(Service) | true.
service(_,Service,Service^) :-
	otherwise | true.


computation(N,N^,N^).
computation(N,_,N^) :-
	N > 0 | true.
computation(_,N,N^) :-
	otherwise | true.

% ---------------------------- computations/3 ---------------------------------

/*
** serve computation oriented commands
** forward others
*/

% 	State = {All,Next,Stack}

initialize_computations({_All,0,[]}^).

reset(State,State') :-
	terminate(State),
	initialize_computations(State').

terminate(stop(_,_)^).


computations(Xs, State, Ys) :-
  Xs ? filter_computations(Xs1',Xs'') :
     Xs1' = Xs' |
	computations;

  Xs ? X,
  X =\= filter_computations(_,_) |
	execute(X,State,State'),
	forward(State',Ys,Ys',State''),
	computations;

  Xs = [] :
     Ys = [] |
	terminate(State);

  otherwise |
	computations(Relay,State,Ys),
	broken_stream(filter_computations,Xs,Relay).


forward(StateIn, Xs, Ys, StateOut) :-
  StateIn = {X,State} :
     Xs = [X | Ys],
     StateOut = State;

  otherwise :
     Xs = Ys,
     StateIn = StateOut.


broken_stream(From,Residue,To) :-
	computation#
	    display(term, shell-terminating(broken_stream_from-From, Residue),
		    close([],To)
	    ).

execute(G, State, State') :-

  G = reset |
	reset(State, State');

  G = status(Status) :
     State = State'  |
	unify_without_failure(Status, Status'?),
	status_range(State, Status');

  G = start(RPC, N, Events, Reply, Intercept),
  RPC = _#_ |
	unify_without_failure(N, N'?),
	unify_without_failure(Events, Events'?),
	unify_without_failure(Reply, Reply'?),
	start(RPC, N', Events', Reply', Intercept, State, State');

  G = monitor(N, Events, Events', Requests, Reply) |
	unify_without_failure(N, N'?),
	unify_without_failure(Requests, Requests'?),
	unify_without_failure(Events', Events''?),
	unify_without_failure(Reply, Reply'?),
	monitor(N', Events, Events'', Requests', Reply', State, State');

  G = request(Request, N, Reply) |
	unify_without_failure(Reply, Reply'?),
	send_request(N, Request, Reply', State, State');

  G = events(N, Events, Reply) |
	unify_without_failure(Reply, Reply'?),
	send(N,events(Events, Reply'), State, State') ;

  G = new_goal(N, Goal) |
	send(N, new_goal(Goal), State, State');

  G = resolvent(N, Resolvent, Reply) |
	unify_without_failure(Resolvent, Resolvent'?),
	unify_without_failure(Reply, Reply'?),
	send(N, control(state(Resolvent'), Resolvent', Reply'), State, State');

  G = state(N, Goal, Events, Reply) |
	unify_without_failure(Goal, Goal'?),
	unify_without_failure(Events, Events'?),
	unify_without_failure(Reply, Reply'?),
	send(N, state(Goal', Events', Reply'),State,State');

  G = extract(Index, N, RPC, Reply) |
	unify_without_failure(RPC, RPC'?),
	unify_without_failure(Reply, Reply'?),
	send(N,control(extract(Index, Result), Result, Answer), State, State'),
	extract(Answer, Result, RPC', Reply');

  G = Service # Goal :
       State' = {to_context(Service#Goal),State};

  otherwise :
       State' = {G, State}.

% Computation = {RPC,Commands,Events}  on stack

start(RPC, Next, UserEvents, Reply, Intercept, State, State') :-
  State  = {All,Last,Stack},
  Next^ := Last + 1 :
     UserEvents ! started,
     Reply = true,
     State'= {All, Next, [ {RPC, Commands, Events} | Stack ]} |
	shell([RPC, identifier(Next) | Requests], Events, Intercept),
	computation(running, Commands, Events, All, verbose(UserEvents'),
			Requests
	);

  otherwise : RPC = _, Intercept = _,
     UserEvents = [] ,
     State = State' |
	illegal_service(State,Next,Reply).

monitor(Next, Events, UserEvents, Requests, Reply, State, State') :-
  State = {All, Last, Stack},
  Next^ := Last + 1 :
     UserEvents ! monitored,
     Reply = true,
     State' = {All, Next, [{monitored, Commands, Events} | Stack]} |
	computation(running, Commands, Events, All, quiet(UserEvents'),
			Requests
	).


illegal_service(State, Next, Reply) :-
  State  = {_,Last,_},
  Last++ |
	unify_without_failure(Reply, false(illegal_service)),
	unify_without_failure(Last', Next).


status_range(Status, Range) :-
  Status = {_,0,_} :
     Range = idle;

  Status = {_,1,_} :
     Range = 1;

  otherwise,
  Status = {_,Last,_} :
     Range = 1-Last.


send_request(N,Request,Reply,State,State') :-
  N = all,
  string(Request),
  State = {All,Next,Stack} :
     All   ! Request,
     Reply = true,
     State' = {All',Next,Stack};

  integer(N) |
	send(N,{Request,Reply},State,State');

  otherwise :
     N = _, Request = _,
     Reply = false(invalid_computation),
     State = State'.


send(N,Command,State,State') :-
  unknown(N),
  State = {_,Last,_} :
     N = Last |
	send(N,Command,State,State');

  State = {All,Last,Stack},
  0 < N, N =< Last,
  Index := Last - N :
     State' = {All,Last,Stack1} |
	send1(Index,Command,Stack,Stack1);

  otherwise :
     N = _,
     State = State' |
	reject_command(Command,no_computation).

send1(N,Command,Stack,Stack1) :-
  N = 0,
  Stack = [{G,Cs,Es}|Rest] :
     Stack1 = [{G1,Cs1,Es}|Rest] |
	send2(Command,G,G1,Cs,Cs1,Es);

  N > 0, N' := N - 1,
  Stack ? Computation :
     Stack1 ! Computation |
	send1.
  
send2(Command,G,G1,Cs,Cs1,Es) :-
  Command = events(Events,True) :
     Events = Es,
     True = true,
     G    = G1,
     Cs   = Cs1;

  Command = new_goal(Goal) :
     G    = _, Es = _,
     Goal = G1,
     Cs   = Cs1;

  Command = state(Goal,Events,Reply) :
     Goal = G, G = G1,
     Cs   = Cs1 |
        copy_events(Es,EL,EL,Events,Reply);

  otherwise :
     Es   = _,
     G    = G1,
     Cs   = [ Command | Cs1 ].


copy_events(Es,Fs,El,Events,Reply) :-
  Es ? E :
     Fs ! E |
	copy_events;

  unknown(Es) :		% Change to ask_unknown;  Add tell_unknown?
     Fs = [],
     El = Events,
     Reply = true;

  Es = [] :
     Fs = [],
     El = Events,
     Reply = true.



extract(Answer,Result,RPC,Reply) :-
  Result = [] :
     RPC = false,
     Reply = Answer;

  Result = Name#Goal :
     Answer = _,
     RPC = Name#Goal,
     Reply = true;

  otherwise :
     Answer = _, 
     RPC   =  false,
     Reply = false(Result).
     

/*
** process per shell computation
*/

%procedure computation(Status+,Commands?,Events?,All?,UserState^,Requests^),

computation(Status,Commands,Events,All,UserState,Requests) :-
  Commands ? Command |
	command(Command,Status,Commands',Events,All,UserState,Requests);

  Events ? Event,
  known(Event),
  UserState = Report(UserEvents) :
     UserState' = Report(UserEvents') |
	computation_event(Event, Report, Status, Status', Requests, Requests'),
	user_event(Status, Event, Report, UserEvents, UserEvents'),
	computation;

  All ? Signal |
	command({Signal,_},Status,Commands,Events,All',UserState,Requests);

  Commands = [] :
     Status = _, Events = _, All = _,
     UserState = _([]),
     Requests   = [];

  All = stop :
     Status = _, Commands = _, Events = _,
     UserState = _([]),
     Requests   = [].


computation_event(Event, Report, Status, Status', Requests, Requests') :-

  Event = suspended,
  Status = running : Report = _,
    Status' = waiting,
    Requests = Requests' ;

  Event = resumed,
  Status = waiting : Report = _,
    Status' = running,
    Requests = Requests' ;

  Event = terminated,
  Status =\= aborted : Report = _,
    Status' = terminated,
    Requests = Requests' ;

  Event = failed(Goal, _),
  Status = running |
	computation_failed(Goal, Report, Status', Requests, Requests');

  Event = event(_),
  Report =\= diagnostic,
  Status = running :
     Status' = waiting,
     Requests ! suspend ;

  otherwise : Event = _, Report = _,
     Status' = Status,
     Requests = Requests' .

computation_failed(Goal, Report, Status, Requests, Requests') :-

  Goal =\= call(_), Report =\= diagnostic :
     Status = waiting,
     Requests ! suspend ;

  otherwise : Goal = _, Report = _,
     Status = running,
     Requests = Requests' .

user_event(Status, Event, Report, Events, Events') :-

    Status = terminated,
    string(Event), Event =\= aborted : Report = _,
      Events = Events' ;

    Status = aborted : Event = _, Report = _,
      Events = Events' ;

    otherwise : Status = _ |
	report_user_event(Report, Event, Events, Events').

report_user_event(Report, Event, Events, Events') :-

    Event = comment(_),
    Report =\= verbose :
      Events = Events' ;

    Event = diagnostic(_),
    Report = silent :
      Events = Events' ;

    otherwise : Report = _,
      Events ! Event .


command(Command,Status,Commands,Events,All,UserState,Requests) :-
  Status =\= terminated, Status =\= aborted |
	command1(Command,Status,Commands,Events,All,UserState,Requests);

  Command = abort(True) :
     Events = _,
     True = true,
     Status = terminated,
     UserState = _Report([aborted]),
     Requests = [abort] |
	computation(aborted,Commands,NoEvents,All,silent(NoEvents),_);

  otherwise |
	reject_command(Command,Status),
	computation(Status,Commands,Events,All,UserState,Requests).

  
command1(Command,Status,Commands,Events,All,UserState,Requests) :-
  Command = control(Request,Result,Reply) |
	suspend_computation(Status,Requests,[Request|Requests']),
	controlled_computation(Request,Result,Reply),
	computation(waiting,Commands,Events,All,UserState,Requests');

  Command = {report(New, Old),true^},
  UserState = Report(UserEvents) :
     UserState' = New?(UserEvents) |
	computation(Status,Commands,Events,All,UserState',Requests),
	unify_without_failure(Report, Old);

  Command = {Request,Reply},
  Request =\= report(_,_) |
	computation_request(Request,Status,Status',Requests,Requests',Reply),
	abort_events(Request,Events,Events',UserState,UserState'),
	computation(Status',Commands,Events',All,UserState',Requests').


reject_command(Command,Code) :-
	arity(Command,A), arg(A,Command,false(Code)^) |
	reject_command(Command).

reject_command(Command) :-
  Command = events(List,_) :
     List = [];

  Command = state(False,List,_) :
     False= false,
     List = [];

  Command = control(_,List,_) :
     List = [];

  otherwise :
     Command = _ .


/***/
suspend_computation(running,[suspend|Requests]^,Requests).
suspend_computation(waiting,Requests,Requests^).


abort_events(abort,_,NoEvents,_([aborted]^),silent(NoEvents)^).
abort_events(_,Events,Events^,UserState,UserState^) :-
	otherwise | true.


controlled_computation(state(Resolvent),Resolvent^,true^).
controlled_computation(extract(_Index,Found),Found^,true^) :-
	Found = (_#_) | true.
controlled_computation(extract(_Index,Failed),Result,Reply) :-
	Failed =\= (_#_) |
	{Result, Reply} = {[],false(Failed)}.


% computation_request(Request?,Status?,Status'^,Requests^,Requests',Reply^)

computation_request(suspend,running,waiting^,
		    [suspend|Requests]^,Requests,true^).
computation_request(resume,waiting,running^,[resume|Requests]^,Requests,true^).
computation_request(abort,Status,aborted^,[abort]^,_,true^) :-
	Status =\= aborted | true.
computation_request(Request,Status,Status^,[Request|Requests]^,Requests,true^):-
	Request =\= suspend, Request =\= resume, Request =\= abort,
	Status =\= terminated, Status =\= aborted | true.
computation_request(_,Status,Status^,Requests,Requests^,false(Status)^) :-
	otherwise | true.


% ------------------------------ output/4 ------------------------------------

/*
** serve output oriented commands
** Forward others
*/

output(	In, Prompts, Break, Out) :-
  In ? to_context(Goal) |
	to_context(Goal, Out),
	output;

  In ? prompt(Prompt),
  string_length(Prompt,N) |
	prompt_string(N, Prompt, Prompt'),
	output(In',[Prompt' | Prompts],Break,Out);

  In ? pop_prompt |
        pop_prompt(Prompts, Prompts'),
        output;

  In ? delegate(NewForward,OldForward),
  Out = Controls(ContextId,OldForward) :
     Out' = Controls(ContextId,NewForward) |
        output;

  In ? break :
     Break = [abort],
     Break' = _ |
        output;

  In ? display_stream(Stream, Options) |
	output,
	computation #
		call([display(stream, Stream, Options) | Break], _);

  In ? display_variable(Name,Value) |
	computation # [dictionary(state, S) | Cs?],
	output,
	stream_or_incremental(S, Break, Name, Value, Cs);

  In ? '_set_scope'(NewId, ContextId, Reply),
  Out = Controls(ContextId',Forward) :
     ContextId = ContextId',
     Out'      = Controls(ContextId1,Forward) |
	computation # change_scope(NewId, Reply),
	change_scope_reply(Reply, NewId, ContextId, ContextId1),
	output;

  In ? shell_computation(Control) :
     Out     = {[Control'|Controls],Super,Forward},
     Control = Control',
     Out'    = Controls(Super,Forward) |
	output;

  In = [] :
     Prompts = _,
     Break   = [abort],
     Out     = done(_,[]);

  otherwise,
  In ? X :
     Out     = Controls(ContextId,[X|Forward]),
     Out'    = Controls(ContextId,Forward) |
	output.


change_scope_reply(Reply, NewId, ContextId, ContextId') :-
  Reply = true :
     ContextId = _,
     NewId = ContextId';

  Reply = false(Reason) :
     ContextId = ContextId' |
	computation # event(change_context(NewId, Reason)).

to_context(Goals, Out) :-
  Goals ? Goal,
  Out = _Controls(ContextId, _Forward) |
	ContextId # Goal,
	to_context;

  Goals = [] :
     Out = _;

  otherwise |
	to_context([Goals], Out).


prompt_string(N, Prompt, Prompt') :-
  N = 1 :
     Prompt = Prompt' |
	computation # sio(prompt, Prompt);

  otherwise :
     N = _ |
	utils#append_strings([Prompt,' '],Prompt'),
	computation # sio(prompt, Prompt').

pop_prompt(Prompts, Prompts') :-
  Prompts = [_Current,Previous|Stack] :
     Prompts' = [Previous|Stack] |
	computation # sio(prompt, Previous);

  otherwise :
     Prompts = Prompts'.


stream_or_incremental(S, Break, Name, Value, Cs) :-
  S = incremental :
     Cs = [] |
	copy_variable_incrementally(Break, Name, Value);

  otherwise : S = _ |
	copy_variable_to_stream(Break, Name, Value, Cs).


copy_variable_incrementally(Break,Name,Value) :-
  Break = [abort] :
     Name   = _, Value = _;

  list(Value) |
	copy_list_incrementally(Break,Name,1,Value);

  Value =\= [_|_] :
     Break  = _ |
	incremental_element_display(Name = Value, Break, done, _).


copy_list_incrementally(Break,Name,N,Values) :-
  Values ? Value,
  N' := N + 1 |
	incremental_element_display(Name/N = Value, Break, N', N''),
	copy_list_incrementally;

  known(N),
  Values =\= [_|_] |				% usually Values = []
	incremental_element_display(Name/' ' = Values, Break, N, _);

  Break = [abort] :
     Name   = _, N = _, Values = _.

incremental_element_display(NamedTerm, Break, Done1, Done2) :-
	stream # merger(InStream, OutStream),
	computation # call([identifier(shell_list),
			    display(term, First,
				    [type(ground), close(Done1,Done2)]),
			    display(stream, Stream,
				    [known(Done2), type(ground)])
			   | Requests],
			   _
			),
        incremental_display # ground(NamedTerm, InStream, [], Break),
	OutStream? = [First | Rest],
	copy_rest_incrementally(Break, Requests, Rest, Stream).

copy_rest_incrementally(Break, Requests, In, Out) :-
  known(Break) :
     In = _,
     Break = Requests,
     Out = [];

  In ? Term :
     Out ! Term |
	copy_rest_incrementally;

  In = [] : Break = _,
     Requests = [],
     Out = [].


copy_variable_to_stream(Break,Name,Value,Cs) :-
  Break = [abort] :
     Name   = _, Value = _,
     Cs = [];

  Value = [_|_] :
    Cs = call([identifier(shell_list),
	       display(stream, Stream) | Requests], _) |
	copy_list_to_stream(Break,Name,1,Value,Requests,Stream);

  Value =\= [_|_] : Break  = _,
     Cs = display(term, Name=Value).

copy_list_to_stream(Break,Name,N,Values,Requests,Stream) :-
  Break = [abort] :
     Name   = _, N = _, Values = _,
     Requests = Break,
     Stream = [];

  unknown(Break),  
  Values ? Value,
  N' := N + 1 :
     Stream ! Name/N = Value |
	self;

  unknown(Break),  
  Values =\= [_|_] : N = _,
    Requests = [],
     Stream = [Name/' ' = Values].
