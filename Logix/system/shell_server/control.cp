-export([root/7, user_shell_context/3]).
-mode(trust).
-language(compound).

% Start root shell

Signals ::= [Any | Nil] ; [Any | Signals].
MachineError ::= Any(Any, Any).
Delegated ::= failed(Any, Any) ;
	      request([String], Any, Any, Any) ;
	      delegated(Vector, Any, Any, Any) ;
	      link(Any, Any, Any) .

procedure root([Integer], [MachineError], [Any],
		Signals, Delegates, Vector, Vector
).

root(Bytes, Faults, Events, Signals, Out, Root, TerminalCH) :-
    true :
      Signals ! "_domain"(domain_channel(ShellDomain)) |
	delegates # start(Delegates?, Redelegates,
		    	  [dictionary, display, input, library,
			   shell(Bytes, TerminalCH, Commands, Controls)]),
	monitor(Faults?, Events?, Commands?, Controls?, Out'?, Redelegates?,
		control(lost, 0), Signals', Delegates, ShellDomain?, Root
	),
	kluge_initialization(Out, Out').

kluge_initialization(Ds1, Ds2) + (Done = 0) :-

    Ds1 ? delegated(dictionary(inherit, Goals), {_, Left, Right, _}) :
      Left = Right |
	dictionary_server # start(Goals),
	kluge_update;

    Ds1 ? delegated(display(options, Options), {_, Left, Right, _}) :
      Left = Right |
	display_server # initial([{options(Options), _Done}]),
	kluge_update;

    Ds1 ? delegated(library(retrieve, Current), {_, Left, Right, _}) :
      Current = [],
      Left = Right |
	kluge_update;

    Ds1 ? Other,
    otherwise :
      Ds2 ! Other |
	self.

kluge_update(Ds1, Ds2, Done) :-

    Done++ < 2 |
	kluge_initialization;

    Done >= 2 |
      Ds1 = Ds2 .

procedure user_shell_context(String, String, [Any]).

% Change shell context

user_shell_context(OldWD, BinMode, Input) :-
	processor # interface(user_data(UserData)),
	change_shell_context(OldWD, BinMode, Input, UserData).

procedure change_shell_context(String, String, [Any], {String, String}).

change_shell_context(OldWD, BinMode, Input, UserData) :-
	computation # shell(filter_user_macros, In'', In),
	file # working_directory(Directory),
	string_to_dlist(Directory, DNew, []),
	string_to_dlist(OldWD, DOld, Sub),
	start_shell(UserData, BinMode, DNew?, DOld?, Sub, Change),
	new_shell_context(Change, Input, In, In'),
	processor # machine(idle_queue(Done, 0)),
	Done?(In') = done(In'').

start_shell(_, _, DOld, DOld^, [Slash | [Char | More]]^, Change) :-
    true : ascii('/', Slash) |
	extend_hierarchy([Char | More], [], Change).
start_shell(_, _, DOld, DOld^, [Slash]^, true^) :-
    true : ascii('/', Slash) | true.
start_shell(_, _, DOld, DOld^, []^, true^).
start_shell({User, UserDir}, BinMode, DNew, _, []^, Change) :-
    otherwise |
	string_to_dlist(UserDir, UD, Sub),
	user_home_directory(User, BinMode, UserDir, DNew, UD, Sub, Change).

user_home_directory(User, BinMode, UserDir, UD^, UD, [Slash | Extension]^,
			 Change
) :-
    true : ascii('/',Slash) |
	user_shell(User, BinMode, UserDir, Extension, Change).
user_home_directory(User, BinMode, UserDir, UD^, UD, []^, Change) :-
	user_shell(User, BinMode, UserDir, [], Change).
user_home_directory(User, BinMode, _, DNew, _, _, Change) :-
    otherwise |
	string_to_dlist(User, UName, Extension),
        user_directory(User, BinMode, DNew, UName, Extension, DHead, DHead,
			Change
	).

user_directory(User, BinMode, DTail, DTail^, DTail^, Rest, DHead, Change) :-
    true : ascii('/', Slash) |
	complete_rest(DTail, Slash, Rest, DHead, Front, Back),
	user_shell(User, BinMode, Front, Back, Change).
user_directory(User, BinMode, UName, UName^, DTail, [C|Rest]^, DHead,
		Change
) :-
    otherwise,
    UName = [C | UName'] |
	user_directory(User, BinMode, UName', UName', DTail, Rest, DHead,
			Change
	).
user_directory(User, BinMode, DNew, UName, Extension, Rest, DHead, Change) :-
    otherwise,
    DNew ? C :
      Rest ! C |
	user_directory.
user_directory(User, BinMode, [], _, _, []^, DNew, Change) :-
	list_to_string(DNew,UNIXPath),
	user_shell(User, BinMode, UNIXPath, [], Change).

complete_rest(DTail, Slash, Rest, DHead, Front, Back) :-
    DTail ? C,
    C =\= Slash :
      Rest ! C |
	complete_rest.
complete_rest(Back, _, []^, DHead, Front, Back^) :-
    otherwise |
	list_to_string(DHead, Front).


% Add user service in current context

user_shell(User, BinMode, UserDir, DTail, Change) :-
    string_to_dlist(UserDir, UserDirC, _) :
      ascii('/', Slash),
      UserScope = [User | Scope] |
	computation # ( self # service_id(Scope) ),
	ends_in_slash(UserDirC, Slash, UserDirC, UserWorkingDirectory),
	add_user_director(Scope, BinMode, UserWorkingDirectory, UserScope,
				Ready
	),
	start(Ready, DTail, DTail'),
	extend_hierarchy(DTail', UserScope, Change).

ends_in_slash(UDCEnd, Slash, UserDirC, UserWorkingDirectory) :-
    true :
      UDCEnd = [Slash] |
	list_to_string(UserDirC, UserWorkingDirectory);

    UDCEnd ? _,
    otherwise |
	ends_in_slash(UDCEnd', Slash, UserDirC, UserWorkingDirectory).

add_user_director(Scope, BinMode, UserWorkingDirectory, UserScope, Ready) :-
    known(Scope) |
	computation # '_domain'(add_service(UserScope,
					    director(UserWorkingDirectory,
						     BinMode
					    ),
					    Ready
					   )
			).

start(true, Close, Close^).


extend_hierarchy([], NewId, change_context(NewId)^).
extend_hierarchy(Extension, OldId, Change) :-
    otherwise :
       ascii('/', Slash) |
	extend_hierarchy(Extension, Slash, NewChars, NewChars, OldId, Change).

extend_hierarchy(Rest, Slash, More, NewChars, OldId, Change) :-

    Rest ? C,
    C =\= Slash :
      More ! C |
	extend_hierarchy;

    Rest ? Slash :
      More = [] |
	extend_hierarchy1(NewChars, OldId, NewId),
	extend_hierarchy(Rest', NewId, Change);

    Rest = [] :
      More = [] |
	extend_hierarchy([Slash], Slash, _, NewChars, OldId, Change).

extend_hierarchy1([], OldId, OldId^).
extend_hierarchy1(NewChars, OldId, [NewName | OldId]^) :-
    otherwise,
    list_to_string(NewChars, NewName) |
	true.


new_shell_context(Change, Input, [Change, input(Input, _) | In]^, In) :-
    known(Change),
    string(Input) |
	true.
new_shell_context(Change, _, [Change | In]^, In) :-
    otherwise |
	true.

% Monitor Shell Computation

monitor(Faults, Events, Commands, Controls, Out, Redelegates,
	DLN, Ss, Delegates, ShellDomain, Root
) :-

    Faults ? Fault(_, Descriptor) |
	shell_event(failed(Descriptor, Fault), DLN, DLN', Ss, Ss'),
	self;

    Events ? Event |
	shell_event(Event, DLN, DLN', Ss, Ss'),
	self;

    Commands ? Command |   
	unknown_command(Command, DLN, Ss, Ss'),
	self;

    Controls ? Control|
	control_monitor(Control, Events, DLN, DLN', Ss, Ss'),
	self;

    Out ? Delegate :
      Delegates ! Delegate |
	self;

    Redelegates ? linked(_, _, Link) :
      write_channel(Link, ShellDomain) |	% try domain intercept
	self;

    Redelegates ? Delegate,
    Delegate =\= linked(_,_,_) |
	redelegated(Delegate, Ss, Ss'),		% refer to root computation
	self;

    Events = [], Commands = [],	% This cant happen in the normal system.
    unknown(Faults) : Controls = _, Out = _, Redelegates = _, DLN = _,
      Ss = [], Delegates = [],
      ShellDomain = _ |
	close_channel(Root);

    Controls =\= [_|_], Commands = [],
    unknown(Events), unknown(Out), unknown(Redelegates) : Faults = _, DLN = _,
      Ss = [],
      Delegates = [],
      ShellDomain = _ |
	processor # machine(idle_queue(Idle, -1)),
	close_system(Idle, Root).

close_system(Idle, Domain) :-
    known(Idle) |
	close_channel(Domain).

redelegated(Delegate, Ss, Ss') :-

    Delegate = request(_, failed(call(_), _), Left, Right) :
      Left = Right,
      Ss' = Ss ;

    Delegate = request(_, failed(Other, _), _, _),
    Other =\= call(_) :
      Ss ! Delegate ;

    Delegate = request(Scope, Request, Left, Right),	% from root service ?
    Request =\= failed(_, _) :
      Ss ! request(Scope, failed(Request, unknown), Left, Right) ;

    Delegate = delegated(Request, CCC) :	% try directors
      Ss' = Ss |
	request_service(CCC, Request);

    otherwise :
      Ss ! request([computation], Delegate, [], []) .


request_service(CCC, Request) :-

    CCC = {_, Left, Right, Channel},
    channel(Channel) :
      write_channel(request([computation], self # Request, Left, Right),
		    Channel
      ) ;

    otherwise : Request = _,			% Channel closed ?
      CCC = {_, Close, Close, _} .


control_monitor(events(Events^), Events, DLN, DLN^, Ss, Ss^).
control_monitor(delegate_unknown, _, _(L, N), control(L, N)^, Ss, Ss^).
control_monitor(diagnose_unknown, _, _(L, N), diagnose(L, N)^, Ss, Ss^).
control_monitor(display_lost, _, D(_, N), D(lost, N)^, Ss, Ss^).
control_monitor(ignore_lost, _, D(_, N), D(no_lost, N)^, Ss, Ss^).
control_monitor(lost(N), _, DLN, DLN^, Ss, Ss^) :-
    arg(3, DLN, N^) |
	true.
control_monitor(Other, _, DLN, DLN^, Ss, Ss') :-
    otherwise |
	shell_diagnostic(prefix("[LOGIX]"), unknown_computation_control(Other),
				Ss, Ss'
	).

shell_event(failed(call(_), _), DLN, DLN^, Ss, Ss^).
shell_event(failed(_, lost), D(no_lost, N), D(no_lost, N')^, Ss, Ss^) :-
    N' := N + 1 | true.
shell_event(aborted, DLN, DLN^, []^, []^).
shell_event(terminated, DLN, DLN^, []^, []^).
shell_event(failed(Goal, unknown), DLN, DLN^, Ss, Ss') :-
	shell_diagnostic(prefix("[LOGIX]"), unknown(Goal), Ss, Ss').
shell_event(comment(Comment), DLN, DLN^, Ss, Ss') :-
    Comment = (_:_) |
	shell_diagnostic([type(unparse)], Comment, Ss, Ss').
shell_event(comment(Comment), DLN, DLN^, Ss, Ss') :-
    Comment =\= (_:_) |
	shell_diagnostic([type(unparse)], (Comment : ""), Ss, Ss').
shell_event(diagnostic(Diagnostic), DLN, DLN^, Ss, Ss') :-
	shell_diagnostic([type(unparse)], Diagnostic, Ss, Ss').
shell_event(Diagnostic, DLN, DLN^, Ss, Ss') :-
    otherwise |
	shell_diagnostic(prefix("[LOGIX]"), Diagnostic, Ss, Ss').

unknown_command(Failed, _, Ss, Ss') :-
    Failed = failed(_,_) |
	shell_diagnostic(prefix("[LOGIX]"), Failed, Ss, Ss').
unknown_command(Command, control(_,_), [Command | Ss]^, Ss) :-
    Command =\= failed(_,_) | true.
unknown_command(Command, _, Ss, Ss') :-
    otherwise |
	shell_diagnostic(prefix("[LOGIX]"), unknown_shell_command(Command),
				Ss, Ss'
	).

shell_diagnostic(Options, Diagnostic, Signals, Signals'') :-
    true :
      Signals ! display(term, Message,
			[known(Message), close(Signals', Signals'') | Options])
	|
	diagnostic_message(Diagnostic, Message).


diagnostic_message(Diagnostic, Message) :-

    Diagnostic = event(Event),
    arity(Event, A),
    N := A + 1,
    make_tuple(N, M), arg(1, M, event) |
	copy_event(1, Event, M, Message);

    Diagnostic = failed(Goal, Reason) :
      Message = Reason(Goal) ;

    otherwise :
      Diagnostic = Message .

copy_event(N, E, M, Message) :-
    arg(N, E, A),
    N' := N + 1,
    arg(N', M, A^) |
	copy_event(N', E, M, Message).
copy_event(_, _, Message, Message^) :-
    otherwise |
	true.
