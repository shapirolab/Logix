/*

Functions delegated to  shell - Bill Silverman 04/92.

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:32 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/system/shell_server/delegates.cp,v $

Copyright (C) 1992, Weizmann Institute of Science - Rehovot, ISRAEL

*/


-language([evaluate, inherit, dfcp]).
-export([start/3]).
-mode(trust).


OPEN => (Port = Type(Goals?)).

DELEGATED =>  Delegates ? delegated(Request, CCC).
REDELEGATED => Redelegates ! delegated(Request, CCC), self.

DELEGATED0(Type, Port, Port', Send) =>
   (OPEN,
    DELEGATED,
    Request = Type(Arg),
    string(Arg) |
	SERVE(Send, Arg);
    OPEN,
    DELEGATED,
    Request = Type(Tuple),
    tuple(Tuple),
    channel(Retry) |
	delegated_tuple(Type, Tuple, CCC, Goals, Goals'?, Retry),
	Port' = Type(Goals'),
	self).

DELEGATED1(Type, Port, Port', Send) =>
   (OPEN,
    DELEGATED,
    Request = Type(Functor, Arg),
    string(Functor) |
	SERVE(Send, Functor(Arg))).

DELEGATED2(Type, Port, Port', Send) =>
   (OPEN,
    DELEGATED,
    Request = Type(Functor, Arg1, Arg2),
    string(Functor) |
	SERVE(Send, Functor(Arg1, Arg2))).
DELEGATED3(Type, Port, Port', Send) =>
   (OPEN,
    DELEGATED,
    Request = Type(Functor, Arg1, Arg2, Arg3),
    string(Functor) |
	SERVE(Send, Functor(Arg1, Arg2, Arg3))).

DELEGATED4(Type, Port, Port', Send) =>
   (OPEN,
    DELEGATED,
    Request = Type(Functor, Arg1, Arg2, Arg3, Arg4),
    string(Functor) |
	SERVE(Send, Functor(Arg1, Arg2, Arg3, Arg4))).

DELEGATED5(Type, Port, Port', Send) =>
   (OPEN,
    DELEGATED,
    Request = Type(Functor, Arg1, Arg2, Arg3, Arg4, Arg5),
    string(Functor) |
	SERVE(Send, Functor(Arg1, Arg2, Arg3, Arg4, Arg5))).

DELEGATED6(Type, Port, Port', Send) =>
   (OPEN,
    DELEGATED,
    Request = Type(Functor, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6),
    string(Functor) |
	SERVE(Send, Functor(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6))).

SERVE(Send, Request) =>
	Goal = Request,
	Send,
	Port' = Type(Goals'),
	self.

REDELEGATE => (Type =\= dictionary,
	       Type =\= display,
	       Type =\= input,
	       Type =\= library,
	       Type =\= shell,
	       Type =\= sio).

WRAPPEDGOAL => Goals ! goal([computation], Goal?, CCC).
PLAINGOAL => Goals ! Goal?, closeCC(CCC).

UNKNOWN1 =>
    Request = Functor(_),
    unknown(Functor),
    channel(Retry).

UNKNOWN2 =>
    Request = Functor(_, _),
    unknown(Functor),
    channel(Retry).

UNKNOWN3 =>
    Request = Functor(_, _, _),
    unknown(Functor),
    channel(Retry).

UNKNOWN4 =>
    Request = Functor(_, _, _, _),
    unknown(Functor),
    channel(Retry).

UNKNOWN5 =>
    Request = Functor(_, _, _, _, _),
    unknown(Functor),
    channel(Retry).

UNKNOWN6 =>
    Request = Functor(_, _, _, _, _, _),
    unknown(Functor),
    channel(Retry).

UNKNOWN_ARG1_2 =>
    Request = _(Functor, _),
    unknown(Functor),
    channel(Retry).

UNKNOWN_ARG1_3 =>
    Request = _(Functor, _, _),
    unknown(Functor),
    channel(Retry).

UNKNOWN_ARG1_4 =>
    Request = _(Functor, _, _, _),
    unknown(Functor),
    channel(Retry).

UNKNOWN_ARG1_5 =>
    Request = _(Functor, _, _, _, _),
    unknown(Functor),
    channel(Retry).

UNKNOWN_ARG1_6 =>
    Request = _(Functor, _, _, _, _, _),
    unknown(Functor),
    channel(Retry).

Intercept ::= [(dictionary ; 
		display ;
		input ;
		library ;
		shell([Integer], Channel) ;
		shell([Integer], Channel, Stream, Stream)
		)
	      ].


start(Delegates, Redelegates, Intercept) :-
	serve_delegated(Delegates, Redelegates, Deferred?, Deferred!,
			Dictionary?, Display?, Input?, Library?, Shell?, Sio?
	),
	initialize(Intercept, Dictionary, Display, Input, Library, Shell, Sio).


initialize(Intercept, Dictionary, Display, Input, Library, Shell, Sio) :-

    Intercept ? dictionary |
	Dictionary = dictionary(Goals),
	computation # dictionary(inherit, Goals?),
	Dictionary' = [],
	self;

    Intercept ? display |
	Display = display(Goals),
	computation # (self # "_private_goals"(Goals?, SId?, In)),
	computation # display(options, Options),
	option_tuple(Options?, Tuple),
	display_server # service_id(SId),
	display_server # initial(In?, Tuple?),
	Display' = [],
	self;

    Intercept ? library |
	Library = library(Goals),
	computation # library(retrieve, Current),
	library_server # serve(Goals?, Current?),
	Library' = [],
	self;

    Intercept ? input |
	Input = input(Goals),
	input_server # serve(Goals?),
	Input' = [],
	self;

    Intercept ? shell(Bytes, TC) |
	computation # Forward?,
	Intercept'' = [shell(Bytes, TC, Forward, done) | Intercept'?],
	self;

    Intercept ? shell(Bytes, TC, Forward, Controls) |
	Shell = shell(Goals),
	parse # characters(LineOut?, Parsed, Errors),
	super # serve(Goals?, Parsed?, Forward, Controls),
	computation # display(stream, Errors?,
			      [type(ground), prefix(syntax_error)]
		      ),
	Sio = sio(SGoals),
	computation # (self # "_private_goals"(SGoals?, SioId?, SIn)),
	sio_server # service_id(SioId),
	sio_server # serve(Bytes, SIn?, LineOut, TC),
	Shell' = [],
	Sio' = [],
	self;

     Intercept ? Other |
	computation # event("invalid shell intercept"(Other)),
	self;

     Intercept = [] |
	unify_without_failure(Dictionary, []),
	unify_without_failure(Display, []),
	unify_without_failure(Input, []),
	unify_without_failure(Library, []),
	unify_without_failure(Shell, []),
	unify_without_failure(Sio, []);

     Intercept =\= [], Intercept =\= [_|_] |
	Intercept'' = [Intercept],
	self.
		
option_tuple(OTs, Tuple) + (Os, Ts) :-

    initially |
	Os = Ts? ;

    OTs ? _Type(Value) |
	Ts ! Value,
	self;

    OTs = [] |
	Ts = [],
	list_to_tuple(Os, Tuple).


serve_delegated(Delegates, Redelegates, Deferred, Retry,
		Dictionary, Display, Input, Library, Shell, Sio
) :-

    + dictionary;
    + display;
    + input;
    + library;
    + shell;
    + sio;

    DELEGATED,
    Request = Type(_),
    REDELEGATE |
	REDELEGATED;

    DELEGATED,
    Request = Type(_,_),
    REDELEGATE |
	REDELEGATED;

    DELEGATED,
    Request = Type(_,_,_),
    REDELEGATE |
	REDELEGATED;

    DELEGATED,
    Request = Type(_,_,_,_),
    REDELEGATE |
	REDELEGATED;

    DELEGATED,
    Request = Type(_,_,_,_,_),
    REDELEGATE |
	REDELEGATED;

    DELEGATED,
    Request = Type(_,_,_,_,_,_),
    REDELEGATE |
	REDELEGATED;

    Delegates ? Other,
    otherwise |
	Redelegates ! Other,	% includes NOT delegated/2
	self;

    DELEGATED,
    Request = Type(Functor),
    unknown(Functor),
    Type =\= display,
    channel(Retry) |    
	defer,
	self;

    DELEGATED,
    UNKNOWN1 |
	defer,
	self;

    DELEGATED,
    UNKNOWN2 |
	defer,
	self;

    DELEGATED,
    UNKNOWN3 |
	defer,
	self;

    DELEGATED,
    UNKNOWN4 |
	defer,
	self;

    DELEGATED,
    UNKNOWN5 |
	defer,
	self;

    DELEGATED,
    UNKNOWN6 |
	defer,
	self;

    DELEGATED,
    UNKNOWN_ARG1_2 |
	defer,
	self;

    DELEGATED,
    UNKNOWN_ARG1_3 |
	defer,
	self;

    DELEGATED,
    UNKNOWN_ARG1_4 |
	defer,
	self;

    DELEGATED,
    UNKNOWN_ARG1_5 |
	defer,
	self;

    DELEGATED,
    UNKNOWN_ARG1_6 |
	defer,
	self;

    Deferred ? Defer |
	Delegates' = [Defer | Delegates],
	self;

    Delegates = [], unknown(Deferred) | Retry = _,
	Redelegates = [],
	close_goals(Dictionary),
	close_goals(Display),
	close_goals(Input),
	close_goals(Library),
	close_goals(Shell),
	close_goals(Sio).

close_goals(Descriptor) :-

    Descriptor = [] | true;

    Descriptor = _(Goals?) |
	Goals = [] .

defer(Functor, Request, CCC, Retry) :-

    known(Functor) |
	write_channel(delegated(Request, CCC), Retry);

    CCC = {[abort | _], Left, Right, _} |
	Functor = _, Request = _, Retry = _,
	unify_without_failure(Left, Right);

    CCC = {CO, CL, CR, CC},
    CO ? suspend, known(CO') |
	CCC' = {CO'?, CL, CR, CC},
	self;

    CCC = {CO, CL, CR, CC},
    CO ? resume |
	CCC' = {CO'?, CL, CR, CC},
	self;

    CCC = {_CO, CL, CR, CC},
    known(CL),
    channel(CC) |
	Functor = _, Retry = _,
	write_channel(request([computation], Request, CL, CR), CC).

DEFERTUPLE => 
	Goals = Goals1,
	Request' = Type(Request),
	defer.

delegated_tuple(Type, Request, CCC, Goals, Goals1, Retry) :-

    UNKNOWN1 |
	DEFERTUPLE;

    UNKNOWN2 |
	DEFERTUPLE;

    UNKNOWN3 |
	DEFERTUPLE;

    UNKNOWN4 |
	DEFERTUPLE;

    UNKNOWN5 |
	DEFERTUPLE;

    UNKNOWN6 |
	DEFERTUPLE;

    otherwise,
    Type =\= dictionary, Type =\= input, Type =\= library, Type =\= shell |
	Retry = _,
	Goals = [goal([computation], Request?, CCC) | Goals1];

    otherwise |
	Type = _, Retry = _,
	Goals = [Request? | Goals1],
	closeCC(CCC).

    
dictionary(Delegates, Redelegates, Retry, Dictionary) :-

    DELEGATED0(dictionary, Dictionary, Dictionary', PLAINGOAL);
    DELEGATED1(dictionary, Dictionary, Dictionary', PLAINGOAL);
    DELEGATED2(dictionary, Dictionary, Dictionary', PLAINGOAL);
    DELEGATED3(dictionary, Dictionary, Dictionary', PLAINGOAL);
    DELEGATED4(dictionary, Dictionary, Dictionary', PLAINGOAL);
    DELEGATED5(dictionary, Dictionary, Dictionary', PLAINGOAL);
    DELEGATED6(dictionary, Dictionary, Dictionary', PLAINGOAL).
	
display(Delegates, Redelegates, Display) :-

    DELEGATED,
    Display = display(Goals?),
    Request = display(_) |
	Goals ! goal([computation], Request?, CCC),
	Display' = display(Goals'),
	self;

    DELEGATED1(display, Display, Display', WRAPPEDGOAL);
    DELEGATED2(display, Display, Display', WRAPPEDGOAL);
    DELEGATED3(display, Display, Display', WRAPPEDGOAL).

input(Delegates, Redelegates, Retry, Input) :-

    DELEGATED0(input, Input, Input', PLAINGOAL);
    DELEGATED1(input, Input, Input', PLAINGOAL).

library(Delegates, Redelegates, Retry, Library) :-

    DELEGATED0(library, Library, Library', PLAINGOAL);
    DELEGATED1(library, Library, Library', PLAINGOAL);
    DELEGATED2(library, Library, Library', PLAINGOAL);
    DELEGATED3(library, Library, Library', PLAINGOAL);
    DELEGATED4(library, Library, Library', PLAINGOAL).

shell(Delegates, Redelegates, Retry, Shell) :-

    DELEGATED0(shell, Shell, Shell', PLAINGOAL);
    DELEGATED1(shell, Shell, Shell', PLAINGOAL);
    DELEGATED2(shell, Shell, Shell', PLAINGOAL);
    DELEGATED3(shell, Shell, Shell', PLAINGOAL);
    DELEGATED4(shell, Shell, Shell', PLAINGOAL);
    DELEGATED5(shell, Shell, Shell', PLAINGOAL);
    DELEGATED6(shell, Shell, Shell', PLAINGOAL).

sio(Delegates, Redelegates, Retry, Sio) :-

    DELEGATED0(sio, Sio, Sio', WRAPPEDGOAL);
    DELEGATED1(sio, Sio, Sio', WRAPPEDGOAL);
    DELEGATED2(sio, Sio, Sio', WRAPPEDGOAL).
