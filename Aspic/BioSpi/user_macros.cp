/*

User Shell default macros
Ehud Shapiro, 01-09-86

Last update by		$Author: bill $
		       	$Date: 2002/08/07 07:10:30 $
Currently locked by 	$Locker:  $
			$Revision: 1.5 $
			$Source: /home/qiana/Repository/Aspic/BioSpi/user_macros.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([expand/2]).
-mode(trust).
-language([evaluate,compound,colon]).
-include(spi_constants).

/*	expand/2

	Expand macro commands.

	expand(Command, Commands\Commands1)

	->	Command  is a console command.
	<=	Commands\Commands1  is a difference list of commands to the
		system-macro processor and the user shell.

	Commands which are not expanded here are forwarded for system-macro
	expansion and user shell processing.
*/

procedure expand(Any, [Any]\[Any]).

expand(Command, Cs) :-

    Command = hi :
      Cs = Commands\Commands |
	computation # display(term, hello);

% add your macros here....

% BioSpi Calculus macros

    Command = psc :
      Cs = [to_context(spi_monitor # spifunctions([])) | Commands]\Commands;

    Command = psc(FS) :
      Cs = [to_context(spi_monitor # spifunctions(FS)) | Commands]\Commands;

    Command = options(New) :
      Command' = options(New, _) |
	self;

    Command = options(New, Old) :
      Cs = [to_context(spi_monitor # options(New, Old)) | Commands]\Commands;

    Command = pc(C) :
      Cs = [to_context(spi_utils # make_channel(C)) | Commands]\Commands;

    Command = pc(C, S, B) :
      Cs = [to_context(spi_utils # make_channel(C, S, B)) | Commands]\Commands;

    Command = pc(C, S, B, W) :
      Cs = [to_context(spi_utils # make_channel(C,S,B,W)) | Commands]\Commands;

    Command = pdb(Service) :
      Cs = [to_context(spi_monitor # options(O, O)),
	    spidbg # interpret(Name?, Service, O)
	   | Commands]\Commands |
	parse_rpc(Service, Name);

    Command = pdb(Service, Options) :
      Cs = [spidbg # interpret(Name?, Service, Options) | Commands]\Commands |
	parse_rpc(Service, Name);

    Command = ph :
      CL = [	" a / a(No)          - abort computation No",
		" at / at(Service)   - attributes(Service)",
		" i(File)            - input file",
		" options(New, Old)  - default BioSpi Options",
		" pc(C)              - make BioSpi channel C",
		" ph                 - get this list",
		" pr(C,M)            - receive M from BioSpi channel C",
		" prgcs              - reset BioSpi global channels",
		" ps(M,C)            - send M on BioSpi channel C",
		" spc(C)             - Show BioSpi channel",
		" spgcs              - Show BioSpi global channels",
		" record(GS, F, L)   - run Goals, record to File until Limit.",
		" record(GS,F,L,S,O) - run Goals, record to File until Limit,",
		"                      scaled by Scale, with format Option.",
		" run(GS)            - run Goals.",
		" run(GS, L)         - run Goals until Limit.",
		" trace(GS, F, L)    - run Goals, trace to File until Limit.",
		" trace(GS,F,L,S,O)  - run Goals, trace to File until Limit,",
		"                      scaled by Scale, with format Option.",
		" weighter(W)        - set the default weighter",
		" {String}           - invoke UNIX shell sh with String",
		"",
		"        options for sp* :",
		" none/active        - type of messages displayed",
		" sender/no_sender   - show name of message sender",
		"       format options for record and trace :",
		"           short/process/creator/full",
		"       format options for channel display :",
		"             short/base/creator/full"
		
	 ],
      Cs = [to_context(computation # display(stream,CL)) | Commands]\Commands ;

    Command = pr(C, M) :
      Cs = [to_context(spi_utils # receive(C, M)) | Commands]\Commands;

    Command = pr(C, M, N) :
      Cs = [to_context(spi_utils # receive(C, M, N)) | Commands]\Commands;

    Command = prgcs :
      Command' = prgcs(_) |
	expand;

    Command = prgcs(Channels) :
      Cs = [to_context(spi_monitor # reset(Channels)) | Commands]\Commands;

    Command = ps(M, C) :
      Cs = [to_context(spi_utils # send(M, C)) | Commands]\Commands;

    Command = ps(M, C, N) :
      Cs = [to_context(spi_utils # send(M, C, N)) | Commands]\Commands;

    Command = spc(C) :
      Cs = [to_context([spi_monitor # options(O, O),
			spi_utils # show_channel(C, O?, Channel),
			computation # display(term, Channel, known(Channel))]) 
	   | Commands]\Commands;

    Command = spc(C, Options) :
      Cs = [to_context([spi_utils # show_channel(C, Options, Channel)]),
			computation # display(term, Channel, known(Channel))
	   | Commands]\Commands;

    Command = spg :
      Command' = spg(_) |
	expand;

    Command = spg(No) :
      Cs = [state(No, Goal, _, _),
	    to_context([spi_monitor # options(O, O),
			spi_utils # show_goal(Goal, O?, Term),
			computation # display(term, Term, known(Term))]) 
	   | Commands]\Commands;

    Command = spg(No, Options) :
      Cs = [state(No, Goal, _, _),
	    to_context([spi_utils # show_goal(Goal, Options, Term),
			computation # display(term, Term, known(Term))]) 
	   | Commands]\Commands;

    Command = spgcs :
      Cs = [to_context([spi_monitor # [options(O, O),get_global_channels(Gs)], 
			spi_utils # show_value(Gs, O?, Gs'),
			computation # display(stream, Gs', [])]) 
	   | Commands]\Commands;

    Command = spgcs(Options) :
      Cs = [to_context([computation # display(stream, Gs', []),
			spi_monitor # get_global_channels(Gs),
			spi_utils # show_value(Gs, Options, Gs')]) 
	   | Commands]\Commands;

    Command = record(Goals, File, Limit) |
	ambient_run(Goals, Run, spi_record#run(Run, File, Limit), Cs);

    Command = record(Goals, File, Limit, Scale) |
	ambient_run(Goals, Run, spi_record#run(Run, File, Limit, Scale), Cs);

    Command = record(Goals, File, Limit, Scale, Format) |
	ambient_run(Goals, Run,
		    spi_record#run(Run, File, Limit, Scale, Format), Cs);

    Command = run(Goals) |
	ambient_run(Goals, Run, Run, Cs);

    Command = run(Goals, Limit) |
	ambient_run(Goals, Run, spi_record#run(Run, Limit), Cs);

    Command = trace(Goals, File, Limit) |
	ambient_run(Goals, Run, spi_trace#run(Run, File, Limit), Cs);

    Command = trace(Goals, File, Limit, Scale) |
	ambient_run(Goals, Run, spi_trace#run(Run, File, Limit, Scale), Cs);

    Command = trace(Goals, File, Limit, Scale, Format) |
	ambient_run(Goals, Run,
		    spi_trace#run(Run, File, Limit, Scale, Format), Cs);

    Command = weighter(Weighter) :
      Cs = [to_context([spi_utils # weighter(Weighter)])
	   | Commands]\Commands;

    Command = '^' :
      Cs = [display_stream(Bindings',[]),
            to_context([computation # dictionary(bindings, Bindings, 0),
			spi_monitor # options(O, O),
		spi_utils # show_value(Bindings?, O?, Bindings')])
	   | Commands]\Commands;

    Command = {Hat, X}, Hat =?= '^',
    X =?= '^' :
      Cs = [to_context([computation # dictionary(bindings, Bindings, 1),
			spi_monitor # options(O, O),
			spi_utils # show_value(Bindings?, O?, Bindings')]),
	    display_stream(Bindings'?, [])
	   | Commands]\Commands;

    Command = {Hat, X}, Hat =?= '^',
    X =\= '^' :
      Cs = [to_context([spi_monitor # options(O, O) | Stuff])
	   | Commands]\Commands |
	display_variables(X, O?, Stuff);

    Command = forward(Any) :
      Cs = [Any | Commands]\Commands;

% For testing only!

    Command = spi2cmp(N) :
      Command' = spi2cmp(N, []) |
	expand;

    Command = spi2cmp(N, Options) :
      Cs = [to_context(computation # display(stream,Results, [type(unparse)])),
	    spi_macros # spi2cmp(N, Options, Results) |Commands]\Commands;

    Command = spi2fcp(N) :
      Command' = spi2fcp(N, []) |
	expand;

    Command = spi2fcp(N, Options) :
      Cs = [to_context(computation # display(stream,Results, [type(unparse)])),
	    spi_macros # spi2fcp(N, Options, Results) |Commands]\Commands;

    Command = tpf(N) :
      Cs = [to_context(computation # display(stream,Results, [type(unparse)])),
	    spi_macros # transform(N, Results) |Commands]\Commands;

% To retain system-macro and normal shell capabilities, forward generated
% commands to the shell via the  Commands  difference list.

    Command = goal :
      Command' = goal(_) |
	expand;

    Command = goal(No) :
      Cs = [state(No, Goal, _, _),
	    to_context(computation # display(term, Goal,
					     [prefix(IdG), known(IdG)]))
	   | Commands]\Commands |
	utils#append_strings(['<',No,'> goal = '], IdG);

    Command = O/V :
      Cs = [to_context(computation # display(option, Option, NewValue, _))
	   | Commands]\Commands |
	screen_option(O, V, Option, NewValue);

    Command = Service - Goal :
      Cs = [service(Service), to_context(Service # Goal)
	   | Commands]\Commands ;

    Command = - Goal :
      Command' = _Service - Goal |
	expand;

    Command = (Id | X),
    Id >= 0 |
	computation # dictionary(find, Id, Variable, Reply),
	assign(Reply, Id, X, Variable, Cs);

    Command = a :
      Cs = [abort|Commands]\Commands ;
    Command = a(Computation) :
      Cs = [abort(Computation)|Commands]\Commands ;

    Command = at |
	service_attributes(_Service, Cs);
    Command = at(Service) |
	service_attributes(Service, Cs);

    string_to_dlist(Command,[X, CHAR_t, CHAR_a], []),
    CHAR_b =< X, X =< CHAR_d :
      Cs = [state(No,_,_,_),
	    to_context(utils # append_strings(["<",No,"> "], IdG))
           | Commands]\Commands1 |
	spi_monitor#status(Status),
	spi_channels(X, Status, Out),
	stream_out(Out?, true, IdG, Commands, Commands1);

    string_to_dlist(Command,[X, CHAR_t, CHAR_r], []),
    CHAR_a =< X, X =< CHAR_d :
      Command' = Command(all) |
	self;
    Command = Xtr(N),
    string_to_dlist(Xtr,[X, CHAR_t, CHAR_r], []),
    CHAR_a =< X, X =< CHAR_d :
      Cs = [state(No,Goal,_,_),
	    to_context(utils # append_strings(["<",No,"> "], IdG))
           | Commands]\Commands1 |
	xtr_tree(Goal, X, N, Out),
	stream_out(Out?, true, IdG, Commands, Commands1);

    Command = c :
      Cs = [compile |Commands]\Commands ;
    Command = c(Computation) :
      Cs = [compile(Computation)|Commands]\Commands ;
    Command = c(Computation,Options) :
      Cs = [compile(Computation,Options)|Commands]\Commands ;

    Command = d(I) :
      Cs = [debug(I)|Commands]\Commands ;

    Command = h :
      CL = [	" a / a(No)          - abort computation No",
		" at / at(Service)   - attributes(Service)",
		" c / c(Module)      - compile(Module)",
		" d(It)              - debug(It) (Goal or RPC)",
		" goal / goal(No)    - goal of computation No",
		" h                  - get this list",
		" i(File)            - input file",
		" l / l(Module)      - lint(Module)",
		" less(Service)      - activate:  less Service.cp",
		" more(Service)      - activate:  more Service.cp",
		" quit               - quit logix system",
		" r / r(No)          - resolvent of computation No",
		" r(Ids) / r(No, Ids)- extract Ids of resolvent of computation No",
		" re / re(No)        - resume computation No",
		" s / s(No)          - suspend computation No",
		" vi / vi(Module)    - edit Module",
		" O/V                - set screen option O to V",
		" N|X                - assign numbered variable to X",
		" Out!Term           - Send Term on stream or channel",
		" Out!               - close stream or channel",
		" Service - Goal     - call Service#Goal",
		" - Goal             - call Current#Goal",
		" {String}           - invoke UNIX shell sh with String"
	 ],
      Cs = [to_context(computation # display(stream,CL)) | Commands]\Commands ;

    Command = i(Path) :
      Cs = [input(Path)|Commands]\Commands ;

    Command = l :
      Cs = [lint |Commands]\Commands ;
    Command = l(Service) :
      Cs = [lint(Service)|Commands]\Commands ;
    Command = l(Service,Options) :
      Cs = [lint(Service,Options)|Commands]\Commands ;

    Command = less(Path) :
      Cs = Commands\Commands |
	computation_utils # call_id_goal(self # Path, Dir, Service, Ok),
	display_source(Ok, less, Dir, Service);
    Command = more(Path) :
      Cs = Commands\Commands |
	computation_utils # call_id_goal(self # Path, Dir, Service, Ok),
	display_source(Ok, more, Dir, Service);

    Command = quit :
      Cs = Commands\Commands',
      Commands ! to_context(processor # device(quit, _Ok)) ;

    Command = r :
      Command' = r(_) |
	expand;
    Command = r(No) :
      Cs = [state(No,Goal,_,_) | Commands]\Commands1 |
	ambient_resolvent(No, Goal, Commands, Commands1);

    Command = re :
      Command' = re(_) |
	expand;
    Command = re(No) :
      Cs = [state(No,Goal,_,_)|Commands]\Commands1 |
	ambient_signal(resume, No, Goal, Commands, Commands1) ;

    Command = s :
      Command' = s(_) |
	expand ;
    Command = s(No) :
      Cs = [state(No,Goal,_,_)|Commands]\Commands1 |
	ambient_signal(suspend, No, Goal, Commands, Commands1) ;

    Command = t :
      Cs = [trace|Commands]\Commands ;

    Command = vi |
	edit(vi, _Module, Cs);
    Command = vi(Module) |
	edit(vi, Module, Cs);

    Command = (Out ! V) :
      Cs = Commands\Commands |
	write(Out, V);
    Command = (Out!) :
      Cs = Commands\Commands |
	close(Out);

    Command = {String},
    string(String) :
      Cs = Commands\Commands |
	execute(true, String);

    Command = macros :
      Cs = [close(user_macros),
	    to_context([service_id(SID), computation # shell(change_context,SID)])
	   | Commands]\Commands ;

    otherwise :
      Cs = [Command | Commands]\Commands .



ambient_resolvent(No, Goal, Commands, Commands1) :-

    Goal =?= ambient_server#run(_Goals, Root),
    channel(Root) :
      write_channel(resolvent(R), Root),
      Commands = [to_context([spi_monitor # options(O, O),
			      spi_utils # show_resolvent(R?, O, Vs),
			      utils # append_strings(["<",No,">"], IdG),
			      computation # display(stream, Vs?,
						    [prefix(IdG), known(IdG)])
			     ])
		 | Commands1];

    Goal =?= ambient_server#run(_Goals, Root, _Debug),
    channel(Root) :
      write_channel(resolvent(R), Root),
      Commands = [to_context([spi_monitor # options(O, O),
			      spi_utils # show_resolvent(R?, O, Vs),
			      utils # append_strings(["<",No,">"], IdG),
			      computation # display(stream, Vs?,
						    [prefix(IdG), known(IdG)])
			     ])
		 | Commands1];

    otherwise :
      Goal = _,
      Commands = [resolvent(No) | Commands1].


ambient_run(Goals, Run, Action, Cs) :-

    Goals =?= (Service # _Call) :
      Run = Goals |
      Cs = [to_context(spi_monitor#reset),
	    ambient_server#run(Action, _ControlChannel),
	    service(Service?) | Commands]\Commands;

    Goals =?= (# Call) :
      Run = (Service? # Call),
      Cs = [service(Service), to_context(spi_monitor#reset),
	    ambient_server#run(Action, _ControlChannel),
	    service(Service?) | Commands]\Commands;

    otherwise :
       Run = repeat#run(Goals),
       Cs = [to_context(spi_monitor#reset),
	     ambient_server#run(Action, _ControlChannel) | Commands]\Commands.

ambient_signal(Signal, No, Goal, Commands, Commands1) :-

    Goal =?= ambient_server#run(_Goals, Root),
    channel(Root) :
      write_channel(Signal(Reply), Root),
      Commands = [to_context([utils # append_strings(["<",No,">"], IdG),
		  computation#display(term, Reply,
				      [prefix(IdG),known(IdG),known(Reply)])])
		 | Commands1];

    Goal =?= ambient_server#run(_Goals, Root, _Debug),
    channel(Root) :
      write_channel(Signal(Reply), Root),
      Commands = [to_context([utils # append_strings(["<",No,">"], IdG),
		  computation#display(term, Reply,
				      [prefix(IdG),known(IdG),known(Reply)])])
		 | Commands1];

    Goal =\= ambient_server#run(_, _), Goal =\= ambient_server#run(_,_,_) :
      Commands = [Signal(No) | Commands1];

    otherwise :
      Goal = _,
      Commands = [Signal(No) | Commands1].


assign(true, _, X, Variable, ([(Variable = X) | Commands]\Commands)^).
assign(	_, Id, _, _,
	([to_context(computation # display(term, unknown(Id)))
	 | Commands]\Commands)^
) :-
    otherwise |
	true.


edit(Editor, Module,
	([service(Module),
	  to_context(file # pwd(PWD)),
	  close(Module, Ok)
	 | Commands]\Commands
	)^
) :-
	utils # append_strings([Editor, ' ', PWD, Module, '.cp'], Command),
	execute(Ok, Command).


execute(Ok, String) :-

    Ok = true,
    known(String) |
	processor # interface(unix_call(String));

    Ok =\= true : String = _ .

screen_option(t, n, type^, namevars^).
screen_option(t, p, type^, parsed^).
screen_option(t, f, type^, freeze^).
screen_option(s, h, special^, hex^).
screen_option(s, m, special^, melted^).
screen_option(s, n, special^, none^).
screen_option(d, I, depth^, I^).
screen_option(l, I, length^, I^).
screen_option(w, I, width^, I^).
screen_option(i, I, indent^, I^).
screen_option(r, I, iterations^, I^).
screen_option(A, V, A^, V^) :-
	otherwise | true.


service_attributes(New, ([service(New, Old) | Commands]\Commands1)^) :-
	computation_utils # call_list([New # attributes(A)], Reply),
	display_service_attributes(Reply, A, New, Old, Commands, Commands1).

display_service_attributes(true, A, New, _,
		[display_stream(A, prefix(New)) | Commands]^, Commands
).
display_service_attributes(_, _, _, Old,
		[service(Old) | Commands]^, Commands
) :-
    otherwise |
	true.

display_source(true, Way, ID, Service) :-
	ID # '_unique_id'(UID),
	utils # append_strings([Way, ' ', UID, Service, '.cp'], String),
	execute(true, String).
display_source(_, _, _, _) :-
    otherwise |
	true.

write(Out, V) :-

    var(Out) :
      Out = [V | _] ;

    Out ? _ |
	write;

    channel(Out) :
      write_channel(V, Out) .

close(Out) :-

    var(Out) :
      Out = [] ;

    Out ? _ |
	close;

    true :
      close_channel(Out) .

/********************** Added for spifcp development *************************/

parse_rpc(RPC, Name) :-

    RPC =?= Module # _ :
      Name = Module;

    otherwise :
      RPC = _,
      Name = "?".

display_variables(X, Options, Xs) :-

    X =  `VarName,
    string(VarName) :
      Xs ! computation # dictionary(find, VarName, Value, Reply) |
        display_variable(Reply, VarName, Value, Options, Xs');

    otherwise :
      Xs ! computation # dictionary(find, X, Value, Reply) |
        display_variable(Reply, X, Value, Options, Xs').

display_variable(Reply, Id, Value, Options, Xs) :-

    Reply = true,
    string(Id), known(Value) :
      Xs = [spi_utils # show_value(Value, Options, Value'),
	    computation # display(term, Id = Value', known(Value'))];

    integer(Id) :
      Xs ! processor # link(execute(concatenate,{['_X',Id], Id', 0, 10000})) | 
	self;

    Reply = true,
    unknown(Value), string(Id) : Options = _,
      Xs = [computation # display(term, uninstantiated_variable(Id))] ;
        
    Reply = false,
    unknown(Value) , string(Id) : Options = _,
      Xs = [computation # display(term, undeclared_variable(Id))] ;

    otherwise :
      Reply = _, Value = _, Options = _,
      Xs = [computation # display(term, invalid_variable_name(Id))].


copy_list(In, Out, Next) :-

    In ? Item :
      Out ! Item |
	self;

    In =?= [] :
      Out = Next.


stream_out(Terms, Done, IdG, Commands, Commands1) + (Indent = "") :-

    Terms ? "+",
    string_to_dlist(Indent, LI, [CHAR_SPACE]),
    list_to_string(LI, Indent') |
	self;

    Terms ? "-",
    string_to_dlist(Indent, LI, []),
    LI ? CHAR_SPACE,
    list_to_string(LI', Indent') |
	self;

    Terms ? "-",
    string_to_dlist(Indent, LI, []),
    LI ? CHAR_SPACE,
    LI' =?= [] :
      Indent' = "" |
	self;

    Terms ? Term, Term =\= "+", Term =\= "-",
    string_to_dlist(IdG, LIdG, LI),
    string_to_dlist(Indent, LI, []),
    list_to_string(LIdG, P) :
      Commands ! to_context(computation#display(term, Term,
						[prefix(P), type(ground),
						known(Done),
						close(Done, Done')])) |
	self;

    Terms = [] :
      Done = _,
      IdG = _,
      Indent = _,
      Commands = Commands1.

/*
** Kind == ascii b | c | d
** Status == [..., anchors([BasedAnchor, InstantaneousAnchor)]), ...]
** Out = display_stream
*/
spi_channels(Kind, Status, Out) :-

    Status ? anchors([BasedAnchor, InstantaneousAnchor]) :
      Status' = _ |
	extract_anchored_channels(BasedAnchor, Channels, Instantaneous?),
	extract_anchored_channels(InstantaneousAnchor, Instantaneous, []),
	format_channels(Kind, Channels, Out, []);

    Status ? _Other,
    otherwise |
	self;

    Status =?= [] :
      Kind = _,
      Out = [no_anchors].

  extract_anchored_channels(Channel, Channels, Channels1) :-

    vector(Channel),
    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    Type =\= SPI_CHANNEL_ANCHOR :
      Channels ! Channel' |
	self;

    vector(Channel),
    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    Type =?= SPI_CHANNEL_ANCHOR :
      Channels = Channels1.
    

/*
** Goal =?= ambient_server#run(_Goals, Root)
** Kind == ascii a | b | c | d
** integer(Index) (negative implies only one ambient, -Index)
** Out = display_stream
*/
xtr_tree(Goal, Kind, Index, Out) :-

    Goal =?= ambient_server#run(_Goals, Root),
    channel(Root) |
	format_xtr(Kind, Index, [Root], Out, []);

    Goal =?= ambient_server#run(_Goals, Root, _Debug),
    channel(Root) |
	format_xtr(Kind, Index, [Root], Out, []);

    otherwise :
      Index = _,
      Kind = _,
      Out = ["No ambient tree" - Goal].

  format_xtr(Kind, Index, Ambients, Out, Out1) :-

    Ambients ? Ambient,
    vector(Ambient) :
      write_channel(state(State), Ambient) |
	extract_ambient_state,
	format_xtr_parts(Kind, Index, Id, Channels, Children, Out, Out'),
	self;

    Ambients ? _ClosedAmbient,
    otherwise |
	self;

    Ambients = [] :
      Index = _,
      Kind = _,
      Out = Out1.

  extract_ambient_state(State, Id, Channels, Children) :-

    State ? id(Id^) |
	self;

    State ? children(Children^) |
	self;

    State ? private(PrivateChannels) |
	copy_list(PrivateChannels, Channels, Channels'),
	self;

    State ? global(GlobalChannels) |
	copy_list(GlobalChannels, Channels, Channels'),
	self;

    State ? shared(SharedChannels) |
	copy_list(SharedChannels, Channels, Channels'),
	self;

    State ? _Other,
    otherwise |
	self;

    State =?= [] :
      Children = _,
      Id = _,
      Channels = [] .


format_xtr_parts(Kind, Index, Id, Channels, Children, Out, Out1) :-

    Index =?= all,
    Id =?= system |
	format_xtr_parts1;

    Index =?= all,
    Id =\= system :
      Out ! "+" |
	format_xtr_parts1 + (Out1 = ["-" | Out1]);

    Id =?= _Name(Index) :
      Index' = all |
	format_xtr_parts1;

    Id =?= _Name(I),
    I =:= -Index :
      Index' = none |
	format_xtr_parts1;

    otherwise :
      Channels = _,
      Id = _ |
	format_xtr(Kind, Index, Children, Out, Out1).

  format_xtr_parts1(Kind, Index, Id, Channels, Children, Out, Out1) :-
    true :
      Out = [Id, "+", "+" | Out'] |
	format_channels(Kind, Channels, Out', ["-", "-" | Out'']),
	format_xtr(Kind, Index, Children, Out'', Out1).

format_channels(Kind, Channels, Out, Out1) :-

    Kind =?= CHAR_a :
      Channels = _,
      Out = Out1;

    Kind =\= CHAR_a, Kind =\= CHAR_d,
    Channels ? Channel,
    vector(Channel),
    read_vector(SPI_SEND_WEIGHT, Channel, SendWeight),
    read_vector(SPI_RECEIVE_WEIGHT, Channel, ReceiveWeight),
    SendWeight + ReceiveWeight =:= 0 |
	self;

    Channels ? Channel,
    vector(Channel),
    otherwise |
	format_channel,
	format_channel_out(FormattedChannel, Out, Out'),
	self;

    Channels =?= [] :
      Kind = _,
      Out = Out1.

  format_channel(Kind, Channel, FormattedChannel) :-

    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Type =?= SPI_BIMOLECULAR,
    read_vector(SPI_SEND_WEIGHT, Channel, SendWeight),
    read_vector(SPI_RECEIVE_WEIGHT, Channel, ReceiveWeight),
    /* Eventually use c-extencion (spifcp.c) to compute weight */
    Weight := SendWeight + ReceiveWeight,
    Weight =:= 0,
    Kind =?= CHAR_d :
      FormattedChannel = Name - Refs;

    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Type =?= SPI_BIMOLECULAR,
    read_vector(SPI_SEND_WEIGHT, Channel, SendWeight),
    read_vector(SPI_RECEIVE_WEIGHT, Channel, ReceiveWeight),
    Weight := SendWeight + ReceiveWeight,
    Weight > 0 |
	format_channel_b;

    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Type =?= SPI_HOMODIMERIZED,
    read_vector(SPI_DIMER_WEIGHT, Channel, DimerWeight),
    DimerWeight =:= 0,
    Kind =?= CHAR_d :
      FormattedChannel = Name - Refs;

    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Type =?= SPI_HOMODIMERIZED,
    read_vector(SPI_DIMER_WEIGHT, Channel, DimerWeight),
    DimerWeight > 0 |
	format_channel_h;

    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Type =?= SPI_INSTANTANEOUS,
    read_vector(SPI_SEND_ANCHOR, Channel, SendAnchor),  
    read_vector(SPI_RECEIVE_ANCHOR, Channel, ReceiveAnchor) |
	count_requests(SendAnchor, SendRequests),
	count_requests(ReceiveAnchor, ReceiveRequests),
	format_channel_i;

    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Type =?= SPI_UNKNOWN,
    Kind =?= CHAR_d :
      FormattedChannel = (Name ? Refs);

    otherwise,
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    Kind =?= CHAR_d,
    read_vector(SPI_CHANNEL_REFS, Channel, Refs) :
      FormattedChannel = Name(type = Type) - Refs;

    otherwise,
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    Kind =\= CHAR_d :
      FormattedChannel = Name(type = Type).

  format_channel_b(Channel, Kind, SendWeight, ReceiveWeight, Name, Refs,
				FormattedChannel) :-

    SendWeight * ReceiveWeight =:= 0,
    Kind =?= CHAR_b :
      Channel = _,
      Name = _,
      Refs = _,
      FormattedChannel = "";

    SendWeight =\= 0, ReceiveWeight =\= 0,
    Kind =?= CHAR_b,
    read_vector(SPI_CHANNEL_RATE, Channel, Rate),
    Weight := Rate * SendWeight * ReceiveWeight :
      Refs = _,
      FormattedChannel = Name(Weight);

    Kind =?= CHAR_c,
    SendWeight =?= 0 :
      Channel = _,
      Refs = _,
      QM = "?",
      FormattedChannel = Name(QM(ReceiveWeight));

    Kind =?= CHAR_c,
    ReceiveWeight =?= 0 :
      Channel = _,
      Refs = _,
      FormattedChannel = Name(SendWeight!);

    Kind =?= CHAR_d,
    read_vector(SPI_SEND_ANCHOR, Channel, SendAnchor),  
    read_vector(SPI_RECEIVE_ANCHOR, Channel, ReceiveAnchor) :
      ReceiveWeight = _,
      SendWeight = _ |
	count_requests(SendAnchor, SendRequests),
	count_requests(ReceiveAnchor, ReceiveRequests),
	format_d_channel_b.

  format_d_channel_b(Name, Refs, SendRequests, ReceiveRequests,
				FormattedChannel) :-

    SendRequests =?= 0 :
      QM = "?",
      FormattedChannel = Name(QM(ReceiveRequests)) - Refs;

    ReceiveRequests =?= 0 :
      FormattedChannel = Name(SendRequests!) - Refs;

    otherwise :
      FormattedChannel = Name(SendRequests, ReceiveRequests) - Refs.

  format_channel_h(Channel, Kind, DimerWeight, Name, Refs,
				FormattedChannel) :-

    Kind =?= CHAR_b,
    read_vector(SPI_CHANNEL_RATE, Channel, Rate),
    Weight := Rate*DimerWeight*(DimerWeight - 1),
    Weight =?= 0 :
      Channel = _,
      Name = _,
      Refs = _,
      FormattedChannel = "";

    Kind =?= CHAR_b,
    read_vector(SPI_CHANNEL_RATE, Channel, Rate),
    Weight := Rate*DimerWeight*(DimerWeight - 1),
    Weight =\= 0 :
      Channel = _,
      Refs = _,
      FormattedChannel = Name(Weight);

    Kind =?= CHAR_c,
    read_vector(SPI_CHANNEL_RATE, Channel, Rate),
    Weight := Rate*DimerWeight*(DimerWeight - 1) :
      Channel = _,
      Refs = _,
      FormattedChannel = Name(Weight);

    Kind =?= CHAR_d,
    read_vector(SPI_SEND_ANCHOR, Channel, DimerAnchor) :
      DimerWeight = _,
      FormattedChannel = Name(DimerRequests) - Refs |
	count_requests(DimerAnchor, DimerRequests).


  format_channel_i(Kind, SendRequests, ReceiveRequests, Name, Refs,
				FormattedChannel) :-

    SendRequests =:= 0,
    ReceiveRequests =:= 0,
    Kind =?= CHAR_d :
      FormattedChannel = Name - Refs;

    SendRequests =:= 0,
    ReceiveRequests =:= 0,
    Kind =\= CHAR_d :
      Name = _,
      Refs = _,
      FormattedChannel = "";

    SendRequests > 0,
    ReceiveRequests =:= 0,
    Kind =?= CHAR_d :
      FormattedChannel = (Name ! SendRequests - Refs);

    SendRequests > 0,
    ReceiveRequests =:= 0,
    Kind =\= CHAR_d :
      FormattedChannel = (Name ! SendRequests - Refs);

    SendRequests > 0,
    ReceiveRequests =:= 0,
    Kind =?= CHAR_c :
      Refs = _,
      FormattedChannel = (Name ! SendRequests);

    SendRequests =:= 0,
    ReceiveRequests > 0,
    Kind =?= CHAR_d :
      QM = "?",
      FormattedChannel = (Name(QM(ReceiveRequests)) - Refs);

    SendRequests =:= 0,
    ReceiveRequests > 0,
    Kind =?= CHAR_c :
      Refs = _,
      QM = "?",
      FormattedChannel = Name(QM(ReceiveRequests));

    SendRequests > 0,
    ReceiveRequests > 0,
    Kind =?= CHAR_d :
      FormattedChannel = (Name : blocked(SendRequests, ReceiveRequests) - Refs);

    SendRequests > 0,
    ReceiveRequests > 0,
    Kind =\= CHAR_d :
      Refs = _,
      FormattedChannel = (Name : blocked(SendRequests, ReceiveRequests));

    otherwise :
      Kind = _,
      Name = _,
      ReceiveRequests = _,
      Refs = _,
      SendRequests = _,
      FormattedChannel = "".

  format_channel_out(FormattedChannel, Out, Out1) :-

    FormattedChannel =?= "" :
      Out = Out1;

    FormattedChannel =\= "" :
      Out = [FormattedChannel | Out1].
    

  count_requests(Anchor, Requests) + (Request = Anchor, Counter = 0) :-

    arg(SPI_MESSAGE_LINKS, Request, Links),
    read_vector(SPI_NEXT_MS, Links, Request'),
    Request' =\= Anchor,
    Request' =\= Request,
    Counter++ |
	self;

    otherwise :
      Anchor = _,
      Request = _,
      Requests = Counter.
