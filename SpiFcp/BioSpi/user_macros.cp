/*

User Shell default macros
Ehud Shapiro, 01-09-86

Last update by		$Author: bill $
		       	$Date: 2002/05/15 08:10:09 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/SpiFcp/BioSpi/user_macros.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([expand/2]).
-mode(trust).
-language(compound).

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
		" bc(Module)         - compile(Module)",
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

    Command = record(Goals, File, Limit) :
      Cs = [ambient_server#run(spi_record#run(repeat#run(Goals), File, Limit),
			       _ControlChannel)
	   | Commands]\Commands;

    Command = record(Goals, File, Limit, Scale) :
      Cs = [ambient_server#run(spi_record#run(repeat#run(Goals), File,
					      Limit, Scale),
			       _ControlChannel)
	   | Commands]\Commands;

    Command = record(Goals, File, Limit, Scale, Format) :
      Cs = [ambient_server#run(spi_record#run(repeat#run(Goals), File, 
					      Limit, Scale, Format),
			       _ControlChannel)
	   | Commands]\Commands;

    Command = run(Goals) :
      Cs = [ambient_server#run(repeat#run(Goals), _ControlChannel)
	   | Commands]\Commands;

    Command = run(Goals, Limit) :
      Cs = [ambient_server#run(spi_record#run(repeat#run(Goals), Limit),
			       _ControlChannel)
	   | Commands]\Commands;

    Command = trace(Goals, File, Limit) :
      Cs = [ambient_server#run(spi_trace#run(repeat#run(Goals), File, Limit),
			       _ControlChannel)
	   | Commands]\Commands;

    Command = trace(Goals, File, Limit, Scale) :
      Cs = [ambient_server#run(spi_trace#run(
				  repeat#run(Goals), File, Limit, Scale),
			       _ControlChannel)
	   | Commands]\Commands;

    Command = trace(Goals, File, Limit, Scale, Format) :
      Cs = [ambient_server#run(spi_trace#run(
				repeat#run(Goals), File, Limit, Scale, Format),
				_ControlChannel)
	   | Commands]\Commands;

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

    Command = atree :
      Cs = [state(No,Goal,_,_) | Commands]\Commands1 |
	ambient_tree(No, Goal, Commands, Commands1);

    Command = c :
      Cs = [compile |Commands]\Commands ;
    Command = c(Computation) :
      Cs = [compile(Computation)|Commands]\Commands ;
    Command = c(Computation,Options) :
      Cs = [compile(Computation,Options)|Commands]\Commands ;

    Command = bc :
      Cs = [compile(_, [mode(interrupt),
			control(system#transform#biospi#control)])
	   | Commands]\Commands ;
    Command = bc(Computation) :
      Cs = [compile(Computation,[mode(interrupt),
				 control(system#transform#biospi#control)])
	   | Commands]\Commands ;
    Command = bc(Computation,Options) :
      Cs = [compile(Computation, [mode(interrupt),
				  control(system#transform#biospi#control)
				 | Options])
	   | Commands]\Commands ;

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

    otherwise :
      Goal = _,
      Commands = [resolvent(No) | Commands1].


ambient_signal(Signal, No, Goal, Commands, Commands1) :-

    Goal =?= ambient_server#run(_Goals, Root),
    channel(Root) :
      write_channel(Signal(Reply), Root),
      Commands = [to_context([utils # append_strings(["<",No,">"], IdG),
		  computation#display(term, Reply,
				      [prefix(IdG),known(IdG),known(Reply)])])
		 | Commands1];

    Goal =\= ambient_server#run(_, _) :
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

ambient_tree(No, Goal, Commands, Commands1) :-

    Goal =?= ambient_server#run(_Goals, Root),
    channel(Root) :
      Commands = [to_context([utils # append_strings(["<",No,">"], IdG),
		  computation#display(stream, Tree,
				      [prefix(IdG),known(IdG), type(ground)])])
		 | Commands1] |
	format_ambient_tree([Root], "", Tree, []);

    otherwise :
      Commands = [to_context([utils # append_strings(["<",No,">"], IdG),
		  computation#display(term, "No ambient tree" - Goal,
				      [prefix(IdG),known(IdG)])])
		 | Commands1].

  format_ambient_tree(Ambients, Indent, Tree, NextTree) :-
    Ambients ? Ambient :
      write_channel(state(State), Ambient) |
	format_ambient_state_parts(Indent, State?, Tree, Tree'),
	self;

    Ambients ? _ClosedAmbient,
    otherwise |
	self;

    Ambients = [] :
      Indent = _,
      Tree = NextTree.

  format_ambient_state_parts(Indent, State, Tree, NextTree) :-

    State ? id(Name(Index)) :
      Tree ! Node |
	utils # append_strings([Indent, Name, "(", Index, ")"], Node),
	self;

    State ? id(system) |
	self;

    State ? children(Children),
    string_to_dlist(Indent, IL, [32, 32]),
    list_to_string(IL, Indent') :
      State' = _ |	% Assuming that id precedes children in state
	format_ambient_tree(Children, Indent', Tree, NextTree);

    State ? Other,
    Other =\= id(_), Other =\= children(_) |
	self;

    State =?= [] :
      Indent = _,
      NextTree = Tree.


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
