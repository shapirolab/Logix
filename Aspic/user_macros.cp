/*

User Shell default macros
Ehud Shapiro, 01-09-86

Last update by		$Author: bill $
		       	$Date: 2002/10/09 07:20:54 $
Currently locked by 	$Locker:  $
			$Revision: 1.5 $
			$Source: /home/qiana/Repository/Aspic/user_macros.cp,v $

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

% Spi Calculus macros

    Command = format_channel(Kind, Channel, FormattedChannel?^) :
      Cs = Commands\Commands |
	format_channel;

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
		" i(File)            - input file",
		" options(New, Old)  - default Spi Options",
		" pc(C)              - make spi channel C",
		" pdb(RPC)           - debug(RPC)",
		" ph                 - get this list",
		" pr(C,M)            - receive M from spi channel C",
		" prgcs              - reset Spi global channels",
		" ps(M,C)            - send M on spifcp channel C",
		" re / re(No)        - resume computation No",
		" s / s(No)          - suspend computation No",
		" spc(C)             - Spi channel",
		" spg / spg(No)      - Spi goal of computation No",
		" spgcs              - Spi global channels",
		" spr / spr(No)      - Spi resolvent of computation No",
		" ctree(Tree)        - Close a vanilla tree",
		" ptree(Tree)        - Spi execution tree",
		" record(GS, F, L)   - run Goals, record to File until Limit.",
		" record(GS,F,L,S,O) - run Goals, record to File until Limit,",
		"                      scaled by Scale, with format Option.",
		" run(GS)            - run Goals.",
		" run(GS, L)         - run Goals until Limit.",
		" trace(GS, F, L)    - run Goals, trace to File until Limit.",
		" trace(GS,F,L,S,O)  - run Goals, trace to File until Limit,",
		"                      scaled by Scale, with format Option.",
		" vtree(Co, G, Tree) - Call widgets#vanilla#tree(Co, G, Tree)",
		" weighter(W)        - set the default weighter",
		" {String}           - invoke UNIX shell sh with String",
		"",
		"        options for sp*, pdb and ptree:",
		" none/active        - type of messages displayed",
		" sender/no_sender   - show name of message sender",
		"        additional options for ptree:",
		" prefix/execute     - order of tree display",
		"       format options for record and trace",
		"           short/process/creator/full",
		"       format options for channel display",
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

    Command = spr :
      Command' = spr(_) |
	expand;

    Command = spr(No) :
      Cs = [resolvent(No, Resolvent),
	    to_context([spi_monitor # options(O, O),
			spi_utils # show_resolvent(Resolvent, O?, Stream),
			computation # display(stream, Stream, [])])
	   | Commands]\Commands;

    Command = spr(No, Options) :
      Cs = [resolvent(No, Resolvent),
	    to_context([spi_utils # show_resolvent(Resolvent, Options, Stream),
			computation # display(stream, Stream)])
	   | Commands]\Commands;

    Command = ctree(Tree) :
      Cs = [to_context(spi_utils # close_tree(Tree))| Commands]\Commands;

    Command = ptree(Tree) :
      Cs = [to_context([spi_monitor # options(O, O),
			spi_utils # show_tree(Tree, O?, Stream),
			computation # display(stream, Stream)])
	   | Commands]\Commands;

    Command = ptree(Tree, Options) :
      Cs = [to_context([computation # display(stream, Stream, []),
			spi_utils # show_tree(Tree, Options, Stream)]) 
	   | Commands]\Commands;

    Command = record(Goals, File, Limit) |
	spi_run(Goals, Run, spi_record#run(Run, File, Limit), Cs);

    Command = record(Goals, File, Limit, Scale) |
	spi_run(Goals, Run, spi_record#run(Run, File, Limit, Scale), Cs);

    Command = record(Goals, File, Limit, Scale, Format) |
	spi_run(Goals, Run,
		    spi_record#run(Run, File, Limit, Scale, Format), Cs);

    Command = run(Goals) |
	spi_run(Goals, Run, Run, Cs);

    Command = run(Goals, Limit) |
	spi_run(Goals, Run, spi_record#run(Run, Limit), Cs);

    Command = trace(Goals, File, Limit) |
	spi_run(Goals, Run, spi_trace#run(Run, File, Limit), Cs);

    Command = trace(Goals, File, Limit, Scale) |
	spi_run(Goals, Run, spi_trace#run(Run, File, Limit, Scale), Cs);

    Command = trace(Goals, File, Limit, Scale, Format) |
	spi_run(Goals, Run,
		    spi_trace#run(Run, File, Limit, Scale, Format), Cs);

    Command = vtree(Context, Conjunction, Tree) :
      Cs = [widgets # vanilla # tree(Context, Conjunction, Tree) 
	   | Commands]\Commands;

    Command = vtree(Context, Conjunction, Tree, Depth) :
      Cs = [widgets # vanilla # tree(Context, Conjunction, Tree, Depth) 
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
	    to_context(computation # display(term, Goal, [prefix(IdG), known(IdG)]))
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
		" bta                - busy channels",
		" c / c(Module)      - compile(Module)",
		" cta                - communicating channels",
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
      Cs = [resolvent|Commands]\Commands ;
    Command = r(Computation), integer(Computation) :
      Cs = [resolvent(Computation)|Commands]\Commands ;

    Command = r(Ids), "" @< Ids :
      Cs = [computation(CO), display(stream, Goals, prefix(CN))|Commands]\Commands |
	utils # append_strings(["<",CO,">"], CN),
        resolvent # extract(CO, Ids, Goals);
    Command = r(CO, Ids) :
      Cs = [display(stream, Goals, prefix(CN))|Commands]\Commands |
	utils # append_strings(["<",CO,">"], CN),
        resolvent # extract(CO, Ids, Goals);

    Command = re :
      Cs = [resume|Commands]\Commands ;
    Command = re(Computation) :
      Cs = [resume(Computation)|Commands]\Commands ;

    Command = s :
      Cs = [suspend|Commands]\Commands ;
    Command = s(Computation) :
      Cs = [suspend(Computation)|Commands]\Commands ;

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

spi_run(Goals, Run, Action, Cs) :-

    Goals =?= (Service # _Call) :
      Run = Goals |
      Cs = [to_context(spi_monitor#reset), Action, service(Service?)
	   | Commands]\Commands;

    Goals =?= (# Call) :
      Run = Service? # Call,
      Cs = [service(Service), to_context(spi_monitor#reset), Action,
	    service(Service?) | Commands]\Commands;

    otherwise :
       Run = repeat#run(Goals),
       Cs = [to_context(spi_monitor#reset), Action | Commands]\Commands.


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
						[prefix(P),
						known(Done), known(Term),
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
    read_vector(SPI_BLOCKED, Channel, Blocked),
    read_vector(SPI_DIMER_WEIGHT, Channel, DimerWeight),
    DimerWeight > 0,
    read_vector(SPI_DIMER_ANCHOR, Channel, DimerAnchor) |
	count_requests(DimerAnchor, DimerRequests),
	format_channel_h;

    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Type =?= SPI_INSTANTANEOUS,
    read_vector(SPI_RECEIVE_ANCHOR, Channel, ReceiveAnchor),
    read_vector(SPI_SEND_ANCHOR, Channel, SendAnchor) |
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

    SendWeight =\= 0, ReceiveWeight =\= 0,
    read_vector(SPI_BLOCKED, Channel, Blocked),
    Blocked =?= FALSE,
    CHAR_b =< Kind, Kind =< CHAR_c,
    read_vector(SPI_CHANNEL_RATE, Channel, Rate),
    /* Eventually use c-extension (spifcp.c) to compute weight */
    Weight := Rate * SendWeight * ReceiveWeight :
      Refs = _,
      FormattedChannel = Name(Weight);

    SendWeight =\= 0, ReceiveWeight =\= 0,
    read_vector(SPI_BLOCKED, Channel, Blocked),
    Blocked =?= TRUE,
    Kind >= CHAR_c :
      Refs = _,
      QM = "?" |
      FormattedChannel = (Name : blocked(SendWeight!, QM(ReceiveWeight)));

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
    read_vector(SPI_BLOCKED, Channel, Blocked),
    read_vector(SPI_SEND_ANCHOR, Channel, SendAnchor),  
    read_vector(SPI_RECEIVE_ANCHOR, Channel, ReceiveAnchor) :
      ReceiveWeight = _,
      SendWeight = _ |
	count_requests(SendAnchor, SendRequests),
	count_requests(ReceiveAnchor, ReceiveRequests),
	format_d_channel_b;

    otherwise :
      Channel = _,
      Kind = _,
      Name = _,
      ReceiveWeight = _,
      Refs = _,
      SendWeight = _,
      FormattedChannel = "".


  format_d_channel_b(Name, Blocked, Refs, SendRequests, ReceiveRequests,
				FormattedChannel) :-

    SendRequests =?= 0 :
      Blocked = _,
      QM = "?",
      FormattedChannel = Name(QM(ReceiveRequests)) - Refs;

    ReceiveRequests =?= 0 :
      Blocked = _,
      FormattedChannel = Name(SendRequests!) - Refs;

    otherwise,
    Blocked = FALSE :
      QM = "?",
      FormattedChannel = Name(SendRequests!, QM(ReceiveRequests)) - Refs;

    otherwise,
    Blocked = TRUE :
      QM = "?",
      FormattedChannel = (Name : blocked(SendRequests!, QM(ReceiveRequests))
				- Refs).

  format_channel_h(Channel, Kind, Blocked, DimerWeight, DimerRequests, Name,
			Refs, FormattedChannel) :-


    CHAR_b =< Kind, Kind =< CHAR_c,
    DimerRequests >= 2,
    Blocked =?= FALSE,
    read_vector(SPI_CHANNEL_RATE, Channel, Rate),
    Weight := Rate*DimerWeight*(DimerWeight - 1) :
      Channel = _,
      Refs = _,
      FormattedChannel = Name(Weight);

    Kind =?= CHAR_d,
    DimerRequests >= 2,
    Blocked =?= FALSE :
      Channel = _,
      DimerWeight = _,
      QM = "?",
      FormattedChannel = Name(QM(DimerRequests!)) - Refs;

    Kind =?= CHAR_b,
    otherwise :
      Blocked = _,
      Channel = _,
      DimerRequests = _,
      DimerWeight = _,
      Name = _,
      Refs = _,
      FormattedChannel = "";

    Kind =?= CHAR_c,
    DimerRequests < 2 :
      Blocked = _,
      Channel = _,
      DimerWeight = _,
      Refs = _,
      QM = "?",
      FormattedChannel = Name(QM(DimerRequests!));

    Kind =?= CHAR_d,
    DimerRequests < 2 :
      Blocked = _,
      Channel = _,
      DimerWeight = _,
      QM = "?",
      FormattedChannel = Name(QM(DimerRequests!)) - Refs;

    Kind =?= CHAR_c,
    Blocked = TRUE :
      Channel = _,
      DimerWeight = _,
      Refs = _,
      QM = "?",
      FormattedChannel = (Name : blocked(QM(DimerRequests!)));

    Kind =?= CHAR_d,
    Blocked = TRUE :
      Channel = _,
      DimerWeight = _,
      Refs = _,
      QM = "?",
      FormattedChannel = (Name : blocked(QM(DimerRequests!)) - Refs).


  format_channel_i(Kind, SendRequests, ReceiveRequests, Name, Refs,
				FormattedChannel) :-

    SendRequests =:= 0,
    ReceiveRequests =:= 0,
    Kind =?= CHAR_d :
      FormattedChannel = Name - Refs;

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
    Kind =?= CHAR_c :
      Refs = _,
      QM = "?",
      FormattedChannel = (Name : blocked(SendRequests!, QM(ReceiveRequests)));

    SendRequests > 0,
    ReceiveRequests > 0,
    Kind =?= CHAR_d :
      QM = "?",
      FormattedChannel = (Name : blocked(SendRequests!, QM(ReceiveRequests))
				- Refs);

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
