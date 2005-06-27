/*

User Shell default macros
Ehud Shapiro, 01-09-86

Last update by		$Author: bill $
		       	$Date: 2005/06/27 04:27:15 $
Currently locked by 	$Locker:  $
			$Revision: 1.22 $
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

    Cs = SCs\LCs :
      Cs' = Commands\[] |
        expand_biospi,
        relay_derived.

  relay_derived(Commands, SCs, LCs) :-

    Commands ? Command |
        widgets#user_macros#expand(Command, SCs\SCs'),
        self;

    Commands =?= [] :
      SCs = LCs.

expand_biospi(Command, Cs) :-

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
		" atr / atr(Ambient) - display ambient tree",
		" btr / btr(Ambient) - ambient tree with busy channels",
		" c / c(Module)      - compile(Module)",
		" cno / cno(No)      - current computation number",
		" bta                - busy channels table",
		" cta                - communicating channels table",
		" ctr / ctr(Ambient) - ambient tree with communicating channels",
		" i(File)            - input file",
		" options(New, Old)  - default BioSpi Options",
		" pc(C)              - make BioSpi channel C",
		" ph                 - get this list",
		" pr(C,M)            - receive M from BioSpi channel C",
		" ps(M,C)            - send M on BioSpi channel C",
		" rtr / rtr(Ambient) - ambient tree with resolvent",
		" spc(C)             - Show BioSpi channel",
                " randomize          - set all new channels to randomize",
                " serialize          - set all new channels to serialize",
 		" record(GS, F, L)   - run Goals, record to File until Limit.",
		" record(GS,F,L,S,O) - run Goals, record to File until Limit,",
		"                      scaled by Scale, with format Option.",
                " reset              - reset Spi monitor",
		" run(GS)            - run Goals (no limit).",
		" run(GS, L)         - run Goals until Limit.",
		" atrace(GS, F)      - run Goals, trees to File (no limit).",
		" atrace(GS, F, L)   - run Goals, trees to File until Limit.",
		" atrace(GS,F,L,S)   - run Goals, trees to File until Limit,",
                "                      scaled by Scale.",
		" trace(GS, F, L)    - trace Goals to File until Limit.",
		" trace(GS,F,L,S,O)  - trace Goals to File until Limit,",
		"                      scaled by Scale, with format Option.",
		" weighter(W)        - set the default weighter",
		" {String}           - invoke UNIX shell sh with String",
		"",
		"        name options for channel display",
		"             short/base/creator/full",
		"        annotation options for channel display:",
                "             none/active/note"
		
	 ],
      Cs = [to_context(computation # display(stream,CL)) | Commands]\Commands ;

    Command = atrace(Goals, File) |
	ambient_list(Goals, Run ,ambient_list#run(Run?, File), Cs);

    Command = atrace(Goals, File, Limit) |
	ambient_list(Goals, Run ,ambient_list#run(Run?, File, Limit), Cs);

    Command = atrace(Goals, File, Limit, Scale) |
	ambient_list(Goals, Run ,ambient_list#run(Run?, File, Limit, Scale),
			Cs);

    Command = trace(Goals, File, Limit) |
	ambient_run(Goals, Run, spi_trace#run(Run, File, Limit), Cs);

    Command = trace(Goals, File, Limit, Scale) |
        ambient_run(Goals, Run, spi_trace#run(Run, File, Limit, Scale), Cs);

    Command = trace(Goals, File, Limit, Scale, Format) |
	ambient_run(Goals, Run,
		    spi_trace#run(Run, File, Limit, Scale, Format), Cs);

    Command = pr(C, M) :
      Cs = [to_context(spi_utils # receive(C, M)) | Commands]\Commands;

    Command = pr(C, M, N) :
      Cs = [to_context(spi_utils # receive(C, M, N)) | Commands]\Commands;

    Command = ps(M, C) :
      Cs = [to_context(spi_utils # send(M, C)) | Commands]\Commands;

    Command = ps(M, C, N) :
      Cs = [to_context(spi_utils # send(M, C, N)) | Commands]\Commands;

    Command = randomize :
      Cs = [to_context([spi_monitor # randomize])
           | Commands]\Commands;

    Command = serialize :
      Cs = [to_context([spi_monitor # serialize])
           | Commands]\Commands;

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
	expand_biospi;

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

    Command = record(Goals, File, Limit) |
	ambient_run(Goals, Run, spi_record#run(Run, File, Limit), Cs);

    Command = record(Goals, File, Limit, Scale) |
	ambient_run(Goals, Run, spi_record#run(Run, File, Limit, Scale), Cs);

    Command = record(Goals, File, Limit, Scale, Format) |
	ambient_run(Goals, Run,
		    spi_record#run(Run, File, Limit, Scale, Format), Cs);

    Command = reset :
      Cs = [to_context(spi_monitor # reset) | Commands]\Commands;

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
	expand_biospi;

    Command = spi2cmp(N, Options) :
      Cs = [to_context(computation # display(stream,Results, [type(unparse)])),
	    spi_macros # spi2cmp(N, Options, Results) |Commands]\Commands;

    Command = spi2fcp(N) :
      Command' = spi2fcp(N, []) |
	expand_biospi;

    Command = spi2fcp(N, Options) :
      Cs = [to_context(computation # display(stream,Results, [type(unparse)])),
	    spi_macros # spi2fcp(N, Options, Results) |Commands]\Commands;

    Command = tpf(N) :
      Cs = [to_context(computation # display(stream,Results, [type(unparse)])),
	    spi_macros # transform(N, Results) |Commands]\Commands;

% override logix user macros.

    Command = rtr :
      Command' = rtr("") |
	expand_biospi;
    Command = rtr(Selector) :
      Cs = [state(No,Goal,_,_),
	    to_context([utils # append_strings(["<",No,">"], IdG),
			spi_monitor # options(Options, Options),
			spi_utils#Requests?,
			spi_debug # stream_out(Out?, IdG, Commands, Commands1)]),
			ExtraCommand | Commands]\Commands1,
      Aux = resolvent(Options, Requests, []) |
	ambient_tree;

    Command = re :
      Command' = re(_) |
	expand_biospi;
    Command = re(No) :
      Cs = [state(No,Goal,_,_)|Commands]\Commands1 |
	ambient_signal(resume, No, Goal, Commands, Commands1) ;

    Command = s :
      Command' = s(_) |
	expand_biospi ;
    Command = s(No) :
      Cs = [state(No,Goal,_,_)|Commands]\Commands1 |
	ambient_signal(suspend, No, Goal, Commands, Commands1) ;
    string_to_dlist(Command,[X, CHAR_t, CHAR_a], []),
    CHAR_b =< X, X =< CHAR_d :
      Cs = [state(No,_,_,_),
            to_context([utils # append_strings(["<",No?,"> "], IdG),
                        spi_monitor # status(Status),
                        spi_debug # spi_channels(X, Status?, Out),
                        spi_debug # stream_out(Out?, IdG?, Commands, Commands1)
                       ])
           | Commands]\Commands1;

    string_to_dlist(Command,[X, CHAR_t, CHAR_r], []),
    CHAR_a =< X, X =< CHAR_d :
      Command' = Command("") |
        self;

    Command = Xtr(Selector),
    string_to_dlist(Xtr,[X, CHAR_t, CHAR_r], []),
    CHAR_a =< X, X =< CHAR_d :
      ExtraCommand = _,
      Aux = channels(X, [], []),
      Cs = [state(No,Goal,_,_),
            to_context([utils # append_strings(["<",No,">"], IdG),
                        spi_debug # stream_out(Out?, IdG, Commands, Commands1)
                       ])
           | Commands]\Commands1 |
        ambient_tree;

    otherwise :
      Cs = [Command | Commands]\Commands .


% Added for BioSpi development.

ambient_tree(Selector, Aux, Goal, No, ExtraCommand, Out) :-

    Goal =?= ambient_server#Run,
    arg(1, Run, run),
    arg(3, Run, Root),
    arg(1, Aux, Type),
    channel(Root) :
      No = _,
      write_channel(tree(Type, Tree), Root),
      ExtraCommand = true |
	ambient_tree1 + (Out1 = []);

    otherwise,
    Aux =?= _Type(_Arg, Requests, NextRequests) :
      Goal = _,
      Selector = _,
      Requests = NextRequests,
      ExtraCommand = resolvent(No),
      Out = [].

  ambient_tree1(Selector, Aux, Tree, Out, Out1) :-

    Tree =?= _Type(Id, List, SubTree),
    Selector =?= "",
    Id =?= system |
	format_tree_parts;

    Tree =?= _Type(Id, List, SubTree),
    Selector =?= "",
    Id =\= system :
      Out ! "+" |
	format_tree_parts + (Out1 = ["-" | Out1]);

    Tree =?= _Type(Id, List, SubTree),
    Selector =?= Id |
	format_tree_parts;

    Tree =?= _Type(Id, List, SubTree),
    Id =?= Selector(_Index) |
	format_tree_parts;

    Tree =?= _Type(Id, List, SubTree),
    Id =?= _Name(Selector) :
      Selector' = "" |
	format_tree_parts;

    Tree =?= _Type(Id, List, SubTree),
    Id =?= _Name(I),
    I =:= -Selector |
	format_tree_parts;

    Tree =?= _Type(_Id, _List, SubTree),
    otherwise |
	ambient_tree2.

  ambient_tree2(Selector, Aux, SubTree, Out, Out1) :-

    SubTree ? Tree,
    Aux = Type(Arg, Requests, Requests1) :
      Aux' = Type(Arg, Requests', Requests1) |
	ambient_tree1(Selector, Type(Arg, Requests, Requests'?), Tree, Out, Out'?),
	self;

    SubTree =?= [],
    Aux =?= _Type(_Arg, Requests, NextRequests) :
      Requests = NextRequests,
      Selector = _,
      Out = Out1.

format_tree_parts(Selector, Aux, SubTree, Out, Out1, Id, List) :-

    Aux =?= channels(Kind, _Requests, _NextRequests) :
      Out = [Id, "+", "+" | Out'?] |
	spi_debug # format_channels(Kind, List, Out', ["-", "-" | Out''?]),
	ambient_tree2(Selector, Aux, SubTree, Out'', Out1);

    Aux =?= resolvent(Options, Requests, NextRequests) :
      Requests ! show_resolvent(List, Options, Vs),
      Out = [Id, "+", "+" | Out'?],
      Aux' = resolvent(Options, Requests', NextRequests) |
	copy_list(Vs?, Out', ["-", "-" | Out''?]),
	ambient_tree2(Selector, Aux', SubTree, Out'', Out1).


ambient_list(Goals, Run, AList, Cs)  :-

    Goals =?= (# Call) :
      Run = repeat#run(Service? # Call),
      Cs = [service(Service), AList , service(Service?)
	   | Commands] \ Commands;

    Goals =?= (Service # Call),
    Goals =\= (_ * _ # _), Call =\= [_|_], Call =\= (_#_) :
      Run = Goals |
	Cs = [AList, service(Service) | Commands]\Commands;

    otherwise :
      Run = repeat#run(Goals),
      Cs = [AList | Commands]\Commands.	

ambient_run(Goals, Run, Action, Cs) :-

    Goals =?= (# Call) :
      Run = repeat#run(Service? # Call),
      Cs = [service(Service),
	    ambient_server#run(Action, _ControlChannel),
	    service(Service?) | Commands] \ Commands;

    Goals =?= (Service # Call),
    Goals =\= (_ * _ # _), Call =\= [_|_], Call =\= (_#_) :
      Run = Goals |
      Cs = [ambient_server#run(Action, _ControlChannel), service(Service)
	   | Commands]\Commands;

    otherwise :
      Run = repeat#run(Goals),
      Cs = [ambient_server#run(Action, _ControlChannel)
	    | Commands]\Commands.

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
      Out = Next .
