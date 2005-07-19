/*

SpiFcp Shell macros
William Silverman - 2005

Last update by		$Author: bill $
		       	$Date: 2005/07/19 14:46:53 $
Currently locked by 	$Locker:  $
			$Revision: 1.17 $
			$Source: /home/qiana/Repository/Aspic/user_macros.cp,v $

Copyright (C) 1998, Weizmann Institute of Science - Rehovot, ISRAEL

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

	Commands which are not expanded here are forwarded to the logix
	user_macros expansion and user shell processing.
*/

procedure expand(Any, [Any]\[Any]).

expand(Command, Cs) :-

    Cs = SCs\LCs :
      Cs' = Commands\[] |
	expand_spifcp,
	relay_derived.

  relay_derived(Commands, SCs, LCs) :-

    Commands ? Command |
	widgets#user_macros#expand(Command, SCs\SCs'),
	self;

    Commands =?= [] :
      SCs = LCs.

expand_spifcp(Command, Cs) :-

% Spi Calculus macros

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
		" cta                - display communicating channels",
		" i(File)            - input file",
		" options(New, Old)  - default Spi Options",
		" pc(C)              - make spi channel C",
		" pdb(RPC)           - debug(RPC)",
		" ph                 - get this list",
		" pr(C,M)            - receive M from spi channel C",
		" ps(M,C)            - send M on spifcp channel C",
		" randomize          - set all new channels to randomize",
		" serialize          - set all new channels to serialize",
		" re / re(No)        - resume computation No",
		" s / s(No)          - suspend computation No",
		" spc(C)             - Spi channel",
		" spg / spg(No)      - Spi goal of computation No",
		" spr / spr(No)      - Spi resolvent of computation No",
		" ctree(Tree)        - Close a vanilla tree",
		" ptree(Tree)        - Spi execution tree",
		" record(GS, F, L)   - run Goals, record to File until Limit.",
		" record(GS,F,L,S,O) - run Goals, record to File until Limit,",
		"                      scaled by Scale, with format Option.",
		" reset              - reset Spi monitor",
		" run(GS)            - run Goals.",
		" run(GS, L)         - run Goals until Limit.",
		" trace(GS, F, L)    - run Goals, trace to File until Limit.",
		" trace(GS,F,L,S,O)  - run Goals, trace to File until Limit,",
		"                      scaled by Scale, with format Option.",
		" vtree(Co, G, Tree) - Call widgets#vanilla#tree(Co, G, Tree)",
		" weighter(W)        - set the default weighter",
		" {String}           - invoke UNIX shell sh with String",
		"",
		"        name options for channel display",
		"             short/base/creator/full",
		"        annotation options for channel display:",
		"             none/active/note",
		"        additional option for ptree:",
		"           process replaces base above",
		"           goal order:",
		"             prefix/execute"
		
	 ],
      Cs = [to_context(computation # display(stream,CL)) | Commands]\Commands ;

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
	expand_spifcp;

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

    Command = spr :
      Command' = spr(_) |
	expand_spifcp;

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
			spi_utils # show_tree(Tree, [none | O?], Stream),
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

    Command = reset :
      Cs = [to_context(spi_monitor # reset) | Commands]\Commands;

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
	expand_spifcp;

    Command = spi2cmp(N, Options) :
      Cs = [to_context(computation # display(stream,Results, [type(unparse)])),
		       spi_macros # spi2cmp(N, Options, Results)
	   | Commands]\Commands;

    Command = spi2fcp(N) :
      Command' = spi2fcp(N, []) |
	expand_spifcp;

    Command = spi2fcp(N, Options) :
      Cs = [to_context(computation # display(stream,Results, [type(unparse)])),
		       spi_macros # spi2fcp(N, Options, Results)
	   | Commands]\Commands;

    Command = tpf(N) :
      Cs = [to_context(computation # display(stream,Results, [type(unparse)])),
		       spi_macros # transform(N, Results)
	   | Commands]\Commands;

    string_to_dlist(Command,[X, CHAR_t, CHAR_a], []),
    CHAR_b =< X, X =< CHAR_d :
      Cs = [state(No,_,_,_),
	    to_context([utils # append_strings(["<",No?,"> "], IdG),
			spi_monitor # status(Status),
			spi_debug # spi_channels(X, Status?, Out),
			spi_debug # stream_out(Out?, IdG?, Commands, Commands1)
		       ])
	   | Commands]\Commands1;

    otherwise :
      Cs = [Command | Commands]\Commands .


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

    Goals =?= (# Call) :
      Run = repeat#run(Service? # Call),
      Cs = [service(Service), to_context(spi_monitor#reset), Action,
	    service(Service?) | Commands]\Commands;

    Goals =?= (Service # Call),
    Goals =\= (_ * _ # _), Call =\= [_|_], Call =\= (_#_) :
      Run = Goals,
      Cs = [to_context(spi_monitor#reset), Action, service(Service?)
	   | Commands]\Commands;

    otherwise :
       Run = repeat#run(Goals),
       Cs = [to_context(spi_monitor#reset), Action | Commands]\Commands.
