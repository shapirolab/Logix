/*

User Shell default macros
Ehud Shapiro, 01-09-86

Last update by		$Author: bill $
		       	$Date: 2000/01/23 11:46:36 $
Currently locked by 	$Locker:  $
			$Revision: 1.8 $
			$Source: /home/qiana/Repository/PiFcp/user_macros.cp,v $

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

% Pi Calculus macros

    Command = pc(N) :
      Cs = [to_context(pi_utils # make_channel(N))| Commands]\Commands;

    Command = pdb(Service) :
      Command' = pdb(Service, []) |
	expand;

    Command = pdb(Service, Options) :
      Cs = [pidbg#interpret(Name?, Service , Options)|Commands]\Commands |
	parse_rpc(Service, Name);

    Command = ph :
      CL = [	" a / a(No)          - abort computation No",
		" d(It)              - debug(It) (Goal or RPC)",
		" i(File)            - input file",
		" pc(C)              - make pifcp channel C",
		" pdb(RPC)           - debug(RPC)",
		" ph                 - get this list",
		" pr(C,M)            - receive M from pifcp channel C",
		" ps(M,C)            - send M on pifcp channel C",
		" re / re(No)        - resume computation No",
		" s / s(No)          - suspend computation No",
		" spc(C)             - show Pi channel",
		" spr / spr(No)      - resolvent of computation No",
		" spg / spg(No)      - show Pi goal No",
                " ctree(Tree)        - Close a vanilla tree",
                " ptree(Tree)        - Show Pi execution tree",
                " vtree(Co, G, Tree) - Call widgets#vanilla#tree(Co, G, Tree)",
		" Service - Goal     - call Service#Goal",
		" - Goal             - call Current#Goal",
		" {String}           - invoke UNIX shell sh with String",
                "",
                "           options for sp*, pdb and ptree:",
		" Integer            - depth of channel display",
		" none/active/all    - type of messages displayed",
		" sender/no_sender   - show name of message sender",
		" prefix/execute     - order of tree display"
	 ],
      Cs = [to_context(computation # display(stream,CL)) | Commands]\Commands ;

    Command = pr(M, C) :
      Cs = [to_context(pi_utils # receive(M, C)) | Commands]\Commands;

    Command = ps(M, C) :
      Cs = [to_context(pi_utils # send(M, C)) | Commands]\Commands;

    Command = spc(C) :
      Command' = spc(C, []) |
	expand;

    Command = spc(C, Options) :
      Cs = [to_context([computation # display(term, Channel, known(Channel)),
			pi_utils # show_channel(C, Options, Channel)]) 
	   | Commands]\Commands;

    Command = spg :
      Command' = spg(_) |
	expand;

    Command = spg(No) :
      Command' = spg(No, []) |
	expand;

    Command = spg(No, Options) :
      Cs = [state(No, Goal, _, _),
	    to_context([computation # display(term, Term, known(Term)),
			pi_utils # show_goal(Goal, Options, Term)]) 
	   | Commands]\Commands;

    Command = spr :
      Command' = spr(_, []) |
	expand;

    Command = spr(No) :
      Command' = spr(No, []) |
	expand;

    Command = spr(No, Options) :
      Cs = [resolvent(No, Resolvent),
	    to_context([computation # display(stream, Stream, []),
			pi_utils # show_resolvent(Resolvent, Options, Stream)])
	   | Commands]\Commands;

    Command = ctree(Tree) :
      Cs = [to_context(pi_utils # close_tree(Tree)) | Commands]\Commands;

    Command = ptree(Tree) :
      Command' = ptree(Tree, []) |
	expand;

    Command = ptree(Tree, Options) :
      Cs = [to_context([computation # display(stream, Stream, []),
			pi_utils # show_tree(Tree, Options, Stream)]) 
	   | Commands]\Commands;

    Command = vtree(Context, Conjunction, Tree) :
      Cs = [widgets # vanilla # tree(Context, Conjunction, Tree) 
	   | Commands]\Commands;

    Command = vtree(Context, Conjunction, Tree, Depth) :
      Cs = [widgets # vanilla # tree(Context, Conjunction, Tree, Depth) 
	   | Commands]\Commands;


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

parse_rpc(RPC, Name) :-

    RPC =?= Module # _ :
      Name = Module;

    otherwise :
      RPC = _,
      Name = "?".
