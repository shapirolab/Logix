-language(compound).

%procedure boot(Any, Nil).

boot(_In, Out) :-
    true :
      Out = [] |
	boot_io.

boot_io :-

    true :
      link(['file.o'], IO) |
	phase1(IO);

    true :
      link(['bin/file.o'], IO) |
	phase1(IO);

    otherwise :
      ttyput_string('Boot failed - no file kernel found.') .


phase1(IO) :-
    true :
      execute(IO, {6, _, WD, _}) |
	add_slash(WD, WorkingDirectory),
	get_modules(IO, WorkingDirectory,
		    [processor, domain_server, computation_server,
		     hierarchy_server
		    ],
		    AddServices,
		    Done
	),
	phase2(Done, AddServices).


add_slash('/','/'^).
add_slash(WD, WorkingDirectory) :-
    otherwise,
    Slash := ascii('/'),
    string_to_dlist(WD, [Slash | Rest], [Slash]^) |
	list_to_string([Slash | Rest], WorkingDirectory).


get_modules(IO, WorkingDirectory, Names, Adds, Done) :-
    Names ? Name,
    string_to_dlist(WorkingDirectory, WDNB, CName^),
    string_to_dlist(WorkingDirectory, WDBNB, CBinName^),
    string_to_dlist(Name, CName, []^),
    string_to_dlist('Bin/', CBinName, CName^) :
      Adds ! Add |
	list_to_string(WDNB, WDName),
	list_to_string(WDBNB, WDBinName),
	compose_names([WDName, WDBinName], [".bin", ".o"], FullNames),
	load_modules(IO, FullNames, Name, Choices),
       	choose_module(Name, Choices, Add, Done, Done'),
	get_modules.

get_modules(_, _, [], []^, done^).


compose_names(Prefixes, Suffixes, FullNames) + (ReSuffix = Suffixes) :-

    Prefixes = [Prefix | _],
    Suffixes ? Suffix,
    string_to_dlist(Prefix, PL, SL),
    string_to_dlist(Suffix, SL, []) :
      FullNames ! Suffix(FullName) |
	list_to_string(PL, FullName),
	compose_names;

   Prefixes ? _,
   Suffixes = [] :
     Suffixes' = ReSuffix |
	compose_names;

   Prefixes = [] : Suffixes = _, ReSuffix = _,
     FullNames = [] .

/*
print_results(Rs) :-
    Rs ? Name(R, _, _),
    known(Name),
    integer(R) :
      ttyput_string(Name),
      ttyput_string(" = "),
      ttyput_integer(R) |
	print_results;
    known(Name),
    Rs ? Name(R, _, _),
    string(R) :
      ttyput_string(Name),
      ttyput_string(" = "),
      ttyput_string(R) |
	print_results;
    Rs = [] .
*/

load_modules(IO, FullNames, Name, Choices) :-

    FullNames ? Type(FileName),
    known(FileName), Type = '.bin' :
      execute(IO, {2, FileName, Module, Result}),
      Choices ! Name(Result, Module, FileName) |
	load_modules;


    FullNames ? Type(FileName),
    known(FileName), Type = '.o' :
      execute(IO, {7, Name, Reply}),
      Choices ! Name(Result, Module, FileName) |
        get_doto(Name, Module, Reply, Result),
	load_modules;

	FullNames = [] : IO = _, Name = _,
      Choices = [] .


get_doto(Name, Module, Reply, Result) :-

    string(Reply) :
      link({[Name]}, Module),
      Result = 0;

    Reply = 0 : Name = _, Module = _,
      Result = zero ;

    otherwise : Name = _, Module = _,
      Result = Reply .


choose_module(Name, Choices, Add, Done1, Done2) :-

    Choices ? Name(0, Module, FileName) : Choices' = _,
      Add = Name(Module, FileName),
      Done1 = Done2 ;

    Choices ? _Name(Result, _Module, _FileName),
    Result =\= 0 |
	choose_module;

    Choices = [] : Add = _, Done2 = _,
      Done1 = failed(Name) .


phase2(Done, AddServices) :-

    Done = done,
    AddServices ? processor(Module, _),
    module(Module) :
      make_channel(CC, Delegates),
      make_channel(OutC, Outs),
      make_channel(Domain, FromHS'),
      Controls1 = {Control, Left, OutC} |

	activate(domain_server, domain(FromHS, SCC, ToHS, Module),
		 AddServices', Controls1, {Control, Right, OutC}
	),

	monitor_controls(Outs, Left, Right, KernelEvents),
	monitor_circuit([kernel | KernelEvents], Left, Right),

/******************** A D D E D   S E R V I C E S ****************************/

	add_services(AddServices', FromHS, Finds, _Close),

	Finds = [find([computation_server], CSC) | FromHS'],
	
	StartInput = file('.logix_cold_start'),

	Computation =	['_controls'(SCC), identifier(root),
		         hierarchy_server # start(ToHS),
			 processor # machine(faults(Faults, [])),
			 boot_terminal # io(Bytes, TCH),
			 fault_server # serve(Faults, Faults'),
			 change_scope([system], _Changed),
			 shell_server # root(Bytes?, Faults'?, Events?, Signals,
					     Delegates?, Domain?, TCH?),
			 input(StartInput)
			| Signals?],

	write_channel(export([self],[],
			     computation(Computation, {_CO, _CL, _CR, CC},
					 Domain, Events
			     ),
			     {_, _, _, _}
		      ),
		      CSC
	);

    Done = failed(Name) : AddServices = _,
      ttyput_string('Boot failed - '),
      ttyput_string(Name),
      ttyput_string(' not found.') .


activate(Name, Goal, Services, Controls1, Controls2) :-
    Services ? Name(Module, _),
    Controls1 = {Control, Left, Channel} : Services' = _,
      activate(Module, export(Goal), _(Left, Right, Channel)),
      Controls2 = {Control, Right, Channel} ;

    Services ? Other(_, _),
    Other =\= Name |
	activate.


add_services(AddServices, FromHS, Finds, Close) :-
    AddServices ? Name(Module, BinName) :
      FromHS ! add_service([Name], module(Module, BinName), _) |
	add_services.
add_services([], Finds^, Finds, _).


monitor_controls(Outs, Left, Right, Signals) :-

    Outs ? exception(Reason, Goal, Close, Close^) :
      Signals ! failed('_kernel' # Goal, Reason) |
	monitor_controls;

    Outs ? transmit(Service, Goal, Close, Close^) :
      Signals ! self # [Service] # Goal |
	monitor_controls;

    Left = Right : Outs = _,
      Signals = [] .


monitor_circuit(Out, SL, SR) :-
    SL = SR : Out = _ .




