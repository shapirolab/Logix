/*

Top level of the compiler - put's all the pieces together.

William Silverman, Michael Hirsch 

$Header: /home/qiana/Repository/Logix/system/compile/self.cp,v 1.1 1999/07/09 07:03:35 bill Exp $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([context/4, file/3, characters/3, string/3, parsed/3, deadcode/2]).
-mode(trust).
-language(compound).

List ::= [Any].

procedure file(String, List, List).

file(File, Options, Results):-
	compile(context(computation, File), Options, Results).

procedure context(Any, String, List, List).

context(Context, Name, Options, Results):-
	compile(context(Context, Name), Options, Results).

procedure characters([Integer], List, List).

characters(Chars, Options, Results) :-
	compile(characters(Chars), Options, Results).

procedure string(String, List, List).

string(String, Options, Results) :-
	compile(string(String), Options, Results).

procedure parsed(List, List, List).

parsed(Terms, Options, Results) :-
	compile(parsed(Terms), Options, Results).

procedure deadcode([procedure(Id, Ids)], Ids).

Id ::= String/Integer.
Ids ::= [Id].

deadcode(Ps, Is) :-
	precompile # deadcode(Ps, Is).

compile(Source, Options, Results) :-
	monitor_functions # choke_stream(Done, Output, Results, Abort),
	prepare_controls(Options, Abort,
		SourceControl, PrecomControl, OutputControl
	),
	get_source_terms(SourceControl, Source, Result, {Output, Output1},
				Abort
	),
	precompile_terms(Abort, Result, PrecomControl, Output1, PrecomReply),
	compile_and_output(Abort, PrecomReply, OutputControl, Done).


get_source_terms(SourceControl, Call, Result, Outs, Abort) :-

    Call = context(SId', Source), list(SId'), string(Source) : Abort = _,
      SId ! Source,
      SourceControl = _(Options, SId) |
	get_source # context(SId', Source, [service_id(SId) | Options], 
			     Result, Outs
		     );

    Call = context(_, Id),
    Id ? Name : 
      Call' = context(Id', Name) |
	get_source_terms;

    Call = context(_, T),
    tuple(T) :
      Call' = context([], Id) |
	computation_utils # path_id(T, Id, _),
	get_source_terms;

    Call = context(Path, Source),
    otherwise |
	computation_utils # call_id_goal(Path#Source, SId, Name, Reply),
	verify_source_id(Reply, SId, Name, Call'),
	get_source_terms;

    Call = Oper(Rand) : Abort = _,
      SourceControl = Name(Options, SId?),
      Options' ! service_id(SId?) |
	scan_id(Name, Options, Options'', SId),
	get_source # Oper(Rand, Options', Result, Outs);

    known(Abort) : Call = _,
      SourceControl = _(_, ['_']),
      Result = false(aborted),
      Outs = {O, O} .      

verify_source_id(true, Id, Name, context(Id, Name)^) :-
    string(Name) | true.
verify_source_id(true, Id, Name, context([], Name)^) :-
    Id =\= [],
    compound(Name) | true.
verify_source_id(_, _, _, parsed([])^) :-
    otherwise | true.

scan_id(Name, Opt1, Opt2, SId) :-

    Opt1 ? service_id(Id) :
      SId' = SId? |
	self,
	unify_without_failure(SId, Id?);

    Opt1 ? Opt, Opt =\= service_id(_) :
      Opt2 ! Opt |
	self;

    Opt1 =\= [_|_], Opt1 =\= [] :
      Opt1' = [Opt1] |
	self;

    Opt1 = [],
    writable(SId) :
      SId = [Name | CId?],
      Opt2 = [] |
	computation # (self # service_id(CId));

    Opt1 = [], known(SId) : Name = _,
      Opt2 = [] .


precompile_terms(Abort, Result, PrecomControl, Output, PrecomReply) :-

    Result = library(Options, Attributes, Terms) : Abort = _,
      PrecomControl = {_, _, [], Target, _},
      Output ! library,
      PrecomReply = halt |
	languages(Options, Attributes, Terms, Output', Target,
			_, Terms', Output''
	),
	precompile # library(Terms', Output'', Target);

    Result = module(Options, Attributes, Terms) :
      PrecomControl = {ModuleName, Control, Intermediate, Target, Mode},
      PrecomReply = ok |
	languages(Options, Attributes, Terms, Output, Target,
			Combined, Terms', Output''
	),
	precompile # transform(Combined, Terms', Mode, Exported,
				Importing, Precompiled, Output', Target
	),
	Control # Mode({Exported, Importing}, Precompiled, Protected),
	close_intermediate(Abort, ModuleName, Protected, Intermediate, 
				Output', Output''
	);

    Result = false(Remark) : Abort = _,
      PrecomControl = {_, _, [], _, _},
      Output = [Remark],
      PrecomReply = halt .


languages(Options, Attributes1, Terms1, Output1, Target,
		Attributes2, Terms2, Output2
) :-
	transform # languages(Options, Target, Attributes1, Attributes1',
				Terms1, Terms2, Output1, Output2
		  ),
	scan_id(_, Attributes1', Attributes2, _).


close_intermediate(abort, Name, _, module(Name, [])^, _, []^).
close_intermediate(_, Name, Code, module(Name, Code)^, Output, Output^) :-
    known(Code) |
	true.


compile_and_output(_, _, halt, done^).
compile_and_output(Abort, PrecomReply, OutputControl, Done) :-

    PrecomReply = ok,
    OutputControl = file(Display, Intermediate, Encoder, Module, ServiceId,
				EOptions, Mode) |
	encode_assemble(Display, Intermediate, Encoder, EOptions, Mode,
			Module, Ready
	),
	write_module(Abort, Ready, Module, ServiceId, Done);

    PrecomReply = ok,
    OutputControl = save(Display, Intermediate, Encoder, Module, _, EOptions,
			 Mode) |
	encode_assemble(Display, Intermediate, Encoder, EOptions, Mode,
			Module, Ready
	),
	wait_ready_module(Abort, Ready, Module, Done);

    PrecomReply = ok,
    OutputControl = none(Display, Intermediate, _, []^, _, _, _) : Abort = _ |
	display_intermediate(Display, Intermediate, [], done, Done).

compile_and_output(_, halt, {_, _, [], _, [], _, _, _}^, done^).
compile_and_output(abort, _, {_, _, Intermediate, _, [], _, _, _}^, done^) :-
	unify_without_failure(Intermediate, []).


write_module(_, done, Module, ServiceId, done^) :-
    module(Module) |
	file # put_module(ServiceId, Module).
write_module(_, _, error(Diagnostic), [Name | _], done^) :-
	computation # display(term, Name-not_written(Diagnostic), known(Name)).
write_module(abort, _, _, _, done^).


wait_ready_module(_, done, Module, done^) :-
    known(Module) |
	true.
wait_ready_module(abort, _, _, done^).


encode_assemble(Display, Intermediate, Encoder, EOptions, Mode,
		Module, Ready
) :-

    Encoder = wam : EOptions = _, Mode = _ |
	Encoder # encode(Intermediate, Encoded, Module),
	display_intermediate(Display, Intermediate, Encoded, done, Ready);

    Encoder =\= wam |
	choose_opt(Mode, EOptions, EOptions'),
	Encoder # encode(Intermediate, [module(Module) | EOptions']),
	display_intermediate(Display, Intermediate, [], done, Ready).

choose_opt(Mode, EOs1, EOs2) + (Ctlopt = false, Opt = false) :-

    EOs1 ? Optimize, Optimize = opt(_),
    Opt = false :
      Opt' = true,
      EOs2 ! Optimize |
	self;

    EOs1 ? opt(_),
    Opt =\= false |
	self;

    EOs1 ? Optimize, Optimize = ctlopt,
    Ctlopt = false :
      Ctlopt' = true,
      EOs2 ! Optimize |
	self;

    EOs1 ? ctlopt,
    Ctlopt =\= false |
	self;

    EOs1 ? Other, Other =\= opt(_), Other =\= ctlopt :
      EOs2 ! Other |
	self;

    EOs1 = [], Opt = false, Ctlopt = false,
    Mode = interpret :
      EOs2 = [opt(0), ctlopt] ;

    EOs1 = [], Opt = false, Ctlopt = false,
    Mode = interrupt :
      EOs2 = [opt(0), ctlopt] ;

    EOs1 = [], Opt = false, Ctlopt = false,
    Mode = failsafe :
      EOs2 = [opt(10), ctlopt] ;

    EOs1 = [], Opt = false, Ctlopt = false,
    otherwise : Mode = _,
	EOs2 = [opt(20), ctlopt] ;

    EOs1 = [], Opt(Ctlopt) =\= false(false) : Mode = _,
      EOs2 = [] .


display_intermediate(none, _, _, Done, Done^).
display_intermediate(Display, ModuleProcedures, ModuleCode, Left, Right):-
    otherwise |
	module_procedures_to_clauses(Display, ModuleProcedures, Clauses),
	display_clauses_type(Display, Type),
	computation # shell(display_stream, Clauses,
			    [type(Type), close(Left, Middle)]
	),
	module_code_to_instructions(Display, ModuleCode, Code),
	computation # shell(display_stream, Code,
			    [known(Middle), type(ground), close(Middle, Right)]
	).

display_clauses_type(source, unparse^).
display_clauses_type(_, ground^) :-
    otherwise |
	true.


module_procedures_to_clauses(code, _, []^).
module_procedures_to_clauses(_, module(_Name, Procedures), ['['|Clauses]^) :-
    otherwise |
	procedures_to_list(Procedures, Clauses).

procedures_to_list(Ps, Xs) :-

    Ps ? procedure(Id, Clauses) :
      Xs ! Id |
	clauses_to_list(Clauses, Xs', Xs''),
	procedures_to_list;

    Ps = [] :
      Xs = ']' .


clauses_to_list(As, Xs, Ys) :-

    As ? A :
       Xs ! A' |
	display_clause(A, A'),
	clauses_to_list;

    As = [] :
      Xs = Ys .


display_clause(Internal, Clause) :-

    Internal = {H, {A, T}, B} |
	untuple_string(H, H1),
	comma_list(A, CA),
	comma_list(T, CT),
	comma_list(B, CB),
	display_clause(H1, CA, CT, CB, Clause);

    Internal = {H, G, B},
    otherwise |
	untuple_string(H, H1),
	comma_list(G, CG),
	comma_list(B, CB),
	display_clause(H1, CG, true, CB, Clause).


display_clause(H, true, true, true, H^).
display_clause(H, true, true, B, (H :- B)^) :-
    B =\= true |
	true.
display_clause(H, A, true, B, (H :- A | B)^) :-
    A =\= true |
	true.
display_clause(H, A, T, true, (H :- A : T)^) :-
    T =\= true |
	true.
display_clause(H, A, T, B, (H :- A : T | B)^) :-
    T =\= true, B =\= true |
	true.


comma_list(List, Goals) :-

    List = [Goal] |
	untuple_string(Goal, Goals);

    otherwise,
    List ? Goal :
      Goals = (Goal', Goals') |
	untuple_string(Goal, Goal'),
	comma_list;

    List = [] :
      Goals = true .


untuple_string({String}, String^).
untuple_string(Tuple, Tuple^) :-
    otherwise |
	true.


module_code_to_instructions(Display, ModuleCode, Instructions) :-

    ModuleCode = module(_Name, Code, _Strings),
    Display =\= source, Display =\= intermediate :
      Instructions ! '{' |      
	code_to_list(Code, Instructions', '}');

    otherwise : Display = _, ModuleCode = _,
      Instructions = [] .


code_to_list(Code, Instructions1, Instructions2) :-

    Code = [] :
      Instructions1 = Instructions2 ;

    Code ? procedure(Name, Label, ProcCode) :
      Instructions1 ! procedure(Name, Label) |
	code_to_list(ProcCode, Instructions1', Instructions1''),
	code_to_list;

    otherwise,
    Code ? Atom :
      Instructions1 ! Atom |
	code_to_list.

prepare_controls(	Options, Abort,
			{OptionName, Residue, ServiceId}^,
			{ModuleName, Control, Intermediate, Target, Mode}^,
			{Action, Display, Intermediate, Encoder, Module,
			 ServiceId, EOptions, Mode?
			}^
) :-
	scan_options(	Abort, Options, 
			OptionName, Control, Display, II, MM, TT,
			EOptions, Residue
	),
	choose_module_name(OptionName, ServiceId, ModuleName),
	choose_encoder(TT, Target, Encoder),
	complete_output_control(II, MM, Action, Intermediate, Module).

choose_encoder(fcp, dg^, ndg^).		% Default (new ndg encoder)
choose_encoder(dg, dg^, ndg^).		% Default decision graph encoder
choose_encoder(wam, wam^, wam^).	% Only wam encoder
choose_encoder(cdg, cdg^, cdg^).	% Only cdg encoder
choose_encoder(Other, dg^, Other^) :-	% Other decision graph encoder
    Other =\= fcp, Other =\= dg, Other =\= wam |
	true.

complete_output_control([], [], file^, _, _).
complete_output_control(i(I), [], none^, Intermediate, _) :-
	unify_without_failure(Intermediate, I).
complete_output_control(II, m(M), save^, Intermediate, Module) :-
	unify_without_failure(Module, M),
	complete_output_control(II, [], _, Intermediate, _).
complete_output_control(II, wait(Continue, M), Continue^, Intermediate,
			Module
) :-
	unify_without_failure(Module, M),
	complete_output_control(II, [], _, Intermediate, _).

choose_module_name('_', ServiceId, ModuleName) :-
	ServiceId? = [ModuleName | _].
choose_module_name(OptionName, _, ModuleName) :-
    OptionName =\= '_' :
      ModuleName = OptionName .


scan_options(Abort, Options, Name, Control, Display, II, MM, TT,
		EOptions, Residue
) :-

    Options ? name(String),
    string(String) :
      String = Name, Name' = _ |
	scan_options;

    Options ? control(Path) :
      Control = Path, Control' = _ |
	scan_options;

    Options ? display(Show) :
      Display = Show, Display' = _ |
	scan_options;

    Options ? intermediate(Intermediate) :
      II = i(Intermediate), II' = _ |
	scan_options;

    Options ? module(Module) :
      MM = m(Module), MM' = _ |
	scan_options;

    Options ? Wait, Wait = wait(_, _) :
      MM = Wait, MM' = _ |
	scan_options;

    Options ? target(Target) :
      TT = Target, TT' = _ |
	scan_options;

    Options ? Optimize, 
    Optimize = opt(_) :
      EOptions ! Optimize |
	scan_options;

    Options ? Ctlopt, 
    Ctlopt = ctlopt :
      EOptions ! Ctlopt |
	scan_options;

    Options ? Output, 
    Output = output(_) :
      EOptions ! Output |
	scan_options;

    otherwise,
    Options ? Other :
      Residue ! Other |
	scan_options;

    Options = [] : Abort = _,
      Name = '_',
      Control = control,
      Display = none,
      II = [],
      MM = [],
      TT = fcp,
      Residue = [],
      EOptions = [] ;

    Options =\= [_|_], Options =\= [] : Options' = [Options] |
	scan_options;

    Abort = abort : Options = _,
      Abort' = done, Options' = [] |
	scan_options.
