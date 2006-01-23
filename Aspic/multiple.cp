-language([evaluate,compound,colon]).
-export([run/2]).
-mode(failsafe).

AMBIENT_CONTROL => 1.

DEFAULT_LIMIT  => 32000000 .
DEFAULT_SCALE  => 1 .
DEFAULT_FILE   => record .
DEFAULT_FORMAT => none .
NO_FILE => "".

DEFAULT_VARIABLES => [] .
DEFAULT_RUN => {NO_FILE, DEFAULT_LIMIT, DEFAULT_SCALE, DEFAULT_FORMAT} .
DEFAULT_RECORD =>
	       {DEFAULT_FILE, DEFAULT_LIMIT, DEFAULT_SCALE, DEFAULT_FORMAT} .

CHAR_0  =>  48 .
CHAR_9  =>  57 .
CHAR_A  =>  65 .
CHAR_Z  =>  90 .
CHAR_US =>  95 .
CHAR_a  =>  97 .
CHAR_z  => 122 .


run(Parameters, Stream) :-

    string(Parameters),
    writable(Stream) :
      Stream = Stream'? |
	system#parse#string(Parameters, Terms, Errors),
	get_file;

    otherwise :
      Stream = [] |
	fail("not a string" - Parameters);

    otherwise :
      Parameters = _ |
	fail(Stream, unwritable).

get_file(Parameters, Stream, Terms, Errors) :-

    Errors =?= [],
    Terms =?= [Parameters] |
	file#get_file(Parameters, Characters, [chars], Status),
	system#parse#string(Characters, Terms', Errors'),
	parsing_errors;

    otherwise |
	parsing_errors + (Status = true).

parsing_errors(Parameters, Stream, Terms, Errors, Status) :-

    Status =\= true :
      Terms = _,
      Errors = _,
      Stream = [] |
	fail(file(Parameters) - Status);

    Status =?= true,
    Errors =\= [],
    ground(Errors) :
      Parameters = _,
      Terms = _,
      Stream = [] |
	fail("parsing" - Errors);

    Status =?= true,
    Errors =?= [] :
      Parameters = _ |
	transform_terms(Terms, Declarations, Result),
	stream#hash_table(Hash),
	run_declarations.

transform_terms(Terms, Declarations, Result) +
	(PartialResult = ok,
	 Defaults = {DEFAULT_VARIABLES, DEFAULT_RUN}) :-

  known(PartialResult),
  Terms ? Term :
    Declarations ! Declaration? |
	classify_term,
	transform_term(Term, Type?, Defaults, Defaults', Declaration, Errors),
	term_errors(Term, Errors?, PartialResult, PartialResult'),
	self;

    Terms =?= [] :
      Defaults = _,
      Declarations = [],
      Result = PartialResult.


  term_errors(Term, Errors, PartialResult, Result) :-

    Errors =?= [] :
      Term = _,
      Result = PartialResult;

    Errors =\= [] :
      PartialResult = _ |
        screen#display(Term, [prefix(input), type(unparse),
			      depth(50), close(Done,ng)]),
        screen#display_stream(Errors,
			      [wait(Done), prefix(error), type(unparse),
			       depth(50), close(Result, Done)]).


  classify_term(Term, Type) :-

    string(Term), Term =?= true :
      Type = call;

    string(Term),  Term =\= true :
      Type = reset;

    Term =?= (PreProcess | _) |
	classify_process_declaration;

    Term =?= (Term' , _) |
	classify_element,
	complete_default_classification(Type', Type);

    otherwise |
	classify_element,
	complete_default_classification(Type', Type).

    complete_default_classification(reset, error^).
    complete_default_classification(Type, Type^) :-
      otherwise | true.

  classify_process_declaration(PreProcess, Type) :-

    PreProcess =?= true :
      Type = tcall;

    PreProcess =?= (_ : _) :
      Type = full;

    PreProcess =?= (Term , _) |
	classify_element,
	complete_process_classification(Type', Type);

    otherwise |
	classify_element + (Term = PreProcess),
        complete_process_classification(Type', Type).

    complete_process_classification(variables, vcall^).
    complete_process_classification(arguments, acall^) :-
      otherwise |
	true.

  classify_element(Term, Type) :-

    string(Term) :
      Type = reset;

    tuple(Term),
    arg(1, Term, `_) :
      Type = variables;

    tuple(Term),
    arg(1, Term, Functor), Functor =\= `_,
    Functor =\= "=",
    Functor =\= run, Functor =\= record, Functor =\= file,
    Functor =\= limit, Functor =\= scale, Functor =\= format :
      Type = call;

    tuple(Term),
    Term =?= (Name = Value) :
      Term' = Name(Value) |
	self;

    tuple(Term),
    otherwise :
      Type = arguments.


transform_term(Term, Type, Defaults, NewDefaults, Declaration, Errors) :-

    Type =?= full,
    Term =?= (VariableList : RunArguments | CallList) :
      Declaration = {VarList?, {File?, Limit?, Scale?, Format?}, GoalList?},
      Defaults = NewDefaults |
	transform_variables(VariableList, VarList, Errors, ArgErrors),
	transform_arguments + (Errors = ArgErrors, NextErrors = CallErrors),
	transform_calls + (Errors = CallErrors);

    Type =?= vcall,
    Term =?= (VariableList | CallList),
    Defaults =?= {_VarList, ArgumentTuple} :
      Declaration = {VarList?, ArgumentTuple, GoalList?},
      Defaults = NewDefaults |
	transform_variables + (NextErrors = CallErrors),
	transform_calls + (Errors = CallErrors);

    Type =?= acall,
    Term =?= (RunArguments | CallList),
    Defaults =?= {VarList, _ArgumentTuple} :
      Declaration = {VarList, {File?, Limit?, Scale?, Format?}, GoalList?},
      Defaults = NewDefaults |
	transform_arguments + (NextErrors = CallErrors),
	transform_calls + (Errors =  CallErrors);

    Type =?= tcall,
    Term = (true | Term') :
      Type' = call |
	self;

    Type =?= call,
    Defaults =?= {VarList, ArgumentTuple} :
      Declaration = {VarList, ArgumentTuple, GoalList?},
      Defaults = NewDefaults |
	transform_calls + (CallList = Term);

    otherwise :
      Declaration = [] |
	update_defaults.

  update_defaults(Term, Type, Defaults, NewDefaults, Errors) :-

    Type =?= variables,
    Defaults =?= {_VarList, ArgumentTuple} :
      NewDefaults = {VarList?, ArgumentTuple} |
	comma_list_to_list(Term, VariableList),
	transform_variables(VariableList, VarList, Errors, []);

    Type =?= arguments,
    Defaults =?= {VarList, _ArgumentTuple} :
      ArgumentTuple = {File?, Limit?, Scale?, Format?},
      NewDefaults = {VarList, ArgumentTuple} |
	transform_arguments + (RunArguments = Term, NextErrors = []);

    Type =?= reset |
	reset_default;

    Type =?= error :
      Term = _,
      Defaults = NewDefaults,
      Errors = invalid_declaration.

  reset_default(Term, Defaults, NewDefaults, Errors) :-

    Term =?= all :
      Defaults = _,
      Errors = [],
      NewDefaults = {DEFAULT_VARIABLES, DEFAULT_RUN};

    Term =?= run,
    Defaults =?= {VarList, _ArgumentTuple} :
      Errors = [],
      NewDefaults = {VarList, DEFAULT_RUN};

    Term =?= record,
    Defaults =?= {VarList, _ArgumentTuple} :
      Errors = [],
      NewDefaults = {VarList, DEFAULT_RECORD};

    Term =?= variables,
    Defaults =?= {_VarList, ArgumentTuple} :
      Errors = [],
      NewDefaults = {DEFAULT_VARIABLES, ArgumentTuple};

    otherwise :
      Term = _,
      Defaults = NewDefaults,
      Errors = invalid_reset.


transform_arguments(RunArguments, Defaults, File, Limit, Scale, Format,
		     Errors, NextErrors) :-

    RunArguments =?= run(Expression),
    Defaults =?= {_VarList, {File^, _Limit, Scale^, Format^}} |
	validate_expression + (ValidExpression = Limit);

    arg(1, RunArguments, record) |
	utils#tuple_to_dlist(RunArguments, [_record | ArgumentList], []),
	transform_simple_arguments;

    otherwise :
      RunArguments = _ |
	comma_list_to_list(RunArguments, ArgumentList),
	transform_named_argument_list.


transform_named_argument_list(ArgumentList, Defaults,
			      File, Limit, Scale, Format,
			      Errors, NextErrors) :-

    ArgumentList = [],
    Defaults =?= {_VarList, {DFile, DLimit, DScale, DFormat}} :
      Errors = NextErrors |
	unify_without_failure(File, DFile?),
	unify_without_failure(Limit, DLimit?),
	unify_without_failure(Scale, DScale?),
	unify_without_failure(Format, DFormat);

    ArgumentList ? file(Name),
    known(Name) :
      File = Name |
	self;

    ArgumentList ? limit(Expression),
    known(Expression) :
      Limit = Expression |
	self;

    ArgumentList ? scale(Expression),
    ground(Expression) :
      Scale = Expression |
	self;

    ArgumentList ? format(none) :
      Format = none |
	self;

    ArgumentList ? format(process) :
      Format = process |
	self;

    ArgumentList ? format(creator) :
      Format = creator |
	self;

    ArgumentList ? format(full) :
      Format = full |
	self;

    ArgumentList ? format(ambient) :
      Format = ambient |
	self;

    ArgumentList ? format(atrace) :
      Format = atrace |
	self;

    ArgumentList ? format(Variable), Variable =?= `_ :
      Format = Variable |
	self;

    ArgumentList ? file(Conflict),
    otherwise :
      Errors ! conflicting_file(File, Conflict) |
	self;

    ArgumentList ? limit(Conflict),
    otherwise :
      Errors ! conflicting_limit(Limit, Conflict) |
	self;

    ArgumentList ? scale(Conflict),
    otherwise :
      Errors ! conflicting_scale(Scale, Conflict) |
	self;

    ArgumentList ? format(Conflict),
    otherwise :
      Errors ! conflicting_format(Format, Conflict) |
	self;

    ArgumentList ? format(Other),
    otherwise :
      Errors ! invalid_format(Other) |
        self;

    ArgumentList ? Other,
    otherwise :
      Errors ! invalid_named_argument(Other) |
	self.

transform_simple_arguments(RunArguments, Defaults, ArgumentList,
			    File, Limit, Scale, Format,
			    Errors, NextErrors) :-

    Defaults =?= {_VarList, {_File, Limit^, Scale^, Format^}},
    ArgumentList =?= [F] :
      RunArguments = _,
      File = F,
      Errors = NextErrors;

    Defaults =?= {_VarList, {_DFile, _DLimit, DScale, DFormat}},
    ArgumentList =?= [F, L] :
      RunArguments = _,
      File = F,
      Limit = L,
      Scale = DScale,
      Format = DFormat,
      Errors = NextErrors;

    Defaults =?= {_VarList, {_DFile, _DLimit, _DScale, DFormat}},
    ArgumentList =?= [F, L, S] :
      RunArguments = _,
      File = F,
      Limit = L,
      Scale = S,
      Format = DFormat,
      Errors = NextErrors;

    ArgumentList =?= [F, L, S, O] :
      Defaults = _,
      RunArguments = _,
      File = F,
      Limit = L,
      Scale = S,
      Format = O,
      Errors = NextErrors;

    Defaults =?= {_VarList, {File^, Limit^, Scale^, Format^}},
    otherwise :
      ArgumentList = _,
      Errors = [invalid_arguments(RunArguments) | NextErrors].

/******************************************************************/

transform_variables(VariableList, VarList, Errors, NextErrors) :-

    VariableList =?= [] :
      VarList = [],
      Errors = NextErrors;

    list(VariableList) |
	transform_variable_list + (Names = []);

    otherwise |
	comma_list_to_list(VariableList, VariableList'),
	transform_variable_list + (Names = []).

  transform_variable_list(VariableList, Names, VarList, Errors, NextErrors) :-

    VariableList =?= [] :
      Names = _,
      VarList = [],
      Errors = NextErrors;

    VariableList ? Element,
    tuple(Element), Element =\= `_,
    arity(Element) > 1, arg(1, Element, `Name) :
      VarList ! UniqueName?(ValidExpressionList?) |
	validate_variable_name(Name, ValidName, Errors, Errors'),
	unique_name(ValidName, UniqueName, Names, Names', Errors', Errors''),
	utils#tuple_to_dlist(Element, [_ | ExpressionList], []),
	validate_expression_list(ExpressionList, ValidExpressionList,
				 Errors'', Errors'''),
	self;

    VariableList ? Element,
    otherwise :
      Errors ! improper_variable_list_element(Element) |
	self.

  unique_name(Name, UniqueName, Names, NewNames, Errors, NextErrors)
		+ (OldNames = Names) :-

    Names ? Other, Other =\= Name |
	self;

    Names ? Name,
    Name =\= "_" :
      UniqueName = "_",
      Names' = _,
      NewNames = OldNames,
      Errors = [duplicate_variable_name(Name) | NextErrors];

    Names ? Name,
    Name =?= "_" |
      self;

    Names =?= [] :
      UniqueName = Name,
      NewNames = [Name | OldNames],
      Errors = NextErrors.

transform_calls(CallList, GoalList, Errors) :-

        comma_list_to_list(CallList, CallList'),
        transform_call_list.

transform_call_list(CallList, GoalList, Errors) :-

    CallList = [] :
      GoalList = [],
      Errors = [];

    CallList ? Evaluation,
    Evaluation =?= (Variable := Expression),
    Variable =?= `Name :
      GoalList ! utils#evaluate(ValidExpression, `ValidName) |
	validate_variable_name(Name, ValidName, Errors, Errors'),
	validate_expression(Expression, ValidExpression,
				Errors', Errors''),
	self;

    CallList ? (NonVariable := Expression),
    NonVariable =\= `_ :
      Errors ! invalid_assigned_variable(NonVariable) |
	validate_expression(Expression, _ValidExpression,
				Errors, Errors'),
	self;

    CallList ? Other,
    Other =\= (_ := _),
    otherwise :
      GoalList ! Other |
	self.

	
run_declarations(Declarations, Stream, Hash, Result) :-

    Result =?= ok,
    Declarations =?= [{VarList, _, _} | _] |
	extract_variables_sets(VarList, [], VarSets, []),
	prepare_run(Declarations, Stream, Hash, VarSets);

    Result =?= ok,
    Declarations ? [] |
	self;
	
    Result =?= ok,
    Declarations =?= [] :
      Hash = [],
      Stream = [];

    Result =\= ok :
      Hash = [],
      Stream = [] |
	screen#display_stream(Declarations,
		[prefix(undone), type(unparse), depth(20)]).


extract_variables_sets(VarList, SetStack, StackLeft, StackRight) :-

    VarList ? VariableName(ValueList) |
        extract_variables_subset;

    VarList =?= [] :
      StackLeft = [SetStack | StackRight].

  extract_variables_subset(VarList, SetStack, StackLeft, StackRight,
                     VariableName, ValueList) :-

    ValueList ? Value :
      SetStack' = [VariableName(Value) | SetStack] |
        extract_variables_sets(VarList, SetStack', StackLeft, StackRight'),
        extract_variables_subset(VarList, SetStack, StackRight', StackRight,
                                VariableName, ValueList');

    ValueList =?= [] :
      VarList = _,
      SetStack = _,
      VariableName = _,
      StackLeft = StackRight.

prepare_run(Declarations, Stream, Hash, VarSets) :-

    VarSets ? VariablesSet,
    Declarations =?= [{_, ArgumentTuple, Goals} | _] :
      Hash ! lookup(FileName?, Index, OldIndex, HashReply) |
	bind_variables_expressions(VariablesSet, Triples),
	complete_variable_bindings(Triples, Requests, Evaluates),
	wrap_requests,
%		(Requests, DictReqs, Done)
	computation # DictReqs?,
	get_file_index,
%		(HashReply, Index, OldIndex)
	run_evaluates,
%		(Done, Evaluates, VOutcome)
	prepare_arguments,
%		(VOutcome, ArgumentTuple, Arguments, FileName, VPOutcome),
	prepare_goals,
%		(VPOutcome, Goals, CompiledGoals, CallGoals, Status),
	run_or_record_goals,
%		(VPOutcome, Arguments, CallGoals, Index,
%		 RunOrRecord, Status, Outcome)
	format_goals,
%		(CompiledGoals, FormattedGoals)
	format_call,
%		(RunOrRecord, CompiledGoals, FormattedCall)
	check_run_outcome;
%		(Declarations, Stream, Hash, VarSets, FormattedCall, Outcome)

    VarSets =?= [],
    Declarations ? _Declaration :
      Result = ok |
	run_declarations.
    

run_evaluates(Done, Evaluates, VOutcome) :-

    known(Done),
    Evaluates =?= [] :
      VOutcome = done;

    known(Done),
    Evaluates =\= [] |
	start_run([utils#Evaluates, 
		   spi_status#get_status(cutoff_status,Status)],
		  Status, Outcome),
	evaluates_outcome(Outcome, Evaluates, VOutcome).

  evaluates_outcome(done, _, done^).
  evaluates_outcome(Other, [Evaluates], ng(incomplete(Evaluates))^) :-
    Other =\= done | true.
  evaluates_outcome(_Other, Evaluates, ng(incomplete(Evaluates))^) :-
    otherwise | true.


prepare_goals(VPOutcome, Goals, CompiledGoals, CallGoals, Status) :-

    VPOutcome =?= done :
      CallGoals = [spi_status#get_status(cutoff_status,Status)
		  | CompiledGoals?] |
	goal_compiler#term(Goals, CompiledGoals);

    VPOutcome =\= done :
      Goals = _,
      CallGoals = [],
      Status = VPOutcome |
	goal_compiler#term(Goals, CompiledGoals).

  get_file_index(HashReply, Index, OldIndex) :-

    HashReply = new :
      OldIndex = _,
      Index = 0;

    HashReply = old,
    OldIndex++ :
      Index = OldIndex'.

  wrap_requests(Requests, DictReqs, Done) :-

    Requests ? R :
      DictReqs ! dictionary(R) |
        self;

    Requests = [] :
      DictReqs = [dictionary(find([], _, Done))] .

format_goals(CompiledGoals, FormattedGoals) :-

    CompiledGoals =?= [Goal] :
      FormattedGoals = Goal;

    otherwise :
      FormattedGoals = CompiledGoals.

format_call(RunOrRecord, FormattedGoals, FormattedCall) :-

    RunOrRecord =?= run(_, Limit),
    real(Limit) >= real(DEFAULT_LIMIT) :
      FormattedCall = run(FormattedGoals);

    RunOrRecord =?= run(_, Limit),
    otherwise :
      FormattedCall = run(FormattedGoals, Limit);

    RunOrRecord =?= run(_, FileName, Limit, Scale, DEFAULT_FORMAT) |
	format_record + (Functor = record);

    RunOrRecord =?= run(_, FileName, Limit, Scale, atrace) |
	format_record + (Functor = list);

    RunOrRecord =?= run(_, FileName, Limit, Scale, Format),
    Format =\= DEFAULT_FORMAT, Format =\= atrace :
      FormattedCall = run(FormattedGoals, FileName, FormattedLimit, Scale,
			     Format) |
	format_limit.

  format_record(Functor, FormattedGoals, FileName, Limit, Scale,
		FormattedCall) :-

    real(Limit) =:= real(DEFAULT_LIMIT),
    real(Scale) =:= real(DEFAULT_SCALE) :
      FormattedCall = Functor(FormattedGoals, FileName);

    L := real(Limit),
    DL := real(DEFAULT_LIMIT),
    L =\= DL,
    real(Scale) =:= real(DEFAULT_SCALE) :
      FormattedCall = Functor(FormattedGoals, FileName, Limit);

    real(Limit) >= real(DEFAULT_LIMIT),
    S := real(Scale),
    DS := real(DEFAULT_SCALE),
    S =\= DS :
      FormattedCall = Functor(FormattedGoals, FileName, "*", Scale);

    otherwise :
      FormattedCall = Functor(FormattedGoals, FileName, Limit, Scale).

  format_limit(Limit, FormattedLimit) :-

    L := real(Limit),
    DL := real(DEFAULT_LIMIT),
    L =?= DL :
      FormattedLimit = "*";

    otherwise :
      Limit = FormattedLimit.


check_run_outcome(Declarations, Stream, Hash, VarSets,
		  FormattedCall, Outcome) :-

    Outcome =?= ng(What),
    Declarations ? _ :
      VarSets = _,
      Result = ng,
      Stream ! (What <- FormattedCall) |
	run_declarations;

    arg(1, Outcome, failed),
    Declarations ? _ :
      VarSets = _,
      Result = ng,
      Stream ! (Outcome <- FormattedCall) |
	run_declarations;

    otherwise :
      Stream ! (Outcome <- FormattedCall) |
	prepare_run.


run_or_record_goals(VPOutcome, Arguments, CallGoals,
		    Index, RunOrRecord, Status, Outcome) :-

    VPOutcome =?= done,
    Arguments =?= {_FileName, Limit, Scale, atrace} :
      Run = _,
      RunOrRecord = run(CallGoals, IndexedFile, Limit, Scale, atrace) |
	run_or_record,
	computation#events(ExternalEvents),
	computation # self # service_id(SId),
	start_computation([ambient_list#run(repeat#run(CallGoals),
					    IndexedFile?, Limit, Scale)
			  | Requests?], Events, FromSub, SId),
	monitor_run(Events?, FromSub?, Status?, ExternalEvents?,
		    Requests, Outcome);

    VPOutcome =?= done,
    arg(4, Arguments, Format), Format =\= atrace :
      Run = repeat#run(CallGoals) |
	run_or_record,
	start_run(spi_record#RunOrRecord, Status, Outcome);

    VPOutcome =\= done,
    known(CallGoals) :
      Index = _,
      Arguments = _,
      Status = _,
      Outcome = VPOutcome,
      RunOrRecord = run(CallGoals, DEFAULT_LIMIT).

  run_or_record(Run, Arguments, Index, RunOrRecord) :-

    Arguments = {NO_FILE, Limit, _Scale, _Format} :
      Index = _,
      RunOrRecord = run(Run, Limit);

    Index = 0, 
    Arguments = {File, Limit, Scale, Format}, File =\= NO_FILE :
      RunOrRecord = run(Run, File, Limit, Scale, Format);

    Index > 0, 
    Arguments = {File, Limit, Scale, Format}, File =\= NO_FILE :
      RunOrRecord = run(Run, IndexedFile?, Limit, Scale, Format) |
	utils#append_strings([File,"_",Index], IndexedFile).


prepare_arguments(VOutcome, ArgumentTuple, Arguments, FileName, VPOutcome) :-

    VOutcome =?= done :
      Arguments = {FileName?, L?, S?, Format?} |
	goal_compiler#term(ArgumentTuple, {File, Limit, Scale, Format}),
	file_is_string_or_none(ArgumentTuple, File, FileName, E1, E2),
	validate_format(Format, E2, []),
	evaluate_arguments;

    VOutcome =\= done :
      ArgumentTuple = _,
      FileName = NO_FILE,
      Arguments = {NO_FILE, 0.0, 1.0, none},
      VPOutcome = VOutcome.

  file_is_string_or_none(ArgumentTuple, File, FileName, E, NE) :-

    File =\= NO_FILE,
    convert_to_string(File, FileName^) :
      ArgumentTuple = _,
      E = NE;

    File =?= NO_FILE,
    arg(3, ArgumentTuple, DEFAULT_SCALE),
    arg(4, ArgumentTuple, DEFAULT_FORMAT) :
      FileName = NO_FILE,
      E = NE;

    File =?= NO_FILE,
    otherwise :
      ArgumentTuple = _,
      FileName = DEFAULT_FILE,
      E = NE;

    otherwise :
      ArgumentTuple = _,
      FileName = File,
      E = [file_name_must_be_string(File) | NE].

  evaluate_arguments(E1, Limit, L, Scale, S, VPOutcome) :-

    E1 =\= [] :
      Limit = _,
      Scale = _,
      L = DEFAULT_LIMIT,
      S = DEFAULT_SCALE,
      VPOutcome = failed(argument_error(E1));

    E1 = [],
    convert_to_real(Limit, Limit'),
    convert_to_real(Scale, Scale') :
      L = Limit',
      S = Scale',
      VPOutcome = done;

    E1 = [],
    otherwise |
	start_run(computation#[utils#[evaluate(Limit, L),
				      evaluate(Scale, S)],
		   spi_status#get_status(cutoff_status, Status)],
		  Status, VPOutcome).


bind_variables_expressions(VariablesSet, Evaluated)
			+ (List = Triples, Triples) :-

	/* loop over {V, E}
		compile(E -> Et)
		if E is `_ or E is ?_, or E is constant =\= "random", 
			then unify Vt = Et
			else produce utils#evaluate(Et, Vt) 
		produce  {V, Vt}
           loop over {V, Vt}
		unbind V
		add V,Vt  (reply should be new)
	*/

    VariablesSet ? {Name, random} :
      List ! evaluate(Name, Binding?) |
	utils#evaluate(random, Binding),	
	self;

    VariablesSet ? {Name, Binding},
    constant(Binding), Binding =\= random :
      List ! constant(Name, Binding) |
	self;

    VariablesSet ? {Name, Binding},
    [] @< Binding, Binding =\= [_|_], Binding =\= `_ :
      List ! Kind?(Name, Variable) |
	goal_compiler#term({`"_",Binding,Binding}, Value),
	Value? = Variable(A,B),
	synchronize_binding(VariablesSet', Evaluated, List', Triples,
			    {A,B}, Binding',
			    evaluate(Binding', Variable), Kind);

    VariablesSet ? {Name, `"_"} :
      List ! undefined(Name, _) |
	self;

    VariablesSet ? {Name, Binding},
    otherwise :
      List ! Kind?(Name, Binding') |
	goal_compiler#term({Binding,Binding}, Value),
	synchronize_binding(VariablesSet', Evaluated, List', Triples,
			    Value, Binding', compound, Kind);

    VariablesSet =?= [] :
      List = [],
      Evaluated = Triples.

  synchronize_binding(VariablesSet, Evaluated, List, Triples,
		      {Binding,Binding}, Binding^, Kind, Kind^) :-
	bind_variables_expressions(VariablesSet, Evaluated, List, Triples).

complete_variable_bindings(VariablesSet, Requests, Evaluates) :-

    VariablesSet ? Kind(Name, Binding) :
      Requests = [unbind(Name), add(Name, Binding, new) | Requests'] |
	add_goal(Kind, Evaluates, Evaluates'),
	self;

    VariablesSet = [] :
      Requests = [],
      Evaluates = [].

  add_goal(Kind, List, Tail) :-

    Kind = evaluate(_, _) :
      List = [utils#Kind | Tail];

    otherwise :
      Kind = _,	% constant or compound (list or variable)
      List = Tail.
      
/*************************** Utilities *********************************/

comma_list_to_list(Items, List) :-

    Items = (Item, Items') :
      List ! Element? |
	item_to_element,
	self;

    otherwise :
      List = [Element?] |
	item_to_element(Items, Element).

  item_to_element(Item, Element) :-

    Item =?= (Name = Value) :
      Element = Name(Value);

    otherwise :
      Element = Item.

validate_expression_list(Expressions, ValidExpressions, Errors, NextErrors) :-


    /* Special case to undefine the named variable */
    Expressions ? `"_" :
      ValidExpressions ! `"_" |
	self;

    Expressions ? Expression,
    [] @< Expression, Expression =\= [_|_], Expression =\= `"_" :
      ValidExpressions ! ValidExpression? |
	validate_expression(Expression, ValidExpression, Errors, Errors'),
	self;

    Expressions ? LogixElement,
    otherwise :
      ValidExpressions ! LogixElement |   % constant | list | variable
	self;

    Expressions = [] :
      ValidExpressions = [],
      Errors = NextErrors.

validate_expression(Expression, ValidExpression, Errors, NextErrors) :-

    number(Expression) :
      ValidExpression = Expression,
      Errors = NextErrors;

    string(Expression),
    convert_to_real(Expression, E) :
      ValidExpression = E,
      Errors = NextErrors;

    Expression = `Name :
      ValidExpression = `ValidName |
	validate_variable_name;

    Expression =?= E1 + E2 |
        validate_expression(E1, VE1, Errors, Errors'),
        validate_expression(E2, VE2, Errors', NextErrors),
        evaluate_binary_operation;

    Expression =?= E1 - E2 |
	validate_expression(E1, VE1, Errors, Errors'),
	validate_expression(E2, VE2, Errors', NextErrors),
	evaluate_binary_operation;

    Expression =?= E1 * E2 |
        validate_expression(E1, VE1, Errors, Errors'),
        validate_expression(E2, VE2, Errors', NextErrors),
	evaluate_binary_operation;

    Expression =?= E1 / E2 |
        validate_expression(E1, VE1, Errors, Errors'),
        validate_expression(E2, VE2, Errors', Errors''),
	validate_nonzero_operand_2;

    Expression =?= E1 \ E2 |
        validate_expression(E1, VE1, Errors, Errors'),
        validate_expression(E2, VE2, Errors', Errors''),
	validate_nonzero_operand_2;

    Expression =?= E1 /\ E2 |
        validate_expression(E1, VE1, Errors, Errors'),
        validate_expression(E2, VE2, Errors', Errors''),
	validate_integer_operands;

    Expression =?= E1 \/ E2 |
        validate_expression(E1, VE1, Errors, Errors'),
        validate_expression(E2, VE2, Errors', Errors''),
	validate_integer_operands;

    Expression =?= +E |
	validate_expression(E, VE, Errors, NextErrors),
	evaluate_unary_operation;

    Expression =?= -E |
        validate_expression(E, VE, Errors, NextErrors),
	evaluate_unary_operation;

    Expression =?= ~E |
        validate_expression(E, VE, Errors, Errors'),
	validate_integer_operand;

/** Put Functions here **/

    Expression =?= min(E1, E2) |
        validate_expression(E1, VE1, Errors, Errors'),
        validate_expression(E2, VE2, Errors', NextErrors),
	evaluate_binary_operation;

    Expression =?= max(E1, E2) |
        validate_expression(E1, VE1, Errors, Errors'),
        validate_expression(E2, VE2, Errors', NextErrors),
	evaluate_binary_operation;

    Expression =?= pow(E1, E2) |
        validate_expression(E1, VE1, Errors, Errors'),
        validate_expression(E2, VE2, Errors', NextErrors),
	evaluate_binary_operation;

    Expression =?= log(E1, E2) |
        validate_expression(E1, VE1, Errors, Errors'),
        validate_expression(E2, VE2, Errors', Errors''),
	validate_nonzero_operand_2;

    Expression =?= abs(E) |
        validate_expression(E, VE, Errors, NextErrors),
	evaluate_unary_operation;

    Expression =?= round(E) |
        validate_expression(E, VE, Errors, NextErrors),
	evaluate_unary_operation;

    Expression =?= real(E) |
        validate_expression(E, VE, Errors, NextErrors),
	evaluate_unary_operation;

    Expression =?= sin(E) |
        validate_expression(E, VE, Errors, NextErrors),
	evaluate_unary_operation;

    Expression =?= cos(E) |
        validate_expression(E, VE, Errors, NextErrors),
	evaluate_unary_operation;

    Expression =?= tan(E) |
        validate_expression(E, VE, Errors, Errors'),
	validate_nonzero_operand_1;

    Expression =?= sqrt(E) |
        validate_expression(E, VE, Errors, NextErrors),
	evaluate_unary_operation;

    Expression =?= asin(E) |
        validate_expression(E, VE, Errors, NextErrors),
	evaluate_unary_operation;

    Expression =?= acos(E) |
        validate_expression(E, VE, Errors, NextErrors),
	evaluate_unary_operation;

    Expression =?= atan(E) |
        validate_expression(E, VE, Errors, NextErrors),
	evaluate_unary_operation;

    Expression =?= exp(E) |
        validate_expression(E, VE, Errors, NextErrors),
	evaluate_unary_operation;

    Expression =?= ln(E) |
        validate_expression(E, VE, Errors, Errors'),
	validate_nonzero_operand_1;

    Expression =?= random :
      ValidExpression = Expression,
      Errors = NextErrors;

    Expression = length(String),
    string(String),
    string_length(String, VE) :
      ValidExpression = VE,
      Errors = NextErrors;

    Expression =?= length(Variable),
    Variable =?= `_ :
      ValidExpression = Expression,
      Errors = NextErrors;

    otherwise :
      ValidExpression = `"_",
      Errors = [invalid_expression(Expression) |NextErrors].

  evaluate_unary_operation(Expression, ValidExpression, VE) :-

    arg(1, Expression, Operator),
    number(VE) |
	utils#evaluate({Operator, VE}, ValidExpression);

    arg(1, Expression, Operator),
    otherwise :
      ValidExpression = {Operator, VE}.

  evaluate_binary_operation(Expression, ValidExpression, VE1, VE2) :-

    arg(1, Expression, Operator),
    number(VE1), number(VE2) |
	utils#evaluate({Operator, VE1, VE2}, ValidExpression);

    arg(1, Expression, Operator),
    otherwise :
      ValidExpression = {Operator, VE1, VE2}.

  validate_integer_operand(Expression, ValidExpression, VE,
				Errors, NextErrors) :-

    arg(1, Expression, Operator),
    integer(VE) :
      Errors = NextErrors |
	utils#evaluate({Operator, VE}, ValidExpression);

    otherwise,
    number(VE) :
      ValidExpression = `"_",
      Errors = [operand_must_be_an_integer(Expression) | NextErrors];

    arg(1, Expression, Operator),
    otherwise :
      Expression = _,
      ValidExpression = {Operator, VE},
      Errors = NextErrors.

  validate_integer_operands(Expression, ValidExpression, VE1, VE2,
                                Errors, NextErrors) :-

    arg(1, Expression, Operator),
    integer(VE1),
    integer(VE2) :
      Errors = NextErrors |
        utils#evaluate({Operator, VE1, VE2}, ValidExpression);

    otherwise,
    number(VE1),
    number(VE2) :
      ValidExpression = `"_",
      Errors = [operands_must_be_integers(Expression) | NextErrors];

    arg(1, Expression, Operator),
    otherwise :
      Expression = _,
      ValidExpression = {Operator, VE1, VE2},
      Errors = NextErrors.

  validate_nonzero_operand_1(Expression, ValidExpression, VE,
				Errors, NextErrors) :-

    convert_to_real(VE, RVE),
    convert_to_real(0, Zero),
    RVE =:= Zero :
      ValidExpression = `"_",
      Errors = [operand_must_not_be_zero(Expression) | NextErrors];

    otherwise :
      Errors = NextErrors |
	evaluate_unary_operation.

  validate_nonzero_operand_2(Expression, ValidExpression, VE1, VE2,
                                Errors, NextErrors) :-

    convert_to_real(VE2, RVE2),
    convert_to_real(0, Zero),
    RVE2 =:= Zero :
      VE1 = _,
      ValidExpression = `"_",
      Errors = [second_operand_must_not_be_zero(Expression) | NextErrors];

    otherwise :
      Errors = NextErrors |
	evaluate_binary_operation.


  validate_format(Format, Errors, NextErrors) :-

    Format =?= none :
      Errors = NextErrors;

    Format =?= process :
      Errors = NextErrors;

    Format =?= creator :
      Errors = NextErrors;

    Format =?= full :
      Errors = NextErrors;

    Format =?= ambient :
      Errors = NextErrors;

    Format =?= atrace :
      Errors = NextErrors;

    otherwise :
      Errors = [invalid_format(Format) | NextErrors].


validate_variable_name(Name, ValidName, Errors, NextErrors) :-

    string(Name), Name =\= "" :
      N = 1 |
	validate_variable_name_characters;

    Name = "" :
      ValidName = "_",
      Errors = [empty_variable_name | NextErrors];

    otherwise :
      ValidName = "_",
      Errors = [variable_name_is_not_a_string(Name) | NextErrors].

  validate_variable_name_characters(Name, ValidName, Errors, NextErrors, N) :-

    N =< string_length(Name),
    nth_char(N, Name, C),
    N++,
    CHAR_A =< C, C =< CHAR_Z |
	self;

    1 < N, N =< string_length(Name),
    nth_char(N, Name, C),
    N++,
    CHAR_a =< C, C =< CHAR_z |
	self;

    1 < N, N =< string_length(Name),
    nth_char(N, Name, C),
    N++,
    CHAR_0 =< C, C =< CHAR_9 |
	self;

    1 < N, N =< string_length(Name),
    nth_char(N, Name, C),
    N++,
    C =?= CHAR_US |
	self;

    N > string_length(Name) :
      ValidName = Name,
      Errors = NextErrors;

    otherwise :
      ValidName = "_",
      Errors = [invalid_variable_name_character(N, in(Name)) | NextErrors].

/****************************** Computations *******************************/

start_run(Run, Status, Outcome) :-

	computation#events(ExternalEvents),
	computation # self # service_id(SId),
	start_computation([ambient_server#run(Run) | Requests?],
			  Events, FromSub, SId),
	monitor_run(Events?, FromSub?, Status?, ExternalEvents?,
		    Requests, Outcome).

start_computation(Requests0, Events, FromSub, SId) :-

    ground(SId) :
      make_channel(FromSubCH, FromSub),
      CCC = {[], done, _Done, FromSubCH},
      Requests ! change_scope(SId, _Ok),
      Requests' = Requests0 |
	computation # "_domain"(domain_channel(Domain)),
	computation_server # computation(Requests?, CCC, Domain?, Events).


monitor_run(Events, FromSub, Status, ExternalEvents, Requests, Outcome)
		+ (Idle = _, Timeout = _) :-

    unknown(Events),
    known(Status) :
      Timeout = _ |
	handle_status;

% Ignore requests from subordinate computation.
    FromSub ? request(_From, _Event, Latch, Latch^) |
        self;

    FromSub ? filter(FromSub'', FromSub') |
        self;

    FromSub ? delegated([], CCC),
    CCC = {_, Left, Right, _} :
      Left = Right |
        self;

    FromSub ? delegated(Message, CCC),
    Message =\= [],
    CCC = {_, Left, Right, _} :
      Left = Right |
        computation # Message,
        self;

% Do not accept links from subordinate computation.
%    FromSub ? Link,
%    Link =?= link(Name, _, _)

% Do not accept linked from subordinate computation.
%    FromSub ? Linked,
%    Linked = linked(_, _, _)

    ExternalEvents ? aborted :
      Events = _,
      ExternalEvents' = _,
      FromSub = _,
      Idle = _,
      Status = _,
      Timeout = _,
      Outcome = ng(aborted_run) |
	abort_run;

    ExternalEvents ? Other,
    Other =\= aborted |
	self;

    Events ? Event | /* failed et al ??? */
	handle_computation_event;

    Events =?= [] :
      Events' = _ |
	self;

    known(Timeout) :
      Events = _,
      ExternalEvents = _,
      FromSub = _,
      Status = _,
	Outcome = Idle |
	abort_run.

handle_status(Events, FromSub, Status, ExternalEvents, Requests, Outcome,
		Idle) :-

    Status = [] :
      Events = _,
      ExternalEvents = _,
      FromSub = _,
      Idle = _,
      Outcome = done |
	abort_run;

    Status ? Idle', Idle' =?= idle(_) :
      Idle = _ |
	processor # machine(idle_wait(Timeout), _Ok),
	monitor_run;

    Status =?= [Status' | _], Status' =\= idle(_) |
	self;

    Status =\= [_ | _], Status =\= [] :
      Events = _,
      ExternalEvents = _,
      FromSub = _,
      Idle = _,
      Outcome = Status |
	abort_run.

handle_computation_event(Events, FromSub, Status, ExternalEvents,
			 Requests, Outcome, Event, Timeout, Idle) :-

    Event =?= failed(Where, What) :
      Events = _,
      ExternalEvents = _,
      FromSub = _,
      FromSub = _,
      Idle = _,
      Status = _,
      Timeout = _ |
	edit_where,
	edit_what,
	abort_run;	% fix to post-analyze

    Event =?= terminated :
      Events = _,
      ExternalEvents = _,
      FromSub = _,
      FromSub = _,
      Idle = _,
      Status = _,
      Timeout = _,
      Outcome = done |
	abort_run;

    otherwise :
      Event = _ |
	monitor_run.

  edit_where(Where, EditedWhere) :-

    Where =?= List#Goal,
    List ? Module :
      List' = _,
      Where' = Module#Goal |
	self;

    Where =?= Prefix#(Goal@public(1)) :
      Where' = Prefix#Goal |
	self;

    Where =?= Prefix#(Goal@Ambient),
    Ambient =\= public(1) :
      Where' = Prefix#Goal@Ambient |
	self;

    Where =?= _Prefix#Suffix#Goal@Ambient,
    Ambient =\= public(1) :
      Where' = Suffix#Goal@Ambient |
	self; 

    Where =?= _Prefix#Suffix#Goal :
      Where' = Suffix#Goal |
	self; 

    Where =?= List#Goal@Ambient,
    Ambient =\= public(1),
    List ? Module :
      List' = _,
      Where' = Module#Goal@Ambient |
	self;

    otherwise :
      EditedWhere = Where.

  edit_what(What, EditedWhere, Outcome) :-

    What =?= failed :
      Outcome = failed(EditedWhere);

    otherwise :
      Outcome = failed(EditedWhere, What).

abort_run(Requests) :-
      Requests = [abort].

wait(A,B) :- A =?= B | wait(_, ok-B).
