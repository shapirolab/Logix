-language([evaluate,compound,colon]).
-export([run/2]).
-mode(trust).

AMBIENT_CONTROL => 1.

DEFAULT_LIMIT  => 32000000 .
DEFAULT_SCALE  => 1 .
DEFAULT_FILE   => record .
DEFAULT_FORMAT => none .
NO_FILE => "".

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

transform_terms(Terms, Declarations, Result) + (PartialResult = ok) :-

    known(PartialResult),
    Terms ? Term |
	transform_term(Term, Declarations, Declarations',
			PartialResult, PartialResult'),
	self;

    Terms =?= [] :
      Declarations = [],
      Result = PartialResult.

transform_term(Term, Declarations, NextDeclarations, PartialResult, Result) :-

    Term =?= (VariableList : RunParameters | CallList) :
      Declaration = {{Limit?, Scale?, File?, Format?}, VarList?, GoalList?} |
	transform_parameters(RunParameters, Limit, Scale, File, Format,
					 Errors, VarErrors),
	transform_variables(VariableList, VarList, VarErrors, CallErrors),
	transform_calls(CallList, GoalList, CallErrors, []),
	term_errors;

    otherwise :
      PartialResult = _,
      Declaration = [],
      Errors = [not_a_process_declaration] |
	term_errors.

  term_errors(Term, Declarations, NextDeclarations, PartialResult, Result,
		Errors, Declaration) :-

    Errors =?= [] :
      Term = _,
      Declarations = [Declaration | NextDeclarations],
      Result = PartialResult;

    Errors =\= [] :
      Declaration = _,
      PartialResult = _,
      Declarations = NextDeclarations |
        screen#display(Term, [prefix(input), type(unparse),
			      depth(50), close(Done,ng)]),
        screen#display_stream(Errors,
			      [wait(Done), prefix(error), type(unparse),
			       depth(50),close(Result, Done)]).


transform_parameters(RunParameters, Limit, Scale, File, Format,
		     Errors, NextErrors) :-

	comma_list_to_list(RunParameters, ParameterList),
	transform_parameter_list.

transform_parameter_list(RunParameters, ParameterList,
			 Limit, Scale, File, Format,
			 Errors, NextErrors) :-

    ParameterList =?= [Name(_) | _],
    /* "ln" is the shortest function name. */
    string_length(Name) > 1 |
	validate_expression(Name(`"A"), _, E, []),
	maybe_named_parameter_list;

    otherwise |
	transform_simple_parameters.

  maybe_named_parameter_list(RunParameters, ParameterList,
			     Limit, Scale, File, Format,
			     Errors, NextErrors, E) :-

    E =?= [] |
        transform_simple_parameters;

    E =\= [] :
      RunParameters = _ |
	transform_named_parameter_list.


transform_named_parameter_list(ParameterList, Limit, Scale, File, Format,
			       Errors, NextErrors) :-

    ParameterList = [] |
	Errors = NextErrors,
	unify_without_failure(Limit, DEFAULT_LIMIT),
	unify_without_failure(Scale, DEFAULT_SCALE),
	unify_without_failure(File, DEFAULT_FILE),
	unify_without_failure(Format, DEFAULT_FORMAT);

    ParameterList ? limit(Expression),
    ground(Expression) :
      Limit = Expression |
	self;

    ParameterList ? file(Name),
    ground(Name) :
      File = Name |
	self;

    ParameterList ? scale(Expression),
    ground(Expression) :
      Scale = Expression |
	self;

    ParameterList ? format(none) :
      Format = none |
	self;

    ParameterList ? format(process) :
      Format = process |
	self;

    ParameterList ? format(creator) :
      Format = creator |
	self;

    ParameterList ? format(full) :
      Format = full |
	self;

    ParameterList ? format(ambient) :
      Format = ambient |
	self;

    ParameterList ? format(atrace) :
      Format = atrace |
	self;

    ParameterList ? format(Variable), Variable =?= `_ :
      Format = Variable |
	self;

    ParameterList ? limit(Conflict),
    otherwise :
      Errors ! conflicting_limit(Limit, Conflict) |
	self;

    ParameterList ? scale(Conflict),
    otherwise :
      Errors ! conflicting_scale(Scale, Conflict) |
	self;

    ParameterList ? file(Conflict),
    otherwise :
      Errors ! conflicting_file(File, Conflict) |
	self;

    ParameterList ? format(Conflict),
    otherwise :
      Errors ! conflicting_format(Format, Conflict) |
	self;

    ParameterList ? format(Other),
    otherwise :
      Errors ! invalid_format(Other) |
        self;

    ParameterList ? Other :
      Errors ! invalid_named_parameter(Other) |
	self.

transform_simple_parameters(RunParameters, ParameterList,
			    Limit, Scale, File, Format,
			    Errors, NextErrors) :-

    ParameterList =?= [L] :
      RunParameters = _,
      Limit = L,
      Scale = DEFAULT_SCALE,
      File = NO_FILE,
      Format = DEFAULT_FORMAT,
      Errors = NextErrors;

    ParameterList =?= [L, S],
    number(S) :
      RunParameters = _,
      Limit = L,
      Scale = S,
      File = DEFAULT_FILE,
      Format = DEFAULT_FORMAT,
      Errors = NextErrors;

    ParameterList =?= [L, S],
    tuple(S), S =\= `_ :
      RunParameters = _,
      Limit = L,
      Scale = S,
      File = DEFAULT_FILE,
      Format = DEFAULT_FORMAT,
      Errors = NextErrors;

    ParameterList =?= [L, S, F] :
      RunParameters = _,
      Limit = L,
      File = F,
      Scale = S,
      Format = DEFAULT_FORMAT,
      Errors = NextErrors;

    ParameterList =?= [L, S, F, O] :
      RunParameters = _,
      Limit = L,
      Scale = S,
      File = F,
      Format = O,
      Errors = NextErrors;

    otherwise |
	transform_simple_parameters1.

  transform_simple_parameters1(RunParameters, ParameterList,
			       Limit, Scale, File, Format,
			       Errors, NextErrors) :-

    ParameterList =?= [L, F],
    string(F) :
      RunParameters = _,
      Limit = L,
      File = F,
      Scale = DEFAULT_SCALE,
      Format = DEFAULT_FORMAT |
        simple_real_parameters + (S = DEFAULT_SCALE);

    ParameterList =?= [L, F],
    F =?= `Name, Name =\= "_" :
      RunParameters = _,
      Limit = L,
      File = F,
      Scale = DEFAULT_SCALE,
      Format = DEFAULT_FORMAT |      
	simple_real_parameters + (S = DEFAULT_SCALE);

    ParameterList =\= [_,_] :
      Limit = DEFAULT_LIMIT,
      File = NO_FILE,
      Scale = DEFAULT_SCALE,
      Format = DEFAULT_FORMAT,
      Errors = [invalid_simple_parameter(RunParameters) | NextErrors];

    otherwise :
      ParameterList = _,
      Limit = DEFAULT_LIMIT,
      File = NO_FILE,
      Scale = DEFAULT_SCALE,
      Format = DEFAULT_FORMAT,
      Errors = [invalid_parameters(RunParameters) | NextErrors].

  simple_real_parameters(L, S, Errors, NextErrors) :-

    convert_to_real(L, RL), RL > 0.0,
    convert_to_real(S, RS), RS > 0.0 :
      Errors = NextErrors;

    convert_to_real(L, RL), RL =< 0.0,
    convert_to_real(S, RS), RS > 0.0 :
      Errors = [negative_limit(L) | NextErrors];

    convert_to_real(L, RL), RL > 0.0,
    convert_to_real(S, RS), RS =< 0.0 :
      Errors = [negative_scale(S) | NextErrors];

    convert_to_real(L, RL), RL =< 0.0,
    convert_to_real(S, RS), RS =< 0.0 :
      Errors = [negative_limit(L), negative_scale(S) | NextErrors].

/******************************************************************/

transform_variables(VariableList, VarList, Errors, NextErrors) :-

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

    Names ? Name :
      UniqueName = "_",
      Names' = _,
      NewNames = OldNames,
      Errors = [duplicate_variable_name(Name) | NextErrors];

    Names =?= [] :
      UniqueName = Name,
      NewNames = [Name | OldNames],
      Errors = NextErrors.

transform_calls(CallList, GoalList, Errors, NextErrors) :-

        comma_list_to_list(CallList, CallList'),
        transform_call_list.

transform_call_list(CallList, GoalList, Errors, NextErrors) :-

    CallList = [] :
      GoalList = [],
      Errors = NextErrors;

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
      GoalList ! Repeat |
	transform_repeat_call(Other, Repeat, Errors, Errors'),
	self;
	
    CallList ? BadCall,
    otherwise :
      Errors ! improper_call(BadCall) |
	self.

  transform_repeat_call(Call, Repeat, Errors, NextErrors) :-

    Call =?= _#_ :
      Repeat = Call,
      Errors = NextErrors;

    /** elaborate to allow Integer or Name to be a <pi_variable> **/
    Call = Integer*Name#_Goal, integer(Integer), string(Name) :
      Repeat = Call,
      Errors = NextErrors;
    /*************************************************************/

    Call = Integer*Other, integer(Integer) :
      Repeat = Integer*Repeated |
	comma_list_to_list(Other, RepeatList),
	transform_repeat_list;

    otherwise :
      Errors = [improper_call(Call) | NextErrors],
      Repeat = [].

  transform_repeat_list(RepeatList, Repeated, Errors, NextErrors) :-

    RepeatList ? Goals :
      Repeated ! RepeatCall |
	transform_repeat_call(Goals, RepeatCall, Errors, Errors'),
	self;

    RepeatList =?= [] :
      Repeated = [],
      Errors = NextErrors.

	
run_declarations(Declarations, Stream, Hash, Result) :-

    Result =?= ok,
    Declarations =?= [{_, VarList, _} | _] |
	extract_variables_sets(VarList, [], VarSets, []),
	prepare_run(Declarations, Stream, Hash, VarSets);
	
    Result =?= ok,
    Declarations = [] :
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
    Declarations = [{RunParameters, _, Goals} | _] :
      Hash ! lookup(FileName?, Index, OldIndex, HashReply) |
	bind_variables_expressions(VariablesSet, Triples),
	complete_variable_bindings(Triples, Requests, Evaluates),
	wrap_requests,
	computation # DictReqs?,
	get_file_index,
	run_evaluates,
%		(Done, Evaluates, VOutcome)
	prepare_parameters,
%		(VOutcome, RunParameters, Parameters, FileName, VPOutcome),
	prepare_goals,
%		(VPOutcome, Goals, CompiledGoals, CallGoals, Status),
	run_or_record_goals,
%		(VPOutcome, Parameters, CallGoals, Index,
%		 RunOrRecord, Status, Outcome)
	format_goals,
%		(CompiledGoals, FormattedGoals)
	format_call,
%		(RunOrRecord, CompiledGoals, FormattedCall)
	check_run_outcome;
%		(Declarations, Stream, Hash, VarSets, FormattedCall, Outcome)

    VarSets =?= [],
    Declarations ? _DoneWithIt :
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
		  Status, VOutcome).

prepare_goals(VPOutcome, Goals, CompiledGoals, CallGoals, Status) :-

    VPOutcome =?= done :
      CallGoals = [spi_status#get_status(cutoff_status,Status) | CompiledGoals?] |
	goal_compiler#term(Goals, CompiledGoals);

    VPOutcome =\= done :
      Goals = _,
      CallGoals = [],
      CompiledGoals = [],
      Status = VPOutcome .

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


run_or_record_goals(VPOutcome, Parameters, CallGoals,
		    Index, RunOrRecord, Status, Outcome) :-

    VPOutcome =?= done,
    Parameters =?= {Limit, Scale, _FileName, atrace} :
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
    arg(4, Parameters, Format), Format =\= atrace :
      Run = repeat#run(CallGoals) |
	run_or_record,
	start_run(spi_record#RunOrRecord, Status, Outcome);

    VPOutcome =\= done,
    known(CallGoals) :	% avoid conflicting use of 
      Index = _,
      Parameters = _,
      RunOrRecord = _,
      Status = _,
      Outcome = VPOutcome.

  run_or_record(Run, Parameters, Index, RunOrRecord) :-

    Parameters = {Limit, _Scale, NO_FILE, _Format} :
      Index = _,
      RunOrRecord = run(Run, Limit);

    Index = 0, 
    Parameters = {Limit, Scale, File, Format}, File =\= NO_FILE :
      RunOrRecord = run(Run, File, Limit, Scale, Format);

    Index > 0, 
    Parameters = {Limit, Scale, File, Format}, File =\= NO_FILE :
      RunOrRecord = run(Run, IndexedFile?, Limit, Scale, Format) |
	utils#append_strings([File,"_",Index], IndexedFile).


prepare_parameters(VOutcome, RunParameters, Parameters, FileName, VPOutcome) :-

    VOutcome =?= done :
      Parameters = {L?, S?, FileName?, Format?} |
	goal_compiler#term(RunParameters, {Limit, Scale, File, Format}),
	file_is_string_or_none(Format, File, FileName, E1, E2),
	validate_format(Format, E2, []),
	evaluate_parameters;

    VOutcome =\= done :
      RunParameters = _,
      FileName = "",
      Parameters = {0.0, 1.0, NO_FILE, none},
      VPOutcome = VOutcome.

  file_is_string_or_none(Format, File, FileName, E, NE) :-

    File =?= NO_FILE,
    Format =?= atrace :
      FileName = DEFAULT_FILE,
      E = NE;

    File =?= NO_FILE,
    Format =\= atrace :
      FileName = NO_FILE,
      E = NE;

    File =\= NO_FILE,
    convert_to_string(File, FileName^) :
      Format = _,
      E = NE;

    otherwise :
      Format = _,
      FileName = File,
      E = [file_name_must_be_string(File) | NE].

  evaluate_parameters(E1, Limit, L, Scale, S, VPOutcome) :-

    E1 =\= [] :
      Limit = _,
      Scale = _,
      L = DEFAULT_LIMIT,
      S = DEFAULT_SCALE,
      VPOutcome = failed(parameter_error(E1));

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
      List ! Item |
	self;

    otherwise :
      List = [Items].

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

/************************/

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

/*************************************************************************/

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


monitor_run(Events, FromSub, Status, ExternalEvents, Requests, Outcome) :-

    unknown(Events),
    known(Status) :
      ExternalEvents = _,
      FromSub = _ |
	abort_run,
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

% Do not accept linked from subordinate computation.
%    FromSub ? Linked,
%    Linked = linked(_, _, _) |                  /* from sub-computation */

    ExternalEvents ? aborted :
      Events = _,
      ExternalEvents' = _,
      Status = _,
      FromSub = _,
      Outcome = ng(aborted_run) |
	abort_run;

    ExternalEvents ? Other,
    Other =\= aborted |
	self;

    Events ? Event | /* failed et al ??? */
	handle_computation_event;

    Events =?= [] :
      Events' = _ |
	self.

  handle_status(Status, Outcome) :-

    Status =?= [] :
      Outcome = done;

    Status =?= [Done | _] :
      Outcome = Done;

    Status =\= [_ | _], Status =\= [] :
      Outcome = Status.


handle_computation_event(Events, FromSub, Status, ExternalEvents,
			 Requests, Outcome, Event) :-

    Event =?= failed(Where, What) :
      Events = _,
      ExternalEvents = _,
      FromSub = _,
      FromSub = _,
      Requests = _,
      Status = _ |
	edit_where,
	edit_what,
	abort_run;	% fix to post-analyze

    Event =?= terminated :
      Events = _,
      ExternalEvents = _,
      FromSub = _,
      FromSub = _,
      Requests = _,
      Status = _,
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
