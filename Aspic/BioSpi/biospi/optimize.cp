-export([initialize/3, procedures/4]).
-language([evaluate, compound, colon]).
%-mode(failsafe).

-include(spi_constants).
-include(bio_constants).

initialize(In, ExportIds, Accessible) :-

	stream#hash_table(Procedures),
	export_names,
	serve.

export_names(ExportIds, Exports) :-

    ExportIds ? (Name/_Arity) :
      Exports ! Name |
	self;

    ExportIds ? Name,
    string(Name) :
      Exports ! Name |
	self;

    ExportIds = [] :
      Exports = [].


serve(In, Procedures, Exports, Accessible) :-

    In ? procedure(Notes, Arity, LHS, Value),
    arg(1, LHS, Name) :
      Procedures ! lookup(Name, Value, Old, Status) |
	procedure(Notes, Arity, LHS, Value, Old, Status),
	self;

    In = [] |
	stream#hash_table(Called),
	called(Exports, Called, Accessible, Procedures, []).

called(Exports, Called, NextCalled, Procedures, NextProcedures) :-

    Exports ? Name :
      Procedures ! member(Name, Value, Ok),
      Called ! lookup(Name, New, Old, Found) |
	is_called(Ok, Found, Value, New, Old, Exports', Exports''),
	self;

    Exports = [] :
      NextCalled = Called,
      Procedures = NextProcedures.


is_called(false, new, _Value, {[], []}^, _Old, Exports, Exports^).
is_called(_Member,  old, _Value, Old^, Old, Exports, Exports^).
is_called(true, new, Value, New, _Old, Exports, AugmentedExports) :-
    Value = {Calls, _Variables} :
      New = Value |
	utilities#concatenate_lists([Calls, Exports], AugmentedExports).

procedure(Notes, Arity, LHS, Value, Old, Status) :-

    Status = old :
      Notes = _,
      Arity = _,
      LHS = _,
      Value = Old;

    Status =\= old,
    Index := Arity + 1 : /* new */
      Old = _,
      Value = {Calls''?, Variables''?} |
	note_parameters(Index, LHS, Parameters),
	serve_procedure(Notes, Calls, Variables),
	utilities#concatenate_lists(Calls, Calls'),
	utils#binary_sort_merge(Calls'?, Calls''),
	utilities#concatenate_lists([Parameters | Variables], Variables'),
	utils#binary_sort_merge(Variables'?, Variables'').

note_parameters(Arity, LHS, Parameters) :-

    Arity-- > 1,
    arg(Arity, LHS, Variable),
    Variable = `String, string(String) :
      Parameters ! Variable |
	self;

    Arity > arity(LHS),
    arity(LHS, Arity') |
	self;

    Arity =< 1 :
      LHS = _,
      Parameters = [].


serve_procedure(In, Calls, Variables) :-

    In ? call(Name), string(Name) :
      Calls ! [Name] |
	self;

    In ? call(List), list(List) :
      Calls ! List |
	self;

    In ? call([]) |
	self;

    In ? call(Name + Added) :
      Calls ! [Name],
      Variables ! Arguments? |
	utilities#untuple_predicate_list(',', Added, Added'),
	add_argument_variables(Added'?, Arguments),
	self;

    In ? variables(NamedVariables) :
      Variables ! Vars? |
	add_variables(NamedVariables, Vars),
	self;

    In = [] :
      Calls = [],
      Variables = [].    


add_argument_variables(Added, Arguments) :-

    Added ? Variable, Variable = `String, string(String) :
      Arguments ! Variable |
	self;

    Added ? (_Parameter = Variable), Variable = `String, string(String) :
      Arguments ! Variable |
	self;

    Added ? _Other,
    otherwise | /* ignore other stuff */
      self;

    Added = [] :
      Arguments = [].


add_variables(Variables, Vars) :-

    Variables ? Name, string(Name) :
      Vars ! `Name |
	self;

    Variables ? Variable, Variable = `String, string(String) :
      Vars ! Variable |
	self;

    Variables ? _Other,
    otherwise |
	self;

    Variables =\= [_|_], Variables =\= [] :
      Variables' = [Variables] |
	self;

    Variables = [] :
      Vars = [].


procedures(In, Called, NewCalled, Out) :-

    In ? Procedure, Procedure = Type(LHS, RHS, Communicator),
    arg(1, LHS, Name) :
      Called ! member(Name, Value, Ok) |
	output_procedure;

    In ? _Other,
    otherwise |
	self;

    In = [] :
      NewCalled = Called,
      Out = [].

output_procedure(In, Called, NewCalled, Out,
			Name, Value, Ok, Type, LHS, RHS, Communicator) :-

    Ok = true,
    Type =?= export :
      Name = _,
      Value = _,
      Out ! Type(LHS, RHS, Communicator) |
	procedures;

    Ok = true,
    Type =\= export :
      Closure = _,				/* Debugging aid */
      Out ! Type(NewLHS?, RHS, Communicator'?),
      Value = {Calls, Variables},
      Called' ! replace(Name, {[], Uses?}, _Value, _Found) |
	procedures,
	stream#hash_table([lookup(Name, Name, _, _) | Lookup?]),
	continue_calls + (NewCalls = Calls, NewVariables = [],
				OldCalls = [Name], OldVariables = Variables,
			Procedures = Called, NextProcedures = Called'?),
	remove_unused(LHS, Uses, NewLHS),
	update_communicator(Communicator, LHS, NewLHS, Communicator');

    Ok =\= true :
      Name = _,
      Value = _,
      Type = _,
      LHS = _,
      RHS = _,
      Communicator = _,
      Out = Out' |
	procedures.

continue_calls(NewCalls, NewVariables, OldCalls, OldVariables,
		Procedures, NextProcedures, Lookup, Closure, Uses) :-


    NewCalls =?= [] :
      Closure = OldCalls,
      Procedures = NextProcedures,
      Lookup = [] |
	utils#binary_merge([OldVariables, NewVariables], Uses);

    NewCalls =\= [] |
	add_new_calls(NewCalls, NewVariables, NewCalls', NewVariables',
			Lookup, Lookup'?, Procedures, Procedures'?),
	utils#binary_merge([OldCalls, NewCalls], OldCalls'),
	utils#binary_merge([OldVariables, NewVariables], OldVariables'),
	self.


add_new_calls(Calls, Variables, NewCalls, NewVariables,	Lookup, NextLookup,
		Procedures, NextProcedures) +
		(SumCalls = [], SumVariables = []) :-

    Calls =?= [] :
      NewCalls = SumCalls,
      Lookup = NextLookup,
      Procedures = NextProcedures |
	utils#binary_merge([Variables, SumVariables], NewVariables);

    Calls ? Call :
      Lookup ! lookup(Call, Call, _, Status),
      Procedures ! member(Call, Value, Ok) |
	check_status(Status, Ok, Value, SumCalls, SumVariables,
			SumCalls', SumVariables'),
	self.

check_status(Status, Ok, Value, SumCalls, SumVariables,
		NewSumCalls, NewSumVariables) :-

    Status = old :
      Ok = _,
      Value = _,
      NewSumCalls = SumCalls,
      NewSumVariables = SumVariables;

    Status = new,
    Ok = false :
      Value = {[], []},
      NewSumCalls = SumCalls,
      NewSumVariables = SumVariables;

    Status = new,
    Ok = true,
    Value = {Calls, Variables} |
	utils#binary_merge([Calls, SumCalls], NewSumCalls),
	utils#binary_merge([Variables, SumVariables], NewSumVariables).


update_communicator(Communicator, LHS, NewLHS, NewCommunicator) :-

    Communicator =?= [] :
      LHS = _,
      NewLHS = _,
      NewCommunicator = [];

    Communicator =?= (Atom :- RHS) :
      NewCommunicator = (Atom'? :- RHS) |
	utils#tuple_to_dlist(Atom, [Name | CL], []),
	utils#tuple_to_dlist(LHS, [_Name | CL], DT),
	utils#tuple_to_dlist(NewLHS, [_ | NL], DT),
	utils#list_to_tuple([Name | NL], Atom').


remove_unused(LHS, Uses, NewLHS) + (Index = 2, Outdex = 1) :-

    arg(Index, LHS, Variable), Variable = `Name, string(Name),
    Index++ |
	check_included_variable(NewLHS, Uses, Variable, Outdex, Outdex'),
	self;

    arg(Index, LHS, Variable), Variable = `Invented, tuple(Invented),
    Index++ |
        Outdex++,
	arg(Outdex'?, NewLHS, Variable),
	self;

    otherwise,
    arg(1, LHS, Functor) :
      Uses = _,
      Index = _,
      NewLHS = OutLHS,
      Name = Functor |
	check_included_variable(NewLHS, Uses, BIO_SCHEDULER, Outdex, Outdex'),
	make_tuple(Outdex', OutLHS),
	arg(1, OutLHS, Name).

  check_included_variable(NewLHS, Uses, Variable, Outdex, NewOutdex) :-

    Uses ? Variable :
      Uses' = _ |
	NewOutdex := Outdex + 1,
	arg(NewOutdex, NewLHS, Variable);

    Uses ? Other,
    Other =\= Variable |
	self;

    Uses = [],
    Variable =\= BIO_SCHEDULER :
      NewLHS = _,
      Variable = _,
      NewOutdex = Outdex;

    Uses = [],
    Variable =?= BIO_SCHEDULER |
	NewOutdex := Outdex + 1,
	arg(NewOutdex, NewLHS, Variable).
