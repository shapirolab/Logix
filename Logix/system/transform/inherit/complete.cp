-language(compound).
-export(module).

module(Attributes1, Clauses, Attributes2, Evaluated) :-
   true :
      Requests2 = [],
      Trace = [],
      Depends2 = [],
      Attributes2 ! depends(Depends1?),
      Attributes2' = Attributes1 |
	utilities # extract_attributes(Attributes1, SId, Includes,
					OverRides, Exports),
	source # procedures(new, Clauses, SId?, Exports?, Dict?,
				Requests1, Requests1'?, Procedures, Remotes),
	dictionary # server(Requests1?, _PId),
	complete,
	evaluate.

evaluate(Evaluates, Completed, Evaluated) :-

   Evaluates = true |
	evaluate # clauses(Completed, Evaluated1, Errors),
	evaluate_errors;

   Evaluates =\= true :
      Evaluated = Completed.

evaluate_errors(Errors, Evaluated1, Evaluated) :-

   Errors ? Error :
      Evaluated ! error(Error) |
	self;

   Errors = [] :
      Evaluated = Evaluated1 .


complete(SId, OverRides, Procedures, Includes, Depends1, Depends2,
	 Completed, Evaluates, Remotes, Trace, Dict, Requests1, Requests2
) :-
   true :
      Trace' = [SId | Trace],
      Added = [] |
	includes,
	augment,
	utilities # overrides(Es?, OverRides),
	combine(SId, Trace, Procedures, Augments?, Dict, Es,
		Completed, Evaluates2),
	utilities # or(Evaluates1?, Evaluates2?, Evaluates).


augment(Trace, Adds, Depends1, Depends2, Augments, Requests1, Requests2,
	Evaluates1
) :-

   Adds ? add(SId, RemoteCalls),
   list(SId) |
	recursive_inclusion,
	add_module(Trace, Evaluates2, Augments, Augments'?,
		   Depends1, Depends1'?, Requests1, Requests1'?,
		   SId, RemoteCalls, Reply
	),
	self,
	utilities # or(Evaluates1'?, Evaluates2?, Evaluates1);

   Adds ? add(Other, RemoteCalls),
   tuple(Other) :
      Augments ! Other |
	self,
	unknown_remotes;

   Adds = [] : Trace = _,
      Depends1 = Depends2,
      Augments = [],
      Requests1 = Requests2,
      Evaluates1 = false .

unknown_remotes(RemoteCalls) :-

   RemoteCalls ? ModuleName(_Functor, IId) :
      IId = remote_error(ModuleName, module_not_found) |
	self;

   RemoteCalls = [] : true.


recursive_inclusion(Trace, SId, Reply) :-

   Trace ? SId : Trace' = _,
      Reply = recursive ;

   Trace ? Other, Other =\= SId |
	self;

   Trace = [] : SId = _,
      Reply = new.


add_module(Trace, Evaluates, Augments1, Augments2, Depends1, Depends2,
	   Requests1, Requests2, SId, RemoteCalls, Reply
) :-

   Reply = new :
      Requests1 ! get(SId, Result) |
	add_module1;

   Reply = recursive : Trace = _,
      Evaluates = false,
      SId = [Name | _],
      Augments1 = [error((SId : recursively_included(List))) | Augments2],
      Depends1 = Depends2,
      Requests1 = Requests2,
      Reverse = [Name] |
	recursively_includes,
	unknown_remotes.

recursively_includes(SId, Trace, Reverse, List) :-

   Trace = [SId | _],
   SId = [Name | _] :
      List = [Name | Reverse] ;

   Trace ? Id, Id =\= SId,
   Id = [Name | _] :
      Reverse' =  [Name | Reverse] |
	self.

add_module1(Trace, Evaluates, Augments1, Augments2, Depends1, Depends2,
		Requests1, Requests2, SId, RemoteCalls, Result
) :-

   Result = false(Diagnostic) : Trace = _,
      Evaluates = false,
      Augments1 = [error((SId : Diagnostic)) | Augments2],
      Depends1 = Depends2,
      Requests1 = Requests2 |
	unknown_remotes;

   Result = Age(Attributes, Terms) :
      SId = [ModuleName | _],
      Depends1 ! ModuleName,
      Attributes' = [service_id(SId) | Attributes] |
	depends,
	utilities # extract_attributes(Attributes'?, _, Includes,
					OverRides, Exports),
	source # procedures(Age, Terms, SId, Exports?, Dict?,
				Requests1, Requests1'?, Procedures, Remotes),
	lookup_remotes(RemoteCalls, Dict, Dict'?),
	complete,
	utilities # append_list(Completed?, Augments2, Augments1).

lookup_remotes(RemoteCalls, Dict1, Dict2) :-

   RemoteCalls ? ModuleName(Functor, IId), Functor =\= "" :
      Dict1 ! member(Functor, ProcState, Reply) |
	self,
	found_remote;

   RemoteCalls ? _ModuleName("", 0^) |
	self;

   RemoteCalls = [] :
      Dict1 = Dict2 .

found_remote(ModuleName, Reply, ProcState, IId) :-

   Reply = true, ProcState = _(PId) : ModuleName = _,
      IId = PId ;

   Reply =\= true : ProcState = _,
      IId = remote_error(ModuleName, procedure_not_found) .


depends(SId, ModuleName) :-

   SId = [Name | _], Name =\= self :
      ModuleName = Name ;

   SId = [self, Name | _] :
      ModuleName = Name .


includes(SId, Includes, Remotes, Added, Adds) :-

   Includes ? ModuleName |
	include,
	exclude_duplicate(Add, [], Added, Added'),
	self;

   Remotes ? remote_inherit(ModuleName, Functor, IId) :
      Remote = [ModuleName(Functor, IId)] |
	include,
	exclude_duplicate(Add, Remote, Added, Added'),
	self;

   Includes = [], Remotes = [] : SId = _,
      Adds = Added .

exclude_duplicate(Add, Remote, Added1, Added2) :-

   Added1 ? add(Add, Remotes) :
      Added2 = [add(Add, Remotes'?) | Added1'] |
	utilities # append_list(Remote, Remotes, Remotes');

   Added1 ? Other, Other =\= add(Add, _) :
      Added2 ! Other |
	self;

   Added1 = [] :
      Added2 = [add(Add, Remote)] .


include(SId, ModuleName, Add) :-

   ModuleName = super,
   SId ? self |
	self;

   ModuleName = super,
   SId = [Self | Id],
   Self =\= self :
      Name = ModuleName, Reply = true |
	include1;

   string(ModuleName), ModuleName =\= super,
   SId = [_Self | Id] :
      Name = ModuleName, Reply = true |
	include1;

   ModuleName ? Self |
	computation_utils # call_id_goal(ModuleName'#Self, Id, Name, Reply),
	include1;

   otherwise,
   SId = [_Self | Super] |
	computation_utils # call_id_goal(Super#ModuleName, Id, Name, Reply),
	include1.

include1(SId, ModuleName, Add, Id, Name, Reply) :-

   Reply = true,
   string(Name),
   Name =\= super, Name =\= self, Name =\= processor, Name =\= computation |
	utils # append_strings([Name, ".cp"], NameCp),
	file # execute_in_context(Id, fileinfo(NameCp?, Reply')),
	include2;

   Reply = true,
   Name = super :
      Name' = self,
      NameCp = "self.cp" |
	include2;

   Reply = true,
   otherwise : Id = _, Name = _,
      Add = error((SId : invalid_include(ModuleName)));

   Reply = false(Reason) : Id = _, Name = _,
      Add = error((SId : cant_include(ModuleName, Reason))).

include2(SId, ModuleName, Add, Id, Name, NameCp, Reply) :-

   Reply = 0,
   Id ? _ |
	file # execute_in_context(Id', fileinfo(NameCp, Reply')),
	self;

   string(Reply) : SId = _, ModuleName = _, NameCp = _,
      Add ! Name,
      Add' = Id ;

   Reply = 0,
   Id = [] : Name = _, NameCp = _,
      Add = error((SId : cant_include(ModuleName, not_found))).


combine(SId, Trace, Procedures, Augments, Dict, Es, Completed, Evaluates) :-

   true :
      Dict = Dict1?,
      Dict2 = DictG?,
      Ps = Procedures, Plus = Augments |
	combines.

combines(SId, Trace, Ps, Plus, DictG, Es, Completed, Evaluates, Dict1, Dict2) :-

   Ps ? proc(Functor, Arity, Args, RHs, State),
   State = primary(PId) : SId = _,
      Es ! Functor(Status),
      State' = Status?(PId),
      Dict1 ! replace(Functor, State'?, _, _),
      Completed ! proc(Functor, Arity, Args, RHs'?, State'?) |
	complete_rhs(Trace, RHs, RHs', DictG, DictG'?),
	self;

   Ps ? proc(Functor, Arity, Args, RHs, State),
   State = override(PId) :
      Dict1 ! lookup(Functor, NewState?, OldState, Reply) |
	complete_rhs(Trace, RHs, RHs', DictG, DictG'?),
	included_state(SId, Reply?, OldState?, NewState, Functor, Arity, Args,
			RHs'?, PId, Es, Es'?, Completed, Completed'?
	),
	self;

   Ps ? proc(Functor, Arity, Args, RHs, OldState(PId)),
   OldState =\= override, OldState =\= primary :
      Completed ! proc(Functor, Arity, Args, RHs', private(PId)) |
	complete_rhs(Trace, RHs, RHs', DictG, DictG'?),
	self;

   Ps ? Evaluate, Evaluate = (_ => _) :
      Evaluates = true, Evaluates' = _,
      Completed ! Evaluate |
	self;

   Ps ? Other,
   Other =\= proc(_, _, _, _, _), Other =\= (_ => _) :
      Completed ! Other |
	self;

   Ps = [], Plus =\= [] :
      Ps' = Plus,
      Plus' = [] |
	self;

   Ps = [], Plus = [] : Trace = _, SId = _,
      Es = [],
      Completed = [],
      Evaluates = false,
      DictG = [],
      Dict1 = Dict2 .


included_state(SId, Reply, OldState, NewState, Functor, Arity, Args, RHs, PId,
		Es1, Es2, Completed1, Completed2
) :-

   Reply = new : SId = _, OldState = _,
      NewState = {import(Status?), PId},
      Completed1 = [proc(Functor, Arity, Args, RHs, Status?(PId))
		   | Completed2],
      Es1 = [Functor(Status) | Es2];

   Reply = old,
   OldState =\= {import(_), _} : SId = _,
      NewState = OldState,
      Completed1 = [proc(Functor, Arity, Args, RHs, private(PId))
		   | Completed2],
      Es1 = Es2 ;

   Reply = old,
   OldState = {import(_), _} :
      NewState = OldState,
      Completed1 = [error((SId : include_conflict - Functor/Arity)),
		    proc(Functor, Arity, Args, RHs, private(PId))
		   | Completed2],
      Es1 = Es2 .


complete_rhs(Trace, RHs1, RHs2, Ds1, Ds2) :-

   RHs1 ? unknown(Guard, Body) :
      RHs2 ! Result?(Guard, Body'?) |
	body_goals(Body, Body', Ds1, Ds1'?, Result),
	self;

   RHs1 ? Other,
   Other =\= inherit(_, _, _, _), Other =\= unknown(_, _) :
      RHs2 ! Other |
	self;

   RHs1 ? inherit(Functor, Arity, Renames, OldState),
   OldState = local(PId) :
      RHs2 ! inherit(Functor, Arity, Renames, private(PId)) |
	self;

   Trace =\= [],
   RHs1 ? Inherit, Inherit = inherit(_, _, _, private(PId)),
   integer(PId) :
      RHs2 ! Inherit |
	self;

   Trace = [],
   RHs1 ? inherit(Functor, Arity, Renames, private(PId)),
   integer(PId) :
      RHs2 ! inherit(Functor, Arity, Renames, ToState?),
      Ds1 ! member(Functor, DictState, Reply) |
	last_private_state,
	self;

   RHs1 ? inherit(Functor, _Arity, _Renames,
		  private(remote_error(ModuleName, Reason))) :
      RHs2 ! error((ModuleName#Functor, cant_inherit-Reason)) |
	self;

   RHs1 ? inherit(Functor, Arity, Renames, FromState),
   otherwise :
      RHs2 ! inherit(Functor, Arity, Renames, ToState?),
      Ds1 ! member(Functor, DictState, Reply),
      OldResult = NewResult? |
	update_state,
	self;

   RHs1 = [] : Trace = _,
      RHs2 = [],
      Ds1 = Ds2 .


body_goals(Bs1, Bs2, Ds1, Ds2, Result) :-

   Bs1 ? Goal, Goal = goal(Functor, Call, OldState),
   OldState = OldStatus(PId), OldStatus =\= override :
						% OldStatus = local or private
      Bs2 ! goal(Functor, Call, private(PId)) |
	self;

   Bs1 ? goal(Functor, Goal, FromState),
   otherwise :				% FromState = unknown or override(_)
      Ds1 ! member(Functor, DictState, Reply),
      Bs2 ! goal(Functor, Goal, ToState?),
      Result = NewResult?,
      OldResult = Result'? |
	update_state,
	self;

   Bs1 ? Other, Other =\= goal(_, _, _) :
      Bs2 ! Other |
	self;

   Bs1 = [] :
      Bs2 = [],
      Ds1 = Ds2,
      Result = known .


update_state(FromState, Reply, DictState, ToState, OldResult, NewResult) :-

   Reply = false,
   FromState = unknown : DictState = _, OldResult = _,
      ToState = unknown,
      NewResult = unknown ;

   Reply = true,
   FromState = unknown : Reply = _, OldResult = _,
      ToState = DictState,
      NewResult = unknown ;

   Reply = true,
   FromState = _(_), DictState = Status(_),
   Status =\= import(_) : OldResult = _,
      ToState = DictState,
      NewResult = unknown ;

   Reply = true,
   FromState = _(PId), DictState = {import(Status), PId} : OldResult = _,
      ToState = Status(PId),
      NewResult = unknown ;

   otherwise,
   FromState = _(PId) : Reply = _, DictState = _,
      ToState = private(PId),
      NewResult = OldResult .


last_private_state(PId, Reply, DictState, ToState) :-

   Reply = true,
   DictState = {import(Status), PId} :
      ToState = Status(PId) ;

   otherwise : Reply = _, DictState = _,
      ToState = private(PId) .
