-language(compound).
-export([procedures]).


/* procedures/4
**
** Extract and format errors.
** Replace goals by rpcs where appropriate.
** Collect variable tree for each normal body.
*/

procedures(Completed, Edited, ErrorsOut, More) :-

   Completed ? error(Error) :			% copy evaluate output.
      ErrorsOut ! Error |
	self;

   Completed = [Descriptor | _],
   Descriptor = descriptor(SId, ExportedIds\_Rest, _PId1, _PIdN) :
      Reqs ! procedure('#'/0, ExportedIds) |
	requests(Completed, SId, Procedures, Reqs', Errors),
	graph # lists(Reqs?, _, DeadCode, _),
	utilities # sort_ids(DeadCode, DIds),
	filter_dead_code,
	replace_remote_calls(PList?, Alive?, Edited),
	serve_errors(Errors?, SId, [], ErrorsOut, More).

serve_errors(In, SId, Dups, Out, More) :-

   In ? error(SId, Error) |
	duplicate_error(Error, Dups, Dups', Out, Out'?),
	self;

   In ? error(IId, Error), IId =\= SId,
   IId ? Name : IId' = _ |
	construct_diagnostic(Name, Error, Diagnostic),
	duplicate_error(Diagnostic?, Dups, Dups', Out, Out'?),
	self;

   In ? error(SId, ProcId, Error) |
	construct_diagnostic(ProcId, Error, Diagnostic),
	duplicate_error(Diagnostic?, Dups, Dups', Out, Out'?),
	self;

   In ? error(IId, ProcId, Error), IId =\= SId,
   IId ? Name : IId' = _ |
	construct_diagnostic(Name#ProcId, Error, Diagnostic),
	duplicate_error(Diagnostic?, Dups, Dups', Out, Out'?),
	self;

   In = [] : SId = _, Dups = _,
      Out = More .

construct_diagnostic(Part1, Part2, Diagnostic) :-

   Part1 =\= _-_,
   Part1 =\= _#_,
   Part2 =\= _-_ :
      Diagnostic = Part1 - Part2 ;

   Part1 = M#Id,
   Part2 =\= _-_ :
      Diagnostic = M#Id - Part2 ;

   Part1 = M#Id,
   Part2 = C - D :
      Diagnostic = M#Id - C - D ;

   Part1 = A - B,
   Part2 =\= _-_ :
      Diagnostic = A - B - Part2 ;

   Part1 =\= _-_,
   Part1 =\= _#_,
   Part2 = C - D :
      Diagnostic = Part1 - C - D ;

   Part1 = A - B,
   Part2 = C - D :
      Diagnostic = A - B - C - D .

duplicate_error(Error, Dups1, Dups2, Out1, Out2) :-

   Dups1 ? Error : Dups1' = _,
      Dups2 = Dups1,
      Out1 = Out2 ;

   Dups1 ? Other, Error =\= Other :
      Dups2 ! Other |
	self;

   Dups1 = [] :
      Dups2 = [Error],
      Out1 = [Error | Out2] .


requests(Completed, SId, Procedures, Reqs, Errors) :-

   Completed ? Descriptor, Descriptor = descriptor(SId', _Es, _P1, _PN) :
      SId = _,
      Procedures ! Descriptor |
	self;

   Completed ? error(Error), Error =\= (_:_) :
      Errors ! error(SId, Error) |
	self;

   Completed ? error((EId : Error)) :
      Errors ! error(EId, Error) |
	self;

   Completed ? comment(Ms) |
	computation # comment((SId : Ms)),		% debugging aid
	self;

   Completed ? Declaration,
   Declaration = Other(_), Other =\= error, Other =\= comment :
      Procedures ! Declaration |
	self;

   Completed ? Procedure,
   Procedure = proc(Functor, Arity, Args, RHs, State), State = _(PId) :
      Procedures ! proc(Functor, Arity, Args'?, RHs'?, State),	
      Reqs ! procedure(Functor/PId, GIds?) |
	verify_args(Args, SId, Functor/Arity, Args', Errors, Errors'?, 0),
	rhs(SId, Functor/Arity, 1, RHs, RHs', GIds, Errors', Errors''?),
	self;

   Completed = [] : SId = _,
      Procedures = [],
      Reqs = [],
      Errors = [] .

verify_args(Args1, SId, FA, Args2, Errors1, Errors2, Index) :-

   Args1 ? `Arg, string(Arg),
   Index++ :
     Args2 ! `Arg |
	self;

   Args1 ? ?Arg, string(Arg),
   Index++ :
     Args2 ! ?Arg |
	self;

   Args1 ? Other,
   otherwise,		% Bitten by evaluate!
   Index++,
   string_to_dlist("_$INHERIT_ARGUMENT_ERROR$_", IE, IEN),
   convert_to_string(Index', SEN),
   string_to_dlist(SEN, IEN, []) :
      Errors1 ! error(SId, FA, invalid_arg_after_evaluation(Other)),
      Args2 ! `(Arg?) |
	list_to_string(IE, Arg),
	self;

   Args1 = [] : SId = _, FA = _, Index = _,
      Args2 = [],
      Errors1 = Errors2.


rhs(SId, FA, Index, RHs1, RHs2, GIds, Errors1, Errors2) :-

   RHs1 ? Inherit, Inherit = inherit(Functor, _Arity, _Renames, _(RId)),
   Index++ :
      RHs2 ! Inherit,
      GIds ! Functor/RId |
	self;

   RHs1 ? inherit(Functor, _Arity, _Renames, Unknown),
   Unknown =\= _(_) :
      RHs1'' = [error(unknown_super_class(Functor)) | RHs1'] |
	self;

   RHs1 ? _(Guard1, Body),
   Guard1 ? initially :
      Guard2 ! initially, 
      RHs2 ! rhs(Guard2, Body'?),
      CId = FA-initially |
	labelled_guard(SId, CId, Guard1', Guard2', Errors1, Errors1'?),
	body_goals(SId, CId, Body, Body', GIds, GIds'?, Errors1', Errors1''?),
	self;

   RHs1 ? _(Guard1, Body),
   Guard1 ? finally :
      Guard2 ! finally,
      RHs2 ! rhs(Guard2, Body'?),
      CId = FA-finally |
	labelled_guard(SId, CId, Guard1', Guard2', Errors1, Errors1'?),
	body_goals(SId, CId, Body, Body', GIds, GIds'?,	Errors1', Errors1''?),
	self;

   RHs1 ? _(Guard, Body), Guard =\= [initially | _], Guard =\= [finally | _],
   Index++ :
      RHs2 ! rhs(Guard, Body'?) |
	body_goals(SId, FA-Index, Body, Body', GIds, GIds'?,
			Errors1, Errors1'?),
	self;

   RHs1 ? error(Error),
   Index++ :
      Errors1 ! error(SId, FA-Index, Error) |
	self;

   RHs1 ? Other,
   otherwise,
   Index++ :
      RHs2 ! Other |
	self;

   RHs1 = [] : SId = _, FA = _, Index = _,
      RHs2 = [],
      GIds = [],
      Errors1 = Errors2.

labelled_guard(SId, CId, Guard1, Guard2, Errors1, Errors2) :-

   Guard1 ? Listener, Listener = listener(`Name),
   string(Name), Name =\= "_" :
      Guard2 ! Listener |
	self;

   Guard1 ? Other,
   otherwise :
      Errors1 ! error(SId, CId, invalid_guard(Other)) |
	self;

   Guard1 = [] : SId = _, CId = _,
      Guard2 = [],
      Errors1 = Errors2 .


body_goals(SId, CId, Body1, Body2, GIds1, GIds2, Errors1, Errors2 ) :-

   Body1 ? error(Error) :
      Errors1 ! error(SId, CId, Error) |
	self;

   Body1 ? Goal, Goal = goal(Functor, _Call, _(PId)) :
      Body2 ! Goal,
      GIds1 ! Functor/PId |
	self;

   Body1 ? Other,
   otherwise :
      Body2 ! Other |
	self;

   Body1 = [] : SId = _, CId = _,
      Body2 = [],
      GIds1 = GIds2,
      Errors1 = Errors2 .


filter_dead_code(Procedures, DIds, Alive, PList) :-

   DIds ? _/DId |
	filter_dead_code1;

   DIds = [] :
      Alive = Procedures |
	fill_plist.

filter_dead_code1(Procedures, DIds, Alive, PList, DId) :-

   Procedures ? proc(_Functor, _Arity, _Args, _RHs, _(DId)) :
      PList ! dead |
	filter_dead_code;

   Procedures ? A,
   A = proc(_, _, _, _, _(OId)), OId =\= DId :
      Alive ! A,
      PList ! _ |
	self;

   Procedures ? A,
   A =\= proc(_, _, _, _, _) :
      Alive ! A |
	self.

fill_plist(Alive, PList) :-

   Alive ? proc(_Functor, _Arity, _Args, _RHs, _State) :
      PList ! _ |
	self;

   Alive ? _,
   otherwise |
	self;

   Alive = [] :
      PList = [] .


replace_remote_calls(PList, Alive, Edited) :-

   Alive ? descriptor(SId, Exports, PId1, PIdN),
   PList =\= [] :
      Edited ! descriptor(SId, Exports, PIdN) |
	list_to_tuple(PList, PTuple),
	copy_primary;

   Alive = [descriptor(SId, Exports, _PId1, PIdN)],
   PList = [] :
      Edited = [descriptor(SId, Exports, PIdN)] .


copy_primary(Alive, Edited, PId1, PIdN, PTuple) :-

   Alive ? proc(Functor, Arity, Args, RHs1, State),
   State = _(PId),
   arg(PId, PTuple, goal^) :
      Edited ! proc(Functor, Arity, Args, RHs2?, State),
      GIds = _ |
	replace_rpcs,
	self;

   Alive ? Other, Other =\= proc(_,_,_,_,_), Other =\= descriptor(_,_,_,_) :
      Edited ! Other |
	self;

   otherwise : PId1 = _, PIdN = _ |
	replace_inherited_goals.
      
replace_inherited_goals(Alive, Edited, PTuple) :-

   Alive ? descriptor(SId, ExportedIds\_Rest, PId1, PIdN) :
      Edited ! descriptor(SId) |
	utilities # sort_ids(ExportedIds, EIds),
	imported_procedures,
	graph # imports(Procedures?, Importers),
	utilities # sort_ids(Importers?, IIds),
	put_rpcs;

   Alive = [] : PTuple = _,
      Edited = [] .


imported_procedures(Alive, Edited, PTuple, PId1, PIdN, Procedures) :-

   Alive ? proc(Functor, Arity, Args, RHs1, State),
   State = _(PId) :
      Procedures ! procedure(Functor/PId, GIds?),
      Edited ! proc(Functor, Arity, Args, RHs2?, State) |
	self,
	replace_rpcs;

   Alive ? Other, Other =\= proc(_,_,_,_,_), Other =\= descriptor(_,_,_,_) :
      Edited ! Other |
	self;

   otherwise : PId1 = _, PIdN = _,
      Procedures = [] |
	replace_inherited_goals.


put_rpcs(PId1, PIdN, SId, EIds, IIds, PTuple) :-

   PId1 >= PIdN : SId = _, EIds = _, IIds = _, PTuple = _ ;

   EIds ? _/PId1,
   IIds =\= [_/PId1 | _],
   arg(PId1++, PTuple, rpc(SId)^) |
	self;

   EIds =\= [_/PId1 | _],
   IIds ? _/PId1,
   arg(PId1++, PTuple, goal^) |
	self;

   EIds ? _/PId1,
   IIds ? _/PId1,
   arg(PId1++, PTuple, goal^) |
	self;

   PId1 < PIdN,
   EIds =\= [_/PId1 | _],
   IIds =\= [_/PId1 | _],
   arg(PId1++, PTuple, goal^) |
	self;

   EIds ? _/PId1,
   IIds =\= [_/PId1 | _],
   arg(PId1++, PTuple, Done), known(Done) |
	self;

   EIds =\= [_/PId1 | _],
   IIds ? _/PId1,
   arg(PId1++, PTuple, Done), known(Done) |
	self;

   EIds ? _/PId1,
   IIds ? _/PId1,
   arg(PId1++, PTuple, Done), known(Done) |
	self;

   PId1 < PIdN,
   EIds =\= [_/PId1 | _],
   IIds =\= [_/PId1 | _],
   arg(PId1++, PTuple, Done), known(Done) |
	self.

replace_rpcs(RHs1, Arity, Args, PTuple, PId1, PIdN, RHs2, GIds) :-

   RHs1 ? Inherit, Inherit = inherit(Functor, _Arity, _Renames, _(RId)) :
      RHs2 ! Inherit,
      GIds ! Functor/RId |
	self;

   RHs1 ? _(Guard, Body), Guard = [initially | _] :
      RHs2 ! rhs(Guard, Body'?, ITree?, STree?) |
	self,
	external_arguments,
	utilities # variable_trees(Implicit?, true, CallArgs?, Guard, Body'?,
					ITree, STree),
	replace_body_rpcs(Body, Body', PId1, PIdN, PTuple, GIds, GIds'?,
				Implicit, _Self
	);

   RHs1 ? _(Guard, Body), Guard =\= [initially | _] :
      RHs2 ! rhs(Guard, Body'?, ITree?, STree?) |
	self,
	utilities # variable_trees(Implicit?, Self?, Args, Guard, Body'?,
					ITree, STree),
	replace_body_rpcs(Body, Body', PId1, PIdN, PTuple, GIds, GIds'?,
				Implicit, Self
	);

   RHs1 ? Other,
   otherwise :
      RHs2 ! Other |
	self;

   RHs1 = [] : Arity = _, Args = _, PId1 = _, PIdN = _, PTuple = _,
      RHs2 = [],
      GIds = [] .

external_arguments(Arity, Args, CallArgs) :-

   Arity-- > 0,
   Args ? Arg :
      CallArgs ! Arg |
	self;

   Arity =< 0 : Args = _,
      CallArgs = [] .

replace_body_rpcs(Body1, Body2, PId1, PIdN, PTuple, GIds1, GIds2,
			Implicit, Self
) :-

   Body1 ? Goal1, Goal1 = goal(Functor, Call, _(PId)),
   Functor =\= Call,
   PId1 =< PId, PId < PIdN :
      Body2 ! Goal2?,
      GIds1 ! Functor/PId |
	self,
	replace_by_rpc;

   Body1 ? Goal1, Goal1 = goal(Functor, Functor, _(PId)),
   PId1 =< PId, PId < PIdN :
      Body2 ! Goal2?,
      GIds1 ! Functor/PId,
      Implicit = true, Implicit' = _ |
	self,
	replace_by_implicit_rpc;

   Body1 ? Iterate, Iterate = primitive(self) :
     Body2 ! Iterate,
     Self = true, Self' = _ |
	self;

/* These are imports */

   Body1 ? Goal, Goal = goal(Functor, Call, _(PId)),
   PId < PId1,
   Functor =\= Call :
      Body2 ! Goal,
      GIds1 ! "#"/2 |
	self;

   Body1 ? Goal, Goal = goal(Functor, Functor, _(PId)),
   PId < PId1 :
      Body2 ! Goal,
      GIds1 ! "#"/2,
      Implicit = true, Implicit' = _ |
	self;

   Body1 ? Goal, Goal = goal(Functor, Call, _(PId)),
   PIdN =< PId,
   Functor =\= Call :
      Body2 ! Goal,
      GIds1 ! "#"/2 |
	self;

   Body1 ? Goal, Goal = goal(Functor, Functor, _(PId)),
   PIdN =< PId :
      Body2 ! Goal,
      GIds1 ! "#"/2,
      Implicit = true, Implicit' = _ |
	self;

/* * * * * * * * * * */

   Body1 ? Other,
   otherwise :
      Body2 ! Other |
	self;

   Body1 = [] : PId1 = _, PIdN = _, PTuple = _,
      Body2 = [],
      GIds1 = GIds2,
      Implicit = false,
      Self = false .

replace_by_rpc(PId, PTuple, Call, Goal1, Goal2) :-

   arg(PId, PTuple, rpc(SId)),
   SId = [Service | _] : Goal1 = _,
      Goal2 = rpc(Service, Call) ;

   otherwise : Call = _, PId = _, PTuple = _,
      Goal2 = Goal1 .

replace_by_implicit_rpc(PId, PTuple, Functor, Goal1, Goal2) :-

   arg(PId, PTuple, rpc(SId)),
   SId = [Service | _] : Goal1 = _,
      Goal2 = implicit_rpc(Service, Functor, PId) ;

   otherwise : Functor = _, PId = _, PTuple = _,
      Goal2 = Goal1 .
