-language(compound).
-export(procedures).


procedures(Age, Terms, SId, Exports, Ps2, Rs1, Rs2, Procedures, Remotes) :-
   true :
      Rs1 ! index_value(Ix1),
      Rs2' = [index_value(Ix2) | Rs2],
      Procedures ! descriptor(SId, ExportedIds? \ Rest?, Ix1?, Ix2?) |
	serve_procs,
	clauses,
	stream # hash_table(Ps1?).


serve_procs(Age, Procs, Exports, Ps1, Ps2, Rs1, Rs2, Procedures, ExportedIds,
		Rest
) :-

   Procs ? proc(Functor, Arity, Args, Assigns, RHSs) :
      Ps1 ! lookup(Functor, NewState?, OldState, Reply),
      Procedures ! RProc?,
      RsA = Rs1,
      RsB = Rs1'? |
	self,
	unique_functor,
	exported(Reply?, Functor, NewState?,
		 Exports, Exports', ExportedIds, ExportedIds'?);

   Procs ? (_ => _),		% Discard duplicate evaluation directives
   Age = old |
	self;

   Procs ? Other,
   otherwise :
      Procedures ! Other |
	self;

   Procs = [],
   Exports =\= all : Age = _,
      Ps1 = Ps2,
      Rs1 = Rs2,
      Procedures = [],
      ExportedIds = [],
      Rest = Exports ;

   Procs = [],
   Exports = all : Age = _,
      Ps1 = Ps2,
      Rs1 = Rs2,
      Procedures = [],
      ExportedIds = [],
      Rest = [] .


unique_functor(Functor, Arity, Args, Assigns, RHSs, Reply,
		OldState, NewState, RsA, RsB, RProc
) :-

   Reply = new,
   Assigns = [] : OldState = _,
      RsA = [index(Ix) | RsB],
      NewState = primary(Ix?),
      RProc = proc(Functor, Arity, Args, RHs?, NewState),
      RHs = RHSs ;

   Reply = new,
   Assigns =\= [] : OldState = _,
      RsA = [index(Ix) | RsB],
      NewState = primary(Ix?),
      RProc = proc(Functor, Arity, Args, RHs?, NewState),
      RHs = [unknown([initially], Assigns) | RHSs] ;

   Reply = old : Args = _, Assigns = _, RHSs = _,
      NewState = OldState,
      RsA = RsB,
      RProc = error(duplicate_functor - Functor/Arity) .

exported(Reply, Functor, NewState, Es1, Es2, Eds1, Eds2) :-

   Reply = new,
   NewState = primary(Index),
   Es1 = all :
      Es2 = Es1,
      Eds1 = [Functor/Index | Eds2] ;

   Reply = new,
   NewState = primary(Index),
   Es1 =\= all |
	exported1;
   
   Reply =\= new : Functor = _, NewState = _,
      Es2 = Es1,
      Eds1 = Eds2 .

exported1(Functor, Index, Es1, Es2, Eds1, Eds2) :-

   Es1 ? Functor :
      Es2 = Es1',
      Eds1 = [Functor/Index | Eds2];

   Es1 ? Functor/_ :
      Es2 = Es1',
      Eds1 = [Functor/Index | Eds2];

   Es1 ? Other, Other =\= Functor, Other =\= Functor/_ :
      Es2 ! Other |
	self;

   Es1 = [] : Functor = _, Index = _,
      Es2 = [],
      Eds1 = Eds2;

   Es1 =\= [_|_], Es1 =\= [] :
      Es1' = [Es1] |
	self.


clauses(Terms, Procs, Remotes) :-

   Terms ? (Head :- RHSS),
   tuple(Head),
   Head =\= (`_ = _), Head =\= ((`_) ! _),
/* Exclude other primitive procedures here */
   Arity := arity(Head) |
	tuple_to_dlist(Head, Args, Rest),
	parse_head;

   Terms ? (Functor :- RHSS),
   string(Functor) :
      Arity = 0,
      Args = [],
      Assigns = [] |
	parse_args;

   Terms ? Other,
   otherwise :
      Procs ! Proc? |
	clauses,
	not_a_procedure;

   Terms = [] :
      Procs = [],
      Remotes = [] .


not_a_procedure(Other, Proc) :-

   Other = "procedure"(_) :
      Proc = typedef(Other);

   Other = (_ ::= _) :
      Proc = typedef(Other);

   Other = (_ => _) :
      Proc = Other;

   otherwise :
      Proc = error((illegal_procedure - Other)).


parse_head(Terms, Procs, Remotes, Args, RHSS, Arity, Rest) :-

   Args ? "+", Arity = 3 :
      Rest = _ |
	parse_head1;

   Args ? Functor, Functor(Arity--) =\= "+"(3) :
      Rest = [],
      Assigns = [] |
	parse_args.

parse_head1(Terms, Procs, Remotes, Args, RHSS) :-

   Args = [Functor, Locals | _],
   constant(Functor),
   tuple(Locals) :
      Arity = 0,
      Assigns = [] |
	tuple_to_dlist(Locals, Args', []),
	parse_args;

   Args = [Head, Locals | _],
   tuple(Head),
   Arity := arity(Head) - 1,
   tuple(Locals) |
	tuple_to_dlist(Head, Args', LocalArgs?),
	Args'? = [Functor | Args''],
	local_args(Locals, LocalArgs, Assigns),
	parse_args;

   otherwise,
   Args = [Head, Locals | _] :
      RHSS = _,
      Procs ! error((illegal_lhs - Head+Locals)) |
	clauses.

local_args(Locals, Args, Assigns) :-

   Locals = (Assign, Locals'),
   Assign = (Arg = _), Arg = `_ :
      Args ! Arg,
      Assigns ! primitive(Assign) |
	self;

   Locals = (Arg, Locals'),
   Arg =\= (`_ = _) :
      Args ! Arg |
	self;

   Locals = (Arg = _), Arg = `_ :
      Args = [Arg],
      Assigns = [primitive(Locals)] ;

   otherwise :
      Args = [Locals],
      Assigns = [] .


parse_args(Terms, Procs, Remotes, Args, RHSS, Arity, Assigns, Functor) :-

   string(Functor),
/* Add other illegal functors here */
   Functor =\= self, Functor =\= "" :
      Procs ! Proc?,
      Remotes = Rs1?,
      Rs2 = Remotes'? |
	arguments,
	serve_arguments(NDRs?),
	clauses,
	parsed_args;

   otherwise : Args = _, RHSS = _, Arity = _, Assigns = _,
      Procs ! error((illegal_functor - Functor)) |
	clauses.


parsed_args(Reply, Proc, Rs1, Rs2, Args, RHSS, Arity, Functor, Assigns) :-

   Reply = [] :
      Proc =  proc(Functor, Arity, Args, Assigns, RHSs?) |
	serve_rhs,
	expand_rhs(RHSS, Requests, []);

   Reply = [Diagnostic] :
      RHSS = _, Args = _, Assigns = _,
      Rs1 = Rs2,
      Proc = error((Functor/Arity - Diagnostic)) ;

   otherwise :
      RHSS = _, Args = _, Assigns = _,
      Rs1 = Rs2,
      Proc = error((Functor/Arity - Reply)) .

serve_rhs(Requests, Rs1, Rs2, RHSs) :-

   Requests ? remote(Remote) :
      Rs1 ! Remote |
	self;

   Requests ? rhs(RHS) :
      RHSs ! RHS |
	self;

   Requests = [] :
      Rs1 = Rs2,
      RHSs = [] .


expand_rhs(RHSS, Requests1, Requests2) :-

   RHSS = (RHSS' ; RHSS'') |
	expand_rhs(RHSS', Requests1, Requests1'?),
	self;

   RHSS = (Guard1 | Body1) :
      Requests1 = [rhs(unknown(Guard2?, Body2?)) | Requests2],
      GuardN = [],
      BodyN = [] |
	expand_guard,
	expand_body;

   RHSS = +Inherit :
      Requests1 = [rhs(Inherited) | Requests2],
      Kind = unknown,
      Functor = _ |
	parse_inherit;

   RHSS = (Service # Goal) :
      Requests1 ! rhs(RHS?) |
	parse_remote_inherit;

   RHSS =\= (_ ; _), RHSS =\= (_ | _) :
      Requests1 = [rhs(unknown([], Body?)) | Requests2] |
	expand_body(RHSS, Body, []).

expand_guard(Guard1, Guard2, GuardN) :-

   Guard1 = (Guard, Guard1') |
	expand_guard(Guard, Guard2, Guard2'?),
	self;

   Guard1 = true :
      Guard2 = GuardN;

   Guard1 = (`VarName ? Guard1'),
   string(VarName),
   string_to_dlist(VarName, VL, [Prime]) :
      ascii("'", Prime),
      Guard2 ! (`VarName = [Term? | `(VarNameP?)]) |
	list_to_string(VL, VarNameP),
	annotated_term(Guard1', Guard2', GuardN, Term);

   Guard1 = (`VarName += Guard1'),
   string(VarName),
   string_to_dlist(VarName, VL, [Prime]) :
      ascii("'", Prime),
      Guard2 ! (`(VarNameP?) := `VarName + Term?) |
	list_to_string(VL, VarNameP),
	annotated_term(Guard1', Guard2', GuardN, Term);

   Guard1 = (`VarName -= Guard1'),
   string(VarName),
   string_to_dlist(VarName, VL, [Prime]) :
      ascii("'", Prime),
      Guard2 ! (`(VarNameP?) := `VarName - Term?) |
	list_to_string(VL, VarNameP),
	annotated_term(Guard1', Guard2', GuardN, Term);

   Guard1 = {PP, `VarName},
   PP = "++",
   string(VarName),
   string_to_dlist(VarName, VL, [Prime]) :
      ascii("'", Prime),
      Guard2 = [(`(VarNameP?) := `VarName + 1) | GuardN] |
	list_to_string(VL, VarNameP);

   Guard1 = {MM, `VarName},
   MM = "--",
   string(VarName),
   string_to_dlist(VarName, VL, [Prime]) :
      ascii("'", Prime),
      Guard2 = [(`(VarNameP?) := `VarName + 1) | GuardN ] |
	list_to_string(VL, VarNameP);

   otherwise :
      Guard2 ! Term? |
	annotated_term(Guard1, Guard2', GuardN, Term).

annotated_term(In, L1, L2, Out) :-

   constant(In) :
      Out = In,
      L1 = L2;

   In ? Term :
      Out ! Term' |
	self,
	annotated_term(Term, L1, L1'?, Term');

   In = {PP, `VarName},
   PP = "++",
   string_to_dlist(VarName, VL, [Prime]) :
      Out = `VarName,
      ascii("'", Prime),
      L1  = [(`(VarNameP?) := `VarName + 1) | L2] |
	list_to_string(VL, VarNameP);

   In = {MM, `VarName},
   MM = "--",
   string_to_dlist(VarName, VL, [Prime]) :
      Out = `VarName,
      ascii("'", Prime),
      L1  = [(`(VarNameP?) := `VarName - 1) | L2] |
	list_to_string(VL, VarNameP);

   otherwise,			% some other kind of tuple
   N := arity(In),
   make_tuple(N, T) :
      Out = T |
	annotated_args.

annotated_args(In, L1, L2, Out, N) :-

   N-- > 0,
   arg(N, In, G1),
   arg(N, Out, T) |
	self,
	annotated_term(G1, L1, L1'?, T);

   N =< 0 : In = _, Out = _,
      L1 = L2.
	

expand_body(Body1, Body2, BodyN) :-

   Body1 = (Body, Body1') |
	expand_body(Body, Body2, Body2'?),
	self;

   Body1 ? Body |
	expand_body(Body, Body2, Body2'?),
	self;

   Body1 = [] :
      Body2 = BodyN ;

   Body1 = true :
      Body2 = BodyN ;

   Body1 = self :
      Body2 = [primitive(Body1) | BodyN] ;

   Body1 = finalize :
      Body2 = [primitive(Body1) | BodyN] ;

   Body1 = (`_ := _) :
      Body2 = [primitive(Body1) | BodyN] ;

   Body1 = (`_ = _) :
      Body2 = [primitive(Body1) | BodyN] ;

   Body1 = (NonVar = _), NonVar =\= `_ :
      Body2 = [error((invalid_primitive_goal - Body1)) | BodyN] ;

   Body1 = (NonVar := _), NonVar =\= `_ :
      Body2 = [error((invalid_primitive_goal - Body1)) | BodyN] ;

/* Add other primitive procedures here */

   Body1 = (Var ! T), Var = `_ :
      Predicate = (Var = [T | ?(Primed?)]) |
	primitive;

   Body1 = (Var ! _), Var =\= `_ :
      Body2 = [error(invalid_primitive_goal - Body1) | BodyN] ;

   Body1 = (Var += T) :
      Predicate = (`(Primed?) := Var + T) |
	primitive;

   Body1 = (Var -= T) :
      Predicate = (`(Primed?) := Var - T) |
	primitive;

   Body1 = {PP, Var}, PP = "++" :
      Predicate = (`(Primed?) := Var + 1) |
	primitive;

   Body1 = {MM, Var}, MM = "--" :
      Predicate = (`(Primed?) := Var - 1) |
	primitive;

   Body1 = Service # Goal :
      Body2 = [rpc(Service, Goal) | BodyN]; 

   otherwise :
      Body2 = [Goal? | BodyN] |
	goal(Body1, Goal).
      
goal(Call, Goal) :-

   string(Call) :
      Goal = goal(Call, Call, unknown) ;

   tuple(Call), Call =\= `_, Call =\= ?_,
   arg(1, Call, Functor),
   string(Functor) :
      Goal = goal(Functor, Call, unknown) ;

   Call = `_ :
      Goal = primitive(Call);

   Call = ?_ :
      Goal = primitive(Call);

   tuple(Call),
   arg(1, Call, `_) :
      Goal = primitive(Call);

   tuple(Call),
   arg(1, Call, ?_) :
      Goal = primitive(Call);
   
   otherwise :
      Goal = error(illegal_goal-Call).

primitive(Body1, Body2, BodyN, Var, Predicate, Primed) :-

   Var = `V,
   string(V),
   string_to_dlist(V, L, [Prime]) : Body1 = _,
      ascii("'", Prime),
      Body2 = [primitive(Predicate) | BodyN] |
	list_to_string(L, Primed);

   Var = ?V,
   string(V),
   string_to_dlist(V, L, [Prime]) : Body1 = _,
      ascii("'", Prime),
      Body2 = [primitive(Predicate) | BodyN] |
	list_to_string(L, Primed);

   otherwise : Var = _, Predicate = _, Primed = _,
      Body2 = [error(invalid_primitive_goal - Body1) | BodyN] .
	

parse_remote_inherit(Service, Goal, RHS, Requests1, Requests2) :-

   Service =\= +_ :
      RHS = known([], [rpc(Service, Goal)]),
      Requests1 = Requests2 ;

   Service = +ModuleName,
   string(ModuleName) :
      Requests1 = [remote(remote_inherit(ModuleName, Functor?, IId))
		  | Requests2] |
	parse_inherit(remote(ModuleName, IId?), Goal, RHS, Functor);

   otherwise :
      RHS = error(invalid_remote_inherit - Service#Goal),
      Requests1 = Requests2 .

parse_inherit(Kind, Inherit, Inherited, Functor) :-

   string(Inherit), Inherit =\= "", Kind =\= remote(_,_) :
      Functor = Inherit,
      Inherited = inherit(Inherit, -1, [], Kind) ;

   string(Inherit), Inherit =\= "", Kind = remote(_ModuleName, IId) :
      Functor = Inherit,
      Inherited = inherit(Inherit, -1, [], private(IId)) ;

   tuple(Inherit), Inherit =\= (_+_),
   Arity := arity(Inherit) - 1 |
	tuple_to_dlist(Inherit, Args, []),
	parse_inherit_args;

   otherwise,
   Kind =\= remote(_,_) : Functor = _,
      Inherited = error(invalid_inherit - Inherit) ;

   otherwise,
   Kind = remote(ModuleName,_) : Functor = "",
      Inherited = error(invalid_inherit - ModuleName#Inherit) .


locals_to_list(Locals, LocalsList) :-

   Locals = (Local, Locals') :
      LocalsList ! Local |
	self;

   Locals =\= (_, _) :
      LocalsList = [Locals] .


parse_inherit_args(Kind, Args, Inherited, Functor, Arity, Inherit) :-

   Args ? First,
   string(First), First =\= "" : Inherit = _,
      Functor = First,
      Renames = Args' |
	arguments,
	serve_arguments(NDRs?),
	parse_inherit1;

   otherwise, Kind =\= remote(_,_) : Args = _, Arity = _, Functor = _,
      Inherited = error(invalid_inherit_functor - Inherit) ;

   otherwise, Kind = remote(ModuleName,_) : Args = _, Arity = _,
      Functor = "",
      Inherited = error(invalid_remote_inherit_functor - ModuleName#Inherit) .
	

parse_inherit1(Kind, Renames, Functor, Arity, Inherited, Reply) :-

   Reply = [], Kind =\= remote(_,_) : Arity = _,
      Inherited = inherit(Functor, Arity, Renames, Kind) ; 

   Reply = [], Kind = remote(_, IId) : Arity = _,
      Inherited = inherit(Functor, Arity, Renames, private(IId)) ; 

   Reply = [(Diagnostic - Argument)] : Renames = _, Kind = _,
      Inherited = error((inherit - Functor/Arity, Diagnostic - Argument)) ;

   otherwise : Renames = _, Kind = _,
      Inherited = error((inherit - Functor/Arity, Reply)).


arguments(Args, NDRs, Reply)  :-

   Args ? Arg :
      NDRs ! Request? |
	argument,
	self;

   Args = [] :
      NDRs = reply(Reply) .

argument(Arg, Request) :-

   Arg = `S,
   string(S) :
      Request = enter(S);

   otherwise :
      Request = error(invalid_argument - Arg).

serve_arguments(NDRs)
 + (Diagnostics = Errors?, Errors, Tree = []) :-

   NDRs ? error(Error) :
      Errors ! Error |
	self;

   NDRs ? enter(Name),
   nth_char(string_length(Name), Name) =\= ascii("'") |
	lookup_argument(Name, Tree, Tree', Looked),
	enter_argument_name(Looked?, NDRs'?, NDRs''),
	self;

   NDRs ? enter(Name),
   nth_char(string_length(Name), Name) =:= ascii("'") :
      Errors ! error(primed_name - Name) |
	self;

   NDRs = reply(Diagnostics^) : Tree = _,
      Errors = [] ;

   NDRs = [] :
      Diagnostics = _, Tree = _, Errors = _ .

lookup_argument(Name, LookupTree, Tree, Looked) :-

   LookupTree = { Other, LookupTree', Right },
   Name @< Other :
      Tree = { Other, Tree'?, Right } |
	self;

   LookupTree = { Other, Left, LookupTree' }, 
   Other @< Name :
      Tree = { Other, Left, Tree'? } |
	self;

   LookupTree = { Name, _, _ } :
      Tree = LookupTree,
      Looked = duplicate_argument - Name ;

   LookupTree  = [] :
      Tree = { Name, [], [] },
      Looked =  new .

enter_argument_name(Looked, Errors1, Errors2) :-

   Looked =\= new :
      Errors2 = [error(Looked) | Errors1];

   Looked = new :
      Errors2 = Errors1 .
