-language(compound).
-export([initially, finally]).

initially(GlobalArgs, NA, Inis, Tests, Initially, CallArgs) :-
	initials(0, NA, Inis, Tests, Initially, Lookups, Renames?, Renames),
	stream # hash_table(HT?),
	serve_requests(GlobalArgs, NA, Lookups?, 0, CallArgs, HT).
	
initials(IX, NA, Inis, Tests,Initially, L1, L2, Renames) :-

   Inis ? initial(BaseArgs, Guards, Goals, tree(VDict)),
   IX++ :
      L1 ! arguments(IX', BaseArgs, AL?),
      Renames ! group(IX') |
	utilities # primed_arguments(NA, BaseArgs, VDict, AL),
	rename_goal_variables(Guards, Tests, Tests'?, Renames', Renames''?),
	rename_goal_variables(Goals, Initially, Initially'?,
				Renames'', Renames'''?),
	self;

   Inis = [] : IX = _, NA = _,
      L1 = L2,
      Tests = [],
      Initially = [],
      Renames = [] .

rename_goal_variables(Goals, I1, I2, R1, R2) :-

/* Body goals */

   Goals ? goal(Functor, Goal, State) :
      I1 ! goal(Functor, Goal'?, State) |
	rename_variables(Goal, Goal', R1, R1'?),
	self;

   Goals ? primitive(Goal) :
      I1 ! primitive(Goal'?) |
	rename_variables(Goal, Goal', R1, R1'?),
	self;

   Goals ? rpc(Target, Goal) :
      I1 ! rpc(Target'?, Goal'?) |
	rename_variables(Target, Target', R1, R1'?),
	rename_variables(Goal, Goal', R1', R1''?),
	self;

/* Guard goals */

   Goals ? listener(`VN) :
      I1 ! listener(`(RN?)),
      R1 ! rename(RN, VN) |
	self;

/* End of goals list */

   Goals = [] :
      I1 = I2,
      R1 = R2 .

rename_variables(T1, T2, R1, R2) :-

   T1 ? Car :
      T2 ! Car'? |
	rename_variables(Car, Car', R1, R1'?),
	self;

   T1 = `"_" :
      T2 = T1,
      R1 = R2 ;

   T1 = `VN, string(VN), VN =\= "_" :
      T2 = `(RN?),
      R1 = [rename(RN, VN) | R2] ;

   T1 = ?VN, string(VN) :
      T2 = ?(RN?),
      R1 = [rename(RN, VN) | R2] ;

   tuple(T1),
   otherwise,
   A := arity(T1),
   make_tuple(A, Tuple) :
      T2 = Tuple |
	replace_args;

   otherwise :
      T2 = T1,
      R1 = R2 .

replace_args(T1, T2, R1, R2, A) :-

   A-- > 0,
   arg(A, T1, A1),
   arg(A, T2, A2) |
	rename_variables(A1, A2, R2', R2),
	self;

   A =< 0 : T1 = _, T2 = _,
      R1 = R2 .


serve_requests(GlobalArgs, Last, Requests, Index, CallArgs, HT) :-

   Requests ? arguments(IX, BArgs, Args) |
	rename_arguments(IX, BArgs, Args, GlobalArgs, GlobalArgs', HT, HT'),
	self;

   Requests ? group(Index') : Index = _ |
	self;

   Requests ? rename(RN, VN) :
      HT ! lookup(VN(Index), RN, Old, Reply) |
	rename_argument(Reply, Last, VN, Old?, Last', RN),
	self;

   Requests = [] : Index = _, Last = _,
      CallArgs = GlobalArgs,
      HT = [] .      


rename_arguments(IX, BArgs, Args, G1, G2, H1, H2) :-

   BArgs ? Arg, Arg = _(Name),
   Args ? Arg,
   G1 ? Arg :
      G2 ! Arg,
      H1 ! lookup(Name(IX), Name, _, _new) |
	self;

   BArgs ? _(Name),
   Args ? TypeNew(Name),
   G1 ? TypeOld(Name),
   TypeOld =\= TypeNew :
      G2 ! ?Name,
      H1 ! lookup(Name(IX), Name, _, _new) |
	self;

   BArgs ? _(New),
   Args ? TypeNew(New),
   G1 ? TypeOld(Name), Name =\= New :
      H1 ! lookup(New(IX), Name, _, _new),
      G2 ! Annotated? |
	self,
	annotate_rename;

   BArgs ? TypeNew(New),
   Args = [_(Name) | _],
   New @< Name,				% therefore primed!
   G1 ? TypeOld(Old),
   string_to_dlist(Old, OL, [Prime]),
   string_to_dlist(New, NL, [Prime]) :
      ascii("'", Prime),
      H1 ! lookup(New(IX), Old, _, _new),
      BArgs'' = [TypeNew(New'?) | BArgs'],
      G1'' = [TypeOld(Old'?) | G1'] |
	list_to_string(OL, Old'),
	list_to_string(NL, New'),
	self;

   Args = [] : BArgs = _, IX = _, G1 = _,
      G2 = [],
      H1 = H2 .

annotate_rename(TypeOld, TypeNew, Name, Annotated) :-

   TypeOld = TypeNew :
      Annotated = TypeNew(Name);

   TypeOld =\= TypeNew :
      Annotated = ?Name .


rename_argument(Reply, L1, VN, Old, L2, RN) :-

   Reply = old : VN = _,
      RN = Old,
      L2 = L1 ;

   Reply = new,
   L1++,
   convert_to_string(L1', LS),
   string_to_dlist(LS, NL', NT),
   string_to_dlist(VN, NT', []) : Old = _,
      ascii('$', Dollar),
      NL ! Dollar,
      NT ! Dollar,
      L2 = L1' |
	list_to_string(NL, RN).


finally(Fins, GlobalArgs, NA, Tests, Finally) :-

   Fins = [] : GlobalArgs = _, NA = _,
      Tests = [],
      Finally = [] ;


   otherwise |
	finals(0, Fins, Tests, Finally, Lookups, Renames?, Renames),
	stream # hash_table(HT?),
	serve_requests(GlobalArgs, NA, Lookups?, 0, _CallArgs, HT).
	
finals(IX, Inis, Tests, Finally, L1, L2, Renames) :-

   Inis ? final(BaseArgs, Guards, Goals),
   IX++ :
      L1 ! arguments(IX', BaseArgs, BaseArgs),
      Renames ! group(IX') |
	utilities # append_list(Guards, Tests'?, Tests),
	rename_goal_variables(Goals, Finally, Finally'?, Renames', Renames''?),
	self;

   Inis = [] : IX = _, 
      Tests = [],
      Finally = [],
      L1 = L2,
      Renames = [] .
