-language(compound).
-export([procedures]).


/* procedures/3
**
** Expand edited procedures by inheritance.
*/

procedures(Edited, Expanded, Errors) :-

   Edited ? descriptor(SId, Exports, PN),
   SId = [Name | _] :
      Expanded ! descriptor(Exports, PN) |
	stream # hash_table(HT?),
	prepare(Edited', Name, Name, Errors, Augmented, HT, HT'?, HT', HT''?),
	inherit(Augmented, Expanded', HT'').

/*
** Notes:
**
** NG = Number of global arguments.
** NA = Total number of arguments.
*/

prepare(Edited, PName, MName, Errors, Augmented, D1, D2, D3, D4) :-

   Edited ? descriptor(SId),
   SId = [MName' | _] : MName = _ |
	self;

   Edited ? proc(Functor, NG, Args, RHs, State),
   State = _Status(Id) :
      D1 ! lookup(Functor(Id), New?, _None, Reply),
      Augmented ! proc(Functor, NG, Args, NA?, RHs'?, State),
      New = MName(NG, Args, NA?, RHs'?) |
	NA := length(Args) + InheritedLocals?,
	self,
	rhs(PName(MName, Functor, NG, Args), RHs, RHs', InheritedLocals, D3, D3'?,
		Errors, Errors'?
	),
	reply(Reply, new, lookup(Functor(Id)));

   Edited ? Declaration, Declaration = typedef(_) :
      Augmented ! Declaration |
	self;

   Edited = [] : PName = _, MName = _,
      Augmented = [],
      D1 = D2,
      D3 = D4,
      Errors = [] .

rhs(PId, In, Out, ILs, D1, D2, E1, E2) :-

   In ? RHS, RHS = rhs(Guard, Body, ITree, STree), ITree = tree(VDict) :
      Out ! rhs(Guard, Body'?, STree) |
	expand_implicit(Body, Body', VDict, D1, D1'?),
	self;

   In ? RHS, RHS = rhs(Guard, Body, None, STree), None =\= tree(_) :
      Out ! rhs(Guard, Body, STree) |
	self;

   In ? Inherit, Inherit = _inherit(Functor, _NR, _Renames, State),
   State = _Status(Id),
   PId = _PName(_MName, _Functor, _Arity, PArgs) :
      D1 ! member(Functor(Id), Data, Reply) |
	self,
	verify_inherit,
	add_inherited_locals(Result?, FromName?, PId, ILs'?, ILs,
				Out, Out'?, E1, E1'?
	),
	reply(Reply?, true, member(Functor(Id)));

   In = [] : PId = _,
      Out = [],
      ILs = 0,
      D1 = D2,
      E1 = E2 .

verify_inherit(Reply, Data, Inherit, PArgs, Result, FromName) :-

   Reply =\= true : Data = _, Inherit = _, PArgs = _,
      Result = ng, FromName = "?";			% Weird?

   Reply = true,
   Inherit = _inherit(_Functor, -1, _nil, _State) : NI = _,
      Args = IArgs? |
	subset;

   Reply = true,
   Inherit = _inherit(_Functor, NR, Args, _State), NR =\= -1 : IArgs = _ |
	subset,
	right_length(NI?, NR, Inherit, Result'?, Result).

right_length(NI, NR, Inherit, R1, R2) :-

   NI = NR : Inherit = _,
      R2 = R1 ;

   otherwise : NI = _, NR = _, R1 = _,
      R2 = ng(Inherit) .

subset(Inherit, PArgs, Data, Args, NI, IArgs, Result, FromName) :-
   Data = Name(NG, EArgs, NA, _RHs) :
      FromName = Name,
      NI = NG,
      IArgs = EArgs?,
      Result = {Ok?, Inherit, NG, NA},
      Previous = true,
      State = ok |
	subset1.
subset1(Previous, PArgs, NG, Args, State, Ok) :-

   Previous = true,
   NG-- > 0,
   Args ? Arg |
	member(Arg, PArgs, Previous'),
	self;

   Previous = true,
   NG =< 0 : Args = _, PArgs = _,
      Ok = State;

   Previous = true,
   Args = [] : NG =_, PArgs = _ |
      Ok = State;

   Previous = ng(Arg),
   State = ok :
      Ok = ng(Ok'?),
      Ok' ! Arg,
      Previous' = true,
      State' = [] |
	self;

   Previous = ng(Arg),
   State = [] :
      Ok ! Arg,
      Previous' = true |
	self.

member(Arg, PArgs, Previous) :-

   PArgs ? Arg : PArgs' = _,
      Previous = true ;

   PArgs ? Other, Other =\= Arg |
	self;

   PArgs = [] :
      Previous = ng(Arg) .

add_inherited_locals(Result, FromName, PId, RLs, TLs, Out1, Out2, E1, E2) :-

   Result = ok(Inherit, NG, NA),
   ALs := NA - NG : FromName = _, PId = _,
      Out1 = [Inherit | Out2],
      E1 = E2 |
	TLs := RLs + ALs;

   Result = ng(Inherit),
   Inherit = _inherit(IF, NG, _, _) :
      TLs = RLs,
      Out1 = Out2,
      Diagnostic = argument_count_mismatch,
      E1 = [Error | E2]  |
	diagnostic_names;

   Result = {ng(Args), Inherit, NG, _},
   Inherit = _inherit(IF, _, _, _) :
      TLs = RLs,
      Out1 = Out2,
      Diagnostic = not_in_subclass(Args),
      E1 = [Error| E2] |
	diagnostic_names;

   Result = ng : FromName = _, PId = _, 		% Weird?
      TLs = RLs,
      Out1 = Out2,
      E1 = E2 .

diagnostic_names(FromName, PId, IF, NG, Diagnostic, Error) :-

   PId = FromName(FromName, Functor, Arity, _PArgs) :
      Error = Functor/Arity - inheriting - IF/NG -Diagnostic ;

   PId = FromName(ModuleName, Functor, Arity, _PArgs),
   ModuleName =\= FromName :
      Error = ModuleName#Functor/Arity - inheriting - IF/NG - Diagnostic ;

   PId = PrimaryName(PrimaryName, Functor, Arity, _PArgs),
   PrimaryName =\= FromName :
      Error = Functor/Arity - inheriting - FromName#IF/NG - Diagnostic ;

   PId = PrimaryName(ModuleName, Functor, Arity, _PArgs),
   PrimaryName =\= ModuleName,
   PrimaryName =\= FromName :
      Error = ModuleName#Functor/Arity - inheriting - FromName#IF/NG - Diagnostic .


expand_implicit(Body1, Body2, VDict, D1, D2) :-

   Body1 ? goal(Functor, Functor, State),
   State = _Status(Id) :
      D1 ! member(Functor(Id), Data, Reply),
      Body2 ! goal(Functor, Call?, State) |
	self,
	expand_implicit_call,
	reply(Reply?, true, member(Functor(Id)));

   Body1 ? implicit_rpc(Service, Functor, Id) :
      D1 ! member(Functor(Id), Data, Reply),
      Body2 ! rpc(Service, Call?) |
	self,
	expand_implicit_call,
	reply(Reply?, true, member(Functor(Id)));

   Body1 ? Other,
   otherwise :
      Body2 ! Other |
	self;

   Body1 = [] : VDict = _,
      Body2 = [],
      D1 = D2 .

expand_implicit_call(Reply, Data, VDict, Functor, Call) :-

   Reply = true,
   Data = _Name(NG, Args, _NA, _RHs) :
      List ! Functor |
	utilities # primed_arguments(NG, Args, VDict, List'),
	list_to_tuple(List?, Call);

   Reply =\= true : Data = _, VDict = _,
      Call = Functor .


inherit(Augmented, Expanded, Dict) :-

   Augmented ? proc(Functor, NG, As, NA, RHs, State) |
/*
** Gs	All previous arguments (global and local - last local un-numbered)
** Rs	This set of arguments (global renames)
** As   This set of arguments (internal names - NG globals, rest local)
** NG	Number of Global Arguments (this set)
** Gs'	All arguments, including this set
** Is	Iterative arguments (transformed globals | locals | remnant numbered)
** Ns	Numbered (output)
** Ns'	Next numbered (input)
*/
	extend_head([], [], As, 0, Gs, Is, Ns, Ns'?),
	global_procedure(Content?, NG, NA, As, Ns?, Inis?, State, Functor, Functor',
				Expanded, Expanded'?
	),
	expand_procedure(Functor'?, Is?, NA, Fins?, RHs, State, Content,
			 Gs?, _, Ns', [], Inis, [], Fins,  [],
			 Expanded', Expanded''?, Dict, Dict'?
	),
	self;

   Augmented ? Other, Other =\= proc(_,_,_,_,_,_) :
      Expanded ! Other |
	self;

   Augmented = [] :
      Expanded = [],
      Dict = [] .


extend_head(Gs1, Rs, As, NG, Gs2, Is, Ns, Rest) :-
	extend_head1(Gs1, Rs, As, NG, Gs2, Is, Ns, Rest, 0).
extend_head1(Gs1, Rs, As, NG, Gs2, Is, Ns, Rest, N) :-

   Gs1 ? GN,
   string(GN),
   N++ :
      Gs2 ! GN,
      Is ! `GN |
	self;

   Gs1 ? numbered(GN, NGN),
   N++ :
      Gs2 ! GN'?,
      Is ! `(Renamed?) |
	self,
	rename_arg(GN, NGN, NG, NG', Rs, Rs', As, As', GN', Renamed);

   Gs1 = [],
   NG = 0 : Rs = _ |			% testing (remove later)
	number_arguments.

rename_arg(GN1, NGN, NG1, NG2, Rs1, Rs2, As1, As2, GN2, Renamed) + (I = 0) :-

   I < NG1,
   Rs1 ? _(GN1),
   As1 ? _(IN),
   NG1-- :
      NG2 = NG1',
      Rs2 = Rs1',
      As2 = As1',
      GN2 = numbered(IN, NGN),
      Renamed = IN;

   I++ < NG1,
   Rs1 ? ON,
   As1 ? AN, ON =\= _(GN1) :
      Rs2 ! ON,
      As2 ! AN |
	self;

   I >= NG1 : GN1 = _,
      NG2 = NG1,
      Rs2 = Rs1,
      As2 = As1,
      GN2 = NGN,
      Renamed = NGN .

number_arguments(As, N, Gs2, Is, Ns, Rest) :-

   As ? A, A = _(AN),
   N++,
   convert_to_string(N', SN),
   string_to_dlist(SN, NL', NT),
   string_to_dlist(AN, NT', []) :
      Gs2 ! numbered(AN, NAN?),
      Is ! A,
      ascii('$', Dollar),
      NL ! Dollar,
      NT ! Dollar,
      Ns ! `(NAN?) |
	self,
	list_to_string(NL, NAN);

   As = [] : N = _,
      Gs2 = [],
      Is = Rest,
      Ns = Rest .


global_procedure(Content, NG, NA, BaseArgs, Ns, Inis, State,
		 Functor1, Functor2, E1, E2
) :-

   NG = NA,
   Inis = [] : BaseArgs = _, Ns = _, State = _, Content = _,
      Functor2 = Functor1,
      E1 = E2 ;

   Content = false : NG = _, NA = _, BaseArgs = _, Ns = _, Inis = _, State = _,
      Functor2 = Functor1,
      E1 = E2 ;

   otherwise : Content = _,
      RHs = [rhs(Guard?, [goal(Functor2?, Iterate?, State) | Initially?])],
      E1 = [proc(Functor1, NG, ExternalArgs?, RHs, State) | E2] |
	global_arguments,
	global # initially(GlobalArgs?, NA, Inis, Guard, Initially, CallArgs),
	global_procedure_head,
	list_to_tuple([Functor2? | CallArgs?], Iterate).

global_arguments(NG, BaseArgs, Ns, ExternalArgs, GlobalArgs) :-

   NG-- > 0,
   BaseArgs ? Arg,
   Ns ? _ :
      GlobalArgs ! Arg,
      ExternalArgs ! Arg |
	self;

   NG =< 0 : BaseArgs = _,
      GlobalArgs = Ns,
      ExternalArgs = [] .

global_procedure_head(Functor1, NG, NA, Functor2) :-

   NG >= NA,
   string_to_dlist(Functor1, FL', []) :
      ascii('_', UL),
      FL ! UL |
	list_to_string(FL, Functor2);

   NG < NA :
      Functor2 = Functor1 .


expand_procedure(Functor, Is, NA, Finally, RHs, State, Content,
		 Gs1, Gs2, Ns1, Ns2, I1, I2, F1, F2, E1, E2, D1, D2
) :-

   RHs ? rhs([initially | Tests], InitialGoals, STree) :
      I1 ! initial(Is, Tests, InitialGoals, STree) |
	self;

   RHs ? rhs([finally | Tests], FinalGoals, _STree) :
      F1 ! final(Is, Tests, FinalGoals) |
	self;

   RHs ? rhs(Guard, Goals, STree),
   Guard =\= [initially | _], Guard =\= [finally | _] : Content' = _,
      Content = true,
      E1 ! proc(Functor, NA, Is, [rhs(Guard'?, Goals'?)], State) |
	final_or_self(Functor, STree, Is, NA, Finally, State,
			Guard, Guard', Goals, Goals'),
	self;

   RHs ? inherit(Name, _NI, Renames, _(PId)) :
      D1 ! member(Name(PId), Data, Ok) |
	Data? = _Name(NG, Args, _NA, IRHs),
	ok(Ok?, inherit_member),
	inherit_renames(Args?, Renames, Renames'),
	extend_head(Gs1, Renames'?, Args?, NG?, GsA, NIs, Ns1, Ns1'?),
	expand_procedure(Functor, NIs?, NA, Finally, IRHs, State, Content1,
			 GsA?, GsN, Ns1', Ns1''?,
			 I1, I1'?, F1, F1'?, E1, E1'?, D1', D1''?
	),
	utilities # or(Content1?, Content'?, Content),
	adjust_head_arguments(Gs1, GsN?, Gs1'),
	self;

   RHs = [] : Functor = _, Is = _, NA = _, Finally = _, State = _,
      Content = false,
      Gs2 = Gs1,
      Ns1 = Ns2,
      I1 = I2,
      F1 = F2,
      E1 = E2,
      D1 = D2 .

adjust_head_arguments(Gs1, GsN, Gs2) :-

   Gs1 ? G,
   GsN ? _ :
      Gs2 ! G |
	self;

   Gs1 = [],
   GsN ? NGN, string(NGN) :
      Gs2 ! NGN |
	self;

   Gs1 = [],
   GsN ? numbered(_, NGN) :
      Gs2 ! NGN |
	self;

   Gs1 = [],
   GsN = [] :
      Gs2 = [] .

inherit_renames(Args, Renames1, Renames2) :-

   Renames1 = [] :
      Renames2 = Args ;

   Renames1 =\= [] : Args = _,
      Renames2 = Renames1 .


final_or_self(Functor, STree, IterArgs, NA, Finally, State,
		Guard1, Guard2, Goals1, Goals2
) :-

   STree =\= tree(_) : Functor = _, Finally = _, State = _,
   Finalize = false |
	finalize;

   STree = tree(VDict) : Finally = _,
      Out ! Functor,
      IterateGoal = goal(Functor, Iterate?, State),
      Guard2 = Guard1 |
	utilities # primed_arguments(NA, IterArgs, VDict, Out'),
	list_to_tuple(Out?, Iterate),
	replace_self.

finalize(Goals1, Finalize, IterArgs, NA, Finally, Guard1, Guard2, Goals2) :-

   Goals1 ? primitive(finalize), Finalize = false :
     Finalize' = true |
	self;

   Goals1 ? Goal,
   otherwise :
      Goals2 ! Goal |
	self;

   Goals1 = [], Finalize = false : Finally = _, IterArgs = _, NA = _,
      Guard2 = Guard1,
      Goals2 = [] ;

   Goals1 = [], Finalize = true |
	global # finally(Finally, IterArgs, NA, Tests, Goals2),
	utilities # append_list(Guard1, Tests?, Guard2).

replace_self(IterateGoal, Goals1, Goals2) :-

   Goals1 ? Other, Other =\= primitive(self) :
      Goals2 ! Other |
	self;

   Goals1 ? primitive(self) :
      Goals2 = [IterateGoal | Goals1'] .


reply(Reply, ShouldBe, Function) :-

   Reply = ShouldBe : Function = _ ;

   Reply =\= ShouldBe |
	computation # event(("For function" : Function ; "Received" - Reply,
				"Expecting" - ShouldBe)).
