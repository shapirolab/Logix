/* $Header: /home/qiana/Repository/Logix/system/transform/lpi/remote.cp,v 1.1 1999/07/09 07:03:17 bill Exp $ */
/*

Remote inheritance for lpi

  Yaron Goldberg - March 1991.

  Updated for parametric Inheritance, Yaron Goldberg - May 1991

  Updated for full variable list, Yaron Goldberg - June 1991

  Updated for revised syntax and revised RPC semantics, Bill Silverman -
						   November 1991

*/


-language(compound).
-export([search/11, filter_calls/2]).
-mode(trust).

search(ModuleName, Current, SubName, N, Result, WithArgs,
		Rscs, Rscs1, IdC, IdZ, Diags
) :-

    N < 0 : ModuleName = _, Current = _, SubName = _, N = _, WithArgs = _,
      Result = notfound_error,
      Diags = Errors(Errors),
      IdZ = IdC,
      Rscs = Rscs1;

    N = 0 |
	search_implicit(ModuleName, Current, SubName, Inherit, Rscs, WithArgs),
	located_procedure(ModuleName, Current, SubName, N, Result, WithArgs,
				Rscs, Rscs1, IdC, IdZ, Diags, Inherit
	);

    N > 0 |
	search_explicit(ModuleName, Current, SubName, N, Inherit, Rscs,
			 WithArgs
	),
	located_procedure(ModuleName, Current, SubName, N, Result, WithArgs,
				Rscs, Rscs1, IdC, IdZ, Diags, Inherit
	).


search_implicit(ModuleName, Current, SubName, Result, Rscs, WithArgs)
	+ (Found = [], Inherit = _) :-

    Rscs ? Rsc,
    Rsc = rsc(ModuleName#SubName, Head, Locals, RHSS, Depends),
    N := arity(Head) - 1,
    WithArgs = false : Inherit = _,
      Found = [N | Found],
      Inherit' = inherit(novars, [Head, Locals, RHSS], Current, Depends) |
	search_implicit;

    Rscs ? rsc(ModuleName#SubName, Head, Locals, RHSS, Depends),
    N := arity(Head) - 1,
    WithArgs = localonly(Args) : Inherit = _,
      Found = [N | Found],
      Inherit' = inherit(localonly, [Args, Head, Locals, RHSS],
			 Current, Depends
		 ) |
	search_implicit;

    Rscs ? _Rsc,
    otherwise |
	search_implicit;

    Rscs = [], Found = [] : ModuleName = _, Current = _, SubName = _,
			     WithArgs = _, Inherit = _,
      Result = false ;
	
    Rscs = [], Found = [_] : ModuleName = _, Current = _, SubName = _,
			      WithArgs = _,
      Result = Inherit ;

    Rscs = [], Found = [_,_|_] : ModuleName = _, Current = _, SubName = _,
				  WithArgs = _, Inherit = _,
      Result = ambiguous(SubName/Found) .



search_explicit(ModuleName, Current, SubName, N, Result, Rscs, WithArgs) :-

    Rscs ? rsc(ModuleName#SubName, Head, Locals, RHSS, Depends),
    N =:= arity(Head),
    WithArgs = false : Rscs' = _,
      Result = inherit(novars, [Head, Locals, RHSS], Current, Depends);

    Rscs ? rsc(ModuleName#SubName, Head, Locals, RHSS, Depends),
    N =:= arity(Head),
    WithArgs = Mode(Args) : Rscs' = _,
      Result = inherit(Mode, [Args, Head, Locals, RHSS], Current, Depends);

    Rscs ? rsc(ModuleName#SubName, Head, Locals, RHSS, Depends),
    N =:= arity(Head),
    WithArgs = all(Args, Local) : Rscs' = _,
      Result = inherit(all, [Args, Local, Head, Locals, RHSS],
		       Current, Depends
	       );

    otherwise,
    Rscs ? rsc(_, _, _, _, _) |
      search_explicit;

    Rscs = [] : ModuleName = _, SubName = _, N = _,  WithArgs = _, Current = _,
      Result = false .


located_procedure(ModuleName, Current, SubName, N, Result, WithArgs,
			Rscs, Rscs1, IdC, IdZ, Diags, Inherit
) :-

    Inherit = false,
    Diags = Errors(Errors1) :
      write_channel(get_module(ModuleName, Status, Errors, Errors'), IdC) |
	status(Status?, Estate),
	locate_subclass(SubName, N, Estate, Result0),
	subclass_result(ModuleName, Current, SubName, N, WithArgs, Result0?,
			Status?, Rscs, Rscs1, IdC, IdZ, Errors', Errors1,
			Result
	);

    Inherit = ambiguous(SubName/Arities) : Current = _, N = _, WithArgs = _,
      Rscs1 = Rscs,
      IdZ = IdC,
      Diags = Errors(Errors'),
      Errors ! diagnostic("ambiguous remote implicit reference" -
				ModuleName#SubName - Arities
	       ),
      Result = notfound_error ;

    Inherit = inherit(_,_,_,_) : ModuleName = _, Current = _,
				 SubName = _, N = _, WithArgs = _,
      Rscs1 = Rscs,
      IdZ = IdC,
      Diags = Errors(Errors),
      Result = Inherit .

status(Status, Estate) :-

    Status = module(Estate^, _Exports) |
	true;

    Status =\= module(_, _) :
      Estate = [] .

/***
* Find the Current Rsc in the list of heritable procs of the remote module.
***/

locate_subclass(SubName, N, Estate, Result) + (Found = [], Inherit = _) :-

    Estate ? Proc,
    N > 0,
    Proc = _sub(Head, _, _, _),
    N =:= arity(Head),
    arg(1, Head, SubName) : Estate' = _, Found = _, Inherit = _,
      Result = Proc ;

    Estate ? Inherit',
    N = 0,
    Inherit' = _sub(Head, _, _, _),
    arg(1, Head, SubName),
    A := arity(Head) : Inherit = _,
      Found' = [A | Found] |
	locate_subclass;

    Estate ? _,
    otherwise |
	locate_subclass;

    Estate = [], Found = [] : SubName = _, N = _, Inherit = _,
      Result = notfound_error ;

    Estate = [], Found = [_] : SubName = _, N = _ |
      Result = Inherit ;

    Estate = [], Found = [_,_|_] : N = _, Inherit = _,
      Result = ambiguous(SubName/Found) .


subclass_result(ModuleName, Current, SubName, N, WithArgs, Result, Status,
		Rscs, Rscs', IdC, IdZ, Errors, Errors', Result'
) :-

    Result = notfound_error,
    Status =\= false(_) : Current = _, WithArgs = _,
      Rscs' = Rscs,
      IdZ = IdC,
      Errors ! diagnostic("remote subclass not found" - Identifier),
      Result' = Result |
	format_identifier(ModuleName, SubName, N, Identifier);

    Result = notfound_error,
    Status = false(Diagnostic) : Current = _, SubName = _, N = _,WithArgs = _,
      Rscs' = Rscs,
      IdZ = IdC,
      Errors ! diagnostic(Diagnostic - ModuleName),
      Result' = Result ;

    Result = sub(Head, Local, RHSS, Depends),
    WithArgs = false : SubName = _, N = _,
      Rscs' = [rsc(ModuleName#SubName, Head, Local, RHSS', Depends) | Rscs],
      Errors = Errors',
      Result' = inherit(novars, [Head, Local, RHSS'],
			Current, [ModuleName | Depends]
		) |
	edit_bodies(Status, ModuleName, SubName, RHSS, RHSS', IdC, IdZ);

    Result = sub(Head, Local, RHSS, Depends),
    WithArgs = Mode(Args) : SubName = _, N = _,
      Rscs' = [rsc(ModuleName#SubName, Head, Local, RHSS', Depends) | Rscs],
      Errors = Errors',
      Result' = inherit(Mode, [Args, Head, Local, RHSS'],
			Current, [ModuleName | Depends]
		) |
	edit_bodies(Status, ModuleName, SubName, RHSS, RHSS', IdC, IdZ);

    Result = sub(Head, Locals, RHSS, Depends),
    WithArgs = all(Args, Local) : SubName = _, N = _,
      Rscs' = [rsc(ModuleName#SubName, Head, Locals, RHSS', Depends) | Rscs],
      Errors = Errors',
      Result' = inherit(all, [Args, Local, Head, Locals, RHSS'],
			Current, [ModuleName | Depends]
		) |
	edit_bodies(Status, ModuleName, SubName, RHSS, RHSS', IdC, IdZ);

    Result = ambiguous(SubName/Arities) : Current = _, N = _,
				       WithArgs = _, Status = _,
      Rscs' = Rscs,
      IdZ = IdC,
      Errors ! diagnostic("ambiguous remote implicit reference"
				- ModuleName#SubName - Arities
	       ),
      Result' = notfound_error .
      
format_identifier(ModuleName, SubName, N, Identifier) :-

    N = 0 :
      Identifier = ModuleName # SubName ;

    N-- =\= 0 :
      Identifier = ModuleName # SubName/N' .


edit_bodies(Status, M, F, RHSS1, RHSS2, IdC1, IdC2) :-

    RHSS1 = (RHS ; RHSS1'),
    RHS = (_;_) :
      RHSS2 = (RHS'; RHSS2') |
	edit_bodies(Status, M, F, RHS, RHS', IdC1, IdC1'),
	edit_bodies;

    RHSS1 = (RHS ; RHSS1'),
    RHS =\= (_;_) :
      RHSS2 = (RHS'; RHSS2') |
	edit_body(Status, M, F, RHS, RHS', IdC1, IdC1'),
	edit_bodies;

    RHSS1 =\= (_;_) |
	edit_body(Status, M, F, RHSS1, RHSS2, IdC1, IdC2).

edit_body(Status, M, F, RHS1, RHS2, IdC1, IdC2) :-

    RHS1 = (Guard | Body) :
      RHS2 = (Guard | Body') |
	edit_goals(Status, M, F, Body, Body', IdC1, IdC2);

    RHS1 = (_ : _) : Status = _, M = _, F = _,
      RHS2 = RHS1,
      IdC2 = IdC1 ;

    RHS1 =\= (_ | _), RHS1 =\= (_ : _) |
	edit_goals(Status, M, F, RHS1, RHS2, IdC1, IdC2).


edit_goals(Status, M, F, Body1, Body2, IdC1, IdC2) :-

    Body1 = (Goal, Body1') :
      Body2 = (Goal', Body2') |
	edit_goals(Status, M, F, Goal, Goal', IdC1, IdC1'),
	edit_goals;

    Body1 =\= (_, _) |
	edit_goal(local, Status, M, F, Body1, Body2, IdC1, IdC2).


edit_goal(Locus, Status, M, F, Body1, Body2, IdC1, IdC2) :-

    string(Body1), Body1 =\= F, Body1 =\= self, Body1 =\= true :
      write_channel(find(Body1, Reply), IdC1, IdC2) |
	local_goal;

    Body1 = (Functor + _Associates),
    string(Functor), Functor =\= F, Functor =\= self, Functor =\= true :
      write_channel(find(Functor, Reply), IdC1, IdC2) |
	local_goal;

    Body1 = (Body1' @ Processor) :
      Body2 = (Body2' @ Processor) |
	self;

    Body1 = (-M' # Body1'),
    string(M'), Locus = local : M = _,
      Locus' = inherited |
	self;

    tuple(Body1),
    otherwise,
    arg(1, Body1, Functor), Functor =\= F,
    N := arity(Body1) :
      write_channel(find(Functor, Reply), IdC1, IdC2) |
	explicit_goal(N, Reply, Reply'),
	local_goal;

    otherwise, Locus = local : Status = _, M = _, F = _,
      Body2 = Body1,
      IdC2 = IdC1 ;

    otherwise : Locus = _, Status = _, M = _, F = _,
      Body2 = Body1,
      IdC2 = IdC1 .

explicit_goal(N, Reply1, Reply2) :-

    Reply1 = found(N) :
      Reply2 = Reply1;

    Reply1 = found(L), L ? N : L' = _,
      Reply2 = found(N) ;

    Reply1 = found(L), L ? M, M =\= N :
      Reply1' = found(L') |
	explicit_goal;

    otherwise : N = _, Reply1 = _,
      Reply2 = notfound .

local_goal(Locus, Status, M, Body1, Body2, Reply) :-

% Body is defined locally !
    Reply = found(_) : Locus = _, Status = _, M = _,
      Body2 = Body1 ;

% Body is not defined locally - look for remote definition !
    Reply = notfound,
    Locus = inherited : Status = _,	% Already found once
      Body2 = (-M#Body1) ;

    Reply = notfound,
    Locus =\= inherited,
    Status = module(_Estate, Exports) |
	search_ids(Body1, Exports, Exported),
	exported_goal(Exported, M, Body1, Body2).

search_ids(Goal, Es, Reply) :-

    Es ? Goal/_ : Es' = _,
      Reply = true ;

    Es ? Functor/_, Goal = (Functor + _) : Es' = _,
      Reply = true ;

    Es ? F/N,
    N =:= arity(Goal) - 1,
    arg(1, Goal, F) : Es' = _,
      Reply = true ;

    Es ? _Other,
    otherwise |
	search_ids;

    Es = [] : Goal = _,
      Reply = false .

exported_goal(true, M, Goal, (-M#Goal)^).
exported_goal(false, _, Goal, Goal^).


filter_calls(In, Out) :-

    In ? (Head :- Rhs1) :
      Out ! (Head :- Rhs2?) |
	self,
	filter_calls_rhss;

    In ? Other, Other =\= (_ :- _) :
      Out ! Other |
	self;

    In = [] :
      Out = [] .

filter_calls_rhss(Rhs1, Rhs2) :-

    Rhs1 = (Rhs1' ; R1) :
      Rhs2 = (Rhs2'? ; R2?) |
	self,
	filter_calls_rhss(R1, R2);

    Rhs1 = (Guard | Body1) :
      Rhs2 = (Guard | Body2?) |
	filter_calls_body;

    Rhs1 =\= (_;_), Rhs1 =\= (_|_) |
	filter_calls_body(Rhs1, Rhs2).


filter_calls_body(Body1, Body2) :-

    Body1 = (Body1' , B1) :
      Body2 = (Body2'? , B2?) |
	self,
	filter_calls_body(B1, B2);

    Body1 = (-M#G), string(M) :
      Body2 = (M#G) ;

    otherwise :
      Body2 = Body1 .	
