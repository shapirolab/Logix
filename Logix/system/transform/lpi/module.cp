/* $Header: /home/qiana/Repository/Logix/system/transform/lpi/module.cp,v 1.1.1.1 1999/07/09 07:03:18 bill Exp $ */
/*

Module transformation for lpi - logic programs with inheritance.

  Yaron Goldberg - March 1991.

  Updated for parametric Inheritance, Yaron Goldberg - May 1991

  Updated for full variable list, Yaron Goldberg - June 1991

  Updated for revised syntax and revised remote super class semantics,
				  Bill Silverman - November 1991

*/


-language(compound).
-export([clauses/6]).
-mode(trust).

/***
* four phases in the transformation:
* 1. clauses - transform program clauses into terms (for convenience)
* 2. group - group clauses of the same procedure into lists
* 3. filter_non_compound - filters clauses which cannot inherit or be
*	inherited.
* 4. main loop, resolve inheritance graph.
*
*
* Three important list are processed by the main loop:
*
*   "Subs" list of all procedures with no remaining super classes.
*
*   	Subs ::= [Proc | Subs] ; []
*
*	Proc ::= sub(Head, Locals, RHSS, Depends) .
*
*   "Classes" list of procedures that still need super classes.
*
*    	Classes ::= [Class | Classes] ; []
*
*	Class ::= class(Head, Locals, Inherits, RHSS, Terms-Terms', Depends) .
*
*   "Rscs" list which is [] at first and contains all remote subclass
*    calls analysed so far.
*
*	Rscs ::= [Rsc | Rscs] ; []
*
*	Rsc  ::= rsc(ModuleName#SubName, Head, Locals, RHSS, Depends)
*
***/

clauses(Clauses, Terms, Classified, IdC, IdZ, Diags) :-
        clauses_to_terms(Clauses, Tuples, Terms, IdC, IdC'),
	group(Tuples, Groups),
	filter_non_compound(Groups, Subs, Classes, Diags, Diags'),
	lpi(Subs, Classes, Classified, IdC', IdZ, Diags').


/***
* Transform clauses into terms, filter type declarations.
***/

clauses_to_terms(Clauses, Out, Terms, IdC1, IdC2) :-

    Clauses ? (Head + Local :- RHSS), RHSS =\= true, Local =\= [] :
      Out ! proc(Head', Local, RHSS', Inherits, Terms-Terms')  |
	normalize(Head, Head', IdC1, IdC1'),
	heritage(RHSS, RHSS', Inherits, []),
        clauses_to_terms;

    Clauses ? (Head :- RHSS), RHSS =\= true,
    Head =\= (_ + _) :
      Out ! proc(Head', [],  RHSS', Inherits, Terms-Terms') |
	heritage(RHSS, RHSS', Inherits, []),
	normalize(Head, Head', IdC1, IdC1'),
 	clauses_to_terms;

    Clauses ? (Atom :- true),
    Atom =\= (_ + _) :
      Terms ! Atom,
      Out ! proc(Atom', [], true) |
	normalize(Atom, Atom', IdC1, IdC1'),
 	clauses_to_terms;

    Clauses ? (Atom + Local :- true), Local =\= [] :
      Terms ! (Atom + Local),
      Out ! proc(Atom', Local, true) |
	normalize(Atom, Atom', IdC1, IdC1'),
 	clauses_to_terms;

    Clauses ? (Atom + Local), Local =\= [] :
      Terms ! (Atom + Local),
      Out ! proc(Atom', Local, true) |
	normalize(Atom, Atom', IdC1, IdC1'),
 	clauses_to_terms;

    Clauses ? Atom,
    Atom =\= (_ + _),
    Atom =\= (_ :- _),
    Atom =\= (_ ::= _),
    Atom =\= (procedure _) :
      Terms ! Atom,
      Out ! proc(Atom', [], true) |
	normalize(Atom, Atom', IdC1, IdC1'),
 	clauses_to_terms;

   otherwise,
   Clauses ? Other |
	Terms ! Other, 
   	clauses_to_terms;

   Clauses = [] :
     Out = [],
     Terms = [],
     IdC2 = IdC1 .

heritage(RHSS, RHSS1, Inherits, Rest) :-
	rhss_to_dlists(RHSS, Inherits, Rest, RHL, []),
	clause_list_to_rhss(RHL, RHSS1).

/***
* Take incoming tuples and group them into groups (lists) containing
* clauses of the same procedure.
***/

group(In, Out) + (Name = "_lpi-null", Args = (-1), Group=[]) :-

    In ?  Proc,
    arg(2, Proc, Head),
    arity(Head, Args),
    arg(1, Head, Name) :
	Group' = [Proc | Group] |
	group;

    otherwise,
    In ?  Proc,
    arg(2, Proc, Head),
    arity(Head, Args'),
    arg(1, Head, Name') :
        Out ! Group, Args = _, Name = _,
    	Group' = [Proc] |
    	group;

    In = [],
    Group = [] : Out = [], Name = _, Args = _;

    In = [],
    Group =\= [] : Out = [Group], Name = _, Args = _.



/***
* Filter clauses which cannot inherit or be inherited, send them and
* the members of Subs List to next transformer
***/

filter_non_compound(In, Subs, Classes, Diags, Diags1) :-

    In ? [] |
	filter_non_compound;

    In ? [proc(Atom, Local, true)] |
	check_head(Atom, Local, Locals, Ok),
	to_subs(Ok, sub(Atom, Locals, true, []), Subs, Subs'),
	filter_non_compound;

    In ? [proc(Atom, [], RHSS, [], Terms-Terms')],
    RHSS =\= true :
      Terms ! (Atom' :- RHSS) |
	restore(Atom, Atom'),
	check_head(Atom, [], Locals, Ok),
	to_subs(Ok, sub(Atom, Locals, RHSS, []), Subs, Subs'),
	filter_non_compound;

    In ? [proc(_Atom, [], true, [], (Terms-Terms)^)] |
% Only bad inheritance - no local or right-hand-side.
% (see not_found_errors/3 and include/7 below.)
	filter_non_compound;

    In ? [proc(Atom, Local, RHSS, [], Terms-Terms')],
    Local =\= [], RHSS =\= true :
      Terms ! (Atom' + Local :- RHSS) |
	restore(Atom, Atom'),
	check_head(Atom, Local, Locals, Ok),
	to_subs(Ok, sub(Atom, Locals, RHSS, []), Subs, Subs'),
	filter_non_compound;

    In ? [proc(Atom, Local, true, [], Terms-Terms')],
    Local =\= [] :
      Terms ! (Atom' + Local) |
	restore(Atom, Atom'),
	check_head(Atom, Local, Locals, Ok),
	to_subs(Ok, sub(Atom, Locals, true, []), Subs, Subs'),
	filter_non_compound;

    In ? [proc(Atom, Local, RHSS, Inherits, DTs)],
    Inherits =\= [] |
	check_head(Atom, Local, Locals, Ok),
	to_classes(Ok, class(Atom, Locals, RHSS, Inherits', DTs, []),
		   Atom, DTs,
		   Inherits, Inherits', Classes, Classes', Diags, Diags'
	),
	filter_non_compound;

    In ? Procs,
    Procs = [_, _ |_] |
	cannot_inherit(Procs, Diags, Diags'),
	filter_non_compound;

    In = [] :
      Classes = [],
      Subs = [],
      Diags = Diags1 .

/***
* Check Head and Local for normal compound structure.
***/

check_head(Atom, Local, Locals, Ok) :-

	check_local(Local, LVars, Locals),
	tuple_to_dlist(1, Atom, Vars, LVars),
	servers # dictionary(In?),
	is_diff(Vars?, In, Ok).


check_local(Local, LVars, Locals) :-

    Local = (Var , Local'), Var = `Name :
      LVars ! Var,
      Locals ! none(Name) |
	check_local;

    Local = (Var = Value , Local'), Var = `Name, Value =\= `"_" :
      LVars ! Var,
      Locals ! initial(Name, Value) |
	check_local;

    Local = (Var = `"_" , Local'), Var = `Name :
      LVars ! Var,
      Locals ! null(Name) |
	check_local;

    Local = `Name :
      LVars = [Local],
      Locals = [none(Name)] ;

    Local = (Var = Value), Var = `Name, Value =\= `"_"  :
      LVars = [Var],
      Locals = [initial(Name, Value)] ;

    Local = (Var = `"_"), Var = `Name  :
      LVars = [Var],
      Locals = [null(Name)] ;

    Local = [] :
      LVars = [],
      Locals = [] ;

    otherwise : Local = _,
      LVars = [nonlocal],
      Locals = [none("_")] .

is_diff(Vars, In, Ok) + (Next = new) :-

    Next = new,
    Vars ? `X :
      In ! lookup(X, '!', Next') |
	is_diff;

    Next = new,
    Vars ? ?X :
      In ! lookup(X, '?', Next') |
	is_diff;

    Next = new, 
    Vars = [] :
      In = [], Ok = true;

    Next = old(_) : Vars = _,
      In = [], Ok = false;

    otherwise,
    Vars ? _ : Vars' = _, Next = _,
      In = [], Ok = false .


to_subs(true, Good, [Good | Subs1]^, Subs1).
to_subs(false, _, Subs^, Subs).


to_classes(Ok, Compound, Atom, DTs, Inherits, Inherits',
	   Classes, Classes', Diags, Diags''
) :-

    Ok = true : DTs = _,
      Classes ! Compound |
	validate_inherits(Inherits, Inherits', Atom, Diags, Diags'');

    Ok = false : Compound = _,
      Classes = Classes',
      DTs = Terms-Terms |
	proc_diagnostic(Atom, "left-hand-side ill-formed, cannot inherit",
			Diags, Diags'
	),
	validate_inherits(Inherits, Inherits', Atom, Diags', Diags'').

validate_inherits(Inherits1, Inherits2, Atom, Diags1, Diags2) :-

    Inherits1 ? Inherit |
	validate_inherit(Inherits2, Inherits2', Atom, Diags1, Diags1',
				Inherit, Inherit
	),
	self;

    Inherits1 = [] : Atom = _,
      Inherits2 = [],
      Diags1 = Diags2 .

validate_inherit(Inherits, Inherits', Atom, Diags, Diags',
		 Inherit, Original
) :-

    Inherit = H + L,
    H =\= _#_ |
	inherited_local_variables(L, H, Inherit'),
	self;

    Inherit = M # H + L,
    string(M), H =\= _#_ |
	inherited_local_variables(L, H, Inherit'),
	self;

    Inherit = M # Inherit',
    string(M), Inherit' =\= _#_, Inherit' =\= _+_ |
	self;

    Inherit = S/I,
    string(S), integer(I), string_length(S) > 0 : Atom = _,
      Inherits ! Original,
      Diags = Diags' ;

    string(Inherit), string_length(Inherit) > 0 : Atom = _,
      Inherits ! Original,
      Diags = Diags' ;
      
    tuple(Inherit), Inherit =\= _+_, Inherit =\= _#_, Inherit =\= _/_,
    arg(1, Inherit, Functor),
    string(Functor),
    I := arity(Inherit) |
	inherited_head_variables(Inherit, I, Functor, Inherit'),
	self;

    otherwise : Inherit = _,
      Inherits = Inherits' |
	proc_diagnostic(Atom, "invalid super class designator" - Original,
			Diags, Diags'
	).

inherited_local_variables(LVs, Inherit, Inherit') :-

    LVs = (`_, LVs') |
	self;

    LVs = (`_) :
      Inherit = Inherit' ;

    otherwise : LVs = _, Inherit = _,
      Inherit' = [] .		% Indicate error.

inherited_head_variables(Head, I, Inherit, Inherit') :-

    I-- > 1,
    arg(I, Head, `_) |
	self;

    I > 1,
    arg(I, Head, A),
    otherwise : Inherit = _,
      Inherit' = [A] ;		% Indicate error.

    I =< 1 : Head = _,
      Inherit = Inherit' .
    

/***
* Diagnose incorrect 
***/

cannot_inherit(Procs, Diags, Diags1) :-

    Procs ? proc(Head, _, _, Inherits, Terms-Terms'), Inherits =\= []:
      Terms=Terms' |
	proc_diagnostic(Head, "composite procedure cannot inherit",
			Diags, Diags'
	),
	cannot_inherit;

    Procs ? proc(Head, [], RHSS, [], Terms-Terms') :
      Terms ! (Head' :- RHSS) |
	restore(Head, Head'),
	cannot_inherit;

    Procs ? proc(Head, Local, RHSS, [], Terms-Terms'),
    Local =\= [] :
      Terms ! (Head' + Local :- RHSS) |
	restore(Head, Head'),
	cannot_inherit;

    Procs ? proc(_,_,_) |
	cannot_inherit;

    Procs = [] :
      Diags = Diags1.



/***
* The routine makes continuous passes over the Classes list, trying to complete
* all missing superclasses, until Classes list is empty, or nothing was changed
* in the previous pass.
***/

lpi(Subs, Classes, Classified, IdC, IdZ, Diags)
	+ (Rscs = [], Changed = changed) :-

    Classes =\= [],			% perform a pass over Classes list
    Changed = changed |
	pass(Subs, Subs', Classes, Classes', Rscs, Rscs',
		Diags, Diags', Changed', IdC, IdC'
	),
	lpi;

    Classes = [] : Changed = _,
      Classified = Subs(Rscs),
      IdZ = IdC,
      Diags = Ds(Ds) ;

    Classes =\= [],
    Changed = unchanged :
      Classified = Subs(Rscs),
      IdZ = IdC |	% nothing changed, but unresolved classes remain.
		 	% produce error msgs for all remaining classes.
	not_found_errors(Classes, Diags, Filter),
	filter_non_compound(Filter, _, _, _, _).


/***
* Perform a pass over Classes.
*
*	A "Class" cannot become a "Sub" until all of its superclasses have
*	been found. This implies 2 phases:
*
*	1. match - find matches to as many as possible successive superclass
*	   names in list
*	2. include - include a "Class" in "Subs" list when nothing is left
*	   to inherit.
***/

pass(Subs, Subs1, Classes, Classes1, Rscs, Rscs1,
     Diags, Diags1, Changed1, IdC, IdZ
) + (Changed=unchanged) :-

    Classes ? Class,
    Class = class(Head, _, _, Inherits, _, _),
    Diags = Ds(Dsn) :
      Ds ! id(Head),
      Diags' = Ds2(Dsn),
      write_channel(name(ModuleName), IdC) |
	match(Class, Inherits, Subs, Rscs, Rscs', Ds', Ds1,
		IdC, IdC', Result
	),
	include(Result, Class, Subs, Subs', Classes1, Classes1', ModuleName,
			Ds1, [id | Ds2]
	),
	changed(Changed, Result, Changed'),
	pass;

    Classes = [] :
      IdZ = IdC,
      Diags = Diags1,
      Rscs = Rscs1,
      Classes1 = [],
      Subs = Subs1,
      Changed1 = Changed .



/***
* Try and match as many succesive superclasses (Inherits) of class (Class)
* using the current Subs and Rscs lists
***/

match(Class, Inherits, Subs, Rscs, Rscs1, Errors, Errors1, 
		IdC, IdZ, Result
) + (Current = _, ResultH = Null, ResultT = Null, Flag = true) :-

    Flag = true,
    Inherits ?  Current',			% non remote superclass
    string(Current') : Current = _ |     	% "q"
	search_implicit(Current', Subs, Heritage),
	format_differently(novars, Heritage, Heritage', Current'),
	var_inclusion(Heritage', Class, ResultT, ResultT', 
			Errors, Errors', Flag'
	),
	match;

    Flag = true,
    Inherits ?  Current',
    Current' = Functor/N,			% non remote superclass
    string(Functor) : Current = _ |     	% "q/n"
	search(Functor, N, Subs, Heritage),
	format_differently(novars, Heritage, Heritage', Current'),
	var_inclusion(Heritage', Class, ResultT, ResultT', 
			Errors, Errors', Flag'
	),
	match;

    Flag = true,
    Inherits ? Current',
    Current' =\= (_ / _), Current' =\= (_ # _),	% non remote superclass
    Current' =\= (_ + _),			% "q(X1,...,Xn)"
    arity(Current', N), N--,
    arg(1, Current', Functor) : Current = _ |
	search(Functor, N', Subs, Heritage),
	format_differently(globalonly, Heritage, Heritage', Current'),
	var_inclusion(Heritage', Class, ResultT, ResultT',
			Errors, Errors', Flag'
	),
	match;

    Flag = true,
    Inherits ? Current',			% non remote superclass
    Current' = ((Functor / N) + _) :		% "q + (Xn+1,..)"
      Current = _ |
	search(Functor, N, Subs, Heritage),
	format_differently(localonly, Heritage, Heritage', Current'),
	var_inclusion(Heritage', Class, ResultT, ResultT',
			Errors, Errors', Flag'
	),
	match;

    Flag = true,
    Inherits ? Current',
    Current' = (Functor + _),			% non remote superclass
    string(Functor): Current = _ |		% "q + (Xn+1,..)"
	search_implicit(Functor, Subs, Heritage),
	format_differently(localonly, Heritage, Heritage', Current'),
	var_inclusion(Heritage', Class, ResultT, ResultT',
			Errors, Errors', Flag'
	),
	match;

    Flag = true,
    Inherits ? Current',			% non remote superclass
    Current' = (AtomP + _),			% "q(X1,...,Xn) + (Xn+1,..)"
    arity(AtomP, N), N--,
    AtomP =\= (_ / _),
    arg(1, AtomP, Functor) : Current = _ |
	search(Functor, N', Subs, Heritage),
	format_differently(all, Heritage, Heritage', Current'),
	var_inclusion(Heritage', Class, ResultT, ResultT',
			Errors, Errors', Flag'
	),
	match;

    Flag = true,
    Inherits ? Current',				% remote superclass
    Current' = (_ # _) : Current = _ |
	analyse_remote_reference(Current', ModuleName, SubName, N, WithArgs,
					Errors, Errors'
	),
	remote # search(ModuleName, Current', SubName, N, Heritage, WithArgs,
			Rscs, Rscs', IdC, IdC', Errors'(Errors'')
	),
	var_inclusion(Heritage, Class, ResultT, ResultT',
			Errors'', Errors''', Flag'
	),
	match;

    Flag = true,
    Inherits = [] : Subs = _, Class = _, Current = _,
      Errors = Errors1,
      Rscs = Rscs1,
      ResultT = [],
      IdZ = IdC,
      Result = ResultH;

    Flag = false,
    known(ResultH) : Subs = _, Class = _,
      Errors = Errors1,
      Rscs = Rscs1,
      IdZ = IdC,
      ResultT = [left_to_inherit([Current|Inherits])],
      Result = ResultH;

    Flag = false,
    unknown(ResultH) : Subs = _, IdC = _, Class = _, Current = _,
                       Inherits = _, ResultT = _,
      Errors = Errors1,
      Rscs = Rscs1,
      IdZ = IdC,
      Result = false;

    Flag = error : Class = _, Current = _, Inherits = _, ResultT = _,
                   Subs = _, ResultH = _,
      Result = error,
      Rscs = Rscs1,
      IdZ = IdC,
      Errors = Errors1 ;

    otherwise,
    Flag = true,
    Inherits ? Wrong : Class = _,
      Subs = _, Current = _, Inherits' = _, ResultT = _, ResultH = _,
      Errors = [diagnostic("illegal class in inheritance list" - Wrong)
	       | Errors1],
      IdZ = IdC,
      Result = error,
      Rscs = Rscs1.

/***
* Add arguments of call to superclass tuple.
***/

format_differently(Mode, Heritage, Heritage1, Current) :-
    
    Mode = novars,
    Heritage = inherit(A, B, C, Depends) :
      Heritage1 = inherit(novars, [A, B, C], Current, Depends);

    Mode = globalonly,
    Heritage = inherit(A, B, C, Depends) :
      Heritage1 = inherit(globalonly, [AtomPVars, A, B, C],
			  Current, Depends
		  ) |
	tuple_to_dlist(1, Current, AtomPVars, []);

    Mode = localonly, Current = _ + LocalP,
    Heritage = inherit(A, B, C, Depends) :
      Heritage1 = inherit(localonly, [LocalPVars, A, B, C],
			  Current, Depends
		  ) |
 	local_to_vars(LocalP, LocalPVars);

    Mode = all, Current = AtomP + LocalP,
    Heritage = inherit(A, B, C, Depends) :
      Heritage1 = inherit(all, [AtomPVars, LocalPVars, A, B, C],
			  Current, Depends
		  ) |
	tuple_to_dlist(1, AtomP, AtomPVars, []),
	local_to_vars(LocalP, LocalPVars);

    Heritage =\= inherit(_,_,_,_) :  Mode = _, Current = _,
      Heritage1 = Heritage .



/***
* Result = false ==> No match has been found to any superclass.
* Else Result is a list of superclasses which are included here.
***/

include(Result, Class, Subs, Subs1, Classes, Classes1, ModuleName,
	Diags1, Diags2
) :-

% "q"

    Class = class(Head, Locals, RHSS, _, DTs, Depends),
    Result ? inherit(novars, [SuperHead, SuperLocals, SuperRHSS],
			Current, Appends
	     ) :
      Class' = class(Head, Locals', NewRHSS, _, DTs, Depends') |
	append(Depends, Appends, Depends'),
	functor_vars(Head, Locals, Functor, ClassVars, _),
	functor_vars(SuperHead, SuperLocals, SuperFunctor, SuperVars, _),
	rename # requests(In, SuperVars, SuperVars, ClassVars, ModuleName),
	update_locals(Locals, SuperLocals, Locals', In, In',
			Diags1, Diags1'
	),
	add(SuperFunctor, Functor, SuperRHSS, RHSS, Current, NewRHSS,
		SuperVars, In'
	),
	include;

% "q(X1,...Xn)"

    Class = class(Head, Locals, RHSS, _, DTs, Depends),
    Result ? inherit(globalonly,
		     [Arguments, SuperHead, SuperLocals, SuperRHSS],
		     Current, Appends
	     ) :
      Class' = class(Head, Locals', NewRHSS, _, DTs, Depends') |
	append(Depends, Appends, Depends'),
	functor_vars(Head, Locals, Functor, ClassVars, _),
	functor_vars(SuperHead, SuperLocals, SuperFunctor, SuperVars,
			SuperLocalVars
	),
	append(Arguments, SuperLocalVars, Equivalents),
	rename # requests(In, SuperVars, Equivalents, ClassVars, ModuleName),
	update_locals(Locals, SuperLocals, Locals', In, In',
			Diags1, Diags1'
	),
	add(SuperFunctor, Functor, SuperRHSS, RHSS, Current, NewRHSS,
		Equivalents, In'
	),
	include;

% "q + (Xn+1,...)"

    Class = class(Head, Locals, RHSS, _, DTs, Depends),
    Result ? inherit(localonly,
		     [LocalArgs, SuperHead, SuperLocals, SuperRHSS],
		     Current, Appends
	     ) :
      Class' = class(Head, Locals', NewRHSS, _, DTs, Depends') |
	append(Depends, Appends, Depends'),
	functor_vars(Head, Locals, Functor, ClassVars, _),
	functor_vars(SuperHead, SuperLocals, SuperFunctor, SuperVars, _),
	tuple_to_dlist(0, SuperHead, [Functor | SuperHeadVars], []),
	append(SuperHeadVars, LocalArgs, Equivalents),
	rename # requests(In, SuperVars, Equivalents, ClassVars, ModuleName),
	update_locals(Locals, SuperLocals, Locals', In, In',
			Diags1, Diags1'
	),
	add(SuperFunctor, Functor, SuperRHSS, RHSS, Current, NewRHSS,
		Equivalents, In'
	),
	include;


% "q(X1,...Xn) + (Xn+1,...)"

    Class = class(Head, Locals, RHSS, _, DTs, Depends),
    Result ? inherit(all,
		     [HeadArgs, LocalArgs, SuperHead, SuperLocals, SuperRHSS],
		     Current, Appends
	     ),
    arg(1, Head, Functor) :
      Class' = class(Head, Locals', NewRHSS, _, DTs, Depends') |
	append(Depends, Appends, Depends'),
	functor_vars(Head, Locals, Functor, ClassVars, _),
	functor_vars(SuperHead, SuperLocals, SuperFunctor, SuperVars, _),
	append(HeadArgs, LocalArgs, Equivalents),
	rename # requests(In, SuperVars, Equivalents, ClassVars, ModuleName),
	update_locals(Locals, SuperLocals, Locals', In, In',
			Diags1, Diags1'
	),
	append(HeadArgs, LocalArgs, Equivalents),
	add(SuperFunctor, Functor, SuperRHSS, RHSS, Current, NewRHSS,
		Equivalents, In'
	),
	include;

    Class = class(Head, [], RHSS, _, Terms-Terms', Depends),
    Result = [] : ModuleName = _,		% all supers have been added
      Terms  ! (NewHead :- RHSS),		% add result to Output
						% add result to Subs list
      Subs1 = [sub(Head, [], RHSS, Depends) | Subs],
      Classes = Classes1,
      Diags1 = Diags2 |
      	restore(Head, NewHead);

    Class = class(Head, Locals, RHSS, _, Terms-Terms', Depends),
    Locals =\= [],				% The same with local vars
    Result = [] : ModuleName = _,
      Terms ! (NewHead + Local :- RHSS),
      Subs1 = [sub(Head, Locals, RHSS, Depends) | Subs],
      Classes = Classes1,
      Diags1 = Diags2 |
     	 restore(Head, NewHead),
	 restore_local(Locals, Local);

					% "match" found a partial match
    Class = class(Head, Locals, RHSS, _, DTs, Depends),
    Result = [left_to_inherit(Inherit)] : ModuleName = _,
					% return to Classes list
      Classes = [class(Head, Locals, RHSS, Inherit, DTs, Depends) | Classes1],
      Subs = Subs1,
      Diags1 = Diags2 ;

    Result = false : ModuleName = _,	% "match" did not find a match
      Subs = Subs1,
      Classes = [Class | Classes1],
      Diags1 = Diags2 ;

    Result = error,			% "match"  found an error
    Class = class(Head, Locals, RHSS, _, DTs, _) : ModuleName = _,
      Subs = Subs1,
      Classes = Classes1,
      Diags1 = Diags2 |
	drop_inherits(RHSS, RHSS'),
	restore_local(Locals, Local),
	filter_non_compound([[proc(Head, Local, RHSS', [], DTs)]],
				_, _, _, _
	).

drop_inherits(RHSS, RHSS1) + (Head = List, Tail = List) :-

    RHSS = (+ _ ; RHSS') |
	drop_inherits;

    RHSS = (RHS ; RHSS'), RHS =\= + _ :
      Tail ! RHS |
	drop_inherits;

    RHSS =\= (_ ; _), RHSS =\= + _ :
      Tail = [RHSS] |
	clause_list_to_rhss(Head, RHSS1);

    RHSS = + _ :
      Tail = [] |
	clause_list_to_rhss(Head, RHSS1).



update_locals(Locals, SuperLocals, Locals', In, In', Diags, Diags') :-
	rename_super_locals(SuperLocals, SuperLocals', In, In'),
	sort_locals(Locals, OrderedLocals),
	sort_locals(SuperLocals', OrderedSuperLocals),
	inherit_initialization(OrderedSuperLocals, OrderedLocals,
				Heirs, Diags, Diags'
	),
	substitute_locals(Locals, Heirs, Locals').

rename_super_locals(SLs, RLs, In1, In2) :-

    SLs ? _(_) |	% uninteresting
	rename_super_locals;

    SLs ? Initial(Name, Value) :
      In1 ! find(Name, Name'),
      In1' ! term(Value, Value'),
      RLs ! Initial(Name', Value') |
	self;

    SLs = [] :
      In1 = In2,
      RLs = [] .

inherit_initialization(OSs, OLs, Heirs, Diags1, Diags2) :-

    OSs = [] : OLs = _,
      Heirs = [],
      Diags1 = Diags2 ;

    OSs ? _(Name, Value),
    OLs ? _(Name) :
      Heirs ! inherited(Name, Value) |
	inherit_initialization;

    OSs ? initial(Name, _),
    OLs ? Local, Local = _(Name, _) |
	inherit_initialization;

    OSs ? inherited(Name, _),
    OLs ? Local, Local = _(Name, _) :
      Diags1 ! diagnostic("conflicting inherited local values" - Name) |
	inherit_initialization;

    OSs = [_(Name, _) | _],
    OLs ? Local, arg(2, Local, Name1),
    Name1 @< Name |
	inherit_initialization.

substitute_locals(Locals, Heirs, Substituted) :-

    Heirs ? Heir, Heir = _(Name, _) |
	substitute_local(Name, Heir, Locals, Locals'),
	self;

    Heirs = [] :
      Substituted = Locals .

substitute_local(Name, Heir, Locals1, Locals2) :-

    Locals1 ? Other,
    Other =\= _(Name) :
      Locals2 ! Other |
	self;

    Locals1 ? _(Name) :
      Locals2 = [Heir | Locals1'] .

/***
* ADD SUPER CLASS (g) TO CLASS (f)
*
*   rename variables in RHSS of superclass 
*   replace iterations of the forms: "g", "g(..)" in g's-body
*   make new RHSS = (f-RHSS ; g-Modified SuperRHSS)
*
*   see formal semantics for further explanation
*
***/


add(SuperFunctor, Functor, SuperRHSS, RHSS, Current, NewRHSS, SuperVars, In) :-

    true :
      In = [term(SuperRHSS, SuperRHSS')] |
	replace_iterations(SuperFunctor, Functor, SuperRHSS', SuperRHSS'',
				SuperVars
	),
	edit_rhss(RHSS, Current, SuperRHSS'', ClauseList, []),
	clause_list_to_rhss(ClauseList, NewRHSS).


/***
* SEARCH FOR A SUBCLASS WHICH SHOULD APPEAR IN THIS MODULE
***/

search_implicit(Functor, Subs, Result) + (Found = [], Proc = _) :-

    Subs ? Proc', Proc' = sub(Head, _, _, _),
    arity(Head, N--),
    arg(1, Head, Functor) : Proc = _,
      Found' = [N' | Found] |
	search_implicit;

    otherwise,
    Subs ? _ |
	search_implicit;

    Subs = [], Found = [_],
    Proc = sub(Head, Locals, RHSS, Depends) : Functor = _,
      Result = inherit(Head, Locals, RHSS, Depends);
    
    Subs = [], Found = [] : Functor = _, Proc = _,
	Result = notfound ;

    Subs = [], Found = [_, _ | _] : Proc = _,
      Result = ambiguous(Functor/Found) .


search(Functor, N, Subs, Result) :-

    Subs ? sub(Head, Locals, RHSS, Depends),
    arity(Head, N'), N++,
    arg(1, Head, Functor) :
      Subs' = _,
      Result = inherit(Head, Locals, RHSS, Depends);

    otherwise,
    Subs ? _ |
	search;

    Subs = [] |
	Result = notfound, Functor = _, N = _.


/***
* CHECK SUBCLASS AND CLASS VARIABLES MATCHING
***/

var_inclusion(Heritage, Class, Result, Result1, Errors, Errors1, Flag) :-

    Heritage = notfound : Class = _,
      Errors1 = Errors,
      Result1 = Result,
      Flag = false;

    Heritage = notfound_error : Class = _,
      Errors1 = Errors, 
      Result1 = Result,
      Flag = error;

    Heritage = ambiguous(Functor/Arities) : Class = _,
      Errors = [diagnostic("ambiguous implicit reference"
			    - Functor - Arities)
	       | Errors1],
      Result1 = Result,
      Flag = error;

    Heritage = inherit(novars, [Satom, Slocals, _], _, _),
    Arity := arity(Satom) - 1,
    Class = class(Head, Locals, _, _, _, _)  |
	split_atom(Satom, Functor, SatomVars),
	tuple_to_dlist(1, Head, HeadVars, []),
	subset(HeadVars, SatomVars, Part1, Part2),
	subset(Locals, Slocals, Part2, []),
        ok_result(Part1, Flag, Result, Result1, Errors, Errors1,
			Heritage, Functor/Arity
	);

    Heritage = inherit(globalonly, [ArgsVars, Satom, Slocals, _], _, _),
    arg(1, Satom, Functor),
    Arity := arity(Satom) - 1,
    Class = class(Head, Locals, _, _, _, _) |
	tuple_to_dlist(1, Head, HeadVars, []),
	subset(HeadVars, ArgsVars, Part1, Part2),
	subset(Locals, Slocals, Part2, []),
        ok_result(Part1, Flag, Result, Result1, Errors, Errors1,
			Heritage, Functor/Arity
	);

    Heritage = inherit(localonly, [Args2Vars, Satom, _, _], _, _),
    Arity := arity(Satom) - 1,
    Class = class(Head, Locals, _, _, _, _) | 	
	split_atom(Satom, Functor, SatomVars),
	tuple_to_dlist(1, Head, HeadVars, []),
	subset(HeadVars, SatomVars, Part1, Part2),
	subset(Locals, Args2Vars, Part2, []),
        ok_result(Part1, Flag, Result, Result1, Errors, Errors1,
			Heritage, Functor/Arity
	);

% what if the size of Args2 is different from Slocals?

    Heritage = inherit(all, [ArgsVars, Args2Vars, Satom, _, _], _, _),
    arg(1, Satom, Functor),
    Arity := arity(Satom) - 1,
    Class = class(Head, Locals, _, _, _, _) |
	tuple_to_dlist(1, Head, HeadVars, []),
	subset(HeadVars, ArgsVars, Part1, Part2),
	subset(Locals, Args2Vars, Part2, []),
        ok_result(Part1, Flag, Result, Result1, Errors, Errors1,
			Heritage, Functor/Arity
	).



ok_result([], true^, [Heritage|Result]^, Result,
	  Errors^, Errors, Heritage, _
).
ok_result([Var], error^, Result^, Result,
	  [diagnostic("superclass var is not subset of class vars"
			 - Functor - Var)
	  | Errors]^,
	  Errors, _, Functor
).
ok_result(Others, error^, Result^, Result,
	  [diagnostic("superclass vars are not subset of class vars"
			 - Functor - Others)
	  | Errors]^,
	  Errors, _, Functor
) :-
    otherwise | true.


subset(Big, Small, Head, Tail) :-

    Small ? _(Mem),
    string(Mem) |
 	subset_member(Mem, Big, Head, Head'),
	self;

    Small ? _(Mem, _) |
 	subset_member(Mem, Big, Head, Head'),
	self;

    Small = [] : Big = _,
      Head = Tail .


subset_member(X, Xs, Head, Tail) :-

    Xs ? _(X) : Xs' = _,
      Head = Tail ;

    Xs ? _(X, _) : Xs' = _,
      Head = Tail ;

    Xs ? _,
    otherwise |
      subset_member;

    Xs = [] :
      Head = [X | Tail] .


/***
* REPLACE RECURSIVE CALLS IN BODY.
***/

replace_iterations(OldFunctor, Functor, RHSS, ModRHSS, OldVars) :-

    RHSS = (OldFunctor, RHSS') :
      ModRHSS = (Functor, ModRHSS') |
	replace_iterations;

    RHSS = (OldFunctor ; RHSS') :
      ModRHSS = (Functor ; ModRHSS') |
	replace_iterations;

    RHSS = OldFunctor :
      ModRHSS = Functor, OldVars = _;


    RHSS = (OldFunctor + Equalities, RHSS') :
      ModRHSS = (Functor + Equalities, ModRHSS') |
	replace_iterations;

    RHSS = (OldFunctor + Equalities ; RHSS') :
      ModRHSS = (Functor + Equalities ; ModRHSS') |
	replace_iterations;

    RHSS = (OldFunctor + Equalities) :
      ModRHSS = (Functor + Equalities), OldVars = _;


    RHSS = (Head, RHSS'),
    arg(1, Head, OldFunctor) :
      ModRHSS = (Functor + Equalities , ModRHSS') |
	tuple_to_dlist(1, Head, Values, []),
	make_iteration_equalities(OldVars, Values, Equalities),
	replace_iterations;

    RHSS = (Head ; RHSS'),
    arg(1, Head, OldFunctor) :
      ModRHSS = (Functor + Equalities ; ModRHSS') |
	tuple_to_dlist(1, Head, Values, []),
	make_iteration_equalities(OldVars, Values, Equalities),
	replace_iterations;

    arg(1, RHSS, OldFunctor) :
      ModRHSS = (Functor + Equalities) |
	tuple_to_dlist(1, RHSS, Values, []),
    	make_iteration_equalities(OldVars, Values, Equalities);


    otherwise,
    RHSS = (RHSS1, RHSS') :
       ModRHSS = (RHSS1', ModRHSS') |
	replace_iterations(OldFunctor, Functor, RHSS1, RHSS1', OldVars),
	replace_iterations;

    RHSS = (RHSS1 ; RHSS') :
      ModRHSS = (RHSS1' ; ModRHSS') |
	replace_iterations(OldFunctor, Functor, RHSS1, RHSS1', OldVars),
	replace_iterations;

    RHSS = (Something | RHSS') :
      ModRHSS = (Something | ModRHSS') |
	replace_iterations;

    otherwise,
    RHSS = _(_, _) :			%????
      ModRHSS = RHSS, Functor = _, OldFunctor = _, OldVars = _;

    otherwise :
      RHSS = ModRHSS, Functor = _, OldFunctor = _,OldVars = _.



make_iteration_equalities(Vars, Values, Equalities) :-

    Vars ? Var,
    Values ? Val,
    Values' =\= [] :
	Equalities = ((Var = Val), Equalities') |
	make_iteration_equalities;

    Vars ? Var,
    Values = [Val] :
	Equalities = (Var = Val), Vars' = _;

    Vars = [],
    Values =\= [] :
	Equalities = (Values).		% this is an error -should note??


analyse_remote_reference(Remote, ModuleName, SubName, N, Arguments,
				Errors, Errors1
) :-

    Remote = M1 # (C1 / N1++), string(M1), string(C1) :
      SubName = C1,
      N = N1',
      ModuleName = M1,
      Arguments = false,
      Errors = Errors1;

    Remote = M1 # Head + LocalP, string(M1),
    Head =\= (_ / _),
    arity(Head, N1) :
      ModuleName = M1,
      N = N1,
      SubName = Functor,
      Arguments = all(Vars, LocalPVars),
      Errors = Errors1 |
	split_atom(Head, Functor, Vars),
 	local_to_vars(LocalP, LocalPVars);

    Remote = M1 # Head, string(M1),
    Head =\= (_ / _), Head =\= (_ + _),
    arity(Head, N1) :
      ModuleName = M1,
      N = N1,
      SubName = Functor,
      Arguments = globalonly(Vars),
      Errors = Errors1 |
	split_atom(Head, Functor, Vars);

    Remote = M1 # ((Functor / Arity) + LocalP),
    string(M1),
    integer(Arity), Arity++ >= 0,
    string(Functor) :
      ModuleName = M1,
      N = Arity',
      SubName = Functor, 
      Arguments = localonly(LocalPVars),
      Errors = Errors1 |
 	local_to_vars(LocalP, LocalPVars);

    Remote = M1 # (Head + LocalP),
    string(M1), string(Head) :
      ModuleName = M1,
      N = 0,
      SubName = Head, 
      Arguments = localonly(LocalPVars),
      Errors = Errors1 |
	local_to_vars(LocalP, LocalPVars);

    Remote = M1 # Head,
    string(M1), string(Head) :
      ModuleName = M1,
      N = 0,
      SubName = Head,
      Arguments = false,
      Errors = Errors1 ;

    otherwise :
      ModuleName = "...",
      N = -1,
      SubName = "...",
      Arguments = false,
      Errors = [diagnostic("cannot analyse remote name" - Remote)
	       | Errors1] .



/***
* Make a list of variables occuring in Head + Locals
***/

functor_vars(Atom, Locals, Functor, Vars, LocalVars) :-
    	tuple_to_dlist(0, Atom, [Functor | Vars], LocalVars),
	locals_to_vars(Locals, LocalVars).


split_atom(Atom, Functor, Vars) :-
    	tuple_to_dlist(0, Atom, [Functor | Vars], []).

locals_to_vars(Locals, Vars) :-

    Locals ? Tuple,
    arg(2, Tuple, Var) :
      Vars ! `Var |
	locals_to_vars;

    Locals = [] :
      Vars = [] .

/***
* Make a list from external form of local variables
***/

local_to_vars(Local, Vars) :-

    Local = [] : Vars = [];

    Local = (Var, Local'),
    Var =\= (_ = _) :
      Vars ! Var |
	local_to_vars;

    Local = (Var = _, Local') :
      Vars ! Var |
	local_to_vars;

    Local = (Var = _) :
    Vars = [Var];

    Local =\= (_, _),
    Local =\= (_ = _) :
      Vars=[Local].

/***
* Extract Inherits list and reconstruct Right-Hand-Side.
***/

rhss_to_dlists(RHSS, Inherits, Rest, RHH, RHT) :-

    RHSS = (RHS ; RHSS') |
	rhss_to_dlists(RHS, Inherits, Inherits', RHH, RHH'),
	rhss_to_dlists;

    RHSS = + Inherit :
      RHH = [RHSS | RHT],
      Inherits = [Inherit | Rest];

    RHSS = + F/N :
      Inherit = F/N,
      RHH = [+Inherit | RHT],
      Inherits = [Inherit | Rest];

    RHSS = + F/N + L :
      Inherit = F/N + L,
      RHH = [+Inherit | RHT],
      Inherits = [Inherit | Rest];

    RHSS = + G + L :
      Inherit = G + L,
      RHH = [+Inherit | RHT],
      Inherits = [Inherit | Rest];

    RHSS = + M # Id :
      Inherit = M # Id,
      RHH = [+Inherit | RHT],
      Inherits = [Inherit | Rest];

    otherwise :
      RHH = [RHSS | RHT],
      Inherits = Rest .

clause_list_to_rhss(ClauseList, NewRHSS) :-

    ClauseList ? Next,
    list(ClauseList') :
      NewRHSS = (Next ; NewRHSS') |
	clause_list_to_rhss;

    ClauseList = [Last] :
      NewRHSS = Last ;

    ClauseList = [] :
      NewRHSS = true .

/***
* Replace occurences of current inherited process in RHSS by SuperRHSS.
***/

edit_rhss(RHSS, Current, SuperRHSS, C1, Cn) :-

    RHSS = +Current,
    SuperRHSS =\= true :
      Current' = [],
      RHSS' = SuperRHSS |
	edit_rhss;

    RHSS = +Current, SuperRHSS = true :
      C1 = Cn ;

    RHSS = (Part ; RHSS') |
	edit_rhss(Part, Current, SuperRHSS, C1, C1'),
	edit_rhss;

    RHSS =\= (_ ; _), RHSS =\=  +Current : SuperRHSS = _,
      C1 = [RHSS | Cn] .

/***
* Print errors for every remaining Class.
***/

not_found_errors(Classes, Diags, Filter) :-

    Classes ?  class(Head, Locals, RHSS, [SuperClass], DTs, _) :
      Filter ! [proc(Head, Local, RHSS', [], DTs)] |
	proc_diagnostic(Head, "superclass not found" - SuperClass,
			Diags, Diags'
	),
	restore_local(Locals, Local),
	drop_inherits(RHSS, RHSS'),
	not_found_errors;

    Classes ?  class(Head, Locals, RHSS, SuperClasses, DTs, _),
    SuperClasses = [_, _ | _] :
      Filter ! [proc(Head, Local, RHSS', [], DTs)] |
	restore_local(Locals, Local),
	proc_diagnostic(Head, "superclasses not found " - SuperClasses,
			Diags, Diags'
	),
	drop_inherits(RHSS, RHSS'),
	not_found_errors;

    Classes = [] :
      Diags = Ds(Ds),
      Filter = [].


changed(Changed1, Changed2, Changed3) :-

    Changed1 = unchanged, Changed2 =  false :
      Changed3 = unchanged;

    Changed1 = unchanged, Changed2 =  error :
      Changed3 = unchanged;

    otherwise :
      Changed3 = changed, Changed1 = _, Changed2 = _.


proc_diagnostic(Atom, Diagnostic,
		{[diagnostic(Atom, Diagnostic) | Errors]^, Errors'},
		{Errors, Errors'}^
).

/***
* Transform head to tuple form - and inverse.
***/

normalize(Head1, Head2, IdC1, IdC2) :-

    tuple(Head1),
    arg(1,Head1,F), A := arity(Head1) :
      Head2 = Head1,
      write_channel(lookup(F, A, _), IdC1, IdC2) ;

    string(Head1) :
      Head2 = {Head1},
      write_channel(lookup(Head1, 1, _), IdC1, IdC2) ;

    otherwise :
      Head2 = [Head1],
      IdC2 = IdC1 .

restore(Head, Head1) :-

    Head = {Str}, string(Str) : Head1 = Str;

    Head = [Other] :
      Head1 = Other;

    otherwise :
      Head1 = Head .

restore_local(Locals, Local) :-

    Locals ? none(Var), Locals' =\= [] :
      Local = (`Var, Local') |
	restore_local;

    Locals ? null(Var), Locals' =\= [] :
      Local = (`Var = `"_", Local') |
	restore_local;

    Locals ? _(Var, Value), Locals' =\= [] :
      Local = (`Var = Value, Local') |
	restore_local;

    Locals = [none(Var)] :
      Local = `Var ;

    Locals = [null(Var)] :
      Local = (`Var = `"_") ;

    Locals = [_(Var, Value)] :
      Local = (`Var = Value) ;

    Locals =\= [_|_] :
      Local = Locals .

/***
* Append utility
***/

append(L1, L2, L3) :-

    L1 = [] |
      L3 = L2;

    L2 = [] |
      L3 = L1;

    L1 ? A, L2 =\= [] :
      L3 ! A |
	append.


/***
* Tuple to dlist utility
***/

tuple_to_dlist(N, Tuple, List, Tail) :-

    N++ < arity(Tuple),
    arg(N', Tuple, Arg) :
      List ! Arg |
	tuple_to_dlist;

    N >= arity(Tuple) :
      List = Tail .


/***
* Sort utilities
***/


sort_locals(In, Out) + (Tail = []) :-

    In ? X,
    arg(2, X, A) |
	partition(In', A, Ss, In''),
	sort_locals(Ss, Out, [X | Out']),
	sort_locals;

    In = [] :
      Out = Tail.

partition(In, A, Ss, Ls) :-

    In ? X,
    arg(2, X, B), B @< A :
      Ss ! X |
	partition;

    In ? X,
    arg(2, X, B), A @< B :
      Ls ! X |
	partition;

    In = [] : A = _,
      Ss = [],
      Ls = [].
