/* $Header: /home/qiana/Repository/Logix/system/lint/main.cp,v 1.2 2002/11/16 11:37:10 bill Exp $ */
-export([first/6]).
%-mode(trust).
-language(compound).

procedure first(Kind, Attributes, Terms, Dict_dl, L_options, Gi_options).

Kind ::= module ; library.
Attributes, Terms ::= [Any].

/*
 *
 * first/6 determines the kind of module and
 * analyses the attributes of the module (if any).
 * All of the procedures in the export list-export([first/6]).
-mode(trust).
-language(compound).

procedure first(Kind, Attributes, Terms, Dict_dl, L_options, Gi_options).

Kind ::= module ; library.
Attributes, Terms ::= [Any].

/*
 *
 * first/6 determines the kind of module and
 * analyses the attributes of the module (if any).
 * All of the procedures in the export list-export([first/6]).
-mode(trust).
-language(compound).

procedure first(Kind, Attributes, Terms, Dict_dl, L_options, Gi_options).

Kind ::= module ; library ; none.
Attributes, Terms ::= [Any].

/*
 *
 * first/6 determines the kind of module and
 * analyses the attributes of the module (if any).
 * All of the procedures in the export list are
 * marked as used in "dummy" (by do_export/3).
 * procedures/5 analyses regular procedures.
 *
 */

first(Kind, Attributes, Terms, Dict_dl, L_options, Gi_options) :-

    Kind = module,
    Attributes =\= [] :
      Dict_dl = [ident(attributes([]), Id, Gi_options),
	      add_proc(use(user), attributes/1, head, attributes([]), _, false)
		| Ds0] \ Ds3 |
	check_head(start, Id, Id', attributes([]), L_options, Ds0, Ds1),
	check_attributes(Attributes, _, {Monitor, Exports}, done),
	check_export(Monitor, Exports, Ds1, Ds2),
	procedures(Id', Terms, Ds2, Ds3, L_options, Gi_options);

    Kind = module,
    Attributes = [],
    Terms = [ (boot(_,_) :- _) | _ ] |
	procedures(Terms, Dict_dl, L_options, Gi_options);

    Kind = library : Attributes = _ |
	procedures(Terms, Dict_dl, L_options, Gi_options);

    Kind = none : Attributes = _, Terms = _, Dict_dl = _,
		  L_options = _, Gi_options = _ ;

    otherwise,
    Attributes = [] : Kind = _ |
	display_missing_attributes(L_options),
	procedures(Terms, Dict_dl, L_options, Gi_options).


display_missing_attributes([true | _]) :-
	computation # comment(missing_attributes).
display_missing_attributes(_) :-
	otherwise | true.


check_attributes(As, Ids, MonitorExports, Done) + (Attributes = {_M,_X,_E}) :-

    As ? Attribute |
	attribute_id(Attribute, Id),
	duplicate_attribute(Id, Ids, Reply),
	check_attribute(Reply, Attribute, Attributes, Done, Done'),
	check_attributes; 

    As = [],
    Attributes = {M, X, E} : Ids = _ |
	unify_without_failure(Done?(M), done([])),
	unify_without_failure(Done?(X), done([])),
	unify_without_failure(Done?(E), done([])),
	export_list.

  export_list(M, X, E, MonitorExports) :-

    X =?= [] :
      MonitorExports = {M, E};

    E =?= [] :
      MonitorExports = {M, X};

    X =\= [], E =\= [] :
      MonitorExports = {M, XE?} |
	concatenate_lists([[X],[E]],XE).

  concatenate_lists(Lists, Out) :-

    Lists = [List | Rest],
    List ? Item, Item =\= [], Item =\= [_ | _] :
      Out ! Item,
      Lists' = [List' | Rest] |
	self;

    Lists = [List | Rest],
    List ? Item, Item =\= [], Item =?= [_ | _] :
      Lists' = [Item, List' | Rest] |
	self;

    Lists = [List | Rest],
    List ?  [] :
      Lists' = [List' | Rest] |
	self;

    Lists ? [] |
	concatenate_lists;

    Lists =?= [] :
      Out = [].


check_attribute(Reply, Attribute, Attributes, Done1, Done2) :-

    Reply = new,
    Attribute = monitor(Name),
    Attributes = {M, _, _} |
	check_monitor_name(Name, M, Done1, Done2);

    Reply = new,
    Attribute = export(X),
    X =\= all,
    Done1 = done :
      Attributes = {_, X, _},
      Done2 = done ;

    Reply = new,
    Attribute = entries(E),
    E =\= all,
    Done1 = done :
      Attributes = {_, _, E},
      Done2 = done ;

    Reply = new,
    Done1 = done,
    Attribute = mode(Mode),
    Mode =\= trust, Mode =\= failsafe,
    Mode =\= interrupt, Mode =\= interpret,
    Mode =\= user, Mode =\= system : Attributes = _,
      Done2 = done |
	computation # diagnostic(invalid_mode - Mode);

    tuple(Reply),
    Done1 = done : Attribute = _, Attributes = _,
      Done2 = done |
	computation # diagnostic(Reply);

    otherwise : Reply = _, Attribute = _, Attributes = _,
      Done1 = Done2 .

check_monitor_name(Name, [Name/1]^, done, done^) :-
    string(Name) |
	true.
check_monitor_name(Name, M, done, Done) :-
    otherwise, unknown(M) :
      Done = done |
	computation # diagnostic(illegal_monitor_name - Name).
check_monitor_name(_, M, Done, Done^) :-
    known(M) |
	true.


attribute_id(Attribute, Id) :-

    string(Attribute) :
      Attribute = Id ;

    tuple(Attribute),
    A := arity(Attribute) - 1, A > 0,
    arg(1, Attribute, F),
    string(F) :
      Id = F/A ;

    otherwise :
      Id = error(Attribute) |
	computation # diagnostic(invalid_attribute - Attribute).


duplicate_attribute(Id, Ids, Reply) :-

    Ids ? Other,
    Id =\= Other |
	duplicate_attribute;

    Ids ? Id : Ids' = _,
      Reply = duplicate_attribute(Id) ;

    true :
      Ids = Ids'?,
      Reply = new |
	Ids' =  [Id | _].

/*
 * do_export/4 handles the precompiler export/1 attribute.
 * It marks all the given procedures in export/1 (and the named
 * monitor entry /1) as used (referenced) under export/1-1.
 */

check_export(Monitor, Exports, Ds1, Ds2) :-

    Monitor = [] |
	do_export(Exports, Ds1, Ds2);

    otherwise |
	do_export(Monitor, Ds1, Ds2),
	do_export(Exports, _, _).

do_export(Ps, Ds1, Ds2) :-

    Ps ? P,
    P = Functor/Arity, string(Functor), Arity >= 0 :
      Ds1 ! add_proc(use(user), P, head, dummy, _, false) |
	self;

    Ps ? S,
    string(S) |		% ignore for now
	self;

    Ps =\= [_|_],
    Ps =\= [] :
      Ps' = [Ps] |
	self;

    Ps = [] :
      Ds1 = Ds2 ;

    otherwise :
      Ds1 = Ds2 |
	computation # diagnostic(export_argument_error - Ps).

/*
 *
 * procedures/6 is the procedure that identifies the goals in the parsed file
 * calls the head/guard/body special check procedures and tells the dictionary
 * about the newly defined goal (including getting the Id from the dictionary).
 *
 */

procedures(Terms, Ds1\Ds2, L_options, Gi_options) :-
	procedures(start, Terms, Ds1, Ds2, L_options, Gi_options).

procedures(LastId, Terms, Ds1, Ds2, L_options, Gi_options) :-

    Terms ? (H :- (G | B)) :
      Ds1 ! ident(H,Id, Gi_options) |
	check_head(LastId, Id, LastId', H, L_options, Ds1', Dict1),
	check_guard(Id, G, L_options, Dict1, Dict2),
	check_body(Id, B, L_options, Dict2, Ds1''),
	procedures;

    Terms ? (H :- B),
    B =\= (_|_), B =\= (_:_) :
      Ds1 ! ident(H, Id, Gi_options) |
	check_head(LastId, Id, LastId', H, L_options, Ds1', Dict),
	check_body(Id, B, L_options, Dict, Ds1''),
	procedures;

    Terms ? (H :- G),
    G = (_:_) :
      Ds1 ! ident(H, Id, Gi_options) |
	check_head(LastId, Id, LastId', H, L_options, Ds1', Dict),
	check_guard(Id, G, L_options, Dict, Ds1''),
	procedures;

    Terms ? (_ ::= _) |
	procedures;

    Terms ? (procedure _) |
	procedures;

    otherwise,
    Terms ? H :
      Ds1 ! ident(H, Id, Gi_options) |
	check_head(LastId, Id, LastId', H, L_options, Ds1', Ds1''),
	procedures;

    Terms = [] : LastId = _, L_options = _, Gi_options = _,
       Ds1 = Ds2 .

/*
 *
 * check_head/7 checks the legality of the given head,
 * and sends data to the dictionary server.
 *
 */

check_head(LastId, Id, NextId, H, L_options, Ds1, Ds2) :-

    known(Id) |
	check_outer_head(LastId, Id, NextId, H),
	check_inner_head(Id, H, L_options, Ds1, Ds2).

/*
 *
 * check_guard/5 checks the legality of the given guard,
 * and sends data to the dictionary server.
 *
 */

check_guard(Id, G, L_options, Ds1, Ds2) :-

    known(Id) |
	check_outer_guard(Id, G),
	check_inner_guard(Id, G, L_options, Ds1, Ds2).

/*
 *
 * check_body/4 checks the legality of the given body,
 * and sends data to the dictionary server.
 *
 */

check_body(Id, B,  L_options, Ds1, Ds2) :-

    known(Id) |
	check_outer_body(Id, B),
	check_inner_body(Id, B, L_options, Ds1, Ds2).


/*
 *
 * check_outer_head/4 check the outer level of the head.
 *
 */

check_outer_head(LastId, Id, NextId, Head) :-

    string(Head) : LastId = _,
    NextId = Id ;

    Head = {String, _, _},
    string(String) |
      check_legal_head(LastId, Id, NextId, Head, String);

    Arity := arity(Head), Arity =\= 3,
    arg(1, Head, F),
    string(F) : LastId = _,
      NextId = Id ;
%	check_head_arguments(Id, Head, 2, Arity);

    otherwise : Id = _,
      NextId = LastId |
	computation # diagnostic((following - LastId >
				  non_string_functor(Head))).

check_legal_head(LastId, Id, NextId, Head, String) :-

    String =\= ",", String =\= ":", String =\= "|",
    String =\= "#", String =\= "@", String =\= ";" : LastId = _, Head = _,
      NextId = Id ;

    otherwise : Id = _, String = _,
      NextId = LastId |
	computation # diagnostic(following - LastId - illegal_head(Head)).

/*
 *
 * check_head_arguments/4 checks if all the arguments in the head of the
 * procedure are legal. Currently doesnt make any checks.
 *
 */

%check_head_arguments(_Id, _H, _M, _Arity).

/*
 *
 * check_outer_guard/2 checks the outer level of the guard.
 * It uses check_outer_guard_and_body/5 for the check because
 * they are mostly synonymous.
 *
 */

check_outer_guard(Id, G) :-

    G =\= (_:_) |
	check_outer_guard_and_body(Id, guard, G);

    G = (Ask : Tell) |
	check_outer_guard_and_body(Id, guard, Ask),
	check_outer_guard_and_body(Id, guard, Tell).

/*
 *
 * check_outer_body/2 checks the outer level of the body.
 * It uses check_outer_guard_and_body/5 for the check because
 * they are mostly synonymous.
 *
 */

check_outer_body(Id, B) :-
	check_outer_guard_and_body(Id, body, B).

/*
 *
 * check_outer_guard_and_body/3 checks the outer level the guard and body
 * of the goals.
 * Where differences between guard and body are permitted, it discriminates
 * them by Part.
 *
 */

check_outer_guard_and_body(Id, Part, Call) :-

    string(Call)  : Id = _, Part = _, Call = _ ;

    Call = (Call', Call'') |
	check_outer_guard_and_body(Id, Part, Call'),
	check_outer_guard_and_body;

    Part = body,
    Call = (Service # Goal) |
	check_left_side(Service, Ok),
	check_right_side(Goal, Ok, Ok'),
	display_check(Ok', Id, Call);

    Part = body,
    Call = (Goal @ Link) |
	check_link(Link, Ok),
	check_right_side(Goal, Ok, Ok'),
	display_check(Ok', Id, Call);

    Part = body,
    tuple(Call),
    arity(Call) > 1,
    arg(1, Call, `Name), Name =\= "_" : Id = _ ;

    Part = body,
    tuple(Call),
    arity(Call) > 1,
    arg(1, Call, ?Name), Name =\= "_" : Id = _ ;

    otherwise,
    tuple(Call), Call =\= (_ ; _), Call =\= (_ | _), Call =\= (_ : _),
    Call =\= `"_", Call =\= ?"_",
    arity(Call) > 1,
    arg(1, Call, F), string(F) : Id = _, Part = _ ;

    otherwise |
	computation # diagnostic((Id, Part - illegal_goal(Call))).

check_link(Link, Ok) :-

    string(Link) :
      Ok = true ;

    Link = `_ :
      Ok = suspect ;

    Link = ?_ :
      Ok = suspect ;

    otherwise : Link = _,
      Ok = false .

check_left_side(Service, Ok) :-

    string(Service) :
      Ok = true ;

    Service = (Service' # Segment) |
	check_left_side,
	check_inside(Segment, Ok', Ok);

    Service = F(V) |
	check_context(F, V, true, Ok);

    Service = F(V, _, _) |
	check_context(F, V, true, Ok);

    list(Service) |
	check_service_id(Service, true, Ok);

    otherwise : Service = _,
      Ok = false .


check_context(Functor, V, Ok1, Ok2) :-

    Functor = context,
    V = `_ :
      Ok1 = Ok2 ;

    Functor = context,
    V = ?_ :
      Ok1 = Ok2 ;

    Functor = '_var' : V = _, Ok1 = _,
      Ok2 = suspect ;

    Functor = '_ro' : V = _, Ok1 = _,
      Ok2 = suspect ;

    otherwise : Ok1 = _ |
	check_context1(Functor, V, Ok2).

check_context1(F, V, Ok) :-

    F =\= `_,
    F =\= ?_ : V = _,
      Ok = false ;

    V =\= `_,
    V =\= ?_ : F = _,
      Ok = false ;

    otherwise : F = _, V = _,
      Ok = suspect .

check_inside(Segment, Ok1, Ok2) :-

    string(Segment) :
      Ok1 = Ok2 ;

    Segment = (Segment' # Segment'') |
	check_inside(Segment', Ok1, Ok1'),
	check_inside;

    Ok1 =\= false,
    Segment = F(V) |
	check_context(F, V, suspect, Ok2);

    Ok1 =\= false,
    Segment = F(V, _, _) |
	check_context(F, V, suspect, Ok2);

    otherwise : Segment = _, Ok1 = _,
      Ok2 = false .

check_right_side(Goal, Ok1, Ok2) :-

    string(Goal) :
      Ok1 = Ok2 ;

    tuple(Goal),
    A := arity(Goal), A > 1,
    arg(1, Goal, F), string(F),
    F =\= '#',
    Goal =\= `_,
    Goal =\= ?_ :
      Ok1 = Ok2 ;

    Goal = (Segment # Goal') |
	check_inside(Segment, Ok1, Ok1'),
	check_right_side;

    Ok1 = false : Goal = _,
      Ok2 = false ;

    otherwise : Goal = _, Ok1 = _,
      Ok2 = suspect .

check_service_id(Service, Ok1, Ok2) :-

    Service ? String,
    string(String) |
	check_service_id;

    Service ? `_ : Ok1 = _,
      Ok1' = suspect |
	check_service_id;

    Service ? ?_ : Ok1 = _,
      Ok1' = suspect |
	check_service_id;

    Service = `_ : Ok1 = _,
      Ok2 = suspect ;

    Service = ?_ : Ok1 = _,
      Ok2 = suspect ;

    Service = [] : 
      Ok1 = Ok2 ;

    otherwise : Service = _, Ok1 = _,
      Ok2 = false .

display_check(true, _, _).
display_check(false, Id, Call) :-
	computation # diagnostic((Id, body - illegal_RPC(Call))).
display_check(suspect, Id, Call) :-
	computation # diagnostic((Id, body - suspect_RPC(Call))).

/*
 * check_inner_head/5 sends the head data to the dictionary server.
 * Declaration type is declare in folder user.
 */

check_inner_head(Id, H, L_options, Ds1, Ds2) :-
	send_dict(Id, head, declare(user), H, L_options, Ds1, Ds2).

/*
 * check_inner_guard/5 sends the guard data to the dictionary server.
 * Declaration type is use in folder guardp.
 *
 */

check_inner_guard(Id, G, L_options, Ds1, Ds2) :-

    G =\= (_:_) |
	send_dict(Id, guard, use(guardp), G, L_options, Ds1, Ds2);

    G = (A : T) |
	send_dict(Id, guard, use(guardp), A, L_options, Ds1, Ds1'),
	send_dict(Id, guard, use(guardp), T, L_options, Ds1', Ds2).

/*
 * check_inner_body/5 sends the body data to the dictionary server.
 * Declaration type is use in folder user.
 */

check_inner_body(Id, B, L_options, Ds1, Ds2) :-
	send_dict(Id, body, use(user), B, L_options, Ds1, Ds2).

/*
 *
 * send_dict/7 is the lint procedure that sends data to the dictionary server.
 * It has two levels: the outer level where procedures are declared and the
 * inner level where only variables and constants appear. send_dict/7 is the 
 * outer level procedure and calls send_dict_inner for the inner level work.
 * It sends the procedures data to the dictionary.
 * Its arguments are an  Id , the  Part  of the goal which it is analysing,
 * the declaration  Type {use/declare, folder}, the  Atom  to be diagnosed
 * and a stream to the dictionary.
 * When the expression laws are different for the 3 parts of the goal, it
 * discriminates by the  Part  argument.
 *
 * 					%%% update about Property %%%%%%%%%%%%%
 *
 */

send_dict(Id, Part, Type, Atom, L_options, Ds1, Ds2) :-

    string(Atom),
    L_options = [Outer, Callcheck | _] :
      Ds1 ! add_proc(Type, Id1?, Part, Atom, Properties, Callcheck) |
	get_term_id(Atom, Id1),
	send_dict_inner(Id, Part, Atom, Outer, Properties, Ds1', Ds2);

    Atom = (Atom'', Atom'),
    Part =\= head |
	send_dict,
	send_dict(Id, Part, Type, Atom', L_options, Ds2', Ds2);

    Atom = Service # Atom',
    Part = body,
    string(Service),
    L_options = [Outer, Callcheck | _] : Type = _,
      Ds1 ! add_proc(use(external), Service # AId, Part, Atom, Properties,
			Callcheck
	    ) |
	get_term_id(Atom', AId),
	send_dict_inner(Id, Part, Atom, Outer, Properties, Ds1', Ds2);

    Atom = `_,
    L_options = [Outer, Callcheck | _] : Type = _,
      Ds1 ! add_proc(use(external), self # "_" / -1, Part, Atom, _Properties,
			Callcheck
	    ) |
	send_dict_inner(Id, Part, Atom, Outer, calculate, Ds1', Ds2);

    Atom = ?_,
    L_options = [Outer, Callcheck | _] : Type = _,
      Ds1 ! add_proc(use(external), self # "_" / -1, Part, Atom, _Properties,
			Callcheck
	    ) |
	send_dict_inner(Id, Part, Atom, Outer, suspend, Ds1', Ds2);

    otherwise,
    tuple(Atom), Atom =\= (_ ; _), Atom =\= (_ | _), Atom =\= (_ : _),
    arg(1, Atom, F),
    string(F),
    L_options = [Outer, Callcheck |_] :
      Ds1 ! add_proc(Type, AId, Part, Atom, Properties, Callcheck) |
	get_term_id(Atom, AId),
	send_dict_inner(Id, Part, Atom, Outer, Properties, Ds1', Ds2);

    otherwise,			% There is an error in the linted module.
    L_options = [Outer | _] :	% This error found previously
      Type = _ |		% by check_outer_head/guard/body
	send_dict_inner(Id, Part, Atom, Outer, calculate, Ds1, Ds2).

/*
 *
 * send_dict_inner/7 is the inner level of send_dict/7 above.
 * It sends its data to the dictionary as constants and variables only.
 *
 */

send_dict_inner(Id, Part, Term, Outer, Property, Ds1, Ds2) :-
				
    constant(Term), Term =\= [] : Id = _, Outer = _, Property = _,
      Ds1 = [add_dict(Term, Part) | Ds2] ;

    Term = [] : Id = _, Part = _, Outer = _, Property = _,
      Ds1 = Ds2 ;

    Term = `'_' : Id = _, Part = _, Outer = _, Property = _,
      Ds1 = Ds2 ;

    Term = `X, X =\= '_' : Id = _, Outer = _,
      Ds1 = [add_var(X?, Var_mode?, Part) | Ds2] |
	get_var_mode(Property, Var_mode);

    Term = ?'_',
    Outer = true : Property = _,
      Ds1 = Ds2 |
	computation # diagnostic((Id, Part - anonymous_read_only_variable));

    Term = ?'_',
    Outer = false : Id = _, Part = _, Property = _,
      Ds1 = Ds2 ;

    otherwise,
    Term = ?X : Id = _, Outer = _, Property = _,
      Ds1 = [add_var(X?, ro, Part) | Ds2] ;

    otherwise,
    tuple(Term),
    Arity := arity(Term) |
	send_dict_tuple(1, Arity, Term, Outer, Property, Id, Part, Ds1, Ds2);

    Term = [Term'' | Term'] |
	get_property(Property, Property'),
	send_dict_inner,
	send_dict_inner(Id, Part, Term', Outer, Property', Ds2', Ds2).

/*
 *
 * send_dict_tuple/9 is a subprocedure of send_dict_inner/7 that handles
 * tuples, calling send_dict_inner/7 for each argument of the tuple.
 *
 */

send_dict_tuple(Cnt, Arity, Tuple, Outer, Properties, Id, Part, Ds1, Ds2) :-
    Cnt < Arity,
    arg(Cnt, Tuple, Arg),
    Cnt' := Cnt + 1 |
	get_property(Cnt, Properties, Property),
	send_dict_inner(Id, Part, Arg, Outer, Property, Ds1, Ds1'),
	send_dict_tuple;

    Cnt = Arity,
    arg(Arity, Tuple, Arg) |
	get_property(Arity, Properties, Property),
	send_dict_inner(Id, Part, Arg, Outer, Property, Ds1, Ds2).

/*
 *
 * get property/2 & get_property/3 get the property of the next level of the
 * term. get_property/3 does that for tuples with argument specific
 * properties as well.
 *
 * get_var_mode/2 translates the given property to the mode a variable
 * (var mode/suspend mode).
 *
 */

get_property(suspend, calculate^).
get_property(calculate, calculate^).
get_property(suspend_subs, suspend_subs^).

get_property(_Cnt, suspend, calculate^).
get_property(_Cnt, calculate, calculate^).
get_property(_Cnt, suspend_subs, suspend_subs^).
get_property(Cnt, Properties, Property^) :-
    Cnt > 1,
    tuple(Properties),
    arg(Cnt, Properties, Property) |
	true.
get_property(1, Properties, calculate^) :-
    tuple(Properties) |
	true.

get_var_mode(calculate, var^).
get_var_mode(suspend, suspend^).
get_var_mode(suspend_subs, suspend^).

/*
 *
 * get_term_id/2 gets a term and returns an identifier in the format
 * Name/Arity. When it finds a non-atom, it returns term_error/error.
 *
 */

get_term_id(Term, Id) :-
    tuple(Term),
    arg(1, Term, Name),
    Arity := arity(Term) - 1 :
      Id = Name/Arity ;

    string(Term) :
      Id = Term/0 ;

    otherwise : Term = _,
      Id = term_error/error .
