/* $Header: /home/qiana/Repository/Logix/system/lint/defs.cp,v 1.1 1999/07/09 07:03:10 bill Exp $ */
/*
 *
 * defs identifies predefined procedures and checks the legality of
 * arguments.   It returns for each procedure the expected properties of
 * its arguments.
 *
 * It exports check_call/7, with arguments:
 *
 *	1. The calling procedure IdN.
 *	2. The called procedure Id.
 *	3. The part in the calling procedure where the call occured.
 *	4. The call itself.
 *	5. The Called procedure properties (Output).
 *	6. The clause internal part of the dictionary.
 *	7. The Callcheck lint option (true/false).
 *
 * Properties = mode(A1, A2, ..., An) where n is the number of
 * arguments in the (predefined) called procedure and Ai is one of:
 *
 *	calculate	- If the argument is a variable, it is in write mode.
 *	suspend		- If the argument is a variable, the call to the
 *			  procedure suspends on it, or at least does not
 *			  instantiate it.
 *	suspend_subs	- Every variable that appears in the expression,
 *			  suspends the call to the procedure, or at least is
 *			  not instantiated by the call.
 *
 * For procedures which are not predefined, Properties =  calculate.
 *
 */

-export([check_call/7]).
-mode(trust).
-language(compound).

procedure check_call(Any, Proc, Part, Atom, Tuple, G-dict, Flag).

/*
 *
 * check_call/7 checks a call to a predefined procedure.
 * It returns the properties of the procedure to the caller.
 *
 */

check_call(Id, Proc, Part, Call, Properties, G_dict, Callcheck) :-

    Callcheck = true |
	properties(Proc, Reply),
	proc_specific(Id, Reply, Part, Call, Properties, G_dict);

    Callcheck = false : Id  = _, Part = _, Call = _, G_dict = _ |
	properties(Proc, Reply),
	properties_reply(Reply, Properties).

properties_reply(Reply, Properties) :-

    Reply = _(_, Properties^) |
	true;

    otherwise :
      Properties = Reply |
	true.

/*
 *
 * proc_specific/6 checks the legality of the procedures. Currently it
 * just checks for legal argument types.
 *
 */

proc_specific(Id, Reply, Part, Call, Properties, G_dict) :-

    Reply = Mode(In, Properties^),
    G_dict = g_dict(Cvi_tree, _Cpi_tree) |
	check_proc_specific(Id, Mode, Part, Call, Properties, Cvi_tree, In, 2);

    otherwise : Id = _, Part = _, Call = _, G_dict = _,
      Properties = Reply |
	true.

/*
 *
 * check_proc_specific/8  checks each argument of the call.
 *
 */

check_proc_specific(Id, Mode, Part, Call, Properties, Cvi, In, Index) :-

    arg(Index, Call, Arg),
    arg(Index, In, ArgIn),
    arg(Index, Properties, Property),
    Index' := Index + 1,
    Argno := Index - 1 |
	check_proc_specific,
	say_if_not_in(Id, Part, Argno, Arg, ArgIn, Call),
	check_suspension(Arg, Property, Mode, {Id, Part, Argno, Call, Cvi});

    otherwise : Id = _, Mode = _, Part = _, Call = _, Properties = _,
		Cvi = _, In = _, Index = _ |
	true.

/*
 *
 * if_eq_tell/3 & if_ne_tell/3 checks for unifiability of first 2 arguments
 * and if ok, outputs the diagnostic.
 *
 */

/*
if_eq_tell(X, X, D) :-
	computation # diagnostic(D).
if_eq_tell(_X, _Y, _D) :-
    otherwise |
	true.
*/

if_ne_tell(X, Y, D) :-

    X = Y : D = _ |
	true;

    X =\= Y |
	computation # diagnostic(D).

/*
 *
 * say_if_not_in/6 checks if the given argument is in the given list and
 * outputs a message if not.
 *
 */

say_if_not_in(Id, Part, Argno, X, In, Call) :-

    In = [Only] |
	is_one_of(X, In, Ok),
	if_ne_tell(Ok, true,
		   (Id, Part,
		    'illegal argument' = X - arg(Argno) - in(Call) -
						should_be(Only)));

    otherwise |
	is_one_of(X, In, Ok),
	if_ne_tell(Ok, true,
		   (Id, Part,
		    'illegal argument' = X - arg(Argno) - in(Call) -
						should_be_one_of(In))).

/*
 *
 * is_one_of/3 gets an argument and checks if it is in the given list.
 *
 */

is_one_of(X, List, Ok) :-

    List ? L |
	is_of_kind(X, L, Is),
	if_is_one_of(Is, X, List', Ok);

    List = [] : X = _,
      Ok = false |
	true.

if_is_one_of(true, _, _, true^).
if_is_one_of(false, X, List, Ok) :-
	is_one_of(X, List, Ok).

/*
 *
 * is_of_kind/3 checks if the given argument is of a given kind.
 *
 */

is_of_kind([], list, true^).
is_of_kind([_|_], list, true^).
is_of_kind(X, string, true^) :-
    string(X) |
	true.
is_of_kind(`_, var, true^).
is_of_kind(?_, ro, true^).
is_of_kind(T, tuple, Maybe) :-
    tuple(T) |
	is_of_kind_tuple(T,Maybe).
is_of_kind(I, integer, true^) :-
    integer(I) |
	true.
is_of_kind(R, real, true^) :-
    real(R) |
	true.
is_of_kind(N, number, true^) :-
    number(N) |
	true.
is_of_kind(C, constant, true^) :-
    constant(C) |
	true.
is_of_kind(A, ascii, Ok) :-
	is_ascii(A, Ok).
is_of_kind(X, X, true^) :-
    number(X) |
	true.
is_of_kind(_X, all, true^).
is_of_kind(Exp, expression, Ok) :-
	is_expression(Exp, Ok).
is_of_kind(_, _, false^) :-
    otherwise |
	true.

is_of_kind_tuple(`_, false^).
is_of_kind_tuple(?_, false^).
is_of_kind_tuple(_,true^) :-
     otherwise |
	true.


/*
 *
 * is_expression/2 checks if the given argument is a valid expression.
 * (e.g. integer,variable,ro_var, A+B, A-B, A*B, A/B, A /\ B, A \/ B,
 *  -A, +A, ~A (etc) where A,B are valid expressions).
 *
 */

is_expression(A+B, Ok) :-
	is_expression(A, Ok1),
	is_expression(B, Ok2),
	and(Ok1, Ok2, Ok).
is_expression(A-B, Ok) :-
	is_expression(A, Ok1),
	is_expression(B, Ok2),
	and(Ok1, Ok2, Ok).
is_expression(A*B, Ok) :-
	is_expression(A, Ok1),
	is_expression(B, Ok2),
	and(Ok1, Ok2, Ok).
is_expression(A/B, Ok) :-
	is_expression(A, Ok1),
	is_expression(B, Ok2),
	and(Ok1, Ok2, Ok).
is_expression(A\B, Ok) :-
	is_expression(A, Ok1),
	is_expression(B, Ok2),
	and(Ok1, Ok2, Ok).
is_expression(A/\B, Ok) :-
	is_expression(A, Ok1),
	is_expression(B, Ok2),
	and(Ok1, Ok2, Ok).
is_expression(A\/B, Ok) :-
	is_expression(A, Ok1),
	is_expression(B, Ok2),
	and(Ok1, Ok2, Ok).
is_expression(min(A,B), Ok) :-
	is_expression(A, Ok1),
	is_expression(B, Ok2),
	and(Ok1, Ok2, Ok).
is_expression(max(A,B), Ok) :-
	is_expression(A, Ok1),
	is_expression(B, Ok2),
	and(Ok1, Ok2, Ok).
is_expression(mod(A,B), Ok) :-
	is_expression(A, Ok1),
	is_expression(B, Ok2),
	and(Ok1, Ok2, Ok).
is_expression(info(A), Ok) :-
	is_expression(A, Ok).
is_expression(-A, Ok) :-
	is_expression(A, Ok).
is_expression(+A, Ok) :-
	is_expression(A, Ok).
is_expression(~A, Ok) :-
	is_expression(A, Ok).
is_expression(abs(A), Ok) :-
	is_expression(A, Ok).
is_expression(ascii(A), Ok) :-
	is_ascii(A, Ok).
is_expression(integer(A), Ok) :-
	is_expression(A, Ok).
is_expression(round(A), Ok) :-
	is_expression(A, Ok).
is_expression(real(A), Ok) :-
	is_expression(A, Ok).
is_expression(sin(A), Ok) :-
	is_expression(A, Ok).
is_expression(cos(A), Ok) :-
	is_expression(A, Ok).
is_expression(tan(A), Ok) :-
	is_expression(A, Ok).
is_expression(random, true^).
is_expression(asin(A), Ok) :-
	is_expression(A, Ok).
is_expression(acos(A), Ok) :-
	is_expression(A, Ok).
is_expression(atan(A), Ok) :-
	is_expression(A, Ok).
is_expression(exp(A), Ok) :-
	is_expression(A, Ok).
is_expression(ln(A), Ok) :-
	is_expression(A, Ok).
is_expression(sqrt(A), Ok) :-
	is_expression(A, Ok).
is_expression(log(A,B), Ok) :-
	is_expression(A, Ok1),
	is_expression(B, Ok2),
	and(Ok1, Ok2, Ok).
is_expression(pow(A,B), Ok) :-
	is_expression(A, Ok1),
	is_expression(B, Ok2),
	and(Ok1, Ok2, Ok).
is_expression(arity(T), Ok) :-
	is_one_of(T, [var,ro, tuple], Ok).
is_expression(length(C), Ok) :-
	is_one_of(C, [var,ro, string, list], Ok).
is_expression(string_length(S), Ok) :-
	is_one_of(S, [var,ro, string], Ok).
is_expression(string_hash(S), Ok) :-
	is_one_of(S, [var,ro, string], Ok).
is_expression(nth_char(N, S), Ok) :-
	is_expression(N, Ok1),
	is_one_of(S, [var,ro, string], Ok2),
	and(Ok1, Ok2, Ok).
is_expression(N, true^) :-
    number(N) |
	true.
is_expression(`_, true^).
is_expression(?_, true^).
is_expression(_, false^) :-
    otherwise |
	true.

/*
 *
 * and/3. Do what you think.
 *
 */

and(true, true, true^).
and(_, _, false^) :-
    otherwise |
	true.

/*
 *
 * is_ascii/2 checks if the given argument is a valid argument to the ascii
 * macro - it should be a known ascii mnemonic or a single character.
 *
 */

is_ascii(A, true^) :-
    string_length(A, 1) |
	true.
is_ascii(bel, true^).
is_ascii(bs, true^).
is_ascii(lf, true^).
is_ascii(cr, true^).
is_ascii(esc, true^).
is_ascii(del, true^).
is_ascii(_, false^) :-
    otherwise |
	true.

/*
 *
 * check_suspension/8 checks for suspension on not previously referenced
 * arguments.
 *
 */

check_suspension(Arg, Property, Mode, Data) :-

    Arg = `Name, string(Name),
    Property = suspend, Mode = true |
	check_suspension_var(Name, Data);

    Arg = ?Name, string(Name),
    Property = suspend, Mode = true |
	check_suspension_var(Name, Data);

    Property = suspend_subs, Mode = true |
	check_suspension_subs(Arg, Data);

    otherwise : Arg = _, Property = _, Mode = _, Data = _ |
	true.

/*
 *
 * check_suspension_subs/2 check for suspensions on non predefined variables
 * for suspend_subs properties. (e.g. check for each variable in that term).
 *
 * Data = {Id, Part, Argno, Call, Cvi}.
 */

check_suspension_subs(Arg, Data) :-

    constant(Arg) : Data = _ |
	true;

    Arg = [Arg'' | Arg'] |
	check_suspension_subs,
	check_suspension_subs(Arg', Data);

    Arg = `Name, string(Name) |
	check_suspension_var(Name, Data);

    Arg = ?Name, string(Name) |
	check_suspension_var(Name, Data);

    otherwise, tuple(Arg),
    Arity := arity(Arg) |
	check_suspension_subs_tuple(1, Arity, Arg, Data).

/*
 *
 * check_suspension_subs_tuple/4 is a sub-procedure of check_suspension_subs.
 * It checks the elements of a tuple.
 *
 */

check_suspension_subs_tuple(Index, Arity, Arg, Data) :-

    Index =\= Arity,
    arg(Index, Arg, Sub),
    Index' := Index + 1 |
	check_suspension_subs(Sub, Data),
	check_suspension_subs_tuple;

    Index = Arity,
    arg(Index, Arg, Sub) |
	check_suspension_subs(Sub, Data).

/*
 *
 * check_suspension_var/2 check suspension on non predefined variables for
 * suspend property.
 *
 * Data = {Id, Part, Argno, Call, Cvi}.
 */

check_suspension_var(Name, Data) :-

    Data = {Id, Part, Argno, Call, Cvi} |
	tree # tree_fetch(Name, List, Cvi),
	uninstantiated_var_exists(List, Exists),
	if_ne_tell(Exists, true,
		   (Id, Part,
		    'suspends on ~previously-defined variable' = Name -
						arg(Argno) - of(Call))).

/*
 *
 * uninstantiated_var_exists/2 checks that in the given variable occurances
 * list, there is at least one occurance non ro and non suspended.
 *
 */

uninstantiated_var_exists(List, Exists) :-

    List ? at(_Proc, var, _Part) : List' = _,
      Exists = true |
	true;

    otherwise,
    List ? _ |
	uninstantiated_var_exists;

    constant(List) :
      Exists = false |
	true.

/*
 *
 * properties/2 is the procedure that gives the property and argument validity
 * lists for each predefined procedure.
 *
 */

properties(guardp(known/1), true(in([var]), mode(suspend))^ ).
properties(guardp(ground/1), true(in([var]), mode(suspend))^ ).
properties(guardp(tuple/1), true(in([var]), mode(suspend))^ ).
properties(guardp(string/1), true(in([var]), mode(suspend))^ ).
properties(guardp(module/1), true(in([var]), mode(suspend))^ ).
properties(guardp(integer/1), true(in([var]), mode(suspend))^ ).
properties(guardp(real/1), true(in([var]), mode(suspend))^ ).
properties(guardp(number/1), true(in([var]), mode(suspend))^ ).
properties(guardp(constant/1), true(in([var]), mode(suspend))^ ).
properties(guardp(compound/1), true(in([var]), mode(suspend))^ ).
properties(guardp(channel/1), true(in([var]), mode(suspend))^ ).
properties(guardp(vector/1), true(in([var]), mode(suspend))^ ).
properties(guardp(context/1), true(in([var]), mode(suspend))^ ).
properties(guardp(arg/3), true(in([integer, var], [tuple, var], [all]),
				mode(suspend,suspend,calculate))^
).
properties(guardp(nth_char/3),
			    true(in([integer, var], [string, var],
					[var, ro, integer]),
				mode(suspend,suspend,calculate))^
).
properties(guardp(ascii/2), true(in([ascii], [var, ro, integer]),
				mode(suspend, calculate))^
).
properties(guardp(arity/2), true(in([tuple,var], [var, ro, integer]),
				mode(suspend, calculate))^
).
properties(guardp(string_length/2), true(in([string, var], [var, ro, integer]),
				mode(suspend, calculate))^
).
properties(guardp(string_hash/2), true(in([string, var], [var, ro, integer]),
				mode(suspend, calculate))^
).
properties(	guardp(string_to_dlist/3),
		true(in([string,var], [list, var, ro], [list, var, ro]),
				mode(suspend, calculate, calculate))^
).
properties(guardp(list_to_string/3),
			true(in([0, 1, 2], [list, var], [string, ro, var]),
				mode(suspend, suspend_subs, calculate))^
).
properties(guardp('='/2), true(in([all], [all]), mode(calculate, calculate))^
).
properties(guardp('=='/2), true(in([all], [all]), mode(suspend, suspend))^ ).
properties(guardp('=?='/2), true(in([all], [all]), mode(suspend, suspend))^ ).
properties(guardp('=\='/2), true(in([all], [all]), mode(suspend, suspend))^ ).
properties(guardp('\='/2), true(in([all], [all]), mode(suspend, suspend))^ ).
properties(	guardp(':='/2),
		true(in([var], [expression]),
				mode(calculate, suspend_subs))^
).
properties(	guardp('>'/2),
		true(in([expression], [expression]),
				mode(suspend_subs, suspend_subs))^
).
properties(	guardp('<'/2),
		true(in([expression], [expression]),
				mode(suspend_subs, suspend_subs))^
).
properties(	guardp('>='/2),
		true(in([expression], [expression]),
				mode(suspend_subs, suspend_subs))^
).
properties(	guardp('=<'/2),
		true(in([expression], [expression]),
				mode(suspend_subs, suspend_subs))^
).
properties(	guardp('=:='/2),
		true(in([expression], [expression]),
				mode(suspend_subs, suspend_subs))^
).
properties(guardp(convert_to_integer/2), true(in([string, number, var], [var]),
				mode(suspend,calculate))^
).
properties(guardp(convert_to_real/2), true(in([string, number, var], [var]),
				mode(suspend, calculate))^
).
properties(guardp(convert_to_string/2), true(in([string, number, var], [var]),
				mode(suspend, calculate))^
).
properties(guardp(make_channel/2), true(in([var], [var]),
				mode(calculate, calculate))^
).
properties(guardp(write_channel/2), true(in([all], [var]),
				mode(calculate, calculate))^
).
properties(guardp(write_channel/3), true(in([all], [var], [var]),
				mode(suspend,calculate, calculate))^
).
properties(guardp(close_channel/1), true(in([var]), mode(calculate))^ ).
properties(guardp(close_context/1), true(in([var]), mode(calculate))^ ).
properties(guardp(make_vector/3), true(in([integer, var], [var], [var]),
				mode(suspend, calculate, calculate))^
).
properties(guardp(write_vector/3), true(in([integer, var], [all], [var]),
				mode(suspend,calculate, calculate))^
).
properties(	guardp(write_vector/4),
		true(in([integer, var], [all], [var], [var]),
			mode(suspend, calculate, calculate, calculate))^
).
properties(guardp(store_vector/3), true(in([integer, var], [all], [var]),
				mode(suspend, calculate, calculate))^
).
properties(	guardp(store_vector/4),
		true(in([integer, var], [all], [var], [var]),
			mode(suspend, calculate, calculate, calculate))^
).
properties(guardp(read_vector/3), true(in([integer, var], [var], [var]),
				mode(suspend, calculate, calculate))^
).
properties(guardp(close_vector/2), true(in([integer, var], [var]),
				mode(suspend, calculate))^
).
properties(guardp(otherwise/0), calculate^).
properties(user('='/2), no_prop_check(in([all], [all]),
				mode(calculate, calculate))^
).
properties(	user(':='/2),
		no_prop_check(in([var], [expression]),
				mode(calculate, suspend_subs))^
).
properties(_Proc, calculate^) :-
    otherwise |
	true.
