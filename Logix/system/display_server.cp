/*

Display server procedures

William Silverman, 05-08-85

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:02:53 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
    			$Source: /home/qiana/Repository/Logix/system/display_server.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/
-export([initial/1, initial/2]).
-mode(trust).
-language(compound).

procedure initial([Request]).
procedure initial([Request], State).

% The display server flattens and outputs terms to the display.
% It recognizes some synchronization and formatting options.

initial(In) :-
	dynamic_server(In, freeze(char,8,20,0,78,none,1,[],terminal)).

initial(In, State) :-
	dynamic_server(In, State).	% System library procedure.

/*****************************************************************************/

/*
** select/4 - recognize request
**
*/

select(Request, State1, State2, Common) :-
	arg(1, Request, option) |
	normalize_option(Request, Attribute, New, Old, Comment),
	update_state(Attribute, New, Old', State1, State1'),
	unify_without_failure(Old'?, Old),
	legal_new_option(Attribute, New, State1, State1', State2),
	select1(stream, stream(Comment),
		ground(char, 8, 20, 0, 78, none, 1, [], terminal), Common
	);

    arg(1, Request, Functor),
    Functor =\= option :
      State1 = State2 |
	select1(Functor, Request, State2, Common);

    otherwise :
      State1 = State2 |
	unknown(Request, Common).


select1(Functor, Request, State, Common) :-

    Functor = ask :
      Common = fork(Common1, Common2) |
	sio_mss(Message, Common1),
	normalize_ask(Request, State, Options, Contents),
	make_message(Options, Contents, Common2, Message\[]);

    Functor = display |
	normalize_display(Request, State, Options, Contents),
	route_output(Options, Message, Common, Routed),
	select_display(Routed, Options, Contents, Message);

    Functor = display_stream |
	normalize_display(Request, State, Options, Contents),
	route_output(Options, Messages, Common, Routed),
	select_display_stream(Routed, Options, Contents, Messages);

    Request = options(Options),
    State = {Type, Read, Depth, Length, Indent, Width, Special,
		Iterations, Prefix, Dispose} :
      Functor = _ |
	disposition(Dispose, Disposition),
	unify_without_failure(	
		[type(Type), read(Read), depth(Depth), length(Length),
		 indent(Indent), width(Width), special(Special),
		 iterations(Iterations), prefix(Prefix), Disposition],
		Options
	),
	unify_without_failure(done, Common);

    Functor = stream |
	normalize_display(Request, State, Options, Contents),
	route_output(Options, Messages, Common, Routed),
	select_display_stream(Routed, Options, Contents, Messages);

    Functor = term |
	normalize_display(Request, State, Options, Contents),
	route_output(Options, Message, Common, Routed),
	select_display(Routed, Options, Contents, Message);

    otherwise : Functor = _, State = _ |
	unknown(Request, Common).

disposition(Dispose, Disposition) :-

    Dispose = terminal :
      Disposition = output(terminal) ;

    Dispose =\= terminal :
      Disposition = Dispose .

select_display(routed(Common), Options, Contents, Message) :-
	make_message(Options, Contents, Common, Message\[]).

select_display_stream(routed(Common), Options, Contents, Messages) :-
	make_messages(1, Options, Contents, Common, Messages).

/* argument analysis */

normalize_display(Request, State, Options, Contents) :-

    Request = _(Contents^) |
	default_options(State, _, Options);

    Request = _(Contents^, Arguments) |
	initial_options(_Read, _Dispose, State', Local, Options),
	extract_options(Arguments, State, State', Local);

    otherwise : State = _,
      Contents = dont_understand(Request) |
	default_state(State'),
	default_options(State', _, Options).


normalize_ask(Request, State, Options, Contents) :-

    Request = _(Contents^, Answer) |
	replace_dispose(read(Read?(Answer)), State, State'),
	default_options(State', Read, Options);

    Request = _(Contents^, Answer, Arguments) :
      Dispose = read({Read, Answer}) |
	replace_dispose(Dispose, State, State'),
	initial_options(Read, Dispose, State'', Local, Options),
	extract_options(Arguments, State', State'', Local);

    otherwise : State = _,
      Contents = dont_understand(Request) |
	default_state(State'),
	replace_dispose(read(Read(_)), State', State''),
	default_options(State'', Read, Options).


replace_dispose(Dispose, State, State') :-

    State = {Type, Read, Depth, Length, Indent, Width, Special,
		Iterations, Prefix, _Dispose} :
      State' = {Type, Read, Depth, Length, Indent, Width, Special,
		Iterations, Prefix, Dispose} .


default_options(State, DefaultRead, Options) :-
    State = {Type, Read, Depth, Length, Indent, Width, Special,
		_Iterations, Prefix, Dispose} :
      DefaultRead = Read,
      Options =	{{Type, Depth, Length}, {Indent, Width, Special}, Dispose,
		 term(Prefix, [])} .


initial_options(Read, Dispose, State, Local, Options) :-
    true :
      State = {Type, Read, Depth, Length, Indent, Width, Special,
		_Iterations, Prefix, Dispose},
      Local = {Knowns, Type, Flag, Closes},
      Options = {{Knowns, Depth, Length},
		 {Indent, Width, Special}, Dispose?, 
		 {Flag, Prefix, Closes}
		} .


normalize_option(Option, Attribute, New, Old, Comment) :-

    Option = option(Attribute^) :
      New = Old,
      Comment = (Attribute=Old) ;

    Option = option(Attribute^, New^) :
      Comment = (old(Attribute)=Old) ;

    Option = option(Attribute^, New^, Old') :
      Comment = [] |
	unify_without_failure(Old, Old');

    otherwise :
      Attribute = type,
      Old = New,
      Comment = [] |
	computation # event(display(dont_understand, Option)).


extract_options(Arguments, State1, State2, Local) :-

    Arguments ? known(Known),
    Local = {Ks, T, F, Cs} :
      Ks ! Known,
      Local' = {Ks', T, F, Cs} |
	extract_options;

    Arguments ? wait(Known),
    Local = {Ks, T, F, Cs} :
      Ks ! Known,
      Local' = {Ks', T, F, Cs} |
	extract_options;

    Arguments ? list,
    arg(3, Local, list^) |
	extract_options;

   Arguments ? close(Left, Right),
   Local = {Ks, T, F, Cs} :
     Cs ! {Left, Right},
     Local' = {Ks, T, F, Cs'} |
	extract_options;

    otherwise,
    Arguments ? Attribute |
	extract_options,
	extract_attribute(Attribute, State1, State1');

    Arguments = [] :
      Local = {T, T, F, []},
      State1 = State2 |
	unify_without_failure(term, F);

    Arguments =\= [], Arguments =\= [_ | _] :
      Arguments' = [Arguments] |
	extract_options.

extract_attribute(Descriptor, State1, State2) :-

    Descriptor = {Attribute, Value} |
	update_state(Attribute, Value, _, State1, State2);

    otherwise :
      State1 = State2 |
	computation #
		event(display(option_unrecognized, Descriptor)).

default_state({freeze, char, 3, 0, 0, 78, none, 1, [], terminal}^).


update_state(Attribute, Value, Old, State1, State2) :-

    Attribute = type,
    State1 = {Old^, R, D, L, I, W, M, Z, P, O} :
      State2 = {New?, R, D, L, I, W, M, Z, P, O} |
	validate_type(Value, Old, New);

    Attribute = read,
    State1 = {T, Old^, D, L, I, W, M, Z, P, O} :
      State2 = {T, New?, D, L, I, W, M, Z, P, O} |
	validate_read(Value, Old, New);

    Attribute = depth,
    State1 = {T, R, Old^, L, I, W, M, Z, P, O} :
      State2 = {T, R, New?, L, I, W, M, Z, P, O} |
	validate_value(Attribute, Value, Old, New);

    Attribute = length,
    State1 = {T, R, D, Old^, I, W, M, Z, P, O} :
      State2 = {T, R, D, New?, I, W, M, Z, P, O} |
	validate_value(Attribute, Value, Old, New);

    Attribute = indent,
    State1 = {T, R, D, L, Old^, W, M, Z, P, O} :
      State2 = {T, R, D, L, New?, W, M, Z, P, O} |
	validate_value(Attribute, Value, Old, New);

    Attribute = width,
    State1 = {T, R, D, L, I, Old^, M, Z, P, O} :
      State2 = {T, R, D, L, I, New?, M, Z, P, O} |
	validate_value(Attribute, Value, Old, New);

    Attribute = special,
    State1 = {T, R, D, L, I, W, Old^, Z, P, O} :
      State2 = {T, R, D, L, I, W, New?, Z, P, O} |
	validate_special(Value, Old, New);

    Attribute = iterations,
    State1 = {T, R, D, L, I, W, M, Old^, P, O} :
      State2 = {T, R, D, L, I, W, M, New?, P, O} |
	validate_value(Attribute, Value, Old, New);

    Attribute = prefix,
    State1 = {T, R, D, L, I, W, M, Z, Old^, O} :
      State2 = {T, R, D, L, I, W, M, Z, New?, O} |
	validate_prefix(Value, New);

    Attribute = output,
    Value = terminal,
    State1 = {T, R, D, L, I, W, M, Z, P, Old^} :
      State2 = {T, R, D, L, I, W, M, Z, P, terminal} ;

    Attribute = output,
    unknown(Value),
    State1 = {T, R, D, L, I, W, M, Z, P, Old^} :
      State2 = {T, R, D, L, I, W, M, Z, P, New?} |
	validate_output(Value, Old, New);

    Attribute = append,
    State1 = {T, R, D, L, I, W, M, Z, P, Old^} :
      State2 = {T, R, D, L, I, W, M, Z, P, append(Value)} ;

    Attribute = put,
    State1 = {T, R, D, L, I, W, M, Z, P, Old^} :
      State2 = {T, R, D, L, I, W, M, Z, P, put(Value)} ;

    otherwise :
      Old = unknown,
      State1 = State2 |
	computation # event(display(option_invalid, Attribute(Value))).

validate_value(Attribute, Value, Old, New) :-

    Value >= 0 : Attribute = _, Old = _,
      Value = New ;

    otherwise :
      Old = New |
	computation # event(display_option(Attribute, Value, "not >= 0")).

validate_prefix(Value, New) :-

    Value = [] :
      New = [] ;

    otherwise :
      New = {Value} .

validate_type(freeze, _, freeze^).
validate_type(ground, _, ground^).
validate_type(unparse, _, unparse^).
validate_type(namevars, _, namevars^).
validate_type(parsed, _, parsed^).
validate_type(New, Old, Old^) :-
    otherwise |
	computation # event(display_option(type, New, not_in,
				[freeze, ground, namevars, parsed, unparse])
			).

validate_read(char, _, char^).
validate_read(line, _, string^).
validate_read(string, _, string^).
validate_read(chars, _, chars^).
validate_read(New, Old, Old^) :-
    otherwise |
	computation # event(display_option(read, New, not_in,
					  [char, string, chars]
				)
			).

validate_special(hex, _, hex^).
validate_special(melted, _, melted^).
validate_special(none, _, none^).
validate_special(New, Old, Old^) :-
    otherwise |
	computation # event(display_option(special, New, not_in,
						[hex, melted, none])
			).

validate_output(append(X), _,append(X)^).
validate_output(put(X), _,put(X)^).
validate_output(terminal, _,terminal^).
validate_output(New, Old, Old^) :-
    otherwise |
	computation # event(display_option(output, New, not_in,
				[append("String"), put("String"), terminal])
			).


legal_new_option(type, ground, Attributes, _, Attributes^) :-
	computation # event(display(type_default, cant_be, ground)).
legal_new_option(type, unparse, Attributes, _, Attributes^) :-
	computation # event(display(type_default, cant_be, unparse)).
legal_new_option(read, Value, Attributes, _, Attributes^) :-
    Value =\= char |
	computation # event(display(cant_reset_default_read)).
legal_new_option(output, Value, Attributes, _, Attributes^) :-
    Value =\= terminal, Value =\= append(_) |
	computation # event(display(output_default, cant_be, Value)).
legal_new_option(_, _, _, State, State^) :-
    otherwise |
	true.


route_output(Options, MSS, Common, Routed) :-

    arg(3, Options, terminal) :
      Common = fork(Common1, Common2),
      Routed = routed(Common2) |
	sio_mss(MSS?, Common1);

    arg(3, Options, OpCode(FileName)) :
      Common = fork(Common1, Common2),
      Routed = routed(Common1),
      Common2 =
	  computation(file # put_file(FileName, MSS?, [OpCode], Reply),
		      Common3)
	|
	check_file(Reply, put, FileName, Common3);

    known(Common) : Options = _, MSS = _,
      Routed = routed(Common) .

sio_mss(MSS, Common) :-

    MSS ? MS :
      Common = computation(sio(MS), Common') |
	self;

    MSS = [] :
      Common = done;

    known(Common) : MSS = _ .


make_messages(N, Options, Contents, Common, Messages) :-

    known(Common),
    Options = {_TDL, _IW, _DM, {_F, _P, Cs}} : N = _, Contents = _,
      Messages = [] |
	closes(Cs);

    Contents ? Message :
      Common = fork(Common1, Common2) |
	options_fixed(Options, Common1, Fixed),
	make_messages_plus_n(Fixed, '-', N, _, Options1),
	make_message(Options1, Message, Common1, Messages\Messages',
			Continue
	),
	make_messages(Continue, N, Fixed, Contents', Common2, Messages');

    Contents = [],
    Options = {_TDL, _IW, _DM, {_F, _P, Cs}} : N = _,
      Messages = [] |
	options_fixed(Options, Common, Fixed),
	when(Fixed, Common, [done(Common) | Cs], Closes),
	closes(Closes);

    Contents =\= [_|_], Contents =\= [] |
	make_messages_plus_n(Options, '\', N, done(Common), Options'),
	make_message(Options', Contents, Common, Messages\[]).


make_messages(continue, N, Options, Stream, Common, Messages) :-
    N' := N + 1 |
	make_messages(N', Options, Stream, Common, Messages).


make_messages_plus_n(Options1, Infix, N, Extra, Options2) :-
    Options1 = {TDL, IW, DM, {F, P, Cs}} :
      Options2 = {TDL, IW, DM, {F, P', Cs'}} |
	prefix_message(P, Infix, N, P'),
	close_message(Infix, Extra, Cs, Cs').


prefix_message([], _, _, []^).
prefix_message({Term}, '\', 1, {Term}^).
prefix_message({Term}, Infix, N, {{Infix, Term, N}}^) :-
    otherwise |
	true.


close_message('-', _, _, []^).
close_message('\', Extra, Cs, [Extra | Cs]^).


options_fixed(Options, Common, Fixed) :-

    Options = {{Ks, D, L}, IW, DM, Plus},
    Ks ? Known,
    known(Known) :
      Options' = {{Ks', D, L}, IW, DM, Plus} |
	options_fixed;

    otherwise : Common = _,
      Options = Fixed ;

    known(Common) :
      Options = Fixed .


make_message(Options, Contents, Common, Msd) :-
	options_fixed(Options, Common, Fixed),
	make_message(Fixed, Contents, Common, Msd, _).


make_message(Options, Message, Common, DMs, Continue) :-

    Options = {TDL, IW, Dispose, {Flag, Prefix, Closes}} :
      Common = fork(Common1, Common2) |
	combine_flag_prefix(Flag, Prefix, Combined),
	ground_or_freeze(TDL, Combined, Message, Common1, Front, Back),
	make_head(Front, Common2, List, Tail),
	make_tail(Flag, Dispose, Back, Tail),
	convert_to_string(List, IW, Message', Common2, Common'),
	when(Message', Common', continue(Dispose, Closes),
				Continue(Finish, Cs)
	),
	output_message(Finish, Message', Common', DMs),
	closes(Cs);

    known(Common),
    Options = {_TDL, _IW, Dispose, {_F, _P, Cs}} : Message = _,
      DMs = Ms\Ms,
      Continue = continue |
	invent_answer(Dispose),
	closes(Cs).


combine_flag_prefix(list, _, list^).
combine_flag_prefix(_, Prefix, Prefix^) :-
    otherwise |
	true.


ground_or_freeze(TDL, Combined, Contents, Common, Front, Back) :-

    Combined = [] :
      Front = [] |
	ground_or_freeze(TDL, Common, {Contents}, {Back});

    Combined = {Prefix} :
      Front = {GFP} |
	ground_or_freeze(TDL, Common, {Prefix, Contents}, {GFP, Back});

    Combined = list :
      Common = fork(Common1, Common2),
      Front = [] |
	collapse_list(Contents, Common1, 1, T, T, Tuple),
	when(Tuple, Common1, TDL, TDL'),
	ground_or_freeze(TDL', Common2, Tuple, Tuple'),
	expand_tuple(Contents, 1, Tuple', Contents', Common1(Contents', Back)).


collapse_list(List, Common, N, Tuple1, Tuple2, Tuple3) :-

    List ? C,
    constant(C) |
	collapse_list;

    List =\= [_|_],
    make_tuple(N, Tuple),
    arg(N, Tuple, List) : Common = _,
      Tuple1 = Tuple,
      Tuple2 = Tuple3 ;

    otherwise,
    List ? A,
    N' := N + 1 |
	collapse_list,
	arg(N, Tuple2, A, Common, Tuple2');

    known(Common) : List = _, N = _, Tuple1 = _, Tuple2 = _,
      Tuple3 = {[]} .


arg(N, T1, A, Common, T2) :-

    arg(N, T1, A^) : Common = _,
      T1 = T2 ;

    known(Common) : N = _, T1 = _, A = _,
      T2 = {[]} .


expand_tuple(L1, N, T, L2, Done) :-

    L1 ? C,
    constant(C) :
      L2 ! C |
	expand_tuple;

    otherwise,
    L1 ? _, 
    arg(N, T, A),
    N' := N + 1 :
      L2 ! A |
	expand_tuple;

    L1 =\= [_|_],
    arg(N, T, L2^) :
      Done = done(GFL, GFL) ;

    Done = Common(_, []^),
    known(Common) : L1 = _, N = _, T = _, L2 = _ .


ground_or_freeze(TDL, Common, Tuple1, Tuple2) :-

    TDL = ground(Depth, Length) |
	ground(ground, Common, Tuple1, Tuple2, Depth, Length);

    TDL = unparse(Depth, Length) |
	ground(unparse, Common, Tuple1, Tuple2, Depth, Length);

    otherwise, known(Tuple1),
    TDL = {Other, Depth, Length},
    known(Other), known(Depth), known(Length) :
      Common = computation(dictionary(freeze, Tuple1, Tuple2, Depth,
				      Length, ignore, Other),
			   done
		) ;

    known(Common) : TDL = _, Tuple1 = _ |
	unify_without_failure(Tuple2, {''}),
	unify_without_failure(Tuple2?, {'', ''}).


ground(Style, Common, Tuple1, Tuple2, Depth, Length) :-
    A := arity(Tuple1),
    make_tuple(A, Tuple) |
	ground_args(Style, Common, Tuple1, Tuple, Depth, Length,
			done(Tuple), Done(Tuple2), 1
	),
	when(Done, Common, done, Common).

ground_args(Style, Common, From, To, Depth, Length, Left, Right, N) :-

    N =< arity(From),
    arg(N, From, A),
    invalid(A, Kind),
    arg(N++, To, B),
    string_to_dlist("INVALID[", Invalid, KL) :
      ascii(']', R) |
	string_to_dlist(Kind, KL, [R]),
	list_to_string_close(Invalid , B, Left, Left'),
	ground_args;

    N =< arity(From),
    arg(N, From, A),
    constant(A),
    arg(N++, To, B) :
      A = B |
	ground_args;

    N =< arity(From),
    unknown(Common),
    arg(N, From, A),
    compound(A), A =\= [_|_],
    arg(N++, To, B) |
	ground_args,
	ground_arg(Style, Common, A, B, Depth, Length, Left, Left');

    N =< arity(From),
    unknown(Common),
    arg(N, From, A),
    A ? E, Length > 0,
    arg(N++, To, B),
    Length1 := Length - 1 :
      [E' | A''] = B |
	ground_args,
	ground_arg(Style, Common, E, E', Depth, Length1, Left, Left'),
	ground_arg(Style, Common, A', A'', Depth, Length1, Left', Left'');

    N =< arity(From),
    unknown(Common),
    arg(N, From, A),
    list(A), Length =< 0,
    arg(N++, To, B) :
      "..." = B |
	ground_args;

    N > arity(From) : Style = _, Common = _, To = _, Depth = _, Length = _,
      Left = Right ;

    N =< arity(From),
    known(Common),
    arg(N++, To, B) :
      "..." = B |
	ground_args.

ground_arg(Style, Common, From, To, Depth, Length, Left, Right) :-

    constant(From) : Style = _, Common = _, Depth = _, Length = _,
      From = To,
      Left = Right ;

    unknown(Common),
    From ? E, Length > 0,
    Length' := Length - 1 :
      To ! E' |
	ground_arg,
	ground_arg(Style, Common, E, E', Depth, Length', Left, Left');

    From = `Name, string(Name),
    Style = unparse : Common = _, Depth = _, Length = _,
      Name = To,
      Left = Right ;

    From = ?Name, string(Name),
    Style = unparse,
    string_to_dlist(Name, NL, [QM]) : Common = _, Depth = _, Length = _,
      ascii("?", QM),
      Left = Right |
	list_to_string(NL, To);

    unknown(Common),
    Depth > 0,
    tuple(From),
    otherwise,		% Not  unparse  Parsed Variable  (we or ro)
    Depth' := Depth - 1,
    A := arity(From),
    make_tuple(A, Tuple) :
      Tuple = To |
	ground_args(Style, Common, From, To, Depth', Length, Left, Right, 1);

    unknown(Common),
    compound(From),
    otherwise : Style = _, Depth = _, Length = _,
      Left = Right |
	ground_compound(From, To);

    known(Common) : Style = _, From = _, Depth = _, Length = _,
      To = "...",
      Left = Right .

list_to_string_close(From, To, Left, Right) :-
    list_to_string(From, String) :
      Left = Right,
      To = String.

ground_compound(From, To) :-

    tuple(From) :		% Tuple - Depth limit reached
      To = "..." ;

    list(From) :		% List - Length limit reached
      To = "..." ;

    vector(From) :
      To = From ;

    otherwise : From = _,	% Whatever else there may be
      To = "<??>" .


make_head({GFP}, _, [GFP, ' ' | Tail]^, Tail) :-
    known(GFP) |
	true.
make_head([], _, List^, List).
make_head(_, Done, Tail^, Tail) :-
    known(Done) |
	true.


make_tail(term, Dispose, GFC, [GFC, '
'				]^
) :-
    Dispose =\= read(_),
    known(GFC) |
	true.
make_tail(list, _, GFC, GFC^).
make_tail(Flag, read(_), GFC, Tail) :-
    known(GFC), Flag =\= list :
      Tail = [GFC, ' ? '] .

convert_to_string(Terms, Options, String, Common1, Common2) :-

    known(Terms) :
      Common1 = context(terms_to_string #
		    acyclic_grounded_terms_to_string(Terms, Options, String),
			Common2
		) ;

    known(Common1) : Terms = _, Options = _,
      String = '',
      Common1 = Common2 .

%	Synchronization Procedures

closes(Cs) :-

    Cs ? {Left, Right} |
	closes,
	unify_without_failure(Left, Right);

    Cs = [] |
	true.

when(Complete, Common, Done1, Done2) :-

    known(Complete) : Common = _,
      Done1 = Done2 ;

    known(Common) : Complete = _,
      Done1 = Done2 ;

    otherwise : Complete = _, Common = _, Done1 = _, Done2 = _ .


output_message(Dispose, Message, Common, DMs) :-

    known(Common) : Message = _,
      DMs = Ms\Ms |
	invent_answer(Dispose);

    Dispose = terminal :
      DMs = [output(Message) | Ms]\Ms |
	unify_without_failure(done, Common);

    Dispose = read(Read) :
      DMs = [query(Message, Read) | Ms]\Ms |
	unify_without_failure(done, Common);

    Dispose = put(_FileName) :
      DMs = [Message | Ms]\Ms |
	unify_without_failure(done, Common);

    Dispose = append(_FileName) :
      DMs = [Message | Ms]\Ms |
	unify_without_failure(done, Common).


check_file(true, _, _, done^).
check_file(Reply, Function, FileName, Common) :-

    Reply = true : Function = _, FileName = _,
      Common = done ;

    Reply =\= true :
      Common = computation(event(display(Function(FileName)) - Reply), done) ;

    known(Common) : Reply = _, Function = _, FileName = _ .


invent_answer(Dispose) :-

    Dispose = read(char(Char)) :
      Char = '' ;

    Dispose = read(string(String)) :
      String = aborted ;

    Dispose = read(chars(DL)) :
      DL = L\L ;

    otherwise : Dispose = _ .
