-export([xci/0, file/1, prefix/3, transform/2]).
-language(compound).
-mode(trust).

xci :-
	computation # (self # service_id(SId)),
	edit(SId, xci_codes, SId, xcicodes).

file(File) :-
	computation_utils # call_id_goal(computation#File, SId, Name, Ok),
	Ok? = true,
	edit(SId, Name, SId, Name).

prefix(Prefix, H, CP) :-
    string(Prefix) |
	computation_utils # call_id_goal(computation#H, Id1, Name1, Ok1),
	computation_utils # call_id_goal(computation#CP, Id2, Name2, Ok2),
	Ok1? = true,
	Ok2? = true,
	edit.

transform(H, CP) :-
	computation_utils # call_id_goal(computation#H, Id1, Name1, Ok1),
	computation_utils # call_id_goal(computation#CP, Id2, Name2, Ok2),
	Ok1? = true,
	Ok2? = true,
	edit(Id1, Name1, Id2, Name2).

edit(Id1, Name1, Id2, Name2) + (Prefix = "") :-
    known(Id1), string(Name1),
    known(Id2), string(Name2) :
      Ss !
"-includable(true).
-language(nil).

" 	|
	utils # append_strings([Name1, ".h"], NameH),
	file # execute_in_context(Id1, get_file(NameH, S, [], Ok)),
	Ok? = true,
	string_to_dlist(S, Cs, []),
	parse # tokenize(Cs, Ds),
	undefine(Ds, Prefix, Es),
	format(0, -41, 45, Es, Ss'),
	utils # append_strings([Name2, ".cp"], NameCP),
	file # execute_in_context(Id2, put_file(NameCP, Ss, [], Ok1)),
	Id1 # "_unique_id"(UId),
	written(Ok1, UId, NameCP).

written(true, UId, NameCP) :-
	utils # append_strings([UId, NameCP], UN),
	computation # display(term, UN - written, type(ground)).

undefine(In, Prefix, Out) :-

    In = ["#", define, Name, Value | In'],
    Name = `_,
    number(Value), Value =\= 0,
    Prefix = "" :
      Out ! (Name => Value) |
	self;

    In = ["#", define, Name, "-", Value | In'],
    Name = `_,
    number(Value),
    Value' := -Value,
    Prefix = "" :
      Out ! (Name => Value') |
	self;

    In = ["#", define, Name, 0 | In'],
    Name = `_,
    Prefix = "" :
      Out ! (Name => Value) |
	hex_value(In', In'',Value),
	self;

    In = ["#", define, Var, Value | In'],
    Var = `Name,
    number(Value), Value =\= 0,
    Prefix =\= "",
    string_to_dlist(Prefix, Ps,Cs),
    string_to_dlist(Name, Cs, []) :
      Out ! (`Name' => Value) |
	capitalize(Ps, Name'),
	self;

    In = ["#", define, Var, "-", Value | In'],
    Var = `Name,
    number(Value),
    Value' := -Value,
    Prefix =\= "",
    string_to_dlist(Prefix, Ps,Cs),
    string_to_dlist(Name, Cs, []) :
      Out ! (`Name' => Value') |
	capitalize(Ps, Name'),
	self;

    In = ["#", define, Var, 0 | In'],
    Var = `Name,
    Prefix =\= "",
    string_to_dlist(Prefix, Ps,Cs),
    string_to_dlist(Name, Cs, []) :
      Out ! (`Name' => Value) |
	capitalize(Ps, Name'),
	hex_value(In', In'',Value),
	self;

    In = ["#", define, Name, Value | In'],
    string(Name),
    string_to_dlist(Prefix, Ps,Cs),
    string_to_dlist(Name, Cs, []),
    number(Value), Value =\= 0 :
      Out ! (`Name' => Value) |
	capitalize(Ps, Name'),
	self;

    In = ["#", define, Name, "-", Value | In'],
    string(Name),
    string_to_dlist(Prefix, Ps,Cs),
    string_to_dlist(Name, Cs, []),
    number(Value),
    Value' := -Value :
      Out ! (`Name' => Value') |
	capitalize(Ps, Name'),
	self;

    In = ["#", define, Name, 0 | In'],
    string(Name),
    string_to_dlist(Prefix, Ps,Cs),
    string_to_dlist(Name, Cs, []) :
      Out ! (`Name' => Value) |
	capitalize(Ps, Name'),
	hex_value(In', In'',Value),
	self;

    In = ["#", define, Var, string(Value, DoubleQuote) | In'],
    Var = `Name,
    string_to_dlist(Prefix, Ps,Cs),
    string_to_dlist(Name, Cs, []),
    DoubleQuote =:= ascii('"'),
    string_to_dlist(Value, VL, [DoubleQuote]) :
      Out ! (`Name' => Quoted) |
	list_to_string([DoubleQuote | VL], Quoted),
	capitalize(Ps, Name'),
	self;

    In = ["#", define, Name, string(Value, DoubleQuote) | In'],
    string(Name),
    string_to_dlist(Prefix, Ps,Cs),
    string_to_dlist(Name, Cs, []),
    DoubleQuote =:= ascii('"'),
    string_to_dlist(Value, VL, [DoubleQuote]) :
      Out ! (`Name' => Quoted) |
	list_to_string([DoubleQuote | VL], Quoted),
	capitalize(Ps, Name'),
	self;

    In ? _,
    otherwise |
	self;

    In = [] : Prefix = _,
      Out = [] .

hex_value(In1, In2, Value) :-

    In1 ? Hex,
    string(Hex),
    1 < string_length(Hex),
    string_to_dlist(Hex, Chars, []),
    Chars ? X, X =:= ascii('x') |
	convert_hex + (In0 = In1, Cumulant = 0);

    In1 ? Hex,
    string(Hex),
    1 < string_length(Hex),
    string_to_dlist(Hex, Chars, []),
    Chars ? X, X =:= ascii('X') |
	convert_hex + (In0 = In1, Cumulant = 0);

    otherwise :
      In2 = In1,
      Value = 0 .

convert_hex(In0, In2, Value, In1, Chars, Cumulant) :-

    Chars ? C,
    ascii('0') =< C, C =< ascii('9'),
    Cumulant' := 16 * Cumulant + C - ascii('0') |
	self;

    Chars ? C,
    ascii('a') =< C, C =< ascii('f'),
    Cumulant' := 16 * Cumulant + C - ascii('a') + 10 |
	self;

    Chars ? C,
    ascii('A') =< C, C =< ascii('F'),
    Cumulant' := 16 * Cumulant + C - ascii('A') + 10 |
	self;

    Chars = [] : In0 = _,
      In2 = In1,
      Value = Cumulant;

    otherwise : In1 = _, Chars = _, Cumulant = _,
      Value = 0,
      In2 = In0 .

capitalize(Cs, Name') :-

    Cs ? C,
    ascii('a') =< C, C =< ascii('z'),
    C' := C - ascii('a') + ascii('A') |
	list_to_string([C' | Cs'], Name');

    otherwise,
    list_to_string(Cs, Name) :
      Name' = Name .


format(P1, PO, P2, In, Out) :-

    In ? {OP, `A1, A2},
    convert_to_string(A2, S2) :
      Out = [R, '.
' |   Out'] |
	just("", P1, A1, L),
	just(L, PO, OP, M),
	just(M, P2, S2, R),
	self;

    In ? Other,
    otherwise |
	computation # display(term, Other, type(unparse)),
	self;

    In = [] : P1 = _, PO = _, P2 = _,
      Out = [] .

just(In, At, Add, Out) :-

    At > 0,
    Blanks := string_length(In) + 1 - At |
	add_blanks(In, Blanks, Add, Out);

    At < 0,
    Blanks := -At - string_length(Add) - string_length(In) |
	add_blanks(In, Blanks, Add, Out);

    At = 0, In = "" :
      Add = Out .
	

add_blanks(In, Blanks, Add, Out) + (Head = L, Tail = L) :-

    Blanks-- > 0 :
      ascii(' ', BL),
      Tail ! BL |
	self;

    Blanks =< 0,
    string_to_dlist(In, H, A),
    string_to_dlist(Add, A', E) :
      ascii(' ', BL),
      A = Head,
      Tail = [BL | A'],
      E = [] |
	list_to_string(H, Out).
