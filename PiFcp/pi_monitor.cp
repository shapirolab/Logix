-monitor(serve).
-language(compound).
-export([get_global_channels/1, global_channels/1, options/2, reset/1]).

serve(In) :-
    Dash := ascii('-') |
	serve(In, 1, Dash, nil, [], [{0, _}, {[], _}]).

serve(In, UID, Dash, Tree, Options, Global) :-

    In ? unique_sender(String, Name),
    string_to_dlist(String, Head, [Dash | Tail]),
    convert_to_string(UID++, Suffix),
    string_to_dlist(Suffix, Tail, []),
    list_to_string(Head, Concatenated) :
      Name = Concatenated? |
	self;

    In ? global_channels(List) |
	merge_channels(List, Global, Global', ""),
	self;

    In ? get_global_channels(List),
    we(List) :
      List = List'? |
	copy_global(Global, List', _),
	self;

    In ? reset(List),
    we(List) :
      List = List'? |
	copy_global(Global, List', Global'),
	self;

    In ? options(New, Old) :
      Options' = New? |
	unify_without_failure(Options, Old),
	self;

    In ? Other,
    otherwise |
	self,
	fail(Other, unknown);

    In = [] :
      UID = _,
      Dash = _,
      Tree = _,
      Options = _,
      Global = _ .


merge_channels(List, Global, New_Global, Last) :-

    List = [] :
      Last = _,
      New_Global = Global;

    List ? Name(NewChannel, _ReceiveMean, _SendMean), string(Name),
    Last @< Name,
    we(NewChannel),
    Global ? Name(PiChannel),
    PiChannel = Id(Vector, Stream, ReceiveMean, SendMean) :
      NewChannel = Id(Vector, Stream'?, ReceiveMean, SendMean),
      New_Global ! Name(NewChannel),
      Last = _,
      Last' = Name |
	cdr_past_msgs(Stream, Stream'),
	self;

    List = [Name(_, _, _)|_], string(Name),
    Global ? Entry,
    Entry = Last'(_GlobalChannel),
    Last' @< Name :
      Last = _,
      New_Global ! Entry |
	self;

    List ? Name(NewChannel, ReceiveMean, SendMean),
    we(NewChannel),
    Global = [Name1(_GlobalChannel) | _],
    string(Name),
    Last @< Name, Name @< Name1,
    string_to_dlist("global.", GL, GT),
    string_to_dlist(Name, NL, []) :
      NewChannel = PiChannel?,
      New_Global ! Name(NewChannel),
      GT = NL,
      Last' = Name |
	list_to_string(GL, Id),
	pi_channel(PiChannel, Id?, _, _, ReceiveMean, SendMean),
	self;

    otherwise :
      Last = _,
      New_Global = Global |
	fail(merge_channels(List)).


copy_global(Global, List, Ends) :-
    Global ? Head :
      Ends ! Head |
	copy_interior.

  copy_interior(Global, List, Ends) :-

    Global ? Entry,
    Global' =\= [] :
      List ! Entry |
	self;

    Global = [_] :
      List = [],
      Ends = Global.


cdr_past_msgs(In, Out) :-

    In ? _Sender(_Msg, _Tag, Choice),
    known(Choice) |
	self;

    In = [_Sender(_Msg, _Tag, Choice) | _],
    unknown(Choice) :
      Out = In;

    unknown(In) :
      Out = In.
