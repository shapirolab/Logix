-language([evaluate,compound,colon]).
-mode(interrupt).
-export([based/0, channels/0, channels/1, instantaneous/0, stuff/2]).
-include(spi_constants).

based :-
	channels(based).

instantaneous :-
	channels(instantaneous).

channels + (Type = all) :-
	spi_monitor#status(Status),
	extract_anchors,
	show_queues(Anchors, CHs, []),
	screen#display_stream(CHs?).

extract_anchors(Type, Status, Anchors) :-

    Status =?= [anchors(Channels) | _] |
	extract_anchors_by_type;

    Status ? Other, Other =\= anchors(_) |
	self;

    Status = [] :
      Type = _,
      Anchors = [].

extract_anchors_by_type(Type, Channels, Anchors) :-

    Type =?= all :
      Anchors = Channels;

    Type =\= all,
    Channels = [Channel | _],
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    Name =?= Type :
      Anchors = [Channel];

    Type =\= all,
    Channels ? Channel,
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    Name =\= Type |
	self;

    Channels = [] :
      Type = _,
      Anchors = [].

show_queues(Anchors, CHs, NCHs) :-

    Anchors ? Anchor |
	show_channels(Anchor, CHs, CHs', Anchor),
	self;

    Anchors =?= [] :
      CHs = NCHs.
	

show_channels(Channel, CHs, NCHs, Close) :-

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    Channel' =\= Close,
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    Type =\= SPI_CHANNEL_ANCHOR :
      CHlist = [CH | CHs'] |
	format_channel(Channel', CH, CHs, CHlist),
	self;

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    Channel' =\= Close,
    read_vector(SPI_CHANNEL_TYPE, Channel', Type),
    Type =?= SPI_CHANNEL_ANCHOR |
	self;

    read_vector(SPI_NEXT_CHANNEL, Channel, Channel'),
    Channel' =?= Close :
      CHs = NCHs .


stuff(Name, Value) :-

	screen#display(Name = Out, known(Out)),
	format(Value, Out).

format(Value, Out) :-

    we(Value) : Out = "!";

    ro(Value) : Out = "?";

    constant(Value) : Out = Value;

    list(Value) |
	format_list(Value, NewList, Out, NewList);

    tuple(Value) |
	format_tuple(Value, NewTuple, Out, NewTuple);

    Value = {PId, Channel, V, C},
    string(PId), vector(Channel), we(V), we(C) :
      Common =       common(pid(PId), CH?, value(V), chosen(C)) |
	format_channel(Channel, CH, Common, Out);

    vector(Value) |
	format_channel(Value, Out, _, _);

    otherwise :
      Out = Value.


format_channel(Channel, CH, Left, Right) :-

    vector(Channel),
    read_vector(SPI_BLOCKED,          Channel, Blocked),
    read_vector(SPI_CHANNEL_TYPE,     Channel, Type),
    read_vector(SPI_CHANNEL_RATE,     Channel, Rate),
    read_vector(SPI_CHANNEL_REFS,     Channel, Refs),
    read_vector(SPI_SEND_ANCHOR,      Channel, Sends),
    read_vector(SPI_SEND_WEIGHT,      Channel, WeightS),
    read_vector(SPI_RECEIVE_ANCHOR,   Channel, Receives),
    read_vector(SPI_RECEIVE_WEIGHT,   Channel, WeightR),
/*
    read_vector(SPI_NEXT_CHANNEL,     Channel, Next),
	read_vector(SPI_CHANNEL_NAME, Next, NameN),
	read_vector(SPI_CHANNEL_TYPE, Next, TypeN),
    read_vector(SPI_PREVIOUS_CHANNEL, Channel, Prev),
	read_vector(SPI_CHANNEL_NAME, Prev, NameP),
	read_vector(SPI_CHANNEL_TYPE, Prev, TypeP),
*/
    read_vector(SPI_CHANNEL_NAME,     Channel, Name) :
      CH = Name(TypeRate, Send, Receive) - BlockedRefs |
	format_typerate(Type, Rate, TypeRate, Mid1, Right),
	format_blockedrefs(Blocked, Refs, BlockedRefs, Mid2, Mid1),
	format_send(Sends, WeightS, Send, Mid3, Mid2),
	format_receive(Receives, WeightR, Receive, Left, Mid3);

    otherwise :
      CH = Channel,
      Left = Right.

format_typerate(Type, Rate, TypeRate, Left, Right) :-

    bitwise_and(Type, SPI_TYPE_MASK, BasicType),
    bitwise_and(Type, SPI_PRIME_FLAG, Primed),
    bitwise_and(Type, SPI_RANDOM_FLAG, Randomized) |
	list_randomized,
	list_basic_type,
	list_primed,
	format_typelist_rate.

  list_randomized(Randomized, TypeList, BasicList) :-
    Randomized =?= 0 :
      TypeList = BasicList;
    Randomized =\= 0 :
      TypeList = [CHAR_HASH | BasicList].

   list_basic_type(BasicType, BasicList, PrimeTail) :-

    BasicType =?= SPI_CHANNEL_ANCHOR,
    string_to_dlist(anchor, L, PrimeTail) :
      BasicList = L;

    BasicType =?= SPI_UNKNOWN,
    string_to_dlist(unknown, L, PrimeTail) :
      BasicList = L;

    BasicType =?= SPI_BIMOLECULAR,
    string_to_dlist(bimolecular, L, PrimeTail) :
      BasicList = L;

    BasicType =?= SPI_HOMODIMERIZED,
    string_to_dlist(homodimerized, L, PrimeTail) :
      BasicList = L;

    BasicType =?= SPI_INSTANTANEOUS,
    string_to_dlist(instantaneous, L, PrimeTail) :
      BasicList = L;

    BasicType =?= SPI_SINK,
    string_to_dlist(sink, L, PrimeTail) :
      BasicList = L;

    otherwise,
    convert_to_string(BasicType, S),
    string_to_dlist(S, L, PrimeTail) :
      BasicList = L.

  list_primed(Primed, PrimeTail) :-
    Primed =?= 0 :
      PrimeTail = [];
    Primed =\= 0 :
      PrimeTail = [CHAR_PRIME].

  format_typelist_rate(TypeList, Rate, TypeRate, Left,Right) :-
    list_to_string(TypeList, TypeName) :
      TypeRate = TypeName(Rate),
      Left = Right.

format_blockedrefs(Blocked, Refs, BlockedRefs, Left, Right) :-

    Blocked =?= TRUE :
      BlockedRefs = blocked(Refs),
      Left = Right;

    Blocked =?= FALSE :
      BlockedRefs = Refs,
      Left = Right;

    otherwise :
      BlockedRefs = blockedrefs(Blocked, Refs),
      Left = Right.


format_send(Sends, Weight, Send, Left, Right) :-

    arg(SPI_MESSAGE_LINKS, Sends, NextLink),
    read_vector(SPI_NEXT_MS, NextLink, Message),
    Message =\= Sends :
      Send = sends(Weight),
      Left = Right;

    otherwise :
      Sends = _,
      Weight = _,
      Send = no_sends,
      Left = Right.

format_receive(Receives, Weight, Receive, Left, Right) :-

    arg(SPI_MESSAGE_LINKS, Receives, NextLink),
    read_vector(SPI_NEXT_MS, NextLink, Message),
    Message =\= Receives :
      Receive = receives(Weight),
      Left = Right;

    otherwise :
      Receives = _,
      Weight = _,
      Receive = no_receives,
      Left = Right.

/*
format_link(Name, Type, Out, Left, Right) :-

    Type =?= SPI_CHANNEL_ANCHOR :
      Name = _,
      Out = "",
      Left = Right;

    Type =\= SPI_CHANNEL_ANCHOR :
      Out = Name,
      Left = Right.
*/

format_list(ListIn, ListOut, Left, Right) :-

    ListIn ? In :
      ListOut ! Out |
	format_list_element(In, Out, Left, Left'),
	self;

    otherwise :
      ListOut = ListIn,
      Left = Right.

format_list_element(In, Out, Left, Right) :-

    tuple(In) |
	format_tuple(In, Out, Left, Right);

    vector(In), arity(In, 11) |
	format_channel(In, Out, Left, Right);

    otherwise :
      Out = In,
      Left = Right.

format_tuple(In, Out, Left, Right) :-

    In =?= {PId, OpList, Value, Chosen},
    string(PId), list(OpList),
    we(Value), we(Chosen) :
      Out = common(PId, [messages], Value, Chosen),
      Left = Right;

    In =?= {PId, MsList, Value, Chosen, Reply},
    string(PId), list(MsList),
    unknown(Value), unknown(Chosen), unknown(Reply) :
      Out = transmission(PId, [operations], Value, Chosen, Reply),
      Left = Right;

    In =?= {Type, CId, Channel, Multiplier, Tags},
    integer(Type), 1 =< Type, Type =< 3, string(CId),
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier),
    read_vector(SPI_CHANNEL_NAME, Channel, Name) :
      Out = operation(MsType, CId, Name, Multiplier, Tags) |
	format_message_type(Type, MsType, Left, Right);

    In =?= {Type, CId, Channel, Multiplier, SendTag, ReceiveTag, Common, _},
    integer(Type), 1 =< Type, Type =< 5, string(CId),
    vector(Channel), arity(Channel, CHANNEL_SIZE),
    integer(Multiplier), integer(SendTag), integer(ReceiveTag),
    tuple(Common), arity(Common, 4),
    read_vector(SPI_CHANNEL_NAME, Channel, Name) :
      Out = message(MsType, CId, Name, Multiplier, SendTag, ReceiveTag) |
	format_message_type(Type, MsType, Left, Right);

    In =?= {SPI_MESSAGE_ANCHOR, CId, [], 0, 0, 0, [], _},
    string(CId) :
      Out = message_anchor(CId),
      Left = Right;

    otherwise :
      Out = other(In),
      Left = Right.

format_message_type(Type, MsType, Left, Right) :-

    Type =?= SPI_MESSAGE_ANCHOR :
      MsType = message_anchor,
      Left = Right;

    Type =?= SPI_SEND :
      MsType = send,
      Left = Right;

    Type =?= SPI_RECEIVE :
      MsType = receive,
      Left = Right;

    Type =?= SPI_DIMER :
      MsType = dimer,
      Left = Right;

    otherwise :
      MsType = other(Type),
      Left = Right.
