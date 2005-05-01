-language([evaluate,compound,colon]).
-mode(trust).
-export([make_channel/1, make_channel/2, make_channel/3, make_channel/4,
	 "SPC"/1, "SPC"/2,
	 parse_options/5,
	 show_channel/3, show_goal/3, show_goals/3,
	 show_resolvent/3, show_value/3,
	 show_tree/3, close_tree/1,
	 dimer/3, dimer/4, dimer/5, dimer/6,
	 receive/2, receive/3, receive/4, receive/5,
	 send/2, send/3, send/4, send/5,
	 weighter/1]).

-include(spi_constants).

make_channel(Channel) :-
	make_channel(Channel, spi, infinite).
make_channel(Channel, BaseRate) :-
	make_channel(Channel, spi, BaseRate).
make_channel(Channel, Creator, BaseRate) :-

    we(Channel),
    number(BaseRate) :
      Channel = Channel'? |
	spi_monitor#new_channel(Creator, Channel', BaseRate);

    we(Channel),
    BaseRate =?= infinite :
      Channel = Channel'? |
	spi_monitor#new_channel(Creator, Channel', BaseRate);

    string(Channel), Channel =\= "_", Channel =\= "",
    number(BaseRate) |
	computation#dictionary(add, Channel, Ch?, Reply),
	made_channel(Channel, Creator, BaseRate, Ch, Reply);

    string(Channel), Channel =\= "_", Channel =\= "",
    BaseRate =?= infinite |
	computation#dictionary(add, Channel, Ch?, Reply),
	made_channel(Channel, Creator, BaseRate, Ch, Reply);

    otherwise :
      Creator = _,
      BaseRate = _ |
	computation#display(("Can't make_channel" : Channel)).

  made_channel(Name, Creator, BaseRate, Channel, Reply) :-
    Reply = new,
    string_to_dlist(Creator, CL, CT),
    string_to_dlist(Name, Cl, []) :
      CT = [CHAR_DOT | Cl] |
	list_to_string(CL, Creator'),
	spi_monitor#new_channel(Creator'?, Channel, BaseRate);

    otherwise :
      Creator = _,
      BaseRate = _,
      Channel = _ |
	computation#display(("spi_utils: Can't make_channel" : Name - Reply)).


make_channel(Channel, Creator, BaseRate, ComputeWeight) :-

    we(Channel),
    number(BaseRate) :
      Channel = Channel'? |
	make_channel_with_weight;

    we(Channel),
    BaseRate =?= infinite :
      Channel = Channel'? |
	make_channel_with_weight;

    string(Channel), Channel =\= "_", Channel =\= "",
    number(BaseRate) |
	computation#dictionary(add, Channel, Ch?, Reply),
	made_channel(Channel, Creator, BaseRate, ComputeWeight, Ch, Reply);

    string(Channel), Channel =\= "_", Channel =\= "",
    BaseRate =?= infinite |
	computation#dictionary(add, Channel, Ch?, Reply),
	made_channel(Channel, Creator, BaseRate, ComputeWeight, Ch, Reply);

    otherwise :
      Creator = _,
      BaseRate = _,
      ComputeWeight = _ |
	computation#display(("Can't make_channel" : Channel)).

  made_channel(Name, Creator, BaseRate, ComputeWeight, Channel, Reply) :-
    Reply = new,
    string_to_dlist(Creator, CL, CT),
    string_to_dlist(Name, Cl, []) :
      CT = [CHAR_DOT | Cl] |
	list_to_string(CL, Creator'),
	make_channel_with_weight;

    otherwise :
      Creator = _,
      BaseRate = _,
      ComputeWeight = _,
      Channel = _ |
	computation#display(("spi_utils: Can't make_channel" : Name - Reply)).

make_channel_with_weight(Channel, Creator, BaseRate, ComputeWeight) :-

    string(ComputeWeight) |
	spi_monitor#new_channel(Channel, Creator, ComputeWeight, BaseRate);

    tuple(ComputeWeight) |
	utils#tuple_to_dlist(ComputeWeight, [Name | Parameters], []),
	validate_parameters(Parameters, List, Invalid),
	validated_parameters;

    otherwise :
      Channel = _,
      Creator = _,
      BaseRate = _ |
	computation#display(
		("spi_utils: ComputeWeight must be a string or a tuple" :
				Creator - ComputeWeight)).

  validated_parameters(Channel, Creator, BaseRate, Name, List, Invalid) :-

    string(Name), Invalid =?= [] |
	utils#list_to_tuple([Name, _ | List], WeightTuple),
	spi_monitor#new_channel(Creator, Channel, WeightTuple, BaseRate);

    otherwise,
    known(Invalid) :
      Channel = _,
      Creator = _,
      BaseRate = _,
      List = _ |
	utils#list_to_tuple([Name | Invalid], BadTuple),
	computation#display(
		("spi_utils: Bad ComputeWeight elements" : Creator - BadTuple)).

send(Message, Channel) :-
    send(Message, Channel, 1, _, sender).
send(Message, Channel, Multiplier) :-
    send(Message, Channel, Multiplier, _, sender).
send(Message, Channel, Multiplier, Chosen) :-
	send(Message, Channel, Multiplier, Chosen, sender).
send(Message, Channel, Multiplier, Chosen, Name) :-
    vector(Channel),
    arity(Channel, CHANNEL_SIZE),
    we(Chosen) :
      Send = SPI_SEND(Name, Channel, Multiplier, 1) |
	spi_monitor#scheduler(S),
	write_channel(start(send, [Send], Value, Chosen), S),
	transmitted(sending(Name), 1, Chosen, Message, Value);

    string(Channel), Channel =\= "_", Channel =\= "" :
      Name = _ |
	computation#dictionary(find, Channel, Ch, Reply),
	send_message(Message, Channel, Multiplier, Chosen, Ch, Reply);

    otherwise :
      Message = _,
      Multiplier = _ |
	unify_without_failure(Chosen, 0),
	computation#display(("spi_utils: Can't send to" : Name(Channel))).

  send_message(Message, Name, Multiplier, Chosen, Channel, Reply) :-
    Reply = true |
	send(Message, Channel, Multiplier, Chosen, Name);

    otherwise :
      Message = _,
      Multiplier = _, 
      Channel = _ |
	unify_without_failure(Chosen, 0),
	computation#display(("spi_utils: Can't send to" : Name - Reply)).

transmitted(Id, Tag, Chosen, Message, Value) :-

    Chosen = Tag :
      Id = _,
      Value = Message;

    Chosen =\= Tag :
      Message = _,
      Id = _,
      Value = _.
      
 
receive(Channel, Message) :-
	receive(Channel, Message, 1, _, receiver).
receive(Channel, Message, Multiplier) :-
	receive(Channel, Message, Multiplier, _, receiver).
receive(Channel, Message, Multiplier, Chosen) :-
	receive(Channel, Message, Multiplier, Chosen, receiver).
receive(Channel, Message, Multiplier, Chosen, Name) :-
    string(Channel), Channel =\= "_", Channel =\= "" :
      Name = _ |
	computation#dictionary(find, Channel, Ch, Reply),
	receive_message(Channel, Message, Multiplier, Chosen, Ch, Reply);
    vector(Channel),
    arity(Channel, CHANNEL_SIZE),
    we(Chosen) :
      Receive = SPI_RECEIVE(Name, Channel, Multiplier, 2) |
	spi_monitor#scheduler(S),
	write_channel(start(receive, [Receive], Value, Chosen), S),
	transmitted(receiving(Name), 2, Chosen, Message, Value);

    otherwise :
      Message = _,
      Multiplier = _ |
	unify_without_failure(Chosen, 0),
	computation#display(("spi_utils: Can't receive from" : Name(Channel))).
	

  receive_message(Name, Message, Multiplier, Chosen, Channel, Reply) :-
    Reply = true |
	receive(Channel, Message, Multiplier, Chosen, Name);

    otherwise :
      Message = _,
      Multiplier = _,
      Channel = _ |
	unify_without_failure(Chosen, 0),
	computation#display(
		("spi_utils: Can't receive from" : Name - Reply)).


dimer(SendMessage, ReceiveMessage, Channel) :-
	dimer(SendMessage, ReceiveMessage, Channel, 1, _, dimer).
dimer(SendMessage, ReceiveMessage, Multiplier, Channel) :-
	dimer(SendMessage, ReceiveMessage, Channel, Multiplier, _, dimer).
dimer(SendMessage, ReceiveMessage, Multiplier, Channel, Chosen) :-
	dimer(SendMessage, ReceiveMessage, Channel, Multiplier, Chosen, dimer).
dimer(SendMessage, ReceiveMessage, Channel, Multiplier, Chosen, Name) :-
    vector(Channel),
    arity(Channel, CHANNEL_SIZE) :
      Chosen = Chosen'?,
      Dimer = SPI_DIMER(Name, Channel, {1, 2}, Multiplier) |
	spi_monitor#scheduler(S),
	write_channel(start(dimer, [Dimer], Value, Chosen'), S),
	dimered(Chosen'?, SendMessage, ReceiveMessage, Value);

    string(Channel), Channel =\= "_", Channel =\= "" :
      Name = _ |
	computation#dictionary(find, Channel, Ch, Reply),
	dimer_message(SendMessage, ReceiveMessage, Multiplier,
			Channel, Chosen, Ch, Reply);

    otherwise :
      SendMessage = _,
      ReceiveMessage= _,
      Multiplier = _ |
	unify_without_failure(Chosen, 0),
	computation#display(("spi_utils: Can't dimer on" : Name(Channel))).

  dimer_message(SendMessage, ReceiveMessage, Name, Multiplier,
			Chosen, Channel, Reply) :-
    Reply = true |
	dimer(SendMessage, ReceiveMessage, Channel, Multiplier, Chosen, Name);

    otherwise :
      SendMessage = _,
      ReceiveMessage = _,
      Multiplier = _,
      Channel = _ |
	unify_without_failure(Chosen, 0),
	computation#display(("spi_utils: Can't dimer on" : Name - Reply)).

dimered(Chosen, SendMessage, ReceiveMessage, Value) :-

    Chosen = 1 :
      ReceiveMessage = _,
      Value = SendMessage;

    Chosen = 2 :
      SendMessage = _,
      Value = ReceiveMessage;

    Chosen =\= 1, Chosen =\= 2 :
      SendMessage = _,
      ReceiveMessage = _,
      Value = _.


"SPC"(Channel) :-
	spi_monitor#options(Options, Options),
	"SPC"(Channel, Options?).

"SPC"(SpiChannel, Options) :-
	computation#display(term, Display, known(Display)),
	show_channel.

show_channel(SpiChannel, Options, Display) :-

    string(SpiChannel), SpiChannel =\= "_", SpiChannel =\= "" |
	computation#dictionary(find, SpiChannel, SpiChannel', Reply),
	show_spi_channel;

    otherwise |
	show_spi_channel + (Reply = true).


show_spi_channel(SpiChannel, Options, Display, Reply) :-
	
    Reply =\= true :
      Options = _,
      Display = "not_channel" |
	computation#display(("spi_utils: Can't show channel" :
				SpiChannel-Reply));

    Reply =?= true,
    vector(SpiChannel),
    read_vector(SPI_CHANNEL_NAME, SpiChannel, Name) |
	show_spi_channel1;

    otherwise :
      Options = _,
      Reply = _,
      Display = "not_channel" |
	computation#display(("spi_utils: Not a Spi channel" : SpiChannel)).

  show_spi_channel1(Name, SpiChannel, Options, Display) :-

    Options =?= fcp :
      Display = (Name : SpiChannel);

    Options =\= fcp,
    read_vector(SPI_CHANNEL_REFS, SpiChannel, Refs),
    Refs > 0 :
      make_channel(BadOption, _) |
	parse_options(Options, Depth(1), BadOption(BadOption),
			Which(note), Format(short)),
	inspect_channel(SpiChannel, Result),
	format_channel_name,
	channel_argument + (Display = Content, Status = Result,
		Left = Display, Right = (FormattedName? : Content));

    /* closed channel */
    otherwise :
      SpiChannel = _,
      Options = _,
      Display = [FormattedName?] |
	parse_options(Options, _Depth, _Order, _Which, Format(short)),
	format_channel_name.


inspect_channel(SpiChannel, Status) :-

    read_vector(SPI_CHANNEL_TYPE, SpiChannel, FullType),
    bitwise_and(FullType, SPI_TYPE_MASK, Type),
    read_vector(SPI_CHANNEL_RATE, SpiChannel, Rate),
    read_vector(SPI_SEND_ANCHOR, SpiChannel, SendAnchor),
    read_vector(SPI_RECEIVE_ANCHOR, SpiChannel, ReceiveAnchor),
    read_vector(SPI_CHANNEL_NAME, SpiChannel, Name) :
      Status = {Kind?, NameRate?, Sends?, Receives?} |
	name_rate_to_namerate,
	list_messages(SendAnchor, Sends, []),
	list_messages(ReceiveAnchor, Receives, []).

  name_rate_to_namerate(Type, Name, Rate, Kind, NameRate) :-

    Type =?= SPI_CHANNEL_ANCHOR :
      Rate = _,
      Kind = anchor,
      NameRate = namerate(Name, anchor);

    Type =?= SPI_UNKNOWN :
      Rate = _,
      Kind = unknown,
      NameRate = namerate(Name, unknown);

    Type =?= SPI_BIMOLECULAR :
      Kind = bimolecular,
      NameRate = namerate(Name, Rate);

    Type =?= SPI_HOMODIMERIZED :
      Kind = homodimerized,
      NameRate = namerate(Name, Rate);

    Type =?= SPI_INSTANTANEOUS :
      Rate = _,
      Kind = instantaneous,
      NameRate = namerate(Name, infinite);

    Type =?= SPI_SINK :
      Rate = _,
      Kind = sink,
      NameRate = namerate(Name, sink);

    otherwise :
      Type = _,
      Kind = other(Type),
      NameRate = namerate(other(Name), Rate).

  list_messages(Anchor, List, Tail) + (Message = Anchor) :-

    arg(SPI_MESSAGE_LINKS, Message, Links),
    read_vector(SPI_NEXT_MS, Links, Message'),
    Message' =\= Anchor,
    Message' =?= {Type, CId, _Channel, Multiplier, _SendTag, _ReceiveTag,
			Common, _Links, _Ambient},
    Common = {PId, _MsList, _Value, _Chosen} :
      List ! Kind(PId, CId, Multiplier) |
	message_type_to_kind,
	self;

    otherwise :
      Anchor = _,
      Message = _,
      List = Tail.      

  message_type_to_kind(Type, Kind) :-

    Type =?= SPI_SEND :
      Kind = send;

    Type =?= SPI_RECEIVE :
      Kind = receive;

    Type =?= SPI_DIMER :
      Kind = dimer;

    otherwise :
      Kind = other(Type).


parse_options(Options, Depth, Order, Which, Format) :-

    Options =\= [_|_], Options =\= [] :
      Options' = [Options] |
	self;

    Options ? Option |
	define_option;

    Options = [] |
	unify_without_failure(Options, _),
	unify_without_failure(Depth, D(D)),
	unify_without_failure(Order, O(O)),
	unify_without_failure(Which, W(W)),
	unify_without_failure(Format,F(F)).

  define_option(Options, Depth, Order, Which, Format, Option) :-

    Option = fcp |
	parse_options;

    integer(Option) :
      Depth = Option(_) |
	parse_options;

    Option = depth :
      Order = Option(_) |
	parse_options;

    Option = execute :
      Order = Option(_) |
	parse_options;

    Option = prefix :
      Order = Option(_) |
	parse_options;

    Option = none :
      Which = Option(_) |
	parse_options;

    Option = note :
      Which = Option(_) |
	parse_options;

    Option = active :
      Which = Option(_) |
	parse_options;

    Option = messages :
      Which = Option(_) |
	parse_options;

    Option = all :
      Which = Option(_) |
	parse_options;

    Option = short :
      Format = Option(_) |
	parse_options;

    Option = base :
      Format = Option(_) |
	parse_options;

    Option = creator :
      Format = Option(_) |
	parse_options;

    Option = full :
      Format = Option(_) |
	parse_options;

    otherwise |
	computation#display(("spi_utils: invalid_option" : Option)),
	parse_options.


show_channel_content(Stream, Depth, Format, Content, Left, Right) :-

    Stream =?= [] :
      Depth = _,
      Format = _,
      Content = [],
      Left = Right;

    Stream ? String,
    Stream' =?= [{_,_,_,_}|_] :
      Content ! String |
	self;

    Stream ? _Kind(PId, CId, Multiplier),
    tuple(PId), arg(1, PId, Functor),
    string(Functor) :
      PId' = Functor,
      Content ! Condensed? |
	id_and_message + (Left = Left, Right = Left'),
	self;

    Stream ? _Kind(PId, CId, Multiplier),
    otherwise :
      Content ! Condensed? |
	id_and_message + (Left = Left, Right = Left'),
	self;

    Stream ? _Other,
    otherwise |
	self.

  id_and_message(PId, CId, Multiplier, Condensed, Left, Right) :-

    Multiplier =\= 1 :
      Condensed = {CId, PId}*Multiplier,
      Left = Right;

    Multiplier =?= 1 :
      Condensed = {CId, PId}, 
      Left = Right.


show_tuple_args(Tuple, Which, Depth, Format,
		Index, Args, Left, Right) :-

    Index > 0,
    arg(Index, Args, Display),
    arg(Index, Tuple, Argument),
    Index-- |
	show_argument + (Left = Left, Right = Left'),
	self;

    Index =:= 0 :
      Tuple = _,
      Which = _,
      Depth = _,
      Format = _,
      Args = _,
      Left = Right.


show_value(Argument, Options, Display) :-

    Options =?= fcp :
      Display = Argument;

    Options =\= fcp :
      make_channel(BadOption, _) |
	parse_options(Options, Depth(1), BadOption(BadOption),
			Which(note), Format(short)),
	show_cdr.

show_cdr(Argument, Which, Depth, Format, Display) :-
    known(Argument) |
	show_argument + (Left = Display, Right = Display').

show_argument(Argument, Which, Depth, Format, Display, Left, Right) :-

    vector(Argument), arity(Argument, CHANNEL_SIZE),
    read_vector(SPI_CHANNEL_NAME, Argument, Name) |
	show_channel_argument + (SpiChannel = Argument);

    constant(Argument) :
      Which = _,
      Depth = _,
      Format = _,
      Display = Argument,
      Left = Right;

    ro(Argument) :
      Which = _,
      Depth = _,
      Format = _,
      Display = Argument,
      Left = Right;

    we(Argument) :
      Which = _,
      Depth = _,
      Format = _,
      Display = Argument,
      Left = Right;

    otherwise |
	show_compound.

format_channel_name(Name, Format, FormattedName) :-

    Format =\= short, Format =\= base,
    Name = ChannelName(Ordinal),
    string_to_dlist(ChannelName, CCN, Tail),
    convert_to_string(Ordinal, OS),
    string_to_dlist(OS, COS, [CHAR_RIGHT_PAREN]) :
      Tail = [CHAR_LEFT_PAREN | COS] |
	list_to_string(CCN, FormattedName);

    Format =\= full, Format =\= creator,
    Name = CreatedName(Ordinal),
    string(CreatedName),
    number(Ordinal) :
      FormattedName = CreatedName;

    otherwise :
      Format = _,
      FormattedName = Name.


show_channel_argument(Name, SpiChannel, Which, Depth, Format, Display,
				Left, Right) :-

    Which = none :
      SpiChannel = _,
      Depth = _,
      Format = _,
      Display = FormattedName?,
      Left = Right |
	format_channel_name;

    Which =\= none,
    read_vector(SPI_CHANNEL_REFS, SpiChannel, Refs),
    Refs > 0 :
      Name = _ |
	inspect_channel(SpiChannel, Result),
	channel_argument + (Status = Result);

    otherwise :
      SpiChannel = _,
      Which = _,
      Depth = _,
      Format = _,
      Display = [FormattedName?],
      Left = Right |
	format_channel_name.

channel_argument(Status, Which, Depth, Format, Display, Left, Right) :-

    Depth =\= 0,
    Status =?= _Type(CreatorBase, [], []):
      Which = _,
      Depth = _,
      Format = _,
      Display = Creator,
      Left = Right |
	channel_creator;

    Depth =\= 0,
    Status =?= _Type(CreatorBase, Sends, Receives),
    {Sends, Receives} =\= {[], []},
    Which =?= none :
      Format = _,
      Display = Creator,
      Left = Right |
	channel_creator;

    Depth =\= 0,
    Status =?= _Type(CreatorBase, Sends, []),
    Sends =\= [],
    Which =?= note :
      Format = _,
      Display = (Creator? !),
      Left = Right |
	channel_creator;

    Depth =\= 0,
    Status =?= _Type(CreatorBase, [], Receives),
    Receives =\= [],
    Which =?= note :
      Format = _,
      QM = "?",
      Display = QM(Creator?),
      Left = Right |
	channel_creator;

    Depth =\= 0,
    Status =?= _Type(CreatorBase, Sends, Receives),
    Sends =\= [],
    Receives =\= [],
    Which =\= none,
    Which =\= messages :
      Format = _,
      QM = "?",
      Display = QM(Creator? !),
      Left = Right |
	channel_creator;

    Depth =\= 0,
    Status =?= _Type(CreatorBase, Sends, Receives),
    Which =?= messages,
    Sends =\= [] :
      Display = (Creator? = Content) |
	channel_creator,
	utils#list_to_tuple([send | Sends], ST),
	utils#tuple_to_dlist(ST, Stream, [receive | Receives]),
	show_channel_content;

    Depth =\= 0,
    Status =?= _Type(CreatorBase, Sends, Receives),
    Which =?= messages,
    Receives =\= [] :
      Display = (Creator? = Content) |
	channel_creator,
	utils#list_to_tuple([send | Sends], ST),
	utils#tuple_to_dlist(ST, Stream, [receive | Receives]),
	show_channel_content;

    otherwise,
    Status =?= _Type(CreatorBase, _Sends, _Receives) :
      Depth = _,
      Status = _,
      Which = _,
      Format = _,
      Display = Creator,
      Left = Right |
	channel_creator.

  channel_creator(CreatorBase, Format, Creator) :-

    CreatorBase =?= namerate(ProcessName, Base),
    Format =?= short |
	short_creator + (Suffix = []);

    CreatorBase =?= namerate(ProcessName, Base),
    Format =?= base |
	base_suffix,
	short_creator;

    CreatorBase =?= namerate(ProcessName, _),
    Format = creator |
	full_channel_creator + (Suffix = []);

    CreatorBase =?= namerate(ProcessName, Base),
    Format =?= full |
	base_suffix,
	full_channel_creator.

  short_creator(ProcessName, Base, Suffix, Creator) :-

    ProcessName =\= other(_),
    Base =?= infinite |
	short_channel_creator;

    ProcessName =\= other(_),
    Base =?= unknown |
	short_channel_creator;

    ProcessName =\= other(_),
    Base =?= sink |
	short_channel_creator;

    ProcessName =\= other(_),
    Base =\= infinite, Base =\= unknown, Base =\= sink, Base =\= anchor |
	short_channel_creator;

    otherwise :
      Suffix = _,
      Creator = (ProcessName ? Base).

  base_suffix(Base, Suffix) :-

    Base =?= infinite :
      Suffix = [];

    Base = unknown :
	Suffix = [CHAR_ASTERISK];

    Base = sink :
	Suffix = [CHAR_GREATER];

    real(Base),
    convert_to_integer(Base, I),
    convert_to_real(I, R),
    Base =?= R,
    convert_to_string(I, RS),
    string_to_dlist(RS, RL, []) |
	Suffix = [CHAR_ASTERISK | RL];

    real(Base),
    convert_to_integer(Base, I),
    convert_to_real(I, R),
    Base =\= R,
    convert_to_string(Base, RS),
    string_to_dlist(RS, RL, []) |
	Suffix = [CHAR_ASTERISK | RL];

    otherwise :
      Base = _,
      Suffix = [CHAR_SPACE, CHAR_QUERY].

  short_channel_creator(ProcessName, Creator, Suffix) :-

    string(ProcessName),
    string_to_dlist(ProcessName, PNL, Suffix) |
	list_to_string(PNL, Creator);

    ProcessName =?= LocalName(N),
    string(LocalName),
    number(N),
    string_to_dlist(LocalName, LNL, Suffix) |
	list_to_string(LNL, Creator);

    ProcessName =?= Functor(GlobalName),
    string(Functor),
    string(GlobalName),
    string_to_dlist(GlobalName, GNL, [CHAR_RIGHT_PAREN | Suffix]),
    string_to_dlist(Functor, FL, [CHAR_LEFT_PAREN | GNL]) |
	list_to_string(FL, Creator);

    ProcessName =?= Functor(LocalName(N)),
    string(Functor),
    string(LocalName),
    number(N),
    string_to_dlist(LocalName, LNL, [CHAR_RIGHT_PAREN | Suffix]),
    string_to_dlist(Functor, FL, [CHAR_LEFT_PAREN | LNL]) |
	list_to_string(FL, Creator);

    otherwise :
      Creator = (ProcessName ? CS?) |
	list_to_string(Suffix, CS).

  full_channel_creator(ProcessName, Creator, Suffix) :-

    string(ProcessName),
    string_to_dlist(ProcessName, PNL, Suffix) |
	list_to_string(PNL, Creator);

    ProcessName =?= Functor(Argument),
    string(Functor),
    constant(Argument),
    Argument @< [],
    convert_to_string(Argument, As),
    string_to_dlist(As, AL, [CHAR_RIGHT_PAREN | Suffix]),
    string_to_dlist(Functor, FL, [CHAR_LEFT_PAREN | AL] ) |
	list_to_string(FL, Creator);

    ProcessName =?= Functor(LocalName(Argument)),
    string(Functor),
    string(LocalName),
    constant(Argument),
    Argument @< [],
    convert_to_string(Argument, As),
    string_to_dlist(As, AL, [CHAR_RIGHT_PAREN, CHAR_RIGHT_PAREN | Suffix]),
    string_to_dlist(LocalName,LNL, [CHAR_LEFT_PAREN | AL]),
    string_to_dlist(Functor, FL, [CHAR_LEFT_PAREN | LNL]) |
	list_to_string(FL, Creator);

    otherwise :
      Creator = (ProcessName ? CS?) |
	list_to_string(Suffix, CS).


show_compound(Argument, Which, Depth, Format, Display, Left, Right) :-

    tuple(Argument),
    Depth-- > 0,
    arity(Argument, Index),
    make_tuple(Index, Args) :
      Tuple = Argument,
      Display = Args |
	show_tuple_args;

    tuple(Argument),
    Depth =< 0 :
      Which = _,
      Format = _,
      Display = "...",
      Left = Right;

    Argument ? A :
      Display ! D |
	show_argument + (Argument = A, Display = D),
	show_cdr;

   otherwise :
      Which = _,
      Depth = _,
      Format = _,
      Display = Argument,
      Left = Right.


show_goal(Goal, Options, Output) :-

    Options =?= fcp :
      Output = Goal;

    Options =\= fcp |
	show_goal(Goal, Options, SpiFcp, Output, SpiFcp).

show_goal(Goal, Options, SpiFcp, Left, Right) :-

    string(Goal) :
      Options = _,
      SpiFcp = Goal,
      Left = Right;

    tuple(Goal), Goal =\= (_#_),
    arg(1, Goal, Name), string(Name),
    arity(Goal, Index) :
      make_channel(BadOption, _) |
	parse_options(Options, Depth(1), BadOption(BadOption),
		      Which(note), Format(short)),
	show_goal2;

    Goal =?= (Service#Goal') :
      SpiFcp = (Service#SpiFcp') |
	self;

    otherwise :
      Goal = _,
      Options = _,
      SpiFcp = non_goal(Goal),
      Left = Right.

show_goal1(Goal, Which, Depth, Format, SpiFcp, Left, Right) :-

    string(Goal) :
      Which = _,
      Depth = _,
      Format = _,
      SpiFcp = Goal,
      Left = Right;

    tuple(Goal), Goal =\= (_#_),
    Goal =\= clause(_, _), Goal =\= clause(_, _, _, _),
    arg(1, Goal, Name), string(Name),
    arity(Goal, Index) |
	show_goal2;

    Goal =?= clause(Goal', _Body) |
	self;

    Goal =?= clause(Goal', _Body, _Id, _Time) |
	self;

    Goal =?= (Service#Goal') :
      SpiFcp = (Service#SpiFcp') |
	self;

    otherwise :
      Goal = _,
      Which = _,
      Depth = _,
      Format = _,
      SpiFcp = non_goal(Goal),
      Left = Right.

  show_goal2(Goal, Index, Which, Depth, Format,
		SpiFcp, Left, Right, Name) :-

    nth_char(1, Name, Char1),
    CHAR_A =< Char1, Char1 =< CHAR_Z |
	goal_channels;

    nth_char(1, Name, CHAR_DOT),
    nth_char(2, Name, Char2),
    CHAR_A =< Char2, Char2 =< CHAR_Z |
	goal_channels;

    nth_char(1, Name, Char1),
    CHAR_a =< Char1, Char1 =< CHAR_z,
    X := string_length(Name) |
	show_goal3;

    otherwise,
    make_tuple(Index, Tuple),
    arg(1, Tuple, N) :
      SpiFcp = Tuple,
      N = Name |
	goal_channels2 + (Left = Completed, Right = Tuple),
	complete_goal.

  complete_goal(Left, Right, Completed) :-
    known(Completed) :
      Left = Right.

  % Blocked code may begin with a lower-case directory name.
  % Check the first character after '$'.

  show_goal3(Goal, Index, Which, Depth, Format,
		SpiFcp, Left, Right, Name, X):-

    X--,
    nth_char(X', Name, CHAR_DOLLAR),
    nth_char(X, Name, Char2),
    CHAR_A =< Char2, Char2 =< CHAR_Z |
	goal_channels;

    X--,
    nth_char(X', Name, CHAR_DOLLAR),
    nth_char(X, Name, CHAR_DOT) |
	goal_channels;

    X--,
    nth_char(X', Name, C),
    C =\= CHAR_DOLLAR |
	self;

    otherwise,
    make_tuple(Index, Tuple),
    arg(1, Tuple, N) :
      X = _,
      N = Name,
      SpiFcp = Tuple |
	goal_channels2.


goal_channels(Goal, Index, Which, Depth, Format,
		SpiFcp, Left, Right, Name) :-

    string_length(Name, L),
    nth_char(L, Name, CHAR_PRIME) :
      Leaders = [Name | LeadingArgs?] |
	leading_arguments(2, Goal, LeadingArgs, TrailingChannels),
	trailing_goal_channels(Index, Goal, TrailingChannels, []),
	utils#list_to_tuple(Leaders, Goal'),
	ambient_goal;

    otherwise :
      Name = _ |
	goal_channels1.

  leading_arguments(Index, Goal, LeadingArgs, TrailingChannels) :-

    Index > arity(Goal) :
      TrailingChannels = _,
      LeadingArgs = [];

    Index =< arity(Goal),
    arg(Index, Goal, Arg),
    unknown(Arg),
    Index++ :
      LeadingArgs ! Arg |
	self;

    Index =< arity(Goal),
    arg(Index, Goal, Arg),
    vector(Arg),
    arity(Arg, 2 /* should be AMBIENT_ARITY != CHANNEL_SIZE */) :
      LeadingArgs = TrailingChannels;

    Index =< arity(Goal),
    arg(Index, Goal, Arg),
    Index++,
    otherwise :
      LeadingArgs ! Arg |
	self.

  trailing_goal_channels(Index, Goal, TrailingChannels, AddedChannels) :-

    arg(Index, Goal, Channel),
    vector(Channel),
    arity(Channel, CHANNEL_SIZE),
    Index-- :
      AddedChannels' = [Channel | AddedChannels] |
	self;

    arg(Index, Goal, Other),
    unknown(Other) :
      TrailingChannels = AddedChannels;

    otherwise :
      Goal = _,
      Index = _,
      TrailingChannels = AddedChannels.


  ambient_goal(Goal, Which, Depth, Format,
		SpiFcp, Left, Right) :-
    arg(1, Goal, Name),
    arity(Goal, Index),
    make_tuple(Index, Tuple),
    arg(1, Tuple, N) :
      N = Name,
      SpiFcp = Tuple |
	goal_channels2.

goal_channels1(Goal, Index, Which, Depth, Format,
		SpiFcp, Left, Right) :-

/* Exclude trailing non-pi arguments (mostly) */

    Index > 2, arg(Index, Goal, Argument),
    we(Argument),
    Index-- |
	self;

    Index > 2, arg(Index, Goal, Argument),
    ro(Argument),
    Index-- |
	self;

    Index > 2, arg(Index, Goal, Argument),
    vector(Argument), arity(Argument, CHANNEL_SIZE),
    make_tuple(Index, Tuple),
    arg(1, Goal, Name),
    arg(1, Tuple, N) :
      N = Name,
      SpiFcp = Tuple |
	goal_channels2;

    Index > 2, arg(Index, Goal, Argument),
    vector(Argument), arity(Argument) =< 2,
    Index--,
    make_tuple(Index', Tuple),
    arg(1, Goal, Name),
    arg(1, Tuple, N) :
      N = Name,
      SpiFcp = Tuple |
	goal_channels2;

    Index-- > 2,
    otherwise |
	self;

    Index =< 2,
    arg(1, Goal, Name) :
      Which = _,
      Depth = _,
      Format = _,
      SpiFcp = Name,
      Left = Right.

  goal_channels2(Goal, Index, Which, Depth, Format,
			SpiFcp, Left, Right) :-

    Index > 1,
    arg(Index, Goal, Argument),
    arg(Index, SpiFcp, Display),
    Index-- |
	show_argument + (Left = Left, Right = Left'),
	self;

    Index = 1 :
      Goal = _,
      Which = _,
      Depth = _,
      Format = _,
      SpiFcp = _,
      Left = Right.


close_tree(Tree) :-

    Tree ? tree(_, Context, BranchList) :
      close_channel(Context) |
	close_tree(BranchList),
	close_tree;

    Tree ? reduce(_, _, Time, _),
    unknown(Time) |
	close_tree;

    Tree ? reduce(_, _, Time, BranchList),
    number(Time) |
	close_tree(BranchList),
	close_tree;

    Tree ? _,
    otherwise |
	close_tree;

    Tree = [] |
	true;

    Tree =\= [_|_], Tree =\= [] :
      Tree' = [Tree] |
	close_tree.


show_tree(Tree, Options, TreeTrace) :-

	parse_options(Options, Depth(1), Order(prefix),
		      Which(note), Format(short)),
	nodes([Tree], [Depth, Which, Format], 0, 0, Head, []),
	display_tree(Head, Order, TreeTrace).

nodes(Nodes, Options, Level, Time, Head, Tail) :-

    Nodes ? tree(TreeId, Context, BranchList) :
      Level = _,
      Head ! begin(PiTreeId?, Context, _Link) |
	show_treeid,
	nodes(BranchList, Options, 0, Time,
		Head', [end(PiTreeId?) | Head'']),
	nodes;

    Nodes ? Goal, Goal = _#_ |
      show_remote_goal;
/*
    Nodes ? reduce(Goal, _Id, Time', BranchList),
    Level++ :
      Head ! Executed |
	nodes(Nodes', Options, Level, Time, Head', Head''),
	show_goal(Goal, Options, PiGoal),
	executed(PiGoal, Level, Time', Executed, BranchList, Nodes''),
	nodes;
*/
    Nodes ? reduce(Goal, _Id, GoalTime, BranchList) |
	show_reduced_goal;

    Nodes = [] :
      Options = _,
      Level = _,
      Time = _,
      Head = Tail .

  show_reduced_goal(Nodes, Options, Level, Time, Head, Tail, Goal, GoalTime,
			BranchList) :-

    EXCLUDE_CLAUSE_TESTS(Goal, (GoalTime = _, BranchList = _, nodes));

    otherwise,
    Level++ :
      Head ! Executed |
	nodes(Nodes, Options, Level, Time, Head', Head''),
	show_goal(Goal, Options, PiGoal),
	executed(PiGoal, Level, GoalTime, Executed, BranchList, Nodes'),
	nodes.

  show_remote_goal(Nodes, Options, Level, Time, Head, Tail, Goal) :-

    EXCLUDE_CLAUSE_TESTS(Goal, nodes);

    otherwise :
      Head ! {'#', PiGoal?, Level, Time} |
	show_goal(Goal, Options, PiGoal),
	nodes.

  show_treeid(TreeId, PiTreeId) :-

    TreeId = (ServiceId#TreeId') :
      PiTreeId = (ServiceId#PiTreeId') |
	self;

    TreeId ? Goal :
      PiTreeId ! SpiFcp? |
	show_goal(Goal, [0], SpiFcp),
	self;

    TreeId = [] :
      PiTreeId = [];

    otherwise |
	show_goal(TreeId, [0], PiTreeId).

executed(PiGoal, Level, Time, Executed, BranchList, Nodes) :-

    Time = failed(_, FailTime) : BranchList = _,
      Executed = {'-', PiGoal, Level, FailTime},
      Nodes = [] ;

    Time = unknown(_, UnknownTime) : BranchList = _,
      Executed = {'*', PiGoal, Level, UnknownTime},
      Nodes = [] ;

    number(Time) :
      Executed = {'|', PiGoal, Level, Time} |
	reduced_goal_branchlist;

    unknown(Time) : BranchList = _,
      Executed = {'?', PiGoal, Level, 10000000000000000.0},
      Nodes = [] .

  reduced_goal_branchlist(PiGoal, Nodes, BranchList) :-

    MACRO_CLAUSE_TESTS(PiGoal, (BranchList = _, Nodes = []));

    otherwise :
      PiGoal = _,
      Nodes = BranchList.


display_tree(Head, Order, TreeTrace) :-

    Order =?= prefix |
	display_prefix_order(Head, 0, "", TreeTrace);

    Order =?= execute |
	sort_on_time(Head, Sorted),
	display_execute_order.

  display_execute_order(Sorted, TreeTrace) :-

    Sorted ? {Operator, Goal, _, _}, Operator =\= "|",
    known(Goal) :
      TreeTrace ! {Operator, "", Goal} |
	self;

    Sorted ? {"|", Goal, _, _},
    known(Goal) :
      TreeTrace ! Goal |
	self;

    Sorted =?= [] :
      TreeTrace = [].


display_prefix_order(Head, Level, Indent, TreeTrace) :-

    Head ? begin(PiTreeId, _Context, _Link),
    known(PiTreeId) :
      Level = _,
      Indent = _,
      Level' = 0,
      Indent' = "",
      TreeTrace ! (begin : PiTreeId) |
	self;

    Head ? end(PiTreeId),
    known(PiTreeId) :
      TreeTrace ! (end : PiTreeId) |
	self;

    Head ? Operator(Goal, Level', _Time),
    known(Goal) :
      TreeTrace ! {Operator, Indent', Goal} |
	update_indent(Level, Level', Indent, Indent'),
	self;

    Head =?= [] :
      Level = _,
      Indent = _,
      TreeTrace = [].

  update_indent(Level1, Level2, Indent1, Indent2) :-

    Level2 > 20,
    Level2' := Level2 \ 20 |
	self;

    Level1 < Level2,
    Level2 =< 20,
    Level1++,
    string_to_dlist(Indent1, IL, []),
    list_to_string([32 | IL], Indent1') |
	self;

    Level1 > Level2,
    Level1--,
    string_to_dlist(Indent1, IL, []),
    IL ? _,
    list_to_string(IL', Indent1') |
	self;

    Level1 > Level2,
    Indent1 = " " :
      Indent1' = "",
      Level1' = 0 |
	self;

    Level1 > Level2,
    Indent1 = "" :
      Level1' = 0 |
	self;

    Level1 =?= Level2 :
      Indent2 = Indent1.


sort_on_time(List, Sorted) + (Tail =[]) :-

    List ? Reduce, Reduce = Functor(_, _, Time), Functor =\= begin |
	partition(Time, List', Small, Large),
	sort_on_time(Small?, Sorted, [Reduce | LSorted]),
	sort_on_time(Large?, LSorted, Tail);

    List ? _,
    otherwise |
	self;

    List = [] :
      Sorted = Tail.

  partition(X, List, Small, Large) :-

    List ? begin(_, _, _) |
	self;

    List ? end(_) |
	self;

    List ? Reduce, Reduce =?= Operator(_Goal, _Level, Y), Operator =\= begin,
    X > Y :
      Small ! Reduce |
	partition;

    List ? Reduce, Reduce =?= Operator(_Goal, _Level, Y), Operator =\= begin,
    X =< Y :
      Large ! Reduce |
	partition;

    List = [] :
      X = _,
      Small = [],
      Large = [].


show_goals(Goals, Options, Output) :-

    Options =?= fcp :
      Options = _,
      Options' = [] |
	self;

    Options =\= fcp :
      make_channel(BadOption, _) |
	parse_options(Options, Depth(1), BadOption(BadOption),
		      Which(note), Format(short)),
	show_goals(Goals, Which?, Depth?, Format?,
			Result, Output, Result).

  show_goals(Goals, Which, Depth, Format, Result, Left, Right) :-

    Goals =?= (Goal, Goals') :
      Result = (SpiFcp?, Result') |
	remote_goal([], Goal, Goal', SpiFcp, SpiFcp'),
	show_goal1 + (Left = Left, Right = Left'),
	self;

    Goals ? Goal :
      Result = (SpiFcp?, Result') |
	remote_goal([], Goal, Goal', SpiFcp, SpiFcp'),
	show_goal1 + (Left = Left, Right = Left'),
	self;

    Goals =?= [] :
      Which = _,
      Depth = _,
      Format = _,
      Result = [],
      Left = Right;

    Goals =\= (_, _), Goals =\= [_ | _], Goals =\= [] |
	remote_goal([], Goals, Goal, Result, SpiFcp),
	show_goal1.


show_resolvent(Resolvent, Options, Stream) :-

    Options =?= fcp :
      Options' = [] |
	self;

    Options =\= fcp :
      make_channel(BadOption, _) |
	parse_options(Options, Depth(1), BadOption(BadOption),
		      Which(note), Format(short)),
	collect_resolvent_list(Resolvent, List),
	show_goal_list(List?, Depth?, Which?, Format?, Stream).

show_goal_list(List, Depth, Which, Format, Stream) :-

/* These three clauses have been added to prettify resolvent of vtree. */
    List ? _Name([_|_]) |
	self;

    List ? _Name([]) |
	self;

    List ? Name(Goal),
    Name =?= vanilla,
    arg(1, Goal, Functor), nth_char(1, Functor, C),
    CHAR_a =< C, C =< CHAR_z |
	self;
/***********************************************************************/

    List ? Name(Goal),
    otherwise |
	remote_goal(Name, Goal, Goal', SpiFcp, SpiFcp'),
	output_goal(SpiFcp, Stream, Stream', Right),
	show_goal1 + (Left = done),
	self;

    List =?= [] :
      Which = _,
      Depth = _,
      Format = _,
      Stream = [].

  output_goal(SpiFcp, [SpiFcp | Next]^, Next, done).

collect_resolvent_list(Resolvent, List) :-

    Resolvent ? Call,
    Call = call(_, _) :
      List ! Call |   
	self;

    Resolvent ? [Name | _] # Goals |
	collect_resolvent_goals(Name, Goals, List, List'),
	self;

    Resolvent ? Name # Goals,
    Name =\= [_|_] |
	collect_resolvent_goals(Name, Goals, List, List'),
	self;

    Resolvent ? Goals,
    Goals =\= call(_, _),
    Goals =\= (_#_) |
	collect_resolvent_goals([], Goals, List, List'),
	self;

    Resolvent = [] :
      List = [].

  collect_resolvent_goals(Name, Goals, List, NextList) :-

    Goals ? Goal :
      List ! Name(Goal) |
	self;

    Goals =\= [_ | _],
    Goals =\= [] :
      Goals' = [Goals] |
	self;

    Goals =?= [] :
      Name = _,
      List = NextList.


remote_goal(Name, Goal, OutGoal, SpiFcp, OutSpiFcp) :-

    Name =\= [] :
      Name' = [],
      SpiFcp = Name#SpiFcp'? |
	self;

    Name =?= [],
    Goal =?= Target#Goal' :
      SpiFcp = Target#SpiFcp'? |
	self;

    Name =?= [],
    Goal =\= _#_ :
      OutGoal = Goal,
      OutSpiFcp = SpiFcp.


weighter(Weighter) :-

    string(Weighter) |
	spi_monitor # scheduler(S),
	write_channel(default_weighter(Weighter), S);

    tuple(Weighter) |
	utils#tuple_to_dlist(Weighter, [Name | Parameters], []),
	validate_parameters(Parameters, List, Invalid),
	update_weighter;

    otherwise |
	computation#display(
		"spi_utils: Weighter must be a string or a tuple" - Weighter).

  update_weighter(Name, List, Invalid) :-

    string(Name), Invalid =?= [] |
	utils#list_to_tuple([Name, _ | List], Weighter),
	spi_monitor # scheduler(S),
	write_channel(default_weighter(Weighter), S);

    otherwise :
      List = _ |
	utils#list_to_tuple([Name | Invalid], Weighter),
	computation#display("spi_utils: Bad Weighter elements" - Weighter).


validate_parameters(Parameters, List, Invalid) + (Bad = Tail?, Tail) :-

    Parameters ? P, number(P) :
      List ! P |
	self;

    Parameters ? P, otherwise :
      Tail ! P |
	self;

    Parameters = [] :
      Tail = [],
      List = [],
      Invalid = Bad.

