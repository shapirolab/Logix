-export([string/1]).
-mode(trust).
-language(compound).

procedure string(String).

string(Text) :- true : Text =

"library.
-language([evaluate,compound,colon]).
-include(spi_constants).

spi_channel(Channel) :-
	spi_channel(Channel, 'SYSTEM', infinite).
spi_channel(Channel, Creator) :-
	spi_channel(Channel, Creator, infinite).
spi_channel(Channel, Creator, BaseRate) :-
	spi_channel(Channel, Creator, BaseRate).
spi_channel(Channel, Creator, BaseRate) :-
    number(BaseRate) :
      Channel = Channel'? |
	spi_monitor#new_channel(Creator, Channel', BaseRate);
    BaseRate =?= infinite :
      Channel = Channel'? |
	spi_monitor#new_channel(Creator, Channel', BaseRate);
    otherwise :
      Creator = _,
      BaseRate = _ |
	computation#display(('spi_library: Can''t make channel' : Channel)).

spi_channel(Channel, Creator, BaseRate, ComputeWeight) :-
    string(ComputeWeight) :
      Channel = Channel'? |
	spi_monitor#new_channel(Creator, Channel', ComputeWeight, BaseRate);
    tuple(ComputeWeight) :
      Channel = Channel'? |
	spi_utils#make_channel(Creator, Channel', ComputeWeight, BaseRate);
    otherwise :
      Creator = _,
      BaseRate = _,
      ComputeWeight = _ |
	computation#display(('spi_library: Can''t make channel' : Channel)).

spi_send(Message, Channel) :-
	spi_send(Message, Channel, 1, sender).
spi_send(Message, Channel, Multiplier) :-
	spi_send(Message, Channel, Multiplier, sender).
spi_send(Message, Channel, Multiplier, Id) :-
    vector(Channel) :
      Send = SPI_SEND(Id, Channel, Multiplier, 1) |
	spi_monitor#scheduler(S),
	write_channel(start(spi_send, [Send], Value, Chosen), S),
	spi_transmitted(sending(Id), 1, Chosen, Message, Value);
    otherwise :
      Id = _,
      Message = _,
      Multiplier = _ |
	computation#display(('spi_library: Can''t send to' : Channel)).

spi_receive(Channel, Message) :-
	spi_receive(Channel, Message, 1, receiver).
spi_receive(Channel, Message, Multiplier) :-
	spi_receive(Channel, Message, Multiplier, receiver).
spi_receive(Channel, Message, Multiplier, Id) :-
    vector(Channel) :
      Receive = SPI_RECEIVE(Id, Channel, Multiplier, 2) |
	spi_monitor#scheduler(S),
	write_channel(start(spi_receive, [Receive], Value, Chosen), S),
	spi_transmitted(receiving(Id), 2, Chosen, Message, Value?);
    otherwise :
      Id = _,
      Message = _,
      Multiplier = _ |
	computation#display(('spi_library: Can''t receive from' : Channel)).

spi_close(Channel) :-
    vector(Channel) |
	spi_monitor#scheduler(S),
	write_channel(close({Channel}), S?);
    otherwise |
	computation#display(('spi_library: Can''t close' : Channel)).

spi_transmitted(Id, Tag, Chosen, Message, Value) :-

    Chosen = Tag :
      Id = _,
      Value = Message;

    Chosen =\= Tag :
      Message = _,
      Id = _,
      Value = _.

spi_update_channel_refs(List, S1, S2) :-

    List ? Add,
    Add =?= Increment(Channel),
    integer(Increment),
    vector(Channel),
    arity(Channel, CHANNEL_SIZE),
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Refs += Increment :
      store_vector(SPI_CHANNEL_REFS, Refs', Channel) |
	self;

    List ? Close,
    Close = close(Channels),
    tuple(Channels) :
      List' = _,
      write_channel(Close, S1, S2);

    List =?= [] :
      S2 = S1.
"

	| true.
