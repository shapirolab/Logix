-export([string/1]).
-mode(trust).
-language(compound).

procedure string(String).

string(Text) :- true : Text =

"library.

-language(compound).

pi_channel(Channel) :-
	pi_channel(Channel, 'SYSTEM', 0, 0, _VC).
pi_channel(Channel, Creator) :-
	pi_channel(Channel, Creator, 0, 0, _VC).
pi_channel(Channel, ReceiveMean, SendMean) :-
	pi_channel(Channel, 'SYSTEM', ReceiveMean, SendMean, _VC).
pi_channel(Channel, Creator, ReceiveMean, SendMean) :-
	pi_channel(Channel, Creator, ReceiveMean, SendMean, _VC).
pi_channel(Channel, Creator, ReceiveMean, SendMean, VC) :-
    we(Channel) :
      make_vector(2, VC, Streams),
      Streams = {MsC, _},
      store_vector(2, MsC, VC),
      Channel = Creator(VC, {ReceiveMean, SendMean});
    otherwise :
      Creator = _,
      VC = _,
      ReceiveMean = _,
      SendMean = _ |
	computation#display(('pi_library: Can''t make channel' : Channel)).

pi_send(Message, Channel) :-
    Channel = _Creator(VC, _Args),
    vector(VC), arity(VC) > 1 :
      Ms = Sender?(Message, 1, _),
      write_vector(1, Ms, VC) |
	pi_monitor#unique_sender('PI_SEND.send', Sender);
    otherwise :
      Message = _ |
	computation#display(('pi_library: Can''t send to' : Channel)).

pi_send(Sender, Message, Channel) :-
    Channel = _Creator(VC, _Args),
    vector(VC), arity(VC) > 1 :
      Ms = Sender(Message, 1, _),
      write_vector(1, Ms, VC);
    otherwise :
      Sender = _,
      Message = _ |
	computation#display(('pi_library: Can''t send to' : Channel)).

pi_receive(Channel, Message) :-
    Channel = _Creator(VC, _Args),
    read_vector(2, VC, Stream) |
	pi_receive(Channel, Message, Stream).
pi_receive(Channel, Message, Stream) :-
    Stream ? _Sender(_Message, _N, Choice),
    not_we(Choice) |
	self;
    Stream ? _Sender(M, N, Choice),
    we(Choice),
    Channel = _Creator(VC, _Args) :
      store_vector(2, Stream', VC),
      Choice = N,
      Message = M;
    otherwise :
      Message = _,
      Stream = _ |
	computation#display(('pi_library: Can''t receive from' : Channel)).

pi_wait_to_send(FcpVector, PiMessage) :-
    vector(FcpVector),
    PiMessage = _Sender(_Message, _ChoiceTag, Choice),
    unknown(Choice) :
      write_vector(1, PiMessage, FcpVector);
    PiMessage = _Sender(_Message, _ChoiceTag, Choice),
    known(Choice) :
      FcpVector = _.

pi_wait_to_receive(FcpVector, Chosen, Stream) :-
    vector(FcpVector),
    unknown(Chosen),
    read_vector(2, FcpVector, SubStream) :
      Stream = SubStream;
    known(Chosen) :
      FcpVector = _,
      Stream = _.
"

	| true.
