-export([string/1]).
-mode(trust).
-language(compound).

procedure string(String).

string(Text) :- true : Text =

"library.

-language(compound).

pi_channel(Channel) :-
	pi_channel(Channel, 'SYSTEM', _VC, _MsC, 0, 0).
pi_channel(Channel, ReceiveMean, SendMean) :-
	pi_channel(Channel, 'SYSTEM', _VC, _MsC, ReceiveMean, SendMean).
pi_channel(Channel, Creator, VC, MsC) :-
	pi_channel(Channel, Creator, VC, MsC, 0, 0).
pi_channel(Channel, Creator, VC, MsC, ReceiveMean, SendMean) :-
    we(Channel) :
      make_channel(VC, MsC),
      Channel = Creator(VC, MsC, ReceiveMean, SendMean);
    otherwise :
      Creator = _,
      VC = _,
      MsC = _,
      ReceiveMean = _,
      SendMean = _ |
	computation#display(('pi_utils: Can''t make channel' : Channel)).

pi_send(Message, Channel) :-
    Channel = _Creator(C, _Stream, _, _),
    channel(C) :
      Ms = Sender?(Message, 1, _),
      write_channel(Ms, C) |
	pi_monitor#unique_sender('PI_SEND.send', Sender);
    otherwise :
      Message = _ |
	computation#display(('fubar: Can''t send to' : Channel)).

pi_send(Sender, Message, Channel) :-
    Channel = _Creator(C, _Stream, _, _),
    channel(C) :
      Ms = Sender(Message, 1, _),
      write_channel(Ms, C);
    otherwise :
      Sender = _,
      Message = _ |
	computation#display(('pi_utils: Can''t send to' : Channel)).

pi_receive(Channel, Message) :-
    Channel = Creator(C, Stream, ReceiveMean, SendMean),
    Stream ? _Sender(_Message, _N, Choice),
    not_we(Choice) :
      Channel' = Creator(C, Stream', ReceiveMean, SendMean) |
	self;
    Channel = _Creator(_C, Stream, _, _),
    Stream ? _Sender(M, N, Choice),
    we(Choice) :
      Stream' = _,
      Choice = N,
      Message = M;
    otherwise :
      Message = _ |
	computation#display(('pi_utils: Can''t receive from' : Channel)).

pi_wait_to_send(Ready, PiMessage, PiChannel, Sent) :-
    PiMessage = _Sender(_Message, _ChoiceTag, Choice),
    unknown(Choice),
    known(Ready),
    PiChannel =?= _Creator(FcpChannel, _Stream, _Receive, _Send),
    channel(FcpChannel) :
      write_channel(PiMessage, FcpChannel),
      Sent = Ready;
    PiMessage = _Sender(_Message, _ChoiceTag, Choice),
    known(Choice) :
      Ready = _,
      PiChannel = _,
      Sent = _.

"

	| true.
   
