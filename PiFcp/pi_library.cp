-export([string/1]).
-mode(trust).
-language(compound).

procedure string(String).

string(Text) :- true : Text =

"library.

-language(compound).

pi_channel(Channel)+(Creator = 'SYSTEM', VC = _, MsC = _) :-
    we(Channel) :
      make_channel(VC, MsC),
      Channel = Creator(VC, MsC);
    otherwise :
      Creator = _,
      VC = _,
      MsC = _ |
	screen#display(('pi_utils: Can''t make channel' : Channel)).

pi_send(Message, Channel) :-
    Channel = _Creator(C, _Stream),
    channel(C) :
      Ms = Sender?(Message, 1, _),
      write_channel(Ms, C) |
	pi_monitor#unique_sender('PI_SEND.send', Sender);
    otherwise :
      Message = _ |
	computation#display(('pi_utils: Can''t send to' : Channel)).

pi_send(Sender, Message, Channel) :-
    Channel = _Creator(C, _Stream),
    channel(C) :
      Ms = Sender(Message, 1, _),
      write_channel(Ms, C);
    otherwise :
      Sender = _,
      Message = _ |
	computation#display(('pi_utils: Can''t send to' : Channel)).

pi_receive(Channel, Message) :-
    Channel = Creator(C, Stream),
    Stream ? _Sender(_Message, _N, Choice),
    not_we(Choice) :
      Channel' = Creator(C, Stream') |
	self;
    Channel = _Creator(_C, Stream),
    Stream ? _Sender(M, N, Choice),
    we(Choice) :
      Stream' = _,
      Choice = N,
      Message = M;
    otherwise :
      Message = _ |
	computation#display(('pi_utils: Can''t receive from' : Channel)).


"

	| true.
   
