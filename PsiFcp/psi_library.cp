-export([string/1]).
-mode(trust).
-language(compound).

procedure string(String).

string(Text) :- true : Text =

"library.

-language(compound).

psi_channel(Channel) :-
	psi_channel(Channel, 'SYSTEM', infinite, _FcpChannel).
psi_channel(Channel, Creator) :-
	psi_channel(Channel, Creator, infinite, _FcpChannel).
psi_channel(Channel, Creator, BaseRate) :-
	psi_channel(Channel, Creator, BaseRate, _FcpChannel).
psi_channel(Channel, Creator, BaseRate, FcpChannel) :-
    true :
      Channel = Channel'?,
      FcpChannel = FcpChannel'? |
	psi_monitor#new_channel(Creator, Channel', BaseRate),
	Channel'? = _Creator,(FcpChannel', _Circuit);
    otherwise :
      Creator = _,
      FcpChannel = _,
      BaseRate = _ |
	computation#display(('psi_library: Can''t make channel' : Channel)).

psi_send(Message, Channel) :-
    arg(1, Channel, Creator) |
	psi_send(psi(Creator), Channel, Message, 1).
psi_send(Id, Message, Channel) :-
	psi_send(Id, Message, Channel, 1).
psi_send(Id, Message, Channel, Multiplier) :-
    Channel = _Creator(FcpChannel, _Circuit),
    channel(FcpChannel) :
      Send = send(Id, Message, 1, Multiplier, Chosen),
      write_channel(Send, FcpChannel) |
	psi_sent(1, Chosen);
    otherwise :
      Id = _,
      Message = _,
      Multiplier = _ |
	computation#display(('psi_library: Can''t send to' : Channel)).

psi_receive(Channel, Message) :-
    arg(1, Channel, Creator) |
	psi_receive(psi(Creator), Channel, Message, 1).
psi_receive(Id, Channel, Message) :-
	psi_receive(Id, Channel, Message, 1).
psi_receive(Id, Channel, Message, Multiplier) :-
    Channel = _Creator(FcpChannel, _Circuit),
    channel(FcpChannel) :
      Receive = receive(Id, Ms, 1, Multiplier, Chosen),
      write_channel(Receive, FcpChannel) |
	psi_received(1, Chosen, Ms, Message);
    otherwise :
      Id = _,
      Message = _,
      Multiplier = _ |
	computation#display(('psi_library: Can''t receive from' : Channel)).

psi_close(Channel) :-
    Channel =?= _Creator(_FcpChannel, Circuit) :
      Circuit = {Close, Close}.

psi_received(Tag, Chosen, Ms, Message) :-
    Chosen =?= Tag :
      Ms = Message.

psi_sent(Tag, Chosen) :-
    Chosen =?= Tag | true.

"

	| true.
