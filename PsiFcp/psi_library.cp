-export([string/1]).
-mode(trust).
-language(compound).

procedure string(String).

string(Text) :- true : Text =

"library.
-language([evaluate,compound,colon]).
-include(psi_constants).

psi_channel(Channel) :-
	psi_channel(Channel, 'SYSTEM', infinite).
psi_channel(Channel, Creator) :-
	psi_channel(Channel, Creator, infinite).
psi_channel(Channel, Creator, BaseRate) :-
	psi_channel(Channel, Creator, BaseRate).
psi_channel(Channel, Creator, BaseRate) :-
    number(BaseRate) :
      Channel = Channel'? |
	psi_monitor#new_channel(Creator, Channel', BaseRate);
    BaseRate =?= infinite :
      Channel = Channel'? |
	psi_monitor#new_channel(Creator, Channel', BaseRate);
    otherwise :
      Creator = _,
      BaseRate = _ |
	computation#display(('psi_library: Can''t make channel' : Channel)).

psi_channel(Channel, Creator, BaseRate, ComputeWeight) :-
    string(ComputeWeight) :
      Channel = Channel'? |
	psi_monitor#new_channel(Creator, Channel', ComputeWeight, BaseRate);
    tuple(ComputeWeight) :
      Channel = Channel'? |
	psi_utils#make_channel(Creator, Channel', ComputeWeight, BaseRate);
    otherwise :
      Creator = _,
      BaseRate = _,
      ComputeWeight = _ |
	computation#display(('psi_library: Can''t make channel' : Channel)).

psi_send(Message, Channel) :-
	psi_send(Message, Channel, 1, sender).
psi_send(Message, Channel, Multiplier) :-
	psi_send(Message, Channel, Multiplier, sender).
psi_send(Message, Channel, Multiplier, Id) :-
    vector(Channel) :
      Send = PSI_SEND(Id, Channel, Multiplier, 1) |
	psi_monitor#scheduler(S),
	write_channel(start(psi_send, [Send], Value, Chosen), S),
	psi_transmitted(sending(Id), 1, Chosen, Message, Value);
    otherwise :
      Id = _,
      Message = _,
      Multiplier = _ |
	computation#display(('psi_library: Can''t send to' : Channel)).

psi_receive(Channel, Message) :-
	psi_receive(Channel, Message, 1, receiver).
psi_receive(Channel, Message, Multiplier) :-
	psi_receive(Channel, Message, Multiplier, receiver).
psi_receive(Channel, Message, Multiplier, Id) :-
    vector(Channel) :
      Receive = PSI_RECEIVE(Id, Channel, Multiplier, 2) |
	psi_monitor#scheduler(S),
	write_channel(start(psi_receive, [Receive], Value, Chosen), S),
	psi_transmitted(receiving(Id), 2, Chosen, Message, Value?);
    otherwise :
      Id = _,
      Message = _,
      Multiplier = _ |
	computation#display(('psi_library: Can''t receive from' : Channel)).

psi_close(Channel) :-
    vector(Channel) |
	psi_monitor#scheduler(S),
	write_channel(close({Channel}), S?);
    otherwise |
	computation#display(('psi_library: Can''t close' : Channel)).

psi_transmitted(Id, Tag, Chosen, Message, Value) :-

    Chosen = Tag :
      Id = _,
      Value = Message;

    Chosen =\= Tag :
      Message = _,
      Id = _,
      Value = _.
"

	| true.
