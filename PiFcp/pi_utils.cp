-language(compound).
-export([make_channel/1, make_channel/4, send/2, receive/2]).


make_channel(Channel)+(Creator = "SYSTEM", VC, MsC) :-
  we(Channel) :
    make_channel(VC, MsC),
    Channel = Creator(VC, MsC);
  string(Channel), Channel =\= "_", Channel =\= "" |
	computation#dictionary(add, C, V, R),
	made_channel(C, Creator, VC, MsC, V, R).

  made_channel(C, Creator, VC, MsC, V, R) :-
    R = new :
      make_channel(VC, MsC),
      V = Creator(VC, MsC);
    otherwise |
	screen#display(("pi_utils: Can't make_channel" : C - R)).


send(Message, Channel) :-
  Channel = _Creator(C, _Stream),
  channel(C) :
    Ms = Sender?(Message, 1, _),
    write_channel(Ms, C) |
      pi_monitor#unique_sender("PI_UTILS.send", Sender);

  string(Channel), Channel =\= "_", Channel =\= "" |
	computation#dictionary(find, Channel, V, R),
	send_message(Message, Channel, V, R).

  send_message(Message, Channel, V, R) :-
    R = true,
    [] @< V |
	send(Message, V);

    otherwise |
	computation#display(("pi_utils: Can't send to" : C - R)).


receive(Channel, Message) :-
  string(Channel), Channel =\= "_", Channel =\= "" |
	computation#dictionary(find, Channel, V, R),
	receive_message(Channel, Message, V, R);

  Channel = Creator(C, Stream),
  Stream ? _Sender(_Message, _N, Choice),
  not_we(Choice) :
    Channel' = Creator(C, Stream') |
      self;

  Channel = _Creator(_C, Stream),
  Stream ? _Sender(M, N, Choice),
  we(Choice) :
    Choice = N,
    Message = M.


  receive_message(Channel, Message, V, R) :-
    R = true,
    [] @< V |
	receive(V, Message);

    otherwise |
	computation#display(("pi_utils: Can't receive from" : C - R)).
    
