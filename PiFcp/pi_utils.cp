-language(compound).
-export([make_channel/1, make_channel/4, send/2, receive/2]).


make_channel(Channel)+(Creator = "SYSTEM", VC = _, MsC = _) :-
    we(Channel) :
      make_channel(VC, MsC),
      Channel = Creator(VC, MsC);
    string(Channel), Channel =\= "_", Channel =\= "" |
	computation#dictionary(add, Channel, V, R),
	made_channel(Channel, Creator, VC, MsC, V, R).

  made_channel(Channel, Creator, VC, MsC, V, R) :-
    R = new,
    string_to_dlist(Creator, CL, CT),
    string_to_dlist(Channel, Cl, []) :
      CT = [46 | Cl],
      make_channel(VC, MsC),
      V = Creator'?(VC, MsC) |
	list_to_string(CL, Creator');
    otherwise :
      Creator = _,
      VC = _,
      MsC = _,
      V = _ |
	screen#display(("pi_utils: Can't make_channel" : Channel - R)).


send(Message, Channel) :-
    Channel = _Creator(C, _Stream),
    channel(C) :
      Ms = Sender?(Message, 1, _),
      write_channel(Ms, C) |
	pi_monitor#unique_sender("PI_UTILS.send", Sender);

    string(Channel), Channel =\= "_", Channel =\= "" |
	computation#dictionary(find, Channel, V, R),
	send_message(Message, Channel, V, R);

    otherwise :
      Message = _ |
	computation#display(("pi_utils: Can't send to" : Channel)).

  send_message(Message, Channel, V, R) :-
    R = true,
    [] @< V :
      Channel = _ |
	send(Message, V);

    otherwise :
      Message = _,
      V = _ |
	computation#display(("pi_utils: Can't send to" : Channel - R)).


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
      Stream' = _,
      Choice = N,
      Message = M.


  receive_message(Channel, Message, V, R) :-
    R = true,
    [] @< V :
      Channel = _ |
	receive(V, Message);

    otherwise :
      Message = _,
      V = _ |
	computation#display(("pi_utils: Can't receive from" : Channel - R)).
    
