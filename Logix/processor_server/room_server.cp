/*

Room server
Bill Silverman - 12/91

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:38 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/processor_server/room_server.cp,v $

Copyright (C) 1991, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([start/1]).
-mode(trust).
-language([inherit, dfcp]).

start(In) :-

    list(In) |
	dynamic_attributes(Attributes, PId),
	IdCard = [(name : PId?), (type : processor) | Attributes?],
	system # "Doors" #
		["Processor" # create(IdCard?, Door, Ok),
		 doors # Ignore?],

	ok(Ok?),
	processor # device(signals(Devices)),
	start1(In, Door?, Devices?, Receive),
	ignore(Receive?, Ignore);

    In = [] | true.


dynamic_attributes(Attributes, PIdOut) :-
	processor # [interface(whoami(UserName)),
		     interface(processid(PId)),
		     interface(net_host(NetHostName, NetHostId, DomainName))],
	PIdOut = PId??,
	string_to_dlist(PId?, LPI, []),
	split_pid(LPI?, PN, ID),
	list_to_string(PN?, ProcessorName),
	list_to_string(ID?, ProcessId),
	string_to_dlist(DomainName?, DN, []),
	string_to_dlist(NetHostName?, NN, []),
	prefix_name(NN?, DN?, DR),
        prefix_name(NN??, PN??, PR),
	net_domain(DR?, PR?, NetHostName??, DomainName??,
			NetDomainName, NetServerName
	),
	Attributes = [(user : UserName?),
		      (processor_name : ProcessorName?),
		      (processid : ProcessId?),
		      (net_domain : NetDomainName?),
		      (net_name : NetServerName?),
		      (net_id : NetHostId?)].

prefix_name(Ns, Ps, Reply) :-

    Ns ? C,
    Ps ? C |
	self;

    Ns ? Dot, Dot =:= ascii('.'),
    Ps = [] |
	Reply = true(Ns');

    otherwise |
	Ps = _,
	Reply = false(Ns).

net_domain(DR, PR, NetHostName, DomainName, NetDomainName, NetServerName) :-

    DR = true(_),
    PR = true(_),
    string(NetHostName) |
	DomainName = _,
	NetDomainName = NetHostName,
	NetServerName = NetHostName;

    DR = true(_),
    PR = false(_),
    string(NetHostName) |
	DomainName = _,
	computation # comment((using_domain_name : NetHostName)),
	NetDomainName = NetHostName,
	NetServerName = NetHostName;

    DR = false(_),
    PR = true(NDs) |
	DomainName = _,
	list_to_string(NDs, NetDomainName),
	NetServerName = NetHostName;


    DR = false(_),
    PR = false([]),
    NetHostName =\= localhost,
    string(DomainName) |
	string_to_dlist(DomainName, NDs, []),
	string_to_dlist(NetHostName, NHs, [46 | NDs?]), /* 46 =:= ascii('.') */
	list_to_string(NHs?, NetServerName),
	NetDomainName = DomainName;

    DR = false(_),
    PR = false(PRs),
    NetHostName = localhost |
	DomainName = _,
	NetDomainName = "",
	list_to_string(PRs, NetServerName);

    DR = false(_),
    PR = false(NDs), NDs =\= [],
    NetHostName =\= localhost |
	DomainName = _,
	computation # diagnostic(ambiguous_network_name(NetHostName)),
	split_pid(NDs, _, Ds),
	list_to_string(Ds?, Name),
	NetDomainName = Name?,
	NetServerName = Name??.


split_pid(LPI, PN, ID) :-

    LPI ? C, C =\= 46 /* 46 =:= ascii('.') */ |
	PN ! C,
	self;

    LPI ? C, C =:= 46 /* 46 =:= ascii('.') */ |
	PN = [],
	ID = LPI' ;

    LPI = [] |				% very curious - no '.' ?
	PN = [],
	ID = [] .
	

ignore(Receive, Ignore) :-

    Receive ? Message |
	Ignore ! generic_reply(Message, true),
	self;

    Receive = [] |
	Ignore = [] .


start1(In, Door, Devices, ReceiveOut) :-

    Door = door(Send?, Receive) |
	ReceiveOut = Receive,
	serve.

serve(In, Send, Devices) :-

    Devices ? restart |
	dynamic_attributes(Attributes, PId),
	Send ! restart([(name : PId?) | Attributes?]),
	self;

    In ? Request(Ok?, Common?) |
	Send ! Request,
	Ok = true,
	Common = done,
	serve;
    
% copied:

    In ? Request,
    Request = _(Ok?, Abort), known(Abort) |
	Ok = false(aborted),
	self;

    In = [] |
%    + super#terminate .
	Devices = _,
	Send = [close(Ok)],
	ok(Ok?).

ok(True) :-
    True = true | true.
