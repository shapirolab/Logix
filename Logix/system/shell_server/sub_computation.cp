/*

Computation shell - Bill Silverman 04/92.

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:32 $
Currently locked by 	$Locker:  $
			$Revision: 1.1.1.1 $
			$Source: /home/qiana/Repository/Logix/system/shell_server/sub_computation.cp,v $

Copyright (C) 1992, Weizmann Institute of Science - Rehovot, ISRAEL

*/


-language([inherit, dfcp]).
-export([start/4]).
-mode(trust).

start(Requests, Events, Redelegates, Delegates) :-

	computation # self # service_id(SId),
	start1(Requests, Events, Redelegates, Delegates, SId?).

start1(Requests0, Events1, Redelegates, Delegates, SId) :-

    ground(SId) |
	Requests ! change_scope(SId, Ok),
	Requests' = Requests0,
	computation # "_domain"(domain_channel(Domain)),
	computation # "_controls"(CCC),
	controls(CCC?, Out!, CCC', SuperCH),
	computation_server # computation(Requests?, CCC'?, Domain?, Events),
	delegate(Out?, Events?, Events1, SuperCH?, Redelegates, Delegates),
	ok(Ok?).

ok(True) :-
    True = true | true.

controls(CCIn, Out, CCOut, SuperCHOut) :-

    CCIn = {Ss, L, R, SuperCH} |
	CCOut = {Ss, L, R, Out},
	SuperCHOut = SuperCH .

delegate(Out, InEs, OutEs, SuperCH, Redelegates, Delegates) :-

    Out ? Delegated, Delegated = delegated(_, _) |
	Delegates ! Delegated,
	self;

    Out ? Link, Link = link(_, _, _) |
	Delegates ! Link,
	self;

    Out ? Request, Request =\= delegated(_, _), Request =\= link(_, _, _) |
	write_channel(Request, SuperCH, SuperCH'),
	self;

    Redelegates ? Request |
	write_channel(Request, SuperCH, SuperCH'),
	self;

    InEs ? Event |
	OutEs ! Event,
	self;

    unknown(Out),
    InEs = [] |
	Delegates = [],
	Out' = [],
	Delegates' = [],
	self;

    Out = [], Redelegates = [],
    Delegates = [], InEs = [] |
	SuperCH = _,
	OutEs = [] .


