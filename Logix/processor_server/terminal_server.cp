/*

terminal - tty server

Shimon Cohen & Ehud Shapiro - May 3, 1985
Bill Silverman - 1986-1989

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:39 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/processor_server/terminal_server.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([start/1]).
-mode(trust).
-language(compound).

procedure output(Any).
procedure outputln(Any).
procedure prompt(Any).

Commands ::= [Command].
Datum ::= String ; Integer ; Commands.
Command ::= {flush} ; bytes([Integer]) ; confirm(true) ; Datum.

start(In) :-
    list(In) |
	processor # link(lookup(interface, Flush), Ok),
 	start_server(In, Flush, Ok).
start([]).

start_server(In, Flush, Ok) :-

    Ok = true :
      priority([], low) |
	make_channel(TC, TIn),
	server(In, TIn, TC, Rs, _Os),
	tty(Rs, Flush);

    Ok = false(Reason) : In = _, Flush = _,
      ttyput_string("processor can't start interface - ") |
	reason(Reason).

reason(Reason) :-

    string(Reason) :
      ttyput_string(Reason) ;

    integer(Reason) :
      ttyput_integer(Reason) ;

    otherwise : Reason = _,
      ttyput_string("??").



server(In, TIn, TC, Is, Os) :-

    In ? {spy(Os?^), true^, done^} |
	self;

    In ? {output(Argument), Ok, Common} :
      Ok = true,
      Os ! output(Argument),
      Is ! break(Common, Argument, {flush}) |
	self;

    In ? {outputln(Argument), Ok, Common} :
      Ok = true,
      ascii(lf, LF),
      Os ! outputln(Argument),
      Is ! break(Common, Argument, [bytes([LF]), {flush}]) |
	self;

    In ? {prompt(Argument), Ok, Common} :
      Ok = true,
      Os ! prompt(Argument),
      Is ! break(Common, Argument, {flush}) |
	self;

    In ? {channel(TerminalC?), Ok, Common} :
      Ok = true,
      Common = done |
	TerminalC = TC,
	self;

    In ? _Other(Ok, Common),
    otherwise :
      Ok = false(unknown),
      Common = done |
	self;

    In ? _Any(Ok, Common),
    known(Common) :
      Ok = false(aborted) |
	self;

    TIn ? output(Argument, Common) :
      Os ! output(Argument),
      Is ! break(Common, Argument, {flush}) |
	self;

    TIn ? outputln(Argument, Common) :
      Os ! outputln(Argument),
      ascii(lf, LF),
      Is ! break(Common, Argument, [bytes([LF]), {flush}]) |
	self;

    TIn ? prompt(Argument, Common) :
      Os ! prompt(Argument),
      Is ! break(Common, Argument, {flush}) |
	self;

    TIn ? Other,		% just discard junk on fast line
    Other =\= output(_, _),
    Other =\= outputln(_, _),
    Other =\= prompt(_, _) |
	self;

% copied:

    In ? Request,
    Request = _(Ok, Abort), known(Abort) :
      Ok = false(aborted) |
	self;

    In = [] :
      close_channel(TC),
      Is = [],
      Os = [] |
	close_common_in.

  close_common_in(TIn) :-
    TIn ? _(_, Common), writable(Common) :
      Common = done |
	self;
    TIn ? _, otherwise |
	self;
    TIn = [] | true.

%    + super#terminate .


tty(Rs, Flush) :-

    Rs ? break(done^, Stream, End) :
      Rs'' = [Stream, End | Rs'] |
	self;
/*
	Alternatively, we could monitor  Common  until a corresponding
	mark (e.g. the {flush}) and then set Common = done.  That would give
	finer control over the output-stream.  However, that degree of control
	isn't needed unless very long streams/strings are being output.
*/
    Rs ? I,
    integer(I) :
      ttyput_integer(I) |
	self;

    Rs ? S,
    string(S) :
      ttyput_string(S) |
	self;

    Rs ? bytes(Bs) |
	tty_bytes(Bs, Rs', Flush);

    Rs ? [X|Xs] :
      Rs'' = [X, Xs | Rs'] |
	self;

    Rs ? [] |
	self;

    Rs ? confirm(true^) :
      execute(Flush, {flush}) |
	self;

    Rs ? {flush} :
      execute(Flush, {flush}) |
	self;

    otherwise |
	tty_end(Rs, Flush).

tty_end(Rs, Flush) :-

    Rs ? break(_, _, _) |
	tty(Rs', Flush);

    Rs ? X,
    otherwise |
	computation # error(terminal, dont_understand, X),
	tty(Rs', Flush);

    otherwise : Rs = _, Flush = _,	%	Usually  Rs = []
      ttyput_string('^G') .


tty_bytes(Bs, Rs, Flush) :-

    Bs ? B :
      ttyput_byte(B) |
	self;

    otherwise : Bs = _	|	%	Usually  Bs = []
	tty(Rs, Flush);

    unknown(Bs) :
      execute(Flush,{flush}) |
	tty_bytes_wait(Bs,Rs,Flush).

tty_bytes_wait(Bs, Rs, Flush) :-
    known(Bs) |
	tty_bytes(Bs, Rs, Flush).
