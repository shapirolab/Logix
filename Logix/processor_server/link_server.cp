/*

Link server
Bill Silverman - 06/86

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:39 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/processor_server/link_server.cp,v $

Copyright (C) 1986, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([start/2]).
-mode(trust).
-language(compound).


Context ::= String ; {`'#', String, Context} ; [String] ;
	    context(Channel) ; context(Channel, Any, Any).
Channel ::= Vector.
OffsetReply ::= Integer ; unbound ; aborted.
Files ::= String ; [String].
SystemReply ::= true ; false(Any).

procedure execute_in_context(Context, Request).
procedure lookup(String, OffsetReply).
procedure lookup(String, Files, OffsetReply).
procedure execute(Integer, Tuple).
procedure unbind(String).

Request ::= lookup(String, OffsetReply) ;
	    lookup(String, Unix_ld_Opts, OffsetReply) ;
	    get(String, OffsetReply) ;
	    get(String, Unix_ld_Opts, OffsetReply) ;
	    execute(String, Tuple) ;
	    unbind(String).

start(In, Defer) :-
	server(In, Defer, [], []).

server(In, Defer, Dictionary, DefaultContext) :-

    In ? {Execute, Ok, Common},
    Execute = execute_in_context(Context, _Request),
    unknown(Context) :
      write_channel(defer(Context, {link(Execute, Ok), Common}), Defer) |
	self;

    In ? {Execute, Ok, Common},
    Execute = execute_in_context(_Context, Request),
    unknown(Request) :
      write_channel(defer(Request, {link(Execute, Ok), Common}), Defer) |
	self;

    In ? {Execute, Ok, Common},
    Execute = execute_in_context(_Context, Request),
    arg(1, Request, Functor),
    unknown(Functor) :
      write_channel(defer(Functor, {link(Execute, Ok), Common}), Defer) |
	self;

    In ? {execute_in_context(Context, Request), Ok', Common},
    otherwise :
      Common = context(Context # service_id(SId), Common'),
      Common' = reply(Ok, Ok, Ok', Common'') |
	select(Request, Defer, Dictionary, SId, Dictionary', Ok, Common''),
	self;

    In ? {default_context(DefaultContext', DefaultContext^), Ok, Common} :
      Common = reply(DefaultContext', true, Ok, done) |
	self;

    In ? {Request, Ok', Common},
    Request =\= execute_in_context(_, _),
    Request =\= default_context(_, _) :
      Common = reply(Ok, Ok, Ok', Common') |
	select(Request, Defer, Dictionary, DefaultContext, Dictionary',
		Ok, Common'
	),
	self;

% copied:

    In ? Request,
    Request = _(Ok, Abort), known(Abort) : Dictionary = _, DefaultContext = _,
      Ok = false(aborted) |
	self;

    In = [] : Defer = _, Dictionary = _, DefaultContext = _ .
%    + super#terminate .


select(Request, Defer, Dictionary1, SId, Dictionary2, Ok, Common) :-

    Request = lookup(Name, _),
    string(Name) : Defer = _,
      Common = reply(Offset, Request, lookup(_, Offset), done) |
	find(Name, Dictionary1, Link),
	compose([], [_| SId], Dictionary1, Dictionary2,
			Name, Link, Offset, Ok
	);

    Request = lookup(Name, Unix_ld_Opts, _),
    string(Name),
    known(Unix_ld_Opts) : Defer = _,
      Common = reply(Offset, Request, lookup(_, _, Offset), Common') |
	check_ld_opts(Request, Unix_ld_Opts, Unix_ld_Opts', Common'),
	find(Name, Dictionary1, Reply),
	compose(Unix_ld_Opts', [_| SId], Dictionary1, Dictionary2,
			Name, Reply, Offset, Ok
	);

    Request = get(Name, _),
    string(Name) : Defer = _, SId = _,
      Common = reply(Offset, Request, get(_, Offset), done) |
	find(Name, Dictionary1, Link),
        concatenate(Name, '.o', NDO),
	get([], Dictionary1, Dictionary2, Name, NDO, Link, Offset, Ok);

    Request = get(Name, Unix_ld_Opts, _),
    string(Name),
    known(Unix_ld_Opts) : Defer = _, SId = _,
      Common = reply(Offset, Request, get(_, _, Offset), Common') |
	check_ld_opts(Request, Unix_ld_Opts, Unix_ld_Opts', Common'),
	find(Name, Dictionary1, Link),
        concatenate(Name, '.o', NDO),
	get(Unix_ld_Opts', Dictionary1, Dictionary2, Name,
		 NDO, Link, Offset, Ok
	);

    Request = execute(Name, Arguments),
    string(Name),
    tuple(Arguments) : Defer = _, SId = _,
      Dictionary1 = Dictionary2,
      Common = reply(Status, Status, Ok, done) |
	find(Name, Dictionary1, Offset),
	execute(Offset, Arguments, Status);

    Request = unbind(Name, _),
    string(Name) : Defer = _, SId = _,
      Common = reply(Unbound, Unbound, Ok, done) |
	unbind(Name, Unbound, Dictionary1, Dictionary2);

    otherwise : Defer = _, Request = _, SId = _,
      Dictionary1 = Dictionary2 |
      Common = done,
      Ok = false(unknown) ;

    Request = _Any(Ok, Common),
    known(Common) :
      Ok = false(aborted) |
	self;

    arg(2, Request, Name),
    unknown(Name) : SId = _,
      Dictionary1 = Dictionary2,
      write_channel(defer(Name, {link(Request, Ok), Common}), Defer) ;

    arity(Request) =:= 4,
    arg(3, Request, Unix_ld_Opts),
    unknown(Unix_ld_Opts) : SId = _,
      Dictionary1 = Dictionary2,
      write_channel(defer(Unix_ld_Opts, {link(Request, Ok), Common}), Defer) ;

    Request = execute(_Name, Arguments),
    unknown(Arguments) : SId = _,
      Dictionary1 = Dictionary2,
      write_channel(defer(Arguments, {link(Request, Ok), Common}), Defer) ;

    known(Common) : Defer = _, SId = _, Request = _,
      Ok = false(aborted),
      Dictionary1 = Dictionary2 .


get(Unix_ld_Opts, Dictionary1, Dictionary2, Name, NDO, Link, Offset, Ok) :-

    Unix_ld_Opts = aborted :
      NDO = _, Link = _, Name = _,
      Dictionary2 = Dictionary1,
      Offset = unbound,
      Ok = false(aborted) ;

    Link = unbound, Unix_ld_Opts =\= aborted :
      link({[NDO | Unix_ld_Opts]}, Offset),
      Dictionary2 = [Name(Offset) | Dictionary1],
      Ok = true;
	
    Link = unbound, Unix_ld_Opts =\= aborted, otherwise :
      NDO=_, Name = _,
      Dictionary2 = Dictionary1,
      Offset = unbound,
      Ok = false(unbound);

    known(Link), otherwise :
      NDO = _, Name = _, Unix_ld_Opts = _,
      Offset = Link,
      Dictionary2 = Dictionary1,
      Ok = true .

unbind(Name, Reply, Dictionary1, Dictionary2) :-

    Dictionary1 ? Name(_) :
      Reply = true,
      Dictionary1' = Dictionary2 ;

    otherwise,
    Dictionary1 ? _ |
	unbind.

unbind(_, false(unbound)^, [], []^).


check_ld_opts(Request, Opts, Checked, Common) + (Head, Tail = Head) :-

    Opts ? Opt,
    string(Opt) :
      Tail ! Opt |
	check_ld_opts;

    Opts = [] : Request = _,
      Common = done,
      Checked = Head,
      Tail = [] ;

    Opts =\= [_ | _], Opts =\= [] :
      Opts' = [Opts] |
	check_ld_opts;

    otherwise,
    Opts ? NotString :
      Common = fork(exception(not_a_string(NotString), Request), Common') |
	check_ld_opts;

    known(Common) : Opts = _, Head = _, Tail = _, Request = _,
      Checked = aborted .


compose(Opts, SId, Dictionary1, Dictionary2, Name, Link, Offset, Ok) :-

    Link = unbound,
    string_to_dlist(Name, NameChars, _),
    NameChars ? FirstChar, FirstChar =\= 47 /* Slash */,
    Opts =\= aborted,
    SId ? _ : NameChars' = _   |
	SId' # "_unique_id"(UID),
	concatenate(UID, 'bin/', UIDbin),
	concatenate(Name, '.o', NDO),
	file_names([UID, UIDbin], NDO, Opts, FileLists),
	link(FileLists, Name, Dictionary1, Dictionary1', Link'),
	compose;

    Link = unbound,
    string_to_dlist(Name, NameChars, _),
    NameChars ? FirstChar, FirstChar =:= ascii('/'),
    Opts =\= aborted : SId = _, NameChars' = _,
      FileLists = [ [NDO | Opts] ] |
        concatenate(Name, '.o', NDO),
        link(FileLists, Name, Dictionary1, Dictionary2, Offset),
	check_offset(Offset, Ok);

    Opts = aborted : SId = _, Name = _, Link = _, Offset = _,
      Dictionary1 = Dictionary2,
      Ok = false(aborted) ;

    known(Link),
    otherwise : Opts = _, SId = _, Name = _,
      Ok = true,
      Dictionary1 = Dictionary2,
      Link = Offset .

check_offset(Offset, Ok) :-

    integer(Offset) :
      Ok = true;

    otherwise :
      Ok = false(Offset) .

concatenate(String1, String2, String) :-
    string_to_dlist(String1, Cs, Ts),
    string_to_dlist(String2, Ts, []) |
	list_to_string(Cs, String).


file_names(Prefixes, NDO, Files, FileLists) + (FLT, FLH = FLT, File = ok) :-

    known(File),
    Prefixes ? Prefix,
    string(Prefix) :
      FLT ! [File' | Files] |
	concatenate(Prefix, NDO, File'),
	file_names;

    otherwise : Prefixes = _, NDO = _, Files = _, File = _,
      FileLists = FLH,
      FLT = [] .


find(Name, Dictionary, Offset) :-

    Dictionary ? Name(Offset^) : Dictionary' = _ ;

    Dictionary ? Other(_),
    Other =\= Name |
	find;

    Dictionary = [] : Name = _,
      Offset = unbound.


link(FileLists, Name, Dictionary1, Dictionary2, Offset) :-

    FileLists = [FileList | _] :
      link(FileList, Offset),
      Dictionary2 = [Name(Offset) | Dictionary1] ;

    FileLists ? _,
    otherwise |
	link;

    FileLists = [] : Name = _,
      Dictionary1 = Dictionary2,
      Offset = unbound.


execute(Offset, Arguments, SystemReply) :-

    integer(Offset) :
      execute(Offset, Arguments) |
	SystemReply = true ;

    otherwise : Arguments = _,
      SystemReply = false(Offset) .
