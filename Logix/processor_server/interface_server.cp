/*

Interface Monitor - Communicate with UNIX

Michael Hirsch, Feb 1986
Bill Silverman, Feb 1988

	$Author: bill $
    	$Date: 1999/07/09 07:03:39 $
	$Locker:  $
	$Revision: 1.1 $
	$Source: /home/qiana/Repository/Logix/processor_server/interface_server.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([start/1]).
-mode(trust).
-language(compound).

procedure convert_date(String, String, String).
procedure date(String).
procedure date_time(String, String, String).
procedure errno(Integer, String).
procedure getenv(String, String).
procedure gmtime(String).
procedure gmt2date(String, String).
/* procedure date2gmt(String, String). */
procedure gmdate(String).
procedure gm2local(String, String).
procedure home_dir(String, String).
procedure net_host(String, String, String).
procedure processid(String).
procedure unix_call(String).
procedure user_data({String, String}).
procedure whoami(String).

SystemReply ::= true | false(String).


start(In) :-
	processor # link(lookup(interface, Offset)),
	server(In, Offset).


server(In, Offset) :-

    In ? {convert_date(FullDate, Date?, Time?), Ok, Common} |
	convert_date(FullDate, DateString, Ok, Common),
	date_time(DateString, Date, Time),
	server;

    In ? {date(Date?), Ok, Common} :
      Common = done,
      Ok = true |
	execute(Offset, date(Date)),
	server;

    In ? {date_time(FullDate?, Date?, Time?), Ok, Common} :
      Common = done,
      Ok = true |
	execute(Offset, date(FullDate)),
	date_time(FullDate?, Date, Time),
	server;

    In ? {errno(Errno, Diagnostic?), Ok, Common} |
	errno(Offset, Errno, Diagnostic, Ok, Common),
	server;

    In ? {getenv(Name, Value?), Ok, Common} |
	str2val(getenv, Name, Value, Offset, Ok, Common),
	server;

    In ? {gmtime(Time?), Ok, Common} :
      Common = done,
      Ok = true |
	execute(Offset, gmtime(Time)),
	server;

    In ? {gmt2date(Time, Date?), Ok, Common} |
	str2val(gmt2date, Time, Date, Offset, Ok, Common),
	server;

    In ? {gmdate(Date?), Ok, Common} :
      Common = done,
      Ok = true |
	execute(Offset, gmdate(Date)),
	server;

    In ? {gm2local(Gm, Local?), Ok, Common} |
	str2val(gm2local, Gm, Local, Offset, Ok, Common),
	server;

    In ? {home_dir(Name, Directory?), Ok, Common} |
	home_directory(Offset, Name, Directory, Ok, Common),
	server;

    In ? {net_host(Name?, Id?, Domain?), Ok, Common} |
	net_host(Offset, Name, Id, Domain, Ok, Common),
	server;

    In ? {processid(Name?), Ok, Common} :
      Common = done,
      Ok = true |
	processid(Offset, Name),
	server;

    In ? {unix_call(String), Ok, Common} |
	unix_call(String, Offset, Ok, Common),
	server;

    In ? {user_data({Name?, Directory?}^), Ok, Common} :
      Common = done,
      Ok = true |
	whoami(Offset, Name),
	home_directory(Offset, Name, Directory, Ok, Common),
	server;

    In ? {whoami(Name?), Ok, Common} :
      Common = done,
      Ok = true |
	whoami(Offset, Name),
	server;

    otherwise ,
    In ? _Other(Ok, Common) :
      Ok = false(unknown),
      Common = done |
	server;

    In ? _Any(Ok, Common),
    known(Common) :
      Ok = false(aborted) |
	self;

    In = [] : Offset = _ .


convert_date(GivenDate, FullDate, Ok, Common) :-

    string(GivenDate),
    string_length(GivenDate) =:= 12 :
      FullDate = GivenDate,
      Ok = true,
      Common = done;

    otherwise : GivenDate = _,
      FullDate = yymmddhhmmss,
      Ok = false(wrong_length),
      Common = done ;

    known(Common) : GivenDate = _,
      FullDate = "            ",
      Ok = false(aborted) .
      

execute(Offset, Tuple) :-
    true :
      execute(Offset, Tuple) .

str2val(Function, String, Value, Offset, Ok, Common) :-

    string(String) :
      Ok = true,
      Common = done,
      execute(Offset, Function(String, Value)) ;

    otherwise,
    unknown(Common) : Function = _, String = _, Value = _, Offset = _,
      Ok = false(not_a_valid_string),
      Common = done ;

    known(Common) : Function = _, String = _, Value = _, Offset = _,
      Ok = false(aborted) .


date_time(FullDate, Date, Time) :-
    string(FullDate),
    string_to_dlist(FullDate, [Y1, Y2, M1, M2, D1, D2,
			       H1, H2, I1, I2, S1, S2
			      ],
		    []
    ) :
      ascii('/', S),
      ascii(':', C) |
	list_to_string([D1, D2, S, M1, M2, S, Y1, Y2], Date),
	list_to_string([H1, H2, C, I1, I2, C, S1, S2], Time).

errno(Offset, Errno, Diagnostic, Ok, Common) :-

    integer(Errno) :
      execute(Offset, errno(Errno, Diagnostic)),
      Common = done,
      Ok = true ;

    otherwise : Offset = _,
      Common = done,
      Diagnostic = "",
      Ok = false(invalid - Errno) ;

    known(Common) : Offset = _, Errno = _,
      Diagnostic = "",
      Ok = false(aborted).

unix_call(Call, Offset, Ok, Common) :-

    string(Call) :
      Common = Done?,
      execute(Offset, system(Call, Terminate, ErrorNumber)) |
	unix_call_reply;

    otherwise : Call = _, Offset = _,
      Ok = false(not_a_string),
      Common = done ;

    known(Common) : Call = _, Offset = _,
      Ok = false(aborted) .

unix_call_reply(Terminate, Offset, Ok, ErrorNumber, Done) :-

    Terminate = 0 : Offset = _, ErrorNumber = _,
      Done = done,
      Ok = true ;

    Terminate < 0 :
      execute(Offset, errno(ErrorNumber, ErrorString)) |
	unix_call_error;

    Terminate > 0,
    Code := Terminate/256 : Offset = _, ErrorNumber = _,
      Done = done,
      Ok = false(terminate(Code)).

unix_call_error(ErrorNumber, ErrorString, Ok, Done) :-

    ErrorString =\= "" : ErrorNumber = _,
      Done = done,
      Ok = false(ErrorString) ;

    ErrorString = "" :
      Done = done,
      Ok = false(errno(ErrorNumber)) .

whoami(Offset, Name) :-
    true :
      execute(Offset, whoami(Name)) .


processid(Offset, Name) :-
    true :
      execute(Offset, processid(Name)) .


home_directory(Offset, Name, Directory, Ok, Common) :-

    known(Name) :
      execute(Offset, homedir(Name, Directory)),
      Ok = true,
      Common = done ;

    known(Common) : Offset = _, Name = _, Directory = _,
      Ok = false(aborted) .

net_host(Offset, Name, Id, Domain, Ok, Common) :-

    true :
      execute(Offset, nethost(Name, Id, Domain)),
      Ok = true,
      Common = done ;

    known(Common) : Offset = _, Name = _, Id = _, Domain = _,
      Ok = false(aborted) .
