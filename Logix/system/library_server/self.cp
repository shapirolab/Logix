/*

Library Monitor server for fcp-pre-compiler.

Michael Hirsch,  27 March 1985
Bill Siverman, 1986 - 1989
Eitan Shterenbaum, 1988

Last update by		$Author: bill $
			$Date: 1999/07/09 07:03:34 $
Currently locked by     $Locker:  $
			$Revision: 1.1.1.1 $
			$Source: /users/staff/fcp/Logix/Compiler/RCS/precompil\
er.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([serve/1, serve/2]).
-mode(trust).
-language(compound).

procedure serve([Request]).
procedure serve([Request], Library).

serve(In) + (Library = []) :-
	server(In, Library).

server(In, Library) +  (Defer = []) :-

    In ? Query, 
    Query = query(_, _, _),
    Library = [] : Defer' = [Query | Defer] |
	server;

    In ? query(Queries, Ids, SysCode),
    Library =\= [] |
	queries(Queries, SysCode0, Library),
	build_idlib(Ids, [], IdLib),
	remove_duplicates(SysCode0, Library, IdLib, SysCode),
	server;

    In ? build(Insertions, Errs) |
	insert(Insertions, Library, Library', Errs),
	server;

    In ? retrieve(GroundedLib) |
	utils # ground(Library, GroundedLib),
	server;

    In ? library(OtherIn, OtherLibrary) |
	server(OtherIn, OtherLibrary),
	server;

    otherwise,
    In ? X |
	server,
	fail(X, unknown);

    Defer ? Query, 
    arg(1,  Query,  query),
    Library =\= [] : In' = [Query | In] |
	server;

    In = [], Defer = [] : Library = _ |
	true.


insert(Procedures, Library1, Library2, Errs) :-

    Procedures ? procedure(Id, Code, Xrefs) |
	insert(Id, Code, Xrefs, Library1, Library1', Errs, Errs'),
	insert;

    Procedures = [] :
      Library1 = Library2,
      Errs = [] |
	true.


insert(Id, Code, Xrefs, Library1, Library2, Errs1, Errs2) :-

    Library1 = lib(Id, _, _, Left, Right) :
      Library2 = lib(Id, Code, Xrefs, Left, Right),
      Errs1 = [redefined(Id) | Errs2] |
	true;

    Library1 = lib(BiggerId, C, X, Library1', Right),
    Id @< BiggerId :
      Library2 = lib(BiggerId, C, X, Library2', Right) |
	insert;

    otherwise,
    Library1 = lib(LesserId, C, X, Left, Library1') :
      Library2 = lib(LesserId, C, X, Left, Library2') |
	insert;

    Library1 = [] :
      Library2 = lib(Id, Code, Xrefs, [], []),
      Errs1 = Errs2 |
	true.


queries(Queries, SysCode, Library) :-

    Queries ? query(Id, Reply) |
	query(Id, Library, SysCode, SysCode', Reply),
	queries;

    Queries = [] : Library = _,
      SysCode = [] |
	true.

queries(_, _, _) :-
    otherwise |
	true.


query(Id, Library, SysCode1, SysCode2, Reply) :-

    Library = lib(Id, Code, Xrefs, _, _) :
      SysCode1 ! procedure(Id, Code, Xrefs),
      Reply = true |
	indirect(Xrefs, SysCode1', SysCode2);

    Library = lib(BiggerId, _, _, Library', _),
    Id @< BiggerId |
	query;

    Library = lib(LesserId, _, _, _, Library'),
    LesserId @< Id |
	query;

    Library = [] : Id = _,
      SysCode1 = SysCode2,
      Reply = false |
	true.

query(_, _, _, _, _) :-
    otherwise |
	true.


indirect(Xrefs, SysCode1, SysCode2) :-

    Xrefs ? Id :
      SysCode1 ! procedure(Id) |
	indirect;

    Xrefs = [] :
      SysCode1 = SysCode2 |
	true.


build_idlib(Ids, IdLib1, IdLib2) :-

    Ids ? Id |
	build_idlib,
	insert(Id, [], [], IdLib1, IdLib1', _, []);

    Ids = [] :
      IdLib1 = IdLib2 |
	true.


remove_duplicates(SysIn, Library, OutLib, SysOut) :-

    SysIn ? Procedure,
    Procedure = procedure(Id, Code, Xrefs) |
	insert(Id, Code, Xrefs, OutLib, OutLib', Errs, []),
        duplicate(Errs, Procedure, SysIn', SysIn', Library, OutLib', SysOut);

    SysIn ? procedure(Id) |
        query(Id, Library, SysIn1, SysIn', Res),
	available(Res, SysIn1, SysIn', Library, OutLib, SysOut).

remove_duplicates([], _, _, []^).


duplicate(Errs, Procedure, SysIn1, SysIn2, Library, OutLib, SysOut) :-

    Errs = [] : SysIn2 = _,
      SysOut ! Procedure |
	remove_duplicates(SysIn1, Library, OutLib, SysOut');

    Errs = [redefined(_)] : Procedure = _, SysIn1 = _ |
	remove_duplicates(SysIn2, Library, OutLib, SysOut).


available(Reply, SysIn1, SysIn2, Library, OutLib, SysOut) :-

    Reply = false : SysIn1 = _ |
	remove_duplicates(SysIn2, Library, OutLib, SysOut);

    Reply = true,
    SysIn1 ? Procedure,
    Procedure = procedure(Id, Code, Xrefs) |
	insert(Id, Code, Xrefs, OutLib, OutLib', Errs, []),
	duplicate(Errs, Procedure, SysIn1', SysIn2, Library, OutLib', SysOut).
