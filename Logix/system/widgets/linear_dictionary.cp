/*

Linear Dictionary Monitor

Bill Silverman - 06/89

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:23 $
Currently locked by 	$Locker:  $
			$Revision: 1.1.1.1 $
			$Source: /home/qiana/Repository/Logix/system/widgets/linear_dictionary.cp,v $

Copyright (C) 1989, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language(compound).
-export([requests/2]).
-mode(trust).

Requests ::= [(add(Any, Any) ; add(Any, Any, Reply))].
Reply ::= old ; new.
Dictionary ::= [Entry].
Entry ::= {Any, Any}.

procedure requests(Requests, Dictionary).

/*
	A linear dictionary server :

	In:	requests : add(Name, Value), add(Name, Value, Reply).

	Out:	dictionary is a difference-list; new entries are added to
		the tail.
*/

procedure requests(Requests, Dictionary, Dictionary, Dictionary).

requests(In, Out) + (Head = List, Tail = List) :-

    In ? add(Name, Value) |
	lookup(Name, Value, Head, Tail, Tail', _),
	requests;

    In ? add(Name, Value, Reply) |
	lookup(Name, Value, Head, Tail, Tail', Reply),
	requests;

    In = [] :
      Tail = [],
      Head = Out |
	true.

OpenDictionary ::= [Entry | Dictionary].

procedure lookup(Any, Any, OpenDictionary, OpenDictionary, Dictionary, Reply).

lookup(Name, Value, Head, Tail1, Tail2, Reply) :-

    Head = [Name(Value^) | _] :
      Tail1 = Tail2,
      Reply = old |
	true;

    Head ? _,
    otherwise |
	lookup;

    Head = Tail1 :
      Tail1 = [Name(Value) | Tail2],
      Reply = new |
	true.
