/* $Header: /home/qiana/Repository/Logix/system/block/auxils.cp,v 1.1 1999/07/09 07:03:11 bill Exp $ */
/*
 *  This module contains all auxiliary procedures that are used by the
 *  block-compiler.
 */

-export([append_strings/3, member/3, server_lead/3]).
-language(compound).
-mode(trust).


procedure member(Any, [Any], Answer).

member(Item, ItemList, Answer) :-

    ItemList ? NotItem,
    NotItem =\= Item |
	member;

    ItemList ? Item',
    known(Item')  : ItemList' = _,
      Item = Item',				% allow pattern search!
      Answer = true ;

    ItemList = [] : Item = _,
      Answer = false .


procedure append_strings(String, String, String).

append_strings(String1, String2, String12) :-
    string_to_dlist(String1, List1, List2),
    string_to_dlist(String2, List2, []) |
	list_to_string(List1, String12).


procedure server_lead(String, String, String).

server_lead(Lead, Server, SLead) :-

    Server = self :
      Lead = SLead ;

    otherwise,
    string_to_dlist(Lead, LL, SL),
    string_to_dlist(Server, SL, [Dollar]) :
      ascii('$', Dollar) |
	list_to_string(LL, SLead).
