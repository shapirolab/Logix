/***************************************************************************
 *									
 *  File: 	sets.cp				
 *  Function:	provides elementary set functionality.	
 *  Author:	Paul A.S. Ward						
 *  Date:	23 September 1993						
 *									
 * $Header: /home/qiana/Repository/Logix/system/Doors/sets.cp,v 1.1.1.1 1999/07/09 07:03:24 bill Exp $ 
 */

-language([dfcp]).
-mode(trust).
-export([create/2,
	 contents/2,
	 add/3, 
	 delete/3, 
	 element/3, 
	 union/3, 
	 intersection/3, 
	 difference/3]).

SET(Set)	=>	list(Set), ground(Set).

create(Data, Set) :-
  Data = [] |
	Set = []

; Data ? Datum, listener(Datum) |
	listen2(Data'?, Data1, Data2),
	element(Datum, Data1?, Result),
	add_if(false, Result?, Datum, Set, Set'?),
	Data'' = Data2?,
	self

; otherwise,
  Data = _ |
	Set = error.

contents(Set, Contents) :-
  Set = [] |
	Contents = []

; list(Set) |
	Contents = Set

; otherwise,
  Set = _ |
	Contents = error.

add(Element, Set, Set1) :-
  listener(Element), listener(Set) |
	element(Element, Set, Result),
	add_if(false, Result?, Element, Set1, Set).

delete(Element, Set, Set1) :-
  Set = [],
  Element = _ |
	Set1 = []

; Set ? Element |
	Set1 = Set'?

; Set ? Diff,
  Diff =\= Element |
	Set1 ! Diff,
	self

; otherwise,
  Set = _, Element = _ |
	Set1 = error.

element(Element, Set, Result) :-
  Set = [],
  Element = _ |
	Result = false

; Set ? Element, 
  Set' = _ |
	Result = true

; Set ? Diff,
  Diff =\= Element |
	self

; otherwise,
  Set = _, Element = _ |
	Result = error.

union(Set1, Set2, Union) :-
  Set1 = [] |
	Union = Set2

; Set1 ? Element, listener(Element), listener(Set2) |
	element(Element, Set2, Result),
	add_if(false, Result?, Element, Union, Union'?),
	Set2' = Set2,
	self

; otherwise,
  Set1 = _, Set2 = _ |
	Union = error.

intersection(Set1, Set2, Intersection) :-
  Set1 = [],
  Set2 = _ |
	Intersection = []

; Set1 ? Element, listener(Element), listener(Set2) |
	element(Element, Set2, Result),
	add_if(true, Result?, Element, Intersection, Intersection'?),
	Set2' = Set2,
	self

; otherwise,
  Set1 = _, Set2 = _ |
	Intersection = error.

difference(Set1, Set2, Difference) :-		%  Difference = Set1 - Set2
  Set1 = [],
  Set2 = _ |
	Difference = [] 

; Set1 ? Element, listener(Element), listener(Set2) |
	element(Element, Set2, Result),
	add_if(false, Result?, Element, Difference, Difference'?),
	Set2' = Set2,
	self

; otherwise,
  Set1 = _, Set2 = _ |
	Difference = error.

/*
 *	Support Routines:
 *
 *	add_if/5 	will add an element to a partial set if the 
 *		 	Result is equal to the Test.
 */

add_if(Test, Result, Element, Set, Set1) :-
  Result = Test |
	Set =[ Element | Set1]

; Result =\= Test,
  Element = _ |
	Set = Set1?.

