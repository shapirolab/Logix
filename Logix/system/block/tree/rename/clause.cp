/* $Header: /home/qiana/Repository/Logix/system/block/tree/rename/clause.cp,v 1.1 1999/07/09 07:03:13 bill Exp $ */
/*
 *  Split a clause into its component atoms; each of the atoms is
 *  renamed as appropriate.
 */

-export([transform/4]).
-language(compound).
-mode(trust).

/************************* T R A N S F O R M / 4 *****************************/

procedure transform(Clause, Clause, Changes, Changes).

% Transform clauses, renaming their atoms.
% Split a rule-clause into its head, guard and body, and rename the head
% and body.
% Split a fact-clause into its head and guard, and rename the head.

transform(Clause, Transformed, Changes1, Changes2) :-

    Clause = (Head :- RightSide),
    RightSide = (Guard | Body) :
      Transformed = (RenamedHead :- Guard | RenamedBody),
      Changes1 ! head(Head, RenamedHead) |
	body(Body, RenamedBody, Changes1', Changes2);

    Clause = (Head :- Body),
    Body =\= (_ | _),
    Body =\= (_ : _) :
      Transformed = (RenamedHead :- RenamedBody),
      Changes1 ! head(Head, RenamedHead) |
	body(Body, RenamedBody, Changes1', Changes2);

    Clause = (Head :- Guard),
    Guard = (_ : _) :
      Transformed = (RenamedHead :- Guard),
      Changes1 = [head(Head, RenamedHead) |Changes2] ;

    otherwise :						% Clause is a pure fact
      Changes1 = [head(Clause, Transformed) |Changes2] .


% Recursively parse the body, changing body goals.

procedure body(Body, Body, Changes, Changes).

body(Body, RenamedBody, Changes1, Changes2) :-

    Body = (Body', Body'') :				% conjunction
      RenamedBody = (RenamedBody', RenamedBody'') |
	body(Body', RenamedBody', Changes1, Changes1'),
	body;

    Body = true :					% true goal
      RenamedBody = true,
      Changes1 = Changes2 ;

    Body = (_ = _) :	 				% macro goal
      RenamedBody = Body,
      Changes1 = Changes2 ;

    Body = (_ := _) :					% macro goal
      RenamedBody = Body,
      Changes1 = Changes2 ;

    Body = _ # _ :					% RPC
      Changes1 = [call(Body, RenamedBody) | Changes2] ;

    Body = Body' @ Link :				% Remote processor link
      RenamedBody = RenamedBody' @ Link,
      Changes1 = [call(Body', RenamedBody') | Changes2] ;

    otherwise :						% local goal?
      Changes1 = [goal(Body, RenamedBody) | Changes2] .
