/*
Translate Psi Calculus notation to FCP.

Bill Silverman, May 2000.

Last update by		$Author: bill $
		       	$Date: 2000/06/27 11:01:10 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/PsiFcp/psifcp/topsifcp.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export(translate/4).
-mode(interpret).
-language(compound).

/*
** Translate/4
**
** Translate Psi Calculus module to psifcp.
**
** Input:
**
**   Source      - Psi Calculus code, minus attributes.
**
** Output:
**
**   Psifcp       - Psifcp code.
**   Errors      - Diagnostics in the form:  comment(Argument)
**                 followed by NextErrors.
*/

translate(Source, Psifcp, Errors, NextErrors) :-

    true :
      Psifcp = Terms? |
	filter_psifcp_attributes(Source, Source', Terms, Terms'?),
	processes.

/* Copy Stochastic Psi Calculus Attributes. */

filter_psifcp_attributes(Source, NextSource, Terms, NextTerms) :-

    Source ? String, string(String) :
      Terms ! String |
	self;

    Source ? Tuple, Tuple =\= (_ :- _), Tuple =\= (_ ::= _) :
      Terms ! Tuple |
	self;

    Source = [(_ :- _) | _] :
      NextTerms = _,
      NextSource = [],
      Terms = Source;

    otherwise :
      NextSource = Source,
      Terms = NextTerms.

/* Translate processes. */

processes(Source, Terms, Errors, NextErrors) :-

    Source ? (LHS ::= RHS) :
      Terms ! (LHS :- RHS'?) |
	process_rhs(RHS, RHS', Errors, Errors'?),
	self;

    Source ? Other, Other =\= (_ ::= _) :
      Terms ! Other |
	self;

    Source =?= [] :
      Terms = [],
      Errors = NextErrors.


process_rhs(PC, Psifcp, Errors, NextErrors) :-

    PC =?= (Clause ; PC') :
      Psifcp = (Clause' ; Psifcp') |
	process_rhs(Clause, Clause', Errors, Errors'?),
	self;

    /* Just parallel goals */
    PC =?= (Goal | PC'), PC' =\= (_, _),
    Goal =\= (_ , _) :
      Psifcp = (Goal'?, Psifcp') |
	process_rhs(Goal, Goal', Errors, Errors'?),
	self;

    PC =?= (_ | PC'), PC' =?= (_, _) :
      Psifcp = true,
      Errors = [invalid_body(PC) | NextErrors];

    /* Just compound guard, and single goal */
    PC =?= (_, _) |
	compound_guard(PC, Psifcp'?, PC', Psifcp, Errors, Errors'),
	self;

    /* Possibly compound guard, and at least two parallel goals */
    PC =?= (Head | PC'),
    Head =?= (_, _) :
      PC'' = (Goal? | PC') |
	compound_guard(Head, Psifcp'?, Goal, Psifcp, Errors, Errors'),
	self;

    PC =?= [_ | _] |
	new_scope(PC, Psifcp, Errors, NextErrors);

    PC =?= 0 :
      Psifcp = true,
      Errors = NextErrors;

    otherwise :
      Psifcp = PC,
      Errors = NextErrors.


compound_guard(Guard, Body, Goal, RHS, Errors, NextErrors) :-

    Guard =?= (Predicate, Guard'),
    Predicate =\= (_ | _) :
      RHS = (Predicate | RHS') |
	self;

    Guard =?= (Predicate, _),
    Predicate =?= (_ | _) :
      Goal = 0,
      RHS = Body,
      Errors = [invalid_guard(Predicate) | NextErrors];

    Guard =\= (_, _) :
      Goal = Guard,
      RHS = Body,
      Errors = NextErrors.


new_scope(List, New, Errors, NextErrors) :-

    List =?= [RHS], RHS =\= (_ ::= _) :
      New = [RHS'?] |
	process_rhs(RHS, RHS', Errors, NextErrors);

    List =?= [Body | Processes],
    Body =\= (_ ::= _), Processes =?= [(_ ::= _)| _] :
      New = [Body'? | Processes'?] |
	process_rhs(Body, Body', Errors, Errors'),
	processes(Processes, Processes', Errors', NextErrors);

    List =?= [Channels, Body | Processes],
    Channels =\= (_ ::= _), Body =\= (_ ::= _) :
      New = [Channels, Body'? | Processes'?] |
	process_rhs(Body, Body', Errors, Errors'),
	processes(Processes, Processes', Errors', NextErrors).
