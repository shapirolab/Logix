/*

Precompiler for FCP - add an extra stream to procedures which need it
	Tailored for Hierarchical System.
Michael Hirsch,  27 January 1985
Bill Silverman, 5 September 1985

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:35 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/system/compile/precompile/rpc.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([index/3]).
-mode(trust).
-language(compound).

/*

  Index transforms Remote Procedure Calls  Target#Goal  into:

	distribute # {Integer, Goal}	Target is a string
	transmit # {Target, Goal}	Target is not a string

  Index transforms Distributed Processor Calls  Goal@Target  into:

	distribute # {Integer, Goal}	Target is a string
	link # {Target, Goal}		Target is not a string

  where  Integer  is an index to the modules export list.

  Input:  Program = [procedure(Functor/Arguments,[Clause, ...]), ...]
  Output: NewProgram = [procedure(Functor/Arguments,[NewClause, ...]), ...]
  Output: Exports = [ExternalId, ...]		- external references

  Format: Clause = {H,G,B}
  Format: Identifier = ServiceName
  Format: ExternalId = ServiceName or link(LinkName)
*/

Program ::= [Procedures].
Procedure ::= procedure(GoalId, [Clauses]).
GoalId ::= String/Integer.
Clause ::= {Predicate, [Any], [Predicate]}.
Predicate ::= String ; Tuple.
External ::= String ; link(String).

procedure index(Program, Program, [External]).

index(Program, NewProgram, Exports) :-
	procedures(Program, NewProgram, Imports),
	index_imports(Imports, [], Exports).

index_imports(Imports, Identifiers, Exports) :-

    Imports ? import(Identifier, Index) |
	index_imports,
	index_import(Identifier, Identifiers, Identifiers', 1, Index);

    Imports = [] :
      Identifiers = Exports.

index_import(Identifier, Identifiers1, Identifiers2, Counter, Index) :-

    Identifiers1 ? Identifier : Identifiers1' = _,
      Identifiers2 = Identifiers1,
      Index = Counter ;

    Identifiers1 ? Other, Identifier =\= Other,
    Counter' := Counter + 1 :
      Identifiers2 ! Other |
	index_import;

    Identifiers1 = [] :
      Identifiers2 = [Identifier],
      Index = Counter .


procedures(Program, NewProgram, Imports) :-
    Program ? procedure(Ident, Clauses) : 
      NewProgram ! procedure(Ident, NewClauses?) |
	procedures,
	clauses(Clauses, NewClauses, Imports, Imports').

procedures([], []^, []^).


clauses(Clauses, NewClauses, Imports1, Imports2) :-

    Clauses ? {H, G, B} :
      NewClauses ! {H, G, B'?} |
	clauses,
	rewrite_body(B, B', Imports1, Imports1');

    Clauses = [] :
      Imports1 = Imports2,
      NewClauses = [] |
	true.


rewrite_body(Body, NewBody, Imports1, Imports2) :-

    Body ? Goal :
      NewBody ! Goal'? |
	rewrite_body,
	rewrite_goal(Goal, Goal', Imports1, Imports1');

    Body = [] :
      NewBody = [],
      Imports1 = Imports2 |
	true.


rewrite_goal((Name # Goal), RPC, Im1, Im2) :-
	remote_procedure_call(Name, Goal, RPC, Im1, Im2).
rewrite_goal((Goal @ Link), RPC, Im1, Im2) :-
	remote_procedure_spawn(Link, Goal, RPC, Im1, Im2).
rewrite_goal(Goal, Goal^, Im, Im^) :-
    otherwise |
	true.


remote_procedure_call(	Name, Goal, 
			(distribute # {Index?, Goal})^, 
			[import(Name, Index) | Im]^, Im
) :-
    string(Name) |
	true.
remote_procedure_call(	Service, Goal,
			(transmit # {Service, Goal})^,
			Im^, Im
) :-
    otherwise |
	true.


remote_procedure_spawn(	Link, Goal, 
			(distribute # {Index?, Goal})^,
			[import(link(Link), Index) | Im]^, Im
) :-
    string(Link) |
	true.
remote_procedure_spawn(Link, Goal, (link # {Link, Goal})^, Im^, Im) :-
    otherwise |
	true.

