/* $Header: /home/qiana/Repository/Logix/system/block/tree/parser.cp,v 1.2 2002/05/29 08:04:03 bill Exp $ */
/*
 *  Get and parse a module, rejecting library, monitor (except root-self),
 *  empty module and module with parsing-errors.
 */

-export([parse/6]).
-language(compound).
-mode(trust).

/******* PARSE ***************************************************************/

Parsed ::= {Attributes, Clauses}.

procedure parse(ServiceId, Clauses, ProcIds, Attributes, Result).

% Get and parse module  ServiceId , collecting procedure names and attributes.
%
% Clauses  is a list of parsed terms of the module, except for the attributes.
% ProcIds  is a list of the procedure identifiers of the module.
% Attributes  is a list of the attributes of the module.
% Out  is  "included", or  "excluded" - Reason , where  Reason  is one of:
%   [library, monitor, empty, error(not_found), no_service]
%   or parsing_errors(ParsingErrors)
%   or transformation_errors(TransformationErrors).
% Clauses, ProcIds and Attributes are non-empty iff  "included".

parse(ServiceId, Lead, Clauses, ProcIds, Attributes, Out) :-
    ServiceId ? Name |
		get_source # context(ServiceId', Name, [], Result,
				  {ParsingErrorsStream, []}
			     ),
		interpret(Result, ServiceId, Lead, ParsingErrorsStream,
			  Parsed, Out),
		arg(1, Parsed, Attributes),
		arg(2, Parsed, Clauses),
		get_ids(Clauses, ProcIds).

/************************* GET_IDS *******************************************/
	
procedure get_ids(Terms, ProcIds).
procedure get_ids(Terms, ProcIds, ProcId).

% Collect in ProcIds a list of identifiers, of the procedures
% that appear in Terms. The Previous-argument is used to omit
% (consecutively appearing) duplicates.

get_ids(Terms, ProcIds) + (Previous = ''/(-1)) :-

	Terms ? (Head :- _),				% Term is a rule,
	arg(1, Head, ProcName),
	arity(Head, N1),
	N := N1 - 1,
	ProcName/N = Previous |				% same as Previous Id
		get_ids;

	Terms ? Term,					% Term is a fact,
	arg(1, Term, ProcName),
	arity(Term, N1),
	N := N1 - 1,
	ProcName/N = Previous |				% same as Previous Id
		get_ids;

	Terms ? (Head :- _),				% Term is a rule,
	arg(1, Head, ProcName),
	N := arity(Head) - 1,
	ProcName/N =\= Previous :
	    Previous' = ProcName/N,
	    ProcIds ! Previous' |				% New Id
		get_ids(Terms', ProcIds', Previous');

	Terms ? Term,					% Term is a fact,
	arg(1, Term, ProcName),
	N := arity(Term) -1,
	Previous =\= ProcName/N,
	Term =\= (_ :- _),
	Term =\= (_ ::= _),
	Term =\= procedure(_) :
	    Previous' = ProcName/N,
	    ProcIds ! Previous' |				% New Id
		get_ids(Terms', ProcIds', Previous');

	Terms ? (Head :- _),				% Term is a rule,
	string(Head),
	Previous =\= Head/0 :
	    Previous' = Head/0,
	    ProcIds ! Previous' |				% New Id
		get_ids;

	Terms ? Term,					% Term is a fact,
	string(Term),
	Previous =\= Term/0 :
	    Previous' = Term/0,
	    ProcIds ! Previous' |				% New Id
		get_ids;

	Terms ? Term,					% Term is a fact,
	Term/0 = Previous |				% same as Previous Id
		get_ids;

	Terms ? (Head :- _),				% Term is a rule,
	Head/0 = Previous |				% same as Previous Id
		get_ids;

	otherwise,					% Term is a type- 
	Terms ? _Term					%  definition, a
	|						%  proceduredeclaration
		get_ids;				%  or something else

	Terms = [] :
	    Previous = _,
	    Terms = _,
	    ProcIds = [] .


/******* INTERPRET ***********************************************************/

procedure interpret(get_source#Result, ServiceId, Lead, ParsingErrors, Parsed,
			Result
).

% Interpret the results of get_source (Result  and ParsingErrorsStream).
% Parsed is of the form:
%   {ParsedAttributesList, ParsedClausesList}. 
%   Out is instantiated to one of the values described in the 
% specification of procedure parse.

interpret(Result, ServiceId, Lead, ParsingErrorsStream, Parsed, Out) :-

	Result = library(_, _, _) : ServiceId = _, ParsingErrorsStream = _,
            Lead = _,
	    Parsed = {[], []},
	    Out = excluded - library ;

	Result = false(Reason) : ServiceId = _, ParsingErrorsStream = _,
            Lead = _,
	    Parsed = {[], []},
	    Out = excluded - Reason ;

	Result = module(_, Attributes, Clauses),
	ParsingErrorsStream = [] |

		transform # languages([block_prefix(Lead)], typed,
				      [service_id(ServiceId) 
				      | Attributes], Attributes',
				      Clauses, Clauses', Errors, []
		),
		transformation_errors({Attributes', Clauses'}, Parsed,
				      Errors, Out
		);

	otherwise : Result = _, ServiceId = _, Lead = _, % Parsing-errors
	    Parsed = {[], []},
	    Out = parsing_error(ParsingErrorsStream) .

procedure transformation_errors(Parsed, Parsed, Errors, Result).

transformation_errors(P1, P2, Errors, Out) :-

    Errors = [] :
      P1 = P2,
      Out = included ;

    otherwise : P1 = _,
      P2 = {[], []},
      Out = transformation_error(Errors) .

/******* (END  OF  MODULE) ***************************************************/
