
/*
Precompiler for FCP

Michael Hirsch,  27 January 1985
William Silverman 08/85

Last update by		$Author: bill $
		       	$Date: 2004/06/22 12:02:24 $
Currently locked by 	$Locker:  $
			$Revision: 1.5 $
			$Source: /home/qiana/Repository/Logix/system/compile/precompile/self.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([deadcode/2, library/3, transform/8]).
-mode(trust).
-language(compound).

/*
** Forward  Procedures  to graph to produce a list of dead  Identifiers .
*/

procedure deadcode([procedure(Id, Ids)], Ids).

Ids ::= [Id].
Id ::= String/Integer.

deadcode(Procedures, Ids) :-
	graph # lists(Procedures, _, _, Ids).

List ::= [Any].

procedure transform(List, List, Mode, Ids, [Import], [Procedure], List,
				Target
).

Mode ::= boot ; trust ; failsafe ; interrupt ; interpret ; system ; user.
Import ::= String ; link(String).
Procedure ::= {Predicate, {[Predicate], [Predicate]}, [Predicate]}.
Predicate ::= String ; Tuple.
Target ::= wam ; dg.

/*
** Transform  Attributes  and other  Terms  of a module, producing:
**
**    Exported    - list of exported procedure identifiers
**    Importers   - list of procedure ids of all potential importers
**    PreCompiled - list of precompiled procedures, suitable for encoding, etc.
**    Output      - list of comments/diagnostics.
**
*/

transform(Attributes, Terms, Mode, Exported, Importers, PreCompiled, Output,
		Target
) :-

	module_header(	Attributes, Terms, Imported, LPIds, ProcedureIds,
			Mode, Entries, Exported, RestTerms, 
			{DiagD, DiagM, DiagU}, Output, PreCodeErrs
	),
	code # macro(	RestTerms, DicReqs, 
			ExpandedProgram, SysCode'?,
			PreCodeErrs, UndefErrs?,
			Target
	),
	dummy_dic_reqs(Mode, Entries, DicReqs, DicReqs1),
	graph # lists(DicReqs1, _, _, UnKnown),
	make_queries(UnKnown, Queries),
	local_procedure_identifiers(DicReqs, LPIds),
	computation # library(query, Queries, LPIds, SysCode),
	dg_library(Target, SysCode?, SysCode'),
	combine(ExpandedProgram, UnCode, Program, Idx, ProcedureIds,
		Multiples
	),
	filter_dead(DiagD, Entries, Program, Program', DeadCode, Dead),
	dummy_dic_reqs(Mode, Entries, Idx, NewDicReqs),
	imports(Mode, NewDicReqs, ProcedureIds, Importers),
	dummy_dic_reqs(Mode, Entries, Idx, Idx1),
	graph # lists(Idx1, _, DeadCode, Undefined),
	undef_code(Undefined, UnCode),
	prepare_indices(Mode, Program', PreCompiled, Imported),
	definition_errors(Entries, Dead, DiagM, Multiples, CodeDefErrs, []),
	wrap_errors(DiagU, Undefined, procedure_undefined, UndefErrs,
			CodeDefErrs
	).

filter_dead(DiagD, Entries, Program, Undead, DeadCode, Dead) :-

    DiagD = off, Entries =\= all,
    DeadCode =\= [],
    Program ? Procedure |
	filter_dead_proc(Procedure, Undead, Undead', DeadCode, DeadCode'),
	filter_dead;

    otherwise : DiagD = _, Entries = _,
      Program = Undead,
      DeadCode = Dead .

filter_dead_proc(P, U1, U2, D1, D2) :-

    P = _(ID, _),
    D1 ? ID :
      U1 = U2,
      D1' = D2 ;

    P = _(ID, _),
    D1 ? D,
    ID =\= D :
      D2 ! D |
	filter_dead_proc;

    D1 = [] :
      U1 = [P | U2],
      D1 = D2 .

dummy_dic_reqs(Mode, Entries, DicReqs1, DicReqs2) :-

    Mode =\= boot,
    Entries =\= all :
      DicReqs2 = [procedure('@' / 2, ['#'/2]), procedure('#' / 2, Entries)
	  	 | DicReqs1] ;

    Mode = boot : Entries = _,
      DicReqs2 = [procedure(external/none, [boot/2]) | DicReqs1] ;

    Entries = all : Mode = _,
      DicReqs2 = [procedure('@' / 2, []), procedure('#' / 2, []) | DicReqs1] .


local_procedure_identifiers(DicReqs, Ids) :-

    DicReqs ? _(Id, _) :
      Ids ! Id |
	local_procedure_identifiers;

    DicReqs = [] :
      Ids = [] .


dg_library(Target, SysCode1, SysCode2) :-

    Target =\= wam,
    SysCode1 ? procedure(Id, Clauses, XRef) :
      SysCode2 ! procedure(Id, Clauses', XRef) |
	dg_library,
	dg_clauses(Clauses, Clauses');

    otherwise : Target = _,
      SysCode2 = SysCode1 .

dg_clauses(In, Out) :-

    In ? {Head, {Ask, Tell}, Body} :
      Out ! {Head, {Ask', Tell}, Body} |
	dg_clauses,
	dg_ask(Ask, Ask') ;

    otherwise :
      In = Out .

dg_ask(Ask1, Ask2) :-

    Ask1 ? (A =?= B) :
      Ask2 ! (A = B) |
	dg_ask;

    Ask1 ? Other, Other =\= (_ =?= _) :
      Ask2 ! Other |
	dg_ask;

    Ask1 = [] :
      Ask2 = [] .


undef_code(Ids, ProcIds) :-

    Ids ? Id :
      ProcIds ! procedure(Id, []) |
	undef_code;

    Ids = [] :
      ProcIds = [] .


procedure library(List, List, Target).

library(Terms, PrecodeErrs, Target) :-
	computation # library(build, NewProcedures, LibErrors),
	code # macro(Terms, _, NewProcedures, [], PrecodeErrs, LibErrors,
			Target
		).


prepare_indices(Mode, Program1, Program2, Imported) :-

    Mode =\= interpret, Mode =\= boot |
	rpc # index(Program1, Program2, Imported);

    otherwise : Mode = _,
      Program1 = Program2,
      Imported = [] .


module_header(	Attributes, Terms, Imported, LPIds, Ids, 
		Mode, Entries, Exported, RestTerms, 
		Diagnostics, Output, Output1
) :-
	first_clause(Terms, Type),
	module_kind(	Type, Terms,
			{Attributes, Imported, LPIds, Ids}, 
			Mode, Entries, Exported, RestTerms, 
			Diagnostics, Output, Output1
	).

first_clause(Terms, Type) :-

    Terms = [Clause | _], 
    Clause =\= (boot(_, _) :- _),
    Clause =\= boot(_, _) :
      Type = normal ;

    Terms = [] :		% attributes only
      Type = normal ;

    otherwise : Terms = _,	% boot/2 ==> boot
      Type = boot .


module_kind(boot, Terms, _, boot^, []^, []^, Terms^,
		{on, on, on}^, [comment(system) | Output]^, Output
).
module_kind(	normal, Terms, {Attributes, Imported, LPIds, Ids}, 
		Mode1,
		Exports2,
		Exported, 
		Terms1,
		{DiagD, DiagM, DiagU},
		[comment((Mode1 : External)) | Output]^, Output2
) :-
	edit_attributes(Attributes,
			[mode(Mode), export(Exports), import(_), monitor(Name),
			 entries(Entries),
			 deadcode(DiagD), multiples(DiagM), undefined(DiagU)], 
			Edited, Done
	),
	edit_attributes(Attributes, [language(Languages)], _Edited, Done1),
/* defaults for  exports, deadcode, multiples, undefined, entries, language */
	unify_without_failure({Done?, Exports}, done(all)),
	unify_without_failure({Done?, DiagD}, done(off)),
	unify_without_failure({Done?, DiagM}, done(on)),
	unify_without_failure({Done?, DiagU}, done(on)),
	unify_without_failure({Done?, Entries}, done([])),
	unify_without_failure({Done1?, Languages}, done([])),
/***************** all ***** off ****** on ******* on ***** [] ***** [] *****/
	verify_name_mode(Done, Name, Languages, Mode, Name1, Mode1, 
			Edited1, Edited?, Output, Output1
	),
	verify_export(  Name1, Exports, Entries, LPIds, Ids,
			Exports1, Exports2, Exported, 
			Output1, Output2
	),
	verify_monitor(	Name1,
			[ mode(Mode1?),
			  export(Exported?),
			  import(Imported?)
			| Edited1?],
			Terms, Terms1
	),
	external(Exports1?, Imported?, External).

external(Exports, Imports, External) :-

    Exports = [], Imports = [] :
      External = none ;

    Exports =\= [], Imports = [] :
      External = export(Exports) ;

    Exports = [], Imports =\= [] :
      External = import(Imports) ;

    Exports =\= [], Imports =\= [] :
      External = (export(Exports), import(Imports)) .

edit_attributes(Attributes, Edit, Edited, Done) :-
    Attributes ? Attribute |
	edit_attribute(Attribute, Edit, Edited, Edited',
			Attributes', Attributes''
	),
	edit_attributes;

    otherwise : Attributes = _, Edit = _,
      Edited = [],
      Done = done .

edit_attribute(Attribute, Edit, Edited1, Edited2, Attributes1, Attributes2) :-

    Edit ? Attribute^ : Edit' = _,
      Edited1 = Edited2,
      Attributes1 = Attributes2 ;

    otherwise,
    Edit ? _ |
	edit_attribute;

    Edit = [] :
      Edited1 = [Attribute | Edited2],
      Attributes1 = Attributes2 .


verify_name_mode(Done, Name, Languages, Mode, Name1, Mode1, Edited1, Edited2,
			Output1, Output2
) :-

    Done = done :
      Name = [],
      Name1 = [], Edited1 = Edited2 |
	choose_mode([biospi(interrupt)], interpret, Languages, Mode),
	verify_module_mode(Mode, Mode1, Output1, Output2);

    Done = done,
    string(Name) :
      Languages = _,
      Name = Name1,
      Edited1 = [monitor | Edited2] |
	unify_without_failure(Mode, user),
	verify_monitor_mode(Mode, Mode1, Output1, Output2);

    otherwise : Done = _, Languages = _,
      Name1 = [],
      Mode1 = interrupt,
      Edited1 = Edited2,
      Output1 ! invalid_monitor_name(Name) |
	verify_monitor_mode(Mode, _, Output1', Output2).

  choose_mode(Choices, Default, Languages, Mode) :-

    known(Mode) :
      Choices = _,
      Default = _,
      Languages = _;

    unknown(Mode),
    Choices ? Name(NameMode) |
	languages_name_mode(Languages, Name, NameMode, Mode, Mode'),
	self;

    Choices = [] :
      Languages = _,
      Mode = Default.

  languages_name_mode(Languages, Name, NameMode, Mode, NewMode) :-

    Languages ? Name :
      Languages' = _,
      NewMode = _,
      Mode = NameMode;

    Languages ? Other,
    Other =\= Name |
	self;

    Languages = [] :
      Name = _,
      NameMode = _,
      NewMode = Mode.

verify_module_mode(interpret, interpret^, O, O^).
verify_module_mode(interrupt, interrupt^, O^, O).
verify_module_mode(failsafe, failsafe^, O^, O).
verify_module_mode(trust, trust^, O^, O).
verify_module_mode(Other, interrupt^, [invalid_module_mode(Other) | O]^, O) :-
    otherwise |
	true.


verify_monitor_mode(user, user^, O^, O).
verify_monitor_mode(system, system^, O^, O).
verify_monitor_mode(Other, user^, [invalid_monitor_mode(Other) | O]^, O) :-
    otherwise |
	true.


verify_monitor(Name, Attributes, Terms, Module) :-

    Name = [] :					% Not a monitor
      Module =	[(attributes(`"ATTRIBUTES") :-
		     true :
			`"ATTRIBUTES" = Attributes)
		| Terms ] ;

    string(Name) :
      Module =	[('_boot'(`'IN', `'ATTRIBUTES'):-
			true : `'ATTRIBUTES' = Attributes |
			Name(?'IN')
		 )
	 	| Terms ] .


verify_export(Name, Exports, Entries, LPIds, Ids, Exports1, Exports2, Exported, 
			Output1, Output2
) :-
    Name = [], Exports = all : LPIds = _,
      Entries = _,
      Ids = Exported,
      Exports1 = all,
      Exports2 = all,
      Output1 = Output2 ;

    string(Name) : LPIds = _,
      Name' = [],
      Ids = _, Ids' = [],
      Exports1 = monitor(Name), Exports1' = _,
      Exports2 = ['_boot'/2], Exports2' = _ |
	verify_export;

    otherwise : Name = _, Ids = _,
      Exported = [attributes/1 | Exports1],
      Exports2 = Exported |
	parse_exports(Exports, Entries, LPIds , Exports1, Output1, Output2).


parse_exports(Exports, Entries, LPIds, ExportList, Output1, Output2) :-

    Exports ? Id,
    Id = F/A, string(F), A >=0 :
      ExportList ! Id |
	parse_exports;

    Exports ? Name,
    string(Name) |
	implicit_export(Name, LPIds, Exports'', Exports'),
	self;

    otherwise,
    Exports ? BadId :
      Output1 ! export_syntax_error(BadId) |
	self;

    Exports = [], Entries =\= [] :
      Exports' = Entries,
      Entries' = [] |
	self;

    Exports = [], Entries = [] : LPIds = _,
      ExportList = [],
      Output1 = Output2 ;

    Exports =\= [_|_], Exports =\= [] :
      Exports' = [Exports] |		% not a list (tail)
	self.

implicit_export(Name, LPIds, Exports1, Exports2) + (Found = false) :-

    LPIds ? Id, Id = Name/_ : Found = _,
      Exports1 ! Id,
      Found' = true |
	self;

    LPIds ? Id, Id =\= Name/_ |
	self;

    LPIds = [], Found = true : Name = _,
      Exports1 = Exports2;

    LPIds = [], Found = false :
      Exports1 = [Name/0 | Exports2].


make_queries([], []^).
make_queries(Ids, Queries) :-
    Ids ? Id :
      Queries ! query(Id, _) |
	make_queries.


definition_errors(Entries, DeadCode, DiagM, Multiples, Output1, Output2) :-

    Entries = all : DeadCode = _ |
	wrap_errors(DiagM, Multiples, multiply_defined, Output1, Output2);

    Entries =\= all |
	wrap_errors(DiagM, Multiples, multiply_defined, Output1, Output1'),
	wrap_errors(on, DeadCode, dead_code, Output1', Output2, ":").


wrap_errors(On, Errors, Diagnostic, Output1, Output2) + (Infix = "-") :-

    On =\= off,
    Errors ? Error :
      Output1 ! {Infix, Diagnostic, Error} |
	wrap_errors;

    Errors = [] : On = _, Diagnostic = _, Infix = _,
      Output1 = Output2 ;

    On = off : Errors = _, Diagnostic = _, Infix = _,
      Output1 = Output2 .


imports(boot, _, _, []^).
imports(Mode, DicReqs, ProcedureIds, Importers) :-
    Mode =\= boot |
	graph # imports(DicReqs, ImportingIds),
	utils # binary_sort_merge(ProcedureIds, SortedProcedureIds),
	utils # binary_sort_merge(ImportingIds, SortedImportingIds),
	importers(SortedProcedureIds, SortedImportingIds, Importers).

importers(ProcedureIds, ImportingIds, Importers) :-

    ProcedureIds ? Id1,
    ImportingIds = [Id2 | _],
    Id1 @< Id2 |
	importers;

    ProcedureIds = [Id1 | _],
    ImportingIds ? Id2,
    Id2 @< Id1 |
	importers;

    ProcedureIds ? Id,
    ImportingIds ? Id :
      Importers ! Id |
	importers.

importers([], _, []^).
importers(_, [], []^).
	

% ***** Revise to use tree *****

combine(ExpandedProgram, UnCode, Program, Idx, ProcedureIds, Multiples) :-
    ExpandedProgram ? Procedure(Id, Code, Xrefs) :
      Program ! Procedure(Id, Code),
      Idx ! Procedure(Id, Xrefs),
      ProcedureIds ! Id |
	remove_multiples(Id, ExpandedProgram', ExpandedProgram'',
				Multiples, Multiples'
	),
	combine.
combine([], UnCode, UnCode^, []^, []^, []^).


remove_multiples(Id, ExpandedProgram1, ExpandedProgram2,
			Multiples1, Multiples2
) :-

    ExpandedProgram1 ? _(Id, _,_) :
      Multiples1 ! Id |
	remove_multiples;

    otherwise,
    ExpandedProgram1 ? Procedure :
      ExpandedProgram2 ! Procedure |
	remove_multiples.

remove_multiples(_, [], []^, Multiples^, Multiples).
