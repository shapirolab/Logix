/* $Header: /home/qiana/Repository/Logix/system/lint/self.cp,v 1.1 1999/07/09 07:03:11 bill Exp $ */
/*
 *
 * lint: an FCP verification program.
 *
 * This module exports the primary lint call. It calls  main which
 * handles the outer definition of the language. It checks for legality
 * of the clauses and checks for correctness and style of the program.
 *
 * Module  dict  maintains the dictionary(s).
 * Module  cross provides modulewide and procedurewide analysis, called
 * by   dict .
 * Module  defs  called through  dict, checks for compatibility of
 * arguments in calls to predefined procedures with the expected data
 * types, and checks for legal call to those procedures.
 *
 * The dictionary handler,  dict , manages the data-base, serving messages:
 *
 *	ident(Head, Id, Gi_options)
 *				% Id = Proc/Arity-Counter. No folders used here
 *	add_proc(Type, P/A,Part, Call, Properties, Proccheck)
 *				% declares the proc P/A by type Type
 *				% use(Folder) | declare(Folder).
 *	add_dict(X, Part)	%
 *	add_var(X, var/ro, Part)% to the current procedure Id
 *	options(Options, Gi_options)	% Options to pass to  cross .
 *					% Must be the last message.
 *
 * Types of data in send_dict:
 *
 *	declare(Folder) 
 *	use(Folder)
 *
 * where Folder is a name used for consistency. A proc declared under one
 * folder is undefined under other folders.
 * We use folder 'guardp' for guards.
 * For bodies and heads we use folder 'user'.
 * Folder 'external' is used for imported (Module#ProcedureCall).
 *
 * Written by Eli Biham during the summer 1985.
 *
 */

-export([lint/1, lint/2, lint1/3]).
-mode(trust).
-language(compound).

procedure lint(Source).

procedure lint(Source, (String ; [Option])).

Source ::= String ; context(Path, String).

%Option ::= 

/*
 *
 * lint/1 & lint/2 are the exported procedures of lint. They receive
 * the  Source  and  (lint/2)  Options . Source may be a  File  name or
 * context(Path, File); File  is a source file name.
 *
 * The source is "linted"  according to the given options, or to the default
 * options when lint/1 is used.
 *
 *
 * Implementation:
 * lint/1  calls  lint/2  with the default options.
 * lint/2  calls  lint/3  to get the source to be linted.
 * lint/3  calls  get_source # context/5  to get the parsed attributes and
 * terms.
 * lint1/3  checks if the source was found, and if not closes the output.
 * If found, the options list is compiled, the extra options, attributes and
 * terms are transformed by  transform # languages/8 , and  main # first/6
 * is called with the resulting attributes and terms, and a stream to the
 * dictionary from  dict # dict/3 .
 *
 */


lint(Source) + (Options = query) :-

    string(Source) |
	computation # self # service_id(SId),
	complete_options(Options, Options'),
	lint(computation, Source, [service_id([Source | SId]) | Options']);

    Source = context(Path, Name) |
	complete_options(Options, Options'),
	lint(Path, Name, [service_id([Name | Path]) | Options']).


complete_options(Options, Options') :-

    string(Options) :
      Options' = [Options, deadcode, undef, varoncenusc, outer, callcheck] ;

    otherwise :
      Options' = Options .


lint(Path, Name, Options) :-
	get_source # context(Path, Name, Options, Result, {Output, Output1}),
	lint1(Result, Output, Output1).


lint1(Result, Output, Output1) :- 

    Result = Kind(Options, Attributes, Terms) |
	translate_options(Options,
			  [Constref, Varsref, Procref, Undef, 
			   Deadcode, Casecheck, Var_once_gi, Proclist,
			   Outer, Callcheck, Imported, Prccasechk,
			   Prcvaronce],
			  Default,
			  Extra
	),
	transform # languages(Extra, typed, Attributes, Attributes1,
				 Terms, Terms1, Output1, []
		    ),
	computation # Messages,
	output_diagnostics(Output, Messages, Default, Kind, Kind1),
	main # first(Kind1, Attributes1, Terms1,
		     Dict_in\[options([Constref, Varsref, Procref,
		   		       Undef, Deadcode, Casecheck, Proclist,
				       Imported],
				{[Prccasechk, Prcvaronce], [Var_once_gi]}
			      )
			     ],
		     [Outer, Callcheck],
		     {[Prccasechk, Prcvaronce], [Var_once_gi]}
	       ),
	create_flags([Var_once_gi, Callcheck], Cvi_flags, dummy),
	create_flags([Callcheck], Cpi_flags, real_dummy),
	create_flags([Prccasechk, Prcvaronce], Pvi_flags, dummy),
	create_flags([Prccasechk], Pci_flags, dummy),
	create_flags([Casecheck, Varsref],  Mvr_flag, real_dummy),
	create_flags([Casecheck, Constref], Mcr_flag, real_dummy),
	create_flags([Procref, Undef, Deadcode, Imported],  Mpr_flag,
							    real_dummy),
	start_dict(Kind1,
		   {Cvi_flags, real_dummy, Cpi_flags,
		    Pvi_flags, Pci_flags, real_dummy,
		    Mvr_flag, Mcr_flag, Mpr_flag},
		   Dict_in
	       );

    Result = false(Remark) : Output = _,
      Output1 = [] |
	computation # event(Remark).

start_dict(Kind, A, B) :-

    Kind =\= none |
	dict # dict(A, B);

    Kind = none : A = _, B = _ .


output_diagnostics(Input, Output, Default, Kind, Kind1) + (Ok = y) :-

    Input ? Comment, Comment = comment(_) :
      Output ! Comment |
	self;

    Input ? Diagnostic, Diagnostic = diagnostic(_) : Ok = _,
      Ok' = false,
      Output ! Diagnostic |
	self;

    Input ? Other, Other =\= comment(_), Other =\= diagnostic(_) : Ok = _,
      Ok' = false,
      Output ! diagnostic(Other) |
	self;

    Input = [],
    Ok = y : Default = _,
      Output = [],
      Kind1 = Kind;

    Input = [],
    Ok = false,
    Default = query |
	processor # machine(idle_wait(Idle)),	% A little kluge
	computation # display(ask, proceed - "y/n", Ok',
			      [wait(Idle?), read(char),
			       prefix(preprocessing_error)]),
	self;

    Input = [],
    otherwise : Ok = _, Default = _, Kind = _,
      Output = [],
      Kind1 = none .

/*
**
** translate_options/5 translates the lint options list to an internal form.
** The Default option is "query"/"auto", specifying whether or not to
** ask to proceed after precompilation errors ("auto" implies quit).
**
** options/3  provides option translation into an internal list.
**
*/

translate_options(Options, Modes, Default, Extra) + (Ok = true) :-

    Options ? auto :
      Default = auto, Default' = _ |
	translate_options;

    Options ? query :
      Default = query, Default = _ |
	translate_options;

    Ok = true,
    Options ? Op,
    Op =\= auto, Op =\= query |
	options(Op, Modes, Ok'),
	translate_options;

    Ok = true,
    Options = [] :
      Extra = [],
      Default = query |
	put(false, Modes, _);

    Ok = false(Op) : Ok' = true,
      Extra ! Op |
	translate_options;

    Options =\= [_|_], Options =\= [] : Options' = [Options] |
	translate_options.



put(Put, Modes, Ok) :-

    Modes ? Put^ |
	put;

    Modes = [] : Put = _,
      Ok = true |
	true;

    otherwise,
    Modes ? _ |
	put.


options(crossref,   [true, true, true | _]^, true^).
options(constref,   [true | _]^, true^).
options(varsref,    [_, true | _]^, true^).
options(procref,    [_, _, true | _]^, true^).
options(undef,      [_, _, _, true | _]^, true^).
options(deadcode,   [_, _, _, _,  true | _]^, true^).
options(casecheck,  [_, _, _, _,  _, casecheck | _]^, true^).
options(similarity, [_, _, _, _,  _, true | _]^, true^).
options(varonce,    [_, _, _, _,  _, _, true | _]^, true^).
options(varoncenusc,[_, _, _, _,  _, _, once_non_underscored | _]^, true^).
options(proclist,   [_, _, _, _,  _, _, _, true | _]^, true^).
options(outer,      [_, _, _, _,  _, _, _, _,  true | _]^, true^).
options(callcheck,  [_, _, _, _,  _, _, _, _,  _, true | _]^, true^).
options(imported,   [_, _, _, _,  _, _, _, _,  _, _, true | _]^, true^).
options(prccasechk, [_, _, _, _,  _, _, _, _,  _, _, _, casecheck | _]^,
			true^
).
options(prcsimilar, [_, _, _, _,  _, _, _, _,  _, _, _, true | _]^, true^).
options(prcvaronce, [_, _, _, _,  _, _, _, _,  _, _, _, _,  true | _]^, true^).
options(all, Modes, Ok) :-
	put(true, Modes, Ok).
options(Op, _, false(Op)^) :- 
    otherwise |
	true.


create_flags(List, Flag, Dummy_mode) :-

    List ? false |
	create_flags;

    otherwise,
    list(List) : Dummy_mode = _,
      Flag = reset |
	true;

    List = [] :
      Flag = Dummy_mode |
	true.
