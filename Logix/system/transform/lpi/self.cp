/* $Header: /home/qiana/Repository/Logix/system/transform/lpi/self.cp,v 1.1 1999/07/09 07:03:19 bill Exp $ */

/*
Driver for lpi transformation - logic programs with inheritance.
Bill Silverman - November 1991
*/


-language(compound).
-export([transform/5]).
-mode(trust).


transform(Attributes1, Clauses, Attributes2, Terms, Errors) :-

					% get path of transformed file
	get_context(Attributes1?, ModuleName, Context),
% services for principle module
	servers # ids(IdC, ModuleName?, Context),
% begin with principle module
	module # clauses(Clauses, Terms1, _Estate(Rscs), IdC, IdC', Diags([])),
	remote # filter_calls(Terms1, Terms),
	servers # diagnostics(Diags?, Errors),
	servers # dictionary(Lookups?),
	depends(Rscs?, Lookups, Names),
	depends_on(Names?, Attributes1?, Attributes2),
	close_channel(IdC').

/***
* Search attributes list for id of analysed module.
***/

get_context(Attr, ModuleName, Context) :-

    Attr ? service_id(MId),
    MId ? ModuleName^ : Attr' = _,
      Context = MId' ;

    Attr ? A, A =\= service_id([_|_]) |
	get_context;

    Attr = [] :
      ModuleName = "unknown_module" |
	computation # service_id(Context) .

/***
* Extract dependence on heritage from remote modules.
***/

depends(Rscs, Lookups, Names) :-

    Rscs ? rsc(RPC, Head, Local, RHSS, Depends), RPC = _ModuleName # _Call,
    Depends ? Addition :
      Lookups ! lookup(Addition, [], _),
      Rscs'' = [rsc(RPC, Head, Local, RHSS, Depends') | Rscs'] |
	depends;

    Rscs ? rsc(ModuleName # _Call, _Head, _Local, _RHSS, []) :
      Lookups ! lookup(ModuleName, [], _) |
	depends;

    Rscs = [] :
      Lookups = names(Names, []) .
	

depends_on(Names, Attributes', Attributes) :-

    Names = [] :
      Attributes = Attributes' ;

    Names =\= [] :
      Attributes ! depends(Names) .
