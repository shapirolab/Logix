-language(compound).
-export([server]).


server(Rs, Nx) :-
   true :
      Ix = 1 |
	stream # hash_table(Ms?),
	serve_requests.

serve_requests(Rs, Ms, Ix, Nx) :-

   Rs ? get(SId, Reply) :
      Ms ! lookup(SId, New?, Old, Result) |
	get_source,
	self;

   Rs ? index(Ix^),
   Ix++ |
	self;

   Rs ? index_value(Ix^) |
	self;

   Rs = [] :
      Ms = [],
      Nx = Ix .


get_source(Result, Old, New, SId, Reply) :-

   Result = new,
   SId = [Name | DId] : Old = _,
      Reply = Reply'? |
	get_source # context(DId, Name, [], Result', {Errors, []}),
	analyse_source;

   Result = old : SId = _,
      New = Old,
      Reply = Old .


analyse_source(Result, Errors, New, Reply) :-

   Result = false(_) : Errors = _,
      Reply = Result,
      New = Reply ;

   Result =\= false(_),
   Errors =\= [] :
      Reply = false(parsing_errors(Errors)),
      New = Reply ;

   Result =\= false(_), Result =\= module(_, _, _),
   Errors = [] :
      Reply = false(not_module),
      New = Reply ;

   Result = module(Options, Attributes, Clauses),
   Errors = [] |
	transform # language_names(Options, inherit, Attributes, Languages),
	languages_upto_inherit(Languages, List),
	transform(List, Options, Clauses, Clauses', Errors'),
	transform_result.

languages_upto_inherit(Languages, List) :-

   Languages ? L, L =\= inherit, L =\= compound, L =\= dfcp,
		  L =\= lpi, L =\= include, L =\= nil :
      List ! L |
	self;

   otherwise : Languages = _,
      List = [] .
	

transform(List, Options, Clauses1, Clauses2, Errors) :-

   List = [] : Options = _,
      Clauses2 = Clauses1,
      Errors = [] ;

   otherwise |
	transform # languages(Options, fcp, [language(List)] , _,
				Clauses1, Clauses2, Errors, []).


transform_result(Attributes, Errors, New, Reply, Clauses) :-

   Errors = [] :
      New = old(Attributes, Clauses),
      Reply = new(Attributes, Clauses) ;

   Errors =\= [] : Attributes = _, Clauses = _,
      Reply = false(Errors),
      New = Reply .
