/* $Header: /home/qiana/Repository/Logix/system/transform/lpi/rename.cp,v 1.1 1999/07/09 07:03:18 bill Exp $ */
/*

Replacement dictionary for lpi.

  Bill Silverman - May 1992
*/


-language(compound).
-export([requests/5]).
-mode(trust).

/* Requests/5
**
** Initialize dictionary and start server.
**
*/

requests(In, SuperVars, Equivalents, ClassVars, ModuleName) :-

	make_list(SuperVars, Equivalents, Replaces),
	leaf(Dictionary?),
	make_dictionary(Replaces?, ClassVars, ModuleName,
			Dictionary, Dictionary'?
	),
	serve(In, Dictionary').

/* make_list/3
**
** Create Replaces from SuperVars,Equivalents
** 
** Input:	SuperClass arguments list
**		Equivalents list
**
** Output:      Replaces list = {Key, Rename}
**				 Key	= SuperClass name
**				 Rename = SubClass name
*/

make_list(SuperVars, Equivalents, Replaces) :-

    SuperVars ? `A,
    Equivalents ? `B :
      Replaces ! {A, B} |
	self;

    SuperVars ? ?A,
    Equivalents ? ?B :
      Replaces ! {A, B} |
	self;

    SuperVars ? _(A),
    Equivalents = [] :
      Replaces ! {A, A} |
	self;

    SuperVars = [] : Equivalents = _,
      Replaces = [] .


/* make_dictionary/5
**
** Create dictionary process tree from Replaces list.
** 
** Input:	Replaces list
**		ClassVars
**		ModuleName
**		DictIn = stream to initial empty dictionary
**
** Output:      DictOut = stream to initialized dictionary;
**			  entries contain a Name and its Alias;
**			  an Alias is constructed for each name in
**			  ClassVars which is not in Replaces, by
**			  concatenating  [ModuleName, "$", Name] .
**
** Note: a minor optimization is to defer Alias construction until needed.
*/

make_dictionary(Replaces, ClassVars, ModuleName, DictIn, DictOut) :-

    ClassVars ? _(ClassName) |
	alias(Replaces, ClassName, ModuleName, DictIn, DictIn'?, Replaces'),
	self;

    ClassVars = [],
    Replaces ? Name(ClassName) :
      DictIn ! put(Name, ClassName) |
	self;

    ClassVars = [],
    Replaces = [] : ModuleName = _,
      DictIn = DictOut .

alias(Rs, ClassName, ModuleName, DictIn, DictOut, Replaces) :-

    Rs ? Name(ClassName) : ModuleName = _,
      DictIn ! put(Name, ClassName),
      DictIn' = DictOut,
      Replaces = Rs'? ;

    Rs ? Replace, Replace =\= _(ClassName) : ModuleName = _,
      Replaces ! Replace |
	self;

    Rs = [],
    string_to_dlist(ModuleName, ML, DCL),
    string_to_dlist(ClassName, CL, []) :
      Replaces = [],
      ascii('$', Dollar),
      ascii('_', ULine),
      DCL = [Dollar | CL],
      DictIn ! put(ClassName, Alias),
      DictIn' = DictOut |
	capitalize(ML, ULine, CL, AL),
	list_to_string(AL, Alias).

capitalize(ML, ULine, CL, AL) :-

    CL =\= [ULine | _],
    ML ? Letter, ascii(a) =< Letter, Letter =< ascii(z),
    Capital := Letter - ascii('a') + ascii('A') :
      AL = [Capital | ML'] ;

    otherwise : CL = _,
      AL = [ULine | ML] .

leaf(In) :-

    In ? put(Name, Alias) |
	node(In'?, Name, Alias, L, R),
	leaf(L?),
	leaf(R?);

    In ? rename(Name, _NL, Annotate, {Annotate, Name}^) |
	self;

    In = [] : true.

/* node/5
**
** Node of the (binary tree) dictionary.
**
** Report the (annotated) Alias in response to a request for Name.
*/

node(In, Name, Alias, Left, Right) :-

    In ? Request, arg(2, Request, Other),
    Other @< Name :
      Left ! Request |
	self;      

    In ? find(Name, Alias^) |
	self;

    In ? rename(Name, _NL, Annotate, Annotate(Alias)^) |
	self;

    In ? rename(Primed, NL, Annotate, Result),
    Name @< Primed,
    string_to_dlist(Name, UL, Ps) :
      NL = UL |
	primed(Primed, NL, Annotate, Result, Name, Alias, Ps, Right, Right'?),
	self;

    In ? Request,
    otherwise :
      Right ! Request |
	self;

    In = [] : Name = _, Alias = _,
      Left = [],
      Right = [] .

/* serve/2
**
** Serve requests for dictionary services.
**
** Rename all variables in a term as necessary.
*/

serve(In, Dict) :-

    In ? Find, Find = find(_,_) :
      Dict ! Find |
	self;

    In ? Put, Put = put(_, _) :
      Dict ! Put |
	self;

    In ? term(Value, Value') |
	rename(Value, Value', Dict, Dict'),
	self;

    In = [] :
      Dict = [] .


/* rename/4
**
** Rename Term using Dict
*/

rename(S, S1, In, In1) :-

    S ? Car  :
      S1 ! Car'? |
	rename(Car, Car', In, In'),
	rename;

    arity(S, N) |
	rename_tuple(N, S, S1, In, In1);

    constant(S) :
      S1 = S,
      In = In1 .

rename_tuple(Ar, Tuple, TupleOut, In, In') :-

    Tuple = `Var,
    string_to_dlist(Var, VL, []) : Ar = _,
      In ! rename(Var, VL, "_var", TupleOut);

    Tuple = ?Var,
    string_to_dlist(Var, VL, []) : Ar = _,
      In ! rename(Var, VL, "_ro", TupleOut);

    otherwise,
    make_tuple(Ar, TupleOut^) |
	rename_tuple_args(Ar, Tuple, TupleOut, In, In').


rename_tuple_args(Ar, Tuple, TupleOut, In, In1) :-

    Ar-- > 0,
    arg(Ar,Tuple,TupleAr),
    arg(Ar,TupleOut,TupleOutAr) |
	rename(TupleAr,TupleOutAr, In, In'),
	rename_tuple_args;

    Ar =< 0 : Tuple = _, TupleOut = _,
      In = In1 .


/* primed/9
**
** If  Primed = Name'* , Result = Alias'*
**
** else forward rename/4 request.
*/

primed(Primed, NL, Annotate, Result, Name, Alias, Ps, Right, Right') :-

    string_to_dlist(Primed, PL, _),
    string_to_dlist(Name, UL, Primes),
    string_to_dlist(Alias, AL, As) : NL = _,
      ascii('''', Prime),
      Primes = [Prime | Primes],
      PL = UL,		% iff the difference is all primes.
      As = Ps,
      Result = Annotate(Aliased),
      Right = Right' |
	list_to_string(AL, Aliased) ;

    otherwise : Name = _, Alias = _, Ps = _,
      Right ! rename(Primed, NL, Annotate, Result) .
