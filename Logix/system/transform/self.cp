/*

Language Transformer for FCP

William Silverman 08/88

Last update by		$Author: bill $
		       	$Date: 2002/06/26 06:53:13 $
Currently locked by 	$Locker:  $
			$Revision: 1.2 $
			$Source: /home/qiana/Repository/Logix/system/transform/self.cp,v $

Copyright (C) 1988, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([languages/8, language/8, language_names/4]).
-mode(trust).
-language(compound).


procedure language_attribute(Any, String, Any, language([Any])).

/*
** languages_names/4 collects option/attribute language specifications.
**
** Input:	Options			% esp: 	language(Name)
**						language(NameList)
**		Target			% Truncate list before this name
**		Attributes		% like  Options
** Output:	Languages		% NameList
*/

language_names(Options, Target, Attributes, NameList) :-
	options(Options, Attributes, Attributes', _TransformerId),
	language_list(Attributes', Target, _Attributes, Languages),
	remove_duplicates(Languages, Target, NameList).

procedure languages(Any, String, Any, [Any], [Any], [Any], [Any], Any).

/*
** languages/8 Calls services to transform options, attributes, source terms.
**
** The services are specified in the options and/or attributes.
**
** Input:	Options			% esp: 	transformer(Path)
**						language(Name)
**						language(NameList)
**		Target			% one of [dg, typed] initially
**		Attributes1		% like  Options
**		Terms1			% source
**		Output2			% tail of diagnostic stream
**
** Output:	Attributes4		% transformed  Options..Attributes1
**		Terms3			% transformed source
**		Output1			% diagnostics
*/

languages(Options, Target, Attributes1, Attributes4,
		Terms1, Terms3, Output1, Output2
) :-
	options(Options, Attributes1, Attributes2, TransformerId),
	language_list(Attributes2, Target, Attributes3, Languages1),
	remove_duplicates(Languages1, Target, Languages2),
	call_transformations(	TransformerId, Languages2,
				{[language(Languages2?) | Attributes3], Terms1,
				 Attributes4, Terms3
				},
				Output1, Output2
	).


options(Os, As1, As2, TransformerId) :-

    Os ? transformer(Path) : TransformerId' = _ |
	options,
	computation_utils # path_id(computation # Path, LId, Ok),
	check_context(Ok, Path, LId, TransformerId);

    otherwise,
    Os ?  Attribute :
      As2 ! Attribute |
	options;

    Os = [] :
      As1 = As2,
      TransformerId = standard |
	true;

    otherwise :
      Os' = [Os] |
	options.

check_context(true, _, LId, LId^).
check_context(Other, Path, _, error(Path, Other)^) :-
    otherwise |
	true.


language_list(As1, To, As2, Ls)  :-

    As1 ? language(L) |
	language_list,
	standard_language(L, To, L'),
	include_languages(L', Ls, Ls');

    otherwise,
    As1 ? A :
      As2 ! A |
	language_list;

    As1 = [] :
      As2 = [],
      Ls = [To] |
	true.


standard_language(L, To, Ls) :-

    L = compound : To = _,
      Ls = [compound, colon, typed] ;

    L = d : To = _,		% The successor of c!
      Ls = [dfcp] ;

    L = biospi : To = _,
      Ls = [evaluate, biospi, compound, colon, typed] ;

    L = psifcp : To = _,
      Ls = [evaluate, spifcp, compound, colon, typed] ;

    L = spifcp : To = _,
      Ls = [evaluate, spifcp, compound, colon, typed] ;

    L = fcp :
      Ls = [To] ;		% cut here

    list(L) : To = _,
      Ls = L ;

    L = [] : To = _,
      Ls = []^ ;

    otherwise : To = _,
      Ls = [L] .


include_languages(LNs, Ls1, Ls2) :-

    LNs ? LN :
      Ls1 ! LN |
	self;

    LNs = [] :
      Ls1 = Ls2 ;

    otherwise :
      Ls1 = [LNs | Ls2] .


remove_duplicates(Ls1, To, Ls2) + (Inherit = false) :-

    Ls1 ? To,
    To = dfcp, Inherit = false : Ls1' = _,
      Ls2 = [inherit] ;

    Ls1 ? To, otherwise : Ls1' = _, Inherit = _,
      Ls2 = [] ;

    Ls1 ? L, L =\= To, L = inherit : Inherit = _,
      Inherit' = true,
      Ls2 = [L | Ls2'?] |
	remove_duplicates1(L, Ls1', Ls1''),
	self;

    Ls1 ? L, L =\= To, L = dfcp, Inherit = false :
      Inherit' = true,
      Ls2 = [inherit, L | Ls2'?] |
	remove_duplicates1(L, Ls1', Ls1''),
	self;

    Ls1 ? L, L =\= To, otherwise :
      Ls2 = [L | Ls2'?] |
	remove_duplicates1(L, Ls1', Ls1''),
	self;

    Ls1 = [] : To = _, Inherit = _,
      Ls2 = [] .


remove_duplicates1(L, Ls1, Ls2) :-

    Ls1 ? L |
	self;

    Ls1 ? O, L =\= O :
      Ls2 = [O | Ls2'?] |
	self;

    Ls1 = [] : L = _,
      Ls2 = [] .


call_transformations(LId, Ls, Stuff, O1, O2) :-

    LId = standard |
	self # service_id(LId'),
	call_transformations;

    LId = error(Path, Other) : Ls = _,
      Stuff = {As, _, As, []},
      O1 = [language(Path) - Other | O2] |
	true;

    Ls = [] : LId = _,
      Stuff = {As, Ts, As, Ts},
      O1 = O2 |
	true;

    otherwise,				% LId = Service Id
    Ls ? L |
	computation # call([computation_utils # path_context(LId # L, LC, Ok)],
			   Events
		      ),
	call_transformations,
	call_transformation(Ok, L, LC, Events, Stuff, Stuff', O1, O1').

call_transformation(Ok, L, LC, Events, Stuff1, Stuff2, O1, O2) :-

    Ok = true,
    Stuff1 = {As1, Ts1, As4, Ts4} :
      Stuff2 = {As3, Ts3, As4, Ts4},
      write_channel(transform(As1, Ts1, As2, Ts2, Ds), LC, LC') |
	close_channel(LC'),
	serve_call(Events, Ds, L, As2, Ts2, As3, Ts3, O1, O2);

    Ok = false(Reason) : LC = _, Events = _,
      Stuff1 = Stuff2,
      O1 = [(language(L) : Reason) | O2] |
	true.


Path ::= String ; Channel ; [String] ; {`'#', Path, Path}.
Channel ::= Vector.

procedure language(Path, Any, [Any], [Any], Any, [Any], [Any], Any).

/*
** language/8 calls a Service to transform attributes and source terms.
**
** Input:	Service			% the implementing service
**		As1			% list of attributes
**		Ts1			% source
**		O2			% tail of diagnostic stream
**		Id			% Diagnostic identifier (language name)
**
** Output:	As3			% transformed attributes
**		Ts3			% transformed source
**		O1			% diagnostics
*/

language(Service, As1, Ts1, O1, Id, As3, Ts3, O2) :-
	computation #
		call([Service # transform(As1, Ts1, As2, Ts2, Ds)], Events),
	serve_call(Events, Ds, Id, As2, Ts2, As3, Ts3, O1, O2).


serve_call([aborted | _], _, _, _, _, []^, []^, O, O^).
serve_call(Es, Ds, Id, As2, Ts2, As3, Ts3, O1, O2) :-

    Es ? BE,
    string(BE), BE =\= aborted |
	serve_call;

    Es ? failed(call(_), _) |
	serve_call;

    otherwise,
    Es ? E |
	computation # E,
	serve_call;

    Ds ? Comment, Comment = (_ : _) :
      O1 ! Comment |
	self;

    Ds ? E, E =\= (_ : _) :
      O1 ! (Id ! E) |
	serve_call;

    As2 ? A :
      As3 ! A |
	serve_call;

    Ts2 ? T :
      Ts3 ! T |
	serve_call;

    Ds = [] : Es = _, Id = _,
      As2 = As3,
      Ts2 = Ts3,
      O1 = O2 |
	true.
