/*
Precompiler for Stochastic Pi Calculus procedures - utilities.

Bill Silverman, December 1999.

Last update by		$Author: bill $
		       	$Date: 2003/03/12 14:32:22 $
Currently locked by 	$Locker:  $
			$Revision: 1.2 $
			$Source: /home/qiana/Repository/Aspic/BioSpi/biospi/utilities.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language([evaluate, compound, colon]).
%-mode(interrupt).
-export([append/3,
	 concatenate_lists/2, find_logix_variables/3, make_lhs_tuple/3,
	 make_communicator/3, make_predicate_list/3,
	 untuple_predicate_list/3, untuple_predicate_list/4,
	 names_to_channel_list/2, names_to_variables/3,
	 remove_duplicate_strings/3, sort_out_duplicates/3,
	 remove_item/3, subtract_list/3,
	 tuple_to_atom/2, update_process_mode/3,
	 verify_channel/7, verify_communication_channel/8]).

-include(spi_constants).
-include(bio_constants).


update_process_mode(Mode, GuardMode, NewMode) :-

    GuardMode =?= none :
      NewMode = Mode;

    Mode =?= none,
    GuardMode =?= send :
      NewMode = communicate;

    Mode =?= none,
    GuardMode =?= receive :
      NewMode = communicate;

    Mode =?= none,
    GuardMode =\= send, GuardMode =\= receive :
      NewMode = GuardMode;

    Mode =?= GuardMode :
      NewMode = GuardMode;

    Mode =?= send,
    GuardMode =?= communicate :
      NewMode = communicate;

    Mode =?= receive,
    GuardMode =?= communicate :
      NewMode = communicate;

    Mode =?= communicate,
    GuardMode =?= send :
      NewMode = communicate;

    Mode =?= communicate,
    GuardMode =?= receive :
      NewMode = communicate;

    Mode =?= compare,
    GuardMode =?= otherwise :
     NewMode = compared;

    otherwise :
      Mode = _,
      GuardMode = _,
      NewMode = conflict.

/**** Utilities ****/

tuple_to_atom(LHS, Atom) :-

    LHS = {String} :
      Atom = String;

    otherwise :
      Atom = LHS.


append(List, Tail, NewList) :-

    List ? Item :
      NewList ! Item |
	self;

    List = [] :
      NewList = Tail;

    List =\= [_|_], List =\= [] |
      NewList = [List | Tail].


concatenate_lists(Lists, Out) :-

    Lists = [List | Rest],
    List ? Item, Item =\= [], Item =\= [_ | _] :
      Out ! Item,
      Lists' = [List' | Rest] |
	self;

    Lists = [List | Rest],
    List ? Item, Item =\= [], Item =?= [_ | _] :
      Lists' = [Item, List' | Rest] |
	self;

    Lists = [List | Rest],
    List ?  [] :
      Lists' = [List' | Rest] |
	self;

    Lists ? [] |
	concatenate_lists;

    Lists =?= [] :
      Out = [].


subtract_list(List1, List2, List3) :-

    List2 ? Item |
	remove_item(Item, List1, List1'),
	self;

    List2 = [] :
      List3 = List1.


remove_item(Item, ListIn, ListOut) :-

    ListIn ? I,
    Item =?= I |
	self;

    ListIn ? I,
    Item =\= I :
      ListOut ! I |
	self;

    ListIn =?= [] :
      Item = _,
      ListOut = [].


remove_duplicate_strings(List1, List2, Reply) :-

	ordered_merger(Merger, Duplicates, []),
	utils#binary_sort_merge(List1, _, Merger),
	removed_some(List1, List2, Duplicates, Reply).

  removed_some(List1, List2, Duplicates, Reply) :-

    Duplicates =?= [] :
      Reply = [],
      List2 = List1;

    Duplicates =\= [] |
	/* Delete "duplicate" duplicates. */
	remove_duplicates(Duplicates, Reply),
	remove_duplicates(List1, List2).

  remove_duplicates(List1, List2) :-

    List1 ? Item,
    Item =\= NULL :
      List2 ! Item |
	remove_item(Item, List1', List1''),
	self;

    List1 ? Item,
    Item =?= NULL :
      List2 ! Item |
	self;

    List1 = [] :
      List2 = [].


make_lhs_tuple(Name, ChannelNames, Tuple) :-

    ChannelNames =?= [] :
      Tuple = {Name};

   ChannelNames =\= [] |
	names_to_variables(ChannelNames, Variables, N),
	N++,
	make_tuple(N'?, Tuple),
	arg(1, Tuple, Name),
	fill_tuple(Variables, Tuple, 2, Tuple).


untuple_predicate_list(Operator, Predicates, List) + (NextList = []) :-

    Predicates =?= {Operator, Predicate, Predicates'} |
	untuple_predicate_list(Operator, Predicate, List, List'?),
	self;

    otherwise :
      Operator = _,
      List = [Predicates | NextList].


make_predicate_list(Operator, List, Predicates) :-

    List =?= [] |
      Operator = _,
      Predicates = true;

    List ? true,
    List' =\= [] |
      self;

    List ? Predicate, Predicate =\= true,
    List' ? true :
      List''' = [Predicate | List''] |
	self;

    List ? Predicate, Predicate =\= true,
    List' =\= [], List' =\= [true | _] :
      Predicates = {Operator, Predicate, Predicates'?} |
	self;

    List =?= [Predicate] :
      Operator = _,
      Predicates = Predicate.


sort_out_duplicates(InLists, Out, Reply) :-

	concatenate_lists(InLists, List),
	ordered_merger(Merger, Duplicates, []),
	utils#binary_sort_merge(List, Out, Merger),
	/* Remove duplicate "duplicates" */
	utils#binary_sort_merge(Duplicates, Reply).

ordered_merger(In, Left, Right) :-
    In ? ordered_merge(In1, In2, Out) |
	ordered_merge(In1, In2, Out, Left, Left'),
	ordered_merger;

    In = [] :
      Left = Right.

ordered_merge(In1, In2, Out, Left, Right) :-

    In1 ? I1, In2 = [I2 | _],
    string(I1), string(I2),
    I1 @< I2 :
      Out ! I1 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    string(I1), string(I2),
    I2 @< I1 :
      Out ! I2 |
	ordered_merge;

    In1 = [I | _], In2 ? I,
    string(I) :
      Left ! I |
	ordered_merge;

    In1 ? I1, In2 = [I2 | _],
    arg(1, I1, A1), arg(1, I2, A2),
    A1 @< A2 :
      Out ! I1 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    arg(1, I1, A1), arg(1, I2, A2),
    A2 @< A1 :
      Out ! I2 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    arg(1, I1, A), arg(1, I2, A) :
      Left ! A |
	ordered_merge;

    In1 ? I1, In2 = [I2 | _],
    string(I1), arg(1, I2, A2),
    I1 @< A2 :
      Out ! I1 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    string(I1), arg(1, I2, A2),
    A2 @< I1 :
      Out ! I2 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    string(I1), arg(1, I2, A),
    I1 =?= A :
      Left ! A |
	ordered_merge;

    In1 ? I1, In2 = [I2 | _],
    arg(1, I1, A1), string(I2),
    A1 @< I2 :
      Out ! I1 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    arg(1, I1, A1), string(I2),
    I2 @< A1 :
      Out ! I2 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    arg(1, I1, A), string(I2),
    A =?= I2 :
      Left ! A |
	ordered_merge;

    In1 = [] :
      In2 = Out,
      Left = Right ;

    In2 = [] :
      In1 = Out,
      Left = Right .


verify_communication_channel(Name, ChannelName, ChannelNames, Locals,
			     OkLocus, OkChannelName, Errors, NextErrors) :-

    string(ChannelName) :
      OkLocus = LOCAL |
	verify_channel;

    ChannelName = `ChannelName',
    string(ChannelName') :
      OkLocus = LOCAL |
	verify_channel;

    ChannelName = S2S(ChannelName'),
    string(ChannelName') :
      OkLocus = parent(S2S) |
	verify_channel;

    ChannelName = P2C(ChannelName'),
    string(ChannelName') :
      OkLocus = self(P2C) |
	verify_channel;

    ChannelName = C2P(ChannelName'),
    string(ChannelName') :
      OkLocus = parent(P2C) |
	verify_channel;

    ChannelName = LOCAL(ChannelName'),
    string(ChannelName') :
      OkLocus = LOCAL |
	verify_channel;

    ChannelName = (MERGE + ChannelName'),
    string(ChannelName') :
      OkLocus = parent(MERGE) |
	verify_channel;

    ChannelName = (MERGE - ChannelName'),
    string(ChannelName') :
      OkLocus = parent(MERGE) |
	verify_channel;

    ChannelName = ACCEPT(ChannelName'),
    string(ChannelName') :
      OkLocus = parent(ENTER) |
	verify_channel;

    ChannelName = ENTER(ChannelName'),
    string(ChannelName') :
      OkLocus = parent(ENTER) |
	verify_channel;
    
    ChannelName = EXPEL(ChannelName'),
    string(ChannelName') :
      OkLocus = self(EXIT) |
	verify_channel;

    ChannelName = EXIT(ChannelName'),
    string(ChannelName') :
      OkLocus = parent(EXIT) |
	verify_channel;

   otherwise :
      ChannelNames = _,
      Locals = _,
      OkLocus = LOCAL,
      OkChannelName = NULL,
      Errors = [Name - invalid_channel(ChannelName) | NextErrors].
	

verify_channel(Name, ChannelName, ChannelNames, Locals, OkChannelName,
		Errors, NextErrors) :-

    string(ChannelName),
    nth_char(1, ChannelName, C),
    CHAR_a =< C, C =< CHAR_z |
	defined_channel;

    ChannelName = `ChannelName',
    nth_char(1, ChannelName', C),
    CHAR_A =< C, C =< CHAR_Z |
	defined_channel;

    otherwise :
      ChannelNames = _,
      Locals = _,
      OkChannelName = NULL,
      Errors = [Name - invalid_channel(ChannelName) | NextErrors].

  defined_channel(Name, ChannelName, ChannelNames, Locals, OkChannelName,
		Errors, NextErrors) :-

    ChannelNames ? Other, ChannelName =\= Other |
	self;

    ChannelNames ? ChannelName :
      ChannelNames' = _,
      Locals = _,
      Name = _,
      OkChannelName = ChannelName,
      Errors = NextErrors;

    ChannelNames = [], Locals =\= [] :
      ChannelNames' = Locals,
      Locals' = [] |
	self;

    otherwise :
      ChannelNames = _,
      Locals = _,
      OkChannelName = NULL,
      Errors = [Name - undefined_channel(ChannelName) | NextErrors].

names_to_channel_list(MChannels, ChannelList) :-
	names_to_variables(MChannels, Variables, N),
	make_channel_list(N, Variables, ChannelList).


make_channel_list(N, Variables, ChannelList) :-

    N =< 0 :
      ChannelList = [],
      Variables = _;
    
    N > 0,
    make_tuple(N, Tuple) |
	fill_tuple(Variables, Tuple, 1, ChannelList).

fill_tuple(Cs, T, I, Tuple) :-

    Cs ? C,
    arg(I, T, A),
    I++ :
      A = C |
	self;

    Cs = [] :
      I = _,
      Tuple = T.


names_to_variables(ChannelNames, Variables, Count) +
				(Counter = 0) :-
    ChannelNames ? ChannelName,
    Counter++ :
      Variables ! `ChannelName |
	self;

    ChannelNames =?= [] :
      Variables = [],
      Count = Counter.


find_logix_variables(Predicate, LogixVars, NextLogixVars) :-

    Predicate ? Element |
	find_logix_variables(Element, LogixVars, LogixVars'?),
	self;

    Predicate =?= `String, string(String) :
      LogixVars = [Predicate | NextLogixVars];

    Predicate =?= `_Other,
    otherwise :
      LogixVars = NextLogixVars;

    Predicate =?= ?String, string(String) :
      LogixVars = [`String | NextLogixVars];

    Predicate =?= ?_Other,
    otherwise :
      LogixVars = NextLogixVars;

    tuple(Predicate),
    Predicate =\= `_, Predicate =\= ?_,
    N := arity(Predicate) |
	find_in_tuple + (Tuple = Predicate);

    otherwise :
      Predicate = _,
      LogixVars = NextLogixVars.

  find_in_tuple(N, Tuple, LogixVars, NextLogixVars) :-

    N-- > 0,
    arg(N, Tuple, Argument) |
	find_logix_variables(Argument, LogixVars, LogixVars'?),
	self;

    N =< 0 :
      Tuple = _,
      LogixVars = NextLogixVars.

make_communicator(Atom, Chooser, CommunicationAtom) :-
    arg(1, Atom, Prefix),
    string_to_dlist(Prefix, PL, [COMMUNICATION_SUFFIX]),
    list_to_string(PL, Goal) :
      Chooser = Goal |
	utils#tuple_to_dlist(Atom, [_ | ChannelVariables],
			     [BIO_CHOSEN, BIO_MESSAGE]),
	utils#list_to_tuple([Chooser | ChannelVariables], CommunicationAtom).

    
