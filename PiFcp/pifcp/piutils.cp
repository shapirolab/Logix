/*
Precompiler for Pi Calculus procedures - utilities.

Bill Silverman, December 1999.

Last update by		$Author: bill $
		       	$Date: 2000/02/14 08:35:03 $
Currently locked by 	$Locker:  $
			$Revision: 1.2 $
			$Source: /home/qiana/Repository/PiFcp/pifcp/piutils.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language(compound).
-export([concatenate_lists/2, make_lhs_tuple/3,
	 make_predicate_list/3, names_to_channel_list/2,
	 remove_duplicate_strings/3, sort_out_duplicates/3,
	 subtract_list/3, tuple_to_atom/2, update_process_mode/3,
	 verify_channel/7]).


update_process_mode(Mode, GuardMode, NewMode) :-

    GuardMode =?= none :
      NewMode = Mode;

    Mode =?= none :
      NewMode = GuardMode;

    Mode =?= GuardMode :
      NewMode = GuardMode;

    Mode =?= compare,
    GuardMode =?= otherwise :
      NewMode = compared;

    Mode =?= send,
    GuardMode =?= receive :
      NewMode = mixed;

    Mode =?= receive,
    GuardMode =?= send :
      NewMode = mixed;

    Mode =?= mixed,
    GuardMode =?= receive :
      NewMode = mixed;

    Mode =?= mixed,
    GuardMode =?= send :
      NewMode = mixed;

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


concatenate_lists(Lists, Out) :-

    Lists = [List | Rest],
    List ? Item, Item =\= [] :
      Out ! Item,
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


remove_item(Item, List1, List2) :-

    List1 =?= [] :
      Item = _,
      List2 = [];

    List1 ? Item |
	self;

    List1 ? Other, Other =\= Item :
      List2 ! Other |
	self.


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
    Item =\= "_" :
      List2 ! Item |
	delete_duplicate_items(Item, List1', List1''),
	self;

    List1 ? Item,
    Item =?= "_" :
      List2 ! Item |
	self;

    List1 = [] :
      List2 = [].

  delete_duplicate_items(Item, ListIn, ListOut) :-

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

/*
check_for_duplicates(List1, List2, ErrorCode, Errors, NextErrors) :-

    List1 ? Item,
    Item =\= "_" :
      List2 ! Item |
	search_for_duplicate(Item, List1', List1'',
			ErrorCode, Errors, Errors'?),
	self;

    List1 ? Item,
    Item =?= "_" :
      List2 ! Item |
	self;

    List1 = [] :
      List2 = [],
      ErrorCode = _,
      Errors = NextErrors.

  search_for_duplicate(Item, ListIn, ListOut,
		ErrorCode, Errors, NextErrors) :-

    ListIn ? I,
    Item =?= I,
    ErrorCode =?= {Name, Code} :
      Errors ! (Name: Code(Item)) |
	self;

    ListIn ? I,
    Item =?= I,
    ErrorCode =?= "" |
	self;

    ListIn ? I,
    Item =\= I :
      ListOut ! I |
	self;

    ListIn =?= [] :
      Item = _,
      ErrorCode = _,
      Errors = NextErrors,
      ListOut = [].
*/

make_lhs_tuple(Name, ChannelNames, Tuple) :-

    ChannelNames =?= [] :
      Tuple = {Name};

   ChannelNames =\= [] |
	names_to_variables(ChannelNames, Variables, N),
	N++,
	make_tuple(N'?, Tuple),
	arg(1, Tuple, Name),
	fill_tuple(Variables, Tuple, 2, Tuple).


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
	ordered_merger(Merger, Reply, ok),
	utils#binary_sort_merge(List, Out, Merger).    

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

    In1 = [I | _], In2 ? I :
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

    In1 = [] :
      In2 = Out,
      Left = Right ;

    In2 = [] :
      In1 = Out,
      Left = Right .


verify_channel(Name, ChannelName, ChannelNames, Locals, OkChannelName,
		Errors, NextErrors) :-

    string(ChannelName), ChannelName =\= "_", ChannelName =\= "" |
	defined_channel;

    ChannelName = `ChannelName',
    string(ChannelName'), ChannelName' =\= "_", ChannelName' =\= "" |
	defined_channel;

    otherwise :
      ChannelNames = _,
      Locals = _,
      OkChannelName = "_",
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
      OkChannelName = "_",
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
