/*  $Header: /home/qiana/Repository/Logix/system/Doors/id_card.cp,v 1.1.1.1 1999/07/09 07:03:25 bill Exp $ */

-language([dfcp]).
-mode(interrupt).

query(IdCard1,IdFrame) :-
	query_ok(IdCard1,IdFrame,_Ok).

query_ok(IdCard1,IdFrame,Ok) :-
	query_all(IdCard1,IdFrame,true,Ok).

query_all(IdCard,IdFrame,L,R) :-
	IdFrame ? (Attr:Value?), listener(IdCard) |
		attr_to_value_ok(IdCard,Attr,Value,Ok),		
		query_Ok(Ok?,L,L'),
		self;
	IdFrame ? (Attr:Value), known(Value), listener(IdCard) |
		attr_to_value_ok(IdCard,Attr,Value1,Ok),		
		query_compare(Ok?,Value?,Value1?,Ok1),
		query_Ok(Ok1?,L,L'),
		self;
	IdFrame = [] | IdCard = _, R = L.

query_compare(Ok,Value,Value1,Ok1) :-
	Ok =\= true | Value = _, Value1 = _, Ok1 = Ok;
	Ok = true, Value = Value1 | Ok1 = true;	
	Ok = true, Value =\= Value1 | Ok1 = false(Value =\= Value1).

query_Ok(Ok,L,R) :-
	Ok = true | R = L;
	Ok = false(Reason), L = true | R = false(Reason);
	Ok = false(Reason), L = false(Reason1) | R = false([Reason,Reason1]).


decompose(IdCard,Attrs,Values) :-
	IdCard = [] | Attrs = [], Values = [];

	IdCard ? (Attr: Value) |  Attrs ! Attr, Values ! Value, self.



attr_to_value(IdCard,Attr,Value) :-
	attr_to_value_ok(IdCard,Attr,Value,_Ok).

attr_to_value_ok(IdCard,Attr,Value,Ok) :-
	invalid(IdCard) |
		Ok=false(['Attribute not found, invalid Message',Attr]),
		Value = undefined;
	IdCard=[] | 
		Ok=false(['Attribute not found: ',Attr]),
		Value=undefined;
	IdCard=undefined | 
		Ok=false(['Attribute not found: ',Attr]),
		Value=undefined;
	IdCard ? Invalid, invalid(Invalid) | self;
	IdCard ? Colon(_Attr, _Value), invalid(Colon) | self;
	IdCard ? (Attr1:_Value1), invalid(Attr1) | self;
	IdCard ? (_Attr1:Value1), invalid(Value1) | self;
	IdCard ? (Attr:Value1) | Value=Value1, Ok=true, IdCard' = _;
	IdCard ? (Attr1:_Value), Attr=\=Attr1 | self;
	IdCard = (Attr:Value1) | Value=Value1, Ok=true;
	IdCard = (Attr1:_Value), Attr=\=Attr1 |
		Ok=false(['Attribute not found: ',Attr]),
		Value=undefined;
	otherwise |
		computation#display(term, IdCard, type(ground)),
		Ok=false(['Unexpected id_card. Attribute not found: ',Attr]),
		Value=undefined.



check_attr_value(IdCard,Attr,Value,Ok) :-
	invalid(IdCard) |
		Ok=false(['Attribute not found, invalid Message',Attr]),
		Value = _;
	IdCard=[] | 
		Ok=false(['Attribute not found: ',Attr]), Value = _;
	IdCard ? (Attr:Value) | Ok=true, IdCard' = _;
	IdCard ? (Attr:Value1), Value =\= Value1 | Ok=false, IdCard' = _;
	IdCard ? (Attr1:_Value), Attr=\=Attr1 | self;
	IdCard ? Invalid, invalid(Invalid) | self;
	IdCard ? Colon(_Attr, _Value), invalid(Colon) | self;
	IdCard ? (Attr1:_Value1), invalid(Attr1) | self;
	IdCard ? (_Attr1:Value1), invalid(Value1) | self;
	IdCard = (Attr:Value) | Ok = true;
	IdCard = (Attr1:_Value), Attr=\=Attr1 |
		Ok=false(['Attribute not found: ',Attr]), Value = _;
	otherwise |
		Value = _,
		computation#display(term, IdCard, type(ground)),
		Ok=false(['Unexpected id_card. Attribute not found: ',Attr]).



update(IdCard,UpdateIdCard,IdCard1) :-
	UpdateIdCard ? (Attr:NewValue) |
		update_attr_value(Attr,NewValue,IdCard,IdCard'),
		self;

	UpdateIdCard ? Cdr, invalid(Cdr) |
		self;

	otherwise |	% includes invalid(UpdateIdCard) and IdCard = [].
		UpdateIdCard = _,
		IdCard1 = IdCard.

/* The following is an attempt to protect IdCard **
** contents - it should be implemented in depth. */

update_attr_value(Attr,NewValue,IdCard,IdCard1) :-
	invalid(Attr) | Attr = _, NewValue = _,
		IdCard1 = IdCard;

	invalid(NewValue) | Attr = _, 
		IdCard1 = IdCard;

	otherwise |
		update_attr_value1.

update_attr_value1(Attr,NewValue,IdCard,IdCard1) :-

	invalid(IdCard) |
		IdCard1 = [(Attr:NewValue)];

	IdCard = undefined |
		IdCard1 = [(Attr:NewValue)];

	IdCard = [] | IdCard1 = [(Attr:NewValue)];

	IdCard = (Attr1:Value1), Attr =\= Attr1 |
		IdCard1 = [(Attr1:Value1), (Attr:NewValue)];

	IdCard = (Attr:_OldValue) |
		IdCard1 = [(Attr : NewValue)];

	IdCard ? (Attr:_OldValue) |
		IdCard1 ! (Attr:NewValue),
		IdCard1' = IdCard';

	IdCard ? (Attr1:Value1), Attr =\= Attr1 |
		IdCard1 ! (Attr1:Value1),
		self.

% set is like update but only sets a value if the attribute doesn't exist
set(IdCard,UpdateIdCard,IdCard1) :-
	UpdateIdCard ? (Attr:NewValue) |
		set_attr_value(Attr,NewValue,IdCard,IdCard'),
		self;

	UpdateIdCard ? Cdr, invalid(Cdr) |
		self;

	otherwise |	% includes invalid(UpdateIdCard) and IdCard = [].
		UpdateIdCard = _,
		IdCard1 = IdCard.

/* The following is an attempt to protect IdCard **
** contents - it should be implemented in depth. */

set_attr_value(Attr,NewValue,IdCard,IdCard1) :-
	invalid(Attr) | Attr = _, NewValue = _,
		IdCard1 = IdCard;

	invalid(NewValue) | Attr = _, 
		IdCard1 = IdCard;

	otherwise |
		set_attr_value1.

set_attr_value1(Attr,NewValue,IdCard,IdCard1) :-
	IdCard = [] | IdCard1 = [(Attr:NewValue)];

	IdCard ? (Attr:OldValue) |
		IdCard1 ! (Attr:OldValue), % leave alone if exists
		NewValue = _,
		IdCard1' = IdCard';

	IdCard ? (Attr1:Value1), Attr =\= Attr1 |
		IdCard1 ! (Attr1:Value1),
		self.


query_update(IdCard,UpdateIdCard,IdCard1, Olds) :-
    UpdateIdCard = [] | IdCard1 = IdCard, Olds = [];

    UpdateIdCard ? (Attr:NewValue) |    
        query_update_attr_value(Attr,NewValue,IdCard,IdCard', Olds, Olds'?),
        self.

query_update_attr_value(Attr,NewValue,IdCard,IdCard1, Olds1, Olds2) :-
    IdCard = [], ground(Attr) | 
        IdCard1 = [(Attr:NewValue?)], Olds1 = Olds2;

    IdCard ? (Attr : OldValue) |
        IdCard1 ! (Attr:NewValue),
        Olds1 = [(Attr : OldValue) | Olds2],
        IdCard1' = IdCard';

    IdCard ? (Attr1:Value1), Attr =\= Attr1 |
        IdCard1 ! (Attr1:Value1),
        self.

% match returns Ok = true iff for each Pair in Pairs,
% where Pair = (Attribute : ValueList), Attribute exists in the IdCard
% and its value is an element of ValueList 
match(IdCard, Pairs, Ok) +(Include = true) :-
    Pairs ? (Attr : Values), Include = true, listener(IdCard) |
	check(IdCard, Attr, Values, Include'),
	self;

    Pairs ? (_ : not(_)) |
	computation#display("Negation of Value List NOT YET IMPLEMENTED"),
	IdCard = _, Pairs' = _, Include = _,
	Ok = false("NOT IMPLEMENTED");

    Pairs ? (_:_), Include = false |
	IdCard = _, Pairs' = _,
	Ok = false;

    Pairs = [] |
	IdCard = _,
	Ok = Include.

check(IdCard, Attr, Values, Include) :-

    IdCard ? (Attribute : _Value), Attribute =\= Attr |
	self;

    IdCard = [(Attr : Value) | _] |
	check_values(Values, Value, Include);
  
    IdCard = [] |
	Attr = _, Values = _,
	Include = false.

check_values(Values, Value, Include) :-
    Values = [Value | _] |
	Include = true;

    Values ? Val, Val =\= Value |
	self;

    Values = Value |
	Include = true;

    ground(Values), Values =\= Value | % covers the nil case as well
	Include = false.

/* Routine : remove_attribures 
-------------------------------------------------------
| INPUTS  :  
| OUTPUTS :
| DESCRIPTION : Remove IdFrame attributes from IdCard
-------------------------------------------------------
*/

remove_attributes(IdCard,IdFrame,IdCard1) :-
  IdCard ? (Attr : Value), ground(Attr), listener(IdFrame) |
	member(Attr,IdFrame,Ok),
	remove_if_ok(Ok?,(Attr : Value),IdCard1,IdCard1'?),
	self;

  IdCard = [] | 
	IdFrame = _,
	IdCard1 = [].
	  

member(Elem, List, Ok) :-

  List ? Elem  | Ok = true, List' = _;
  List = []    | Ok = false, Elem = _;
  List ? NotElem, NotElem =\= Elem | self.

remove_if_ok(Ok,Elem,List1,List2) :-
  Ok = true   | Elem = _, List1 = List2 ;
  Ok =\= true | List1 = [Elem | List2].

