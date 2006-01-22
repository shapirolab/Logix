/*

Stream server

 Ehud Shapiro, Muli Safra, and Jim Crammond, 01-04-85
 Added distributor, Ehud Shapiro, 15-09-85

Last update by		$Author: bill $
		       	$Date: 2006/01/22 10:28:49 $
Currently locked by 	$Locker:  $
			$Revision: 1.2 $
			$Source: /home/qiana/Repository/Logix/stream.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/


-export([merger/2, distributor/2, hash_table/1]).
-mode(failsafe).
-language(compound).

%  fast multiway-merger uses mutual reference

procedure merger(Any, Any).

merger(In, Output) :-
    true :
      make_channel(Reference, Output) |
	stream_merger(In, Reference, {1, Done}),
	stream_merger_close(Done, Reference).

stream_merger_close(N, Reference) :-

    N = 1 :
      close_vector(1, Reference) ;

    N > 1,
    N' := N - 1 :
      close_vector(N, Reference) |
	stream_merger_close;

    N = 0 : Reference = _ ;

    otherwise,
    N' := N - 1 |
	stream_merger_close.


% stream_merger is optimized for the uni-processor emulator. ******************

stream_merger(In, Reference, Circuit) :-

    In ? Message,
    Message =\= merge(_) :
      write_channel(Message, Reference, Reference') |
	stream_merger;

    In ? merge(In''),
    Circuit = {Left, Right} :
      Circuit' = {Middle, Right} |
	stream_merger,
	stream_merger(In', Reference, {Left, Middle});

    invalid(In) : Reference = _,
      Circuit = {Done, Done} ;

    In = [] : Reference = _,
      Circuit = {Done, Done} .

%	Generate fast N-way distributor.

procedure distributor([Any], ([[Any]] ; Tuple)).

distributor(In, Streams) :-

    tuple(Streams),
    arity(Streams, N) :
      make_vector(N, Reference, Streams) |
	stream_distributor(In, Reference, {N, Done}),
	stream_merger_close(Done, Reference);

    list(Streams) |
	fanout(Streams, _, Tuple, 0),
	distributor(In, Tuple);

    Streams = [] |
	stream_distributor(In, [], _).


fanout(Streams, Tuple1, Tuple2, N) :-

    Streams ? Stream,
    N' := N + 1 |
	arg(N', Tuple1, Stream, Tuple2', Tuple2),
	fanout;

    Streams = [],
    0 < N,
    make_tuple(N, Tuple) :
      Tuple1 = Tuple,
      Tuple2 = Tuple .


arg(N, Tuple, Stream, Done, Done^) :-
    arg(N, Tuple, Stream^) |
	true.


% fast multiway merge-distributor that uses mutual reference.

% stream_distributor is optimized for the uni-processor emulator. *************

stream_distributor(In, Reference, Circuit) :-

    In ? StreamNo # Message :
      write_vector(StreamNo, Message, Reference, Reference') |
	stream_distributor;

    In ? merge(In''),
    Circuit = {Left, Right} :
      Circuit' = {Middle, Right} |
	stream_distributor,
	stream_distributor(In', Reference, {Left, Middle});

    In = [] : Reference = _,
      Circuit = {Done, Done} .

/*

Manage a dynamic hash table of Values, indexed by various Keys.

messages:

	lookup(Key?,NewValue?,OldValue!,Status^)
		replace OldValue by NewValue indexed by Key;
			Status = old.
		else add NewValue to hash-table, indexed by Key;
			Status = new.
	delete(Key?,OldValue!,Ok^) :- 
		delete OldValue indexed by Key.
	insert(Value,NewKey^)
		Add an entry for a distinct new key NewKey with
                value Value.
	member(Key?,CurrentValue!,Ok^) :-
		CurrentValue is in hash-table, indexed by Key.
	replace(Key?,NewValue?,OldValue!,Ok^)
		replace OldValue by NewValue, indexed by Key.
	send(Key?,Message?,Ok^) :-
		replace(Key,Stream,[Message|Stream],Ok).
	entries(List^) :-
		List is a list of every entry(Key,Value).

Key may be a number, a string, a simple tuple or a list of strings.
If Key is any other term, it must be ground - it is frozen, and the
frozen string serves as a new key.

The hash-table grows and shrinks by powers of 2, in the range 8 .. 256,
whenever the number of entries is more then twice or less then half the
number of buckets.

*/

HashTableCommand ::=	lookup(Key, Any, (old; new)) ; 
			insert(Any, Key) ;
			delete(Key, OK) ;
			member(Key, Any, OK) ;
			replace(Key, Any, Any, OK) ;
			send(Key, Any, OK) ;
			entries([entry(Key, Any)]).


Key ::= Number ; String ; String(String) ; String(Number) ; [String].

OK    ::= true ; false .


procedure hash_table([HashTableCommand]).

hash_table(In) + (N = 8, I = 0) :-
	make_tuple(N, BucketsTuple),
	initialize_buckets(N, BucketsTuple),
	distributor(Distribute, BucketsTuple),
	serve_hash(In, N, I, Distribute).


initialize_buckets(N, T) :-

    arg(N, T, In),
    N' := N - 1 |
	initialize_buckets,
	serve_hash_bucket(In, []);

    N = 0 : T = _ .


serve_hash(In, N, I, Out) + (P = 0) :-

    In ? lookup(Key, NewValue, OldValue, (Ok?)^) :
      Out ! Index # member(Key, OldValue, Reply) |
	key_index(Key, N, Index),
	lookup_reply(Reply, NewValue, I, I', Ok),
	more_entries(In', N, I', Out', P, Ok);

    In ? insert(Value, (NewKey?)^),
    NewKey := I + 1 :
      Out ! Index # member(NewKey, _, not_found(NewKey,[entry(NewKey, Value)]))
	|
	key_index(NewKey, N, Index),
	more_entries(In', N, NewKey, Out', P, new);

    In ? delete(Key, Value, (Ok?)^) :
      Out ! Index#delete(Key, Value, Ok) |
	key_index(Key, N, Index),
	less_entries(In', N, I, Out', P, Ok);

    In ? member(Key, Value, (Ok?)^) :
      Out ! Index # member(Key, Value, Reply) |
	serve_hash,
	key_index(Key, N, Index),
	member_reply(Reply, Value, Ok);

    In ? replace(Key, NewValue, OldValue, (Ok?)^) :
      Out ! Index # member(Key, OldValue, Reply) |
	serve_hash,
	key_index(Key, N, Index),
	member_reply(Reply, NewValue, Ok);

    In ? send(Key, Message, (Ok?)^) :
      Out ! Index # member(Key, [Message | Stream?], Reply) |
	serve_hash,
	key_index(Key, N, Index),
	member_reply(Reply, Stream, Ok);

    In ? entries((List?)^) |
	serve_hash,
	all(N, entries, Out, Out', List, []);

    In = [] : N = _, I = _, P = _,
      Out = [] .


key_index(Key, N, Index) :-

    Index^ := string_hash(Key) \ N + 1 |
	true;

    Key >= 1,
    Index^ := integer(Key) \ N + 1 |
	true;

    Key < 1,
    Index^ := integer(1-Key) \ N + 1 |
	true;

    real(Key),
    convert_to_string(Key, Key') |
	self;

    Key = S(I), I < 1,
    Index^ := (string_hash(S) + integer(1-I)) \ N + 1 |
	true;

    Key = S(I), I >= 1,
    Index^ := (string_hash(S) + integer(I)) \ N + 1 |
	true;

    Key = S1(S2),
    Index^ := (string_hash(S1) + string_hash(S2)) \ N + 1 |
	true;

    Key = [] : N = _,
      Index = 1 ;

    Key = [S1],
    Index^ := string_hash(S1) \ N + 1 |
	true;

    Key = [S1, S2],
    Index^ := (string_hash(S1) + string_hash(S2)) \ N + 1 |
	true;

    Key = [S1, S2, S3],
    Index^ := (string_hash(S1) + string_hash(S2) + string_hash(S3)) \ N + 1 |
	true;

    otherwise,
    ground(Key),
    freeze(Key, Key', _) |
	self.
	
member_reply(found(Key, entry(Key, Value)^), Value, true^).
member_reply(not_found(_, []^), _, false^).


lookup_reply(found(Key, entry(Key, Value)^), Value, I, I^, old^).
lookup_reply(not_found(Key, [entry(Key, Value)]^), Value, I, I', new^) :-
	max(Key, I, I').


max(Key, I1, I2) :-

    Key > I1 :
      Key = I2 ;

    otherwise : Key = _,
      I1 = I2 .


more_entries(In, N, I, Out, P, Ok) :-

    Ok = new,
    P' := P + 1 |
	more_buckets(In, N, I, Out, P');

    Ok = old |
	serve_hash(In, N, I, Out, P).


more_buckets(In, N, I, Out, P) :-

    2*N =:= P,
    N < 256 |
	rehash(In, N, I, Out, P);

    otherwise |
	serve_hash(In, N, I, Out, P).


less_entries(In, N, I, Out, P, Ok) :-

    Ok = true,
    P' := P - 1 |
	less_buckets(In, N, I, Out, P');

    Ok = false |
	serve_hash(In, N, I, Out, P).


less_buckets(In, N, I, Out, P) :-

    2*P =:= N,
    N > 8 |
	rehash(In, N, I, Out, P);

    otherwise |
	serve_hash(In, N, I, Out, P).


rehash(In, N, I, Out, P) :-
	all(N, reenter, Out, [], In', In),
	hash_table(In', P, I).


all(N, Package, Out1, Out2, Es1, Es2) :-

    N > 0,
    N' := N - 1 :
      Out1 ! N # entries(Entries) |
	all,
	package(Package, Entries, Es1, Es1');

    N = 0 : Package = _,
      Out1 = Out2,
      Es1 = Es2 .


package(Package, Entries, Es1, Es2) :-

    Package = entries,
    Entries ? Entry :
      Es1 ! Entry |
	package;

    Package = reenter,
    Entries ? entry(Key, Value) :
      Es1 ! lookup(Key, Value, Value, _) |
	package;

    Entries = [] : Package = _,
      Es1 = Es2 .


serve_hash_bucket(In, Entries) :-

    In ? member(Key, Value, Reply) |
	serve_hash_bucket,
	member(Key, Value, Entries, Entries', Reply);

    In ? delete(Key, Value, Ok) |
	serve_hash_bucket,
	delete(Key, Value, Entries, Entries', Ok);

    In ? entries(Entries^) |
	serve_hash_bucket;

    In = [] : Entries = _ .


member(Key, Value, Es1, Es2, Ok) :-

    Es1 ? Entry, Entry = entry(Other, _),
    Key =\= Other :
      Es2 ! Entry |
	member;

    Es1 ? entry(Key, Value^) :
      Es2 = [NewEntry | Es1'],
      Ok = found(Key, NewEntry) ;

    Es1 ? Entry, Entry = entry(Key, Other),
    Other =\= Value :
      Es2 ! Entry |
	member;

    Es1 = [] : Value = _,
      Ok = not_found(Key, Es2) .


delete(Key, Value, Es1, Es2, Ok) :-

    Es1 ? Entry, Entry = entry(Other, _),
    Key =\= Other :
      Es2 ! Entry |
	delete;

    Es1 ? entry(Key, Value^) :
      Es1' = Es2,
      Ok = true ;

    Es1 ? Entry, Entry = entry(Key, Other),
    Other =\= Value :
      Es2 ! Entry |
	delete;

    Es1 = [] : Key = _, Value = _,
      Es2 = [],
      Ok =  false .
