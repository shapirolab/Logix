/*  $Header: /home/qiana/Repository/Logix/system/Doors/doors.cp,v 1.1 1999/07/09 07:03:24 bill Exp $ */

-language(dfcp).
-mode(interrupt).
-export([close_door,  copy_door, corridor, decompose_door, door_terminator/1,
	 door_terminator1/2, enter_door, generic_reply, make_door,
	 merger, open_door, send_to_door, send_to_self_door]
).

corridor(H1,H2) :-
	H1=door(X,Y?),
	H2=door(Y,X?).

close_door(Door) :-
	Door=door(Snd?,Rcv) | 
		Snd=[],
		door_terminator(Rcv).

decompose_door(Door,Snd,Rcv) :-
	Door=door(Snd1?,Rcv1) | 
		Snd1=Snd,
		Rcv=Rcv1.


send_to_door(Message,Door1,Door2,Ok) :-
	Door1=door(Snd?,Rcv) | 
		Snd ! Message,
		Door2 = door(Snd',Rcv),
		Ok = true;
	invalid(Door1) |
		invalid_reason(Door1,Reason),
		Snd ! Message,
		Door2 = door(Snd',[]),
		door_terminator1(Snd?,invalid_door),
		Ok = false(invalid(Reason?)).


send_to_self_door(Message,Door1,Door2,Ok) :-
	Door1=door(Snd,Rcv') | 
		Rcv ! Message,
		Door2 = door(Snd,Rcv?),
		Ok = true.


door_terminator(Rcv) :-
	door_terminator1(Rcv,discarded_door).

door_terminator1(Rcv,Reason) :-
        Rcv = [] | Reason = _;
	invalid(Rcv) | Reason = _;
        Rcv ? Message, ground(Reason) |
                generic_reply(Message,false(Reason)),
                self.

generic_reply(Message,Reply) :-
%       computation#display(["doors - generic reply:", "Message was ",
%								 Message??]),
	tuple(Message) |
		tuple_to_dlist(Message?,List,[]),
	        extract_Ok(List?,Reply).

extract_Ok(List,Ok) + (Head, Tail) :-
	initially | Head = Tail? ;
        List = [Ok1?] |  Ok1 = Ok, Tail = Head;
        List ? Item, List' = [_|_] | Tail ! Item, self;
        otherwise |
		Tail = List,
		list_to_tuple(Head, Tuple),
		ok(Ok, broken_reply_variable - Tuple?).
		
% Given an Id and a stream Snd to a room, returns Door to door Id in room.

open_door(Id, Snd, Snd1, Door, Id1, Ok) :-
	corridor(Door, Door1),
	Snd ! message([Id],[], paste(Door1?, Id1, Ok1), Ok2), Snd'=Snd1,
	and([Ok1?,Ok2?],Ok).

% Given a door to agent creates a new door with Id to agent.
copy_door(Door, Door1, Door2, Id) :-
	Door=door(Snd?,Rcv) |
	make_door(Snd, Snd'?, Door2, Id),
	Door1=door(Snd',Rcv).


% Given a stream to agent creates a new door with Id to agent.
make_door(Snd1, Snd2, Door1, Id) :-
	corridor(Door1, Door2),
	Snd1 = [paste(Door2?, Id, Ok) | Snd2],
	ok(Ok?,make_door).


enter_door(Id,Door,Id1,Door1,Ok) :-
	Door=door(Snd,Rcv), ground(Id) |
	open_door(Id, Snd, Snd'?, Door2, Id2, Ok1), 
	enter_door1(Ok1?,Id,Snd',Rcv,Id2?,Door2?,Id1,Door1,Ok).

enter_door1(Ok1,Id,Snd,Rcv,Id2,Door2,Id1,Door1,Ok) :-
	Ok1 = true |
		Id1 = Id2, Door1 = Door2,
		close_door(door(Snd,Rcv)), Ok = true,
		Id = _;
	Ok1 =\= true |
		Id1 = Id, Door1 = door(Snd,Rcv), Ok = Ok1,
		Id2 = _, Door2 = _.

% Merge multiple input streams to a room.
% - fast multiway-merger uses mutual reference (channel).

procedure merger(Any, Any).

merger(In, Output) :-
	stream_merger(In, Out!, Out!, Done),
	filter_output(Out?, Output),
	close_channel(Done?) .

stream_merger(In, Merger, Left, Right) :-

    In ? Message,
    Message =\= merge(_),
    channel(Merger) |
	write_channel(Message, Merger, Merger'),
	self;

    In ? merge(In''),
    channel(Merger) |
	stream_merger(In', Merger, Left, Left'),
	stream_merger(In'', Merger, Left'?, Right);

    invalid(In) |
	 Merger = _,
	 Right = Left ;

    In = [] |
	Merger = _,
	Right = Left .

filter_output(Out, Output) :-

    Out ? Message,
    Message =\= ([_] : "_copy_output"(_,_)) |
	Output ! Message,
	self;

    Out ? ([_] : "_copy_output"(Copy?, Stop)) |
	copy_output(Out'?, Out'', Copy, Stop),
	self;

    Out = [] |
	Output = [] .


copy_output(In, Out, Copy, Stop) :-

    In ? T,
    known(T),				% known(INVALID) is true
    listener(Stop) |
	Out ! OutT?,
	copy_term(T, OutT, CopyT, done, Done, Stop),
	copy_done(Done?, CopyT?, Copy, Copy'?, Stop),
	self;

    In = [] |
	Out = [],
	Copy = [],
	Stop = _ ;

    Stop = true |
	Out = In,
	Copy = [];

    invalid(Stop) |
	Out = In,
	Copy = [] .


copy_term(T, OutT, CopyT, Left, Right, Stop) :-

    constant(T) |
	OutT = T,
	CopyT = T,
	Right = Left,
	Stop = _ ;

    vector(T),
    arity(T) > 1 |
	OutT = T,
	CopyT = "<VECTOR>",
	Right = Left,
	Stop = _ ;

    vector(T),
    arity(T) =:= 1 |
	OutT = T,
	CopyT = "<CHANNEL>",
	Right = Left,
	Stop = _ ;

    invalid(T),
    listener(T) |
	OutT = T,
	CopyT = T,
	Right = Left,
	Stop = _ ;

    T ? Term,
    read_only(Term),
    read_only(T'),
    listener(Stop) |
	OutT ! OutTerm?,
	CopyT ! CopyTerm?,
	copy_term(Term, OutTerm, CopyTerm, Left, Left', Stop),
	self;

    T ? T'',
    read_only(T''),
    writable(T'),
    listener(Stop) |
	OutT = [OutT'? | T'],
	CopyT = [CopyT'? | "<WRITER>"],
	self;

    T ? E,
    writable(E),
    read_only(T') |
	OutT ! E,
	CopyT ! "<WRITER>",
	self;

    T ? E,
    writable(E),
    writable(T') |
	OutT = T,
	CopyT = ["<WRITER>" | "<WRITER>"],
	Right = Left,
	Stop = _ ;

    tuple(T),
    T =\= door(_,_),
    listener(Stop) |
	tuple_to_dlist(T, T', []),
	self,
	if_list_to_tuple(OutT'?, OutT),
	if_list_to_tuple(CopyT'?, CopyT),
	copy_tuple(Right'?, Right, Stop);

    T = door(_,_) |
	OutT = T,
	CopyT = "<DOOR>",
	Right = Left,
	Stop = _;

    Stop = true |
	OutT = T,
	CopyT = "<??>",
	Right = Left;

    invalid(Stop) |
	OutT = T,
	CopyT = [],
	Right = Left .

copy_tuple(Left, Right, Stop) :-

    Left = done |
	Right = done,
	Stop = _;

    invalid(Stop) |
	Right = Left;

    Stop = true |
	Right = Left .

copy_done(Done, CopyT, Copy1, Copy2, Stop) :-

    Done = done |
	Copy1 = [CopyT | Copy2],
	Stop = _ ;

    Stop = true |
	Done = _,
	CopyT = _,
	Copy1 = Copy2;

    invalid(Stop) |
	Done = _,
	CopyT = [],
	Copy1 = Copy2 .

if_list_to_tuple(L, T) :-

    list(L) |
	utils # list_to_tuple(L,T);

    L = [] |
	T = [] .
