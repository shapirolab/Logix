/*

File: ring.cp
Authors: Phil Thrift, Norm McCain
Date: 10 Jan 86

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:34 $
Currently locked by 	$Locker:  $
			$Revision: 1.1.1.1 $
			$Source: /home/qiana/Repository/Logix/system/sio_server/ring.cp,v $

Copyright (C) Texas Instruments Inc. 1986

*/

-export([ring/2]).
-mode(trust).
-language(compound).

procedure ring(In, Size).

ring(In, Size) :-
    make_tuple(Size, Tuple) |
	stream # distributor(Dist, Tuple),
	lines(Size, Tuple),
	ring_server(1, 0, Size, In, Dist).
 

procedure lines(N, Tuple).

lines(N, Tuple) :-

    N > 0,
    N' := N - 1,
    arg(N, Tuple, S) |
	line(S, []), 
	lines;

    N =< 0 : Tuple = _ .

 
procedure line(S, Line).

line(S, Line) :-

    S ? get(Line') :
      Line = Line' |
	line;

    S ? put(Line') : Line = _ |
	line;

    S = [] : Line = _ .


procedure initialize_ring(N, Dist, Dist1).

initialize_ring(N, Dist, Dist1) :-

    N > 0,
    N' := N - 1 :
     Dist ! (N # put([])) |
	initialize_ring;

    N = 0 :
      Dist = Dist1 .


procedure ring_server(Top, Fill, Size, In, Dist).

ring_server(Top, Fill, Size, In, Dist) :-

    In ? put(Line),
    Fill = 0 |						% The ring is empty.
	ring_server1(Line, [], Top, Fill, Size, In', Dist);

    In ? put(Line),
    Fill > 0, Top > 1,
    Top1 := Top - 1 :
      Dist ! Top1 # get(Line1) |
	ring_server1(Line, Line1, Top, Fill, Size, In', Dist');

    In ? put(Line),
    Fill > 0, Top = 1 :
      Dist ! Fill # get(Line1) |
	ring_server1(Line, Line1, Top, Fill, Size, In', Dist');

    In ? previous(DCs, _Right, DEchos, Line),
    Fill = 0 :						% The ring is empty.
      DCs = Cs\Cs,
      ascii(bel, Bell), DEchos = [Bell | Echo] \ Echo,
      Line = [] |
	ring_server;

    In ? previous(Cs1\Cs2, Right, Echo\Echo2, Line),
    Fill > 0, Top > 1,
    Top1 := Top - 1 :
      Dist ! Top1 # get(Line1) |
	display_line(Line1, Right, Echo, Echo1),
	retrieve(Cs1, Cs2, Right, Echo1, Echo2, Line1, Line, Top1, Fill,
			Dist', Dist''
	), 
	ring_server;

    In ? previous(Cs1\Cs2, Right, Echo\Echo2, Line),
    Fill > 0, Top = 1 :
      Dist ! Fill # get(Line1) |
	display_line(Line1, Right, Echo, Echo1),
	retrieve(Cs1, Cs2, Right, Echo1, Echo2, Line1, Line, Fill, Fill,
			Dist', Dist''
	),
	ring_server;

    In ? next(DCs, _Right, DEchos, Line),
    Fill = 0 :						% The ring is empty.
      DCs = Cs\Cs,
      ascii(bel, Bell), DEchos = [Bell | Echo] \ Echo,
      Line = [] |
	ring_server;

    In ? next(Cs1\Cs2, Right, Echo\Echo2, Line),
    Fill > 0,
    Top1 := Top\Fill + 1 :
      Dist ! Top1 # get(Line1) |
	display_line(Line1, Right, Echo, Echo1),
	retrieve(Cs1, Cs2, Right, Echo1, Echo2, Line1, Line, Top1, Fill,
			Dist', Dist''
	),
	ring_server;

    In ? initialize : Top = _, Fill = _,
      Top' = 1, Fill' = 0 |
	initialize_ring(Size, Dist, Dist'),
	ring_server;

    In = [] : Top = _, Fill = _, Size = _,
      Dist = [] .


procedure ring_server1(Line, Line1, Top, Fill, Size, In, Dist).

ring_server1(Line, Line1, Top, Fill, Size, In, Dist) :-

    Line = Line1 |
	ring_server(Top, Fill, Size, In, Dist);

    Line =\= Line1,
    Fill = Size,					% The ring is full.
    Top' := Top\Fill + 1 :
      Dist ! Top # put(Line) |
	ring_server(Top', Fill, Size, In, Dist');

    otherwise,
    Fill' := Fill + 1,					% The ring is not full.
    Top' := Top\Size + 1 : Line1 = _,
      Dist ! Top # put(Line) |
	ring_server(Top', Fill', Size, In, Dist').
 

procedure retrieve(Cs, CTs, Right, Echo, EchoTail, InLine, Line, N, Fill,
			Dist, DistTail
).

retrieve(Cs, CTs, Right, Echo, EchoTail, InLine, Line, N, Fill, Dist, DistTail
) :-

    Cs ? Control_P, Control_P =:= ascii(''),
    N > 1,
    N' := N - 1 :
      Dist ! N' # get(InLine') |
	erase_line(InLine, Right, Echo, Echo'),
	display_line(InLine', Right, Echo', Echo''),
	retrieve;

    Cs ? Control_P, Control_P =:= ascii(''),
    N = 1 :
      Dist ! Fill # get(InLine'),
      N' = Fill |
	erase_line(InLine, Right, Echo, Echo'),
	display_line(InLine', Right, Echo', Echo''),
	retrieve;

    Cs ? Control_N, Control_N =:= ascii(''),
    N' := N\Fill + 1 :
      Dist ! N' # get(InLine') |
	erase_line(InLine, Right, Echo, Echo'),
	display_line(InLine', Right, Echo', Echo''),
	retrieve;

    otherwise : Right = _, N = _, Fill = _,
      Cs = CTs,
      Echo = EchoTail,
      InLine = Line,
      Dist = DistTail .
	

procedure display_line(Line, Right, Echo, EchoTail).
 
display_line(Line, Right, Echo, EchoTail) :-

    Line ? C :
      Echo ! C |
	display_line;

    Line = [] |
	display_right(EchoTail, Right, Echo).


procedure erase_line(Line, Right, Echo, EchoTail).

erase_line(Line, Right, Echo, EchoTail) :-
	erase_right(BackSpaceTail, Right, Echo),
	erase_left(Line, BackSpaceTail, EchoTail).


procedure erase_right(BackSpaces, Right, Echo).

erase_right(BackSpaces, Right, Echo) :-

    Right ? _ :
      ascii(' ', SP),
      Echo ! SP,
      ascii(bs, BS),
      BackSpaces' = [BS | BackSpaces] |
	erase_right;

    Right = [] :
      Echo = BackSpaces .


procedure erase_left(Line, Erase, Echo).

erase_left(Line, Erase, Echo) :-

    Line ? _ :
      ascii(bs, BS), ascii(' ', SP),
      Erase = [BS, SP, BS | Erase'] |
	erase_left;

    Line = [] :
      Erase = Echo .


procedure display_right(BackSpaces, Right, Echo).

display_right(BackSpaces, Right, Echo) :-

    Right ? C :
      Echo ! C,
      ascii(bs, BS),
      BackSpaces' = [BS | BackSpaces] |
	display_right;

    Right = [] :
      ascii(bs, BS), ascii(' ', SP),
      Echo = [SP, BS | BackSpaces] .
