/*

File: sio line_editor.cp
Date: 10 Jan 86
Source: Originally in tty.cp
Adapted by Norm McCain, Phil Thrift

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:34 $
Currently locked by 	$Locker:  $
			$Revision: 1.1.1.1 $
			$Source: /home/qiana/Repository/Logix/system/sio_server/line_editor.cp,v $

Copyright (C) Texas Instruments Inc. 1986

*/

-export([requests/1]).
-mode(trust).
-language(colon).

requests(In) :-
	ring#ring(Iring,20),
	ring#ring(Kring,10),
	edit(In,Iring,Kring).
	 
% The line editor maintains streams to an input ring and a kill ring.
% It handles the edit_line message, returning one line at a time.  It
% receives bytes from the keyboard (via sio) and responds to the editing
% keystrokes described below.  Other keystrokes insert characters into
% the line.
	 
edit([edit_line(KbdIn,Out,Echo,Done)|In],Iring,Kring) :-
	edit_line(KbdIn,Out,Echo,Iring\Iring1,Kring\Kring1,Done),
	edit(In,Iring1,Kring1).
edit([],[]^,[]^).

% In these descriptions "point" refers to the position in a line
% represented by left edge of the keyboard cursor.  It is always
% between characters (never on them).

% - Back-Space (and Delete) delete the character to the left of point.
% - Control-D deletes the character to the right of point.

% - Control-A moves point to the beginning of a line.
% - Control-E moves point to the end of a line.

% - Control-U kills all input to the left of point.
% - Control-K kills all input to the right of point.

% - Control-B moves point left one character in a line.
% - Control-F moves point right one character in a line.

% - Control-T swaps the characters just to the left and right of point.

% - Control-X interprets the following sequence of keystrokes (^P, ^N, and
%   ^I are acceptable) as directives aimed at the kill ring rather than the
%   input ring (which is the default.)

% - Control-I initializes either the input or kill ring.

% - Control-N retrieves the next entry in the input or kill ring.  Repeating
%   ^N (or ^P) gives successive next (or previous) entries in the same ring.
% - Control-P retrieves previous entries in the input or kill ring.  Repeating
%   ^P (or ^N) gives successive previous (or next) entries in the same ring.

% - Control-R re-displays the current line; point is unchanged.

% - Control-G terminates input and output.


edit_line(KbdIn\KbdIn1,Out,Echo,Iring,Kring,Done) :-
	edit_line(KbdIn,[],[],Echo,Out,KbdIn1,Iring,Kring,Done).

edit_line([],_Left,_Right,[]^,(Eos\_/Eos)^,[]^,
		(Iring\Iring)^,(Kring\Kring)^,true^
).
edit_line([Control_G|_],_Left,_Right,[]^,(Eos\_/Eos)^,[]^,
		(Iring\Iring)^,(Kring\Kring)^,true^
) :-
    Control_G =:= ascii('') |
	true.
edit_line([LF|Cs],[],[],[LF]^,(Out\_/Out)^,Cs^,
		(Iring\Iring)^,(Kring\Kring)^,true^
) :-
    LF =:= ascii(lf) |
	true.
edit_line([LF|Cs],Left,Right,Echo,(Out\Out1/_)^,Cs^,
		Iring,(Kring\Kring)^,Done
) :-
    otherwise,
    LF =:= ascii(lf) |		  % (Left==[_|_]; Right==[_|_])
	shift_rear(Right,Left,Echo,Left1,[LF]),
	reverse_line(Left1,Out\Out1,Iring,Done).
edit_line([BS|Cs],[],Right,[Bell|Echo]^,Out,Rs,Iring,Kring,Done) :-
    BS =:= ascii(bs) :
      ascii(bel,Bell) |
	edit_line(Cs,[],Right,Echo,Out,Rs,Iring,Kring,Done).
edit_line([BS|Cs],[_Erased|Left],Right,[BS|Echo]^,Out,Rs,Iring,Kring,Done) :-
    BS =:= ascii(bs) |
	display_right(Right,Echo,Echo1),
	edit_line(Cs,Left,Right,Echo1,Out,Rs,Iring,Kring,Done).
edit_line([Delete|Cs],[],Right,[Bell|Echo]^,Out,Rs,Iring,Kring,Done) :-
    Delete =:= ascii(del) :
       ascii(bel,Bell) |
	edit_line(Cs,[],Right,Echo,Out,Rs,Iring,Kring,Done).
edit_line([Delete|Cs],[_Erased|Left],Right,[BS|Echo]^,Out,Rs,
			Iring,Kring,Done
) :-
    Delete =:= ascii(del) :
      ascii(bs,BS) |
	display_right(Right,Echo,Echo1),
	edit_line(Cs,Left,Right,Echo1,Out,Rs,Iring,Kring,Done).
edit_line([Control_D|Cs],Left,[],[Bell|Echo]^,Out,Rs,Iring,Kring,Done) :-
    Control_D =:= ascii('') :
      ascii(bel,Bell) |
	edit_line(Cs,Left,[],Echo,Out,Rs,Iring,Kring,Done).
edit_line([Control_D|Cs],Left,[_Erased|Right],Echo,Out,Rs,Iring,Kring,Done) :-
    Control_D =:= ascii('') |
	display_right(Right,Echo,Echo1),
	edit_line(Cs,Left,Right,Echo1,Out,Rs,Iring,Kring,Done).
edit_line([Control_U|Cs],Left,Right,Echo,Out,Rs,
		Iring,([put(Left1)|Kring]\Kring1)^,Done
) :-
    Control_U =:= ascii('') |
	delete_left(Left,[],Left1,Right,Echo\Echo1),
	edit_line(Cs,[],Right,Echo1,Out,Rs,Iring,Kring\Kring1,Done).
edit_line([Control_K|Cs],Left,Right,Echo,Out,Rs,
		Iring,([put(Right)|Kring]\Kring1)^,Done
) :-
    Control_K =:= ascii('') |
	delete_right(Right,Echo\Echo1),
	edit_line(Cs,Left,[],Echo1,Out,Rs,Iring,Kring\Kring1,Done).
edit_line([Control_B|Cs],[],Right,[Bell|Echo]^,Out,Rs,Iring,Kring,Done) :-
    Control_B =:= ascii('') :
      ascii(bel,Bell) |
	edit_line(Cs,[],Right,Echo,Out,Rs,Iring,Kring,Done).
edit_line([Control_B|Cs],[C|Left],Right,[BS|Echo]^,Out,Rs,Iring,Kring,Done) :-
    Control_B =:= ascii('') :
      ascii(bs,BS) |
	edit_line(Cs,Left,[C|Right],Echo,Out,Rs,Iring,Kring,Done).
edit_line([Control_F|Cs],Left,[],[Bell|Echo]^,Out,Rs,Iring,Kring,Done) :-
    Control_F =:= ascii('') :
       ascii(bel,Bell) |
	edit_line(Cs,Left,[],Echo,Out,Rs,Iring,Kring,Done).
edit_line([Control_F|Cs],Left,[C|Right],[C|Echo]^,Out,Rs,Iring,Kring,Done) :-
    Control_F =:= ascii('') |
	edit_line(Cs,[C|Left],Right,Echo,Out,Rs,Iring,Kring,Done).
edit_line([Control_T|Cs],[],Right,[Bell|Echo]^,Out,Rs,Iring,Kring,Done) :-
    Control_T =:= ascii('') :
      ascii(bel,Bell) |
	edit_line(Cs,[],Right,Echo,Out,Rs,Iring,Kring,Done).
edit_line([Control_T|Cs],Left,[],[Bell|Echo]^,Out,Rs,Iring,Kring,Done) :-
    Control_T =:= ascii('') :
      ascii(bel,Bell) |
	edit_line(Cs,Left,[],Echo,Out,Rs,Iring,Kring,Done).
edit_line([Control_T|Cs],[C1|Left],[C2|Right],[BS,C2,C1,BS|Echo]^,Out,Rs,
		Iring,Kring,Done
) :-
    Control_T =:= ascii('') :
       ascii(bs,BS) |
	edit_line(Cs,[C2|Left],[C1|Right],Echo,Out,Rs,Iring,Kring,Done).
edit_line([Control_A|Cs],Left,Right,Echo,Out,Rs,Iring,Kring,Done) :-
    Control_A =:= ascii('') |
	shift_front(Left,Right,Echo,Right1,Echo1),
	edit_line(Cs,[],Right1,Echo1,Out,Rs,Iring,Kring,Done).
edit_line([Control_E|Cs],Left,Right,Echo,Out,Rs,Iring,Kring,Done) :-
    Control_E =:= ascii('') |
	shift_rear(Right,Left,Echo,Left1,Echo1),
	edit_line(Cs,Left1,[],Echo1,Out,Rs,Iring,Kring,Done).
edit_line([Control_P|Cs],Left,Right,Echo,Out,Rs,
	([previous(Cs \Cs1,Right,Echo\Echo1,Line)|Iring]\Iring1)^,Kring,Done
) :-
    Control_P =:= ascii('') |
	insert_left(Line,Left,Left1),
	edit_line(Cs1,Left1,Right,Echo1,Out,Rs,Iring\Iring1,Kring,Done).
edit_line([Control_N|Cs],Left,Right,Echo,Out,Rs,
	([next(Cs \Cs1,Right,Echo\Echo1,Line)|Iring]\Iring1)^,Kring,Done
) :-
    Control_N =:= ascii('') |
	insert_left(Line,Left,Left1),
	edit_line(Cs1,Left1,Right,Echo1,Out,Rs,Iring\Iring1,Kring,Done).
edit_line([Control_X|Cs],Left,Right,Echo,Out,Rs,Iring,Kring,Done) :-
    Control_X =:= ascii('') |
	kill_ring_entry(Cs,Left,Right,Echo,Out,Rs,Iring,Kring,Done).
edit_line([Control_I|Cs],Left,Right,Echo,Out,Rs,
		([initialize|Iring]\Iring1)^,Kring,Done
) :-
    Control_I =:= ascii('	') |
	edit_line(Cs,Left,Right,Echo,Out,Rs,Iring\Iring1,Kring,Done).
edit_line([Control_R|Cs],Left,Right,Echo,Out,Rs,Iring,Kring,Done) :-
    Control_R =:= ascii('') |
	display_left(Left,Echo,Echo1),
	display_right(Right,Echo1,Echo2),
	edit_line(Cs,Left,Right,Echo2,Out,Rs,Iring,Kring,Done).

%% for now we ignore the Esc character
edit_line([Esc|Cs],Left,Right,Echo,Out,Rs,Iring,Kring,Done) :-
    Esc =:= ascii('') |
	edit_line(Cs,Left,Right,Echo,Out,Rs,Iring,Kring,Done).
edit_line([C|Cs],Left,[],[C|Echo]^,Out,Rs,Iring,Kring,Done) :-
    otherwise |
	edit_line(Cs,[C|Left],[],Echo,Out,Rs,Iring,Kring,Done).
edit_line([C|Cs],Left,Right,[C|Echo]^,Out,Rs,Iring,Kring,Done) :-
    otherwise |
	display_right(Right,Echo,Echo1),
	edit_line(Cs,[C|Left],Right,Echo1,Out,Rs,Iring,Kring,Done).

kill_ring_entry([Control_P|Cs],Left,Right,Echo,Out,Rs,
	Iring,([previous(Cs\Cs1,Right,Echo\Echo1,Line)|Kring]\Kring1)^,Done
) :-
    Control_P =:= ascii('') |
	insert_left(Line,Left,Left1),
	edit_line(Cs1,Left1,Right,Echo1,Out,Rs,Iring,Kring\Kring1,Done).
kill_ring_entry([Control_N|Cs],Left,Right,Echo,Out,Rs,
	Iring,([next(Cs\Cs1,Right,Echo\Echo1,Line)|Kring]\Kring1)^,Done
) :-
    Control_N =:= ascii('') |
	insert_left(Line,Left,Left1),
	edit_line(Cs1,Left1,Right,Echo1,Out,Rs,Iring,Kring\Kring1,Done).
kill_ring_entry([Control_I|Cs],Left,Right,Echo,Out,Rs,
		Iring,([initialize|Kring]\Kring1)^,Done
) :-
    Control_I =:= ascii('	') |
	edit_line(Cs,Left,Right,Echo,Out,Rs,Iring,Kring\Kring1,Done).
kill_ring_entry([_|Cs],Left,Right,[Bell|Echo]^,Out,Rs,Iring,Kring,Done) :-
    otherwise :
       ascii(bel,Bell) |
	edit_line(Cs,Left,Right,Echo,Out,Rs,Iring,Kring,Done).
kill_ring_entry([],Left,Right,Echo,Out,Rs,Iring,Kring,Done) :-
	edit_line([],Left,Right,Echo,Out,Rs,Iring,Kring,Done).


display_left(Left,Echo,Echo1) :-
    true :
      ascii(bs, BS) |
	display_left(Left,BS,Echo,Echo1).

display_left([C|Left],BS,[BS|BSs]^,Echo) :-
	display_left(Left,BS,BSs,[C|Echo]).
display_left([],_,Echo^,Echo).

display_right(Right,Echo,Echo1) :-
    true : ascii(bs,BS) |
	display_right(Right,Echo,BS,Echo1).

display_right([C|Right],[C|Echo]^,BS,BSs) :-
	display_right(Right,Echo,BS,[BS|BSs]).
display_right([],[SP,BS|BSs]^,BS,BSs) :-
    true :
      ascii(' ', SP) |
	true.

reverse_line(Left,Out,Ring,Done) :-
	reverse_line(Left,Out,[],Ring,Done).

reverse_line([],(E\E)^,List,([put(List)|Ring]\Ring)^,true^).
reverse_line([C|Cs],E\Line,List,Ring,Done) :-
	reverse_line(Cs,E\[C|Line],[C|List],Ring,Done).

delete_left([],Out,Out^,_Right,(Echo\Echo)^).
delete_left([C|Cs],In,Out,Right,([BS|Echo]\Echo2)^) :-
    true :
      ascii(bs,BS) |
	display_right(Right,Echo,Echo1),
	delete_left(Cs,[C|In],Out,Right,Echo1\Echo2).

delete_right(Right,Echo\Echo1) :-
	delete_right(Right,Echo,Echo1).

delete_right([_|Right],[SP|Echo]^,BSs) :-
    true :
      ascii(bs,BS), ascii(' ',SP) |
	delete_right(Right,Echo,[BS|BSs]).
delete_right([],BSs^,BSs).

shift_front([C|Left],Right,[BS|Echo]^,Right1,Echo1) :-
    true :
      ascii(bs,BS) |
	shift_front(Left,[C|Right],Echo,Right1,Echo1).
shift_front([],Right,Echo,Right^,Echo^).

shift_rear([C|Right],Left,[C|Echo]^,Left1,Echo1) :-
	shift_rear(Right,[C|Left],Echo,Left1,Echo1).
shift_rear([],Left,Echo,Left^,Echo^).

insert_left([C|Line],Left,Left1) :-
	insert_left(Line,[C|Left],Left1).
insert_left([],Left^,Left).
