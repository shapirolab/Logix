/*

incremental display or grounding of variables.

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:02:54 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/system/incremental_display.cp,v $

Copyright (C) 1987, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-mode(trust).
-language(compound).
-export([display/1, display/2, display/3,
	   display_stream/1, display_stream/2, display_stream/3,
	   ground/2, ground/3, ground/4]).

Term	::= Any.
Format	::= {Depth, Length, Var_Format, Iterations}.
Ground_Format ::= Format.
Depth	::= Integer.
Length	::= Integer.
Self_Ref::= Integer.
Var_Format ::= String.
Iterations ::= Integer.
Outs	::= Non_Empty_List.
Ins	::= Outs.
Done	::= done.
OutV	::= Vector.
Circuit	::= Done-Done.
Non_Empty_List::= [Any] ; [Any | Non_Empty_List].
Ground_Result ::= {Ground_Term, Events}.
Ground_Term   ::= Term.
Events	::= [Event].
Event	::= {Name,E}.
Name	::= Integer ; String.
E	::= ref(Var) ; value(Term) ; nop.
Var	::= we(GID) ; ro(GID).
GID	::= Integer.
Xs	::= List.
Ys	::= List.
Zs	::= List.

% procedure display_stream(Term)+(Format=[])+(Break=_).

display_stream(List) :-
	display_stream(List,[],_).

display_stream(List,Format) :-
	display_stream(List,Format,_).

display_stream(List, Format, Break) :-

	unknown(Break),
	List = [ Term | List' ]   |
		display(Term, Format, Break),
		display_stream(List', Format, Break);

	known(Break) :
	    List = _, Format = _ ;

	List = [] :
	    Format = _, Break = _ .


% procedure display(Term)+(Format=[])+(Break=_).

display(Term) :-
	display(Term,[],_).

display(Term,Format) :-
	display(Term,Format,_).

display(Term,Format,Break) :-
	ground(Term,Outs,Format,Break),
	display_item(done,Outs,Format).


procedure display_item(Done, Outs, Format).

display_item(Done, Outs, Format) :-

	Outs = [] :
	    Done = _, Format = _ ;

	Done = done,
	Outs ? Out |
		append(Format, [type(ground),close(done,Done')], Options),
		computation # display(term, Out, Options),
		display_item.


procedure append(Xs, Ys, Zs).

append(Xs, Ys, Zs) :-

	Xs = [] : 
	    Ys = Zs ;

	Xs ? length(_) |
		append;

	Xs ? depth(_) |
		append;

	Xs ? X,
	X =\= depth(_), X =\= length(_) :
	    Zs ! X |
		append.


% procedure ground(Term, Outs).	%+(User_Format=[])+(Break=_).

ground(Term,Outs) :-
	ground(Term,Outs,[],_).

% procedure ground(Term,Outs,User_Format).

ground(Term,Outs,User_Format) :-
	ground(Term,Outs,User_Format,_).

% procedure ground(Term,Outs,User_Format,Break).

ground(Term,Outs,User_Format,Break) :-
	computation # display(options,Default_Options),
	format_options(Default_Options,Default_Format),
	format_options(User_Format,Format,Default_Format),
	incremental_ground(Break, Term, Format, Outs).
	

procedure format_options(Options, Ground_Format).
procedure format_options(Options, Ground_Format, Format).

format_options(Options, Ground_Format) + (Format={8,20,namevars,0}) :-

	Options = [] :
	    Ground_Format = Format ;

	Options?depth(Depth),
	Format={_OldDepth,Length,Type, Iterations} :
	    Format' = {Depth, Length, Type, Iterations} |
		format_options;

	Options?length(Length),
	Format={Depth,_OldLength,Type, Iterations} :
	    Format' = {Depth, Length, Type, Iterations} |
		format_options;

	Options?type(Type),
	Format={Depth,Length,_OldType, Iterations} :
	    Format' = {Depth, Length, Type, Iterations} |
		format_options;

	Options?iterations(Iterations),
	Format={Depth,Length,Type, _OldIterations},
	Iterations' := Iterations - 1 :
	    Format' = {Depth, Length, Type, Iterations'} |
		format_options;

	Options?Option,
	Option =\= depth(_), Option =\= length(_),
	Option =\= type(_),  Option =\= iterations(_) |
		format_options.


procedure incremental_ground(Break, Term, Format, Outs).

incremental_ground(Break, Term, Format, Outs) :-

	Term = (_Name = Value),
	known(Value) |
		iground_known(Break, Term, Format, Outs);

	Term =\= (_ = _) |
		iground_known(Break, Term, Format, Outs).


procedure iground_known(Break, Term, Format, Outs).

iground_known(Break, Term, Format, Outs) :-

	true :
	    make_channel(Vector, Stream) |
		close(Vector,Done),
		send(Stream, Outs),
		iground(Break, Term, Format, Vector, done-Done).


procedure close(Vector, Done).

close(Vector, Done) :-

	Done = done : 
	    close_channel(Vector) .


procedure send(Ins, Outs).

send(Ins, Outs) :-

	Ins ? I :
	    Outs ! I |
		send;

	Ins = [] :
	    Outs = [] .


procedure iground(Break, Term, Format, OutV, Circuit).

iground(Break, Term, Format, OutV, Circuit) :-

	unknown(Break),
	Format = {Depth, Length, Var_Format, _Iterations} :
	    Dict_Format = {Depth, Length, Var_Format} |
		computation # dictionary(ground,Term, Result, Dict_Format),
		iground_first(Break, Result, OutV, Format, Circuit);

	known(Break),
	Circuit = L - R :
	    Term = _, Format = _, OutV = _,
	    L = R .

procedure iground_first(Break, Ground_Result, OutV, Format, Circuit).

iground_first(Break, Ground_Result, OutV, Format, Circuit) :-

	Ground_Result = {Ground_Term, Events} :
	    write_channel(Ground_Term, OutV) |
		do_events(Break, Events, OutV, Format, Circuit).


procedure do_events(Break, Events, OutV, Format, Circuit).

do_events(Break, Events, OutV, Format, Circuit) :-

	Events ? Event,
	Circuit = L - R :
	    Circuit' = M - R |
		do_event(Break, Event, Format, OutV, L - M),
		do_events;

	Events = [],
	Circuit = L - R :
	    Break = _, OutV = _, Format = _,
	    L = R .

procedure do_event(Break, Event, Format, OutV, Circuit).

do_event(Break, Event, Format, OutV, Circuit) :-

	unknown(Break),
	Event = {Name, ref(What)},
	Circuit = L - R,
	Format = {_Depth, _Length, Var_Format, Iterations},
	Iterations > 0 :
	    L = R,
	    write_channel(Name=Local_Id, OutV) |
		computation #
		    dictionary(globalid_to_name, What, Var_Format, Local_Id);

	unknown(Break),
	Event = {Name, value(What)},
	Format = {Depth, Length, Var_Format, Iterations},
	Iterations > 0,
	Iterations' := Iterations - 1 :
	    Format'  = {Depth, Length, Var_Format, Iterations'} |
		iground(Break, Name=What, Format', OutV, Circuit);

	unknown(Break),
	Event = {_Name, nop},
	Circuit = L - R :
	    Format = _, OutV = _,
	    L = R ;

	known(Break),
	Circuit = L - R :
	    Event = _, Format = _, OutV = _,
	    L = R ;

	Format = {_Depth, _Length, _Var_Format, Iterations},
	Iterations = 0,
	Circuit = L - R :
	    Break = _, Event = _, Format = _, OutV = _,
	    L = R .
