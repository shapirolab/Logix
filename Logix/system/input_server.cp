/*

Input Monitor.

William Silverman - 08/12/85

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:02:57 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/system/input_server.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export(serve/1).
-mode(interrupt).
-language(compound).

/*
	Serves: file(File), string(String), halt

	This monitor gets the data of the text file,  File  (path), or of
	the string  String , and passes it, one line at a time, to 
	computation # sio.  That service uses the characters in place of
	normal keyboard input.

 	Before sending each line, it waits for the system to prompt.  The
        prompt is suppressed, and the line is sent to  computation # sio
	which "echoes" it to the display and passes it to the shell as a
	shell command line.

	The input lines may include variables, which share the system
	bindings with normal console input.

	An input line may include an input command.  All input lines of the
	file designated by such a command are processed, following later
	commands on the current line - i.e. the following lines of the current
	input file are stacked until each new input file has been processed.

	To continue a command over several lines, end each line of the command,
	except for the last line, with the <begin comment> character, "%".
	Console input may be the continuation of such a line, when it is the
	last line of the file.  To run several commands concurrently, input
        them as a disjunction on one (continued) line.

	Since  input  works a line at a time, commands from the console
	may be entered between commands from the file.

	To abort all stacked input lines, request  halt .
*/

procedure serve(In).
procedure serve(In, Stack, Prompts).

serve(In) + (Stack = [], Prompts = []) :-

    In ? file(File) :
      Stack' = {Chars?, Stack} |
	computation_utils # call_id_goal(self#File, ServiceId, Name, Ok),
	get_chars(Ok, File, ServiceId, Name, Chars),
	serve,
	open_prompts(Prompts, Prompts');

     In ? halt : Stack = _ |
	serve(In', [], []),
	close_prompts(Prompts);

    In ? string(String),
    string_to_dlist(String, Chars, []) :
      Stack' = {Chars, Stack} |
	serve,
	open_prompts(Prompts, Prompts');

    In = [] : Stack = _ |
	close_prompts(Prompts);

    Stack = {Lines, Stack'} |
	wait(In, Prompts, Prompts'),
	lines(Lines, In, Stack', Prompts');

    Stack = [] :
      Prompts = {Close, Close} |
	serve(In, [], []);

    In ? Other,
    otherwise |
	fail(Other, unknown),
	serve.


procedure get_chars(SystemReply, File, ServiceId, Name, Chars).

get_chars(SystemReply, File, ServiceId, Name, Chars) :-

     SystemReply = true : File = _ |
	file # execute_in_context(ServiceId,
				  get_file(Name, String, [], _Status)
		),
	string_to_dlist(String, Chars, []);

    otherwise : ServiceId = _, Name = _,
      Chars = [] |
	fail(file(File), SystemReply).


procedure open_prompts(Prompts1, Prompts2).

open_prompts(Prompts1, Prompts2) :-

    Prompts1 = [] :
      Prompts2 = {Stream, Forward} |
	computation # sio(serve_prompts, Stream, Forward);

    otherwise :
      Prompts1 = Prompts2 .


procedure close_prompts(Prompts).

close_prompts(Prompts) :-

    Prompts = [] : true ;

    Prompts = {L, R} :
      L = R .


procedure wait(In, Prompts1, Prompts2).

wait(In, Prompts1, Prompts2) :-

    known(In) :
      Prompts1 = Prompts2 ;

    Prompts1 = {Stream, _},
    known(Stream) : In = _,
      Prompts1 = Prompts2 .


procedure lines(Chars, In, Stack, Prompts).

lines(Chars, In, Stack, Prompts) :-

    Chars ? C,
    Prompts = {[{_, Close, Close^} | Stream], Forward} |
	line(C, Chars', Chars'', Line),
	computation # sio(line, [C | Line?]),
	serve(In, {Chars''?, Stack}, {Stream, Forward});

    Chars = [] |
	serve(In, Stack, Prompts);

    known(In) |
	serve(In, {Chars, Stack}, Prompts);

    Prompts =  {[], R} : Chars = _, In = _, Stack = _,
      R = [] .


procedure line(Char, Chars1, Chars2, Line).

line(Char, Chars1, Chars2, Line) :-

    Char =:= ascii(lf) :
      Chars1 = Chars2,
      Line = [] ;

    otherwise : Char = _ |
	line1(Chars1, Chars2, Line).


procedure line1(Chars1, Chars2, Line).

line1(Chars1, Chars2, Line) :-

    Chars1 ? C :
      Line ! C |
	line(C, Chars1', Chars2, Line');

    Chars1 = [] :
      Chars2 = [],
      Line = [] .
