/*

synchronized input/output server
Ehud Shapiro, March 22, 1985
Modified: Norm Mccain, Phil Thrift 8 Jan 86
Revised: Bill Silverman 1986-1989

Last update by		$Author: bill $
			$Date: 1999/07/09 07:03:34 $
Currently locked by	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/system/sio_server/server.cp,v $
 
Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL
 
*/

-export([initial/5]).
-mode(trust).
-language(compound).

initial(Bytes, In, Edit, LineOut, TC) :-
	prompter(Stream),
	server(Bytes, In, {' ',Stream}, true, {Edit, LineOut, TC}).

/*
** Bytes : bytes from the keyboard
**Prompt : String(Stream)
**		String used to prompt for new input.
**        	Stream for "spontaneous" prompt.
**    In : requests for the output display.
**  Done : prompt needed - false => already displayed.
**  Outs : Output Streams - {Edit, Lines, TC}
**		Edit    : requests to editor.
**		Lines   : edited bytes from the keyboard, line at a time.
**		TC	: Channel to terminal output
*/

server(Bytes, In, Prompt, Done, Outs) :-

    Done = false,
    Bytes ? C :
      ascii(' ', Space),
      ascii('.', Dot),
      ascii(lf, LF) |
	output(output, [bytes(Echo?)], _, Outs, Edit, Outs'),
	edit(Edit,
	     edit_line([C | Bytes'] \ Bytes'',
		       Line \ [Space, Dot, LF | Line'] / Line',
		       Echo,
		       Done'
	     ),
	     Line, Line'
	),
	self;

    Done =\= false,
    Bytes ? C,
    Prompt = String(_Stream) :
      ascii(' ', Space),
      ascii('.', Dot),
      ascii(lf, LF) |
	output(prompt,[String,bytes(Echo?)], _, Outs, Edit, Outs'),
	absorb_interrupt([C | Bytes'], Bytes''),
	edit(Edit,
	     edit_line(Bytes'' \ Bytes''',
		       Line \ [Space,Dot,LF|Line'] / Line',
		       Echo,
		       Done'
	     ),
	     Line, Line'
	),
	self;

    In ? Command(Common) |
	verify_command(Command, Common, Command'),
	serve(Bytes, Command', In', Prompt, Done, Outs);

    Done = true,
    unknown(In), unknown(Bytes) |
	processor # machine(idle_queue(Idle, 0)),	% low-priority wait
	self + (Done = wait(Idle));

    Done = wait(Idle),
    unknown(In),
    unknown(Bytes),
    known(Idle),
    Prompt = String(Stream) :
      Stream ! String(Outs, Outs') |
	% just so you'll know that the system is idle
	self + (Prompt = String(Stream'), Done = false);

    Bytes = [],
    Prompt = _String(Stream),
    Outs = {Edit, Lines, _TC} : Done = _,
      Stream = [],
      Edit = [],
      Lines = [] |
	close_common_in.

  close_common_in(In) :-
    In ? _(Common), writable(Common) :
      Common = done |
	self;
    In ? Done(_), known(Done) |
	self;
    In = [] | true.

verify_command(In, Common, Out) :-

    In = output(Message) :
      Out = output(Message, Common) ;

    In = query(Message, Answer) :
      Out = query(Message, Answer, Common) ;

    In = prompt(_Prompt) :
      Common = done,
      Out = In ;

    In = serve_prompts(New, Old),
    var(New) :
      Common = done,
      New = New'?,
      Out = serve_prompts(New', Old);

    In = line(_Line) :
      Common = done,
      Out = In ;

    In = filter(New, Old),
    var(Old) :
      Common = done,
      Old = Old'?,
      Out = filter(New, Old');

    otherwise :
      Out = unknown(In, Common) .


serve(Bytes, Command, In, Prompt, Done, Outs) :-

    Command = output(Message,Common) |
	output(output, Message, Common, Outs, {NoEdit, NoEdit}, Outs'),
	server;

    Command = outputln(Message,Common) |
	output(outputln, Message, Common, Outs, {NoEdit, NoEdit}, Outs'),
	server;

    Command = prompt(NewPrompt),
    Prompt = _OldPrompt(Prompts),
    known(Done) :
      Prompt' = NewPrompt(Prompts),
      Done' = true |
	server;

    Command = serve_prompts(NewPrompts, OldPrompts),
    Prompt = String(Prompts),
    known(Done) :
      Prompts = OldPrompts,
      Prompt' = String(NewPrompts),
      Done' = true |
	server;

    Command = query(Question, Answer, Common),
    known(Done) |
	ask_query(Question, Answer, Common, Bytes, Bytes', Outs, Outs', Done'),
	server;

    Command = line(List),
    known(Done),
    unknown(Bytes),
    list(List) :
      ascii(' ', Space),
      ascii('.', Dot),
      ascii(lf, LF) |
	output(output, [bytes([Space | Echo])], _, Outs, Edit, Outs'),
	edit(Edit,
	     edit_line(List \ _,
		       Line \ [Space, Dot, LF | Line'] / Line',
		       Echo,
		       Done'
		),
		Line, Line'
	),
	server;

    Command = filter(Bytes', Old) :
      Old = Bytes |
	server;

    Command = unknown(Other, Common) |
	unknown(Other, Common),
	server.

% serve "ready" prompt

prompter([String(Outs, Outs') | Prompts]) :-
	output(prompt, [String], _, Outs, {NoEdit, NoEdit}, Outs'),
	prompter(Prompts).
prompter([]).

output(	Functor, Output, Common,
	{Edit, Line, TC},
	{ {Edit,Line}, {Edit',Line'} }^,
	{Edit', Line', TC'}^
) :-
	write_output(Functor, Output, Common, TC, TC').

  write_output(Functor, Output, Common, TC1, TC2) :-
    channel(TC1) :
      write_channel(Functor(Output, Common), TC1, TC2);
    otherwise,			% Closed/Bad channel
    writable(Common) : Functor = _, Output = _,
      Common = done,		% Not handling queries properly
      TC2 = TC1;
    known(Common) : Functor = _, Output = _,
      TC2 = TC1.

edit(	{ {[EditLine | Edit], Line}, {Edit, Line'} }^,
	EditLine,
	Line, Line'
).


absorb_interrupt(In, Out) :-

    In ? C,
    C =< ascii(' '),
    C =\= ascii('') :
      Out = In' ;

    otherwise :
      Out = In .


ask_query(Question, Answer, Common, Bytes, Bytes2, Outs, Outs1, Done) :-

    Answer = char(Char) |
	output(output, [Question, confirm(Ok), A?, '
'	       | Remark?],
	       Commit,
	       Outs,
	       {NoEdit, NoEdit},
	       Outs1
	),
	flush_keyboard(Bytes, Bytes1, Ok, Common, Commit),
	get_answer(Bytes1, Bytes2, A, Done),
	set_answer(Done, A, Char, Remark);

    Answer = chars(Reply) :
      ascii(' ', Space),
      ascii('.', Dot),
      ascii(lf, LF) |
	output(output, [Question, ' ', confirm(Ok), bytes(Echo?) | Remark?],
		Commit,
		Outs, Edit, Outs1
	),
	edit(	Edit,
		edit_line(	Bytes1 \ Bytes2,
				L \ [Space, Dot, LF | T] / T,
				Echo,
				Done
		),
		Line, Line
	),
	flush_keyboard(Bytes, Bytes1, Ok, Common, Commit),
	set_answer(Done, L \ T, Reply, Remark);

    Answer = string(String) |
	output(output, [Question, ' ', confirm(Ok), bytes(Echo?) | Remark?],
		Commit,
		Outs, Edit, Outs1
	),
	flush_keyboard(Bytes, Bytes1, Ok, Common, Commit),
	edit(	Edit,
		edit_line(Bytes1 \ Bytes2, Reply \ LF / LF, Echo, Done),
		Line, Line
	),
	reply_to_line(Done, Reply, LF, L, Done1),
	set_answer(Done1, L, String, Remark);

    otherwise : Answer = _,
      Bytes = Bytes2,
      Done = true |
	output(output, [Question, "
sio: UnAnswerable
"			],
		Common,
		Outs,
		{NoEdit, NoEdit},
		Outs1
	).


set_answer(true, Unify, Unify^, []^).
set_answer(_, _Reply, _DoesntUnify, ['sio: cant_unify_with_Answer
']^
) :-
    otherwise |
	true.


flush_keyboard(Bytes, Bytes1, Ok, Common, Commit) :-

    unknown(Ok),
    unknown(Common),
    Bytes ? _ |
	self;    

    Ok = true :
      Bytes = Bytes1 |
	unify_without_failure(Common, Commit);

    known(Common) : Ok = _,
      Bytes = Bytes1 |
	unify_without_failure(Common, Commit).


get_answer([], []^, ''^, true^).
get_answer([Control_G | _], []^, ''^, true^) :-
    Control_G =:= ascii('') |
	true.
get_answer([C | Bytes],
	   Bytes^,
	   Answer,
	   true^
) :-
    otherwise,
    list_to_string([C], Answer^) |
	true.


reply_to_line(true, Reply, [LF]^, Line, true^) :-
    true :
      ascii(lf, LF) |
	list_to_string(Reply, Line).
