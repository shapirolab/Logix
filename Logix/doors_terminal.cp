-language([inherit,dfcp]).
-mode(trust).
-export(io).

/*
** io/2 - connect to io servers.
**
** Bytes from tty input;
** TCH channel to tty output.
*/

io(Bytes, TCH) :-
	processor # [terminal(channel(TCH)),
		     device(create(tty, Bytes))].
