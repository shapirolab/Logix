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
      processor # [device(create(tty, Bytes)),
		   device(open(tty)),
		   terminal(channel(TCH))].
