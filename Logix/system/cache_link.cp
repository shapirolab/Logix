/* $Header: /home/qiana/Repository/Logix/system/cache_link.cp,v 1.1.1.1 1999/07/09 07:02:55 bill Exp $ */
-export([stream/5, named/4]).
-mode(trust).
-language(compound).


Processor ::= Integer ; String.
ServiceId ::= Nil ; [String].
PID ::= fwd ; bwd ; next.

procedure stream(Processor, ServiceId, Any, PID, Vector).

stream(Processor, ServiceId, Stream, PID, FanOut) :-

    integer(Processor),
    PN := Processor + 2 : PID = _,
      write_vector(PN, link(ServiceId, Stream), FanOut) ;

    string(Processor),
    ServiceId = [_ | Scope],
    N := arity(FanOut) |
	[cache_link | Scope] # named(Processor, PID, N, Processor'),
	stream;

    otherwise : PID = _, FanOut = _, Stream = _ |
	fail(ServiceId # ('*' @ Processor), no_link).


procedure named(String, PID, Integer, Processor).

named(String, PID, N, Processor) :-

    String = fwd |
	forward(PID, N, Processor);

    String = bwd |
	backward(PID, N, Processor);

    String = next |
	forward(PID, N, Processor);

    otherwise : PID = _, N = _,
      Processor = unknown(String) .


procedure forward(Integer, Integer, Integer).

forward(PID, N, CENumber) :-

    PID > 1,
    CENumber^ := (PID - 1) \ (N - 1) |
	true;

    N > 1, PID = 1 :
      CENumber = 0 ;

    otherwise : PID = _, N = _,
      CENumber = -1 .


procedure backward(Integer, Integer, Integer).

backward(PID, N, CENumber) :-

    PID > 1,
    CENumber^ := (PID + N - 4) \ (N - 1) |
	true;

    otherwise,
    CENumber^ := N - 2 : PID = _ .
