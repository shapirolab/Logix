/* $Header: /home/qiana/Repository/Logix/system/array.cp,v 1.1 1999/07/09 07:02:53 bill Exp $ */
-mode(failsafe).
-export([make/3]).
-language(compound).

N ::= Integer.
In ::= [Command].
Ok ::= Tuple ; false.
Command ::= read(Integer, Any) ; write(Integer, Any).
A ::= Vector.


procedure make(N, In, Ok).

make(N, In, Ok) :-

    true :
      make_vector(N, A, Ok) |
	array(In, A);

    otherwise : N = _, In = _,
      Ok = false .


procedure array(In, A).

array(In, A) :-

    In ? read(I, V),
    read_vector(I, A, V^) |
	array;

    In ? write(I,V) :
      store_vector(I, V, A, A') |
	array;

    In = [] |
	close_array(1, A).


procedure close_array(N, A).

close_array(N, A) :-

    N' := N + 1 :
      close_vector(N, A) |
	close_array;

    otherwise : N = _, A = _ .
