/*

 System library

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:34 $
Currenly locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/system/library_server/system_text.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([string/1]).
-mode(trust).
-language(compound).

procedure string(String).

string(Text) :- true : Text =

"library.

-language(compound).

X=Y :- true : X=Y | true.
X:=Y :- number(Y) : X = Y | true.

arity(T,A) :- A^ := arity(T) | true.
arity(List,2^) :- list(List) | true.

arg(N,T,A) :- arg(N,T,A^) | true.
arg(1,List,Car) :- List = [Car^|_] | true.
arg(2,List,Cdr) :- List = [_|Cdr^] | true.

length(S,I) :- string_length(S,I^) | true.
length(L,N) :- list(L) | utils#list_length(L,N).
length([],0^).

string_hash(S,H) :- string_hash(S,H^) | true.

string_length(S,L) :- string_length(S,L^) | true.

nth_char(S,N,C) :- nth_char(S,N,C^) | true.

make_tuple(N,T) :- make_tuple(N,T^) | true.

invalid_reason(V,T) :- invalid(V,S) : T = S.

copy_skeleton(T,T1) :-
	tuple(T), arity(T,N), make_tuple(N,T1^) | true.
copy_skeleton(List,[_|_]^) :-
	list(List) | true.
copy_skeleton(I,J) :-
	otherwise : I = J | true.

plus(X,Y,Z) :- plus(X,Y,Z^) | true.
diff(X,Y,Z) :- diff(X,Y,Z^) | true.
times(X,Y,Z) :- times(X,Y,Z^) | true.
div(X,Y,Z) :- div(X,Y,Z^) | true.
mod(X,Y,Z) :- mod(X,Y,Z^) | true.
bitwise_and(X,Y,Z) :- bitwise_and(X,Y,Z^) | true.
bitwise_or(X,Y,Z) :- bitwise_or(X,Y,Z^) | true.
bitwise_not(X,Z) :- bitwise_not(X,Z^) | true.

abs(X,X^) :-
	X >= 0 | true.
abs(X,Y) :-
	X < 0, Y^ := - X | true.

round(R,I) :-
	R >= 0 , I^ := integer(R + 1/real(2)) | true.
round(R,I) :-
	R < 0, I^ := integer(R - 1/real(2)) | true.

max(X,Y,X^) :-
	X >= Y | true.
max(X,Y,Y^) :-
	X < Y | true. 

min(X,Y,X^) :-
	X =< Y | true.
min(X,Y,Y^) :-
	X > Y | true.

random(R) :- processor # math(random(R)).
srandom(S) :- processor # math(srandom(S)).
sin(X,S) :- processor # math(sin(X,S)).
cos(X,C) :- processor # math(cos(X,C)).
tan(X,T) :- processor # math(tan(X,T)).
asin(S,A) :- processor # math(asin(S,A)).
acos(C,A) :- processor # math(acos(C,A)).
atan(T,A) :- processor # math(atan(T,A)).
exp(X,E) :- processor # math(exp(X,E)).
ln(X,L) :- processor # math(ln(X,L)).
log(X,L) :- processor # math(ln(X,L)).
sqrt(X,S) :- processor # math(sqrt(X,S)).
pow(X,Y,S) :- processor # math(pow(X,Y,S)).
log(X,Y,S) :- processor # math(log(X,Y,S)).


list_to_string(L,S) :-
    list_to_string(0,L,M) : S = M;
    L = [] : S = '' .
string_to_list(S,L) :-	string_to_dlist(S,L^,[]) | true.
string_to_dlist(S,L,E) :- string_to_dlist(S,L^,E^) | true.
convert_to_integer(X,Y) :- convert_to_integer(X,Y^) | true.
convert_to_real(X,Y) :- convert_to_real(X,Y^) | true.
convert_to_string(X,Y) :- convert_to_string(X,Y^) | true.
convert_if_integer(X,Y) :-
    convert_to_integer(X,I) : Y = I ;
    otherwise : Y = X .


tuple_to_dlist(Tuple,List,E) :- tuple(Tuple) |
	utils#tuple_to_dlist(Tuple,List,E).

list_to_tuple(List,Tuple) :- list(List) | utils#list_to_tuple(List,Tuple).


string_to_integer(String,Integer) :-
	string(String), convert_to_integer(String,Integer^) | true.

integer_to_string(Integer,String) :-
	integer(Integer), convert_to_string(Integer,String^) | true.

string_to_number(String,Number) :-
	string(String), convert_to_integer(String,Number^) | true.
string_to_number(String,Real) :-
	otherwise, string(String), convert_to_real(String,Real^) | true.

number_to_string(Number,String) :-
	number(Number), convert_to_string(Number,String^) | true.

integer_to_dlist(Integer,List,E) :-
	integer(Integer), convert_to_string(Integer,S),
	string_to_dlist(S,List^,E^) | true.

convert_to_dlist(Constant,List,E) :-
    convert_to_string(Constant,S),
    string_to_dlist(S,List^,E^) |
	true;
    Contant = [] :
      List = E .

/*
  dfcp implementation - listeners
*/

dfcp_copy(From, To, Listen) :-
    tuple(From),
    A := arity(From),
    make_tuple(A, ToVia),
    make_tuple(A, ListenVia) |
	dfcp_copy_tuple(From, To, Listen, ToVia, ListenVia, A);
    From ? Car :
      ToVia = {_, _},
      ListenVia = {_, _} |
	dfcp_copy_tuple({Car, From'}, Toward, Through, ToVia, ListenVia, 2), 
	dfcp_copy_list(Toward, Through, To, Listen);
    otherwise :			% constant or vector
      To = From,
      Listen = From .

dfcp_copy_list({TCar, TCdr}, {LCar, LCdr}, [TCar | TCdr]^, [LCar | LCdr]^).
      
dfcp_copy_tuple(From, To, Listen, ToVia, ListenVia, A) :-
    A > 0,
    arg(A, From, FromA),
    var(FromA),
    arg(A, ToVia, ToViaA),
    arg(A, ListenVia, ListenViaA),
    A' := A - 1 : ListenViaA = ListenViaA'? |
	dfcp_copy_tuple,
	dfcp_copy(ToViaA?, FromA, ListenViaA');
    A > 0,
    otherwise,
    arg(A, From, FromA),
    arg(A, ToVia, ToViaA),
    arg(A, ListenVia, ListenViaA),
    A' := A - 1 : ListenViaA = ListenViaA'?, ToViaA = ToViaA'? |
	dfcp_copy_tuple,
	dfcp_copy(FromA, ToViaA', ListenViaA');
    A =< 0 : From = _,
      To = ToVia, Listen = ListenVia .

dfcp_listen(From, To) :-
    tuple(From),
    A := arity(From),
    make_tuple(A, Via) |
	dfcp_listen_tuple(From, To, Via, A);
    From ? Car :
      To = [Car'? | To'?] |
	dfcp_listen(Car?, Car'),
	dfcp_listen;
    otherwise :			% constant or vector
      To = From ;
    var(From) : To = _ .	% read_only writable sub-term.
dfcp_listen_tuple(From, To, Via, A) :-
    A > 0,
    arg(A, From, FromA),
    arg(A, Via, ToV),
    A' := A - 1 :
      ToV = ToA? |
	dfcp_listen_tuple,
	dfcp_listen(FromA?, ToA);
    A =< 0 : From = _,
      To = Via .

/*
	dfcp => d transition predicates
*/

listen2(R, W1, W2) :-
	read_only(R) : W1 = R, W2 = R.

listen3(R, W1, W2, W3) :-
	read_only(R) : W1 = R, W2 = R, W3 = R.

listen4(R, W1, W2, W3, W4) :-
	read_only(R) : W1 = R, W2 = R, W3 = R, W4 = R.

listen5(R, W1, W2, W3, W4, W5) :-
	read_only(R) : W1 = R, W2 = R, W3 = R, W4 = R, W5 = R.

% freeze/dfreeze/melt predicates

freeze(T,FT,FVL) :-
	freeze(T,[],FT^,FVL^) | true.

freeze(T,FO,FT,FVL) :-
	freeze(T,FO,FT^,FVL^) | true.

freeze(T,D,L,S,FT,FVL) :-
	known(D),known(L),known(S), freeze(T,{D,L,S},FT^,FVL^) | true.

dfreeze(T,FT,FVL,FTL) :-
	dfreeze(T,[],FT^,FVL^,FTL^) | true.

dfreeze(T,FO,FT,FVL,FTL) :-
	dfreeze(T,FO,FT^,FVL^,FTL^) | true.

dfreeze(T,TL,VL,FT,FVL,FTL) :-
	known(TL),known(VL), dfreeze(T,{TL,VL},FT^,FVL^,FTL^) | true.

melt(FT,MT,MVL) :-
	known(FT) : melt(FT,MT,MVL) .

% vector reference predicates

make_channel(CH,Stream) :-
	true : make_channel(CH,Stream) .

write_channel(X,CH) :-
	channel(CH) : write_channel(X,CH) .

write_channel(X,CH,CH') :-
	channel(CH) : write_channel(X,CH,CH') .

close_channel(CH) :-
	channel(CH) : close_channel(CH) .

make_vector(N,V,T) :-
	integer(N) : make_vector(N,V,T) .

write_vector(I,X,V) :-
	vector(V) : write_vector(I,X,V) .

write_vector(I,X,V,V') :-
	vector(V) : write_vector(I,X,V,V') .

store_vector(I,X,V) :-
	vector(V) : store_vector(I,X,V) .

store_vector(I,X,V,V') :-
	vector(V) : store_vector(I,X,V,V') .

read_vector(I,V,X) :-
	vector(V), read_vector(I,V,X^) | true.

close_vector(I,V) :-
	vector(V) : close_vector(I,V) .

/*
	Other special guard predicates.
*/

execute(Offset, Tuple) :-
    integer(Offset), tuple(Tuple) : execute(Offset, Tuple) .

make_invalid(V, String) :-
    unknown(V), string(String) : make_invalid(V, String) .

/*
	Procedures for trusted-controller interface.
*/

unify_without_failure(X,Y) :-
	true : X = Y .
unify_without_failure(_,_) :-
	otherwise | true.

/*
	Procedures for system monitor interface
*/

unknown(Goal, Common) :-
    true : Common = exception(unknown,Goal) .
unknown(_, Common) :-
    known(Common) | true.

reply(Status, CommandIn, CommandOut, Common) :-
    true : Common = reply(Status, CommandIn, CommandOut, done) .
reply(_, _, _, Common) :-
    known(Common) | true.

closeCC({_,Close,Close,_}^).
closeCC(_) :-
	otherwise | true.

/*
		stateless-server
*/

stateless_server(In) :-
	In = [{Request,Common} | In'] |
	stateless_server(In'),
	select(Request,Common).
stateless_server(In) :-
	In = [] | true.

/*
		static-server
*/

static_server(In,State) :-
	In = [{Request,Common} | In'] |
	static_server(In',State),
	select(Request,State,Common).
static_server(In,_) :-
	In = [] | true.

/*
		dynamic-server
*/

dynamic_server(In,State) :-
	In = [{Request,Common} | In'] |
	dynamic_server(In',State'),
	select(Request,State,State',Common).
dynamic_server(In,State) :-
	In = [] |
	close_dynamic_server_state(State).

close_dynamic_server_state(_).

/*
	Procedures for computation interface
*/

fail :-
	fail(fail,failed).
fail(Goal) :-
	fail(Goal,failed).
fail(Goal,Diagnostic) :-
	self#service_id(Scope),
	service_id_path(Goal,Scope,RPC),
	computation#failed(RPC,Diagnostic).

service_id_path(Scope,Path) :-
    Scope ? Part |
	service_id_path(Part,Scope',Path);
    otherwise :
     Path = Scope.
service_id_path(Part,Scope,Path) :-
    Scope ? Node :
      Part' = Node # Part |
	service_id_path;
    Scope = [] :
      Part = Path ;
    otherwise :
      Path = Scope # Part .
   
copy_goal_args(From, Functor, To) :-
    From = {_} :
      To = Functor ;
    A := arity(From),
    A > 1,
    make_tuple(A,Via),
    arg(1,Via,F) :
      F = Functor |
	copy_goal_args(A,From,Via,To).
copy_goal_args(N,From,Via,To) :-
    N-- > 1,
    arg(N,From,A),
    arg(N,Via,B) :
      B = A |
	copy_goal_args;
    N =< 1 : From = _,
      To = Via .


"

	| true.
