/* $Header: /home/qiana/Repository/Logix/system/ndg/guards.cp,v 1.1 1999/07/09 07:02:57 bill Exp $ */
-language([dguards,colon]).
-export([server/1]).
-mode(trust).

/*
The different entries are:

connect(G, I, O) - G is a guard 
	           I is a list of its input args
	           O is a list of its output args
	   This is a required entry for every guard to be supported. 

A => B  :  A implies B.
          (No need to specify A => A, it is implied automatically).

A <=> B :  A => B & B => A.
          (No need to specify A <=> A, it is implied automatically).

A > B   :  A dominates B, i.e. has to be executed before B.
          (When preconditions will be added then if A is a precondition of B
           then it also proceeds B, and this will be implied automatically).
*/

%
% connect table
%
connect(     integer(_),                  [2],         []).
connect(     real(_),                     [2],         []).
connect(     number(_),                   [2],         []).
connect(     string(_),                   [2],         []).
connect(     constant(_),                 [2],         []).
connect(     list(_),                     [2],         []).
connect(     tuple(_),                    [2],         []).
connect(     compound(_),                 [2],         []).

connect(     known(_),                    [2],         []).
connect(     unknown(_),                  [2],         []).
connect(     var(_),                      [2],         []).
connect(     ro(_),                       [2],         []).
connect(     not_we(_),                   [2],         []).

connect(     module(_),                   [2],         []).
connect(     vector(_),                   [2],         []).
connect(     ground(_),                   [2],         []).

connect(     arity(_,_),                  [2],         [3]).
connect(     make_tuple(_,_),             [2],         [3]).
connect(     info(_,_),                   [2],         [3]).
connect(     code_info(_,_),              [2],         [3]).
connect(     bitwise_not(_,_),            [2],         [3]).
connect(     string_length(_,_),          [2],         [3]).
connect(     string_hash(_,_),            [2],         [3]).
connect(     convert_to_string(_,_),      [2],         [3]).
connect(     convert_to_integer(_,_),     [2],         [3]).
connect(     convert_to_real(_,_),        [2],         [3]).
connect(     code_info(_,_),              [2],         [3]).
connect(     {'=?=',_,_},                 [2],         [3]).
connect(     list_to_string(_,_,_),       [2,3],       [4]).

connect(     {'=',_,_},                   [2,3],       []).
connect(     {'==',_,_},                  [2,3],       []).
connect(     {'=\=',_,_},                 [2,3],       []).
connect(     {'=<',_,_},                  [2,3],       []).
connect(     {'>=',_,_},                  [2,3],       []).
connect(     {'<',_,_},                   [2,3],       []).
connect(     {'>',_,_},                   [2,3],       []).
connect(     {'@<',_,_},                  [2,3],       []).
connect(     debug(_,_),                  [2,3],       []).

connect(     arg(_,_,_),                  [2,3],       [4]).
connect(     nth_char(_,_,_),             [2,3],       [4]).
connect(     read_vector(_,_,_),          [2,3],       [4]).
connect(     plus(_,_,_),                 [2,3],       [4]).
connect(     diff(_,_,_),                 [2,3],       [4]).
connect(     times(_,_,_),                [2,3],       [4]).
connect(     div(_,_,_),                  [2,3],       [4]).
connect(     mod(_,_,_),                  [2,3],       [4]).
connect(     bitwise_and(_,_,_),          [2,3],       [4]).
connect(     make_module(_,_,_),          [2,3],       [4]).
connect(     ask_shared_var(_,_,_),       [2,3],       [4]).
connect(     bitwise_or(_,_,_),           [2,3],       [4]).

connect(     invalid(_,_),                [2],         [3]).
connect(     var_info(_,_),               [2],         [3]).
connect(     string_to_dlist(_,_,_),      [2],         [3,4]).
connect(     freeze(_,_,_,_),             [2,3],       [4,5]).
connect(     dfreeze(_,_,_,_,_),          [2,3],       [4,5,6]).
connect(     make_procedure(_,_,_,_),     [2,3,4],     [5]).

connect(     exceptions(_),                [],         [2]).

%
% relations table
%
(X = integer(X))              =>     integer(X).

integer(X)                  =>     number(X).

number(X)                   =>     constant(X).
string(X)                   =>     constant(X).

tuple(X)                    =>     compound(X).
list(X)                     =>     compound(X).

string(X)                   =>     ~(number(X)).

compound(X)                 =>     ~(constant(X)).

constant(X)                 =>     known(X).
compound(X)                 =>     known(X).

unknown(X)                  <=>    var(X).

arity(X,Y)                  =>     tuple(X),integer(Y).
make_tuple(X,Y)             =>     tuple(Y),integer(X). 
arg(X,Y,_)                  =>     tuple(Y),integer(X).
nth_char(X,Y,Z)             =>     string(Y),integer(X),integer(Z).
plus(X,Y,Z)                 =>     number(X),number(Y),number(Z).
diff(X,Y,Z)                 =>     number(X),number(Y),number(Z).

(X < Y)                     =>	   number(X),number(Y).
(X =< Y)                    =>	   number(X),number(Y).
(X >= Y)                    =>	   number(X),number(Y).
(X > Y)                     =>	   number(X),number(Y).
	
~(X = Y)                    <=>    (X =\= Y).
~(X = Y)                    <=>    (Y =\= X).

(X < Y)                     =>     (X =< Y).
(X > Y)                     =>     (X >= Y).
(X < Y)                     =>     (Y >= X).
(X > Y)                     =>     (Y =< X).

~(X < Y)                    =    (X >= Y). 
~(X > Y)                    =    (X =< Y).
~(X < Y)                    =    (Y =< X). 
~(X > Y)                    =    (Y >= X).

((X > integer(N)),(N > N1)) =>     (X > integer(N1)).
((X >= integer(N)),(N > N1))=>     (X >= integer(N1)).
((X < integer(N)),(N < N1)) =>     (X < integer(N1)).
((X =< integer(N)),(N < N1))=>     (X =< integer(N1)).

list_to_string(X,Y,Z)       =>     integer(X),string(Y),list(Z).

/*
integer(X)                  =>     ~(real(X)).
real(X)                     =>     number(X).
tuple(X)                    =>     ~(real(X)).

vector(X)                   =>     known(X).
vector(X)                   =>     ~(constant(X)).
vector(X)                   =>     ~(compound(X)).

module(X)                   =>     known(X).
module(X)                   =>     ~(constant(X)).
module(X)                   =>     ~(compound(X)).
					 
~(known(X))                 <=>    read_only(X). 

info(X,_)                   =>     integer(X).
bitwise(X,Y)                =>     integer(X),integer(Y).
string_length(X,Y)          =>     string(X),integer(Y).
string_hash(X,Y)            =>     string(X),integer(Y).
convert_to_string(X,Y)      =>     list(X),string(Y).
convert_to_integer(X,Y)     =>     constant(X),integer(Y).
convert_to_real(X,Y)        =>     constant(X),real(Y).
read_vector(X,Y,_)          =>     integer(X),vector(Y).
times(X,Y,Z)                =>     number(X),number(Y),number(Z).
div(X,Y,Z)                  =>     number(X),number(Y),number(Z).
mod(X,Y,Z)                  =>     number(X),number(Y),number(Z).
bitwise_and(X,Y,Z)          =>     number(X),number(Y),number(Z).
bitwise_or(X,Y,Z)           =>     number(X),number(Y),number(Z).
string_to_dlist(X,Y,_)      =>     string(X),list(Y).

(X = Y)                     =>     ~(X @< Y).
(X = Y)                     =>     ~({'>@',X,Y}).

~(X =?= Y)                    <=>    (X =\= Y).

(X =?= Y)                     =>     ~(X @< Y).
(X =?= Y)                     =>     ~({'>@',X,Y}).

(X < Y)                     =>     (X @< Y).
(X > Y)                     =>     ({'>@',X,Y}).

((X > integer(N)),(N > N1)) =>     (X > real(N1)).
((X > real(N)),(N > N1))    =>     (X > integer(N1)).
((X > real(N)),(N > N1))    =>     (X > real(N1)).

((X >= integer(N)),(N >= N1)) =>     (X >= real(N1)).
((X >= real(N)),(N >= N1))    =>     (X >= integer(N1)).
((X >= real(N)),(N >= N1))    =>     (X >= real(N1)).

((X < integer(N)),(N < N1)) =>     (X < real(N1)).
((X < real(N)),(N < N1))    =>     (X < integer(N1)).
((X < real(N)),(N < N1))    =>     (X < real(N1)).
((X =< integer(N)),(N =< N1)) =>     (X =< real(N1)).
((X =< real(N)),(N =< N1))    =>     (X =< integer(N1)).
((X =< real(N)),(N =< N1))    =>     (X =< real(N1)).

*/

%
% dominate table
%
(X < Y)                     >      (X >= Y).
integer(X)                  >      real(X).
integer(X)                  >      string(X).
real(X)                     >      number(X).
string(X)                   >      real(X).
number(X)                   >      constant(X).
list(X)                     >      tuple(X).
tuple(X)                    >      compound(X).
constant(X)                 >      compound(X).
compound(X)                 >      known(X).
known(X)                    >      unknown(X).

(number(X),number(Y))         >      (X > Y).
(number(X),number(Y))         >      (X >= Y).
(number(X),number(Y))         >      (X =< Y).
(number(X),number(Y))         >      (X < Y).
