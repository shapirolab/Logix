
/* $Header : $ */
-export([begin,start]).
-language(dfcp).
-mode(interrupt).
-scope("Doors").

start(Ready, Ok) :-
	doors_server#start(Ready, Ok).


begin(DoorsRoot, Ok) :-
	doors_server#begin(DoorsRoot, Ok).
