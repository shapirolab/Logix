-export([create/3]).
-mode(interrupt).
-scope("Doors").

create(IdCard, Door, Ok) :-
	processor_room # create(IdCard, Door, Ok).
