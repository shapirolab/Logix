-language(compound).
-export([serve/4]).
-mode(trust).

serve(Bytes, In, LineOut, TC) :-
	server # initial(Bytes, In, Edit, LineOut, TC),
	line_editor # requests(Edit?).
