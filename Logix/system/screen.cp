-export([display/1,display_stream/1,
	 display/2,display_stream/2,
	 ask/2,ask/3,
	 option/1,option/2,option/3,
	 options/1
	]
).
-language(compound).
-mode(trust).

OptionName ::= type ; read ; depth; length ; indent ; width ; special ;
	       iterations.
Arguments ::= [Argument].
Argument ::= type(Type) ; read(Read) ; depth(Integer); length(Integer);
	     indent(Integer), width(Integer), special(Special),
	     iterations(Integer) ; known(Any) ; wait(Any) ; close(Any, Any) ;
	     list ; prefix(Any) ; append(String) ; put(String) .
Value ::= String ; Integer.
Options ::= [type(OType), read(char), depth(Integer), length(Integer),
	     indent(Integer), width(Integer), special(Special),
	     iterations(Integer)
	    ].
OType ::= freeze ; namevars ; parsed.
Type ::= freeze ; ground ; namevars ; parsed ; unparse.
Special ::= hex ; melted ; none.
Read ::= char ; line ; string ; chars.

procedure display(Any).

display(Term) :-
	computation # display(term, Term).

procedure display(Any, Arguments).

display(Term, Arguments) :-
	computation # display(term, Term, Arguments).

procedure display_stream(Any).

display_stream(Stream) :-
	computation # display(stream, Stream).

procedure display_stream(Any, Arguments).

display_stream(Stream, Arguments) :-
	computation # display(stream, Stream, Arguments).

procedure ask(Any, Any).

ask(Query, Read) :-
	computation # display(ask, Query, Read).

procedure ask(Any, Any, Arguments).

ask(Query, Read, Arguments) :-
	computation # display(ask, Query, Read, Arguments).

procedure option(OptionName).

option(OptionName) :-
	computation # display(option, OptionName).

procedure option(OptionName, Value).

option(OptionName, New) :-
	computation # display(option, OptionName, New).

procedure option(OptionName, Value, Value).

option(OptionName, New, Old) :-
	computation # display(option, OptionName, New, Old).

procedure options(Options).

options(Options) :-
	computation # display(options, Options).
