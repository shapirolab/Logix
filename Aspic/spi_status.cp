-language(compound).
-mode(interrupt).
-export([now/1, cutoff/1, debug/1, ordinal/1, record/1, get_status/2,
	 extract/3]).

/*
** now, cutoff, ordinal: return the value of the corresponding named
** BioSpi parameter.
**
** debug, record: return the output stream corresponding to the named
** BioSpi parameter.
**
** get_status: return the value of the named BioSpi parameter.
**
** extract: return the named item from the Status list.
*/

/* Return Value = current internal time. */
now(Value?^):- get_status(now, Value).

/* Return Value = limit on internal time. */
cutoff(Value?^) :- get_status(cutoff, Value).

/* Return the private ordinal value which is next to be assigned. */
ordinal(Value?^) :- get_status(ordinal, Value).

/* Return the debug stream. */
debug(Stream?^) :- get_status(debug, Stream).

/* Return the record stream. */
record(Stream?^) :- get_status(record, Stream).

/* Return the named status value. */
get_status(Name, Value?^) :-
	spi_monitor#status(Status), extract(Status, Name, Value).

extract(Status, Name, Value):-

    Status =\= [_|_], Status =\= [] :
      Status' = [Status] |
	self;

    Status ? Name(VV) :
      Status' = _ ,
      Value = VV? ;

    Status ? (Name = VV) :
      Status' = _ ,
      Value = VV? ;

    Status ? X ,
    X =\= Name(_), X =\= (Name = _) | 
	self ; 

    Status =?= [] :
      Name = _ ,
      Value = [] .
