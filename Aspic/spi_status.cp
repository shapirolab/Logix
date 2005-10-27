/*

SpiFcp Return monitor status
William Silverman

Last update by          $Author: bill $
                        $Date: 2005/10/27 17:05:11 $
Currently locked by     $Locker:  $
                        $Revision: 1.3 $
                        $Source: /home/qiana/Repository/Aspic/spi_status.cp,v $

Copyright (C) 2004, Weizmann Institute of Science - Rehovot, ISRAEL

*/
-language(compound).
-mode(interrupt).
-export([now/1, debug/1, ordinal/1, record/1, get_status/2, extract/3]).

/*
** now, ordinal: return the value of the corresponding named BioSpi parameter.
**
** debug, record: return the output stream corresponding to the named
** BioSpi parameter.
**
** get_status: return the value of the named BioSpi parameter;
**             use to get cutoff_limit, cutoff_status or any other.
**
** extract: return the named item from the Status list.
*/

/* Return Value = current internal time. */
now(Value?^):- get_status(now, Value).

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
