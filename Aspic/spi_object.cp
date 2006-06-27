/*

SpiFcp Object Monitor
William Silverman

Last update by          $Author: bill $
                        $Date: 2006/06/27 08:47:01 $
Currently locked by     $Locker:  $
                        $Revision: 1.1 $
                        $Source: /home/qiana/Repository/Aspic/spi_object.cp,v $

Copyright (C) 2006, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language([evaluate, compound, colon]).
-export([create/2, create/3, create/4]).
-mode(interrupt).

OBJECT_VALUES => 1.
OBJECT_COMMANDS => 2.

create(Name, Object) :-
    create(Name, 0, Object, _).

create(Name, Value, Object) :-
	create(Name, Value, Object, _).

create(Name, Value, Object, Values) :-

    we(Object) :
      make_vector(OBJECT_COMMANDS, Object, Output),
      Output = {Values, Commands},
      store_vector(OBJECT_VALUES, Value, Object, Object') |
	read_vector(OBJECT_VALUES, Object', Value'),
	monitor;

    vector(Object),
    arity(Object,OBJECT_COMMANDS) :
      Value = _,
      Name = _,
      Values' = Values?,
      write_vector(OBJECT_COMMANDS, values(Values'), Object);

    otherwise |
	fail(create(Name, Value, Object, Values)).  

monitor(Name, Value, Object, Values, Commands) :-

    Commands ? close :
      close_vector(OBJECT_VALUES, Object) |
	self;

    Commands ? name(Name?^) |
	self;

    Commands ? read(V?^) :
      V = Value |
	self;

    Commands ? store(V) :
      store_vector(OBJECT_VALUES, V, Object, Object') |
	self;

    Commands ? values([Value | Values?]^) |
	self;

    Commands ? Command,
    otherwise |
	fail(spi_object(Name) - Command),
	self;

    Values ? Value' :
      Value = _ |
	self;

    Values =?= [] :
      Commands = _,
      Value = _,
      Name = _,
      Values = _,
      close_vector(OBJECT_COMMANDS, Object).
