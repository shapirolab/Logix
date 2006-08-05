/*

SpiFcp Object Monitor
William Silverman

Last update by          $Author: bill $
                        $Date: 2006/08/05 05:48:27 $
Currently locked by     $Locker:  $
                        $Revision: 1.2 $
                        $Source: /home/qiana/Repository/Aspic/spi_object.cp,v $

Copyright (C) 2006, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language([evaluate, compound, colon]).
-export([create/2, create/3, create/4, monitor/5]).
-include(spi_constants).
-mode(interrupt).

create(Name, Object) :-
    create(Name, 0, Object, _).

create(Name, Value, Object) :-
	create(Name, Value, Object, _).

create(Name, Value, Object, Values) :-

    we(Object) :
      make_vector(OBJECT_ARITY, Object, Output),
      Output = {Values, Requests},
      store_vector(OBJECT_VALUES, Value, Object, Object') |
	read_vector(OBJECT_VALUES, Object', Value'),
	monitor;

    vector(Object),
    arity(Object,OBJECT_REQUESTS) :
      Value = _,
      Name = _,
      Values' = Values?,
      write_vector(OBJECT_REQUESTS, values(Values'), Object);

    otherwise |
	fail(create(Name, Value, Object, Values)).  

monitor(Name, Value, Object, Values, Requests) :-

    Requests ? close(true^) :
      close_vector(OBJECT_VALUES, Object) |
	self;

    Requests ? name(Name?^, true^) |
	self;

    Requests ? read(V?^, true^) :
      V = Value |
	self;

    Requests ? store(V, true^) :
      store_vector(OBJECT_VALUES, V, Object, Object') |
	self;

    Requests ? values([Value | Values?]^, true^) |
	self;

    Requests ? Request,
    arity(Request, Arity),
    arg(Arity, Request, Reply),
    otherwise :
      Reply = false |
	self;

    Requests ? Request,
    otherwise |
	fail(spi_object(Name) - Request),
	self;

    Requests =?= [] :
      Requests' = _,
      close_vector(OBJECT_VALUES, Object) |
	self;

    Values ? Value' :
      Value = _ |
	self;

    Values =?= [] :
      Requests = _,
      Value = _,
      Name = _,
      Values = _,
      close_vector(OBJECT_REQUESTS, Object).
