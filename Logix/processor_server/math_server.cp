/*

  math.cp  (fcp monitor to go with math.c) 

margaret purtill	August 1986
bill silverman		1987

Last update by          $Author: bill $
                        $Date: 1999/07/09 07:03:39 $
Currently locked by     $Locker:  $
                        $Revision: 1.1.1.1 $
                        $Source: /home/qiana/Repository/Logix/processor_server/math_server.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/  
-export([start/1]).
-serve([sin/2,cos/2,tan/2,
	random/1,srandom/1,
	asin/2,acos/2,atan/2,
	ln/2,exp/2,sqrt/2,
	pow/3,log/3
       ]
).
-mode(trust).
-language([evaluate, compound, colon]).

SIN =>  1.					/* sine function */
COS =>  2.					/* cosine function */
TAN =>  3.					/* tangent function */
RAN =>  4.					/* random real number */
%						/* seed random */
ASIN => 5.					/* arcsine function */
ACOS => 6.					/* arccosine function */
ATAN => 7.					/* arctangent function */
EXP =>  8.					/* natural exponent */
LN  =>  9.					/* natural logarithm */
SQRT => 10.					/* square root */
POW => 11.					/* power operator */
LOG => 12.					/* log base N operator */

SystemReply ::= true | false(String).

procedure sin(Number, Real).
procedure cos(Number, Real).
procedure tan(Number, Real).
procedure random(Real).
procedure srandom(Number).
procedure asin(Number, Real).
procedure acos(Number, Real).
procedure atan(Number, Real).
procedure ln(Number, Real).
procedure exp(Number, Real).
procedure sqrt(Number, Real).
procedure pow(Number, Number, Number).
procedure log(Number, Integer, Real).


start(In) :-
	processor# link(lookup(math, ['/usr/lib/libm.a'], Offset)),
	server(In, Offset).

server(In, Offset) :-

    In ? {sin(Argument, Result), Ok, Common} |
	execute(SIN, Argument, Result, Offset, Ok, Common),
	self;

    In ? {cos(Argument, Result), Ok, Common} |
	execute(COS, Argument, Result, Offset, Ok, Common),
	self;

    In ? {tan(Argument, Result), Ok, Common} |
	execute(TAN, Argument, Result, Offset, Ok, Common),
	self;

    In ? {random(Result), Ok, Common} |
	execute(RAN, 0, Result, Offset, Ok, Common),
	self;
    In ? {srandom(Seed), Ok, Common} |
	execute(RAN, Seed, _Result, Offset, Ok, Common),
	self;

    In ? {asin(Argument, Result), Ok, Common} |
	execute(ASIN, Argument, Result, Offset, Ok, Common),
	self;

    In ? {acos(Argument, Result), Ok, Common} |
	execute(ACOS, Argument, Result, Offset, Ok, Common),
	self;

    In ? {atan(Argument, Result), Ok, Common} |
	execute(ATAN, Argument, Result, Offset, Ok, Common),
	self;

    In ? {exp(Argument, Result), Ok, Common} |
	execute(EXP, Argument, Result, Offset, Ok, Common),
	self;

    In ? {ln(Argument, Result), Ok, Common} |
	execute(LN, Argument, Result, Offset, Ok, Common),
	self;

    In ? {log(Argument, Result), Ok, Common} |
	execute(LN, Argument, Result, Offset, Ok, Common),
	self;

    In ? {sqrt(Argument, Result), Ok, Common} |
	execute(SQRT, Argument, Result, Offset, Ok, Common),
	self;

    In ? {pow(Argument, Argument2, Result), Ok, Common} |
	execute(POW, Argument, Argument2, Result, Offset, Ok, Common),
	self;

    In ? {log(Argument, Argument2, Result), Ok, Common} |
	execute(LOG, Argument, Argument2, Result, Offset, Ok, Common),
	self;

    In ? _Other(Ok, Common),
    otherwise :
      Ok = false(unknown),
      Common = done |
	self;

    In ? _Any(Ok, Common),
    known(Common) :
      Ok = false(aborted) |
	self;

    In = [] : Offset = _ .

/*

   This is the main clause, all math functions call it.

   The corresponding C program expects a three-argument tuple.

     The first argument is the operation to be done (an integer code);
     The second argument is an FCP number (a real or integer);
     The third argument is the result variable.

*/

execute(OpCode, Argument, Result, Offset, Ok, Common) :-

    number(Argument) :
      execute(Offset, {OpCode, Argument, Result}),
      Ok = true,
      Common = done ;

    otherwise : OpCode = _, Argument = _, Result = _, Offset = _,
      Ok = false(failed),
      Common = done ;

    known(Common) : OpCode = _, Argument = _, Result = _, Offset = _,
      Ok = false(aborted).


execute(OpCode, Argument, Argument2, Result, Offset, Ok, Common) :-

    number(Argument), number(Argument2) :
      execute(Offset, {OpCode, Argument, Argument2, Result}),
      Ok = true,
      Common = done ;

    otherwise : OpCode = _, Argument = _, Argument2 = _, Result = _, Offset = _,
      Ok = false(failed),
      Common = done ;

    known(Common) : OpCode = _, Argument = _, Argument2 = _,
		    Result = _, Offset = _,
      Ok = false(aborted).
