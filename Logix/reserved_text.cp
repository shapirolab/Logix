/*

 Reserved guard predicate library

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:02:52 $
Currenly locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/reserved_text.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([string/1]).
-mode(trust).
-language(compound).

procedure string(String).

string(Text) :- true : Text =

"library.

-language(compound).

info(I, V) :-
  writable(V),
  info(I,R) :
    V = R;
  otherwise :
    V = false(out_of_range(I)).

activate(Module, Arg1, Arg2) :-
  module(Module) :
    activate(Module, Arg1, Arg2);
  otherwise : Module = _ |
      unify_without_failure(Arg1, []),
      unify_without_failure(Arg2, unknown).

machine_output(Machine) :-
  writable(Machine) :
    machine_output(Machine).

copy_listener(L, L1, L2) :-
  read_only(L) : L1 = L, L2 = L .

test_activate(Module, Arg1, Arg2, Ok) :-
  module(Module) :
    activate(Module, A1, A2),
    deschedule |
      test_activate1;
   otherwise : Module = _, Arg1 = _, Arg2 = _ |
      unify_without_failure(Ok, false(unknown)).
test_activate1(A1, A2, Arg1, Arg2, Ok) :-
  true :
    A1 = Arg1,
    Ok = true,
    Arg2 = A2 ;
  A1 ? system : A1' = _, A2 = _,
    Arg1 = _, Arg2 = _,
    Ok = false(boot);
  otherwise : A1 = _, A2 = _,
    Arg1 = _, Arg2 = _,
    Ok = false(unknown).

/* domain_server extensions for common writers */

shared_common_interface(Goal, GoalCommon, Done, CO, COC) :-
  true :
    GoalCommon = {Goal, Common} |
      common_interface(Common, Done, done([]), CO, COC).

shared_activate_clause_MR(Module, Goal, Body, LO, RO, Interrupt, Result,
				Goals, Id1
) :-
  true : Id1 = Id? |
    activate(Module, reduce(Goal), meta(Body, Id, MR)),
      clause(Id?, LO, RO, MR, Interrupt, Result, Goals).

shared_activate_reduce(Module, Functor,Goal, MK, LO, RO, Interrupt, CO) :-
  true : 
    activate(Module, Functor(Goal), MK(Body, _, MR)) |
      reduce(Body, LO, RO, MR, Goal, Interrupt, Module, CO, MK).


shared_common_reply(Ready, LO, RO, Signals, COC, In, Out) :-

  known(Ready) : Signals = _, COC = _,
    Out = In,
    LO = RO;

  otherwise : Ready = _, Signals = _,
    write_channel(exception(can't_reply, In =\= Out, LO, RO), COC) ;

  Signals ? abort : Ready = _, Signals' = _, COC = _,
    LO = RO |
      unify_without_failure(In, Out);
    
  Signals ? Other,
  Other =\= abort |
      self.

"

.
