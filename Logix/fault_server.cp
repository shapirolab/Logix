/*

William Silverman 05/88

Fault manager process

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:02:51 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/fault_server.cp,v $

Copyright (C) 1988, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([serve/2]).
-mode(trust).
-language(compound).

/*
** This server is specialized to use the Logix System controls to recognize
** the mode of the module, whose procedure failed or was lost, and to send
** a suitable diagnostic to the next responsible server.
**
** Various games are played to discover the nature of the original layer
** transformation, to invert it correctly and to avoid recursive failure/loss.
** The object is to produce good diagnostics without altering user data, or
** introducing unacceptable overhead.
** These games can be played better!
*/


procedure serve([Any], [Any]).

serve(Faults, Delegate) :-

    Faults ? Diagnostic(Module, Name#Goal),
    Name =\= boot |
	simplify_goal(Module, Name, Goal, Goal', CCC),
	fault(Diagnostic, Name, Goal', CCC),
	serve;

    otherwise,
    Faults ? Fault :
      Delegate ! Fault |
	serve;

    Faults =\= [_ | _] :
      Faults = Delegate .

/*
** simplify_goal/5 tests  Goal1  to see if it is a large enough tuple to have
** been produced by the failsafe or interrupt layer transformation.
** Ifso, and if its last argument is a channel, it activates the module
** and proceeds to analyze the arguments of the activation.
** If the last argument is uninstantiated, the goal cannot have been
** produced by one of those transformations, since the communication
** channel is local to the processor, and is always created before the
** (root) goal is dispatched by domain_server.
**
** Output: Goal2 - the revised goal, after the inverse transformation,
**		   or  Goal1 , if an inversion cannot be determined.
**	   CCC - the extracted  Control-Circuit-Channel or nil.
*/

simplify_goal(Module, Name, Goal1, Goal2, CCC) :-

    Goal1 = {String} : Module = _, Name = _,
      Goal2 = String,
      CCC = [] ;

    A := arity(Goal1), A > 3,
    arg(A, Goal1, LastArg) |
	simplify_emulated_goal;

/* The following clause should be revised when executable (C-coded) modules
   are (re)introduced; currently it tests for an old model. */

    Name = (ModuleIndex : ProcedureIndex),
    integer(ModuleIndex), ModuleIndex >= 0,
    integer(ProcedureIndex), ProcedureIndex >= 0,
    Goal1 = rpc(Goal, Events) : Module = _,
      CCC = [] |
	simplify_executed_goal;

    otherwise : Module = _, Name = _,
      Goal1 = Goal2,
      CCC = [] .

  simplify_emulated_goal(Module, Name, Goal1, Goal2, CCC, LastArg) :-

    unknown(LastArg) : Module = _, Name = _,
      Goal1 = Goal2,
      CCC = [] ;

    channel(LastArg),
    arg(1, Goal1, Functor),
    string_to_dlist(Functor, FL, []) : Name = _,
      activate(Module, Sync, Controls),
      deschedule |
	module_attributes(Sync, Controls, Attributes),
	mode_attribute(Attributes, Mode),
	mode_arguments(Mode, Functor, FL, Id),
	circuit_and_channel(Id, Goal1, CCC0),
        remove_system_arguments(Id, Goal1, Goal2, CCC0, CCC);

    otherwise : Module = _, Name = _, LastArg = _,
      Goal1 = Goal2,
      CCC = [] .

  simplify_executed_goal(Goal, Events, Goal1, Goal2) :-

    channel(Events) : Goal1 = _,
      Goal2 = Goal;

    otherwise : Events = _, Goal = _,
      Goal1 = Goal2;

    unknown(Events) : Goal = _,
      Goal1 = Goal2.

/*
** module_attributes/3 - If the  Sync  argument of the activated module
** has the normal form,  Attributes  is returned, and the   Controls
** argument is "closed".
** Otherwise, nil  Attributes  is returned.
*/

module_attributes(Sync, Controls, Attributes) :-

    true :
      Sync = export(attributes(Attributes)) |
	close_input(Controls);

    otherwise : Sync = _, Controls = _,
      Attributes = [] .

/*
** close_input/1 - If the Controls argument appears to denote a monitor,
** its input stream is closed, to terminate the (just activated) server.
*/

close_input(Controls) :-

    Controls = Monitor([]^, _, _, _),
    Monitor =\= procedures |		% user ; system (trust ; failsafe)
	true ;

    otherwise : Controls = _ .

/*
** mode_attributes/2 - Search the  Attributes  list for the  Mode  of the
** module.
** If none, return "unknown".
*/

mode_attribute(Attributes, Mode) :-

    Attributes =\= [_ | _] :
      Mode = unknown ;

    Attributes ? mode(Mode^) : Attributes' = _ ;

    otherwise,
    Attributes ? _ |
	mode_attribute.

/*
** mode_arguments/4 - Determine the number of arguments added to the goal by
** the layer transformation.
** Return the true functor of the goal and the number of extra arguments as
** Id .
** If there are no extra arguments, nil  Id  is returned.
** A trust mode goal which includes communication controls (Circuit-Channel),
** has a modified functor, with first character an ascii <Space> .  Note that
** monitors are compiled in trust mode.
*/

mode_arguments(Mode, Functor, FL, Id) :-

    FL = [SP, C | Cs],
    SP =:= ascii(' '), C =\= SP : Functor = _ |
	mode_kluge(Mode, [C | Cs], Id);

    Mode = failsafe : FL = _,
      Id = Functor-3 ;

    Mode = interrupt : FL = _,
      Id = Functor-4 ;

    otherwise : Mode = _, Functor = _, FL = _,
      Id = [] .

mode_kluge(Mode, NSL, Id) :-

    Mode =\= trust, Mode =\= user, Mode =\= system : NSL = _,
      Id = [] ;

    otherwise,
    list_to_string(NSL, RealFunctor) : Mode = _,
      Id = RealFunctor-3 .

/*
** circuit_and_channel/3 - If  Id  indicates excess arguments, they are
** extracted from  Goal  and returned as  CCC  (the Control argument is
** unnecessary).
** Otherwise, nil  CCC  is returned.
*/

circuit_and_channel(Id, Goal, CCC) :-

    Id = _-_,
    A := arity(Goal),
    arg(A, Goal, CH),
    R := A - 1, arg(R, Goal, Right),
    L := R - 1, arg(L, Goal, Left) :
      CCC = {_, Left, Right, CH} ;

    otherwise : Id = _, Goal = _,
      CCC = [] .

/*
** remove_system_arguments/5 - If  Id  specifies extra arguments, a revised
** Goal2  is constructed by copying the first part of  Goal1 , and  CCC1  is
** returned as  CCC2 .
** Otherwise,  Goal1  is returned as  Goal2 , and  CCC2 is returned nil.
*/

remove_system_arguments(Id, Goal1, Goal2, CCC1, CCC2) :-

    Id = Functor-N,
    A := arity(Goal1) - N,
    A > 1,
    make_tuple(A, RealGoal), arg(1, RealGoal, Functor) :
      CCC1 = CCC2 |
	make_goal(2, A, Goal1, RealGoal, Goal2);

    Id = Functor-N,
    arity(Goal1) - N =:= 1 :
      Functor = Goal2,
      CCC1 = CCC2 ;

    otherwise : Id = _, CCC1 = _,
      Goal1 = Goal2,
      CCC2 = [] .

/*
** make_goal/5 - Copy the the arguments of  Goal1  to  Goal2 .
*/

make_goal(A1, A2, Goal1, RealGoal, Goal2) :-

    A1 =< A2,
    arg(A1, Goal1, X),
    arg(A1, RealGoal, X^),
    A1' := A1 + 1 |
	make_goal;

    otherwise : A1 = _, A2 = _, Goal1 = _,
      RealGoal = Goal2 .

/*
** fault/4 - If  CCC  is a  Control-Circuit-Channel (as a result of previous
** analysis and extraction), send the diagnostic message to the corresponding
** output server (see domain_server).
** Otherwise, send it to the computation of this (fault_server) process.
*/

fault(Diagnostic, Name, Goal, CCC) :-

    CCC = {_, Left, Right, CH} : Name = _,
      write_channel(exception(Diagnostic, Goal, Left, Right), CH) ;

    otherwise : CCC = _ |
	computation # failed(Name # Goal, Diagnostic).
