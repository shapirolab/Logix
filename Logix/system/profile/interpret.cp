/*

Execution Profiler - profile_interpret

Yossie Lichentenstein, Bill Silverman
28/05/86

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:37 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/system/profile/interpret.cp,v $

Copyright (C) 1986, Weizmann Institute of Science - Rehovot, ISRAEL

  
   These are the procedures in this module :

   goals	       ( Modules list , 
			 Goal,
                         Cumulation stream ) 

   reduce	       ( Conjunction of goals,
			 Circuit including module context channel,
			 Module,
			 Goal which reduced to  Conjunction ,
			 Dictionary of Modules,
			 Cumulation Stream,
			 Abort signal of computation,
			 Time of completion of Goal )

   spawn               ( Goal , Circuit, Module ,
                         Dictionary of modules , 
                         Number of creations ,
                         Number of remote procedure calls ,
                         Cumulation stream,
                         Abort signal ) 

   reduce_new_module   ( Answer,  % is this module in the Dictionary)
			 Circuit,
			 Id of new module ,
			 Goal,
			 Dictionary of modules,
			 Cumulation Stream,
			 Abort signal ) 

   Reduction of conjuctive goal (A,B), and of goal and body  (A :- G | B) are 
   done concurrently. This is needed to allow reverse-producer/consumer
   relation (e.g. a(X?),a(X)).
   However, the second tree (B's tree) can be completed only after the first
   tree is done.
   This synchronization is achieved by annotating the second reduction's tree
   as read only.
   
   Change History:
   1.	Allow profiling of a meta-interpreter (this way we can profile the
	profiler for a specific goal).
	Correct handling of M#G since given the goal a#b#c#x(_), M will
	unify with "a" and G will unify with b#c#x(_), while we want the
	context to be "a#b#c", and the module name is "c".
   1.1. Allow user to abort computation, and supply results "so far".  This
	will allow "never-ending" computations to be profiled if they are
	compiled in "interpret" mode.
	Allow a computation to fail.
   1.2  Allow the module "all" to match any module - i.e. profile all modules.
   1.3  Changed from Path to ServiceId as service identifier.
  
*/

-export([goals / 3]).
-mode(trust).
-language(compound).

procedure goals((String ; [String]), Goal, Cumulate).

Goal ::= Atom ; [Goal].
Atom ::= String ; Tuple.
Cumulate ::= [{{Id, String, Integer}, {Integer, Integer, Integer}}].


goals(Modules, Goal, Cumulants) :-
	make_channel(CCH, Cumulants),
	monitor_functions # monitor_events(_Done, Abort),
	initialize(Modules, Goal, RPC, Dictionary),
	computation_utils # call_context_goal(RPC, Channel, Goal', Ok),
	initial_id(Ok, Channel, Channel', Id),
	check_context(Ok, Id, Goal', Body, Channel', CHLR),
	reduce(Body, CHLR, Id, 'Initial', Dictionary, CCH(CCH, End),
		Abort, 0
	),
	close_channel(End).

check_context(true, _, Goal, Goal^, Channel, Channel(Channel, End)^) :-
	close_channel(End).
check_context(false(Other), Id, Goal, true^, _, _(_, _)^) :-
    otherwise |
	fail(profile(Id, Goal), Other).


initialize(Modules, Goal, RPC, Dictionary) :-

    Modules ? Module^ : Modules' = _,
      RPC = Module # Goal |
	add_if_not_member(Modules, [], Dictionary);

    string(Modules),
    Modules =\= all :
      Modules = Module,
      Dictionary = [Module],
      RPC = Module # Goal ;

    Modules = all :
      Dictionary = all,
      Goal = RPC .

initial_id(Ok, Channel1, Channel2, Id) :-

    Ok = true |
	write_channel(service_id(Id), Channel1, Channel2);

    otherwise : Ok = _, Channel1 = _, Channel2 = _,
      Id = ["?"] .

id_to_path(Id, Path, Left, Right) :-

    Id ? Name, Id' = [_|_] :
      Path = Path' # Name |
	self;

    Id = [Name] :
      Path = Name,
      Left = Right ;

    Id = [] :
      Path = [],
      Left = Right .


reduce(Body, CHLR, Id, Goal, Dictionary, Cumulate, Abort, Time) :- 

    known(Body),
    Cumulate = CCH(L, R) : Time = _ |
	cumulate(Id, Goal, {Creations, 1, Calls}, CCH, L, M),
	spawn(Body, CHLR, Id, Dictionary, Creations, Calls, 
		CCH(M, R), Abort);

    Abort = abort : Body = _, Id = _, Goal = _, Dictionary = _,
      CHLR = _(CH, CH),
      Cumulate = _(CU, CU) |
	unify_without_failure(abort, Time).


spawn(Goal, CHLR, Id, Dictionary, N, C, Cumulate, Abort) :-

    Goal = (Goal', Goals),
    CHLR = Channel(Left, Right),
    Cumulate = CCH(L, R) :
      CHLR' = Channel(Left, Middle),
      Cumulate' = CCH(L, M) |
	spawn,
	spawn(Goals, Channel(Middle, Right), Id, Dictionary,
			N', C', CCH(M, R), Abort
	),
	accumulate(N, N', N'', C, C', C'');

    Goal ? Goal'',
    CHLR = Channel(Left, Right),
    Cumulate = CCH(L, R) :
      CHLR' = Channel(Left, Middle),
      Cumulate' = CCH(L, M) |
	spawn,
	spawn(Goal', Channel(Middle, Right), Id, Dictionary,
			N', C', CCH(M, R), Abort
	),
	accumulate(N, N', N'', C, C', C'');

    Goal = Service # _, Service =\= computation :
      CHLR = _Channel(Close, Close),
      Call = Id # Goal,
      N = 0,
      C = 1 |
	computation_utils # call_id_goal(Call, Id', Goal', Ok),
	module_context(Ok, Id', Dictionary, Channel, Answer),
	reduce_new_module + (OldId = Id, RPC = Goal);

    Goal = computation # (_ # _) :
      CHLR = _Channel(Close, Close),
      Call = Id # Goal,
      N = 0,
      C = 1 |
	computation_utils # call_id_goal(Call, Id', Goal', Ok),
	module_context(Ok, Id', Dictionary, Channel, Answer),
	reduce_new_module + (OldId = Id, RPC = Goal);

    Goal =  computation # Request, Request =\= _#_,
    Cumulate = CCH(L, R) : Dictionary = _, Abort = _,
      CHLR = _Channel(Close, Close),
      N = 0,
      C = 1 |
	cumulate(Id, Goal, {0, 1, 0}, CCH, L, R);

    Goal = Goal' @ _Processor |
	self;

    Goal =\= true, Goal =\= [],
    Goal =\= (_, _), Goal =\= [_|_],
    Goal =\= _#_, Goal =\= _@ _,
    unknown(Abort),
    CHLR = Channel(_, _) :
      N = 0,
      C = 0 |
	execute(clause(Goal, Body, _ClauseId, Time), Channel),
	reduce;

    Goal = true : Id = _, Dictionary = _, Abort = _,
      CHLR = _(CH, CH),
      N = 0, C = 0,
      Cumulate = _(CU, CU);

    Goal = [] : Id = _, Dictionary = _, Abort = _,
      CHLR = _(CH, CH),
      N = 0, C = 0,
      Cumulate = _(CU, CU);

    Abort = abort: Goal = _, Id = _, Dictionary = _,
      CHLR = _(CH, CH),
      N = 0, C = 0,
      Cumulate = _(CU, CU).


reduce_new_module(OldId, RPC, Channel, Id, Goal, Dictionary,
		  Cumulate, Abort, Answer
) :-

    Answer = true : OldId = _, RPC = _,
      CHLR = Channel(Channel, End) |
	reduce + (Body = Goal, Goal = _#_, Time = 0),
	close_channel(End);

    Answer = false,
    Cumulate = CCH(L, R) : Id = _, Dictionary = _, Abort = _,
      write_channel(Goal, Channel, Channel') |
	cumulate(OldId, RPC, {0, 1, 0}, CCH, L, R),
	close_channel(Channel');

    Answer = none,
    Cumulate = CCH(L, R) : Channel = _, Goal = _, Id = _,
			   Dictionary = _, Abort = _ |
	fail(profile(OldId, RPC), none),
	cumulate(OldId, RPC, {0, 1, 0}, CCH, L, R).


accumulate(N, N1, N2, C, C1, C2) :-
   N^ := N1 + N2 + 1, 
   C^ := C1 + C2 |
	true.


module_context(Ok, Id, Dictionary, CCH, Answer) :-

    Ok = true,
    Id = [Name | _] :
      make_channel(CCH, RPCs) |
	Id # RPCs?,
	member(Name, Dictionary, Answer);

    Ok = true,
    Id = [] : Dictionary = _,
      Answer = false,
      make_channel(CCH, RPCs) |
	Id # RPCs? ;

    otherwise : Ok = _, Id = _, Dictionary = _, CCH = _,
      Answer = none .


member(X, Xs, Answer) :-

    Xs ? X : Xs' = _,
      Answer = true ;

    Xs = all,
    X =\= computation, X =\= processor,
    string(X) :
      Answer = true ;

    Xs ? Y,
    X =\= Y |
	member;

    otherwise : X = _, Xs = _,
      Answer = false .


add_if_not_member(Xs, Ys, Zs) :-

    Xs = [] :
      Ys = Zs ;

    Xs ? X |
      member(X, Ys, Answer),
      add(Answer, X, Ys, Ys'),
      add_if_not_member;

    otherwise :		% X is not a list
      Xs' = [Xs] |
	add_if_not_member.

add(Answer, X, Ys1, Ys2) :-

    Answer = false,
    X =\= computation, X =\= processor,
    string(X) :
       Ys2 = [X | Ys1] ;

    otherwise : Answer = _, X = _,
      Ys1 = Ys2 .


cumulate(Id, Goal, Info, CCH, Left, Right) :-

    Goal = _#_ : Id = _, Info = _, CCH = _,
      Left = Right ;

    Goal =\= _#_, 
    arg(1, Goal, Functor),  Arity := arity(Goal) - 1 :
      write_channel({{Id, Functor, Arity}, Info}, CCH),
      Left = Right ;

    string(Goal) :
      write_channel({{Id, Goal, 0}, Info}, CCH),
      Left = Right ;

    otherwise : Goal = _,
      write_channel({{Id, '?', 0}, Info}, CCH),
      Left = Right .

		
execute(Clause, Channel) :-

    Clause = _(clause(UserGoal, UserBody), true^, _Id, Time^) :
      write_channel(clause(UserGoal, UserBody, _, Time), Channel) ;

    Clause = _(Clause3, true^, _Id, Time),
    Clause3 = clause(_UserGoal, _UserBody, Control) :
      Time = UserTime?,
      Control = _Suspense(_UserId, UserTime) |
      write_channel(Clause3, Channel) ;

    Clause = _(Clause4, true^, _Id, Time),
    Clause4 = clause(_UserGoal, _UserBody, _UserId, UserTime) :
      Time? = UserTime, 
      write_channel(Clause4, Channel) ;

    otherwise :
      write_channel(Clause, Channel) ;

    Clause = _(_, _, _, abort) : Channel = _ .	% the channel has closed
