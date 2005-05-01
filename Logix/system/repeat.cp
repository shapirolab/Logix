-language(compound).
-mode(interrupt).
-export(run/1).

run(Specs) :-

    Specs ? Spec |
	self,
	self + (Specs = Spec);

    Specs =?= N*Goals |
	repeat(N, Goals);

    Specs =?= N*Service#Goal |
	repeat(N, Service#Goal);

    Specs =\= [_ | _], Specs =\= [], Specs =\= _*_, Specs =\= _*_#_ |
	repeat(1, Specs);

    Specs =?= [] |
	true.

repeat(Count, Goal) :-

    Count > 0,
    Goal ? SubGoal |
	self,
	repeat(Count, SubGoal);

    Count-- > 0,
    Goal =?= _*_ |
	self,
	run(Goal);

    Count-- > 0,
    Goal =?= _*_#_ |
	self,
	run(Goal);

    Count-- > 0,
    Goal =\= [_ | _], Goal =\= [],
    Goal =\= _*_, Goal =\= _*_#_ |
	self,
	rpc,
	computation#RPC? ;

    otherwise :
      Count = _,
      Goal = _.

rpc(Goal, RPC) :-

    Goal =?= Service#Goal', Goal' =?= _#_ :
      RPC = Service#RPC' |
	self;

    Goal =?= Service#List, List =\= _#_ :
      RPC = Service#PredicateList? |
	predicate_list;

    otherwise:
      RPC = Goal.

predicate_list(List, PredicateList) :-

    List ? N*Predicate, N-- > 0 :
      PredicateList ! Predicate,
      List'' = [N'*Predicate | List'] |
	self;

    List ? N*_, N =< 0 |
	self;

    List ? Other,
    otherwise :
      PredicateList ! Other |
	self;

    List =?= [] :
      PredicateList = [];

    List =\= [_|_], List =\= [] :
      List' = [List] |
	self.
