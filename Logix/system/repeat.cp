-language(compound).
-mode(interpret).
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
    Goal =\= [_ | _], Goal =\= _*_, Goal =\= _*_#_, Goal =\= [] |
	computation#Goal,
	self;

    otherwise :
      Count = _,
      Goal = _.
