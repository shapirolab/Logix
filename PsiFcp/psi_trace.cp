-language([evaluate,compound,colon]).
-mode(trust).
-export([run/1, run/3]).

run(Out) :-

    true :
      Out = Out'? |
	psi_monitor#scheduler(Scheduler),
	write_channel(Scheduler, debug(Out'));

    otherwise |
	fail(run(Out)).


run(Goal, Cutoff, Out) :-

    we(Out),
    Goal =?= _#_,
    Cutoff >= 0 :
      Out = Out'? |
	psi_monitor#scheduler(Scheduler),
	write_channel(debug(Out'), Scheduler, Scheduler'),
	write_channel(cutoff(Cutoff), Scheduler'),
	computation#Goal;

    otherwise |
	fail(run(Goal, Cutoff, Out)).
