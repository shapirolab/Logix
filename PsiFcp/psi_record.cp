-language([evaluate,compound,colon]).
-mode(trust).
-export([run/2, run/3]).

EOL => 10.
PLUS => 43.
MINUS => 45.
QUERY => 63.

MAXINT => 3354431.

REALTIME => 12.

run(Goal, Cutoff) :-

    Goal =?= _#_,
    Cutoff >= 0 |
	psi_monitor#scheduler(Scheduler),
	write_channel(cutoff(Cutoff), Scheduler),
	computation#Goal;

    otherwise |
	fail(run(Goal, Cutoff)).

run(Goal, File, Cutoff) :-
    Goal =?= _#_,
    string(File), File =\= "",
    Cutoff >= 0 |
	psi_monitor#scheduler(Scheduler),
	write_channel(record(Stream), Scheduler, Scheduler'),
	write_channel(cutoff(Cutoff), Scheduler'),
	computation#Goal,
	file#put_file(File, Out?, write, Ok),
	filter_data,
	run_ok;

    otherwise |
	fail(run(Goal, File, Cutoff)).

  run_ok(File, Ok) :-

    Ok = true :
      File = _;

    otherwise |
	fail(("
		write"(File) - Ok)).


filter_data(Stream, Out) :-

    Stream ? Number, number(Number),
    convert_to_string(Number, N),
    string_to_dlist(N, DN, [EOL]),
    list_to_string(DN, String) :
      Out ! String |
	self;

    Stream ? start(Name), string(Name),
    string_to_dlist(Name, DN, [EOL]),
    list_to_string([PLUS | DN], String) :
      Out ! String |
	self;

    Stream ? end(Name), string(Name),
    string_to_dlist(Name, DN, [EOL]),
    list_to_string([MINUS | DN], String) :
      Out ! String |
	self;

    Stream ? end(Name(_ChannelName)), string(Name),
    string_to_dlist(Name, DN, [EOL]),
    list_to_string([MINUS | DN], String) :
      Out ! String |
	self;

    Stream =?= [] :
      Out = [] ;

    otherwise :
      Out = [QUERY, EOL] |
	fail(Stream).
