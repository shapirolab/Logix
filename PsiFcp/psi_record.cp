-language([evaluate,compound,colon]).
-mode(trust).
-export([run/2, run/3, run/4]).

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

run(Goal, File, Cutoff) + (Scale = 1) :-
    Goal =?= _#_,
    string(File), File =\= "",
    Cutoff >= 0,
    convert_to_real(Scale, Scale'),
    0 < Scale' |
	psi_monitor#scheduler(Scheduler),
	write_channel(record(Stream), Scheduler, Scheduler'),
	write_channel(cutoff(Cutoff), Scheduler'),
	computation#[Goal, events(Events)],
	file#put_file(File, Out?, write, Ok),
	filter_data,
	run_ok;

    otherwise :
      Scale = _ |
	fail(run(Goal, File, Cutoff)).

  run_ok(File, Ok) :-

    Ok = true :
      File = _;

    otherwise |
	fail(("
		write"(File) - Ok)).


filter_data(Stream, Events, Out, Scale) :-

    Stream ? Number, number(Number),
    Number' := Scale*Number :
      Out ! Number',
      Out' ! "
" |
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
      Events = _,
      Scale = _,
      Out = [] ;

    otherwise :
      Events = _,
      Scale = _,
      Out = [QUERY, EOL] |
	fail(Stream);

    Events ? Event,
    Event =\= aborted |
	self;

    unknown(Stream),
    Events ? aborted :
      Events' = _,
      Scale = _,
      Out = [].
