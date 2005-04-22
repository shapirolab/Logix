-mode(failsafe).
-language([evaluate,compound,colon]).
-export([run/2, run/3]).
-include(spi_constants).

run(Goal, File) :-
	tree + (Limit = status(_)).

run(Goal, File, Limit) :-
    convert_to_real(Limit, Cutoff),
    Cutoff > 0 |
	tree + (Limit = cutoff(Cutoff));

    otherwise :
      File = _,
      Goal = _ |
	fail(limit_must_be_positive(Limit)).

tree(Goal, File, Limit) :-

    string(File),
    Goal =?= _ # _ |
	append_or_display,
        computation # events(Events),
	watch_events,
	start_ambient;

    Goal =?= _ # _,
    otherwise :
      Limit = _ |
	fail(illegal_file_name(File));

    otherwise :
      File = _,
      Limit = _ |
	fail(not_an_RPC(Goal)).

  append_or_display(Goal, File, Options) :-

    File =?= "" :
      Goal = _,
      Options = [depth(100)];

    File =\= "", string(File),
    string_to_dlist(File, FL, [CHAR_DOT | LL]),
    string_to_dlist(list, LL, []),
    list_to_string(FL, FileL) |
	screen#display(Goal,
		[put(File),
		 close( Done,
			{[width(100000),
			  type(ground), append(File)],

			 [width(100000), depth(100), length(1000),
			  type(ground), append(FileL)]
		  	}
		      )
	        ]
	),
	screen#display_stream([Goal, ""],
			      [put(FileL), close(Done, Options)]).
      
  /* Monitor Events stream */
  watch_events(Events, State) :-

    Events ? Event,
    Event =\= aborted |
        self;

    Events ? aborted :
      Events' = _,
      State = aborted;

    Events =?= [] :
      State = done;

    known(State) :
      Events = _.

start_ambient(Goal, Limit, State, Options) :-

	/* Last N is the best we can do */
	computation # shell(new_goal(_N, ambient_server#run(Goal, System?))),
	computation #
	    ambient_server#run([spi_monitor#scheduler(S),computation#Start?],
				System),
	write_channel(record(Record), S, S'),
	write_channel(Limit, S', Scheduler),
	synchronize_start,
	output_trees(State, System, Record, 0, {[], []}, FileOut, Done),
	write_files(Done?, State, Options, FileOut).

  synchronize_start(Scheduler, Goal, Start) :-
    channel(Scheduler) :
      Goal = Start.

output_trees(State, System, Record, LastTime,
		LastTuple, FileOut, Done) :-

    Record ? Line,
    convert_to_real(Line, LastTime') :
      LastTime = _ |
	self;

    Record ? ambient(Action),
    arg(1, Action, terminated) :
      LastTime = _,
      LastTuple = _,
      Record' = _,
      State = _,
      System = _,
      Done = done,
      FileOut = [];

    Record ? ambient(Action),
    arg(1, Action, Other), Other =\= terminated :
      LastTuple' = OutTuple? |
	output_tree(State, LastTime, System, OutTuple),
	out_tuple(LastTuple, OutTuple, FileOut, FileOut'),
	self;

    Record ? idle(_Number) :
      LastTime = _,
      LastTuple = _,
      Record' = _,
      State = _,
      System = _,
      FileOut = [],
      Done = done;

    Record ? _,
    otherwise |
	self;

    Record =?= [] :
      LastTime = _,
      LastTuple = _,
      State = _,
      System = _,
      FileOut = [],
      Done = done;

    unknown(Record),
    known(State) :
      LastTime = _,
      LastTuple = _,
      Record = _,
      System = _,
      FileOut = [],
      Done = done.

  out_tuple(LastTuple, OutTuple, FileOut, NextFileOut) :-

    arg(2, LastTuple, Last),
    Last ? _,
    arg(2, OutTuple, List),
    List ? _,
    Last' =?= List' :
      FileOut = NextFileOut;

    otherwise :
      LastTuple = _,
      FileOut ! OutTuple,
      FileOut' = NextFileOut.


output_tree(State, Time, System, OutTuple) :-

	ambient_tree,
	format_output(State, Time, Out, "", Output),
	filter_list(State, Time, List, OutList),
	OutTuple = {Output?, OutList?}.


ambient_tree(System, State, Out, List) :-

    channel(System) :
      write_channel(tree(channels, Tree), System) |
        ambient_tree1 + (Out1 = [], List1 = []);

    otherwise :		/* the server has closed the system tree */
      State = _,
      System = _,
      Out = [],
      List = [].

  ambient_tree1(Tree, State, Out, Out1, List, List1) :-

    Tree =?= _Type(Id, _Channels, SubTree),
    Id =?= system :
      Out ! system,
      List = [system, SubList | List1] |
	ambient_tree2 + (SubList1 = []);

    Tree =?= _Type(Id, _Channels, SubTree),
    Id =\= system :
      Out = ["+", Id | Out'?],
      List = [Id, SubList | List1] |
	ambient_tree2(SubTree, State, Out', ["-" | Out1], SubList, []);

    unknown(Tree),
    known(State) :
      Tree = _,
      Out = Out1,
      List = List1.

  ambient_tree2(SubTree, State, Out, Out1, SubList, SubList1) :-

    SubTree ? Tree |
        ambient_tree1(Tree, State, Out, Out'?, SubList, SubList'),
        self;

    SubTree =?= [] :
      State = _,
      Out = Out1,
      SubList = SubList1;

    unknown(SubTree),
    known(State) :
      SubTree = _,
      Out = Out1,
      SubList = SubList1.


format_output(State, Time, Terms, Indent, Output) :-

    Terms ? "+",
    string_to_dlist(Indent, LIndent, [CHAR_SPACE]),
    list_to_string(LIndent, Indent') |
        self;

    Terms ? "-",
    string_to_dlist(Indent, LIndent, []),
    LIndent ? CHAR_SPACE,
    LIndent' =\= [],
    list_to_string(LIndent', Indent') |
        self;

    Terms ? "-",
    string_to_dlist(Indent, LIndent, []),
    LIndent ? CHAR_SPACE,
    LIndent' =?= [] :
      Indent' = "" |
        self;

    Terms ? String,
    string(String) :
      Output ! String(Time) |
	self;

    Terms ? Term, Term = Functor(Index),
    string_to_dlist(Indent, LI, LT),
    string_to_dlist(Functor, LT, []),
    list_to_string(LI, Line) :
      Output ! Line(Index) |
	self;

    Terms = [] :
      Indent = _,
      State = _,
      Time = _,
      Output = [];

    unknown(Terms),
    known(State) :
      Indent = _,
      Terms = _,
      Time = _,
      Output = ["?"].

filter_list(State, Time, Terms, OutList) :-

    Terms ? Id, Id =?= _(_) :
      OutList ! Id |
	self;

    Terms ? [] |
	self;

    Terms ? List, list(List) :
      OutList ! SubList |
	filter_list(State, Time, List, SubList),
	self;

    Terms ? String,
    string(String) :
      OutList = [String(Time) | OutList'] |
	self;

    Terms =?= [] :
      State = _,
      Time = _,
      OutList = [];

    unknown(Terms),
    known(State) :
      Time = _,
      OutList = [].


write_files(Done, State, Options, FileOut) :-

    Done = done,
    FileOut ? {Output, OutList} :
      Done' = _ |
	formatted_output,
	self;

    FileOut =?= [] :
      Done = _,
      State = _,
      Options = _ |
	unify_without_failure(State, terminated);

    known(State) :
      Done = _,
      FileOut = _,
      Options = _.

  formatted_output(Output, OutList, Options, Done) :-

    list(Options) :
      OutList = _ |
	screen # display_stream(["" | Output], [close(Done, done) | Options]);

    Options = {FOpts, LOpts} |
	screen # display_stream(["" | Output], [close(Done1, done) | FOpts]),
	screen # display(OutList, [close(Done, Done1) | LOpts]).
