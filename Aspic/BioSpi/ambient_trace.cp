-mode(failsafe).
-language([evaluate,compound,colon]).
-export([run/2, run/3, indent/2]).
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
		LastList, FileOut, Done) :-

    Record ? Line,
    convert_to_real(Line, LastTime') :
      LastTime = _ |
	self;

    Record ? ambient(Action),
    arg(1, Action, terminated) :
      LastTime = _,
      LastList = _,
      Record' = _,
      State = _,
      System = _,
      Done = done,
      FileOut = [];

    Record ? ambient(Action),
    arg(1, Action, Other), Other =\= terminated :
      LastList' = OutList? |
	ambient_tree,
	filter_list(State, LastTime, List, OutList),
	out_list(LastList, OutList, FileOut, FileOut'),
	self;

    Record ? idle(_Number) :
      LastTime = _,
      LastList = _,
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
      LastList = _,
      State = _,
      System = _,
      FileOut = [],
      Done = done;

    unknown(Record),
    known(State) :
      LastTime = _,
      LastList = _,
      Record = _,
      System = _,
      FileOut = [],
      Done = done.

  out_list(LastList, OutList, FileOut, NextFileOut) :-

    LastList ? _,
    OutList ? _,
    LastList' =?= OutList' :
      FileOut = NextFileOut;

    otherwise :
      LastList = _,
      FileOut ! OutList,
      FileOut' = NextFileOut.


ambient_tree(System, State, List) :-

    channel(System) :
      write_channel(tree(ambients, Tree), System) |
        ambient_tree1 + (List1 = []);

    otherwise :		/* the server has closed the system tree */
      State = _,
      System = _,
      List = [].

  ambient_tree1(Tree, State, List, List1) :-

    Tree =?= _Type(Id, _Channels, SubTree),
    Id =?= system :
      List = [system, SubList | List1] |
	ambient_tree2 + (SubList1 = []);

    Tree =?= _Type(Id, _Channels, SubTree),
    Id =\= system :
      List = [Id, SubList | List1] |
	ambient_tree2(SubTree, State, SubList, []);

    unknown(Tree),
    known(State) :
      Tree = _,
      List = List1.

  ambient_tree2(SubTree, State, SubList, SubList1) :-

    SubTree ? Tree |
        ambient_tree1(Tree, State, SubList, SubList'),
        self;

    SubTree =?= [] :
      State = _,
      SubList = SubList1;

    unknown(SubTree),
    known(State) :
      SubTree = _,
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
    FileOut ? TreeList, list(TreeList) :
      Done' = _ |
	indent(TreeList, Indented),
	formatted_output,
	self;

    FileOut ? [] |
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

  formatted_output(TreeList, Indented, Options, Done) :-

    list(Options) :
      TreeList = _ |
	screen # display_stream(["" | Indented],
				[close(Done, done) | Options]);

    Options = {FOpts, LOpts} |
	screen # display_stream(["" | Indented], [close(Done1, done) | FOpts]),
	screen # display(TreeList, [close(Done, Done1) | LOpts]).


indent(TreeList, Indented) + (Tail = [], Indent = "") :-

    TreeList ? Id(Index),
    string_to_dlist(Id, LC, []),
    string_to_dlist(Indent, LI, LC) :
      Indented ! IndentedId(Index) |
        list_to_string(LI, IndentedId),
        self;

    TreeList ? Children, list(Children),
    string_to_dlist(Indent, LI, []),
    list_to_string([CHAR_SPACE | LI], IndentChild) |
        indent(Children, Indented, Indented', IndentChild),
        self;

    TreeList ? _Other,
    otherwise |
        self;

    TreeList =?= [] :
      Indent = _,
      Indented = Tail.
