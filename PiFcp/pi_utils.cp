-language(compound).
-mode(failsafe).
-export([make_channel/1, make_channel/2, make_channel/3,
	 make_channel/4, make_channel/5,
	 parse_options/5,
	 show_channel/3, show_goal/3, show_goals/3,
	 show_resolvent/3, show_value/3,
	 show_tree/3, close_tree/1,
	 receive/2, send/2]).


make_channel(Channel) :-
	make_channel(Channel, "SYSTEM", 0, 0, _).
make_channel(Channel, Creator) :-
	make_channel(Channel, Creator, 0, 0, _).
make_channel(Channel, ReceiveMean, SendMean) :-
	make_channel(Channel, "SYSTEM",  ReceiveMean, SendMean, _).
make_channel(Channel, Creator, ReceiveMean, SendMean) :-
	make_channel(Channel, Creator,  ReceiveMean, SendMean, _).
make_channel(Channel, Creator, ReceiveMean, SendMean, VC) :-
    we(Channel) :
      make_vector(2, VC, Streams),
      Streams = {MsC, _},
      store_vector(2, MsC, VC),
      Channel = Creator(VC, {ReceiveMean, SendMean});
    string(Channel), Channel =\= "_", Channel =\= "" |
	computation#dictionary(add, Channel, V, R),
	made_channel(Channel, Creator, VC, ReceiveMean, SendMean, V, R).

  made_channel(Channel, Creator, VC, ReceiveMean, SendMean, V, R) :-
    R = new,
    string_to_dlist(Creator, CL, CT),
    string_to_dlist(Channel, Cl, []) :
      CT = [46 | Cl],
      make_vector(2, VC, Streams),
      Streams = {MsC, _},
      store_vector(2, MsC, VC),
      V = Creator'?(VC, {ReceiveMean, SendMean}) |
	list_to_string(CL, Creator');
    otherwise :
      Creator = _,
      VC = _,
      ReceiveMean = _,
      SendMean = _,
      V = _ |
	screen#display(("pi_utils: Can't make_channel" : Channel - R)).


send(Message, Channel) :-
    Channel = _Creator(VC, _Args) :
      Ms = Sender?(Message, 1, _),
      write_vector(1, Ms, VC) |
	pi_monitor#unique_sender("PI_UTILS.send", Sender);

    string(Channel), Channel =\= "_", Channel =\= "" |
	computation#dictionary(find, Channel, V, R),
	send_message(Message, Channel, V, R);

    otherwise :
      Message = _ |
	computation#display(("pi_utils: Can't send to" : Channel)).

  send_message(Message, Channel, V, R) :-
    R = true,
    [] @< V :
      Channel = _ |
	send(Message, V);

    otherwise :
      Message = _,
      V = _ |
	computation#display(("pi_utils: Can't send to" : Channel - R)).


receive(Channel, Message) + (Stream = _) :-
    string(Channel), Channel =\= "_", Channel =\= "" :
      Stream = _ |
	computation#dictionary(find, Channel, V, R),
	receive_message(Channel, Message, V, R);

    we(Stream),
    Channel = _Creator(VC, _Args),
    read_vector(2, VC, Stream') |
	self;

    Stream ? _Sender(_Message, _N, Choice),
    not_we(Choice) |
	self;

    Channel = _Creator(VC, _Args),
    Stream ? _Sender(M, N, Choice),
    we(Choice) :
      store_vector(2, Stream', VC),
      Choice = N,
      Message = M.


  receive_message(Channel, Message, V, R) :-
    R = true,
    [] @< V :
      Channel = _ |
	receive(V, Message);

    otherwise :
      Message = _,
      V = _ |
	computation#display(("pi_utils: Can't receive from" : Channel - R)).
    

show_channel(Channel, Options, Display) :-

    string(Channel), Channel =\= "_", Channel =\= "" |
	computation#dictionary(find, Channel, Channel', Reply),
	show_named_channel;

    otherwise |
	show_named_channel + (Reply = true).


show_named_channel(Channel, Options, Display, Reply) :-

    Reply =\= true :
      Options = _,
      Display = "not_channel" |
	computation#display(("pi_utils: Can't show channel" : Channel-Reply));

    Reply =?= true,
    Channel = Name(Vector, _Args), string(Name), vector(Vector),
    read_vector(2, Vector, Stream) :
      make_channel(BadOption, _) |
	parse_options(Options, Depth(1), BadOption(BadOption),
		      Sender(no_sender), Which(active)),
	show_channel_content + (Left = Display, Right = (Name : Content));

    otherwise :
      Options = _,
      Reply = _,
      Display = "not_channel" |
	computation#display(("pi_utils: Not a channel" : Channel)).

parse_options(Options, Depth, Order, Sender, Which) :-

    Options =\= [_|_], Options =\= [] :
      Options' = [Options] |
	self;

    Options ? Option |
	define_option;

    Options = [] |
	unify_without_failure(Options, _),
	unify_without_failure(Depth, D(D)),
	unify_without_failure(Order, O(O)),
	unify_without_failure(Sender, S(S)),
	unify_without_failure(Which, W(W)).

  define_option(Options, Depth, Order, Sender, Which, Option) :-

    Option = fcp |
	parse_options;

    integer(Option) :
      Depth = Option(_) |
	parse_options;

    Option = depth :
      Order = Option(_) |
	parse_options;

    Option = execute :
      Order = Option(_) |
	parse_options;

    Option = prefix :
      Order = Option(_) |
	parse_options;

    Option = sender :
      Sender = Option(_) |
	parse_options;

    Option = no_sender :
      Sender = Option(_) |
	parse_options;

    Option = none :
      Which = Option(_) |
	parse_options;

    Option = active :
      Which = Option(_) |
	parse_options;

    Option = all :
      Which = Option(_) |
	parse_options;

    otherwise |
	computation#display(("pi_utils: invalid_option" : Option)),
	parse_options.


show_channel_content(Stream, Which, Depth, Sender, Content, Left, Right) :-

    unknown(Stream) :
      Which = _,
      Depth = _,
      Sender = _,
      Content = [],
      Left = Right;

    Stream =\= [_|_] :
      Which = _,
      Depth = _,
      Sender = _,
      Content = "invalid stream"(Stream),
      Left = Right;

    Which =\= none,
    Stream ? Id(Message, Tag, Choice) |
	show_message;

    Which =\= none,
    Stream ? Other, Other =\= _(_, _, _) :
      Content ! "invalid message"(Other) |
	self;

    Which =?= none,
    known(Stream) :
      Stream = _,
      Depth = _,
      Sender = _,
      Content = "?",
      Left = Right.

show_message(Id, Message, Tag, Choice, Stream, Which, Depth, Sender, Content,
		Left, Right) :-

    Which =?= active,
    we(Choice) :
      Tag = _,
      Type = active,
      Content ! Condensed? |
	condense_message,
	show_channel_content;

    Which =?= all :
      Content ! Condensed? |
	type_of_choice,
	condense_message + (Left = Left, Right = Left'?),
	show_channel_content;

    otherwise :
      Id = _,
      Message = _,
      Tag = _,
      Choice = _ |
	show_channel_content.

  type_of_choice(Tag, Choice, Type) :-

    we(Choice) :
      Tag = _,
      Type = active;

    ro(Choice) :
      Tag = _,
      Type = suspended;

    not_we(Choice), Choice =?= 0 :
      Tag = _,
      Type = withdrawn;

    not_we(Choice), Choice =\= 0, Tag =?= Choice :
      Type = consumed;

    not_we(Choice), Choice =\= 0, Tag =\= Choice :
      Type = cancelled.
    

condense_message(Type, Id, Message, Which, Depth, Sender, Condensed,
			Left, Right) :-

    known(Type),
    Message =?= [] :
      Which = _,
      Depth = _ |
	id_and_message + (Msg = Type([]));

    known(Type),
    tuple(Message),
    arity(Message, Index),
    Index++,
    make_tuple(Index', Msg),
    arg(1, Msg, T),
    Depth-- :
      T = Type |
	id_and_message + (Left = Left, Right = Left'?),
	show_message_args;

    otherwise :
      Which = _,
      Depth = _,
      Sender = _,
      Message = _ |
	id_and_message + (Msg  = Type(Message)).

  id_and_message(Sender, Id, Msg, Condensed, Left, Right) :-

    Sender =?= sender :
      Condensed = Id(Msg),
      Left = Right;

    otherwise :
      Sender = _,
      Id = _,
      Condensed = Msg,
      Left = Right.


show_message_args(Message, Which, Depth, Sender, Index, Msg, Left, Right) :-

    Index > 1,
    arg(Index, Msg, Display),
    Index--,
    arg(Index', Message, Argument) |
	show_argument + (PiMacro = false, Left = Left, Right = Left'?),
	self;

    Index =?= 1 :
      Message = _,
      Which = _,
      Depth = _,
      Sender = _,
      Msg = _,
      Left = Right.


show_tuple_args(Tuple, Which, Depth, Sender, Index, Args, Left, Right) :-

    Index > 0,
    arg(Index, Args, Display),
    arg(Index, Tuple, Argument),
    Index-- |
	show_argument + (PiMacro = false, Left = Left, Right = Left'?),
	self;

    Index =:= 0 :
      Tuple = _,
      Which = _,
      Depth = _,
      Sender = _,
      Args = _,
      Left = Right.


show_value(Argument, Options, Display) :-

    Options =?= fcp :
      Display = Argument;

    Options =\= fcp :
      make_channel(BadOption, _) |
	parse_options(Options, Depth(1), BadOption(BadOption),
		      Sender(no_sender), Which(active)),
	show_argument + (PiMacro = false, Left = Display, Right = Display'?).

show_argument(Argument, Which, Depth, Sender, PiMacro, Display, Left, Right) :-

    Argument = Name(Vector, _Args), string(Name), vector(Vector),
    Depth =\= 0,
    read_vector(2, Vector, Stream),
    unknown(Stream) :
      Which = _,
      Sender = _,
      PiMacro = _,
      Display = Name,
      Left = Right;

    Argument = Name(Vector, _Args), string(Name), vector(Vector),
    Depth =\= 0,
    Which =\= none,
    read_vector(2, Vector, Stream),
    known(Stream) :
      PiMacro = _,
      Display = (Name = Content) |
	show_channel_content;

    Argument = Name(Vector, _Args), string(Name), vector(Vector),
    Depth =\= 0,
    Which =?= none,
    read_vector(2, Vector, Stream),
    known(Stream) :
      Sender = _,
      PiMacro = _,
      Display = (Name!),
      Left = Right;

    Argument = Name(Vector, _Args), string(Name), vector(Vector),
    Depth =?= 0 :
      Which = _,
      Sender = _,
      PiMacro = _,
      Display = Name,
      Left = Right;

    constant(Argument) :
      Which = _,
      Depth = _,
      Sender = _,
      PiMacro = _,
      Display = Argument,
      Left = Right;

    tuple(Argument),
    PiMacro = true,
    arity(Argument, Index),
    Index++,
    make_tuple(Index', Msg),
    arg(1, Msg, String) :
      Depth = _,
      Depth' = 0,
      Message = Argument,
      Display = Msg,
      String = message |
	show_message_args;

    ro(Argument) :
      Which = _,
      Depth = _,
      Sender = _,
      PiMacro = _,
      Display = Argument,
      Left = Right;

    we(Argument) :
      Which = _,
      Depth = _,
      Sender = _,
      PiMacro = _,
      Display = Argument,
      Left = Right;

    Argument = Name(_Vector, _Args), unknown(Name) :
      PiMacro = _ |
	show_compound;

    Argument = _Name(Vector, _Args), unknown(Vector) :
      PiMacro = _ |
	show_compound;

    otherwise :
      PiMacro = _ |
	show_compound.

  show_compound(Argument, Which, Depth, Sender, Display, Left, Right) :-

    tuple(Argument),
    arity(Argument, Index),
    make_tuple(Index, Args) :
      Tuple = Argument,
      Display = Args |
	show_tuple_args;

    Argument ? A :
      Display ! D |
	show_argument(A, Which, Depth, Sender, false, D, Left, Left'?),
	show_argument + (PiMacro = false);

   otherwise :
      Which = _,
      Depth = _,
      Sender = _,
      Display = Argument,
      Left = Right.


show_goal(Goal, Options, Output) :-

    Options =?= fcp :
      Output = Goal;

    Options =\= fcp |
	show_goal(Goal, Options, PiFcp, Output, PiFcp?).

show_goal(Goal, Options, PiFcp, Left, Right) :-

    string(Goal) :
      Options = _,
      PiFcp = Goal,
      Left = Right;

    tuple(Goal), Goal =\= (_#_),
    arg(1, Goal, Name), string(Name),
    arity(Goal, Index),
    make_tuple(Index, Tuple),
    arg(1, Tuple, N) :
      N = Name,
      PiFcp = Tuple,
      make_channel(BadOption, _) |
	parse_options(Options, Depth(1), BadOption(BadOption),
		      Sender(no_sender), Which(active)),
	goal_channels;

    Goal =?= (Service#Goal') :
      PiFcp = (Service#PiFcp') |
	self;

    otherwise :
      Goal = _,
      Options = _,
      PiFcp = non_goal(Goal),
      Left = Right.

  goal_channels(Goal, Index, Which, Depth, Sender, PiFcp, Left, Right) :-

    Index =?= 3,
    arg(1, Goal, pi_send),
    arg(Index, Goal, Argument),
    arg(Index, PiFcp, Display),
    Index-- |
	show_argument + (PiMacro = true, Left = Left, Right = Left'?),
	self;

    Index =?= 3,
    arg(1, Goal, pi_receive),
    arg(Index, Goal, Argument),
    arg(Index, PiFcp, Display),
    Index-- |
	show_argument + (PiMacro = true, Left = Left, Right = Left'?),
	self;

    Index > 1,
    otherwise,
    arg(Index, Goal, Argument),
    arg(Index, PiFcp, Display),
    Index-- |
	show_argument + (PiMacro = false, Left = Left, Right = Left'?),
	self;

    Index = 1 :
      Goal = _,
      Which = _,
      Depth = _,
      Sender = _,
      PiFcp = _,
      Left = Right.


close_tree(Tree) :-

    Tree ? tree(_, Context, BranchList) :
      close_channel(Context) |
	close_tree(BranchList),
	close_tree;

    Tree ? reduce(_, _, Time, _),
    unknown(Time) |
	close_tree;

    Tree ? reduce(_, _, Time, BranchList),
    number(Time) |
	close_tree(BranchList),
	close_tree;

    Tree ? _,
    otherwise |
	close_tree;

    Tree = [] |
	true;

    Tree =\= [_|_], Tree =\= [] :
      Tree' = [Tree] |
	close_tree.


show_tree(Tree, Options, TreeTrace) :-

	parse_options(Options, Depth(1), Order(prefix),
		      Sender(no_sender), Which(none)),
	nodes([Tree], [Depth, Sender, Which], 0, 0, Head, []),
	display_tree(Head, Order, TreeTrace).

nodes(Nodes, Options, Level, Time, Head, Tail) :-

    Nodes ? tree(TreeId, Context, BranchList) :
      Level = _,
      Head ! begin(PiTreeId?, Context, _Link) |
	show_treeid,
	nodes(BranchList, Options, 0, Time,
		Head', [end(PiTreeId?) | Head'']),
	nodes;

    Nodes ? Goal, Goal = _#_ :
      Head ! {'#', PiGoal?, Level, Time} |
	show_goal(Goal, Options, PiGoal),
	nodes;

    Nodes ? reduce(Goal, _Id, Time', BranchList),
    Level++ :
      Head ! Executed |
	nodes(Nodes', Options, Level, Time, Head', Head''),
	show_goal(Goal, Options, PiGoal),
	executed(PiGoal, Level, Time', Executed, BranchList, Nodes''),
	nodes;

    Nodes = [] :
      Options = _,
      Level = _,
      Time = _,
      Head = Tail .

  show_treeid(TreeId, PiTreeId) :-

    TreeId = (ServiceId#TreeId') :
      PiTreeId = (ServiceId#PiTreeId') |
	self;

    TreeId ? Goal :
      PiTreeId ! PiFcp? |
	show_goal(Goal, [0], PiFcp),
	self;

    TreeId = [] :
      PiTreeId = [];

    otherwise |
	show_goal(TreeId, [0], PiTreeId).

executed(PiGoal, Level, Time, Executed, BranchList, Nodes) :-

    Time = failed(_, FailTime) : BranchList = _,
      Executed = {'-', PiGoal, Level, FailTime},
      Nodes = [] ;

    Time = unknown(_, UnknownTime) : BranchList = _,
      Executed = {'*', PiGoal, Level, UnknownTime},
      Nodes = [] ;

    number(Time) :
      Executed = {'|', PiGoal, Level, Time},
      Nodes = BranchList ;

    unknown(Time) : BranchList = _,
      Executed = {'?', PiGoal, Level, 10000000000000000.0},
      Nodes = [] .


display_tree(Head, Order, TreeTrace) :-

    Order =?= prefix |
	display_prefix_order(Head, 0, "", TreeTrace);

    Order =?= execute |
	sort_on_time(Head, Sorted),
	display_execute_order.

  display_execute_order(Sorted, TreeTrace) :-

    Sorted ? {Operator, Goal, _, _}, Operator =\= "|",
    known(Goal) :
      TreeTrace ! {Operator, "", Goal} |
	self;

    Sorted ? {"|", Goal, _, _},
    known(Goal) :
      TreeTrace ! Goal |
	self;

    Sorted =?= [] :
      TreeTrace = [].


display_prefix_order(Head, Level, Indent, TreeTrace) :-

    Head ? begin(PiTreeId, _Context, _Link),
    known(PiTreeId) :
      Level = _,
      Indent = _,
      Level' = 0,
      Indent' = "",
      TreeTrace ! (begin : PiTreeId) |
	self;

    Head ? end(PiTreeId),
    known(PiTreeId) :
      TreeTrace ! (end : PiTreeId) |
	self;

    Head ? Operator(Goal, Level', _Time),
    known(Goal) :
      TreeTrace ! {Operator, Indent', Goal} |
	update_indent(Level, Level', Indent, Indent'),
	self;

    Head =?= [] :
      Level = _,
      Indent = _,
      TreeTrace = [].

  update_indent(Level1, Level2, Indent1, Indent2) :-

    Level2 > 20,
    mod(Level2, 20, Level2') |
	self;

    Level1 < Level2,
    Level2 =< 20,
    Level1++,
    string_to_dlist(Indent1, IL, []),
    list_to_string([32 | IL], Indent1') |
	self;

    Level1 > Level2,
    Level1--,
    string_to_dlist(Indent1, IL, []),
    IL ? _,
    list_to_string(IL', Indent1') |
	self;

    Level1 =?= Level2 :
      Indent2 = Indent1.


sort_on_time(List, Sorted) + (Tail =[]) :-

    List ? Reduce, Reduce = Functor(_, _, Time), Functor =\= begin |
	partition(Time, List', Small, Large),
	sort_on_time(Small?, Sorted, [Reduce | LSorted]),
	sort_on_time(Large?, LSorted, Tail);

    List ? _,
    otherwise |
	self;

    List = [] :
      Sorted = Tail.

  partition(X, List, Small, Large) :-

    List ? begin(_, _, _) |
	self;

    List ? end(_) |
	self;

    List ? Reduce, Reduce =?= Operator(_Goal, _Level, Y), Operator =\= begin,
    X > Y :
      Small ! Reduce |
	partition;

    List ? Reduce, Reduce =?= Operator(_Goal, _Level, Y), Operator =\= begin,
    X =< Y :
      Large ! Reduce |
	partition;

    List = [] :
      X = _,
      Small = [],
      Large = [].


show_goals(Goals, Options, Output) :-

    Options =?= fcp :
      Options' = [] |
	self;

    Options =\= fcp :
      make_channel(BadOption, _) |
	parse_options(Options, Depth(1), BadOption(BadOption),
		      Sender(no_sender), Which(active)),
	show_goals(Goals, [Depth?, Sender?, Which?], Result, Output, Result?).

  show_goals(Goals, Options, Result, Left, Right) :-

    Goals =?= (Goal, Goals') :
      Result = (Goal'?, Result') |
	show_goal(Goal, Options, Goal', Left, Left'),
	self;

    Goals =\= (_, _) |
	show_goal(Goals, Options, Result, Left, Right).


show_resolvent(Resolvent, Options, Stream) :-

    Options =?= fcp :
      Options' = [] |
	self;

    Options =\= fcp :
      make_channel(BadOption, _) |
	parse_options(Options, Depth(1), BadOption(BadOption),
		      Sender(no_sender), Which(active)),
	show_resolvent_stream(Resolvent, [Depth?, Sender?, Which?], Stream).

  show_resolvent_stream(Resolvent, Options, Stream) :-

    Resolvent ? Call,
    Call = call(_, _) :
      Stream ! Call |
	self;

    Resolvent ? [Name | _] # Goals |
	show_resolvent_goals(Name, Options, Goals, Stream, Stream'),
	self;

    Resolvent ? Name # Goals,
    Name =\= [_|_] |
	show_resolvent_goals(Name, Options, Goals, Stream, Stream'),
	self;

    Resolvent ? Goals,
      Goals =\= (_#_) |
	show_resolvent_goals([], Options, Goals, Stream, Stream'),
	self;

    Resolvent = [] :
      Options = _,
      Stream = [].


  show_resolvent_goals(Name, Options, Goals, Stream, NextStream) :-

    Goals ? Goal,
    Name =\= [] |
	show_goal(Goal, Options, PiFcp, Stream, [(Name # PiFcp?) | Stream'?]),
	self;

    Goals ? Goal,
    Name =?= [] |
	show_goal(Goal, Options, PiFcp, Stream, [PiFcp? | Stream'?]),
	self;

    Goals =\= [_|_], Goals =\= [],
    Name =\= [] |
	show_goal(Goals, Options, PiFcp, Stream,
	    [(Name # PiFcp?) | NextStream]);

    Goals =\= [_|_], Goals =\= [],
    Name =?= [] |
	show_goal(Goals, Options, PiFcp, Stream, [PiFcp? | NextStream]);

    Goals =?= [] :
      Name = _,
      Options = _,
	Stream = NextStream.
