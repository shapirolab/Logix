-language(compound).
-mode(trust).
-export([make_channel/1, make_channel/2, make_channel/3,
	 make_channel/4,
	 "SPC"/1, "SPC"/2,
	 parse_options/5,
	 show_channel/3, show_goal/3, show_goals/3,
	 show_resolvent/3, show_value/3,
	 show_tree/3, close_tree/1,
	 dimer/3, dimer/4, dimer/5,
	 receive/2, receive/3, receive/4,
	 send/2, send/3, send/4]).


make_channel(Channel) :-
	make_channel(Channel, "SYSTEM", infinite, _).
make_channel(Channel, Creator) :-
    string(Creator) |
	make_channel(Channel, Creator, infinite, _).
make_channel(Channel, BaseRate) :-
	make_channel(Channel, "SYSTEM",  BaseRate, _).
make_channel(Channel, Creator, BaseRate) :-
	make_channel(Channel, Creator, BaseRate, _).
make_channel(Channel, Creator, BaseRate, FcpChannel) :-
    we(Channel) :
      Channel = Channel'?,
      FcpChannel = FcpChannel'? |
	psi_monitor#new_channel(Creator, Channel', BaseRate),
	Channel'? = _Creator(FcpChannel', _Circuit);

    string(Channel), Channel =\= "_", Channel =\= "" |
	computation#dictionary(add, Channel, Ch?, Reply),
	made_channel(Channel, Creator, FcpChannel, BaseRate, Ch, Reply);

    otherwise :
      Creator = _,
      BaseRate = _,
      FcpChannel = _ |
	computation#display(("Can't make_channel" : Channel)).

  made_channel(Channel, Creator, FcpChannel, BaseRate, Ch, Reply) :-
    Reply = new,
    string_to_dlist(Creator, CL, CT),
    string_to_dlist(Channel, Cl, []) :
      ascii('.', Dot),
      CT = [Dot | Cl] |
	list_to_string(CL, Creator'),
	psi_monitor#new_channel(Creator', Ch, BaseRate),
	Ch? = _Creator(FcpChannel, _Circuit);

    otherwise :
      Creator = _,
      FcpChannel = _,
      BaseRate = _,
      Ch = _ |
	screen#display(("psi_utils: Can't make_channel" : Channel - Reply)).


send(Message, Channel) :-
    send(Message, Channel, 1, _).
send(Message, Channel, Multiplier) :-
    send(Message, Channel, Multiplier, _).
send(Message, Channel, Multiplier, Chosen) :-
    Channel = Creator(FcpChannel, _Circuit),
    channel(FcpChannel) :
      Send = send(psi(Creator), Message, 1, Multiplier, Chosen),
      write_channel(Send, FcpChannel);

    string(Channel), Channel =\= "_", Channel =\= "" |
	computation#dictionary(find, Channel, Ch, Reply),
	send_message(Message, Channel, Multiplier, Chosen, Ch, Reply);

    otherwise :
      Message = _,
      Multiplier = _ |
	unify_without_failure(Chosen, 0),
	computation#display(("psi_utils: Can't send to" : Channel)).

  send_message(Message, Channel, Multiplier, Chosen, Ch, Reply) :-
    Reply = true,
    [] @< Ch :
      Channel = _ |
	send(Message, Ch, Multiplier, Chosen);

    otherwise :
      Message = _,
      Multiplier = _, 
      Ch = _ |
	unify_without_failure(Chosen, 0),
	computation#display(("psi_utils: Can't send to" : Channel - Reply)).

 
receive(Channel, Message) :-
	receive(Channel, Message, 1, _).
receive(Channel, Message, Multiplier) :-
	receive(Channel, Message, Multiplier, _).
receive(Channel, Message, Multiplier, Chosen) :-
    string(Channel), Channel =\= "_", Channel =\= "" |
	computation#dictionary(find, Channel, Ch, Reply),
	receive_message(Channel, Message, Multiplier, Chosen, Ch, Reply);

    Channel = Creator(FcpChannel, _Circuit),
    channel(FcpChannel) :
      Receive = receive(psi(Creator), Message, 1, Multiplier, Chosen),
      write_channel(Receive, FcpChannel);

    otherwise :
      Message = _,
      Multiplier = _ |
	unify_without_failure(Chosen, 0),
	computation#display(("psi_utils: Can't receive from" : Channel)).
	

  receive_message(Channel, Message, Multiplier, Chosen, Ch, Reply) :-
    Reply = true,
    channel(Ch) :
      Channel = _ |
	receive(Ch, Message, Multiplier, Chosen);

    otherwise :
      Message = _,
      Multiplier = _,
      Ch = _ |
	unify_without_failure(Chosen, 0),
	computation#display(
		("psi_utils: Can't receive from" : Channel - Reply)).


dimer(SendMessage, ReceiveMessage, Channel) :-
	dimer(SendMessage, ReceiveMessage, Channel, 1, _).
dimer(SendMessage, ReceiveMessage, Multiplier, Channel) :-
	dimer(SendMessage, ReceiveMessage, Channel, Multiplier, _).
dimer(SendMessage, ReceiveMessage, Channel, Multiplier, Chosen) :-
    Channel = Creator(FcpChannel, _Circuit) :
      Dimer = dimer(psi(Creator), Message, {1, 2}, Multiplier, Chosen),
      write_channel(Dimer, FcpChannel) |
	unify_without_failure({Chosen?, Message}, {1, SendMessage}),
	unify_without_failure({Chosen?, Message}, {2, ReceiveMessage});

    string(Channel), Channel =\= "_", Channel =\= "" |
	computation#dictionary(find, Channel, Ch, Reply),
	dimer_message(SendMessage, ReceiveMessage, Multiplier,
			Channel, Chosen, Ch, Reply);

    otherwise :
      SendMessage = _,
      ReceiveMessage= _,
      Multiplier = _ |
	unify_without_failure(Chosen, 0),
	computation#display(("psi_utils: Can't dimer on" : Channel)).

  dimer_message(SendMessage, ReceiveMessage, Channel, Multiplier,
			Chosen, Ch, Reply) :-
    Reply = true,
    [] @< Ch :
      Channel = _ |
	dimer(SendMessage, ReceiveMessage, Multiplier, Chosen, Ch);

    otherwise :
      SendMessage = _,
      ReceiveMessage = _,
      Multiplier = _,
      Ch = _ |
	unify_without_failure(Chosen, 0),
	computation#display(("psi_utils: Can't dimer on" : Channel - Reply)).


"SPC"(Channel) :-
	psi_monitor#options(Options, Options),
	"SPC"(Channel, Options?).

"SPC"(Channel, Options) :-
	computation#display(term, Display, known(Display)),
	show_channel.

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
	computation#display(("psi_utils: Can't show channel" : Channel-Reply));

    Reply =?= true,
    Channel = Name(FcpChannel, {_Left, _Right}),
    string(Name), channel(FcpChannel) |
	show_named_channel1;

    otherwise :
      Options = _,
      Reply = _,
      Display = "not_channel" |
	computation#display(("psi_utils: Not a Psi channel" : Channel)).

  show_named_channel1(Name, FcpChannel, Options, Display) :-

    true :
      write_channel(inspect(Status, []), FcpChannel),
      make_channel(BadOption, _) |
	parse_options(Options, Depth(1), BadOption(BadOption),
		      Sender(no_sender), Which(active)),
	channel_argument + (Display = Content,
		Left = Display, Right = (Name : Content));

    /* closed channel */
    otherwise :
      FcpChannel = _,
      Options = _,
      Display = [Name].


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
	computation#display(("psi_utils: invalid_option" : Option)),
	parse_options.


show_channel_content(Stream, Which, Depth, Sender, Content, Left, Right) :-

    Stream =?= [] :
      Which = _,
      Depth = _,
      Sender = _,
      Content = [],
      Left = Right;

    Stream =\= [], Stream =\= [_|_] :
      Which = _,
      Depth = _,
      Sender = _,
      Content = "invalid stream"(Stream),
      Left = Right;

    Which =\= none,
    Stream ? Kind(Id, Message, Tag, _Multiplier, Chosen) |
	clarify_message(Kind, Message, Message'),
	show_message;

    Which =\= none,
    Stream ? Other, Other =\= _(_, _, _, _, _) :
      Content ! "invalid message"(Other) |
	self;

    Which =?= none,
    Stream =\= [] :
      Depth = _,
      Sender = _,
      Content = "!",
      Left = Right.

  clarify_message(Kind, Message, Clarified) :-

    Kind =?= receive :
      Message = _,
      Clarified = "_";

    Kind =?= send :
      Clarified = Message;

    Kind =?= dimer,
    Message =?= {SendMessage, _ReceiveMessage} :
      Clarified = SendMessage.


show_message(Kind, Id, Message, Tag, Chosen, Stream, Which, Depth, Sender,
		Content, Left, Right) :-

    Which =?= active,
    we(Chosen) :
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
      Kind = _,
      Id = _,
      Message = _,
      Tag = _,
      Chosen = _ |
	show_channel_content.

  type_of_choice(Tag, Chosen, Type) :-

    we(Chosen) :
      Tag = _,
      Type = active;

    ro(Chosen) :
      Tag = _,
      Type = suspended;

    not_we(Chosen), Chosen =?= 0 :
      Tag = _,
      Type = withdrawn;

    not_we(Chosen), Chosen =\= 0, Tag =?= Chosen :
      Type = consumed;

    not_we(Chosen), Chosen =\= 0, Tag =\= Chosen :
      Type = cancelled.
    

condense_message(Kind, Id, Message, Type, Which, Depth, Sender, Condensed,
			Left, Right) :-

    unknown(Message) :
      Which = _,
      Depth = _,
      Sender = _ |
	id_and_message + (Msg = Type);

    known(Type),
    Message =?= [] :
      Which = _,
      Depth = _ |
	id_and_message + (Msg = Type([]));

    known(Type),
    Depth > 1,
    tuple(Message),
    arity(Message, Index),
    Index++,
    make_tuple(Index', Msg),
    arg(1, Msg, T),
    Depth-- :
      T = Type |
	id_and_message + (Left = Left, Right = Left'?),
	show_message_args;

    known(Type),
    Depth =< 1 :
      Message = _,
      Which = _,
      Depth = _ |
	id_and_message + (Msg = Type);

    otherwise :
      Which = _,
      Depth = _,
      Sender = _ |
	id_and_message + (Msg = Type(Message)).

  id_and_message(Kind, Id, Msg, Sender, Condensed, Left, Right) :-

    Sender =?= sender :
      Condensed = {Id, Kind - Msg},
      Left = Right;

    otherwise :
      Sender = _,
      Id = _,
      Condensed = Kind - Msg,
      Left = Right.


show_message_args(Message, Which, Depth, Sender, Index, Msg, Left, Right) :-

    Index > 1,
    arg(Index, Msg, Display),
    Index--,
    arg(Index', Message, Argument) |
	show_argument + (Left = Left, Right = Left'?),
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
	show_argument + (Left = Left, Right = Left'?),
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
	show_argument + (Left = Display, Right = Display'?).

show_argument(Argument, Which, Depth, Sender, Display, Left, Right) :-

    Argument = Name(FcpChannel, {_L, _R}), string(Name), channel(FcpChannel) |
	show_channel_argument;

    constant(Argument) :
      Which = _,
      Depth = _,
      Sender = _,
      Display = Argument,
      Left = Right;

    ro(Argument) :
      Which = _,
      Depth = _,
      Sender = _,
      Display = Argument,
      Left = Right;

    we(Argument) :
      Which = _,
      Depth = _,
      Sender = _,
      Display = Argument,
      Left = Right;

    Argument = Name(_FcpChannel, _Circuit), unknown(Name) |
	show_compound;

    Argument = _Name(FcpChannel, _Circuit), unknown(FcpChannel) |
	show_compound;

    Argument = _Name(_FcpChannel, Circuit), unknown(Circuit) |
	show_compound;

    Argument = _Name(_FcpChannel, Circuit), Circuit =\= {_, _} |
	show_compound;

    otherwise |
	show_compound.

show_channel_argument(Name, FcpChannel, Which, Depth, Sender, Display,
				Left, Right) :-

    true :
      Name = _,
      write_channel(inspect(Status, []), FcpChannel) |
	channel_argument;

    otherwise :
      FcpChannel = _,
      Which = _,
      Depth = _,
      Sender = _,
      Display = [Name],
      Left = Right.

channel_argument(Status, Which, Depth, Sender, Display, Left, Right) :-

    Depth =\= 0,
    Status =?= [_Type(CreatorBase, [])] :
      Which = _,
      Depth = _,
      Sender = _,
      Display = Creator?,
      Left = Right |
	channel_creator;

    Depth =\= 0,
    Status =?= [_Type(CreatorBase, Stream)], Stream =\= [],
    Which =?= none |
      Sender = _,
      Display = (Creator? !),
      Left = Right,
	channel_creator;

    Depth =\= 0,
    Status =?= [_Type(CreatorBase, Stream)], Stream =\= [],
    Which =\= none :
      Display = (Creator? = Content) |
	channel_creator,
	show_channel_content;

    Depth =?= 0 :
    Status =?= [_Type(CreatorBase, _Stream)],
      Status = _,
      Which = _,
      Sender = _,
      Display = Creator?,
      Left = Right |
	channel_creator.

  channel_creator(CreatorBase, Creator) :-

    CreatorBase = ProcessName(infinite) :
      Creator = ProcessName;

    CreatorBase = ProcessName(RealInteger),
    convert_to_integer(RealInteger, I),
    convert_to_real(I, R),
    R =:= RealInteger :
      Creator = ProcessName(I);

    otherwise :
      Creator = CreatorBase.


show_compound(Argument, Which, Depth, Sender, Display, Left, Right) :-

    tuple(Argument),
    arity(Argument, Index),
    make_tuple(Index, Args) :
      Tuple = Argument,
      Display = Args |
	show_tuple_args;

    Argument ? A :
      Display ! D |
	show_argument(A, Which, Depth, Sender, D, Left, Left'?),
	show_argument;

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
    arity(Goal, Index) :
      make_channel(BadOption, _) |
	parse_options(Options, Depth(1), BadOption(BadOption),
		      Sender(no_sender), Which(active)),
	show_goal2;

    Goal =?= (Service#Goal') :
      PiFcp = (Service#PiFcp') |
	self;

    otherwise :
      Goal = _,
      Options = _,
      PiFcp = non_goal(Goal),
      Left = Right.

show_goal1(Goal, Which, Depth, Sender, PiFcp, Left, Right) :-

    string(Goal) :
      Which = _,
      Depth = _,
      Sender = _,
      PiFcp = Goal,
      Left = Right;

    tuple(Goal), Goal =\= (_#_),
    arg(1, Goal, Name), string(Name),
    arity(Goal, Index) |
	show_goal2;

    Goal =?= (Service#Goal') :
      PiFcp = (Service#PiFcp') |
	self;

    otherwise :
      Goal = _,
      Which = _,
      Depth = _,
      Sender = _,
      PiFcp = non_goal(Goal),
      Left = Right.

  show_goal2(Goal, Index, Which, Depth, Sender, PiFcp, Left, Right, Name) :-

    nth_char(1, Name, Char1),
    ascii('A') =< Char1, Char1 =< ascii('Z') |
	goal_channels;

    nth_char(1, Name, Char1),
    Char1 =:= ascii('.'),
    nth_char(2, Name, Char2),
    ascii('A') =< Char2, Char2 =< ascii('Z') |
	goal_channels;

    otherwise,
    make_tuple(Index, Tuple),
    arg(1, Tuple, N) :
      N = Name,
      PiFcp = Tuple |
	goal_channels1.


goal_channels(Goal, Index, Which, Depth, Sender, PiFcp, Left, Right) :-

/* Exclude trailing non-pi arguments (mostly) */

    Index > 1, arg(Index, Goal, Argument),
    we(Argument),
    Index-- |
	self;

    Index > 1, arg(Index, Goal, Argument),
    ro(Argument),
    Index-- |
	self;

    Index > 1, arg(Index, Goal, Argument),
    Argument = Name(_FcpChannel, _Circuit), unknown(Name),
    Index-- |
	self;

    Index > 1, arg(Index, Goal, Argument),
    Argument = _Name(FcpChannel, _Circuit), unknown(FcpChannel),
    Index-- |
	self;

    Index > 1, arg(Index, Goal, Argument),
    Argument = _Name(_FcpChannel, Circuit), unknown(Circuit),
    Index-- |
	self;

    Index > 1, arg(Index, Goal, Argument),
    Argument = _Name(_FcpChannel, Circuit), Circuit =\= {_, _},
    Index-- |
	self;

    Index > 1, arg(Index, Goal, Argument),
    Argument = Id(FcpChannel, {_L, _R}), string(Id), channel(FcpChannel),
    make_tuple(Index, Tuple),
    arg(1, Goal, Name),
    arg(1, Tuple, N) :
      N = Name,
      PiFcp = Tuple |
	goal_channels1;

    Index-- > 1,
    otherwise |
	self;

    Index =< 1,
    arg(1, Goal, Name) :
      Which = _,
      Depth = _,
      Sender = _,
      PiFcp = Name,
      Left = Right.

  goal_channels1(Goal, Index, Which, Depth, Sender, PiFcp, Left, Right) :-

    Index > 1,
    arg(Index, Goal, Argument),
    arg(Index, PiFcp, Display),
    Index-- |
	show_argument + (Left = Left, Right = Left'?),
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
      Options = _,
      Options' = [] |
	self;

    Options =\= fcp :
      make_channel(BadOption, _) |
	parse_options(Options, Depth(1), BadOption(BadOption),
		      Sender(no_sender), Which(active)),
	show_goals(Goals, Which?, Depth?, Sender?, Result, Output, Result?).

  show_goals(Goals, Which, Depth, Sender, Result, Left, Right) :-

    Goals =?= (Goal, Goals') :
      Result = (PiFcp?, Result') |
	remote_goal([], Goal, Goal', PiFcp, PiFcp'),
	show_goal1 + (Left = Left, Right = Left'?),
	self;

    Goals =\= (_, _) |
	remote_goal([], Goals, Goal, Result, PiFcp),
	show_goal1.


show_resolvent(Resolvent, Options, Stream) :-

    Options =?= fcp :
      Options' = [] |
	self;

    Options =\= fcp :
      make_channel(BadOption, _) |
	parse_options(Options, Depth(1), BadOption(BadOption),
		      Sender(no_sender), Which(none)),
	collect_resolvent_list(Resolvent, List),
	show_goal_list(List?, Depth?, Sender?, Which?,
			Result, Stream, Result?).

show_goal_list(List, Depth, Sender, Which, Result, Left, Right) :-

    List ? Name(Goal) :
      Result ! PiFcp? |
	remote_goal(Name, Goal, Goal', PiFcp, PiFcp'),
	show_goal1,
	self;

    List =?= [] :
      Which = _,
      Depth = _,
      Sender = _,
      Result = [],
      Left = Right.


collect_resolvent_list(Resolvent, List) :-

    Resolvent ? Call,
    Call = call(_, _) | % :
%     List ! Call |   
	self;

    Resolvent ? [Name | _] # Goals |
	collect_resolvent_goals(Name, Goals, List, List'),
	self;

    Resolvent ? Name # Goals,
    Name =\= [_|_] |
	collect_resolvent_goals(Name, Goals, List, List'),
	self;

    Resolvent ? Goals,
    Goals =\= call(_, _),
    Goals =\= (_#_) |
	collect_resolvent_goals([], Goals, List, List'),
	self;

    Resolvent = [] :
      List = [].

  collect_resolvent_goals(Name, Goals, List, NextList) :-

    Goals ? Goal,
    Goal =\= psi_send(_, _, _, _),
    Goal =\= psi_receive(_, _, _, _) :
      List ! Name(Goal) |
	self;

    Goals ? _Wait,
    otherwise |
	self;

    Goals =\= [_ | _],
    Goals =\= [] :
      Goals' = [Goals] |
	self;

    Goals =?= [] :
      Name = _,
      List = NextList.


remote_goal(Name, Goal, OutGoal, PiFcp, OutPiFcp) :-

    Name =\= [] :
      Name' = [],
      PiFcp = Name#PiFcp'? |
	self;

    Name =?= [],
    Goal =?= Target#Goal' :
      PiFcp = Target#PiFcp'? |
	self;

    Name =?= [],
    Goal =\= _#_ :
      OutGoal = Goal,
      OutPiFcp = PiFcp.
