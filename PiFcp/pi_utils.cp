-language(compound).
-mode(failsafe).
-export([make_channel/1, make_channel/4,
	 show_channel/3, show_goal/3, show_tree/3, close_tree/1,
	 receive/2, send/2]).


make_channel(Channel)+(Creator = "SYSTEM", VC = _, MsC = _) :-
    we(Channel) :
      make_channel(VC, MsC),
      Channel = Creator(VC, MsC);
    string(Channel), Channel =\= "_", Channel =\= "" |
	computation#dictionary(add, Channel, V, R),
	made_channel(Channel, Creator, VC, MsC, V, R).

  made_channel(Channel, Creator, VC, MsC, V, R) :-
    R = new,
    string_to_dlist(Creator, CL, CT),
    string_to_dlist(Channel, Cl, []) :
      CT = [46 | Cl],
      make_channel(VC, MsC),
      V = Creator'?(VC, MsC) |
	list_to_string(CL, Creator');
    otherwise :
      Creator = _,
      VC = _,
      MsC = _,
      V = _ |
	screen#display(("pi_utils: Can't make_channel" : Channel - R)).


send(Message, Channel) :-
    Channel = _Creator(C, _Stream),
    channel(C) :
      Ms = Sender?(Message, 1, _),
      write_channel(Ms, C) |
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


receive(Channel, Message) :-
    string(Channel), Channel =\= "_", Channel =\= "" |
	computation#dictionary(find, Channel, V, R),
	receive_message(Channel, Message, V, R);

    Channel = Creator(C, Stream),
    Stream ? _Sender(_Message, _N, Choice),
    not_we(Choice) :
      Channel' = Creator(C, Stream') |
	self;

    Channel = _Creator(_C, Stream),
    Stream ? _Sender(M, N, Choice),
    we(Choice) :
      Stream' = _,
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
    Channel = Name(Vector, Stream), string(Name), vector(Vector) :
      make_channel(Order, _),
      Display = (Name : Content) |
	parse_options(Options, Depth(1), Order(Order),
		      Sender(no_sender), Which(active)),
	show_channel_content;

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


show_channel_content(Stream, Which, Depth, Sender, Content) :-

    unknown(Stream) :
      Which = _,
      Depth = _,
      Sender = _,
      Content = [];

    Stream =\= [_|_] :
      Which = _,
      Depth = _,
      Sender = _,
      Content = "invalid stream"/*(Other)*/;

    Which =\= none,
    Stream ? Id(Message, _Tag, Choice) |
	show_message;

    Which =\= none,
    Stream ? Other, Other =\= _(_, _, _) :
      Content ! "invalid message"/*(Other)*/ |
	self;

    Which =?= none,
    known(Stream) :
      Stream = _,
      Depth = _,
      Sender = _,
      Content = "?".

show_message(Id, Message, Choice, Stream, Which, Depth, Sender, Content) :-

    Which =?= active,
    we(Choice) :
      Type = active,
      Content ! Condensed? |
	condense_message,
	show_channel_content;

    Which =?= all :
      Content ! Condensed? |
	type_of_choice,
	condense_message,
	show_channel_content;

    otherwise :
      Id = _,
      Message = _,
      Choice = _ |
	show_channel_content.

  type_of_choice(Choice, Type) :-

    we(Choice) :
      Type = active;

    ro(Choice) :
      Type = suspended;

    not_we(Choice), Choice =?= 0 :
      Type = withdrawn;

    not_we(Choice), Choice =\= 0 :
      Type = consumed.

condense_message(Type, Id, Message, Which, Depth, Sender, Condensed) :-

    Message =?= [] :
      Which = _,
      Depth = _ |
	id_and_message + (Msg = Type([]));

    tuple(Message),
    arity(Message, Index),
    Index++,
    make_tuple(Index', Msg),
    Depth-- |
	arg(1, Msg, Type),
	id_and_message,
	show_message_channels;

    otherwise :
      Which = _,
      Depth = _,
      Sender = _,
      Message = _ |
	id_and_message + (Msg  = Type("non-message")/*(Message)*/).

  id_and_message(Sender, Id, Msg, Condensed) :-

    Sender =?= sender :
      Condensed = Id(Msg);

    otherwise :
      Sender = _,
      Id = _,
      Condensed = Msg.


show_message_channels(Message, Which, Depth, Sender, Index, Msg) :-

    Index > 1,
    arg(Index, Msg, Display),
    Index--,
    arg(Index', Message, Argument) :
      Tuple = false |
	show_argument,
	self;

    Index =?= 1 :
      Message = _,
      Which = _,
      Depth = _,
      Sender = _,
      Msg = _.


show_argument(Argument, Which, Depth, Sender, Tuple, Display) :-

    Argument = Name(Vector, Stream), string(Name), vector(Vector),
    Depth =\= 0,
    unknown(Stream) :
      Which = _,
      Sender = _,
      Tuple = _,
      Display = Name;

    Argument = Name(Vector, Stream), string(Name), vector(Vector),
    Depth =\= 0,
    Which =\= none,
    known(Stream) :
      Tuple = _,
      Display = (Name = Content) |
	show_channel_content;

    Argument = Name(Vector, Stream), string(Name), vector(Vector),
    Depth =\= 0,
    Which =?= none,
    known(Stream) :
      Sender = _,
      Tuple = _,
      Display = (Name!);

    Argument = Name(Vector, _Stream), string(Name), vector(Vector),
    Depth =?= 0 :
      Which = _,
      Sender = _,
      Tuple = _,
      Display = Name;

    constant(Argument) :
      Which = _,
      Depth = _,
      Sender = _,
      Tuple = _,
      Display = Argument;

    tuple(Argument),
    Tuple = true,
    arity(Argument, Index),
    Index++,
    make_tuple(Index', Msg) :
      Depth = _,
      Depth' = 0,
      Message = Argument,
      Display = Msg |
	arg(1, Msg, message),
	show_message_channels;

    ro(Argument) :
      Which = _,
      Depth = _,
      Sender = _,
      Tuple = _,
      Display = "_?";

    we(Argument) :
      Which = _,
      Depth = _,
      Sender = _,
      Tuple = _,
      Display = "_";

    otherwise :
      Argument = _,
      Which = _,
      Depth = _,
      Sender = _,
      Tuple = _,
      Display = other/*(Argument)*/.


show_goal(Goal, Options, PiFcp) :-

    string(Goal) :
      Options = _,
      PiFcp = Goal;

    tuple(Goal), Goal =\= (_#_),
    arg(1, Goal, Name), string(Name),
    arity(Goal, Index),
    make_tuple(Index, Result) :
      PiFcp = Result |
	arg(1, Result, Name),
	parse_options(Options, Depth(1), order(order),
		      Sender(no_sender), Which(active)),
	goal_channels;

    Goal =?= (Service#Goal') :
      PiFcp = (Service#PiFcp') |
	self;

    otherwise :
      Goal = _,
      Options = _,
      PiFcp = "~PiFcp".

  goal_channels(Goal, Index, Which, Depth, Sender, PiFcp) :-

    Index =?= 3,
    arg(1, Goal, pi_send),
    arg(Index, Goal, Argument),
    arg(Index, PiFcp, Display),
    Index-- :
      Tuple = true |
	show_argument,
	self;

    Index =?= 3,
    arg(1, Goal, pi_receive),
    arg(Index, Goal, Argument),
    arg(Index, PiFcp, Display),
    Index-- :
      Tuple = true |
	show_argument,
	self;

    Index > 1,
    otherwise,
    arg(Index, Goal, Argument),
    arg(Index, PiFcp, Display),
    Index-- :
      Tuple = false |
	show_argument,
	self;

    Index = 1 :
      Goal = _,
      Which = _,
      Depth = _,
      Sender = _,
      PiFcp = _.


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
		      Sender(no_sender), Which(active)),
	nodes([Tree], [Depth, Sender, Which], 0, 0, Head, []),
	display_tree(Head, Order, TreeTrace).

nodes(Nodes, Options, Level, Time, Head, Tail) :-

    Nodes ? tree(TreeId, Context, BranchList) :
      Level = _,
      Head ! begin(PiTreeId?, Context, _Link) |
	show_goal(TreeId, [0], PiTreeId),
	nodes(BranchList, Options, 0, Time,
		Head', [end(PiTreeId?) | Head'']),
	nodes;

    Nodes ? RPC, RPC = _#_ :
      Head ! {'#', RPC, Level, Time} |
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

    Sorted ? {Operator, Goal, _, _}, Operator =\= "|" :
      TreeTrace ! (Operator, "", Goal) |
	self;

    Sorted ? {"|", Goal, _, _} :
      TreeTrace ! Goal |
	self;

    Sorted =?= [] :
      TreeTrace = [].


display_prefix_order(Head, Level, Indent, TreeTrace) :-

    Head ? begin(PiTreeId, _Context, _Link) :
      Level = _,
      Indent = _,
      Level' = 0,
      Indent' = "",
      TreeTrace ! (begin : PiTreeId) |
	self;

    Head ? end(PiTreeId) :
      TreeTrace ! (end : PiTreeId) |
	self;

    Head ? Operator(Goal, Level', _Time) :
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
