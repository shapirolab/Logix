-language([evaluate,compound,colon]).
-mode(trust).
-export([make_channel/1, make_channel/2, make_channel/3,
	 "SPC"/1, "SPC"/2,
	 parse_options/5,
	 show_channel/3, show_goal/3, show_goals/3,
	 show_resolvent/3, show_value/3,
	 show_tree/3, close_tree/1,
	 dimer/3, dimer/4, dimer/5, dimer/6,
	 receive/2, receive/3, receive/4, receive/5,
	 send/2, send/3, send/4, send/5]).

-include(psi_constants).


make_channel(Channel) :-
	make_channel(Channel, psi, infinite).
make_channel(Channel, Creator) :-
    string(Creator) |
	make_channel(Channel, Creator, infinite).
make_channel(Channel, BaseRate) :-
	make_channel(Channel, psi, BaseRate).
make_channel(Channel, Creator, BaseRate) :-
    we(Channel) :
      Channel = Channel'? |
	psi_monitor#new_channel(Creator, Channel', BaseRate);

    string(Channel), Channel =\= "_", Channel =\= "" |
	computation#dictionary(add, Channel, Ch?, Reply),
	made_channel(Channel, Creator, BaseRate, Ch, Reply);

    otherwise :
      Creator = _,
      BaseRate = _ |
	computation#display(("Can't make_channel" : Channel)).

  made_channel(Name, Creator, BaseRate, Channel, Reply) :-
    Reply = new,
    string_to_dlist(Creator, CL, CT),
    string_to_dlist(Name, Cl, []) :
      ascii('.', Dot),
      CT = [Dot | Cl] |
	list_to_string(CL, Creator'),
	psi_monitor#new_channel(Creator'?, Channel, BaseRate);

    otherwise :
      Creator = _,
      BaseRate = _,
      Channel = _ |
	screen#display(("psi_utils: Can't make_channel" : Name - Reply)).


send(Message, Channel) :-
    send(Message, Channel, 1, _, sender).
send(Message, Channel, Multiplier) :-
    send(Message, Channel, Multiplier, _, sender).
send(Message, Channel, Multiplier, Chosen) :-
	send(Message, Channel, Multiplier, Chosen, sender).
send(Message, Channel, Multiplier, Chosen, Name) :-
    vector(Channel),
    arity(Channel, 11),
    we(Chosen) :
      Send = PSI_SEND(Name, Channel, Multiplier, 1) |
	psi_monitor#scheduler(S),
	write_channel(start(send, [Send], Value, Chosen), S),
	transmitted(sending(Name), 1, Chosen, Message, Value);

    string(Channel), Channel =\= "_", Channel =\= "" :
      Name = _ |
	computation#dictionary(find, Channel, Ch, Reply),
	send_message(Message, Channel, Multiplier, Chosen, Ch, Reply);

    otherwise :
      Message = _,
      Multiplier = _ |
	unify_without_failure(Chosen, 0),
	computation#display(("psi_utils: Can't send to" : Name(Channel))).

  send_message(Message, Name, Multiplier, Chosen, Channel, Reply) :-
    Reply = true |
	send(Message, Channel, Multiplier, Chosen, Name);

    otherwise :
      Message = _,
      Multiplier = _, 
      Channel = _ |
	unify_without_failure(Chosen, 0),
	computation#display(("psi_utils: Can't send to" : Name - Reply)).

transmitted(Id, Tag, Chosen, Message, Value) :-

    Chosen = Tag :
      Id = _,
      Value = Message;

    Chosen =\= Tag :
      Message = _,
      Id = _,
      Value = _.
      
 
receive(Channel, Message) :-
	receive(Channel, Message, 1, _, receiver).
receive(Channel, Message, Multiplier) :-
	receive(Channel, Message, Multiplier, _, receiver).
receive(Channel, Message, Multiplier, Chosen) :-
	receive(Channel, Message, Multiplier, Chosen, receiver).
receive(Channel, Message, Multiplier, Chosen, Name) :-
    string(Channel), Channel =\= "_", Channel =\= "" :
      Name = _ |
	computation#dictionary(find, Channel, Ch, Reply),
	receive_message(Channel, Message, Multiplier, Chosen, Ch, Reply);
    vector(Channel),
    arity(Channel, 11),
    we(Chosen) :
      Receive = PSI_RECEIVE(Name, Channel, Multiplier, 2) |
	psi_monitor#scheduler(S),
	write_channel(start(receive, [Receive], Value, Chosen), S),
	transmitted(receiving(Name), 2, Chosen, Message, Value);

    otherwise :
      Message = _,
      Multiplier = _ |
	unify_without_failure(Chosen, 0),
	computation#display(("psi_utils: Can't receive from" : Name(Channel))).
	

  receive_message(Name, Message, Multiplier, Chosen, Channel, Reply) :-
    Reply = true |
	receive(Channel, Message, Multiplier, Chosen, Name);

    otherwise :
      Message = _,
      Multiplier = _,
      Channel = _ |
	unify_without_failure(Chosen, 0),
	computation#display(
		("psi_utils: Can't receive from" : Name - Reply)).


dimer(SendMessage, ReceiveMessage, Channel) :-
	dimer(SendMessage, ReceiveMessage, Channel, 1, _, dimer).
dimer(SendMessage, ReceiveMessage, Multiplier, Channel) :-
	dimer(SendMessage, ReceiveMessage, Channel, Multiplier, _, dimer).
dimer(SendMessage, ReceiveMessage, Multiplier, Channel, Chosen) :-
	dimer(SendMessage, ReceiveMessage, Channel, Multiplier, Chosen, dimer).
dimer(SendMessage, ReceiveMessage, Channel, Multiplier, Chosen, Name) :-
    vector(Channel),
    arity(Channel, 11) :
      Chosen = Chosen'?,
      Dimer = PSI_DIMER(Name, Channel, {1, 2}, Multiplier) |
	psi_monitor#scheduler(S),
	write_channel(start(dimer, [Dimer], Value, Chosen'), S),
	dimered(Chosen'?, SendMessage, ReceiveMessage, Value);

    string(Channel), Channel =\= "_", Channel =\= "" :
      Name = _ |
	computation#dictionary(find, Channel, Ch, Reply),
	dimer_message(SendMessage, ReceiveMessage, Multiplier,
			Channel, Chosen, Ch, Reply);

    otherwise :
      SendMessage = _,
      ReceiveMessage= _,
      Multiplier = _ |
	unify_without_failure(Chosen, 0),
	computation#display(("psi_utils: Can't dimer on" : Name(Channel))).

  dimer_message(SendMessage, ReceiveMessage, Name, Multiplier,
			Chosen, Channel, Reply) :-
    Reply = true |
	dimer(SendMessage, ReceiveMessage, Channel, Multiplier, Chosen, Name);

    otherwise :
      SendMessage = _,
      ReceiveMessage = _,
      Multiplier = _,
      Channel = _ |
	unify_without_failure(Chosen, 0),
	computation#display(("psi_utils: Can't dimer on" : Name - Reply)).

dimered(Chosen, SendMessage, ReceiveMessage, Value) :-

    Chosen = 1 :
      ReceiveMessage = _,
      Value = SendMessage;

    Chosen = 2 :
      SendMessage = _,
      Value = ReceiveMessage;

    Chosen =\= 1, Chosen =\= 2 :
      SendMessage = _,
      ReceiveMessage = _,
      Value = _.


"SPC"(Channel) :-
	psi_monitor#options(Options, Options),
	"SPC"(Channel, Options?).

"SPC"(PsiChannel, Options) :-
	computation#display(term, Display, known(Display)),
	show_channel.

show_channel(PsiChannel, Options, Display) :-

    string(PsiChannel), PsiChannel =\= "_", PsiChannel =\= "" |
	computation#dictionary(find, PsiChannel, PsiChannel', Reply),
	show_psi_channel;

    otherwise |
	show_psi_channel + (Reply = true).


show_psi_channel(PsiChannel, Options, Display, Reply) :-
	
    Reply =\= true :
      Options = _,
      Display = "not_channel" |
	computation#display(("psi_utils: Can't show channel" :
				PsiChannel-Reply));

    Reply =?= true,
    vector(PsiChannel),
    read_vector(PSI_CHANNEL_NAME, PsiChannel, Name) |
	show_psi_channel1;

    otherwise :
      Options = _,
      Reply = _,
      Display = "not_channel" |
	computation#display(("psi_utils: Not a Psi channel" : PsiChannel)).

  show_psi_channel1(Name, PsiChannel, Options, Display) :-

    read_vector(PSI_CHANNEL_REFS, PsiChannel, Refs),
    Refs > 0 :
      make_channel(BadOption, _) |
	parse_options(Options, Depth(1), BadOption(BadOption),
		      Sender(no_sender), Which(active)),
	inspect_channel(PsiChannel, Result),
	channel_argument + (Display = Content, Status = [Result],
		Left = Display, Right = (Name : Content));

    /* closed channel */
    otherwise :
      PsiChannel = _,
      Options = _,
      Display = [Name].


inspect_channel(PsiChannel, Status) :-

    read_vector(PSI_CHANNEL_TYPE, PsiChannel, Type),
    read_vector(PSI_CHANNEL_RATE, PsiChannel, Rate),
    read_vector(PSI_SEND_ANCHOR, PsiChannel, SendAnchor),
    read_vector(PSI_RECEIVE_ANCHOR, PsiChannel, ReceiveAnchor),
    read_vector(PSI_CHANNEL_NAME, PsiChannel, Name) :
      Status = {Kind?, NameRate?, Sends?} |
	name_rate_to_namerate,
	list_messages(SendAnchor, Sends, Receives?),
	list_messages(ReceiveAnchor, Receives, []).

  name_rate_to_namerate(Type, Name, Rate, Kind, NameRate) :-

    Type =?= PSI_CHANNEL_ANCHOR :
      Rate = _,
      Kind = anchor,
      NameRate = Name(anchor);

    Type =?= PSI_UNKNOWN :
      Rate = _,
      Kind = unknown,
      NameRate = Name(unknown);

    Type =?= PSI_BIMOLECULAR :
      Kind = bimolecular,
      NameRate = Name(Rate);

    Type =?= PSI_HOMODIMERIZED :
      Kind = homodimerized,
      NameRate = Name(Rate);

    Type =?= PSI_INSTANTANEOUS :
      Rate = _,
      Kind = instantaneous,
      NameRate = Name(infinite);

    Type =?= PSI_SINK :
      Rate = _,
      Kind = sink,
      NameRate = Name(sink);

    otherwise :
      Type = _,
      Kind = other(Type),
      NameRate = other(Name(Rate)).

  list_messages(Anchor, List, Tail) + (Message = Anchor) :-

    arg(PSI_MESSAGE_LINKS, Message, Links),
    read_vector(PSI_NEXT_MS, Links, Message'),
    Message' =\= Anchor,
    Message' = {Type, CId, _Channel, Multiplier, _SendTag, _ReceiveTag,
			Common, _Links},
    Common = {PId, _MsList, _Value, _Chosen} :
      List ! Kind(PId, CId, Multiplier) |
	type_to_kind,
	self;

    otherwise :
      Anchor = _,
      Message = _,
      List = Tail.      

  type_to_kind(Type, Kind) :-

    Type =?= PSI_SEND :
      Kind = send;

    Type =?= PSI_RECEIVE :
      Kind = receive;

    Type =?= PSI_DIMER :
      Kind = dimer;

    otherwise :
      Kind = other(Type).


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

    Which =\= none,
    Stream ? Kind(PId, CId, Multiplier) :
      Content ! Condensed? |
	id_and_message,
	self;

    Which =?= none,
    Stream =\= [] :
      Depth = _,
      Sender = _,
      Content = "!",
      Left = Right.

  id_and_message(Kind, PId, CId, Multiplier, Sender, Condensed, Left, Right) :-

    Sender =?= sender,
    Multiplier =\= 1 :
      Condensed = {PId, CId*Multiplier, Kind},
      Left = Right;

    Sender =?= sender,
    Multiplier =?= 1 :
      Condensed = {PId, CId, Kind},
      Left = Right;

    otherwise :
      CId = _,
      Multiplier = _,
      PId = _,
      Sender = _,
      Condensed = Kind,
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

    vector(Argument), arity(Argument, CHANNEL_SIZE),
    read_vector(PSI_CHANNEL_NAME, Argument, Name) |
	show_channel_argument + (PsiChannel = Argument);

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

    otherwise |
	show_compound.

show_channel_argument(Name, PsiChannel, Which, Depth, Sender, Display,
				Left, Right) :-

    read_vector(PSI_CHANNEL_REFS, PsiChannel, Refs),
    Refs > 0 :
      Name = _ |
	inspect_channel(PsiChannel, Result),
	channel_argument + (Status = [Result]);

    otherwise :
      PsiChannel = _,
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
	show_goal(Goal, Options, PsiFcp, Output, PsiFcp?).

show_goal(Goal, Options, PsiFcp, Left, Right) :-

    string(Goal) :
      Options = _,
      PsiFcp = Goal,
      Left = Right;

    tuple(Goal), Goal =\= (_#_),
    arg(1, Goal, Name), string(Name),
    arity(Goal, Index) :
      make_channel(BadOption, _) |
	parse_options(Options, Depth(1), BadOption(BadOption),
		      Sender(no_sender), Which(active)),
	show_goal2;

    Goal =?= (Service#Goal') :
      PsiFcp = (Service#PsiFcp') |
	self;

    otherwise :
      Goal = _,
      Options = _,
      PsiFcp = non_goal(Goal),
      Left = Right.

show_goal1(Goal, Which, Depth, Sender, PsiFcp, Left, Right) :-

    string(Goal) :
      Which = _,
      Depth = _,
      Sender = _,
      PsiFcp = Goal,
      Left = Right;

    tuple(Goal), Goal =\= (_#_),
    Goal =\= clause(_, _), Goal =\= clause(_, _, _, _),
    arg(1, Goal, Name), string(Name),
    arity(Goal, Index) |
	show_goal2;

    Goal =?= clause(Goal', _Body) |
	self;

    Goal =?= clause(Goal', _Body, _Id, _Time) |
	self;

    Goal =?= (Service#Goal') :
      PsiFcp = (Service#PsiFcp') |
	self;

    otherwise :
      Goal = _,
      Which = _,
      Depth = _,
      Sender = _,
      PsiFcp = non_goal(Goal),
      Left = Right.

  show_goal2(Goal, Index, Which, Depth, Sender, PsiFcp, Left, Right, Name) :-

    nth_char(1, Name, Char1),
    ascii('A') =< Char1, Char1 =< ascii('Z') |
	goal_channels;

    nth_char(1, Name, Char1),
    Char1 =:= ascii('.'),
    nth_char(2, Name, Char2),
    ascii('A') =< Char2, Char2 =< ascii('Z') |
	goal_channels;

    nth_char(1, Name, Char1),
    ascii('a') =< Char1, Char1 =< ascii('z') |
	show_goal3 + (X = 2);

    otherwise,
    make_tuple(Index, Tuple),
    arg(1, Tuple, N) :
      N = Name,
      PsiFcp = Tuple |
	goal_channels1.

  % Blocked code may begin with a lower-case directory name.
  % Check the first character after '$'.

  show_goal3(Goal, Index, Which, Depth, Sender, PsiFcp, Left, Right, Name, X) :-

    X++,
    nth_char(X, Name, Char1),
    Char1 =:= ascii('$'),
    nth_char(X', Name, Char2),
    ascii('A') =< Char2, Char2 =< ascii('Z') |
	goal_channels;

    X++,
    nth_char(X, Name, Char1),
    Char1 =:= ascii('$'),
    nth_char(X', Name, Char2),
    Char2 =:= ascii('.') |
	goal_channels;

    X++,
    otherwise,
    X' < string_length(Name) |
	self;

    X++,
    otherwise,
    X' >= string_length(Name),
    make_tuple(Index, Tuple),
    arg(1, Tuple, N) :
      N = Name,
      PsiFcp = Tuple |
	goal_channels1.


goal_channels(Goal, Index, Which, Depth, Sender, PsiFcp, Left, Right) :-

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
    vector(Argument), arity(Argument, CHANNEL_SIZE),
    make_tuple(Index, Tuple),
    arg(1, Goal, Name),
    arg(1, Tuple, N) :
      N = Name,
      PsiFcp = Tuple |
	goal_channels1;

    Index-- > 1,
    otherwise |
	self;

    Index =< 1,
    arg(1, Goal, Name) :
      Which = _,
      Depth = _,
      Sender = _,
      PsiFcp = Name,
      Left = Right.

  goal_channels1(Goal, Index, Which, Depth, Sender, PsiFcp, Left, Right) :-

    Index > 1,
    arg(Index, Goal, Argument),
    arg(Index, PsiFcp, Display),
    Index-- |
	show_argument + (Left = Left, Right = Left'?),
	self;

    Index = 1 :
      Goal = _,
      Which = _,
      Depth = _,
      Sender = _,
      PsiFcp = _,
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
      PiTreeId ! PsiFcp? |
	show_goal(Goal, [0], PsiFcp),
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
      Result = (PsiFcp?, Result') |
	remote_goal([], Goal, Goal', PsiFcp, PsiFcp'),
	show_goal1 + (Left = Left, Right = Left'?),
	self;

    Goals =\= (_, _) |
	remote_goal([], Goals, Goal, Result, PsiFcp),
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

/* These three clauses have been added to prettify resolvent of vtree. */
    List ? _Name([_|_]) |
	self;

    List ? _Name([]) |
	self;

    List ? Name(Goal),
    Name =?= vanilla,
    arg(1, Goal, Functor), nth_char(1, Functor, C),
    ascii('a') =< C, C =< ascii('z') |
	self;
/***********************************************************************/

    List ? Name(Goal),
    otherwise :
      Result ! PsiFcp? |
	remote_goal(Name, Goal, Goal', PsiFcp, PsiFcp'),
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
    Call = call(_, _) :
      List ! Call |   
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

    Goals ? Goal :
      List ! Name(Goal) |
	self;

    Goals =\= [_ | _],
    Goals =\= [] :
      Goals' = [Goals] |
	self;

    Goals =?= [] :
      Name = _,
      List = NextList.


remote_goal(Name, Goal, OutGoal, PsiFcp, OutPsiFcp) :-

    Name =\= [] :
      Name' = [],
      PsiFcp = Name#PsiFcp'? |
	self;

    Name =?= [],
    Goal =?= Target#Goal' :
      PsiFcp = Target#PsiFcp'? |
	self;

    Name =?= [],
    Goal =\= _#_ :
      OutGoal = Goal,
      OutPsiFcp = PsiFcp.
