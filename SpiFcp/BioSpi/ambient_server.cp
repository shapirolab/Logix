-mode(trust).
-language([evaluate,compound,colon]).
-include(spi_constants).
-export([run/1, run/2,
	 run/3]).	/* debugging */

AMBIENT_IDLE_PRIORITY => 110.
SYSTEM_IDLE_PRIORITY => 105.

%DEBUG(ID,ARG) => true.

DEBUG(ID,ARG) => debug_out((AmbientId:ID => ARG),  Debug).

debug_out(Item, Debug) :-
    channel(Debug) :
      write_channel(Item, Debug);
    otherwise :
      Debug = _,
      Item = _.

run(Commands) :-
	run(Commands, _Root, []).
run(Commands, Root) :-
	run(Commands, Root, []).
run(Commands, Root, Debug) :-
   true :
      AmbientId = system,
      SharedChannels = [],
      make_channel(Root, In),
      Parent = Root,
      Children = [Ambient?] |
	spi_monitor # scheduler(Scheduler),
	computation # events(Events),
	computation # self # service_id(SId),
	serve_system + (UniqueId = 0, Status = running),
	ambient + (AmbientName = global).

/*
 * The system ambient is not a nested computation.
 * It is a normal member of its computation (usually
 * a shell computation).
 */
serve_system(AmbientId, In, Events, Children, UniqueId, Status, SharedChannels,
	     Scheduler, Root, Debug) :-

/************************** Debugging Aids *********************************/
    In ? debug(Out),
    we(Out) :
      make_channel(Debug', Out) |
	new_debug,
	close_debug(Debug),
	self;

    In ? debug(Debug'),
    channel(Debug') |
	new_debug,
	close_debug(Debug),
	self;

/**************************************************************************/

    In ? ambient_id(AmbientId^) |
	self;

    In ? done(Child, Reply) |
	DEBUG(done/1, children),
	remove_child(Child, Children, Children'),
	remove_shared_communications(Child, SharedChannels, Scheduler, Reply),
	self;

    In ? lookup(Locus, PrivateChannel, SharedChannel?^),
    Locus =?= self(Kind),
    read_vector(SPI_CHANNEL_NAME, PrivateChannel, Name) |
	DEBUG(lookup/3 - Locus - PrivateChannel -
		SharedChannels - SharedChannels', search),
	lookup(Kind(Name), PrivateChannel, SharedChannel,
	       SharedChannels, SharedChannels', Scheduler, AmbientId, Debug),
	system_lookup;

    In ? Close, Close =?= close(_ChannelTuple) |
	DEBUG(Close, discarded),
	self;

    In ? new_child(Child) :
      Children' = [Child | Children] |
	DEBUG(new_child, added),
	self;

    In ? new_id(Id),
    UniqueId++ :
      Id = UniqueId' |
	self;

    In ? state(State) :
      State = [id(system), children(Children)] |
	self;

    In ? suspend(Reply),
    Status =?= running :
      Reply = Status'? |
	DEBUG(suspend, suspending),
	processor # machine(idle_queue(Done, SYSTEM_IDLE_PRIORITY), _Ok),
	control_children(suspend, Children, Children', Ready, Done?),
	system_suspending(Ready?, SharedChannels, Status'),
	self;

    In ? suspend(Reply),
    Status =\= running :
      Reply = suspend - false(Status) |
	self;

    In ? resume(Reply),
    Status =?= suspended :
      Status' = Reply? |
	DEBUG(resume, running),
	control_children(resume, Children, Children', Ready),
	Ready?(Reply) = true(running),
	self;

    In ? resume(Reply),
    Status =\= suspended |
	Reply = resume - false(Status),
	self;

    In ? resolvent(Resolvent),
    Status =?= running :
      In'' = [suspend(_Reply), resolvent(Resolvent) | In'] |
	self;

    In ? resolvent(Resolvent),
    Status =?= suspended |
	DEBUG(resolvent, resolving),
	resolve_children(Children, Children', Resolvent, []),
	self;

    In ? resolvent(Resolvent),
    Status =\= running, Status =\= suspended :
      Resolvent = ["Can't resolve"(Status)] |
	self;

    In ? Other,
    otherwise |
	fail(ambient(system) ? Other, unknown),
	self;

    Children =?= [],
    unknown(In) :
      AmbientId = _,
      Events  = _,
      Scheduler = _,
      SharedChannels = _,
      Status = _,
      UniqueId = _,
      close_channel(Root) |
	close_debug;

    Events ? aborted :
      Status = _,
      Status' = aborted |
	DEBUG((system: event-aborted), ambient),
	abort_children(Children, Children'),
	self;

    Events ? Other, Other =\= aborted |
	DEBUG((system: event-Other), trash),
	self.

  system_lookup(In, Events, Children, AmbientId, UniqueId, Status,
		SharedChannels, Scheduler, Root, Debug, SharedChannel) :-
    known(SharedChannel) |
	serve_system.

  new_debug(Debug, Children) :-
    Children ? Child :
      write_channel(debug(Debug), Child) |
	self;
    Children = [] :
      Debug = _.

  close_debug(Debug) :-
    channel(Debug) :
      close_channel(Debug);
    otherwise :
      Debug = _.


  system_suspending(Ready, SharedChannels, Status) :-

    known(Ready) |
	remove_all_communications(SharedChannels, Reply),
	Reply?(Status) = true(suspended).

/*
 * Create a nested computation.
 */
ambient(AmbientName, SId, Commands, Parent, Ambient, Debug) :-
    channel(Parent) :
      write_channel(new_id(UniqueId), Parent, Parent'),
      write_channel(ambient_id(ParentId), Parent', Parent''),
      make_channel(SuperChannel, FromSub),
      make_channel(Ambient, In),
      make_channel(ToDomain, DIn),
      Children = [] |
	spi_monitor # scheduler(Scheduler),
	make_ambient_id,
	computation # "_domain"(domain_channel(Domain)),
	computation_server#computation([identifier(AmbientId?, _) | Requests],
					{Controls, Done, done, SuperChannel?},
					ToDomain?,
					Events),
	request_commands(Commands, Requests, Requests'),
	computation # events(RelayEvents),
	watcher + (Done = AmbientDone),
	serve_ambient0.

  make_ambient_id(AmbientName, UniqueId, ParentId, AmbientId, Scheduler) :-
    channel(Scheduler),
    string(AmbientName),
    integer(UniqueId) :
/*
    convert_to_string(UniqueId, UIS) :
    string_to_dlist(UIS, UIL, []),
    string_to_dlist(AmbientName, ANL, [CHAR_MINUS | UIL]),
    list_to_string(ANL, AI) :
      AmbientId = AI,
*/    AmbientId = AmbientName(UniqueId),
      write_channel(debug_note((ParentId->AmbientId)), Scheduler).


request_commands(Commands, Requests, EndRequests) :-

    Commands ? Command |
	request_commands(Command, Requests, Requests'?),
	self;

    Commands =?= [] :
      Requests = EndRequests;

    Commands =\= [_|_], Commands =\= [] :
      Requests = [Commands | EndRequests].
   

watcher(AmbientId, RelayEvents, DIn, Done, SId, Domain, Ambient, Debug) :-

    RelayEvents ? aborted :
      AmbientId = _,
      Debug = _,
      DIn = _,
      Domain = _,
      Done = _,
      RelayEvents' = _,
      SId = _,
      write_channel(abort, Ambient) |
	DEBUG(relay-aborted, abort);

    RelayEvents ? Other, Other =\= aborted |
	DEBUG(relay-Other, trash),
	self;

    Done =?= done :
      Ambient = _,
      AmbientId = _,
      Debug = _,
      DIn = _,
      Domain = _,
      RelayEvents = _,
      SId = _ |
	DEBUG(done, quit - watcher);

    DIn ? Command |
	domain_command.

  domain_command(AmbientId, RelayEvents,DIn, Done, SId, Domain, Ambient,
		 Debug, Command) :-

    Command =?= export(CallInfo, _Scope, Goals, UCC),
    channel(Domain),
    ground(SId) :
      write_channel(export(CallInfo, SId, Goals, UCC), Domain) |
	DEBUG(Command, domain(SId)),
	watcher;

    Command =?= reduce(CallInfo, _Scope, Goals, UCC),
    channel(Domain),
    ground(SId) :
      write_channel(reduce(CallInfo, SId, Goals, UCC), Domain) |
	DEBUG(Command, domain(SId)),
	watcher.
    

serve_ambient0(AmbientId, In, Events, FromSub, Done,
	       Ambient, Parent,
	       Children,
	       Requests, Controls, AmbientDone, Scheduler, Debug) :-
	serve_ambient + (GlobalChannels = [{0, _, -1, ""}, {[], _, -1, ""}],
			 LocalChannels = [], SharedChannels = []).

serve_ambient(AmbientId, In, Events, FromSub, Done,
	      Ambient, Parent, Children,
	      LocalChannels, GlobalChannels, SharedChannels,
	      Requests, Controls, AmbientDone,
	      Scheduler, Debug) :-

/* Internal Communication */

    In ? abort :
      Controls' = _ |
	DEBUG(abort, children),
	unify_without_failure(Controls, [abort]),
	unify_without_failure(Done, done),
	abort_children(Children, Children'),
	self;

    In ? ambient_id(AmbientId^) |
	self;

    In ? Close, Close =?= close(ChannelTuple) :
      write_channel(close(ChannelTuple, Reply), Scheduler) |
	DEBUG(close - Reply - ChannelTuple - LocalChannels - SharedChannels,
		scheduler),
	remove_channels(ChannelTuple, Reply,
			LocalChannels, LocalChannels',
			SharedChannels, SharedChannels', Unremoved),
	pass_unremoved;

    In ? done(Child, Reply) |
	DEBUG(done/1, children),
	remove_child(Child, Children, Children'),
	remove_shared_communications(Child, SharedChannels, Scheduler, Reply),
	self;

    In ? NewId, NewId =?= new_id(_Id) :
      write_channel(NewId, Parent) |
	DEBUG(NewId, pass),
	self;

/* Procedure Services */

    In ? global_channels(List) |
	DEBUG(global/1, scheduler),
	merge_global_channels(List, GlobalChannels, GlobalChannels',
			      Scheduler),
	self;

    In ? global_channels(List, Ambient^) |
	DEBUG(global/2, scheduler),
	merge_global_channels(List, GlobalChannels, GlobalChannels',
			      Scheduler),
	self;

    In ? lookup(Locus, PrivateChannel, SharedChannel?^),
    Locus =?= "local",
    read_vector(SPI_CHANNEL_NAME, PrivateChannel, Name) |
	DEBUG(lookup/3 - "local" - PrivateChannel -
		LocalChannels - LocalChannels', search),
	lookup(Name, PrivateChannel, SharedChannel,
	       LocalChannels, LocalChannels', Scheduler, AmbientId, Debug),
	ambient_lookup;

    In ? lookup(Locus, PrivateChannel, SharedChannel?^),
    Locus =?= global,
    read_vector(SPI_CHANNEL_NAME, PrivateChannel, Id),
    string(Id),
    string_length(Id) > 7,
    string_to_dlist(Id, IdL, []),
    string_to_dlist("global.", Prefix, NL) :
      IdL = Prefix |
	DEBUG(lookup/3 - global - PrivateChannel -
		GlobalChannels - GlobalChannels', search),
	list_to_string(NL, Name),
	lookup(Name, PrivateChannel, SharedChannel,
	       GlobalChannels, GlobalChannels', Scheduler, AmbientId, Debug),
	ambient_lookup;

    In ? lookup(Locus, PrivateChannel, SharedChannel?^),
    Locus =?= self(Kind),
    read_vector(SPI_CHANNEL_NAME, PrivateChannel, Name) |
	DEBUG(lookup/3 - Locus - PrivateChannel -
		SharedChannels - SharedChannels', search),
	lookup(Kind(Name), PrivateChannel, SharedChannel,
	       SharedChannels, SharedChannels', Scheduler, AmbientId, Debug),
	ambient_lookup;

    In ? lookup(Locus, PrivateChannel, SharedChannel?^),
    Locus =?= parent(Kind) :
      write_channel(lookup(self(Kind), PrivateChannel, SharedChannel), Parent) |
	DEBUG(lookup/3 - Locus - PrivateChannel, pass),
	ambient_lookup;

    In ? new_ambient(Name, ModuleId, Goal, NewAmbient?^),
    ModuleId = [ModuleName | SId] :
      Children' = [NewAmbient? | Children] |
	DEBUG(new_ambient, ambient),
	processor # machine(idle_queue(Idle, AMBIENT_IDLE_PRIORITY), _Ok),
	merge_local_channels(Idle, Goal, NewGoal, Ambient, NewAmbient),
	ambient(Name, SId, ModuleName # NewGoal?,
		Ambient, NewAmbient, Debug),
	self;

    In ? New, New = new_channel(_Creator, Channel, _BaseRate) :
      write_channel(New, Scheduler) |
	DEBUG(new_channel/3, scheduler),
	add_local_channel(Channel, LocalChannels, LocalChannels'),
	self;

    In ? New, New = new_channel(_Creator, Channel, _ComputeWeight,
					_BaseRate) :
      write_channel(New, Scheduler) |
	DEBUG(new_channel/4, scheduler),
	add_local_channel(Channel, LocalChannels, LocalChannels'),
	self;

    In ? new_locals(Locals, NewLocals) |
	DEBUG(new_locals, ambient),
	processor # machine(idle_queue(Idle, AMBIENT_IDLE_PRIORITY), _Ok),
	merge_local_channels(Idle, Locals, NewLocals, Ambient, Ambient),
	self;

    In ? Start, Start =?= start(Signature, Operations, Message, Chosen),
    AmbientId =?= _AmbientName(UniqueId) :
      Start' = start(Signature, Operations, Message, Chosen, UniqueId),
      write_channel(Start', Scheduler) |
	DEBUG(Start, scheduler),
	self;

/* Capability Initialization */

    In ? delegate(Ambient^, Ready) :
      Ready = _ |
	DEBUG(delegate, Ready),
	self;

    In ? enter(Enterer, Ready) :
      write_channel(change_parent(Ambient, Removed, Ready), Enterer) |
	DEBUG("enter"(Removed, Ready), move_ambient),
	remove_shared_communications(Enterer, SharedChannels, Scheduler,
					Removed),
	self;

    In ? exit(Exiter, Ready) :
      write_channel(change_parent(Parent, Removed, Ready), Exiter) |
	DEBUG("exit"(Removed, Ready), move_ambient),
	remove_shared_communications(Exiter, SharedChannels, Scheduler,
					Removed),
	self;

    In ? merge(MergingAmbient, Ready),
    channel(MergingAmbient) :
      write_channel(extract(Goals, Ambient, Ready), MergingAmbient) |
	DEBUG(merge/2, merge(Goals)),
	processor # machine(idle_queue(Idle, AMBIENT_IDLE_PRIORITY), _Ok),
	merge_local_channels(Idle, Goals, MergedGoals,
				MergingAmbient, Ambient),
	add_merged_goals(MergedGoals?, Requests, Requests', Ready),
	self;

/* Capability Services */

    In ? change_parent(Parent', false, Ready) :
      write_channel(done(Ambient, Ready), Parent),
      write_channel(new_child(Ambient), Parent'),
      Ready = true |
	DEBUG(change_parent, "no_remove"),
	self;

    In ? change_parent(Parent', true, Ready) :
      write_channel(done(Ambient, Ready), Parent),
      write_channel(new_child(Ambient), Parent'),
      Controls ! suspend |
	DEBUG(change_parent, "remove p2c & all local communications"),
	remove_shared_communications(Ambient, SharedChannels, Scheduler,
					Reply1),
	copy_global_channels,
	remove_all_communications(Channels?, Reply2),
	remove_all_communications(LocalChannels, Reply3),
	resume_controls_when_ready(Reply1, Reply2, Reply3, Ready,
					Controls, Controls'),
	self;

    In ? extract(Goals, MergedAmbient, Ready) |
	DEBUG(suspend/2, suspend-extract-send-resume),
	copy_global_channels,
	detach_channels(Channels?, Scheduler, Children, Children'),
	detach_channels(LocalChannels, Scheduler, Children', Children''),
	detach_channels(SharedChannels, Scheduler, Children'', Children'''),
	children_to_merged_ambient;

    In ? new_child(Child) :
      Children' = [Child | Children] |
	self;

/* Control Services */

    In ? suspend(Ready) :
      Controls ! suspend |
	control_children(suspend, Children, Children', ReadyChildren),
	ambient_suspending,
	self;

    In ? resolve(Resolvent, NextResolvent) :
      Controls ! request(state(R)),
      Resolvent ! AmbientId |
	copy_resolvent(R, Resolvent', Resolvent''),
	resolve_children(Children, Children', Resolvent'', NextResolvent),
	self;

    In ? resume(Ready) :
      Controls ! resume |
	control_children(resume, Children, Children', Ready),
	self;

/************************** Debugging Aids *********************************/

   In ? ambient_channel(Ambient?^) |
	self;

    In ? debug(Out),
    we(Out) :
      make_channel(Debug', Out) |
	new_debug,
	close_debug(Debug),
	self;

    In ? debug(Debug'),
    channel(Debug') |
	new_debug,
	close_debug(Debug),
	self;

    In ? state(State) :
      State = [id(AmbientId), self(Ambient), parent(Parent),
		children(Children), private(LocalChannels),
		global(Channels?), shared(SharedChannels),
		debug(Debug)] |
	copy_global_channels,
	self;
/***************************************************************************/

    In ? Other,
    otherwise |
	DEBUG((other = Other), fail),
	/* other(Other) = In'?, */
	fail(ambient(AmbientId) ? Other, unknown),
	self;

    Events ? Global,
    Global =?= event(global_channels(List)) |
	DEBUG(delegated-Global, scheduler),
	merge_global_channels(List, GlobalChannels, GlobalChannels',
			      Scheduler),
	self;

    Events ? Global,
    Global =?= event(global_channels(List, AmbientChannel)) |
	DEBUG(delegated-Global, scheduler),
	unify_without_failure(AmbientChannel, Ambient),
	merge_global_channels(List, GlobalChannels, GlobalChannels',
			      Scheduler),
	self;

    Events ? Event,
    Event =\= event(global_channels(_)), Event =\= event(global(_, _)) |
	serve_event;

    FromSub ? delegated([], CCC),
    CCC = {_, Left, Right, _} :
      Left = Right |
	DEBUG(delegated-[], trash),
	self;

    FromSub ? delegated(Message, CCC),
    Message =\= [],
    CCC = {_, Left, Right, _} :
      Left = Right |
	DEBUG((delegated-message = Message), computation),
	computation # Message,
	self;

    FromSub ? request(From, Event, Latch, Latch^) :
      Event = _,
      From = _ |
	DEBUG((request-from(From) = Event), trash),
	self;

    Done =?= done, Children =?= [] :
      AmbientId = _,
      Debug = _,
      Events = _,
      FromSub = _,
      In = _,
      Scheduler = _,
      Controls = [],
      Requests = [],
      AmbientDone = done |
	DEBUG(closed, quit),
	detach_channels(SharedChannels, Scheduler),
	detach_channels(LocalChannels, Scheduler),
	copy_global_channels,
	detach_channels(Channels, Scheduler),
	ambient_done.

  ambient_done(Ambient, Parent) :-

    true :
      write_channel(done(Ambient, _Reply), Parent);

    otherwise :
      Parent = _,
      close_channel(Ambient).


  add_merged_goals(Goals, Requests, NewRequests, Ready) :-

    Goals ? Goal :
      Requests ! Goal |
	self;

    Goals =?= [] :
      NewRequests = Requests,
      Ready = true.

  ambient_lookup(In, Events, FromSub, Done,
		 AmbientId, Ambient, Parent, Children,
		 LocalChannels, GlobalChannels, SharedChannels,
		 Requests, Controls, AmbientDone,
		 Scheduler, Debug,
		 SharedChannel) :-
    known(SharedChannel) |
	serve_ambient.

  copy_resolvent(R, Resolvent, NextResolvent) :-

    R ? Term :
      Resolvent ! Term |
	self;

    R =?= [] :
     Resolvent = NextResolvent.

  pass_unremoved(AmbientId, In, Events, FromSub, Done,
	      Ambient, Parent,
	      Children, LocalChannels, GlobalChannels, SharedChannels,
	      Requests, Controls, AmbientDone,
	      Scheduler, Debug,
	      Unremoved) :-

    Unremoved =?= [] |
	serve_ambient;

    Unremoved =?= [Channel] :
      write_channel(close({Channel}), Parent) |
	serve_ambient;

    Unremoved =?= [Channel1, Channel2] :
      write_channel(close({Channel1, Channel2}), Parent) |
	serve_ambient;

    Unremoved =\= [], Unremoved =\= [_], Unremoved =\= [_, _] :
      write_channel(close(ChannelTuple?), Parent) |
	utils#list_to_tuple(Unremoved, ChannelTuple),
	serve_ambient.

  remove_child(Ambient, Children, NewChildren) :-

    Children ? Ambient :
      NewChildren = Children';

    Children ? Child,
    Child =\= Ambient :
      NewChildren ! Child |
	self;

    Children =?= [] :
      Ambient = _,
      NewChildren = [].

  abort_children(Children, Reply) :-
    Children ? Child :
      write_channel(abort, Child) |
	self;
    Children ? _Child,
    otherwise |
	self;
    Children =?= [] |
      Reply = [].

  serve_event(In, Events, FromSub, Done,
	      AmbientId, Ambient, Parent, Children,
	      GlobalChannels, LocalChannels, SharedChannels,
	      Requests, Controls, AmbientDone,
	      Scheduler, Debug,
	      Event) :-

    Event =?= terminated |
	DEBUG(event-terminated, done),
	unify_without_failure(Done, done),
	serve_ambient;

    Event =?= aborted :
      Controls = [],
      Requests = [],
      close_channel(Ambient) | 
	DEBUG(event-aborted, done),
	unify_without_failure(Done, done),
	abort_children(Children, Children'),
	serve_ambient;
	
    Event = failed(Goal, Reason) |
	computation # failed(AmbientId@Goal, Reason),
	serve_ambient;
		
    Event =?= comment(Comment) |
	computation # comment(AmbientId@Comment),
	serve_ambient;

    Event =?= diagnostic(Diagnostic) |
	computation # diagnostic(AmbientId@Diagnostic),
	serve_ambient;

    otherwise :
      Event = _ |
	DEBUG(event-Event, trash),
	serve_ambient.

  children_to_merged_ambient(In, Events, FromSub, Done,
			     AmbientId, Ambient, Parent, Children,
			     SharedChannels,
			     Requests, Controls, AmbientDone,
			     Scheduler, Debug,
			     MergedAmbient, Goals, Ready) :-

    Children ? Child :
      write_channel(change_parent(MergedAmbient, Removed, _Ready), Ambient) |
	DEBUG("exit"(Removed, Ready), move_ambient),
	remove_shared_communications(Child, SharedChannels, Scheduler,
					Removed),
	self;

    Children =?= [] :
      MergedAmbient = _,
      SharedChannels = _,
      Controls ! suspend,
      Controls' ! request(extract(all, Goals)) |
	resume_ambient_when_ready.


lookup(Id, PrivateChannel, SharedChannel, ChannelList, NewChannelList,
	 Scheduler, AmbientId, Debug) + (Last = "") :-

    ChannelList =?= [Channel | _],
    vector(Channel),
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Refs++ > 0,
    read_vector(SPI_CHANNEL_NAME, Channel, Id) :
      AmbientId = _,
      Debug = _,
      Last = _,
      Scheduler = _,
      store_vector(SPI_CHANNEL_REFS, Refs', Channel) |
	DEBUG(lookup, found(Refs) - Id - Channel),
	verify_shared_channel;

    ChannelList ? Channel,
    vector(Channel),
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Refs =< 0 |
	DEBUG(lookup, deleted(Refs) - Id - Channel),
	self;

    ChannelList ? Channel,
    vector(Channel),
    read_vector(SPI_CHANNEL_NAME, Channel, OtherId),
    OtherId =\= Id :
      NewChannelList ! Channel |
%	DEBUG(lookup, mismatch - Id =\= OtherId),
	self;

    ChannelList = [Global | _],
    Global =?= Id(Channel, _ComputeWeight, _BaseRate),
    vector(Channel),
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Refs++ :
      AmbientId = _,
      Debug = _,
      Last = _,
      Scheduler = _,
      store_vector(SPI_CHANNEL_REFS, Refs', Channel) |
	DEBUG(lookup, found(Refs) - Id - Channel),
	verify_shared_channel;

    ChannelList ? Entry,
    Entry = Last'(_, _, _),
    Last' @< Id :
      Last = _,
      NewChannelList ! Entry |
	self;

    ChannelList =?= [Name1(_, _, _) | _],
    Last @< Id, Id @< Name1,
    string_to_dlist("global.", GL, GT),
    string_to_dlist(Id, NL, []),
    read_vector(SPI_CHANNEL_TYPE, PrivateChannel, Type),
    read_vector(SPI_CHANNEL_RATE, PrivateChannel, Rate),
    read_vector(SPI_WEIGHT_TUPLE, PrivateChannel, WeightTuple) :
      AmbientId = _,
      Debug = _,
      NewChannelList =
	[Id(SharedChannel?, SPI_DEFAULT_WEIGHT_NAME, BaseRate?) | ChannelList],
      write_channel(new_channel(GlobalId, New1, Rate), Scheduler),
      GT = NL |
	list_to_string(GL, GlobalId),
	DEBUG(lookup, new - GlobalId - PrivateChannel),
	rate_to_baserate,
	store_vector(SPI_CHANNEL_TYPE, Type, New1, New2),
	store_vector(SPI_CHANNEL_RATE, Rate, New2, New3),
	store_vector(SPI_WEIGHT_TUPLE, WeightTuple, New3, SharedChannel);

    ChannelList =?= [],
    read_vector(SPI_CHANNEL_TYPE, PrivateChannel, Type),
    read_vector(SPI_CHANNEL_RATE, PrivateChannel, Rate),
    read_vector(SPI_WEIGHT_TUPLE, PrivateChannel, WeightTuple) :
      AmbientId = _,
      Debug = _,
      Last = _,
      NewChannelList = [SharedChannel?],
      write_channel(new_channel(Id, New1, BaseRate?), Scheduler) |
	DEBUG(lookup, new - Id - PrivateChannel),
	rate_to_baserate,
	store_vector(SPI_CHANNEL_TYPE, Type, New1, New2),
	store_vector(SPI_CHANNEL_RATE, Rate, New2, New3),
	store_vector(SPI_WEIGHT_TUPLE, WeightTuple, New3, SharedChannel).

  rate_to_baserate(Type, Rate, BaseRate) :-

    Type =?= SPI_INSTANTANEOUS :
      Rate = _,
      BaseRate = infinite;

    Type =\= SPI_INSTANTANEOUS :
      BaseRate = Rate.

  verify_shared_channel(PrivateChannel, Channel, SharedChannel,
			ChannelList, NewChannelList) :-

    read_vector(SPI_CHANNEL_RATE, PrivateChannel, Rate),
    read_vector(SPI_WEIGHT_TUPLE, PrivateChannel, WeightTuple),
    read_vector(SPI_CHANNEL_RATE, Channel, SharedRate),
    read_vector(SPI_WEIGHT_TUPLE, Channel, SharedWeightTuple),
    Rate =?= SharedRate,
    WeightTuple =?= SharedWeightTuple |
	verify_shared_channel_type;

    otherwise,
    read_vector(SPI_CHANNEL_NAME, PrivateChannel, Id) :
      NewChannelList = [Channel | ChannelList],
      SharedChannel = Channel |
	screen#display("shared weight/rate conflict!" - Id).

  verify_shared_channel_type(PrivateChannel, Channel, SharedChannel,
			     ChannelList, NewChannelList) :-

    read_vector(SPI_CHANNEL_TYPE, PrivateChannel, Type),
    read_vector(SPI_CHANNEL_TYPE, Channel, SharedType),
    Type =?= SharedType :
      SharedChannel = Channel,
      NewChannelList = ChannelList;

    read_vector(SPI_CHANNEL_TYPE, PrivateChannel, Type),
    read_vector(SPI_CHANNEL_TYPE, Channel, SharedType),
    Type =?= SPI_UNKNOWN,
    SharedType =\= SPI_UNKNOWN :
      SharedChannel = Channel,
      NewChannelList = ChannelList;

    read_vector(SPI_CHANNEL_TYPE, PrivateChannel, Type),
    read_vector(SPI_CHANNEL_TYPE, Channel, SharedType),
    Type =\= SPI_UNKNOWN,
    SharedType =?= SPI_UNKNOWN :
      store_vector(SPI_CHANNEL_TYPE, Type, Channel),
      SharedChannel = Channel,
      NewChannelList = ChannelList;

    read_vector(SPI_CHANNEL_TYPE, PrivateChannel, Type),
    read_vector(SPI_CHANNEL_TYPE, Channel, SharedType),
    otherwise,
    read_vector(SPI_CHANNEL_NAME, PrivateChannel, Id) :
      SharedChannel = Channel,
      NewChannelList = ChannelList |
	screen#display("shared type conflict!" - Id(Type =\= SharedType)).
    

add_local_channel(Channel, LocalChannels, NewLocalChannels) :-

    vector(Channel),
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    Name =?= _String(_PrivateId) :
      NewLocalChannels = [Channel | LocalChannels];

    otherwise :
      Channel = _,
      NewLocalChannels = LocalChannels.

remove_channels(ChannelTuple, Indices, LocalChannels, NewLocalChannels,
		SharedChannels, NewSharedChannels, Unremoved) :-

    Indices =?= true(Indices') |
	self;

    Indices ? Index,
    arg(Index, ChannelTuple, Channel),
    vector(Channel),
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    Name =?= _String(LocalId),
    number(LocalId) |
	remove_channel(Channel, LocalChannels, LocalChannels',
			UnremovedLocal, []),
	unremoved_local_channel,
	self;

    Indices ? Index,
    arg(Index, ChannelTuple, Channel),
    vector(Channel),
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    string(Name) |
	/* Global Channel - ignore - they're all going at once ? */
	self;

    Indices ? Index,
    arg(Index, ChannelTuple, Channel),
    vector(Channel),
    otherwise |
	remove_channel(Channel, SharedChannels, SharedChannels',
			Unremoved, Unremoved'),
	self;

    Indices ? Index,
    arg(Index, ChannelTuple, Channel),
    vector(Channel),
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    otherwise |
	screen#display("Unrecognized Channel Kind " - Name),
	self;

    Indices =?= [] :
      ChannelTuple = _,
      NewLocalChannels = LocalChannels,
      NewSharedChannels = SharedChannels,
      Unremoved = [].

  remove_channel(Channel, List, NewList, Unremoved, NewUnremoved) :-

    List ? Channel,
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Refs =< 0 :
      NewList = List',
      NewUnremoved = Unremoved;

    List ? Channel,
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Refs > 0 :
      NewList = [Channel | List'],
      NewUnremoved = Unremoved;

    List ? Other, Other =\= Channel :
      NewList ! Other |
	self;

    List =?= [] :
      NewList = [],
      Unremoved = [Channel | NewUnremoved].

  unremoved_local_channel(UnremovedLocal) :-

    UnremovedLocal =\= [_] | true;

    UnremovedLocal =?= [Channel],
    read_vector(SPI_CHANNEL_NAME, Channel, Name) |
	screen#display("Couldn't find local channel" - Name).


detach_channels(ChannelList, Scheduler) + (Go = go, Reply = _) :-

    ChannelList = [] :
      Scheduler = _,
      Go = Reply;

    list(ChannelList) :
      write_channel(close(ChannelTuple?, _Reply), Scheduler) |
%screen#display(close_list_reply = Reply).
	make_tuple(N?, Tuple),
	detach_channel(Reply, ChannelList, Tuple, Go, 0, N, ChannelTuple);

    otherwise : /* Scheduler closed! */
      ChannelList = _,
      Scheduler = _,
      Reply = Go .

  detach_channel(Reply, List, Tuple1, Go, I, N, Tuple2) :-

    List ? Vector,
    vector(Vector),
    I' := I + 1 :
      store_vector(SPI_CHANNEL_REFS, 1, Vector) |
	arg(I', Tuple1, Vector),
	self;

    otherwise : List = _,
      N = I,
      Tuple2 = Tuple1,
      Reply = Go.


merge_local_channels(Idle, Argument, NewArgument, FromAmbient, Ambient) :-
    known(Idle),
    freeze(Argument, FrozenArgument, FrozenAtoms) :
      melt(FrozenArgument, MeltedArgument, MeltedAtoms) |
	merge_channels.

  merge_channels(FrozenAtoms, MeltedAtoms, NewArgument, FromAmbient, Ambient,
		    MeltedArgument) :-
    /* Variables are inherited by the new ambient,
     * not by the receiver of the channel tuple.
     */
    FrozenAtoms ? Variable,
    unknown(Variable) :
      MeltedAtoms ! Variable |
	self;

    /* Communication channels may not have a unique structure -
       The user should not employ vectors of size SPI_CHANNEL_SIZE. */
    FrozenAtoms ? Channel,
    vector(Channel),
    arity(Channel, CHANNEL_SIZE),
    read_vector(SPI_CHANNEL_NAME, Channel, ChannelName),
    channel(Ambient),
    string(ChannelName) :
      write_channel(lookup(global, Channel, NewChannel), Ambient),
      MeltedAtoms ! NewChannel? |
	self;

    FrozenAtoms ? Channel,
    vector(Channel),
    arity(Channel, CHANNEL_SIZE),
    read_vector(SPI_CHANNEL_NAME, Channel, ChannelName),
    channel(Ambient),
    tuple(ChannelName) :
      write_channel(lookup("local", Channel, NewChannel), Ambient),
      MeltedAtoms ! NewChannel? |
	self;

    FrozenAtoms ? FromAmbient :
      MeltedAtoms ! Ambient |
	self;

    FrozenAtoms ? Other,
    otherwise :
      MeltedAtoms ! Other |
	self;

    FrozenAtoms = [] :
      Ambient = _,
      FromAmbient = _,
      MeltedAtoms = [],
      MeltedArgument = NewArgument.

    
merge_global_channels(List, GlobalChannels, NewGlobalChannels, Scheduler) 
			+ (Last = "") :-

    List =?= [] :
      Last = _,
      Scheduler = _,
      NewGlobalChannels = GlobalChannels;

    List ? Name(NewChannel, BaseRate),
    Last @< Name,
    we(NewChannel),
    GlobalChannels ? Global,
    Global = Name(SpiChannel, _ComputeWeight, BaseRate),
    vector(SpiChannel),
    read_vector(SPI_CHANNEL_REFS, SpiChannel, References),
    References++ :
      Last = _,
      NewChannel = SpiChannel,
      store_vector(SPI_CHANNEL_REFS, References', SpiChannel),
      NewGlobalChannels ! Global,
      Last' = Name |
	self;

    List ? Name(_NewChannel, BaseRate),
    GlobalChannels ? Entry,
    Entry = Name(_SpiChannel, _ComputeWeight, OtherBaseRate),
    BaseRate =\= OtherBaseRate :
      NewGlobalChannels ! Entry |
	fail(global_channel(rate_conflict(Name - BaseRate =\= OtherBaseRate))),
	self;

    List = [Name(_NewChannel, _BaseRate) | _], string(Name),
    GlobalChannels ? Entry,
    Entry = Last'(_, _, _),
    Last' @< Name :
      Last = _,
      NewGlobalChannels ! Entry |
	self;

    List ? Name(NewChannel, BaseRate),
    we(NewChannel),
    GlobalChannels =?= [Name1(_, _, _) | _],
    string(Name),
    Last @< Name, Name @< Name1,
    string_to_dlist("global.", GL, GT),
    string_to_dlist(Name, NL, []) :
      NewChannel = NewChannel'?,
      List'' = [Name(NewChannel', BaseRate) | List'],
      GlobalChannels' =
	[Name(SpiChannel?, SPI_DEFAULT_WEIGHT_NAME, BaseRate)
	| GlobalChannels],
      write_channel(new_channel(Id?, SpiChannel, SPI_DEFAULT_WEIGHT_NAME,
				BaseRate), Scheduler),
      GT = NL |
	list_to_string(GL, Id),
	self;

    List ? Name(NewChannel, BaseRate),
    Last @< Name,
    we(NewChannel),
    GlobalChannels ? Global,
    Global = Name(SpiChannel, _ComputeWeight, BaseRate),
    vector(SpiChannel),
    read_vector(SPI_CHANNEL_REFS, SpiChannel, References),
    References++ :
      Last = _,
      NewChannel = SpiChannel,
      store_vector(SPI_CHANNEL_REFS, References', SpiChannel),
      NewGlobalChannels ! Global,
      Last' = Name |
	self;

    List ? Name(_NewChannel, BaseRate),
    GlobalChannels ? Entry,
    Entry = Name(_SpiChannel, _ComputeWeight, OtherBaseRate),
    BaseRate =\= OtherBaseRate :
      NewGlobalChannels ! Entry |
	fail(global_channel(rate_conflict(Name - BaseRate =\= OtherBaseRate))),
	self;

    List = [Name(_NewChannel, _BaseRate) | _], string(Name),
    GlobalChannels ? Entry,
    Entry = Last'(_, _, _),
    Last' @< Name :
      Last = _,
      NewGlobalChannels ! Entry |
	self;

    List ? Name(NewChannel, BaseRate),
    we(NewChannel),
    GlobalChannels =?= [Name1(_, _, _) | _],
    string(Name),
    Last @< Name, Name @< Name1,
    string_to_dlist("global.", GL, GT),
    string_to_dlist(Name, NL, []) :
      NewChannel = NewChannel'?,
      List'' = [Name(NewChannel', BaseRate) | List'],
      GlobalChannels' =
	[Name(SpiChannel?, SPI_DEFAULT_WEIGHT_NAME, BaseRate)
	| GlobalChannels],
      write_channel(new_channel(Id?, SpiChannel, SPI_DEFAULT_WEIGHT_NAME,
				BaseRate), Scheduler),
      GT = NL |
	list_to_string(GL, Id),
	self;

    List ? Name(NewChannel, CW, BaseRate),
    Last @< Name,
    we(NewChannel),
    GlobalChannels ? Global,
    Global = Name(SpiChannel, ComputeWeight, BaseRate),
    vector(SpiChannel),
    read_vector(SPI_CHANNEL_REFS, SpiChannel, References),
    References++ :
      Last = _,
      CW = ComputeWeight?,
      NewChannel = SpiChannel,
      store_vector(SPI_CHANNEL_REFS, References', SpiChannel),
      NewGlobalChannels ! Global,
      Last' = Name |
	self;

    List ? Name(_NewChannel, BaseRate, _),
    GlobalChannels ? Entry, Entry = Name(_SpiChannel, OtherBaseRate, _),
    BaseRate =\= OtherBaseRate :
      NewGlobalChannels ! Entry |
	fail(global_channel(rate_conflict(Name - BaseRate =\= OtherBaseRate))),
	self;

    List ? Name(_NewChannel, ComputeWeight, _),
    GlobalChannels ? Entry, Entry = Name(_SpiChannel, OtherComputeWeight, _),
    ComputeWeight =\= OtherComputeWeight :
      NewGlobalChannels ! Entry |
	fail(global_channel(compute_weight_conflict(Name -
				ComputeWeight =\= OtherComputeWeight))),
	self;

    List = [Name(_NewChannel, _ComputeWeight, _BaseRate) | _], string(Name),
    GlobalChannels ? Entry,
    Entry = Last'(_, _, _),
    Last' @< Name :
      Last = _,
      NewGlobalChannels ! Entry |
	self;

    List ? Name(NewChannel, ComputeWeight, BaseRate),
    we(NewChannel),
    GlobalChannels =?= [Name1(_, _, _) | _],
    string(Name),
    Last @< Name, Name @< Name1,
    string_to_dlist("global.", GL, GT),
    string_to_dlist(Name, NL, []) :
      NewChannel = NewChannel'?,
      List'' = [Name(NewChannel', ComputeWeight, BaseRate) | List'],
      GlobalChannels' = [Name(SpiChannel?, ComputeWeight, BaseRate)
			| GlobalChannels],
      write_channel(new_channel(Id?, SpiChannel, ComputeWeight, BaseRate),
			Scheduler),
      GT = NL |
	list_to_string(GL, Id),
	self;

    otherwise :
      Last = _,
      Scheduler = _,
      NewGlobalChannels = GlobalChannels |
	fail(merge_global_channels(List)).


/***************************** Utilities ************************************/


ambient_suspending(ReadyChildren,
		   GlobalChannels, LocalChannels, SharedChannels,
		   Ready) :-

    known(ReadyChildren) |
	copy_global_channels,
	remove_all_communications(Channels?, Reply1),
	remove_all_communications(LocalChannels, Reply2),
	remove_all_communications(SharedChannels, Reply3),
	suspended_when_ready.

  suspended_when_ready(Reply1, Reply2, Reply3, Ready) :-

    known(Reply1), known(Reply2), known(Reply3) :
      Ready = done.
    

control_children(Control, Children, NewChildren, Ready) + (Done = done) :-

    known(Done),
    Children ? Child :
      NewChildren ! Child',
      write_channel(Control(Done'), Child, Child') |
	self;

    known(Done),
    Children = [] :
      Control = _,
      NewChildren = [],
      Ready = true.


copy_global_channels(GlobalChannels, Channels) :-
    GlobalChannels ? _Head |
	copy_interior.

  copy_interior(GlobalChannels, Channels) :-

    GlobalChannels ? Entry,
    GlobalChannels' =\= [],
    arg(2, Entry, Channel) :
      Channels ! Channel |
	self;

    GlobalChannels = [_] :
      Channels = [].


remove_all_communications(Channels, Reply) :-

    Channels ? Channel,
    read_vector(SPI_SEND_ANCHOR, Channel, SendQueue),
    read_vector(SPI_RECEIVE_ANCHOR, Channel, ReceiveQueue),
    arg(SPI_MESSAGE_LINKS, SendQueue, SendLinks),
    arg(SPI_MESSAGE_LINKS, ReceiveQueue, ReceiveLinks) :
      store_vector(SPI_NEXT_MS, SendQueue, SendLinks),
      store_vector(SPI_PREVIOUS_MS, SendQueue, SendLinks),
      store_vector(SPI_SEND_WEIGHT, 0, Channel),
      store_vector(SPI_NEXT_MS, ReceiveQueue, ReceiveLinks),
      store_vector(SPI_PREVIOUS_MS, ReceiveQueue, ReceiveLinks),
      store_vector(SPI_RECEIVE_WEIGHT, 0, Channel) |
	self;

    Channels =?= [] :
      Reply = true.

remove_shared_communications(Ambient, Channels, Scheduler, Reply) + 
			(Removed = false) :-

    Channels ? Channel,
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    Type =\= SPI_HOMODIMERIZED,
    read_vector(SPI_SEND_ANCHOR, Channel, SendQueue),
    read_vector(SPI_RECEIVE_ANCHOR, Channel, ReceiveQueue) |
	remove_from_queue(Ambient, Channel, Scheduler, SPI_SEND_WEIGHT,
				SendQueue, Removed, Removed'),
	remove_from_queue(Ambient, Channel, Scheduler, SPI_RECEIVE_WEIGHT,
				ReceiveQueue, Removed', Removed''),
	self;

    Channels ? Channel,
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    Type =?= SPI_HOMODIMERIZED,
    read_vector(SPI_DIMER_ANCHOR, Channel, DimerQueue) |
	remove_from_queue(Ambient, Channel, Scheduler, SPI_DIMER_WEIGHT,
				DimerQueue, Removed, Removed'),
	self;

    Channels =?= [] :
      Ambient = _,
      Scheduler = _ |
	unify_without_failure(Reply, Removed).

  remove_from_queue(Ambient, Channel, Scheduler, Index, Anchor,
			Removed, NewRemoved) + (PreviousMs = Anchor) :-

    arg(SPI_MESSAGE_LINKS, PreviousMs, Links),
    read_vector(SPI_NEXT_MS, Links, PreviousMs'),
    PreviousMs' =\= Anchor,
    arg(SPI_AMBIENT_CHANNEL, PreviousMs', MsAmbient),
    MsAmbient =\= Ambient |
	self;

    arg(SPI_MESSAGE_LINKS, PreviousMs, PreviousLinks),
    read_vector(SPI_NEXT_MS, PreviousLinks, CurrentMs),
    CurrentMs =\= Anchor,
    arg(SPI_AMBIENT_CHANNEL, CurrentMs, MsAmbient),
    MsAmbient =?= Ambient,
    arg(SPI_MESSAGE_LINKS, CurrentMs, CurrentLinks),
    read_vector(SPI_NEXT_MS, CurrentLinks, NextMs),
    arg(SPI_MESSAGE_LINKS, NextMs, NextLinks),
    read_vector(Index, Channel, Weight),
    arg(SPI_MS_MULTIPLIER, CurrentMs, Multiplier),
/************************** revise for C-support *****************************/
    Weight -= Multiplier :
/***************************** using Scheduler *******************************/
      Removed = _,
      Removed' = true,
      store_vector(Index, Weight', Channel),
      store_vector(SPI_NEXT_MS, NextMs, PreviousLinks),
      store_vector(SPI_PREVIOUS_MS, PreviousMs, NextLinks) |
	self;

    arg(SPI_MESSAGE_LINKS, PreviousMs, Links),
    read_vector(SPI_NEXT_MS, Links, NotMs),
    NotMs =?= Anchor :
      Ambient = _,
      Channel = _,
      Index = _,
      Scheduler = _,
      NewRemoved = Removed;

    otherwise |
	remove_from_queue_failed.

  remove_from_queue_failed(Ambient, Channel, Scheduler, Index, Anchor,
				Removed, NewRemoved, PreviousMs) :-

    arg(SPI_MESSAGE_LINKS, PreviousMs, Links),
    read_vector(SPI_NEXT_MS, Links, Ms) :
      Ambient = _,
      Anchor = _,
      write_channel(state(State), Scheduler),
      NewRemoved = Removed |
      /* This should never happen - internal failure!? */
	screen#display(remove-failed(PreviousMs, Index, Ms, Channel)-state-State);
    otherwise :
      /* Scheduler reset - fuggedabotit!! */
      Ambient = _,
      Anchor = _,
      Channel = _,
      Index = _,
      PreviousMs = _,
      Scheduler = _,
      NewRemoved = Removed |
	true. 


resolve_children(Children, NewChildren, Resolvent, NextResolvent) :-

    Children ? Child :
      NewChildren ! Child',
      write_channel(resolve(Resolvent, Resolvent'?), Child, Child') |
	self;

    Children =?= [] :
      NewChildren = [],
      Resolvent = NextResolvent.


resume_ambient_when_ready(AmbientId, In, Events, FromSub, Done,
			  Ambient, Parent, Children,
			  Requests, Controls, AmbientDone,
			  Scheduler, Debug,
			  Ready) :-

    known(Ready) :
      Controls ! resume |
	serve_ambient0.


resume_controls_when_ready(Reply1, Reply2, Reply3, Ready,
				Controls, NewControls) :-
    known(Reply1), known(Reply2), known(Reply3) :
      Ready = true,
      Controls = [resume | NewControls].